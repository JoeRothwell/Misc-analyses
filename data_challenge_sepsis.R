# Physionet data challenge: predicting sepsis
# Joe Rothwell
# Challenge: https://physionet.org/content/challenge-2019/1.0.0/
# Need to predict sepsis 6 hours before the clinical diagnosis (tsepsis)
# For sepsis patients, SepsisLabel is 1 is assigned from 6 hours before clinical diagnosis

# Read in data. First look at a few files 
p1 <- read.delim("training/p000001.psv", sep = "|") # 54 timepoints of 41 features
p2 <- read.delim("training/p000002.psv", sep = "|") # 21 timepoints of 41 features
p3 <- read.delim("training_setB/p100013.psv", sep = "|") # 90

skim(p3)

# Read all hospital A data
library(fs)
library(tidyverse)
file.paths <- dir_ls("training/")
myfiles <- lapply(file.paths, function(x) read.delim(x, sep = "|"))
hospA <- bind_rows(myfiles, .id = "id")

# Distribution of hours in ICU
hospA %>% group_by(id) %>% summarise(timepoints = n()) %>% 
  ggplot(aes(x = timepoints)) + geom_histogram(binwidth = 10)
# As many as 8-336 hours spent in ICU (timepoints)

# Use skimr to get data summaries
library(skimr)
sumhospA <- skim_without_charts(hospA)

# How many instances of sepsis?
hospA %>% group_by(id) %>% summarise(sepsis = max(SepsisLabel)) %>% group_by(sepsis) %>% 
  count()
# 1790/18546 diagnosed: imbalanced data problem

# Strategy: to reduce the data to one line per patient and model this table
# Fill missing values in time series, first filling down then up to fill the remainder
hospAfill <- hospA %>% group_by(id) %>% fill(everything(), .direction = "downup") %>% ungroup()

# Get sepsis patients at tsepsis-6h only
hospA.sepsis <- hospAfill %>% filter(SepsisLabel == 1) %>% group_by(id) %>% slice(1) %>% ungroup()

# Get non-sepsis patients at the timepoint with least NAs
hospA.norm <- hospAfill %>% group_by(id) %>% 
  filter(max(SepsisLabel) == 0) %>% ungroup() %>%
  arrange(rowSums(is.na(.))) %>% distinct(id, .keep_all = TRUE) 

# Bind rows from sepsis and timepoint with least NAs
hospA1 <- bind_rows(hospA.sepsis, hospA.norm) %>% select(-id, -ICULOS) %>%
  mutate(SepsisLabel = as_factor(SepsisLabel), Unit1 = as_factor(Unit1))

# We now have one row per patient. Look at proportions of missing data
skim_without_charts(hospA1)

# Resource used: https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/
# Splits into 75/25 random training and test (although not imbalanced)
set.seed(123)
sepsis.split <- initial_split(hospA1, strata = SepsisLabel)
sepsis.split

#training(sepsis.split)
#testing(sepsis.split)

# Define a pre-processing recipe. Step_smote generates new examples of the minority class 
# using nearest neighbours of these cases
library(themis)
sepsis.rec <- 
  training(sepsis.split) %>%
  recipe(SepsisLabel ~ .) %>%
  step_corr(all_numeric_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_smote(SepsisLabel)

# Recipe info
sepsis.rec

# Specify bagged decision tree model. Min_n is the min number of data points that
# are required for the node to be split further
library(baguette)
sepsis.spec <-
  bag_tree(min_n = 10) %>%
  set_engine("rpart", times = 25) %>%
  set_mode("classification")

# Fit model to training data and get info
sepsis.bag <- sepsis.spec %>% fit(SepsisLabel ~ ., data = training(sepsis.split))
sepsis.bag # Here temperature and HR are the most important in the model

### Model tuning. We should train the complexity and the tree depth
# Resource used: https://www.tidymodels.org/start/tuning/
tune.spec <-
  bag_tree(min_n = 10, cost_complexity = tune(), tree_depth = tune()) %>%
  set_engine("rpart", times = 25) %>%
  set_mode("classification")

# Make tuning grid for cost complexity and tree depth
tree.grid <- grid_regular(cost_complexity(), tree_depth(), levels = 5)
tree.grid

# Make folds for cross validation (resample package)
set.seed(123)
sepsis.folds <- vfold_cv(training(sepsis.split), v = 10, strata = SepsisLabel)

# Define model tuning workflow
tree.wf <-
  workflow() %>%
  add_model(tune.spec) %>%
  add_recipe(sepsis.rec)

# Start cross validation (warning: takes a long time). First define metrics
sepsis.metrics <- metric_set(accuracy, kap)

library(doParallel)
registerDoParallel()
tree.res <- tree.wf %>% 
  tune_grid(resamples = sepsis.folds, grid = tree.grid, metrics = sepsis.metrics)
tree.res %>% collect_metrics()

# Plot metrics with ggplot
tree.res %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

# Automatically get the best tree
tree.res %>% show_best("kap")
best.tree <- tree.res %>% select_best("kap")

# Specify the best tree in the workflow and fit the final model
final.wf <- tree.wf %>% finalize_workflow(best.tree)
final.fit <- final.wf %>% last_fit(sepsis.split) 

# Get confusion matrix for final data from this model
cm1 <- final.fit %>% collect_predictions() %>% 
  conf_mat(truth = SepsisLabel, estimate = .pred_class)
summary(cm1, event_level = "second")
# Kappa is much lower than the accuracy.
# Sensitivity is 0.95 but specificity is only 0.30.

# Roc curve for probabilities
final.fit %>% collect_predictions() %>%
  roc_curve(SepsisLabel, .pred_1, event_level = "second") %>% autoplot()

final.fit %>% collect_predictions() %>%
  roc_auc(SepsisLabel, .pred_1, event_level = "second")

# Predict hospital B from hospital A model
file.pathsB <- dir_ls("training_setB/")
myfilesB <- lapply(file.pathsB, function(x) read.delim(x, sep = "|"))
hospB <- bind_rows(myfilesB, .id = "id")

# Fill missing values in time series, first filling down then up to fill the remainder
hospBfill <- hospB %>% group_by(id) %>% fill(everything(), .direction = "downup") %>% ungroup()

# Get sepsis patients at tsepsis-6h only
hospB.sepsis <- hospBfill %>% filter(SepsisLabel == 1) %>% group_by(id) %>% slice(1) %>% ungroup()

# Get non-sepsis patients at the timepoint with least NAs
hospB.norm <- hospBfill %>% group_by(id) %>% 
  filter(max(SepsisLabel) == 0) %>% ungroup() %>%
  arrange(rowSums(is.na(.))) %>% distinct(id, .keep_all = TRUE) 

# Bind rows from sepsis and timepoint with least NAs
hospB1 <- bind_rows(hospB.sepsis, hospB.norm) %>% select(-id, -ICULOS) %>%
  mutate(SepsisLabel = as_factor(SepsisLabel), Unit1 = as_factor(Unit1))

# Predict 99% of hospital B data using the model derived from A training data
sepsis.splitB <- initial_split(hospB1, prop = 0.01, strata = SepsisLabel)
final.fitB <- final.wf %>% last_fit(sepsis.splitB) 

# Get confusion matrix and summarise
final.fitB %>% collect_metrics()
cm2 <- final.fitB %>% collect_predictions() %>% 
  conf_mat(truth = SepsisLabel, estimate = .pred_class)
summary(cm2, event_level = "second")
# Kappa is lower than for the held-out hospital A set
# Sensitivity is 0.95 but specificity is only 0.21

# Roc curve for probabilities
final.fitB %>% collect_predictions() %>%
  roc_curve(SepsisLabel, .pred_1, event_level = "second") %>% autoplot()

final.fitB %>% collect_predictions() %>%
  roc_auc(SepsisLabel, .pred_1, event_level = "second")

