library(readxl)
library(janitor)
library(tidyverse)

# Compound data
# Drop note, total and empty rows. Convert all to numeric and zeros to NA
dat <- read_xlsx("Final data for France wristbands 10162019.xlsx") %>%
  clean_names() %>%
  remove_empty("rows") %>%
  filter(!str_detect(sample_id, "Total|Note|PBDEs|nBFRs|PAHs|OPFRs")) %>%
  mutate_at(.vars = vars(h2f_w12_020819, h2f_w16_021219), .funs = as.numeric) %>%
  na_if(0)

# Subject metadata (2 files)

# Metadata. Put two files together (same order) and put in same order as matrix
meta1 <- read_xlsx("wristbands_e3n_e4n_20190930.xlsx", na = ".")
meta2 <- read_xlsx("Bracelet_pilote_20190702.xlsx")

meta <- bind_cols(meta1, meta2) %>% 
  separate(Wristbands_num, sep = "#", into = c("A", "B")) %>% arrange(B)

# Make compound metadata
cmpd.meta <- data.frame(Cmpd_group = c(rep("PBDEs", 36), rep("nBFRs", 11), rep("PAHs", 18), 
                                       rep("OPFRs", 24)), Cmpd = dat$sample_id)
mat <- as.matrix(dat[, -1]) %>% t

# Count missing values for the 89 compounds
miss <- apply(mat, 2, function(x) sum(is.na(x)))
hist(miss, main =  "Number of missing values for 89 compounds", breaks = 50, col = "dodgerblue")

# Get matrix of detections/non-detections
binarymat <- apply(mat, 1, function(x) as.integer(!is.na(x)))
binarymat <- cbind(cmpd.meta, binarymat)

# Missingness map
library(reshape2)
df <- melt(binarymat) %>% mutate(Detected = fct_rev(as.factor(value)))

ggplot(df, aes(x = Cmpd, y = variable, fill = Detected)) + geom_tile() +
  scale_fill_grey() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(axis.title = element_blank()) +
  facet_grid(. ~ Cmpd_group, scales = "free", space = "free")

# Get logical vector of missing < 20
include <- miss < 30
mat1 <- mat[, include]

# Impute and log
library(zoo)

mat2 <- na.aggregate(mat1, FUN = min)
logmat <- log(mat2)
colnames(logmat) <- cmpd.meta$Cmpd[include]

# Correlation heatmap
cormat <- cor(logmat)
rownames(cormat) <- cmpd.meta$dat.sample_id[include]
library(corrplot)
corrplot(cormat, method = "square", tl.col = "black", tl.cex = 0.8)

# PCA of compound profiles
pca <- prcomp(logmat, scale. = T)
place.day1 <- factor(meta$wristbands_urban_d1, labels = c("Rural", "Urban"))
place.day2 <- factor(meta$wristbands_urban_d2, labels = c("Rural", "Urban"))
place.day3 <- factor(meta$wristbands_urban_d3, labels = c("Rural", "Urban"))
place.day4 <- factor(meta$wristbands_urban_d4, labels = c("Rural", "Urban"))
couple <- as.factor(meta$num_couple)
sex <- c(rep("M", 20), rep("F", 20))

# With purrr
#meta %>% select(contains("wristbands_u")) %>% map(function(x) factor(x, labels = c("Rural", "Urban")))

# PCAs by different characteristics

library(pca3d)
par(mfrow=c(1,2))

pca2d(pca, group = place.day1, legend = "topright", axe.titles = c("Score on PC1", "Score on PC2"))
title("Place, day 1")
box()

pca2d(pca, col = "grey", show.labels = couple, axe.titles = c("Score on PC1", "Score on PC2"))
title("Couples in study")
box()

pca2d(pca, group = sex, legend = "topright", axe.titles = c("Score on PC1", "Score on PC2"))
title("Participant gender")
box()

# Biplot
dev.off()
pca2d(pca, biplot = T, axe.titles = c("Score on PC1", "Score on PC2"))
title("PCA biplot")
box()

# Summarise compounds
mat3 <- na.aggregate(mat, FUN = min)
meds <- apply(mat3, 2, median)
boxplot(log10(meds), horizontal = T)

# Plot median intensities
plot(log10(meds), xlab = "", ylab = "Concentration (log10 ng/g)", col = "white")
text(log10(meds), labels = dat$sample_id, cex = 0.8)

# Dotchart
dotchart(log10(meds), labels = cmpd.meta$Cmpd, groups = cmpd.meta$Cmpd_group, 
         xlab = "Median concentration log10(ng)/g", cex = 0.7, gcolor = "red",
         main = "Compound concentrations")

# Scatter
plot(log10(meds), xlab = "", ylab = "Concentration (log10 ng/g)", col = "white")
text(log10(meds), labels = dat$sample_id, cex = 0.8)


