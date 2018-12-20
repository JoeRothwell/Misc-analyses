# Data prep
# Make subject and treatment variables and convert treatment to factor

library(tidyverse)
rye <- read_tsv("Rye data.txt") %>% separate(Sample, into = c("treatment", "subject"), sep = "_", convert = T)
rye$treatment <- as.factor(rye$treatment)
rye$treatment <- fct_rev(rye$treatment)
#str(rye)

wheat.rye <- rye %>% filter(treatment == "Wheat" | treatment == "Rye") %>% droplevels
wheat.bran <- rye %>% filter(treatment == "Wheat" | treatment == "Bran") %>% droplevels

# Example for Taurine
boxplot(Taurine ~ treatment, data = wheat.rye)

# Fixed-effects lm and Repeated measures ANOVA
lm(Taurine ~ treatment, data = wheat.rye) %>% summary
aov(Taurine ~ treatment + Error(subject/treatment), data = wheat.rye) %>% summary

# Mixed-effects model
library(lmerTest)
# random intercepts
fit <- lmer(Taurine ~ treatment + (1|subject), data = wheat.rye)
library(psycho)
get_contrasts(fit, "treatment")


# Apply to run many models. First subset and impute matrix (half lowest value)
mat1 <- wheat.rye[-(1:3)] %>% as.matrix
lowest.val1 <- min(mat1, na.rm = T)
mat1[is.na(mat1)] <- lowest.val/2

mat2 <- wheat.bran[-(1:3)] %>% as.matrix
lowest.val2 <- min(mat2, na.rm = T)
mat2[is.na(mat2)] <- lowest.val/2


fibre.lme <- function(x) { result <- lmer(x ~ treatment + (1|subject), data = wheat.rye)
  output <- get_contrasts(result, "treatment")$means
}

modlist.rye <- apply(mat1, 2, fibre.lme)
means.df <- do.call(cbind, modlist.rye)

fibre.lme2 <- function(x) lmer(x ~ treatment + (1|subject), data = wheat.rye)
modlist.bran <- apply(mat2, 2, fibre.lme)


library(broom)
library(psycho)
get_contrasts(fit, "treatment")

modlist <- apply(mat, 2, fibre.lme)
lapply(modlist, tidy)

