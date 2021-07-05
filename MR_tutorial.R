# Setting up an MR analysis
# From: https://mrcieu.github.io/TwoSampleMR/articles/introduction.html
# From: https://marinalearning.netlify.app/2021/03/22/setting-up-multivariable-mendelian-randomization-analysis/

library(tidyverse)
library(TwoSampleMR)

# List available GWASs
ao <- available_outcomes()

# Get instruments
exposure_dat <- extract_instruments("ieu-a-2")

# Get effects of instruments on outcome
outcome_dat <- extract_outcome_data(snps=exposure_dat$SNP, outcomes="ieu-a-7")

# Harmonise the exposure and outcome data
dat <- harmonise_data(exposure_dat, outcome_dat)

# Perform MR
res <- mr(dat)

library(vroom)
library(MVMR)

# Load BMI exposures
adult_bmi_exp <- read_tsv(paste0(data_path, "adult_bmi_tophits.tsv"))
early_bmi_exp <- read_tsv(paste0(data_path, "early_bmi_adj_tophits.tsv"))