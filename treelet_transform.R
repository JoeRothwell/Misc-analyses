library(haven)
library(tidyverse)
auto <- read_dta("auto.dta")

library(treelet)
dat <- select(auto, price:gear_ratio) %>% as.matrix
covdat <- cov(dat)
Run_JTree(covdat, 4)
