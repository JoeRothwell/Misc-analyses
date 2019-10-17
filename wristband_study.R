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
meta1 <- read_xlsx("wristbands_e3n_e4n_20190930.xlsx")
meta2 <- read_xlsx("Bracelet_pilote_20190702.xlsx")

meta <- bind_cols(meta1, meta2) %>% 
  separate(Wristbands_num, sep = "#", into = c("A", "B")) %>% arrange(B)


mat <- as.matrix(dat[, -1]) %>% t

# Count missing values for the 89 compounds
miss <- apply(mat, 2, function(x) sum(is.na(x)))
hist(miss, main =  "Number of missing values for 89 compounds", breaks = 50, col = "dodgerblue")

# Get matrix of detections/non-detections
binarymat <- apply(mat, 1, function(x) as.integer(is.na(x))) #%>% heatmap
#heatmap.2(binarymat, scale = "none", trace = "none", Rowv = F, Colv = F, key = F)

library(reshape2)
df <- melt(binarymat)
ggplot(df, aes(x = Var1, y = Var2, fill = as.factor(value))) + geom_tile() +
  #theme_void() +
  scale_fill_grey()

# Get logical vector of missing < 20
include <- miss < 30
mat1 <- mat[, include]

# Impute and log
library(zoo)

mat2 <- na.aggregate(mat1, FUN = min)
logmat <- log(mat2)

pca <- prcomp(logmat, scale. = T)

cormat <- cor(logmat)
rownames(cormat) <- dat$sample_id[include]
library(corrplot)
corrplot(cormat, method = "square", tl.col = "black", tl.cex = 0.8)


pca2d(pca, group = meta$wristbands_urban_d1, legend = NULL)
title("Contanimant profile of wristbands from 40 subjects")
box()

pca2d(pca, group = meta$wristbands_urban_d1, biplot = T, legend = NULL)
title("Contanimant profile of wristbands from 40 subjects")
box()
