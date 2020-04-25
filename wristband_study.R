# Data prep for France and Italy data from Excel sheets

library(readxl)
library(janitor)
library(tidyverse)

# Metadata for French and Italian subjects
# France: put two files together (same order) and put in same order as matrix
meta1 <- read_xlsx("wristbands_e3n_e4n_20190930.xlsx", na = ".")
meta2 <- read_xlsx("Bracelet_pilote_20190702.xlsx")
meta <- bind_cols(meta1, meta2) %>% 
  separate(Wristbands_num, sep = "#", into = c("A", "B")) %>% arrange(B)

# Metadata for Italy
meta1 <- read_xlsx("Italy wristbands final 02202020.xlsx", n_max = 9, col_names = T) %>%
  t() %>% data.frame() %>% #slice(-1) %>% 
  row_to_names(1)


# Compound data: France
# Drop note, total and empty rows. Convert all to numeric and zeros to NA
dat <- read_xlsx("Final data for France wristbands 10162019.xlsx") %>%
  clean_names() %>%
  remove_empty("rows") %>%
  filter(!str_detect(sample_id, "Total|Note|PBDEs|nBFRs|PAHs|OPFRs")) %>%
  mutate_at(.vars = vars(h2f_w12_020819, h2f_w16_021219), .funs = as.numeric) %>%
  na_if(0)

# Italy
dat1 <- read_xlsx("Italy wristbands final 02202020.xlsx") %>%
  slice(-(1:10)) %>% clean_names() %>%
  remove_empty("rows") %>%
  filter(!str_detect(sample_id, "Total|Note|PBDEs|nBFRs|PAHs|OPFRs|OPEs")) %>%
  mutate_at(vars(-sample_id), .funs = as.numeric) %>%
  na_if(0)

# Make compound metadata
cmpd.meta <- data.frame(Cmpd_group = c(rep("PBDEs", 36), rep("nBFRs", 11), rep("PAHs", 18), 
              rep("OPFRs", 24)), cmpd.fr = dat$sample_id, cmpd.it = dat1$sample_id)

mat <- cbind(dat[, -1], dat1[, -1]) %>% t

# Count missing values for the 89 compounds
miss <- apply(mat, 2, function(x) sum(is.na(x)))
#miss1 <- apply(mtx, 2, function(x) sum(is.na(x)))
hist(miss, main =  "Number of missing values for 89 compounds", breaks = 50, col = "dodgerblue")


# Get matrix of detections/non-detections
binarymat <- apply(mat, 1, function(x) as.integer(!is.na(x)))
binarymat <- cbind(cmpd.meta, binarymat)

# Missingness map
library(reshape2)
df <- melt(binarymat) %>% mutate(Detected = fct_rev(as.factor(value)))

ggplot(df, aes(x = cmpd.fr, y = variable, fill = Detected)) + geom_tile() +
  scale_fill_grey() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(axis.title = element_blank()) +
  facet_grid(. ~ Cmpd_group, scales = "free", space = "free")


# Get logical vector of non-missing < 75%
include <- miss < 53
mat1 <- mat[, include]

# Impute and log
library(zoo)

mat2 <- na.aggregate(mat1, FUN = min)
logmat <- log(mat2)
colnames(logmat) <- cmpd.meta$cmpd.fr[include]




