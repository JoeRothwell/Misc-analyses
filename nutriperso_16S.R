# Microbiome metadata

library(readxl)
library(haven)

micro <- read_xls("microbiome_20210106.xls")
micro1 <- read_sas("nutriperso_20210304.sas7bdat")

# 16S data
dat16s <- read_xlsx("RelAb_OTUs_Tab_NP_Lepage.xlsx")
