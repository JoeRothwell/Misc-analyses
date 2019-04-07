library(tidyverse)
library(readxl)
library(corrr)

d <-
read_xlsx("Comets_sim_dataset81717.xlsx", sheet = "SubjectMetabolites") %>%
  select(-SAMPLE_ID) %>%
  correlate()

