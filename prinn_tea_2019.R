library(readxl)
library(tidyverse)
library(janitor)
# Read data
tea <- read_xlsx("tea_2019.xlsx", sheet = 7, skip = 1)
df0 <- tea %>% select(-(1:5)) %>% prcomp(scale. = T) %>% pluck(5) %>% as_tibble()
df1 <- tea %>% select(-(1:5)) %>% prcomp(scale. = F) %>% pluck(5) %>% as_tibble()
df2 <- tea %>% select(-(1:5), -7) %>% prcomp(scale. = F) %>% pluck(5) %>% as_tibble()
df3 <- tea %>% select(-(1:5), -7) %>% prcomp(scale. = T) %>% pluck(5) %>% as_tibble()

# Make base plot
base <- ggplot(df0, aes(PC1, PC2)) + geom_text(label = tea$...4) + theme_bw() + 
  geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed")

# Plot scaled and unscaled
base + ggsave("scaled_all_vars.png")
base %+% df1 + ggsave("unscaled_all_vars.png")
base %+% df2 + ggsave("scaled_no_aa.png")
base %+% df3 + ggsave("unscaled_no_aa.png")

# New data June 2020
tea <- read_xlsx("Assam tea_2020.xlsx", sheet = 7, skip = 1)
# Excluding total phenolic content
pca <- tea %>% select(-(1:5)) %>% prcomp(scale. = T)
summary(pca)
scores <- pca %>% pluck(5) %>% as_tibble()
loadings <- pca %>% pluck(2) %>% as.tibble()

ggplot(scores, aes(PC1, PC2)) + geom_text(label = tea$...4) + theme_bw() + 
  geom_hline(yintercept = 0, linetype = "dashed") + xlab("Score on PC1 (33.5% variance explained)") +
  ylab("Score on PC2 (22.8% variance explained)") +
  geom_vline(xintercept = 0, linetype = "dashed") + ggsave("scores Assam scaled.png")

ggplot(loadings, aes(PC1, PC2)) + geom_text(label = names(pca$center), size = 3) + theme_bw() + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + ggsave("loadings Assam scaled.png")

write.table(loadings, "loadings assam.txt")

tea2 <- read_xlsx("Assam and Chinese tea_2020.xlsx", sheet = 7, skip = 1)
# Excluding total phenolic content
pca2 <- tea2 %>% select(-(1:6)) %>% remove_constant() %>% prcomp(scale. = T)

scores <- pca2 %>% pluck(5) %>% as_tibble()
loadings <- pca2 %>% pluck(2) %>% as_tibble()
summary(pca2)
ggplot(scores, aes(PC1, PC2)) + geom_text(label = tea2$Abbreviation) + theme_bw() + 
  geom_hline(yintercept = 0, linetype = "dashed") + xlab("Score on PC1 (30.4% variance explained)") +
  ylab("Score on PC2 (25.9% variance explained)") +
  geom_vline(xintercept = 0, linetype = "dashed") + ggsave("scores Assam Chinese scaled.png")

ggplot(loadings, aes(PC1, PC2)) + geom_text(label = names(pca2$center), size = 3) + theme_bw() + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + ggsave("loadings Assam Chinese scaled.png")

write.table(loadings, "loadings assam chinese.txt")
