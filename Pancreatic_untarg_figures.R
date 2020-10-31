library(readxl)
library(tidyverse)
library(MetabolAnalyze)
library(gplots)

# Data from Neil. hlpos has less observations
# Data prep
dir <- "final analytical datasets.xlsx"

rppos <- read_xlsx(dir, sheet = 1) #%>% select(starts_with("Untg_"))
rpneg <- read_xlsx(dir, sheet = 2) %>% select(starts_with("Untg_"))
hlpos <- read_xlsx(dir, sheet = 3) %>% select(Idepic, starts_with("Untg_"))
hlneg <- read_xlsx(dir, sheet = 4) %>% select(starts_with("Untg_"))

dat <- bind_cols(rppos, rpneg, hlneg) %>% inner_join(hlpos, by = "Idepic")
mat <- dat %>% select(starts_with("Untg_")) %>% log2 %>% as.matrix
scalemat <- scaling(mat, type = "Unit")
pca <- prcomp(scalemat, scale. = F)
plot(pca, type = "l")

# 1st PC is PC1, status is Cncr_Caco_Panc, follow up time is Py 
df <- cbind(pca$x, dat) %>% mutate(PanC = as.factor(Cncr_Caco_Panc)) %>%
  #set follow up time of controls to that of corresponding case
  mutate(Py1 = ifelse(PanC == 1, Py, 0)) %>% group_by(Match_Caseset) %>%
  mutate(Py2 = max(Py1))

hist(df$Py)
par(mfrow = c(1,4))
boxplot(PC1 ~ Status, data = df, col = "dodgerblue", main = "Scores on PC1")
boxplot(PC2 ~ PanC, data = df, col = "limegreen", main = "Scores on PC2")
boxplot(PC2 ~ PanC, data = df, col = "hotpink", main = "Scores on PC3")
boxplot(PC2 ~ PanC, data = df, col = "yellow", main = "Scores on PC4")


# Plots
# Basic PCA with cases and controls coloured only
ggplot(df, aes(x=PC1, y=PC2, colour=PanC)) + geom_point() + theme_bw() +
  scale_shape_manual(values= c(17,19)) +
  xlab("Score on PC1") + ylab("Score on PC2") + geom_vline(xintercept=0, linetype = "dashed") +
  geom_hline(yintercept=0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = "top", legend.position = c(0.15, 0.97),
        legend.background = element_rect(color="white")) +
  labs(colour = "Disease status")

# Follow-up time against PC1 with fitted lines
ggplot(df, aes(x=Py2, y=PC1, colour=PanC)) + geom_point() + theme_bw() + geom_smooth(method=lm) +
  xlab("Follow-up time (years)") + labs(colour = "Case-control status") + ylab("Score on PC1") +
  theme(legend.position = c(0.15, 0.15),
        legend.background = element_rect(color = "grey"))

# PCA df by follow-up time (see http://www.cookbook-r.com/Graphs/Scatterplots_(ggplot2)/)
df$py_cat <- factor(dat$py_cat, levels =c( "<5", "5-<10", "10-<15", "15-<20", "20+") )
ggplot(df, aes(x=PC1, y=PC2, colour=PanC)) + geom_point() + theme_bw() + facet_grid(. ~ py_cat) +
  xlab("Score on PC1") + ylab("Score on PC2") + geom_vline(xintercept=0, linetype = "dashed") +
  geom_hline(yintercept=0, linetype = "dashed")  + labs(colour = "Disease status") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "top")


# Heatmap
cormat <- cor(mat)
colnames(cormat) <- NULL
library(pheatmap)
library(corrplot)
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582","#FDDBC7", #"#FFFFFF", 
                           "#D1E5F0", "#92C5DE","#4393C3", "#2166AC", "#053061"))

pheatmap(cormat, color = rev(col2(50)), clustering_method = "complete", border_color = "grey60")
corrplot(cormat, method = "square", tl.col = "black", order="original", tl.cex = 0.8)
