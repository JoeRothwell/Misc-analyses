library(tidyverse)
library(pheatmap)

# Get coffee biomarker data and metadata
coffee.hcc <- readRDS("prepdata/HCC coffee biomarkers and metadata.rds") %>% 
  separate(LabID, into=c("ctr","no"), sep="_", convert=T)

# HCC discriminant data
heatmapdata <- read_csv("data/HCC 46 discriminants.csv") %>% separate(Sample, into=c("ctr","no"), sep="_", convert=T)

# Remove labels and log transform
heatmapdata <- heatmapdata[, -(1:3)] %>% log
cormat <- cor(heatmapdata, use = "pairwise.complete.obs")

# Define colour scheme to copy corrplot and plot with pheatmap
library(pheatmap)
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582","#FDDBC7", #"#FFFFFF", 
                           "#D1E5F0", "#92C5DE","#4393C3", "#2166AC", "#053061"))

pheatmap(cormat, color = rev(col2(50)), clustering_method = "complete", border_color = "grey60")


# Create follow up time variable and plot as a histogram
hcc.meta <- inner_join(coffee.hcc, heatmapdata, by="no") %>% mutate(Tfollowup = Agexit_Frst - Age_Recr)

# histogram
hist(hcc.meta$Tfollowup, breaks = 20, col="dodgerblue", xlab = "Follow-up time (years)", 
     main="Distribution follow-up time")

# subset discriminants for PCA
discs <- hcc.meta %>% select(Retinol:CEHC) %>% as.matrix

# impute with median
library(zoo)
discs.impute <- na.aggregate(discs, FUN = median)

# Run PCA
discpca <- prcomp(discs.impute, scale. = T)

# Extract scores from object and join to follow-up data. Set control follow up time to corresponding case
dfscores <- data.frame(pair = hcc.meta$Match_Caseset, TF = hcc.meta$Tfollowup, 
            HCC = hcc.meta$Caselive_Crs, discpca$x) %>% 
  
            #set follow up time of controls to that of corresponding case
  
            mutate(TF1 = ifelse(HCC == 1, TF, 0)) %>% group_by(pair) %>%
            mutate(TF2 = max(TF1), cats = cut(TF2, breaks=c(0,4,8,12,16), 
                labels = c("0 to 4 years", "4 to 8 years", "8 to 12 years", "12 to 16 years")),
            cats2 = cut(TF2, breaks=c(0,2,6,16),
                labels = c("0 to 2 years", "2 to 6 years", "6 to 16 years"))) %>% 
            select(-(PC3:PC46))

dfscores$HCC <- factor(dfscores$HCC, labels = c("Controls", "Cases"))

# Basic PCA with cases and controls coloured only
ggplot(dfscores, aes(x=PC1, y=PC2, colour=HCC)) + geom_point() + theme_bw() +
  scale_shape_manual(values= c(17,19)) +
  xlab("Score on PC1") + ylab("Score on PC2") + geom_vline(xintercept=0, linetype = "dashed") +
  geom_hline(yintercept=0, linetype = "dashed")

# Follow up time by colour and shape by case control status
ggplot(dfscores, aes(x=PC1, y=PC2, shape=HCC, colour=cats)) + geom_point() + theme_bw() +
  scale_shape_manual(values= c(17,19))

# PCA faceted by follow-up time (see http://www.cookbook-r.com/Graphs/Scatterplots_(ggplot2)/)
ggplot(dfscores, aes(x=PC1, y=PC2, colour=HCC)) + geom_point() + theme_bw() + facet_grid(. ~ cats2) +
  xlab("Score on PC1") + ylab("Score on PC2") + geom_vline(xintercept=0, linetype = "dashed") +
  geom_hline(yintercept=0, linetype = "dashed")

# Follow-up time against PC1 with fitted lines
ggplot(dfscores, aes(x=TF2, y=PC1, colour=HCC)) + geom_point() + theme_bw() + geom_smooth(method=lm) +
  xlab("Follow-up time (years)") + ylab("Score on PC1") #+ ylim(-75,75)

scatterplot3d(dfscores$TF2, dfscores$PC1, dfscores$PC2)
library(car)
scatter3d(dfscores$TF2, dfscores$PC1, dfscores$PC2, surface=F)

# contributions to PC1 and PC2
# plot loadings PC1 vs PC2
plot(discpca$rotation[, 1], discpca$rotation[, 2], pch=19, col="dodgerblue")
abline(h=0, v=0)

# Calculate contributions. Get absolute values of rotation and convert to proportions, join names, plot
# aload <- abs(pcs$rotation)
# do not use absolute values to get positive and negative contributions

aload <- discpca$rotation
contributions <- sweep(aload, 2, colSums(aload), "/") %>% data.frame %>% rownames_to_column(var="Compound")

# plot top 10 contributions for PC1, 2, 3. First prepare data then plot
contr <- contributions %>% select(Compound:PC2) %>% gather(PC, val, -Compound) %>% 
  mutate(absval = abs(val), sign = ifelse(val == absval, "Pos", "Neg")) %>% 
  #Take 25 most contributing compounds only
  group_by(PC) %>% top_n(n=25, wt=absval)

ggplot(contr, aes(x = Compound, y=val, fill=PC)) + geom_bar(stat="identity") + coord_flip() + theme_bw() +
  facet_grid(PC ~ .  , scales = "free_y") + ylab("Relative importance to PC") + 
  xlab("Components contributing most to PC") +
  theme(legend.position = "none")

library(car)
scatter3d(dfscores$TF2, dfscores$PC1, dfscores$PC2)

library(scatterplot3d)

colours <- c("#E69F00", "#56B4E9")
colours <- colours[as.numeric(dfscores$HCC)]

scatterplot3d(dfscores$TF2, dfscores$PC1, dfscores$PC2, color=colours, pch=16,
              xlab="Follow-up time (years)",
              ylab="Score on PC1",
              zlab="Score on PC2")

scatterplot3d(dfscores$PC1, dfscores$PC2, dfscores$TF2, color=colours, pch=16,
              zlab="Follow-up time (years)",
              xlab="Score on PC1",
              ylab="Score on PC2")

write.csv(dfscores, "Discriminant PC scores and follow up time HCC.csv")

# Volcano plot for biocrates metabolomics data
library(ggplot2)
library(RColorBrewer)
data <- read.csv("Biocrates summary jan 2017.csv")


ggplot(data, aes(x=OR, y=-log10(pval), colour=groups)) + geom_point() + theme_bw() +
  geom_hline(yintercept = -log10(0.05/23), linetype = "dashed") +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(breaks=seq(0, 6, 1)) +
  scale_x_continuous(breaks=seq(0, 3.5, 0.5)) +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position=c(0.5, 1),
        legend.justification = c(0.5, 1),
        legend.title = element_blank(),
        legend.background = element_rect(colour = "black")
  )

ggsave("Volcano plot HCC.svg", height=5, width=5.5)
