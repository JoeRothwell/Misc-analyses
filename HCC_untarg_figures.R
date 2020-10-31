# Function to prepare data and make figures for HCC manuscript Stepien et al
library(tidyverse)
cofhcc <- readRDS("HCC coffee biomarkers and metadata.rds")
# Note: added L1_ to compounds directly in .csv
cmpds  <- read_csv("HCC_46_disc_levels.csv") # levels 1-3 annotated

pca.data <- function(dat, ints, lev1 = F, time.start = NULL, type = c("correlation", "scores", "loadings")) {

  # Split columns for both intensities and metadata
  cofhcc <- dat %>% separate(LabID, into=c("ctr","no"), sep="_", convert=T)
  ints <- cmpds %>% separate(Sample, into=c("ctr","no"), sep="_", convert=T)
  ints1 <- if(lev1 == T) ints %>% select(ctr : Label, starts_with("L1_")) else ints
  
  # Create follow up time variable and plot as a histogram
  # Need to ungroup or grouping variable is readded with select()
  meta <- inner_join(cofhcc, ints1, by="no") %>% 
    mutate(Tfollowup = Agexit_Frst - Age_Recr, Tfollowup2 = ifelse(Caselive_Crs == 1, Tfollowup, 0)) %>%
    group_by(Match_Caseset) %>%
    mutate(Tfollowup3 = max(Tfollowup2)) %>% ungroup()
  
  # Subset by follow up time if specified
  meta <- if(!is.null(time.start)) meta %>% filter(Tfollowup3 > time.start) else meta
  print(paste(nrow(meta), "observations included"))
  
  # subset discriminants for PCA
  discs <- if(lev1 == F) meta %>% select(L1_Retinol:L1_CEHC) else meta %>% select(starts_with("L1_"))
  
  # impute with median and run PCA
  library(zoo)
  discs1 <- na.aggregate(as.matrix(discs), FUN = median)
  pca <- prcomp(log2(discs1), scale. = T)
  print(summary(pca))

  # Histogram of follow-up time
  hist(meta$Tfollowup, breaks = 20, col="dodgerblue", xlab = "Follow-up time (years)", 
       main="Distribution follow-up time")
  
  # New
  output <- data.frame(meta, pca$x)
  output$HCC <- factor(output$Caselive_Crs, labels = c("Controls", "Cases"))
  
  if(type == "scores") return(output)
  # Calculate contributions to PC1 and PC2. Get absolute values of rotation and convert to proportions, join names, plot
  # do not use absolute values to get positive and negative contributions
  aload <- pca$rotation
  contributions <- sweep(aload, 2, colSums(aload), "/") %>% data.frame %>% rownames_to_column(var="Compound")
  
  contr <- contributions %>% select(Compound:PC2) %>% gather(PC, val, -Compound) %>% 
    mutate(absval = abs(val), sign = ifelse(val == absval, "Pos", "Neg")) %>% 
  #Take 25 most contributing compounds only
    group_by(PC) #%>% top_n(n=25, wt=absval)
  
  if(type == "contributions") return(contr)
  
  # Remove labels and log transform
  heatdata <- ints[, -(1:3)] %>% log2
  cormat <- cor(heatdata, use = "pairwise.complete.obs")
  
  if(type == "correlation") return(cormat)
}

# Get data for all observations, follow up time > 2, 4, and 10 years

all <- pca.data(cofhcc, cmpds, type = "scores")
lev1 <- pca.data(cofhcc, cmpds, type = "scores", lev1 = T)

ts4 <- pca.data(cofhcc, cmpds, type = "scores", time.start = 4)
ts4.lev1 <- pca.data(cofhcc, cmpds, type = "scores", lev1 = T, time.start = 4)

# Not used in manuscript
#ts2 <- pca.data(cofhcc, cmpds, type = "scores", time.start = 2)
#ts2.lev1 <- pca.data(cofhcc, cmpds, type = "scores", lev1 = T, time.start = 2)
#ts6 <- pca.data(cofhcc, cmpds, type = "scores", time.start = 6)
#ts6.lev1 <- pca.data(cofhcc, cmpds, type = "scores", lev1 = T, time.start = 6)

# Plot figures for manuscript (supp 3A, 3B, 3C) ----

# Fig 2A: Single scores plot with cases and controls coloured only
library(ggplot2)
library(scales)
p1 <- ggplot(all, aes(x=PC1, y=PC2, shape=HCC, colour = HCC)) + geom_point() + theme_bw(base_size = 10) +
  scale_shape_manual(values= c(2,16)) +
  xlab("Score on PC1") + ylab("Score on PC2") + geom_vline(xintercept=0, linetype = "dashed") +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  theme(legend.position = "none", plot.subtitle = element_text(size = 10))

# Plot different follow up time ranges
fig2a1 <- p1          + labs(title = "A", subtitle = "(1) 46 metabolites identified at Levels 1-3") #%>%
  #ggsave(filename = "Picture 1.png", height = 10, width = 15, units = "cm")
  
fig2a2 <- p1 %+% lev1 + labs(title = "", subtitle = "(2) 14 metabolites identified at Level 1") #%>%
  #ggsave(filename = "Picture 2.png", height = 10, width = 15, units = "cm")

library(cowplot)
#fig2a <- plot_grid(fig2a1, fig2a2)

#p1 %+% lev1 + xlim(-14, 8) + ylim(-11, 7) + ggsave(filename = "Picture 2a.png", height = 10, width = 15, units = "cm")


# Fig 2B: Follow-up time against PC1 with fitted lines
p2 <- ggplot(all, aes(x=Tfollowup3, y=PC1, shape=HCC, colour = HCC)) + geom_point() + theme_bw(base_size = 10) + 
  geom_smooth(method = lm) +
  scale_shape_manual(values= c(2,16)) +
  xlab("Follow-up time (years)") + ylab("Score on PC1")  +
  theme(legend.position = "none", plot.subtitle = element_text(size = 10))



# Plot different follow up time ranges (final paper uses 4 years)
fig2b1 <- p2     + labs(title = "B", subtitle = "(1) 46 metabolites identified at Levels 1-3")
  #ggsave(filename = "Picture 3.png", height = 10, width = 15, units = "cm")

fig2b2 <- p2 %+% lev1 + labs(title = "", subtitle = "(2) 14 metabolites identified at Level 1")
  #ggsave(filename = "Picture 4.png", height = 10, width = 15, units = "cm")

fig2b3 <- p2 %+% ts4 + xlim(0, 15) + labs(title = "", subtitle = "Excluding cases with 4 or less years of follow up, Levels 1-3")
  #ggsave(filename = "Picture 5.png", height = 10, width = 15, units = "cm")
  
fig2b4 <- p2 %+% ts4.lev1 + xlim(0, 15) + labs(title = "", subtitle = "Excluding cases with 4 or less years of follow up, Level 1")
  #ggsave(filename = "Picture 6.png", height = 10, width = 15, units = "cm")

# 2 and 6 years, not used in manuscript
#p2 %+% ts2  %>% ggsave(filename = "All_ftime2_15.png", height = 10, width = 15, units = "cm")
#p2 %+% ts2.lev1 %>% ggsave(filename = "Lev1_ftime2_15.png", height = 10, width = 15, units = "cm")
#p2 %+% ts6 + xlim(0, 15) + ggsave(filename = "Picture 5a.png", height = 10, width = 15, units = "cm")
#p2 %+% ts6.lev1 + xlim(0, 15) + ggsave(filename = "Picture 6a.png", height = 10, width = 15, units = "cm")

fig2ab <- plot_grid(fig2a1, fig2a2, fig2b1, fig2b2, fig2b3, fig2b4, ncol = 2)
save_plot("fig2ab.tiff", fig2ab, ncol = 2, nrow = 3, base_asp = 1.2)


# Fig 3C: plot top 10 contributions for PC1, 2, 3. First prepare data then plot
contr <- pca.data(cofhcc, type = "contributions")
contr0 <- pca.data(cofhcc, type = "contributions", lev1 = T)

# Line plot (formerly barplot, remove L1_ where necessary)
p3 <- ggplot(contr0, aes(x = val, y = Compound)) + 
  geom_segment(aes(x = 0, y = Compound, xend = val, yend = Compound), lineend = "butt") + 
  theme_bw(base_size = 10) + geom_point() + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Relative importance to PC") +
  facet_grid(. ~ PC  , scales = "free_x") + ylab("Relative importance to PC") + 
  scale_y_discrete(label = function(x) str_replace(x, "L1_", "")) +
  theme(legend.position = "none", plot.subtitle = element_text(size = 9),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size = 10),
        panel.spacing = unit(1.5, "lines"))
  
fig2c1 <- p3 %+% contr + scale_x_continuous(breaks = c(-0.05, 0, 0.05)) +
  labs(title = "C", subtitle = "(1) 46 metabolites identified at Level 1-3")
  #ggsave(filename = "Picture 7.png", height = 15, width = 20, units = "cm")

fig2c2 <- p3   + scale_x_continuous(n.breaks = 4) +
  labs(subtitle = "(2) 14 metabolites identified at Level 1")
#ggsave(filename = "Picture 8.png", height = 7, width = 20, units = "cm")

  
# Cowplots to align figs A,B,C
fig2c <- plot_grid(fig2c1, fig2c2, nrow = 2, rel_heights = c(2.6,1))
save_plot("fig2c.tiff", fig2c, nrow = 2, base_height = 5)
  
  
# Not used: the following plots are experimental and not used in the manuscript.

# Follow up time by colour and shape by case control status
ggplot(dfscores, aes(x=PC1, y=PC2, shape=HCC, colour=cats)) + geom_point() + theme_bw() +
  scale_shape_manual(values= c(17,19))

# PCA faceted by follow-up time (see http://www.cookbook-r.com/Graphs/Scatterplots_(ggplot2)/)
ggplot(dfscores, aes(x=PC1, y=PC2, colour=HCC)) + geom_point() + theme_bw() + facet_grid(. ~ cats2) +
  xlab("Score on PC1") + ylab("Score on PC2") + geom_vline(xintercept=0, linetype = "dashed") +
  geom_hline(yintercept=0, linetype = "dashed")

ggplot(contr, aes(x = Compound, y=val, fill=PC)) + geom_bar(stat="identity") + coord_flip() + theme_bw() +
  facet_grid(PC ~ .  , scales = "free_y") + ylab("Relative importance to PC") + 
  xlab("Components contributing most to PC") +
  theme(legend.position = "none")

# Correlation heatmap. Define colour scheme to copy corrplot and plot with pheatmap
cormat <- HCC.figs(type = "correlation")
library(pheatmap)
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582","#FDDBC7", #"#FFFFFF", 
                           "#D1E5F0", "#92C5DE","#4393C3", "#2166AC", "#053061"))

pheatmap(cormat, color = rev(col2(50)), clustering_method = "complete", border_color = "grey60")

library(car)
scatter3d(dfscores$TF2, dfscores$PC1, dfscores$PC2)

library(scatterplot3d)

scatterplot3d(dfscores$TF2, dfscores$PC1, dfscores$PC2)
scatter3d(dfscores$TF2, dfscores$PC1, dfscores$PC2, surface=F)

colours <- c("#E69F00", "#56B4E9")
colours <- colours[as.numeric(dfscores$HCC)]

scatterplot3d(dfscores$TF2, dfscores$PC1, dfscores$PC2, color=colours, pch=16,
              xlab="Follow-up time (years)", ylab="Score on PC1", zlab="Score on PC2")

scatterplot3d(dfscores$PC1, dfscores$PC2, dfscores$TF2, color=colours, pch=16,
              zlab="Follow-up time (years)", xlab="Score on PC1", ylab="Score on PC2")

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
