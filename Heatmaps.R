#Making heatmap colour palettes 
#(from http://www.r-bloggers.com/heatmaps-controlling-the-color-representation-with-set-data-range/)

# gplots contains the heatmap.2 function
library(gplots)

# create 50x10 matrix of random values from [-1, +1]
random.matrix  <- matrix(runif(500, min = -1, max = 1), nrow = 50)

# following code limits the lowest and highest color to 5%, and 95% of your range, respectively
quantile.range <- quantile(random.matrix, probs = seq(0, 1, 0.01))
palette.breaks <- seq(quantile.range["5%"], quantile.range["95%"], 0.1)

# use http://colorbrewer2.org/ to find optimal divergent color palette (or set own)
color.palette  <- colorRampPalette(c("#FC8D59", "#FFFFBF", "#91CF60"))(length(palette.breaks) - 1)

heatmap.2(
  
  random.matrix,
  
  dendrogram = "row",
  scale      = "none",
  trace      = "none",
  key        = FALSE,
  labRow     = NA,
  labCol     = NA,
  
  col    = color.palette,
  breaks = palette.breaks
)

# Will-----------------------------------------------------------------------------------------------

#Correlation heatmap
#Will's meatmap
meat <- read.csv("will meat cmpds.csv")
library(corrplot)
cmat.meat <- cor(meat, method="pearson")
rownames(cmat) <- cmpdnames
corrplot(cmat.meat, 
         method="square", 
         tl.col="black", 
         order="hclust", 
         tl.cex=0.75, 
         cl.ratio=0.2, 
         cl.align="l",
         cl.pos="r", 
         cl.cex=0.6, 
         mar=c(1,1,1,1))

# Vibha-------------------------------------------------------------------------------------------

library(tidyverse)
#Read in data from csv
after  <- read.csv("dfl2000.csv") #most differentially expressed probes from before intervention
before <- read.csv("dfo2000.csv") #most differentially expressed probes from after intervention

#calculate overlap between probes
library(tidyverse)
common <- before %>% inner_join(after, by = "X") #572 probes common
all    <- before %>% full_join(after, by = "X")  #3428 probes total


#all.mat <- column_to_rownames(all, var = "X") %>% arrange(X) %>% as.matrix
all.mat <- select(all, before = x.x, after = x.y) %>% as.matrix %>% t

library(gplots)
heatmap.2(all.mat, Rowv = F, Colv = F, labRow = NULL, trace="none", dendrogram = "none")
heatmap(all.mat)

# Elom--------------------------------------------------------------------------------------------

heat <- read.csv("heatmap Elom.csv")
mat <- data.matrix(heat)
cormat <- cor(mat, use = "pairwise.complete.obs")

plasma <- mat[, 1:6]
diet   <- mat[, 7:12]
cormat <- cor(plasma, diet)

#Heatmap 1
corrplot(cormat, method = "color", tl.col="black", order="original", tl.cex=0.8)

#Heatmap 2
corrplot(cormat, method = "number", tl.col="black", order="original", tl.cex=0.8)

library(gplots)
par(mar=c(0, 0, 0, 5))
heatmap.2(cormat, trace="none", col=redblue(256), Rowv = F, Colv = F, key = F)
dev.off()

