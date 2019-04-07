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

#------------------------------------------------------------------------------------------------

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