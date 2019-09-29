#Making heatmap colour palettes 
#(from http://www.r-bloggers.com/heatmaps-controlling-the-color-representation-with-set-data-range/)

library(gplots)

# create 50x10 matrix of random values from -1 to 1
random.mat  <- matrix(runif(500, min = -1, max = 1), nrow = 50)

# limit the lowest and highest color to 5%, and 95% of your range, respectively
quantile.range <- quantile(random.mat, probs = seq(0, 1, 0.01))
palette.breaks <- seq(quantile.range["5%"], quantile.range["95%"], 0.1)

# use http://colorbrewer2.org/ to find optimal divergent color palette (or set own)
# eg green-pink, red-blue

colpalette1 <- colorRampPalette(c("#FC8D59", "#FFFFBF", "#91CF60"))(length(palette.breaks) - 1)
colpalette2 <- colorRampPalette(c('#ef8a62','#f7f7f7','#67a9cf'))(length(palette.breaks) - 1)

heatmap.2(random.mat, dendrogram = "row", scale = "none", trace = "none",
  #key = F, labRow = NA, labCol = NA,
  col    = colpalette2,
  breaks = palette.breaks
)

# With ggplot2
library(reshape2)
library(ggplot2)
df <- melt(random.mat, value.name = "Intensity")

ggplot(df, aes(as.factor(Var2), Var1, fill = Intensity)) + 
  geom_tile() + theme_minimal() +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradientn(colours = colpalette2)
  #scale_fill_gradient2(low = "#FC8D59", high = "#91CF60")

# SO example
qrange <- quantile(mat, probs = seq(0, 1, 0.01))
palette.breaks <- seq(qrange["5%"], qrange["95%"], 0.1)
colpalette3 <- colorRampPalette(c("#FC8D59", "#FFFFBF", "#91CF60"))(length(palette.breaks) - 1)
mat[mat == -5.4] <- NA


dfmat <- cbind(df, mat)
meltdf <- melt(dfmat)
ggplot(meltdf, aes(x = variable, y = gene, fill = value)) + 
  geom_tile() +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  facet_grid(abrev ~ ., scales = "free_y", space = "free_y") + theme_minimal() +
  scale_fill_gradientn(colours = colpalette3, na.value = "white")

ggplot(meltdf, aes(x = variable, y = gene, fill = value)) + geom_tile() +
  facet_wrap(abrev ~ ., ncol = 3, scales = "free_y") + theme_minimal() + 
  scale_fill_gradientn(colours = colpalette3)

# Correlation heatmap
# Will's meatmap
meat <- read.csv("will meat cmpds.csv")
library(corrplot)
cmat.meat <- cor(meat, method="pearson")
rownames(cmat) <- cmpdnames
corrplot(cmat.meat, method="square", tl.col="black", order="hclust", tl.cex=0.75, 
         cl.ratio=0.2, cl.align="l", cl.pos="r", cl.cex=0.6, mar=c(1,1,1,1))

# SO example ggplot