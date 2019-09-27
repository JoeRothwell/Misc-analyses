dfmat <- cbind(df, scale(mat))
head(dfmat)
library(reshape2)
meltdf <- melt(dfmat)


library(ggplot2)
ggplot(meltdf, aes(x = variable, y = gene, fill = value)) + 
  geom_tile() +
  facet_grid(abrev ~ ., scales = "free_y", space = "free_y") + theme_minimal() +
  scale_fill_gradient(low = "green", high = "red")



ggplot(meltdf, aes(x = variable, y = gene, fill = value)) + geom_tile() +
  facet_wrap(abrev ~ ., ncol = 3, scales = "free_y") + theme_minimal() +
  scale_fill_gradient(
    low = "green",
    high = "red")
  
