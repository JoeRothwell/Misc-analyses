souce("wristband_study.R")

# Correlation heatmap
cormat <- cor(logmat)
colnames(cormat) <- NULL
library(corrplot)
corrplot(cormat, method = "square", tl.col = "black", tl.cex = 0.8, order = "hclust")

# Individual PCAs: France
pca <- prcomp(logmat, scale. = T)
#pca2 <- prcomp(logmat2, scale. = T)
place.day1 <- factor(meta$wristbands_urban_d1, labels = c("Rural", "Urban"))
place.day2 <- factor(meta$wristbands_urban_d2, labels = c("Rural", "Urban"))
place.day3 <- factor(meta$wristbands_urban_d3, labels = c("Rural", "Urban"))
place.day4 <- factor(meta$wristbands_urban_d4, labels = c("Rural", "Urban"))
couple <- as.factor(meta$num_couple)
id <- meta$Identifiant
sex <- c(rep("M", 20), rep("F", 20))

library(pca3d)
pca2d(pca, group = sex, legend = "topright", axe.titles = c("Score on PC1", "Score on PC2"))
title("France, by sex")
box()

# Individual PCA Italy
sex <- meta1$Gender
pca0 <- prcomp(logmat0, scale. = T)
pca2d(pca0, group = sex, legend = "topright", axe.titles = c("Score on PC1", "Score on PC2"))
title("Italy, by sex")
box()

# Groups for both countries together
sex1 <- c(sex, as.character(meta1$Gender))
country <- c(rep("France", 40), rep("Italy", 31))
pca2 <- prcomp(logmat2, scale. = T)
pca2d(pca2, group = country, legend = "topright", axe.titles = c("Score on PC1", "Score on PC2"))
title("All, by country")
box()

# With purrr
#meta %>% select(contains("wristbands_u")) %>% map(function(x) factor(x, labels = c("Rural", "Urban")))

# PCAs by different characteristics
library(pca3d)
#pca2d(pca)
pca2d(pca2, group = country)
title("Profiles by country, 40 compounds")
box()

plot(pca2)
print(summary(pca2))

aload <- pca$rotation
contr <- sweep(aload, 2, colSums(aload), "/") %>% data.frame %>% 
  rownames_to_column(var="cpd.code")

pca2d(pca, group = sex1)
title("Profiles by Sex")
box()


library(pca3d)
par(mfrow=c(1,2))

pca2d(pca, group = place.day1, legend = "topright", axe.titles = c("Score on PC1", "Score on PC2"))
title("Place, day 1")
box()

pca2d(pca, col = "grey", show.labels = couple, axe.titles = c("Score on PC1", "Score on PC2"))
title("Couples in study")
box()

pca2d(pca, group = sex, legend = "topright", axe.titles = c("Score on PC1", "Score on PC2"))
title("Participant gender")
box()

# Biplot
dev.off()
pca2d(pca, biplot = T, axe.titles = c("Score on PC1", "Score on PC2"))
title("PCA biplot")
box()

# Summarise compounds
mat3 <- na.aggregate(mat, FUN = min)
meds <- apply(mat3, 2, median)
boxplot(log10(meds), horizontal = T)

# Plot median intensities
plot(log10(meds), xlab = "", ylab = "Concentration (log10 ng/g)", col = "white")
text(log10(meds), labels = dat$sample_id, cex = 0.8)

# Dotchart
dotchart(log10(meds), labels = cmpd.meta$Cmpd, groups = cmpd.meta$Cmpd_group, 
         xlab = "Median concentration log10(ng)/g", cex = 0.7, gcolor = "red",
         main = "Compound concentrations")

# Scatter
plot(log10(meds), xlab = "", ylab = "Concentration (log10 ng/g)", col = "white")
text(log10(meds), labels = dat$sample_id, cex = 0.8)