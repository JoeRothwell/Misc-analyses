library(readxl)
library(tidyverse)

dat <- read_xlsx("All genes normalized reads.xlsx", range = "B1:Y57774")
names <- read_xlsx("All genes normalized reads.xlsx", range = "A1:A57774") %>% pull

# Add gene names
mat <- t(as.matrix(dat))
colnames(mat) <- names


#enlever les colonnes où il n'y a que des 0
vec <- colSums(mat) !=0
sum(vec)
mat1 <- mat[ , vec] 

#on remplace les 0 par la valeur minimum pour ce gène

mat1[mat1 == 0] <- NA

library(zoo)
mat2 <- na.aggregate(mat1, FUN = function(x) min(x))
logmat2 <- log2(mat2)


#il y avait des gènes pourlesquels toutes les valeurs étaient 
#identiques donc on les a enlevés

which(apply(logmat2, 2, var) == 0)
logmat3 <- logmat2[ ,apply(logmat2, 2, var) != 0]
dim(logmat3)
dim(logmat2)



pca <- prcomp(logmat3, scale. = T)

scores <- pca$x[ , 1:3]
scores[ , 1]
plot(scores[,1],scores[,2])

#faire une PCA
cols <- as.factor(c(rep("control", 8), rep("COP", 16) ))
cols
plot(scores[,1],scores[,2], col = cols, pch = 19)
plot(pca)


#pour avoir le pourcentage des variances des composantes
summary(pca)

# Nice PCA
library(pca3d)
pca2d(pca, group = cols, legend = "left")
box(which = "plot", lty = "solid")

# --------------------------------------------------------------------------------------------

# PCA mixomics
pca.copctrl <- pca(logmat3, ncomp = 10, center = T, scale = T)
plot(pca.copctrl)

plotIndiv(pca.copctrl, group = cols, ind.names = FALSE, 
          legend = TRUE, title = 'PCA COP vs CTRL')

# Run PLS-DA specifying number of components
library(mixOmics)
plsda.res <- plsda(logmat3, cols, ncomp = 10)

set.seed(2543) # for reproducibility here, only when the `cpus' argument is not used
perf.plsda <- perf(plsda.res, validation = "Mfold", folds = 5, 
                   progressBar = FALSE, auc = TRUE, nrepeat = 10) 

plot(perf.plsda, col = color.mixo(1:3), sd = TRUE, legend.position = "horizontal")

#coeff <- plsda.res$X
#plot(coeff[1, ])
perf.plsda$choice.ncomp # 3 components appear to be best

# Rerun the PLS-DA with 3 components

plsda.res1 <- plsda(logmat3, cols, ncomp = 3)

plotVar(plsda.res1)

plotIndiv(plsda.res1, ind.names = FALSE, legend = TRUE, ellipse = TRUE,
          title = 'PLS-DA')

plotLoadings(plsda.res1, contrib = "max", ndisplay = 50)

# Prediction

set.seed(999)
predict.class <- predict(plsda.res1, testdata, dist  = "max.dist")

prediction <- predict.class$class$max.dist[, 4]

confusion.mat = get.confusion_matrix(truth = Y[test], predicted = prediction)
get.BER(confusion.mat)




