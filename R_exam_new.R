# New R exam
# Datasets to be used

# Exam anxiety dataset, correlation and t-test
library(medicaldata)
polyps
esoph_ca
library(faraway)
diabetes
exam <- read.delim("Exam Anxiety.dat")
myopia <- read.csv("MYOPIA-fixed.csv")


### Questions

# 1. Nous venons d’installer le package « epitools ». Pour utiliser ses fonctions, on doit utiliser 
# quelle commande ? (based on E2 Q1)
install.packages("epitools")
load("epitools")
library(epitools)
setwd("epitools")

# 2. Comment peut-on afficher une liste de toutes les fonctions du package « metafor » après l’avoir 
# chargé dans la session ? (based on E2 Q2)
??metafor
ls("package:metafor")
library(help=metafor)
help(package=="metafor")

# 3. Les commandes suivantes sont réalisées :
a <- 5:10 ; b <- seq(1, 10, by = 2)
at <- rbind(a, b)
dat
# Quelle sortie s’affiche ?

# 4. Quel est le résultat des commandes suivantes ?
obj <- c(seq(1:3), 4)
mean(obj)
2 #  A.
3 #  B.
1 #  C.
2.5 # D.

# 5. Il y a combien d'elements dans le vecteur "vec" suivant ?
vec <- seq(0, 10, 0.5)
10 # A
20 # B
21 # C
40 # D


# 6. Nous avons importe notre base de donnees « data1 » dans R dans un objet de classe data.frame. 
# Pour afficher la classe de chaque variable, on realise quelle commande ?
summary(data1)
str(data1)
colnames(data1)
lapply(data1, length)

# 7. Nous disposons de la base de données « polyp » dans laquelle nous avons de differentes types de variable.
# Que nous apprend la sortie suivante ? (plusieurs réponses attendues)

summary(polyps)
#participant_id         sex          age           baseline         treatment     number3m        number12m    
#Length:22          female: 9   Min.   :13.00   Min.   :  5.00   placebo :11   Min.   :  1.00   Min.   : 1.00  
#Class :character   male  :13   1st Qu.:18.25   1st Qu.: 10.25   sulindac:11   1st Qu.:  6.00   1st Qu.: 3.75  
#Mode  :character               Median :22.00   Median : 18.00                 Median : 16.00   Median :21.00  
#                               Mean   :24.09   Mean   : 40.95                 Mean   : 38.41   Mean   :24.05  
#                               3rd Qu.:26.00   3rd Qu.: 33.00                 3rd Qu.: 29.25   3rd Qu.:41.00  
#                               Max.   :50.00   Max.   :318.00                 Max.   :347.00   Max.   :63.00  
#                                                                                               NA's   :2

#A. La base de données « polyp » contient 22 observations
#B. Il n'y pas de données manquantes dans cette base de donnees
#C. La variable « number12m » est de type numerique
#D. L'age moyenne des participants est 22.00 ans


# 8. Dans la meme base de donnees polyps, nous souhaitons mettre on ordre les lignes selon le colonne "baseline" (ascendant). 
# Nous pouvons utiliser quelle command pour realiser cela et le stocker dans un objet polyps.ord ?
polyps.ord <- polyps[ , order(polyps$baseline)]
polyps.ord <- reorder(polyps, polyps$baseline)
polyps.ord <- order(polyps[, "baseline"])
polyps.ord <- polyps[order(polyps$baseline), ]


# Q. A partir de la base de donnes polyps, on souhaite selectionner seulement les sujets ages de plus de 20 ans et
# pour qui le traitement a ete le medicament sulindac. 
# Quelle commande peut-en utiliser ? La selection sera stockee dans polyp.sel.
polyp.sel <- subset(polyps, age > 20 & baseline == "sulindac")
polyp.sel <- subset(polyps, age > 20 | treatment == "sulindac")
polyp.sel <- polyps[polyps$age > 20 & polyps$treatment == "sulindac", ]
polyp.sel <- polyps[ ,polyps$age > 20 & polyps$treatment == "sulindac"]

# 9. Si on realise le code 
plot(norm(100))
# quelle est la sortie  ? (get plots of plot, boxplot, barplot, hist)


exam <- read.delim("Exam Anxiety.dat")
# 10. Chez des lyceens, nous nous intéressons au lien entre le note d'en examen (variable "exam"), 
# un score d'anxiete ("anxiety") et 
# le sexe (genre). On souhaite tester le lien entre le "exam" et "anxiete". Quelle commande peut-on utiliser ?
t.test()
cor.test()
anova()
cor()

# 11. En utilisant les memes donnees, nous avons la sortie suivante :
t.test(Anxiety ~ Gender, dat = exam)

# data:  Anxiety by Gender
# t = -0.023754, df = 99.662, p-value = 0.9811
# alternative hypothesis: true difference in means between group Female and group Male is not equal to 0
# 95 percent confidence interval:
#   -6.838685  6.676870
# sample estimates:
#   mean in group Female   mean in group Male 
# 74.30282             74.38373 

# A.* L’hypothèse nulle du test statistique adéquat est l’absence de lien entre le sexe et l'anxiete
# B. D'apres ce test, on peut concluir qu'il y avait plus d'anxiete chez les filles que chez 
# les garcons dans cet echantillon
# C. Le test statistique realise a ete l'ANOVA
# D. Aucune réponse n’est exacte









# Q. Nous disposons des données d’une étude cas-témoins sur le myope. Nous nous intéressons au lien entre le presence ou
# l'absence de myope (variable « MYOPIC », codée 1 si l’individu est MYOPE et 0 si l'individu n'est pas myopie,
# et la variable en continue SPHEQ. Nous disposons de la sortie suivante :

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.6435  -0.4533  -0.2681  -0.1029   3.1602  

# Coefficients:
#              Estimate  Std. Error z value Pr(>|z|)    
# (Intercept)  0.05397    0.20675   0.261    0.794    
# SPHEQ       -3.83310    0.41837  -9.162   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 480.08  on 617  degrees of freedom
# Residual deviance: 337.34  on 616  degrees of freedom
# AIC: 341.34

# Number of Fisher Scoring iterations: 6


# Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A*. Au risque 5%, il existe un lien significatif entre la myope et SPHEQ
# A. Au risque 5%, on montre une aumentation du risque de myope chez les individus
# pour lesquels SPHEQ est plus eleve
# B. La fonction glm() a pu être utilisée pour obtenir ces résultats
# D. L’intervalle de confiance à 95% de l’OR mesurant l’association entre la myope et
# SPHEQ ne contient pas la valeur 1

