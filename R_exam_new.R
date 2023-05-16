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
library(aplore3)


# 2x2 table mobile phone use and melanoma
matrix(c(136, 107, 297, 165), dimnames = list(c("UM+", "UM-"), c("Mobile+", "Mobile-")))


### Questions


### Basic R use

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

# Nous souhaitons supprimer l'ensemble des variables dans le workspace. On realise quelle commande ?
ls() # A
rm(list = ls()) # B
ls(list = rm()) # C
getwd() # D


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

#A*. La base de données « polyp » contient 22 observations
#B. Il n'y pas de données manquantes dans cette base de donnees
#C*. La variable « number12m » est de type numerique
#D*. L'age moyenne des participants est 24.09 ans


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
polyp.sel <- polyps[ , polyps$age > 20 & polyps$treatment == "sulindac"]



# Q. Dans la base de donnees polyps, nous nous interessons si le numero de polyps constates au debut de l'etude est 
# different selon le groupe de traitement (placebo ou sulindac)
# Nous disposons de la sortie suivante :
summary(chisq.test(baseline ~ treatment, data = polyps))

# data:  baseline by treatment
# t = 0.85384, df = 14.6, p-value = 0.407
# alternative hypothesis: true difference in means between group placebo and group sulindac is not equal to 0
# 95 percent confidence interval:
#   -38.92272  90.74090
# sample estimates:
#   mean in group placebo mean in group sulindac 
# 53.90909               28.00000 


# Quelle affirmation est exacte ? (plusieurs reponses attendues)
# A. Le test realise ici c'est l'ANOVA
# B. L'hypothese nulle du test c'est que il n'y a pas de difference entre les groupes de traitement
# C. On rejette l'hypothese null pour inferer qu'il y a un difference entre les deux groupes
# melanoma et l'usage des telephones portables
# D. Selon le test, la moyenne dans le group placebo est plus eleve que le moyenne dans le groupe sulindac


# 9. Si on realise le code 
plot(norm(100))
# quelle est la sortie  ? (get plots of plot, boxplot, barplot, hist)


### Statistical tests


exam <- read.delim("Exam Anxiety.dat")
# 10. Chez des lyceens, nous nous intéressons au lien entre le note d'en examen (variable "exam"), 
# un score d'anxiete ("anxiety") et 
# le sexe (genre). On souhaite tester le lien entre le "exam" et "anxiete". Quelle commande peut-on utiliser ?
t.test()
cor.test()
anova()
cor()


# 11. En utilisant les memes donnees "exam", nous avons la sortie suivante :
t.test(Anxiety ~ Gender, dat = exam)

# data:  Anxiety by Gender
# t = -0.023754, df = 99.662, p-value = 0.9811
# alternative hypothesis: true difference in means between group Female and group Male is not equal to 0
# 95 percent confidence interval:
#   -6.838685  6.676870
# sample estimates:
# mean in group Female   mean in group Male 
# 74.30282             74.38373 

# A.* L’hypothèse nulle du test statistique adéquat est l’absence de lien entre le sexe et l'anxiete
# B. D'apres ce test, on peut concluir qu'il y avait plus d'anxiete chez les filles que chez 
# les garcons dans cet echantillon
# C. Le test statistique realise a ete l'ANOVA
# D. Aucune réponse n’est exacte


# Q. Une etude a constate que 35% des lyceens sont atteints de l'anxiete avant leurs examens. Nous
# souhaitons tester cette hypothese dans un echantillon de 200 lyceens auxquelles on a demande s'ils avait l'anxiete
# avant leurs examens ou non. Quelle fonction est plus pertinant pour pouvoir repondre a cette question ?
t.test()
anova()
binom.test()
chisq.test()




### Epidemiology


# Q. Nous disposons des donnees d'une etude sur l'association entre la fumee et l'hypertension. Au debut de l'etude,
# les subjets ont declare s'ils etaient fumeurs ou pas et puis ils ont declare tout diagnostic d'hypertension
# suivant. La periode de temps avant le diagnostic n'a pas ete declare pas les individus. 
# Quelle mesure d'association est pertinante ?
# Odds ratio
# Rate ratio
#* Risk ratio
# Hazards ratio


# Q Nous disposons des données d’une étude cas-témoins chez 705 participants sur le melanoma et l'exposition 
# aux telephones portables chez 705 participants. Les cas et les temoins sont codes UM+ et UM- et l'usage
# telephone portable est code Mobile+ et Mobile-. Nous disposons de la sortie suivante suite à l’utilisation 
# de la fonction epitab() :
epitab(melanoma, method = "oddsratio", rev = "both")
# [Note: expected structure exposure level 1, 2, disease no, yes]

# $tab
#     Mobile-        p0 Mobile+        p1 oddsratio     lower     upper    p.value
# UM-     165 0.3571429     107 0.4403292 1.0000000        NA        NA         NA
# UM+     297 0.6428571     136 0.5596708 0.7061267 0.5143958 0.9693215 0.03436989
# 
# $measure
# [1] "wald"
# 
# $conf.level
# [1] 0.95
# 
# $pvalue
# [1] "fisher.exact"

# Quelle affirmation est exacte ?
# A. Le risque de melanoma est multiplié par 0.706 pour l'usage telephone portable par rapport au non-usage
# B. L’intervalle de confiance à 95% de l’OR contient 1
# C. Au risque 5%, on ne met pas en évidence de lien entre le risque de
# melanoma et l'usage des telephones portables
# D. Le test realise ici n'est pas pertinent


### GLM and LM

# Q. Nous disposons des données d’une étude cas-témoins sur le myope. Nous nous intéressons au lien entre le presence ou
# l'absence de myope (variable « MYOPIC », codée 1 si l’individu est MYOPE et 0 si l'individu n'est pas myopie),
# et la refraction spherique equivalente mesure pour chaque individu (variable SPHEQ, en continue).

# Quelle affirmation est exact ?
# Si on cherche a tester si SPHEQ  est un facteur de risque pour le myope, le modele de regression a utiliser est un 
# modele de Cox
# modele lineaire
# modele logistique *
# Aucun des modeles des reponses precedentes n'est adaptes



# Q. Nous disposons de la sortie suivante :
summary(glm(MYOPIC ~ SPHEQ, data = myopia, family = "binomial"))

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
# A* La fonction glm() a pu être utilisée pour obtenir ces résultats
# B*. Au risque 5%, il existe un lien significatif entre la myope et SPHEQ
# C. Au risque 5%, on montre une aumentation du risque de myope chez les individus
# pour lesquels SPHEQ est plus eleve
# D*. L’intervalle de confiance à 95% de l’OR mesurant l’association entre la myope et
# SPHEQ ne contient pas la valeur 1


# Nous nous rendons compte qu'il fallait ajuster sur l'age dans notre modele myope.
# Maintenant nous disposons de la sortie suivante : 

# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.7277  -0.4552  -0.2677  -0.0979   3.1643  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   2.2104     1.2141   1.821   0.0687 .  
# SPHEQ        -3.9603     0.4273  -9.268   <2e-16 ***
#   AGE        -0.3337     0.1852  -1.802   0.0716 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 480.08  on 617  degrees of freedom
# Residual deviance: 333.93  on 615  degrees of freedom
# AIC: 339.93

#Number of Fisher Scoring iterations: 6

# Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A Apres avoir ajuste sur l'age, l'association entre le myope et SPHEQ n'est plus significative 
# B*. Au risque 5%, l'age n'est pas un facteur de risque pour le myope si on prend en compte SPHEQ
# C. Dans cet example, ce n'etait pas pertinant d'ajouter l'age dans le modele
# D.  Aucune réponse n’est exacte


