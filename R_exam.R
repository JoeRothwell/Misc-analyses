# R exam session 2 for code testing

# 1. Nous venons d’installer le package « epitools ». Pour utiliser ses fonctions, on doit utiliser 
# quelle commande ? (based on E2 Q1)
install.packages("epitools")
load("epitools")
library(epitools)
setwd("epitools")

# Comment peut-on afficher une liste de toutes les fonctions du package « metafor » après l’avoir 
# chargé dans la session ? (based on E2 Q2)
??metafor
ls("package:metafor")
library(help=metafor)
help(package=="metafor")

# 3. Les commandes suivantes sont réalisées :
a <- 5:10 ; b <- seq(1, 10, by = 2) ; dat <- rbind(a, b)
dat
# Quelle sortie s’affiche ?

# 4. Quel est le résultat des commandes suivantes
obj <- c(seq(1:3), 4); mean(obj)
2 #  A.
3 #  B.
1 #  C.
2.5 # D.

#5. Il y a combien d'elements dans le vecteur "vec" suivant ?
vec <- seq(0, 10, 0.5)
10 # A
20 # B
21 # C
40 # D

#6. Si on realise le code 
plot(rnorm(100))
# quelle est la sortie ? (get plots of plot, boxplot, barplot, hist)

#5. Nous souhaitons sélectionner aléatoirement 300 individus parmi notre base de données
# « data » de 15332 sujets. La sélection est stockée dans « data300 ». Quel code pouvonsnous utiliser ?
data300 <- data[ ,1:300] # A. 
data300 <- data[1:300, ] # B. 
data300 <- data[sample(1 : nrow(data), 300), ] # C. 
data300 <- subset(data, row = 1:300) # D. 


#5. Nous souhaitons importer le RData « dat_examen » sauvegardé lors d’une précédente
#session. Il se trouve dans le même dossier que celui utilisé pour définir le répertoire de
#travail de la session actuelle. Quel code pouvons-nous utiliser ?
load(dat_examen.RData) # A.
save(file = dat_examen.RData) # B.
save("dat_examen.RData") # C.
load(file = "dat_examen.RData") # D.


# Le fichier « Final_synthetic_data2.txt » est le fichier de données que nous souhaitons
# importer dans R. Les séparateurs des colonnes sont des tabulations, les données
# manquantes sont codées vides et les séparateurs de décimales sont des points. La
# première ligne du fichier contient les noms des variables. Quelles commandes pouvons nous 
# utiliser pour importer ce fichier dans R et le stocker dans l’objet « data » ? (plusieurs réponses attendues)
                                                                                                                                                                    
data <- read.delim("Final_synthetic_data2.txt") # A. 
data <- read.csv("Final_synthetic_data2.txt") # B.
data <- read.table("Final_synthetic_data2.txt", sep="\t", na.strings = "", header=TRUE) # C.
data <- read.txt("Final_synthetic_data2.txt", sep="\t", na.strings = "", header=TRUE) # D. 
                                                                                                                                                                         
                                                                                                                                                                         
# 7. Nous travaillons sur une étude impliquant 30 sujets chez lesquels nous avons recueillis
# 25 caractéristiques. Les données sont stockées dans un fichier excel que nous avons
# importé dans R et stocké dans l’objet « data_v2 ». L’utilisation du code « dim(data_v2) »
# nous renvoie la sortie suivante :

# Que pouvons-nous en déduire ?
# A. L’import des données s’est passé correctement
# B. Nous avons oublié de spécifier que la première ligne du tableau à importer contient les noms des variables
# C. Nous avons commis une erreur sur le séparateur utilisé entre chaque cellule du tableau
# D. Nous avons mal précisé comment sont codées les données manquantes dans notre tableau
                                                                                                                                                                         
                                                                                                                                                                         
#8. Nous souhaitons sélectionner aléatoirement 300 individus parmi notre base de données
# « data » de 15332 sujets. La sélection est stockée dans « data300 ». Quel code pouvons nous utiliser ?
data300 <- data[ ,1:300] # A. 
data300 <- data[1:300, ] # B. 
data300 <- data[sample(1 : nrow(data), 300), ] # C. 
data300 <- subset(data, row = 1:300) # D. 

# 9. Nous disposons dans « data » de cinq variables (v5p1, v5p2, v5p3, v5p4, v5p5) ayant
# chacune pour modalités : 1 pour jamais, 2 pour de temps en temps, 3 pour souvent et 4
# pour très souvent. Nous souhaitons ajouter à notre base de données « data » une
# nouvelle variable « resum_sommeil » résumant l’information, valant « diff_sommeil » si la
# somme des scores des cinq variables est supérieure ou égale à 15 et « nodiff_sommeil »
# sinon, avec la classe « nodiff_sommeil » reconnue comme la référence. Aucune donnée
# manquante n’est présente dans le jeu de données. Quel code pouvons-nous utiliser ?

data$resum_sommeil <- 
 # relevel(factor(ifelse(apply(data[ ,c("v5p1","v5p2","v5p3","v5p4","v5p5")], 1, sum) < 15, 
  # nodiff_sommeil","diff_sommeil")), ref="nodiff_sommeil")
data$resum_sommeil <- 
  relevel(factor(ifelse(apply(data[ ,c("v5p1","v5p2","v5p3","v5p4","v5p5")], 1, sum) < 15, 
                        "nodiff_sommeil","diff_sommeil"), ref="nodiff_sommeil"))
data$resum_sommeil <- 
  ifelse(sum(c(data$v5p1, data$v5p2, data$v5p3, data$v5p4, data$v5p5)) < 15, 
         "nodiff_sommeil","diff_sommeil")
data$resum_sommeil <- 
  ifelse(sum(c(data$v5p1, data$v5p2, data$v5p3, data$v5p4, data$v5p5)) >= 15, 
         "diff_sommeil","nodiff_sommeil")
                                                                                                                                                                         
                                                                                                                                                                         
# 10. Nous disposons de deux bases de données « data1 » et « data2 » avec dans chacune
# les mêmes individus identifiés par la variable « ID ». La base « data1 » contient 9 variables
# et la base « data2 » 19 variables. Les variables « Age » et « DEP » sont communes aux
# deux tables. Quels codes pouvons-nous utiliser pour combiner ces deux bases en une
# seule nommée « dataTot » sans avoir d’information redondante ? (plusieurs réponses attendues)
dataTot <- merge(data1, data2) # A. 
dataTot <- merge(data1, data2, by = "ID") # B.
dataTot <- merge(data1, data2, by = c("ID", "Age", "DEP")) # C.
dataTot <- merge(data1, data2, by = "ID", all.x = TRUE) # D.
                                                                                                                                                                         
# 11. Nous disposons dans la base de données d’une variable « réveil_noct » quantifiant la
# fréquence de survenue des réveils nocturnes. Voici la représentation graphique associée
# à cette variable :
                                                                                                                                                                         
# 11. Si on realise le code 
plot(rnorm(100))
# quelle est la sortie ? (get plots of plot, boxplot, barplot, hist)

# 12. Nous disposons de la base de données « polyp » dans laquelle nous avons de differentes types de variable.
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

output
#A. La base de données « polyp » contient 22 observations
#B. Il y a des donnée manquantes dans la base de donnees
#C. La variable « number12m » est de type numerique
#D. L'age moyenne est 22.00 ans

# 13. Nous disposons des figures suivantes :
# (4 histograms)
# Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A. Ces figures ont pu être générées grâce à l’utilisation de la fonction norm()
# B. Toutes les distributions sont normales
# C. La figure 3 est la distribution ayant le plus petit écart-type
# D. La figure 3 est la distribution ayant le plus grand écart-type


