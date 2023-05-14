# R exam session 2 for code testing

load()
# 1.1 Quelle fonction doit être utilisée pour charger un fichier « .Rdata » dans l’environnement ?
setwd()
load() # B*.
names()
getwd()

help()
# 1.2. Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A*. L’utilisation de la commande ??glm affichera les différentes pages d’aide où apparait le texte « glm »
# B. L’utilisation de la commande ??glm affichera un message d’erreur
# C. L’utilisation de la commande ??glm affichera l’aide de la fonction glm() si cette fonction existe
# D*. L’utilisation de la commande ?glm affichera l’aide la fonction glm() si cette fonction existe

output
# 1.3. Les commandes suivantes sont réalisées : x <- 1:4 ; y <- 2*x ; z <- sum(x) ; w <- rbind(y,z) ;
# w. Quelle sortie s’affiche ?
# A. B. C*. D.

read.table()
# 1.5. Le fichier « donnees_prison.csv » est le fichier de données que l’on souhaite importer
# dans R. Les séparateurs des colonnes sont des virgules, les données manquantes sont
# codées vides et les séparateurs de décimales sont des points. La première ligne du fichier
# contient les noms des variables. Quelle commande faut-il utiliser pour importer ce fichier
# dans R et le stocker dans l’objet « data » ?
data <- read.csv("donnees_prison.csv", sep=".", na="", header=TRUE)
data <- read.table("donnees_prison.csv",sep=".",na.strings="NA",header=TRUE)
data <- read.txt("données_prison.csv", sep=",", na.strings=NA, header=TRUE)
data <- read.table("donnees_prison.csv",sep=",",na.strings="",header=TRUE) # 

str()
# 1.6. Quelles fonctions peuvent-elles utilisées pour obtenir un aperçu de la structure et des
# données contenues dans un objet ? (plusieurs réponses attendues)
str() # A*.
dim()
summary()
class()

install.packages()
# 2.1. Après une réinstallation de RStudio, nous souhaitons utiliser les fonctions du package
# « Ball ». Quel code doit-on utiliser ?
# A. install.packages("Ball")
# B. library(help=Ball)
# C. ??Ball
# D. Autre code

# 2.2. Comment peut-on disposer de la liste de l’ensemble des fonctions du package
# « frailtypack » après avoir chargé celui-ci dans la session ?
# A. library(help=frailtypack)
# B. ls("package:frailtypack")
# C. ??frailtypack
# D. help(package=="frailtypack")

# 2.3. Les commandes suivantes sont réalisées :
# age <- c(33,56,76) ; poids <- c(78,98,65) ; data <- cbind(age,poids) ; data
# Quelle sortie s’affiche ?
# A. B.
# C. D.

# 2.4. Quel est le résultat des commandes suivantes : obj <- seq(1,9,1) ; mean(obj) ?
# A. 5
# B. 0
# C. 9
# D. NA

# 2.5. Nous souhaitons importer le RData « dat_examen » sauvegardé lors d’une précédente
# session. Il se trouve dans le même dossier que celui utilisé pour définir le répertoire de
# travail de la session actuelle. Quel code pouvons-nous utiliser ?
load(dat_examen.RData) # A
save(file=dat_examen.RData)
save("dat_examen.RData")
load(file="dat_examen.RData")

read.table()
# 2.6 Le fichier « Final_synthetic_data2.txt » est le fichier de données que nous souhaitons
# importer dans R. Les séparateurs des colonnes sont des tabulations, les données
# manquantes sont codées vides et les séparateurs de décimales sont des points. La
# première ligne du fichier contient les noms des variables. Quelles commandes pouvonsnous utiliser pour 
# importer ce fichier dans R et le stocker dans l’objet « data » ? (plusieurs réponses attendues)
data <- read.delim("Final_synthetic_data2.txt")  # A *
data <- read.csv("Final_synthetic_data2.txt")
data <- read.table("Final_synthetic_data2.txt",sep="\t",na.strings="",header=TRUE) # C *
data <- read.txt("Final_synthetic_data2.txt", sep="\t",na.strings="",header=TRUE)

dim()
# 2.7. Nous travaillons sur une étude impliquant 30 sujets chez lesquels nous avons recueillis
# 25 caractéristiques. Les données sont stockées dans un fichier excel que nous avons
# importé dans R et stocké dans l’objet « data_v2 ». L’utilisation du code « dim(data_v2) »
# nous renvoie la sortie suivante :
# Que pouvons-nous en déduire ?
# A. L’import des données s’est passé correctement
# B. Nous avons oublié de spécifier que la première ligne du tableau à importer contient les noms des variables
# C. Nous avons commis une erreur sur le séparateur utilisé entre chaque cellule du tableau
# D. Nous avons mal précisé comment sont codées les données manquantes dans notre tableau

sample()
# 2.8. Nous souhaitons sélectionner aléatoirement 300 individus parmi notre base de données
# « data » de 15332 sujets. La sélection est stockée dans « data300 ». Quel code pouvons nous utiliser ?
# A. data300 <- data[ ,1:300]
# B. data300 <- data[1:300, ]
# C. data300 <- data[sample(1:nrow(data), 300), ]
# D. data300 <- subset(data, row = 1:300)


var
# 1.7. Dans la base de données « data », pour chaque individu, on dispose de l’heure de début
# de l’entretien : « h.deb.interv » ; et des minutes associées : « m.deb.interv » ; de l’heure
# de fin de l’entretien : « h.fin.interv » ; et des minutes associées : « m.fin.interv ». Quelle
# commande peut-on utiliser pour calculer la durée de l’entretien (variable « t.deb ») pour
# chaque sujet en minutes ?

data$t.deb3 <- ifelse((data$h.fin.interv - data$h.deb.interv) = 0, data$m.fin.interv - data$m.deb.interv,
  ifelse((data$h.fin.interv - data$h.deb.interv) > 0,
  ((data$h.fin.interv-data$h.deb.interv) * 60 + (data$m.fin.intervdata$m.deb.interv)), NA)) # A*

data$t.deb <- ((data$h.fin.interv + data$m.fin.interv/60) - (data$h.deb.interv + data$m.deb.interv/60)) # B*
data$t.deb <- (data$h.fin.interv - data$h.deb.interv) * 60 + (data$m.fin.interv - data$m.deb.interv) # C*
# D. Aucune des commandes proposées n’est correcte

as.factor()
# 1.8. La variable « gravite » est une variable de type numérique allant de 1 à 7 dans la base de
# données « data ». Il s’agit d’un score mesurant la gravité de la maladie psychiatrique des
# sujets. Quelles commandes peuvent-elles utilisées pour transformer cette variable en
# facteur tout en créant la nouvelle variable « gravite.cl » ? (plusieurs réponses attendues)
data$gravite.cl <- as.factor(data$gravite) # A*. 
data$gravite.cl <- factor(data$gravite) # B*. 
data$gravite.cl <- labels(data$gravite, labels=c(1:7))
data$gravite.cl <- relevel(as.factor(data$gravite))

strptime()
# 1.9. Dans la base de données « data », il existe trois variables représentant l’année, le mois,
# et le jour de greffe des sujets, respectivement « SOR_ANN », « SOR_MOI », et
# « SOR_DD ». La variable « date_GRF » est la date de greffe complète du sujet. Quelles
# affirmations sont exactes ? (plusieurs réponses attendues)
# A*. Pour créer la date de greffe des sujets à partir des trois variables « SOR_ANN »,
# « SOR_MOI », et « SOR_DD », on peut utiliser la fonction strptime()
# B*. Pour afficher en sortie dans la console la date de greffe du sujet n°5, on peut utiliser
# la commande data[5,"date_GRF"]
# C*. Pour connaitre le délai entre la plus ancienne date de greffe et la plus récente dans
# la base, on peut utiliser la commande 
max(data$date_GRF,na.rm=TRUE) - min(data$date_GRF,na.rm=TRUE)
# D*. Les fonctions min() et max() ne doivent pas être utilisées pour travailler sur des variables de type date


factor
# 1.10. On dispose dans la base de données « data » du nombre d’enfants des sujets (variable
# « nb.enfants », aucune donnée manquante). On souhaite transformer cette variable en
# une variable de type facteur (variable « nb.enf.cl ») à 4 modalités : pas d’enfant, un enfant,
# deux enfants, trois enfants ou plus. Quelle commande peut-on utiliser ?
# A. "pas d'enfant" <- data$nb.enf.cl ;"un enfant" <- data$nb.enf.cl[data$nb.enfants==1,] ;
# "deux enfants" <- data$nb.enf.cl[data$nb.enfants==2,] ;
# "trois enfants" <- data$nb.enf.cl[data$nb.enfants>=3,]
# B. data$nb.enf.cl <- "pas d'enfant" ;
# data$nb.enf.cl[data$nb.enfants==1,] <- "un enfant" ;
# data$nb.enf.cl[data$nb.enfants==2,] <- "deux enfants" ;
# data$nb.enf.cl[data$nb.enfants>=3,] <- "trois enfants"
# C*. data[,"nb.enf.cl"] <- "pas d'enfant" ;
# data[data$nb.enfants==1,"nb.enf.cl"] <- "un enfant" ;
# data[data$nb.enfants==2,"nb.enf.cl"] <- "deux enfants" ;
# data[data$nb.enfants>=3,"nb.enf.cl"] <- "trois enfants"
# D. data$nb.enf.cl <- ifelse(data$nb.enfants=0, "pas d'enfant", ifelse(data$nb.enfants=1, "un enfant",
#   ifelse(data$nb.enfants=2, "deux enfants", "trois enfants")))


# 2.7. Nous travaillons sur une étude impliquant 30 sujets chez lesquels nous avons recueillis
# 25 caractéristiques. Les données sont stockées dans un fichier excel que nous avons
# importé dans R et stocké dans l’objet « data_v2 ». L’utilisation du code « dim(data_v2) »
# nous renvoie la sortie suivante :
output
# Que pouvons-nous en déduire ?
# A. L’import des données s’est passé correctement
# B. Nous avons oublié de spécifier que la première ligne du tableau à importer contient les noms des variables
# C. Nous avons commis une erreur sur le séparateur utilisé entre chaque cellule du tableau
# D. Nous avons mal précisé comment sont codées les données manquantes dans notre tableau
                                                                                                                                                                         

# 2.9. Nous disposons dans « data » de cinq variables (v5p1, v5p2, v5p3, v5p4, v5p5) ayant
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
                                                                                                                                                                         
                                                                                                                                                                         
# 2.10. Nous disposons de deux bases de données « data1 » et « data2 » avec dans chacune
# les mêmes individus identifiés par la variable « ID ». La base « data1 » contient 9 variables
# et la base « data2 » 19 variables. Les variables « Age » et « DEP » sont communes aux
# deux tables. Quels codes pouvons-nous utiliser pour combiner ces deux bases en une
# seule nommée « dataTot » sans avoir d’information redondante ? (plusieurs réponses attendues)
dataTot <- merge(data1, data2) # A. 
dataTot <- merge(data1, data2, by = "ID") # B.
dataTot <- merge(data1, data2, by = c("ID", "Age", "DEP")) # C.
dataTot <- merge(data1, data2, by = "ID", all.x = TRUE) # D.
                                                                                                                                                                   

# 1.12. A partir de la base de données « data », on souhaite sélectionner seulement les sujets
# atteints de troubles bipolaires (variable « tbl.bipol », valant 1 si le sujet est atteint, 0 sinon)
# ou de dépression (variable « depression », valant 1 si le sujet est atteint, 0 sinon). La
# sélection sera stockée dans data.tbl Quelle commande peut-on utiliser ?
data.tbl <- data$tbl.bipol == 1 | data$depression == 1
data.tbl <- rbind(subset(data,tbl.bipol == 1), subset(data, depression == 1))
data.tbl <- data[ , tbl.bipol == 1|depression == 1]
data.tbl <- subset(data,tbl.bipol == 1|depression == 1) # D*

order()
# 1.13. On dispose d’une base de données (« data.tbl ») avec des sujets atteints soit de
# dépression (variable « depression », valant 1 si le sujet est atteint, 0 sinon) soit de troubles
# bipolaires (variable « tbl.bipol », valant 1 si le sujet est atteint, 0 sinon). On souhaite trier
# cette base pour afficher en premier les sujets atteints de dépression puis ceux atteints de
# troubles bipolaires. Quelle commande peut-on utiliser ?
data.tbl <- reorder(data.tbl, c(depression,tbl.bipol))
data.tbl <- order(data.tbl[, c("depression","tbl.bipol")])
data.tbl <- data.tbl[order(depression,tbl.bipol), ]
data.tbl <- data.tbl[order(tbl.bipol,depression), ] # D*
                                                                                                                                                                         



