# Distributions, plots and stat tests

stat
# 1.4. Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A*. Selon le théorème central limite, la somme d’un grand nombre de variables
# aléatoires indépendantes et de même loi suit une distribution normale
# B. La somme de k variables aléatoires suivant une loi normale centrée réduite est une
# variable aléatoire distribuée selon une loi du Chi-2 à k degrés de liberté
# C. Lors d’un test du Chi-2, la valeur du degré de signification n’est pas dépendante
# des effectifs des deux échantillons
# D*. Le test de Student est robuste à un écart à la normalité de la variable X étudiée

hist()
# 1.11. Quelle fonction a pu être utilisée pour obtenir le graphique suivant (la variable « gravite »
# représente la gravité des troubles psychiatriques des sujets, variant de 1 à 7) :
# A. plot()
# B. hist()
# C. pie()
# D. barplot()

hist()
# 2.11. Nous disposons dans la base de données d’une variable « réveil_noct » quantifiant la
# fréquence de survenue des réveils nocturnes. Voici la représentation graphique associée
# à cette variable :
output
# A quelles fonctions a-t-il fallu faire appel pour réaliser ce graphique ? 
# (plusieurs réponses attendues)
# A. table()
# B. prop.table()
# C. barplot()
# D. hist()

hist()
# 2.13. Nous disposons des figures suivantes (4 histograms):
# Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A. Ces figures ont pu être générées grâce à l’utilisation de la fonction norm()
# B. Toutes les distributions sont normales
# C. La figure 3 est la distribution ayant le plus petit écart-type
# D. La figure 3 est la distribution ayant le plus grand écart-type


# Statistical tests

t.test()
# 1.14. On dispose des sorties suivantes concernant le nombre d’années passées en prison des
# sujets (variable « annees.prison » dans la base de données « dataP »).
output # of mean, sd, t-test
# Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A. La variance estimée dans l’échantillon est égale à environ 0,8
# B. Il n’y a pas de données manquantes pour la variable « annees.prison »
# C. Dans 95% des cas, la vraie valeur de la moyenne du nombre d’années de prison
# dans la population est entre 2,415 et 2,544 années
# D. Il est impossible de connaitre l’intervalle de confiance de la vraie valeur de la
# moyenne du nombre d’années de prison dans la population à partir des sorties
# données

binom.test()
# 1.15. Les données de la base de données « dataP » sont issues d’une enquête transversale
# réalisée en 2021 chez des prisonniers. En France, en 2021, le pourcentage de prisonniers
# atteints d’agoraphobie est de 17%. On dispose des sorties suivantes concernant le statut
# des sujets vis-à-vis de cette pathologie (variable « agoraphobie »).
output
# Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A. La prévalence de l’agoraphobie est égale à 16%
# B. La proportion de prisonniers atteints d’agoraphobie est égale à 16%
# C. La commande n’est pas adéquate pour estimer l’intervalle de confiance à 95% de
# la proportion de prisonniers atteints d’agoraphobie dans la population
# D. Notre échantillon n’est pas représentatif de la population française de prisonniers
# en 2021 concernant l’agoraphobie

# 2.14. Les données de la base de données « dataSom » sont issues d’une enquête transversale
# réalisée en 2022 chez des adolescents en France métropolitaine. Aux Etats-Unis, la
# même année, une étude a montré que 21% des adolescents rencontraient des difficultés
# à dormir. On dispose des sorties suivantes concernant le statut des sujets vis-à-vis des
# difficultés pendant le sommeil (variable « resum_sommeil ») :
output
# Quelle affirmation est exacte ?
#   A. Etant donné la valeur du degré de signification, on peut conclure à un lien très fort
# entre le pays et la prévalence des difficultés de sommeil chez les adolescents
# B. L’hypothèse alternative du test réalisé ici est une différence nulle entre la proportion
# d’adolescents rencontrant des difficultés de sommeil aux Etats-Unis et la même
# proportion en France
# C. Au risque 5%, il existe une différence statistiquement significative entre la
# prévalence des difficultés du sommeil en France et aux Etats-Unis sur la même
# période
# D. La commande utilisée ici n’est pas adéquate pour estimer l’intervalle de confiance
# à 95% de la proportion d’adolescents rencontrant des difficultés du sommeil dans la
# population

stat
# 2.15 Chez des adolescents, nous nous intéressons au lien entre le sexe (variable « SEXE »,
# codée 1 si l’individu est un garçon et 2 si c’est une fille) et le fait d’avoir peur d’être trop
# gros (variable « v22 », codée 1 si l’individu ne ressent pas cette peur, 2 s’il la ressent).
# Quelle affirmation est exacte ?
# A.* L’hypothèse nulle du test statistique adéquat est l’absence de lien entre le sexe et
# la peur d’être trop gros
# B. L’hypothèse alternative du test statistique adéquat est l’existence d’un lien
# significatif entre le sexe et la peur d’être trop gros
# C. Le test statistique adéquat pour tester cette relation est un test du coefficient de corrélation
# D. Aucune réponse n’est exacte

stat
# 2.16 Chez des adolescents (jeu de données « data300 »), nous nous intéressons au lien entre
# le sexe (variable « SEXE », codée 1 si l’individu est un garçon et 2 si c’est une fille) et le
# fait d’avoir peur d’être trop gros (variable « v22 », codée 1 si l’individu ne ressent pas cette
# peur, 2 s’il la ressent). Nous disposons de la sortie suivante :
output
# Quelle affirmation est exacte ?
# A. La proportion observée d’adolescents ayant peur d’être trop gros est inférieure chez
# les garçons par rapport aux filles
# B. Au risque 5%, on met en évidence un lien significatif entre le sexe et le fait d’avoir
# peur d’être trop gros
# C. Au risque 5%, on ne met pas en évidence de lien significatif entre le sexe et le fait
# d’avoir peur d’être trop gros
# D*. Le code utilisé ici n’est pas approprié

anova()
# 2.17. Chez des adolescents (jeu de données « data300 »), nous nous intéressons au lien entre
# l’âge (variable « Age » en années) et l’intérêt pour l’école (variable « v1p5 », à 4 classes :
# « j’aime beaucoup », « j’aime assez », « je n’aime pas trop », « je n’aime pas du tout »).
# Nous disposons de la sortie suivante :
output
# Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A. L’hypothèse nulle du test est l’égalité dans la population des moyennes d’âge dans
# les différents groupes d’intérêt pour l’école
# B. L’hypothèse alternative du test est la différence dans la population des moyennes
# d’âge dans les différents groupes d’intérêt pour l’école
# C. Au risque 5%, on ne met pas en évidence de lien significatif entre l’âge moyen des
# adolescents et l’intérêt pour l’école
# D. Le test réalisé ici est un test non paramétrique


chisq.test()
# 1.16. Dans notre étude, on souhaite tester le lien entre l’alcoolisme et la dépression. Quelle
# fonction peut être utilisée ?
t.test()
cor.test()
chisq.test() # C.
# D. Aucune des fonctions proposées

chisq.test()
# 2.18 Chez des adolescents (jeu de données « data300 »), nous nous intéressons au lien entre
# la prise d’alcool (variable « v9p1.2 », codée 0 si l’adolescent n’a jamais pris d’alcool dans
# sa vie, 1 pour une prise d’alcool, 2 pour entre 2 et 4 prises d’alcool et 3 pour 5 prises
# d’alcool ou plus) et la consommation de cigarettes (variable « v7.2 », codée 0 si
# l’adolescent n’a jamais fumé, 1 si il a fumé 1 ou 2 cigarettes, 2 si il a fumé entre 3 et 19
# cigarettes et 3 si il en a fumé 20 ou plus). Nous disposons de la sortie suivante :
# Quel code a pu être utilisé pour réaliser le test statistique adéquat ?
t.test(data300$v7.2 ~ data300$v9p1.2) # A
chisq.test(table(data300$v9p1.2, data300$v7.2)) #B*
chisq.test(prop.table(table(data300$v9p1.2, data300$v7.2), correct = FALSE)) # C
#D. Aucun des codes n’est approprié

chisq.test()
# 2.19 Chez des adolescents (jeu de données « data300 »), nous nous intéressons au lien entre
# la prise d’alcool (variable « v9p1.2 », codée 0 si l’adolescent n’a jamais pris d’alcool dans
# sa vie, 1 pour une prise d’alcool, 2 pour entre 2 et 4 prises d’alcool et 3 pour 5 prises
# d’alcool ou plus) et la consommation de cigarettes (variable « v7.2 », codée 0 si
# l’adolescent n’a jamais fumé, 1 si il a fumé 1 ou 2 cigarettes, 2 si il a fumé entre 3 et 19
# cigarettes et 3 si il en a fumé 20 ou plus). Nous disposons de la sortie suivante :
output
# Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A. Au risque 5%, on met en évidence un lien significatif entre la prise d’alcool et la
# consommation de cigarettes
# B. Au risque 5%, on ne met pas en évidence de lien significatif entre la prise d’alcool
# et la consommation de cigarettes
# C. Au risque 5%, on rejette l’hypothèse alternative du test
# D. Au risque 5%, on rejette l’hypothèse nulle du test

chisq.test()
# 2.20 Nous disposons d’une variable « varP.2 » prenant des valeurs comprises entre 0 et 4.
# Nous souhaitons savoir si cette variable est distribuée selon une loi de Poisson de
# paramètre 0,7. Nous disposons des sorties suivantes :
output
# Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A. Les effectifs théoriques du test du Chi-2 sont trop petits pour permettre la réalisation
# du test statistique
# B. C’est la variable « x » qui permet le calcul des effectifs théoriques nécessaires à la réalisation du test
# C. Le nombre de degrés de liberté associé à la loi suivie par la statistique de test sous H0 est égal à 5
# D. Le nombre de degrés de liberté associé à la loi suivie par la statistique de test sous H0 est égal à 4

chisq.test()
# 2.21 Nous disposons d’une variable « varP.2 » prenant des valeurs comprises entre 0 et 4.
# Nous souhaitons savoir si cette variable est distribuée selon une loi de Poisson de
# paramètre 0,7. Nous disposons des sorties suivantes :
output
# Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A. La variance de la variable « varP.2 » est supérieure à 0,3
# B. La distribution de la variable « varP.2 » est compatible avec l’hypothèse d’une
# distribution de Poisson de paramètre 0,7
# C. La p-value du test est inférieure à 5%
# D. La p-value du test est supérieure à 5%


cor.test()
# 1.24. On souhaite tester le lien entre X, le nombre d’enfants (variable « nb.enfants ») et Y, le
# nombre d’années passées en prison (variable « annees.prison »). La base de données
# est la base « dataP ». Quelle commande peut-on utiliser ?
t.test()
cor.test()
anova()
cor()

cor.test()
# 1.25. On souhaite tester le lien entre X, le nombre d’enfants (variable « nb.enfants ») et Y, le
# nombre d’années passées en prison (variable « annees.prison »). La base de données
# est la base « dataP ». On dispose de la sortie suivante :
output
# Quelle affirmation est exacte ?
# A. Au risque 5%, on montre un lien significatif entre le nombre d’enfants et le nombre
# d’années passées en prison
# B. Au risque 5%, il n’y a pas de lien entre le nombre d’enfants et le nombre d’années
# passées en prison
# C. La part de variabilité de Y expliquée par X est égal à 1,9%
# D. On peut déduire que, au risque 5%, dans un modèle linéaire univarié avec le nombre
# d’années passées en prison comme variable explicative, on ne montrerait pas que
# le coefficient de régression associé au nombre d’enfants est significativement
# différent de 0


glm()
# 27. On s’intéresse au lien entre la psychose (variable « psychose », valant 1 si l’individu a une
# psychose, 0 sinon), et la dépression (variable « depression », valant 1 si l’individu a une
# dépression, 0 sinon) qui est le critère de jugement principal dans notre étude. On dispose
# de la sortie suivante :
output
#   Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A. Au risque 5%, on montre une diminution du risque de dépression chez les individus
# atteints de psychose par rapport aux individus non atteints de psychose
# B. La fonction glm() a pu être utilisée pour obtenir ces résultats
# C. Au risque 5%, il existe un lien significatif entre la psychose et la dépression
# D. L’intervalle de confiance à 95% de l’OR mesurant l’association entre la psychose et
# la dépression ne contient pas la valeur 1


# 28. On s’intéresse au lien entre le nombre de peines de prison réalisées avant la peine
# actuelle (variable « n.prison ») et la durée de la peine actuelle (variable « duree.peine »).
# On prend également en compte le type de centre dans lequel l’individu est incarcéré
# (variable « type.centre », valant 1 pour les maisons centrales, 2 pour les centres de
#  détention et 3 pour les maisons d’arrêts). On utilise la base de données « dataP ». On
# dispose des sorties suivantes :
output
#   Quelle affirmation est exacte ?
#   A. Le type de centre semble être un facteur de confusion dans la relation entre le
# nombre de peines de prison passées et la durée de la peine de prison actuelle
# B. Au risque 5%, on ne met pas en évidence d’association entre le type de centre et la
# durée de la peine de prison actuelle
# C. La durée de la peine de prison actuelle est égale en moyenne à 4,4 ans
# D. A type de centre égal, le fait d’avoir eu une peine de prison supplémentaire dans le
# passé diminue la durée de la peine de prison actuelle de 0,04 ans






