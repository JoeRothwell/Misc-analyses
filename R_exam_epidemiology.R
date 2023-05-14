# Epidemiology with epitools and survival

# 1.23. Les données de la base de données « dataP » sont issues d’une enquête transversale
# réalisée en 2021 chez des prisonniers. On s’intéresse au lien entre le stress posttraumatique (variable « stress.trauma ») et le risque de suicide (variable « hr.suicide »,
# valant 1 si l’individu est à haut risque de suicide, 0 sinon). On dispose des sorties
# suivantes :
output
# Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A*. Les sorties ont pu être obtenues grâce à la fonction epitab()
# B. Il n’est pas pertinent d’estimer le risque relatif dans cette étude
# C*. Le risque d’être à haut risque de suicide est multiplié par 1,3 chez les patients ayant
# un stress post-traumatique comparés aux patients sans stress post-traumatique
# D*. Au risque 5%, il existe une association significative entre le stress post-traumatique
# et le risque de suicide


# 1.26. On s’intéresse au lien entre les troubles bipolaires (variable « tbl.bipol »), et la dépression
# (variable « depression ») qui est le critère de jugement principal dans notre étude. On
# suspecte que l’abus de substance modifie la relation entre ces deux facteurs. On dispose
# des sorties suivantes :
output
# Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A. Au risque 5%, on ne met pas en évidence d’interaction significative entre l’abus de
# substance et les troubles bipolaires
# B. Au risque 5%, on ne montre pas de différence de l’effet des troubles bipolaires sur
# le risque de dépression entre les individus abusant de substance et ceux n’abusant pas de substance
# C. L’abus de substance n’est pas un facteur de confusion dans la relation entre les
# troubles bipolaires et la dépression
# D. Il n’est pas nécessaire de prendre en compte l’abus de substance quand on étudie
# la relation entre les troubles bipolaires et la depression



# 1.29. On dispose de la sortie suivante :
output 
# kaplan meier survival curve (from survival)
# Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A*. Il s’agit de courbes de Kaplan-Meier
# B*. On peut utiliser le package « survival » pour calculer l’estimateur de Kaplan-Meier
# C. L’évènement étudié ici est forcément le décès
# D*. On applique une analyse de survie sur des données longitudinales

coxph()
# 1.30. On s’intéresse au lien entre la survenue d’un échec de greffe et différents facteurs de
# risque : le sexe du receveur de la greffe (variable « SEXER », valant « M » chez un
# homme, « F » chez une femme) et l’âge du receveur de la greffe (variable « age_tx », en
# années). On dispose de la sortie suivante :
output
# A. Le test du log-rank est un test semi-paramétrique pour comparer la distribution de
# la survie entre deux groupes
# B. Par rapport à être un homme, être une femme multiplie le risque d’échec de greffe
# par 1,2 (effet ajusté sur l’âge)
# C. Ces résultats sont issus d’un modèle de Cox multivarié
# D. L’intervalle de confiance à 95% de l’effet de l’âge (ajusté sur le sexe) sur le risque
# d’échec de greffe contient la valeur 0

epitab()
# 2.22. Nous disposons des données d’une étude cas-témoins réalisée chez des adolescents.
# Elles sont stockées dans l’objet « dataSom ». Parmi les sujets de l’étude, 34 présentaient
# des difficultés de sommeil et 34 n’en présentaient pas (variable « resum_sommeil »,
# codée 1 chez les cas, 0 chez les témoins). Nous nous intéressons au lien entre le sexe
# (variable « SEXE », codée 1 si l’individu est un garçon et 0 si c’est une fille) et les difficultés
# de sommeil. Nous disposons de la sortie suivante :
output
# Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A. Le calcul d’un RR est adapté pour répondre à notre question
# B. Le calcul d’un OR est adapté pour répondre à notre question
# C. La réalisation d’un test du Chi-2 est adapté pour répondre à notre question
# D. La comparaison des proportions de garçons et de filles selon le statut cas/témoins
# est adapté pour répondre à notre question

epitab()
# 2.23. Nous disposons des données d’une étude cas-témoins réalisée chez des adolescents.
# Elles sont stockées dans l’objet « dataSom ». Parmi les sujets de l’étude, 34 présentaient
# des difficultés de sommeil et 34 n’en présentaient pas (variable « resum_sommeil »,
# codée 1 chez les cas, 0 chez les témoins). Nous nous intéressons au lien entre le sexe
# (variable « SEXE », codée 1 si l’individu est un garçon et 0 si c’est une fille) et les difficultés
# de sommeil. Quel code pouvons-nous utiliser ?
epitab(table(dataSom$resum_sommeil,dataSom$sexe),method=c("oddsratio"), pvalue="chi2")
epitab(table(dataSom$sexe,dataSom$resum_sommeil),method=c("oddsratio"), pvalue="chi2")
epitab(table(dataSom$resum_sommeil,dataSom$sexe),method=c("riskratio"), pvalue="chi2")
epitab(table(dataSom$sexe,dataSom$resum_sommeil),method=c("riskratio"), pvalue="chi2")

epitab()
# 2.24 Nous disposons des données d’une étude cas-témoins réalisée chez des adolescents.
# Elles sont stockées dans l’objet « dataSom ». Parmi les sujets de l’étude, 34 présentaient
# des difficultés de sommeil et 34 n’en présentaient pas (variable « resum_sommeil »,
# codée 1 chez les cas, 0 chez les témoins). Nous nous intéressons au lien entre le ressenti
# vis-à-vis de leur santé (variable « v2.cl », codée 1 si l’individu considère sa santé comme
# satisfaisante et 2 si l’individu considère sa santé comme insatisfaisante) et les difficultés
# de sommeil. Nous disposons de la sortie suivante suite à l’utilisation de la fonction epitab() :
# Quelle affirmation est exacte ?
# A. Au risque 5%, on ne met pas en évidence de lien entre le ressenti vis-à-vis de la
# santé et les difficultés de sommeil
# B. Le risque de difficultés de sommeil est multiplié par 4,3 en cas de santé perçue comme insatisfaisante
# C. L’intervalle de confiance à 95% de l’OR ne contient pas 1
# D. On rejette l’hypothèse nulle du test réalisé ici

stat
# 2.25 Quelle affirmation est exacte ?
# A. Si on cherche à identifier les facteurs de risque de difficultés de sommeil (présence
# vs absence), le modèle de régression à utiliser est 
# A. un modèle logistique
# B. un modèle linéaire
# C. un modèle de Cox
# D. Aucun des modèles des réponses précédentes n’est adapté

glm
# 2.26. Nous nous intéressons au lien entre la santé perçue (variable « v2.cl », codée 0 si
# l’individu considère sa santé comme satisfaisante et 1 si l’individu considère sa santé
# comme insatisfaisante) et la capacité de l’adolescent à discuter de ses problèmes
# potentiels avec son médecin (variable « v3p1 », codée 2 si l’adolescent est capable de
# discuter, 1 s’il n’en est pas capable). Les données sont stockées dans l’objet
# « dataHealth ». Nous disposons de la sortie suivante :
output
#   Quelle affirmation est exacte ?
#   A. L’OR mesurant l’association entre la santé perçue et la capacité de l’adolescent à
# discuter de ses problèmes potentiels avec son médecin est inférieur à 0
# B. Le risque d’une santé perçue comme insatisfaisante est plus faible chez les
# adolescents capables de discuter de problèmes potentiels avec leur médecin
# C. Au risque 5%, on met en évidence un lien entre la santé perçue et la capacité de
# l’adolescent à discuter de ses problèmes potentiels avec son médecin
# D. L’intercept de ce modèle est ininterprétable
