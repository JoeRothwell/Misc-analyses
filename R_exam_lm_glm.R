# Questions on linear models and glms


lm()
# 1.25. On souhaite tester le lien entre X, le nombre d’enfants (variable « nb.enfants ») et Y, le
# nombre d’années passées en prison (variable « annees.prison »). La base de données
# est la base « dataP ». On dispose de la sortie suivante :
output
# Quelle affirmation est exacte ?
# A. Au risque 5%, on montre un lien significatif entre le nombre d’enfants et le nombre
# d’années passées en prison
# B. Au risque 5%, il n’y a pas de lien entre le nombre d’enfants et le nombre d’années passées en prison
# C. La part de variabilité de Y expliquée par X est égal à 1,9%
# D*. On peut déduire que, au risque 5%, dans un modèle linéaire univarié avec le nombre
# d’années passées en prison comme variable explicative, on ne montrerait pas que
# le coefficient de régression associé au nombre d’enfants est significativement différent de 0


glm()
# 1.27. On s’intéresse au lien entre la psychose (variable « psychose », valant 1 si l’individu a une
# psychose, 0 sinon), et la dépression (variable « depression », valant 1 si l’individu a une
# dépression, 0 sinon) qui est le critère de jugement principal dans notre étude. On dispose
# de la sortie suivante :
output
# Quelles affirmations sont exactes ? (plusieurs réponses attendues)
# A. Au risque 5%, on montre une diminution du risque de dépression chez les individus
# atteints de psychose par rapport aux individus non atteints de psychose
# B*. La fonction glm() a pu être utilisée pour obtenir ces résultats
# C*. Au risque 5%, il existe un lien significatif entre la psychose et la dépression
# D*. L’intervalle de confiance à 95% de l’OR mesurant l’association entre la psychose et
# la dépression ne contient pas la valeur 1


# 1.28. On s’intéresse au lien entre le nombre de peines de prison réalisées avant la peine
# actuelle (variable « n.prison ») et la durée de la peine actuelle (variable « duree.peine »).
# On prend également en compte le type de centre dans lequel l’individu est incarcéré
# (variable « type.centre », valant 1 pour les maisons centrales, 2 pour les centres de
# détention et 3 pour les maisons d’arrêts). On utilise la base de données « dataP ». On
# dispose des sorties suivantes :
output # unadjusted and adjusted models
# Quelle affirmation est exacte ?
# A. Le type de centre semble être un facteur de confusion dans la relation entre le
# nombre de peines de prison passées et la durée de la peine de prison actuelle
# B. Au risque 5%, on ne met pas en évidence d’association entre le type de centre et la
# durée de la peine de prison actuelle
# C. La durée de la peine de prison actuelle est égale en moyenne à 4,4 ans
# D*. A type de centre égal, le fait d’avoir eu une peine de prison supplémentaire dans le
# passé diminue la durée de la peine de prison actuelle de 0,04 ans