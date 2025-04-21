# SNDS exercises
# Practice code to constitute a cohort of patients meeting particular criteria and investigate their 
# consumption of town health care resources (from DCIR) 
# and hospital health care resources (PMSI).

library(reshape2) 
library(tidyverse)
library(data.table)

# Download directly from gitlab
DATA_DIR <- "https://gitlab.com/healthdatahub/se-former-au-snds/exercices-snds/-/raw/master/data"

ER_PRS_F_FILE <- read.csv(file.path(DATA_DIR, "ER_PRS_F.csv"))      # Table des prestations
ER_PHA_F_FILE <- read.csv(file.path(DATA_DIR, "ER_PHA_F.csv"))      # Table de la pharmacie
T_MCO_C_FILE <- read.csv(file.path(DATA_DIR, "T_MCOaa_nnC.csv"))    # MCO - Pseudonymes des patients et dates de soin
T_MCO_B_FILE <- read.csv(file.path(DATA_DIR, "T_MCOaa_nnB.csv"))    # MCO - Description du séjour

# Define joining variables for prestations/pharmacie
# Définition des variables de jointure
DCIR_join_vars <- c("FLX_DIS_DTD", "FLX_TRT_DTD", "FLX_EMT_TYP", "FLX_EMT_NUM", "FLX_EMT_ORD",
    "ORG_CLE_NUM", "DCT_ORD_NUM", "PRS_ORD_NUM", "REM_TYP_AFF")

MCO_join_vars <- c("ETA_NUM", "RSA_NUM") # PMSI/MCO

# Exercise 1: constitution of a cohort ------
# Females that have consulted an oncologist between the ages of 40 and 70
# Apply filters to ER_PRS_F to select patients meeting the inclusion criteria

# Read in data
# On récupère les données des prestations, cette table comprend également les données des bénéficiaires
df_presta <- read.table(
            ER_PRS_F_FILE,                           # Chemin vers le fichier
            header=TRUE, 
            sep=",")[ ,c("BEN_NIR_PSA",              # Pseudonyme du bénéficiaire
            "BEN_AMA_COD",                           # Age au moment de la prestation
            "BEN_SEX_COD",                           # Code sexe
            "PSE_SPE_COD",                           # Spécialité médicale du professionnel de santé exécutant
            "PSP_SPE_COD",                           # Spécialité médicale du professionnel de santé prescripteur
            "PRS_NAT_REF",                           # Nature de la prestation de référence
            "EXE_SOI_DTD",                           # Date d'exécution du soin
            DCIR_join_vars)]                         # Variables de jointure du DCIR

# Or more concisely
df_presta <- ER_PRS_F_FILE %>% select(BEN_NIR_PSA, BEN_AMA_COD, BEN_SEX_COD, PSE_SPE_COD, PSP_SPE_COD,
                                      PRS_NAT_REF, EXE_SOI_DTD, all_of(DCIR_join_vars))

# 5000 obs of 16 variables

 # On sélectionne les prestations :
cohorte_presta <- df_presta %>% 
  filter(BEN_SEX_COD == 2, # Pour des femmes
         PSP_SPE_COD %in% c(73,74) | PSE_SPE_COD %in% c(73,74), # Prescrites ou exécutées par un oncologue
         BEN_AMA_COD >= 40, BEN_AMA_COD <= 70) # # Patientes âgées de 40 à 70 ans


# NB: On ne prend pas en compte le rang gémellaire "BEN_RNG_GEM"... 
#... qui permet de s'assurer que la cohorte correspond bien à des personnes uniques (chez les enfants notamment)
# Get unique patients
cohorte_liste <- cohorte_presta %>% select(BEN_NIR_PSA) %>% distinct(BEN_NIR_PSA)

paste("Liste de pseudonymes de la cohorte de patientes (20 premières lignes) :") 
print(head(cohorte_liste$BEN_NIR_PSA, 20))

# 23 of these meet the criteria

# Exercise 2: finding the most reimbursed medication ----
# Information on reimbursement of medication are grouped together in the pharmacy table ER_PHA_F
# (make sure to read in files above first)

df_pharma <- read.table(
            ER_PHA_F_FILE,                  # Chemin vers le fichier
            header=TRUE, 
            sep=",")[ ,c("PHA_PRS_C13",     # Code CIP de la pharmacie de ville
            "PHA_ACT_QSN",                  # Quantité de boites facturées
            DCIR_join_vars)]                # Variables de jointure du DCIR

# Or more concisely
df_pharma <- ER_PHA_F_FILE %>% select(PHA_PRS_C13, PHA_ACT_QSN, all_of(DCIR_join_vars))

# 5000 obs of 11 variables

# The code CIP allows us to precisely identify a medication (the molecule but also the commercial name)

# On ne regarde que les données de pharmacie qui concernent les patients dans la cohorte.
# (inner join with previous filtered table of patients)

cohort.pts <- df_presta[df_presta$BEN_NIR_PSA %in% cohorte_liste$BEN_NIR_PSA, ]
df_pharma_coh <- inner_join(cohort.pts, df_pharma, by=DCIR_join_vars)

# Prise en compte de la régularisation des remboursements de médicaments prescrits (utile dans le SNDS réel)
df_pharma_coh_cleaned <- df_pharma_coh %>%
  group_by(PHA_PRS_C13, EXE_SOI_DTD, BEN_NIR_PSA) %>%   # Group by medication, patient, date
  # On somme le nombre de boîtes par prestation
  summarise(sum_PHA_ACT_QSN = sum(PHA_ACT_QSN)) %>%
  # On filtre en gardant uniquement les quantités de boites facturées positives et non nulles
  filter(sum_PHA_ACT_QSN > 0)  

# 261 of a given medication per patient per date

# On calcule le nombre de patients de la cohorte à qui un médicament donné a été prescrit.
medicament_freq <- df_pharma_coh_cleaned %>% 
  group_by(PHA_PRS_C13) %>%
  summarise(nb_patients = n_distinct(BEN_NIR_PSA))

# On sélectionne ensuite le(s) médicament(s) ayant été prescrit(s) au plus grand nombre
# de patients.
top_medicament <- medicament_freq[medicament_freq$nb_patients == max(medicament_freq$nb_patients), ]

paste("Le médicament avec le code CIP",top_medicament[[1,1]],
      "est l'un des médicaments ayant été remboursé au plus grand nombre de patients différents, soit à",
      top_medicament[[1,2]], "patients")



# Exercise 3: top services ---- 
# Calculate the top 10 services performed by oncologists over the year for males and females

# On sélectionne les prestations Exécutées par un oncologue
df_presta_onco <- df_presta %>% filter(PSE_SPE_COD %in% c(73,74))

# Attribution de labels aux codes sexe
df_presta_onco$BEN_SEX_COD[df_presta_onco$BEN_SEX_COD == 1] <- "Masculin"
df_presta_onco$BEN_SEX_COD[df_presta_onco$BEN_SEX_COD == 2] <- "Feminin"
df_presta_onco$BEN_SEX_COD[df_presta_onco$BEN_SEX_COD == 0 | df_presta_onco$BEN_SEX_COD == 9] <- "Inconnu"

#as.factor(df_presta_onco$BEN_SEX_COD, labels = c("inconnu", "masculin", "feminin", "inconnu"))

# NB: Dans les données réelles du SNDS une 5e valeur est possible "" qui correspond à une donnée manquante


# On calcule, en fonction du sexe du patient, le nombre de prestations de
# chaque type réalisées par les oncologues
COUNT_PRS_NAT  <- as.data.frame(table(df_presta_onco$PRS_NAT_REF, df_presta_onco$BEN_SEX_COD, useNA="ifany"))
names(COUNT_PRS_NAT) <- c("PRS_NAT_REF", "BEN_SEX_COD", "COUNT")

#df_presta_onco %>% group_by(PRS_NAT_REF, BEN_SEX_COD) %>% summarise(freq = sum())

# On les ordonne
COUNT_PRS_NAT <- COUNT_PRS_NAT %>%
  group_by(BEN_SEX_COD) %>%
  mutate(ORDER=rank(-COUNT))%>%
  arrange(ORDER)


# On se limite aux n_top_presta prestations les plus fréquentes souhaitées pour chaque sexe
n_top_presta <- 10
COUNT_PRS_NAT_top <- COUNT_PRS_NAT %>% 
  group_by(BEN_SEX_COD) %>%   # Groupage par sexe
  filter(row_number() %in% (1:n_top_presta))               # Top n lignes
  
  
# On affiche les résultats sous forme d'histogramme
ggplot(COUNT_PRS_NAT_top, aes(x=reorder(PRS_NAT_REF, -COUNT),
                              y=COUNT, 
                              fill = BEN_SEX_COD)) +          # Couleur différente pour chaque sexe
  geom_bar(stat='identity') +                                 # Histogramme
  labs(x="Prestations exécutées", y="Count", fill="Sexe") +   # Titres des figures
  theme(
    aspect.ratio = 3,                                     
    axis.text.x = element_text(angle = 45, hjust = 1)         # Angle des labels
  ) +  
  facet_wrap(~BEN_SEX_COD, scales="free")                     # Une figure par sexe
  
  
  
  
# Exercise 4: long-term hospitalisation (using PMSI) ---- 
# Consistute a subcohort of patients. From those extracted previously, find those with the longest stays
# MCO = medecine - chirurgie - obstetrique

# On récupère les données du MCO qui comprennent les pseudonymes des patients et les
# dates des séjours.
df_mco_c <- read.table(
            T_MCO_C_FILE,                    # Chemin vers le fichier
            header=TRUE, 
            sep=",",
            colClasses=c('character')        # Pour éviter l'interprétation des dates en tant que nombres
            )[ ,c("EXE_SOI_DTD",             # Date d'entrée
                      "EXE_SOI_DTF",         # Date de sortie
                      "NIR_ANO_17",          # Pseudonyme du bénéficiaire
                      MCO_join_vars)]        # Variables de jointure MCO

# Définition des numéros d'établissement (APHP, APHM, HCL) doublons à supprimer
ETA_NUM_doubles <- c(
'130780521','130783236', '130783293', '130784234', '130804297', '600100101','750041543', '750100018',
'750100042', '750100075', '750100083', '750100091', '750100109', '750100125','750100166', '750100208', 
'750100216', '750100232', '750100273', '750100299', '750801441', '750803447','750803454', '910100015', 
'910100023', '920100013', '920100021', '920100039', '920100047', '920100054','920100062', '930100011', 
'930100037', '930100045', '940100027', '940100035', '940100043', '940100050','940100068', '950100016', 
'690783154', '690784137', '690784152', '690784178', '690787478', '830100558' 
)

# On supprime les doublons
df_mco_c <- df_mco_c %>% filter(!ETA_NUM %in% ETA_NUM_doubles)


 # On calcule la durée des séjours
df_mco_c$DUREE_SEJOUR <-as.Date(substr(df_mco_c$EXE_SOI_DTF,1,9), "%d%b%Y") -
  as.Date(substr(df_mco_c$EXE_SOI_DTD,1,9), "%d%b%Y")  

# On calcule le troisième quartile des durées de séjour
troisieme_quartile_sejour <- quantile(df_mco_c$DUREE_SEJOUR, 0.75, na.rm = TRUE)
troisieme_quartile_sejour

# On construit la sous-cohorte correspondante aux patients avec le plus long séjour
sous_cohorte_liste <- df_mco_c %>%
  filter(DUREE_SEJOUR>=troisieme_quartile_sejour,
         NIR_ANO_17 %in% cohorte_liste$BEN_NIR_PSA)

paste("Liste des 10 premiers pseudonymes de la sous-cohorte de patients :")
print(head(sous_cohorte_liste$NIR_ANO_17, 10))    


 # On récupère les données du MCO qui concernent la description du séjour
df_mco_b <- read.table(
            T_MCO_B_FILE,                            # Chemin vers le fichier
            header=TRUE, 
            sep=",")[ ,c("DGN_PAL",                  # Diagnostic principal
                         "GRG_GHM",                  # GHM calculé par le GENRSA
                      MCO_join_vars)]                # Variables de jointure MCO
					  
					  
df_mco_b2 <- df_mco_b %>%
    filter(substr(GRG_GHM, 1, 2) != "90",     # suppression des séjours en erreur
    !ETA_NUM %in% ETA_NUM_doubles)            # suprression des doublons pour APHP, etc.
	
	
# On crée une nouvelle variable pour le diagnostic principal avec les 3 premiers caractères de la CIM10
df_mco_b2$DGN_PAL_INT<-substr(df_mco_b2$DGN_PAL, 1,3)

 # On calcule ensuite le nombre de diagnostics de chaque type associés aux
# séjours 
COUNT_DGN_PAL <- df_mco_b2 %>%
  group_by(DGN_PAL_INT) %>%
  summarise(COUNT=length(DGN_PAL_INT)) %>%
  mutate(ORDER=rank(-COUNT))%>%
  arrange(ORDER) 
  
  
  # On définit les n_top_diag diagnostics les plus fréquents souhaités 
n_top_diag <- 10

# On affiche les résultats sous forme d'histogramme
ggplot(COUNT_DGN_PAL[1:n_top_diag,],                  # On se limite aux n_top_diag
       aes(x=reorder(DGN_PAL_INT, -COUNT), 
           y=COUNT, 
           fill=DGN_PAL_INT))+
  
  geom_bar(stat='identity') +                                            # Histogramme        
  geom_text(stat='identity', aes(label=COUNT), vjust=-1) +               # Affichage du compte pour chaque barre
  theme(aspect.ratio = 2) +                                              # Taille de la figure
  labs(x = "Diagnostic principal (CIM10 - 3 premiers caractères)", y="Count", 
       fill="Diagnostic principal")       # Titres de la figure

# Exercise 5 Top diagnostics ----
# Diagnostic most associated with hospitalisation (top 10)

# On récupère les données du MCO qui concernent la description du séjour
df_mco_b <- read.table(
            T_MCO_B_FILE,                            # Chemin vers le fichier
            header=TRUE, 
            sep=",")[ ,c("DGN_PAL",                  # Diagnostic principal
                         "GRG_GHM",                  # GHM calculé par le GENRSA
                      MCO_join_vars)]                # Variables de jointure MCO


 df_mco_b2 <- df_mco_b %>%
    filter(substr(GRG_GHM, 1, 2) != "90",     # suppression des séjours en erreur
    !ETA_NUM %in% ETA_NUM_doubles)            # suprression des doublons pour APHP, etc.

 # On crée une nouvelle variable pour le diagnostic principal avec les 3 premiers caractères de la CIM10
df_mco_b2$DGN_PAL_INT<-substr(df_mco_b2$DGN_PAL, 1,3)


# On calcule ensuite le nombre de diagnostics de chaque type associés aux
# séjours 
COUNT_DGN_PAL <- df_mco_b2 %>%
  group_by(DGN_PAL_INT) %>%
  summarise(COUNT=length(DGN_PAL_INT)) %>%
  mutate(ORDER=rank(-COUNT))%>%
  arrange(ORDER) 

# On définit les n_top_diag diagnostics les plus fréquents souhaités 
n_top_diag <- 10

# On affiche les résultats sous forme d'histogramme
ggplot(COUNT_DGN_PAL[1:n_top_diag,],                  # On se limite aux n_top_diag
       aes(x=reorder(DGN_PAL_INT, -COUNT), 
           y=COUNT, 
           fill=DGN_PAL_INT))+
  
  geom_bar(stat='identity') +                                            # Histogramme        
  geom_text(stat='identity', aes(label=COUNT), vjust=-1) +               # Affichage du compte pour chaque barre
  theme(aspect.ratio = 2) +                                              # Taille de la figure
  labs(x = "Diagnostic principal (CIM10 - 3 premiers caractères)", y="Count", 
       fill="Diagnostic principal")       # Titres de la figure

# Exercise 5 bis: top diagnostics sub-cohort ---- 

# On commence par joindre les tables MCO (dates de soins et description du séjour), afin
# d'associer les informations de séjour aux diagnostics correspondants.
# On ne regarde ici que les patients appartenants à la sous-cohorte.
df_mco_b_coh <- merge(sous_cohorte_liste, 
                      df_mco_b2, 
                      by=MCO_join_vars)

# On calcule ensuite le nombre de diagnostics de chaque type associés aux
# séjours pour notre sous-cohorte de patients et de séjours.
COUNT_DGN_PAL <- df_mco_b_coh %>%
  group_by(DGN_PAL_INT) %>%
  summarise(COUNT=length(DGN_PAL_INT)) %>%
  mutate(ORDER=rank(-COUNT))%>%
  arrange(ORDER) 

# On définit les n_top_diag diagnostics les plus fréquents souhaités 
n_top_diag <- 10

# On affiche les résultats sous forme d'histogramme
ggplot(COUNT_DGN_PAL[1:n_top_diag,],                  # On se limite aux n_top_diag
       aes(x=reorder(DGN_PAL_INT, -COUNT), 
           y=COUNT, 
           fill=DGN_PAL_INT))+
  
  geom_bar(stat='identity') +                                          # Histogramme        
  geom_text(stat='identity', aes(label=COUNT), vjust=-1) +             # Affichage du compte pour chaque barre
  theme(aspect.ratio = 2) +                                            # Taille de la figure
  labs(x = "Diagnostic principal (CIM10 - 3 premiers caractères)", y="Count", 
      fill="Diagnostic principal")     # Titres de la figure


