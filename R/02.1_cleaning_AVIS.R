#! 02_cleaning.R =======================================================
#! Nettoyage des données PMSI psychiatriques
#! =====================================================================



#! Activation environnement =============================================

renv::activate()



#! Packages nécessaires =================================================

library("tidyverse")
library("janitor")
library("skimr")
library("lubridate")
library("here")



#! Import données brutes ================================================

source(
  here("R", "01_import.R")
)



#! TABLE AVIS ===========================================================
#! Unité statistique = avis psychiatrique
#! =====================================================================



# Structure générale AVIS ===============================================


# Dimensions ------------------------------------------------------------

dim(avis)

# 24 691 lignes (avis)
# 16 variables avant nettoyage
# OK


# Types des variables ---------------------------------------------------

glimpse(avis)

walk(
  names(avis),
  ~ cat(.x, ":", class(avis[[.x]]), "\n")
)


# Variables à nettoyer --------------------------------------------------

# TODO: convertir idpat de numeric vers character

# TODO: convertir i_ddos de numeric vers character

#* sexe
# Character
# OK

# cp : numeric vers character

# TODO: comprendre type_sejour
# Mail à DUPORTAIL

# TODO: harmoniser secteur selon format XXGXX

# convertir mls en abréviations standardisées

# TODO: nettoyer dp
# Diagnostic principal CIM-10

#* n_das
# Numeric
# OK

# TODO: nettoyer das
# Diagnostics associés CIM-10

#* age
# Numeric
# OK

#* an
# Année de l'avis
# OK

# TODO: renommer et convertir debu_tt
# Date/heure début passage

# TODO: renommer et convertir fi_nt
# Date/heure fin passage

# TODO: renommer et convertir de_mt
# Date/heure demande avis

# NOTE: variable probablement peu pertinente cliniquement
# mais utile pour vérifier cohérence chronologique

# TODO: renommer et convertir avi_st
# Date/heure avis psychiatrique


# Résumé statistique ----------------------------------------------------


# Données manquantes globales -------------------------------------------



# Identifiants AVIS =====================================================


# id_dos ---------------------------------------------------------------
# Identifiant du passage aux urgences

# Renommage + conversion character

avis <- avis |>
  rename(
    iddos = i_ddos
  ) |>
  mutate(
    iddos = as.character(iddos)
  )

# idpat ----------------------------------------------------------------
# Identifiant patient anonymisé


# Conversion character

avis <- avis |>
  mutate(
    idpat = as.character(idpat)
  )



# Vérification cohérence patient / passages -----------------------------

# 1 passage ne doit correspondre qu'à un seul patient

coherence_passage_patient <- avis |>
  distinct(
    iddos,
    idpat
  ) |>
  count(
    iddos,
    name = "n_patients"
  ) |>
  filter(
    n_patients > 1
  )

coherence_passage_patient

# NOTE:
# Aucun problème de cohérence relationnelle détecté
# Chaque passage (iddos) est associé à un seul patient (idpat)


# Doublons exacts ======================================================

# Nombre de doublons exacts --------------------------------------------

sum(
  duplicated(avis)
)

# NOTE: Les doublons exacts correspondent à des lignes strictement identiques.


# Affichage des doublons -----------------------------------------------

doublons_exacts <- avis[
  duplicated(avis) |
    duplicated(avis, fromLast = TRUE),
]

doublons_exacts

# Vérification des groupes de doublons ---------------------------------

avis |>
  count(
    across(everything())
  ) |>
  filter(
    n > 1
  )

# Suppression des doublons exacts --------------------------------------

avis <- avis |>
  distinct()

# Vérification après suppression ---------------------------------------

sum(
  duplicated(avis)
)

#* Tous les doublons exacts ont été supprimés.


#! Variables temporelles AVIS ============================================

# Nombre d'avis psychiatriques par année -------------------------------

avis_par_an <- avis |>
  count(
    an,
    name = "n_avis"
  ) |>
  arrange(an)

avis_par_an


# Représentation graphique ----------------------------------------------

plot_avis_par_an <- avis_par_an |>
  ggplot(
    aes(
      x = an,
      y = n_avis
    )
  ) +
  geom_col(
    fill = "#4E79A7",
    width = 0.7
  ) +
  geom_text(
    aes(label = n_avis),
    vjust = -0.7,
    size = 4
  ) +
  scale_x_continuous(
    breaks = avis_par_an$an
  ) +
  labs(
    title = "Nombre d'avis psychiatriques par année",
    x = "Année",
    y = "Nombre d'avis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

plot_avis_par_an



# Correction variables temporelles -------------------------------------

# Renommage variables

avis <- avis |>
  rename(
    debut_t = debu_tt,
    fin_t = fi_nt,
    dem_t = de_mt,
    avis_t = avi_st
  )


# Conversion datetime

avis <- avis |>
  mutate(
    debut_t = parse_date_time(
      debut_t,
      orders = "dmy HM"
    ),

    fin_t = parse_date_time(
      fin_t,
      orders = "dmy HM"
    ),

    dem_t = parse_date_time(
      dem_t,
      orders = "dmy HM"
    ),

    avis_t = parse_date_time(
      avis_t,
      orders = "dmy HM"
    )
  )



# Variables horaires ----------------------------------------------------

avis <- avis |>
  mutate(
    heure_debut = hour(debut_t),
    heure_avis = hour(avis_t),
    heure_fin = hour(fin_t)
  )



# Variables nocturnes ---------------------------------------------------

avis <- avis |>
  mutate(
    nuit_debut = hour(debut_t) >= 18 |
      hour(debut_t) < 8,

    nuit_avis = hour(avis_t) >= 18 |
      hour(avis_t) < 8,

    nuit_fin = hour(fin_t) >= 18 |
      hour(fin_t) < 8
  )



# Variables week-end ----------------------------------------------------

avis <- avis |>
  mutate(
    we_debut = wday(debut_t) %in% c(1, 7),

    we_avis = wday(avis_t) %in% c(1, 7),

    we_fin = wday(fin_t) %in% c(1, 7)
  )



# Durée entre arrivée et avis ------------------------------------------------------

avis <- avis |>
  mutate(
    delai_avis = avis_t - debut_t
  )


# Jour de semaine -------------------------------------------------------

avis <- avis |>
  mutate(
    sem_debut = wday(
      debut_t,
      label = TRUE,
      abbr = FALSE
    ),

    sem_avis = wday(
      avis_t,
      label = TRUE,
      abbr = FALSE
    ),

    sem_fin = wday(
      fin_t,
      label = TRUE,
      abbr = FALSE
    )
  )


# TODO: créer variable garde
# Inclure :
# - nuit
# - week-end
# - jours fériés



# Variables démographiques AVIS =========================================


# Âge ===================================================================

# Résumé statistique ----------------------------------------------------

summary(avis$age)

# Min.      : 15
# 1st Qu.   : 25
# Median    : 36
# Mean      : 39
# 3rd Qu.   : 50
# Max.      : 101

#* Pas d'âge aberrant détecté


# Distribution des âges -------------------------------------------------

plot_age <- avis |>
  ggplot(
    aes(x = age)
  ) +
  geom_histogram(
    binwidth = 5,
    fill = "#4E79A7",
    color = "white",
    alpha = 0.9
  ) +
  geom_vline(
    xintercept = median(
      avis$age,
      na.rm = TRUE
    ),
    color = "#E15759",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
      100,
      by = 10
    )
  ) +
  labs(
    title = "Distribution des âges",
    subtitle = "Population des avis psychiatriques",
    x = "Âge (années)",
    y = "Nombre d'avis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

plot_age


# NOTE: distribution légèrement asymétrique à droite
# Médiane à 36 ans
# Population relativement jeune



# Sexe ==================================================================

avis <- avis |>
  mutate(
    sexe_label = case_when(
      sexe == "1" ~ "M",
      sexe == "2" ~ "F",
      TRUE ~ NA_character_
    )
  )



# Code postal ===========================================================

# Conversion character --------------------------------------------------

avis <- avis |>
  mutate(
    cp = as.character(cp)
  )


# Correction zéros initiaux ---------------------------------------------

avis <- avis |>
  mutate(
    cp = str_pad(
      cp,
      width = 5,
      side = "left",
      pad = "0"
    )
  )



# Département de résidence ----------------------------------------------

avis <- avis |>
  mutate(
    cp_dep = case_when(

      # Étranger
      str_starts(cp, "99") ~ "ETRANGER",

      # DOM-TOM
      str_sub(cp, 1, 3) %in% c(
        "971",
        "972",
        "973",
        "974",
        "975",
        "976",
        "977",
        "978"
      ) ~ str_sub(cp, 1, 3),

      # Métropole
      TRUE ~ str_sub(cp, 1, 2)
    )
  )



#! Variables psychiatriques AVIS ========================================


# Diagnostics CIM-10 ----------------------------------------------------

# TODO: nettoyer dp
# Diagnostic principal

# conversion en character -----------------------------------------------

avis <- avis |>
  mutate(
    dp = as.character(dp)
  )

# Harmonisation ------------------------------------------------

avis <- avis |>
  mutate(
    dp = str_trim(dp),
    dp = str_to_upper(dp)
  )

# NOTE:
# - suppression espaces inutiles
# - harmonisation majuscules

# Vérification format CIM-10 -------------------------------------------

avis |>
  filter(
    !is.na(dp),
    !str_detect(dp, "^[A-Z][0-9]")
  ) |>
  count(
    dp,
    sort = TRUE
  )

# NOTE:
# Détecte les diagnostics au format inhabituel

# Fréquence diagnostics principaux -------------------------------------

avis |>
  count(
    dp,
    sort = TRUE
  )

# Première lettre CIM-10 -----------------------------------------------

avis <- avis |>
  mutate(
    dp_lettre = str_sub(dp, 1, 1)
  )

# Décompte par lettre CIM-10 -------------------------------------------

avis |>
  count(
    dp_lettre,
    sort = TRUE
  )

#   dp_lettre     n
#   <chr>     <int>
# 1 F         23544
# 2 R           931
# 3 Z           109
# 4 T            87
# 5 G            13
# 6 D             1

# Grandes classes CIM-10 psychiatriques ================================

avis <- avis |>
  mutate(
    dp_f_classe = case_when(

      str_starts(dp, "F0") ~ "F0 - Neurocognitif",

      str_starts(dp, "F1") ~ "F1 - Addictions",

      str_starts(dp, "F2") ~ "F2 - Psychotique",

      str_starts(dp, "F3") ~ "F3 - Humeur",

      str_starts(dp, "F4") ~ "F4 - Anxieux / stress",

      str_starts(dp, "F5") ~ "F5 - Comportement physiologique",

      str_starts(dp, "F6") ~ "F6 - Personnalité",

      str_starts(dp, "F7") ~ "F7 - Déficience intellectuelle",

      str_starts(dp, "F8") ~ "F8 - Développement",

      str_starts(dp, "F9") ~ "F9 - Enfance / adolescence",

      TRUE ~ NA_character_
    )
  )

# Décompte par grandes classes CIM-10 psychiatriques --------------------------------

avis |>
  count(
    dp_f_classe,
    sort = TRUE
  )

# Représentation graphique par grandes classes CIM-10 psychiatriques --------------------------------

avis |>
  count(
    dp_f_classe,
    sort = TRUE
  ) |>
  ggplot(
    aes(
      x = reorder(dp_f_classe, n),
      y = n
    )
  ) +
  geom_col(
    fill = "#4E79A7"
  ) +
  coord_flip() +
  labs(
    title = "Grandes classes diagnostiques psychiatriques",
    x = "Classe CIM-10",
    y = "Nombre d'avis"
  ) +
  theme_minimal(base_size = 14)
# TODO: nettoyer das
# Diagnostics associés

# TODO: créer catégories diagnostiques
# - suicidaire
# - psychotique
# - addictologique
# - thymique
# - anxieux



# Variables organisationnelles AVIS =====================================


# MLS - Mode légal de soins ---------------------------------------------

avis <- avis |>
  mutate(
    mls_label = case_when(
      mls == "1" ~ "SL",
      mls == "3" ~ "SPDRE",
      mls == "4" ~ "122-1",
      mls == "5" ~ "OPP",
      mls == "7" ~ "SPDT",
      mls == "8" ~ "SPPI",
      TRUE ~ "Inconnu"
    )
  )

avis |>
  count(
    mls_label,
    sort = TRUE
  )

# SL       : 22 450
# SPDT     : 1 428
# SPPI     :   488
# SPDRE    :   321
# 122-1    :     3
# OPP      :     1



# Harmonisation variables catégorielles =================================

# TODO: uniformiser modalités

# TODO: gérer accents

# TODO: gérer espaces

# TODO: gérer majuscules / minuscules

# TODO: regrouper modalités rares



# Valeurs manquantes AVIS ===============================================

# TODO: quantifier NA

# TODO: analyser patterns de NA

# TODO: identifier variables critiques incomplètes

# TODO: décisions méthodologiques



# Valeurs aberrantes AVIS ===============================================

# TODO: détecter âges aberrants

# TODO: détecter dates aberrantes

# TODO: détecter modalités incohérentes

# TODO: détecter codes CIM invalides

# TODO: détecter valeurs impossibles



# Variables dérivées AVIS ===============================================

# TODO: nombre d'avis par passage

# TODO: nombre d'avis par patient

# TODO: catégories diagnostiques

# TODO: variable suicidaire binaire

# TODO: variable psychotique binaire

# TODO: temporalité des avis



# Vérification finale AVIS ==============================================

# TODO: contrôle dimensions finales

# TODO: contrôle unicité

# TODO: contrôle qualité final

# TODO: sauvegarder avis_clean



#! Sauvegarde AVIS ======================================================

saveRDS(
  avis,
  here("data", "interim", "avis_clean.rds")
)