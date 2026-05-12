#! ======================================================================
#! FU-SAU
#! 02_cleaning.R
#! Nettoyage des données PMSI psychiatriques
#! ======================================================================


# Activation environnement ------------------------------------------------

renv::activate()

# Packages nécessaires ---------------------------------------------------

library("tidyverse")
library("janitor")
library("skimr")
library("lubridate")
library("here")

# Import données brutes --------------------------------------------------

source(here("R", "01_import.R"))


#! ======================================================================
#! TABLE AVIS
#! Unité statistique = avis psychiatrique
#! ======================================================================


#? Structure générale AVIS -----------------------------------------------

# Dimensions
dim(avis)
# 24691 lignes (avis) pour 16 variables (avant nettoyage)
# OK

# Types des variables
glimpse(avis)
walk(
  names(avis),
  ~ cat(.x, ":", class(avis[[.x]]), "\n"))

#TODO idpat Identifiant patient anonymisé : numeric -> character

#TODO i_ddos : numeric - Identifiant du passage aux urgences -> character

#* sexe : character : OK

#TODO cp : numeric - Code postal de résidence -> character

#TODO type_sejour : character : à comprendre, mail à DUPORTAIL

#TODO secteur : character → character mais convertir selon standard XXGXX

#* mls : character → à convertir en abbréviation standard

# dp : character 

#* n_das : numeric 

# das : character 

#* age : numeric

#*  an : numeric - nombre d'avis par année

#TODO debu_tt : character - date/heure/minute Début du passage ou de l'hospitalisation associé à l'avis psychiatrique- à renommer et convertir en date-heure

#TODO fi_nt : character - date/heure/minute Fin du passage ou de l'hospitalisation associé à l'avis psychiatrique à renommer et convertir en date-heure

#TODO de_mt : character - date/heure/minute demande d'avis psychiatrique à renommer et convertir en date-heure
#NOTE :(peu pertinent car réalisé par le psychiatre, généralement très proche de la date/heure de l'avis) mais peut être utile pour vérifier la cohérence chronologique des événements (ex : pas d'avis avant le début du passage, pas d'avis après la fin du passage, etc.)

#TODO avi_st : character à renommer et convertir en date-heure

# Résumé statistique

# Données manquantes globales


# ======================================================================
# Identifiants AVIS
# ======================================================================

# i_ddos
# Identifiant du passage aux urgences

# idpat
# Identifiant patient anonymisé

# Vérification doublons

# Vérification cohérence patient/passages


# ======================================================================
# Variables temporelles AVIS
# ======================================================================

#? Nombre d'avis psychiatriques par année

# Tableau récapitulatif -------------------------------------------------

avis_par_an <- avis |>
  count(
    an,
    name = "n_avis"
  ) |>
  arrange(an)

avis_par_an

# Représentation graphique

avis_par_an |>
  ggplot(aes(x = an, y = n_avis)) +
  geom_col(fill = "#4E79A7", width = 0.7) +
  geom_text(aes(label = n_avis), vjust = -0.7, size = 4) +
  scale_x_continuous(breaks = avis_par_an$an) +
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

#? Correction des variables temporelles (nom et classe)

# Nom
avis <- avis |>
  rename(
    debut_t = debu_tt,
    fin_t = fi_nt,
    dem_t = de_mt,
    avis_t = avi_st
  )

# Classe
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

# dt_avis
# Date de l'avis psychiatrique


# ? Heure - variables dérivées ------------------------------------------


# heure_debut
# heure d'arrivée

avis <- avis |>
  mutate(
    heure_debut = hour(debut_t)
  )


# heure_avis
# heure de l'avis psychiatrique

avis <- avis |>
  mutate(
    heure_avis = hour(avis_t)
  )


# heure_fin
# heure de sortie

avis <- avis |>
  mutate(
    heure_fin = hour(fin_t)
  )



# ? Recours nocturnes - variables dérivées ------------------------------

# nuit_debut
# arrivée entre 18h et 8h

avis <- avis |>
  mutate(
    nuit_debut = hour(debut_t) >= 18 |
      hour(debut_t) < 8
  )


# nuit_avis
# avis psychiatrique entre 18h et 8h

avis <- avis |>
  mutate(
    nuit_avis = hour(avis_t) >= 18 |
      hour(avis_t) < 8
  )


# nuit_fin
# sortie entre 18h et 8h

avis <- avis |>
  mutate(
    nuit_fin = hour(fin_t) >= 18 |
      hour(fin_t) < 8
  )



# ? Week-end - variables dérivées ---------------------------------------
# we_debut
# arrivée un week-end

avis <- avis |>
  mutate(
    we_debut = wday(debut_t) %in% c(1, 7)
  )


# we_avis
# avis psychiatrique un week-end

avis <- avis |>
  mutate(
    we_avis = wday(avis_t) %in% c(1, 7)
  )


# we_fin
# sortie un week-end

avis <- avis |>
  mutate(
    we_fin = wday(fin_t) %in% c(1, 7)
  )

# delai_avis
# Délai passage -> avis
avis <- avis |>
  mutate(
    delai_avis = avis_t - debut_t
  )

#? jour de la semaine - variables dérivées
# sem_debut
# jour de la semaine de l'arrivée
avis <- avis |>
  mutate(
    sem_debut = wday(debut_t, label = TRUE, abbr = FALSE)
  )

# sem_avis
# jour de la semaine de l'avis
avis <- avis |>
  mutate(
    sem_avis = wday(avis_t, label = TRUE, abbr = FALSE)
  )

# sem_fin
# jour de la semaine de la sortie
avis <- avis |>
  mutate(
    sem_fin = wday(fin_t, label = TRUE, abbr = FALSE)
  )

#TODO : créér une variable "garde" incluant nuit, week-end et jours fériés
# ======================================================================
# Variables démographiques AVIS
# ======================================================================

# age - Âge du patient

#? Sommaire
summary(avis$age)
  #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #    15      25      36      39      50     101
  # → Pas d'âge aberrant = OK

#? Distribution des âges - représentation graphique

avis |>
  ggplot(aes(x = age)) +

  # Histogramme
  geom_histogram(
    binwidth = 5,
    fill = "#4E79A7",
    color = "white",
    alpha = 0.9
  ) +

  # Médiane
  geom_vline(
    xintercept = median(avis$age, na.rm = TRUE),
    color = "#E15759",
    linewidth = 1.2,
    linetype = "dashed"
  ) +

  # Axes
  scale_x_continuous(
    breaks = seq(0, 100, by = 10)
  ) +

  # Labels
  labs(
    title = "Distribution des âges",
    subtitle = "Population des avis psychiatriques",
    x = "Âge (années)",
    y = "Nombre d'avis"
  ) +

  # Thème
  theme_minimal(base_size = 14) +

  theme(
    plot.title = element_text(
      face = "bold"
    ),

    axis.title = element_text(
      face = "bold"
    )
  )
#NOTE : Distribution des âges légèrement asymétrique à droite, avec une médiane à 36 ans et une moyenne à 39 ans, indiquant une population relativement jeune mais avec une queue de distribution vers les âges plus avancés. Pas d'âge aberrant détecté.

# sexe
# Sexe du patient (1 = homme "M", 2 = femme "F")
avis <- avis |>
  mutate(
    sexe_label = case_when(
      sexe == "1" ~ "M",
      sexe == "2" ~ "F",
      TRUE ~ NA_character_
    )
  )

# cp
# Commune de résidence

# Convertir en character pour éviter les problèmes de codes postaux commençant par 0 et pour faciliter les regroupements ultérieurs
avis <- avis |>
  mutate(
    cp = as.character(cp)
  )

# Corriger ces codes postaux en les complétant avec des zéros à gauche si nécessaire
avis <- avis |>
  mutate(
    cp = str_pad(
      cp,
      width = 5,
      side = "left",
      pad = "0"
    )
  )

# cp_dep
# Département de résidence
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


# ======================================================================
# Variables psychiatriques AVIS
# ======================================================================

# motif_avis
# Motif de demande d'avis psychiatrique

# diagnostic_psy
# Diagnostic psychiatrique principal

# diagnostic_associe
# Diagnostics psychiatriques associés

# cim10
# Codes CIM-10 psychiatriques

# tentative_suicide
# Contexte suicidaire

# intoxication
# Intoxication / addictions

# agitation
# Agitation / trouble comportemental

# psychose
# Symptômes psychotiques

# humeur
# Trouble thymique

# anxiete
# Trouble anxieux


# ======================================================================
# Variables organisationnelles AVIS
# ======================================================================

# demandeur
# Service demandeur

# psychiatre
# Psychiatre évaluateur

# orientation_psy
# Orientation psychiatrique après avis

# hospitalisation_psy
# Hospitalisation psychiatrique

# MLS - Mode légal de soins
# Décompte et mutation en libellé standard
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

# SL        22450
# SPDT       1428
# SPPI        488
# SPDRE       321
# 122-1         3
# OPP           1

# mode_sortie_psy
# Modalité de sortie psychiatrique



# ======================================================================
# Harmonisation des variables catégorielles AVIS
# ======================================================================

# Uniformisation modalités

# Gestion accents

# Gestion espaces

# Gestion majuscules/minuscules

# Regroupement modalités rares


# ======================================================================
# Valeurs manquantes AVIS
# ======================================================================

# Quantification des NA

# Analyse des patterns de NA

# Variables critiques incomplètes

# Décisions méthodologiques


# ======================================================================
# Valeurs aberrantes AVIS
# ======================================================================

# Âges aberrants

# Dates aberrantes

# Modalités incohérentes

# Codes CIM invalides

# Valeurs impossibles


# ======================================================================
# Variables dérivées AVIS
# ======================================================================

# Nombre d'avis par passage

# Nombre d'avis par patient

# Catégories diagnostiques

# Variable suicidaire binaire

# Variable psychotique binaire

# Temporalité des avis


# ======================================================================
# Vérification finale AVIS
# ======================================================================

# Contrôle dimensions finales

# Contrôle unicité

# Contrôle qualité final

# Sauvegarde AVIS nettoyé



# ======================================================================
# TABLE PASS
# Unité statistique = passage aux urgences
# ======================================================================


# Structure générale PASS -----------------------------------------------

# Dimensions

# Types des variables

# Résumé statistique

# Données manquantes globales


# ======================================================================
# Identifiants PASS
# ======================================================================

# i_ddos
# Identifiant du passage

# idpat
# Identifiant patient anonymisé

# Vérification unicité passages

# Vérification multi-recours


# ======================================================================
# Variables démographiques PASS
# ======================================================================

# age
# Âge du patient

# sexe
# Sexe du patient

# commune
# Commune de résidence

# département
# Département de résidence

# couverture_sociale
# Couverture maladie / CMU / AME


# ======================================================================
# Variables temporelles PASS
# ======================================================================

# date_entree
# Date d'entrée aux urgences

# heure_entree
# Heure d'entrée

# date_sortie
# Date de sortie

# heure_sortie
# Heure de sortie

# duree_passage
# Durée de passage aux urgences

# passage_nocturne
# Passage de nuit

# week_end
# Passage week-end

# saison
# Saison du recours


# ======================================================================
# Variables médicales PASS
# ======================================================================

# motif_recours
# Motif de recours aux urgences

# diagnostic_principal
# Diagnostic principal

# diagnostic_associe
# Diagnostics associés

# cim10
# Codes CIM-10

# gravite
# Gravité clinique

# alcoolisation
# Alcoolisation aiguë

# intoxication
# Intoxication / toxiques

# tentative_suicide
# Tentative de suicide


# ======================================================================
# Variables organisationnelles PASS
# ======================================================================

# mode_entree
# Mode d'entrée aux urgences

# provenance
# Provenance du patient

# mode_sortie
# Mode de sortie

# orientation
# Orientation après urgences

# hospitalisation
# Hospitalisation post-urgences

# mutation
# Mutation intra-hospitalière


# ======================================================================
# Variables de recours PASS
# ======================================================================

# recours_repetes
# Passages répétés

# frequent_flyers
# Patients hyper-recourants

# delai_entre_passages
# Temporalité des recours


# ======================================================================
# Harmonisation des variables catégorielles PASS
# ======================================================================

# Uniformisation modalités

# Gestion accents

# Gestion espaces

# Gestion majuscules/minuscules

# Regroupement modalités rares


# ======================================================================
# Valeurs manquantes PASS
# ======================================================================

# Quantification des NA

# Analyse des patterns de NA

# Variables critiques incomplètes

# Décisions méthodologiques


# ======================================================================
# Valeurs aberrantes PASS
# ======================================================================

# Âges aberrants

# Durées aberrantes

# Dates incohérentes

# Modalités impossibles

# Valeurs extrêmes


# ======================================================================
# Variables dérivées PASS
# ======================================================================

# Catégories d'âge

# Temporalité des recours

# Variables psychiatriques synthétiques

# Variables organisationnelles synthétiques

# Variables analytiques finales


# ======================================================================
# Vérification relationnelle PASS / AVIS
# ======================================================================

# Cohérence jointure

# Nombre avis par passage

# Passages sans avis

# Avis sans passage


# ======================================================================
# Sauvegarde données nettoyées
# ======================================================================

# avis_clean

# pass_clean


# ======================================================================
# Contrôle qualité final global
# ======================================================================

# Vérification dimensions finales

# Vérification cohérence globale

# Résumé du cleaning

# Fin du script
# ======================================================================