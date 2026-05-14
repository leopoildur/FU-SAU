# ======================================================================
# FU-SAU
# Importation des données PMSI de 2021 à 2025
# CHU Henri Mondor
# ======================================================================

# Objectif du script :
# - importer les données PMSI depuis Excel
# - vérifier la qualité de l'importation
# - explorer la structure relationnelle des tables

# Tables importées :
# - LEGEND : dictionnaire des variables
# - PASS   : passages aux urgences psychiatriques
# - AVIS   : avis psychiatriques

# Auteur : Léopold ENGELSTEIN
# Date   : 10/05/2026
# ======================================================================


# Activation de l'environnement renv ------------------------------------

renv::activate()


# Packages nécessaires --------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(skimr)


# Définition du chemin du fichier ---------------------------------------

file_path <- here(
  "data",
  "raw",
  "FU-SAU_DATA.xlsx"
)

# Vérification du chemin
file_path


# Import de la table LEGEND ---------------------------------------------

legend <- read_excel(
  path = file_path,
  sheet = "LEGEND"
) |>
  clean_names()


# Vérification structure LEGEND -----------------------------------------

glimpse(legend)
nrow(legend)
names(legend)


# Import de la table PASS -----------------------------------------------

pass <- read_excel(
  path = file_path,
  sheet = "PASS"
) |>
  clean_names()


# Vérification structure PASS -------------------------------------------

glimpse(pass)
nrow(pass)
names(pass)


# Vérification unicité des passages -------------------------------------

pass |>
  summarise(
    n_lignes = n(),
    n_iddos_uniques = n_distinct(i_ddos)
  )

# Résultat attendu : 1 ligne = 1 passage unique


# Vérification unicité des patients -------------------------------------

pass |>
  summarise(
    n_patients = n_distinct(idpat)
  )


# Import de la table AVIS -----------------------------------------------

avis <- read_excel(
  path = file_path,
  sheet = "AVIS"
) |>
  clean_names()


# Vérification structure AVIS -------------------------------------------

glimpse(avis)
nrow(avis)
names(avis)


# Vérification structure relationnelle AVIS -----------------------------

avis |>
  summarise(
    n_lignes = n(),
    n_iddos_uniques = n_distinct(i_ddos)
  )

# Résultat attendu : plusieurs avis possibles pour un même passage


# Nombre d'avis par passage ---------------------------------------------

avis |>
  count(i_ddos, name = "n_avis") |>
  arrange(desc(n_avis))


# Vérification correspondance PASS / AVIS -------------------------------

avis_sans_pass <- avis |>
  anti_join(
    pass,
    by = "i_ddos"
  )

nrow(avis_sans_pass)

# Résultat attendu :
# 0 avis sans passage correspondant


# Résumé rapide des tables ----------------------------------------------

skim(pass)
skim(avis)

# Importation OK ! Etape suivante : nettoyage et préparation des données.

