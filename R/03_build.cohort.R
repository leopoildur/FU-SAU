# ======================================================================
# FU-SAU
# 03_build_cohort.R
# Construction de la cohorte analytique
# ======================================================================
#
# Objectif
# --------
# Construire une cohorte analytique au niveau patient à partir des
# données PASS et AVIS nettoyées.
#
# Une ligne de la table finale correspond à un patient.
#
# Le script :
#   - importe les données nettoyées ;
#   - vérifie leur cohérence ;
#   - fusionne PASS et AVIS ;
#   - construit les variables au niveau passage ;
#   - agrège les informations au niveau patient ;
#   - crée les variables analytiques ;
#   - définit les Frequent Users ;
#   - exporte la cohorte finale.
#
# Entrées
# --------
# data/processed/pass_clean.rds
# data/processed/avis_clean.rds
#
# Sorties
# --------
# data/processed/cohort.rds
# data/exports/cohort_pvalue.csv
#
# Auteur : Léopold ENGELSTEIN
# Date : 02/08/2026
# ======================================================================



# ======================================================================
# 0. Chargement du projet
# ======================================================================
# Pourquoi : Cette section initialise l'environnement R en chargeant les packages nécessaires et en sourçant les fonctions utilitaires du projet. Cela assure que toutes les dépendances sont disponibles avant de commencer le traitement des données.

source(here::here("R", "utils", "load_project.R"))

# ======================================================================
# 1. Import des données
# ======================================================================

# Pourquoi : Cette section charge les fichiers de données nettoyées (avis_clean.rds et pass_clean.rds)
#            qui ont été générés par les scripts de nettoyage précédents (02.1_cleaning_AVIS.R
#            et 02.2_cleaning_PASS.R). Ces fichiers sont la base de la construction de la cohorte.
# Entrée   : Fichiers `data/processed/avis_clean.rds` et `data/processed/pass_clean.rds`.
# Sortie   : Dataframes `avis` et `pass` chargés en mémoire.
# Unité statistique : `avis` (un avis), `pass` (un passage).

avis <- readRDS(here::here("data", "processed", "avis_clean.rds"))
pass <- readRDS(here::here("data", "processed", "pass_clean.rds"))


# ======================================================================
# 2. Contrôle qualité des données
# ======================================================================
#
# Vérifications :
#   - dimensions des jeux de données
#   - nombre de patients
#   - nombre de passages
#   - nombre d'avis
#   - passages sans avis
#   - avis sans passage
#   - nombre maximal d'avis par passage
#

# 2. Structure générale et contrôles préliminaires ======================

# Pourquoi : Avant de fusionner les données, il est crucial de vérifier les dimensions
#            des dataframes importés et la cohérence des identifiants (`id_passage`).
#            Cela permet d'identifier d'éventuels problèmes (passages sans avis ou avis sans passage)
#            qui pourraient affecter la qualité de la cohorte finale.
# Entrée   : Dataframes `avis` et `pass`.
# Sortie   : Messages d'information sur les dimensions et les incohérences d'identifiants.
# Unité statistique : `avis` (un avis), `pass` (un passage).

message("\n--- Contrôles préliminaires ---")
message("Dimensions avis_clean: ", paste(dim(avis), collapse = " x "))
message("Dimensions pass_clean: ", paste(dim(pass), collapse = " x "))

# Vérification des identifiants et cohérence
# Passages dans AVIS sans correspondance dans PASS
avis_sans_pass <- avis |>
  anti_join(pass, by = "id_passage")
message("Nombre d'avis sans correspondance dans PASS: ", nrow(avis_sans_pass))

# Passages dans PASS sans correspondance dans AVIS
pass_sans_avis <- pass |>
  anti_join(avis, by = "id_passage")
message("Nombre de passages sans correspondance dans AVIS: ", nrow(pass_sans_avis))


# ======================================================================
# 3. Fusion PASS + AVIS
# ======================================================================
#
# Objectif : Agréger les avis au niveau du passage puis les fusionner avec
#            les données PASS afin de créer une table enrichie.
#
# Entrées  : pass, avis
# Sortie   : pass_enrichi
# Unité    : 1 ligne = 1 passage

# ----------------------------------------------------------------------
# 3.1 Préparation des données AVIS
# ----------------------------------------------------------------------
#
# Objectif : Trier les avis chronologiquement avant leur agrégation.
# Entrée   : avis (1 ligne = 1 avis)
# Sortie   : avis trié
# Unité    : 1 ligne = 1 avis

avis <- avis |>

  arrange(
    id_passage,
    date_avis,
    heure_avis
  )

# ----------------------------------------------------------------------
# 3.2 Agrégation des avis au niveau passage
# ----------------------------------------------------------------------
#
# Objectif : Résumer les informations de tous les avis psychiatriques d'un
#            même passage dans une seule ligne.
# Entrée   : avis (1 ligne = 1 avis)
# Sortie   : avis_par_passage (1 ligne = 1 passage)
# Unité    : 1 ligne = 1 passage

avis_par_passage <- avis |>

  group_by(
    id_passage
  ) |>

  summarise(

    # Nombre d'avis psychiatriques

    nb_avis =
      n(),

    # Premier avis

    date_premier_avis =
      first(date_avis),

    diag_p_premier_avis =
      first(diag_p),

    diag_p_2_premier_avis =
      first(diag_p_2),

    diag_a_2_premier_avis =
      list(first(diag_a_2)),

    # Dernier avis

    date_dernier_avis =
      last(date_avis),

    diag_p_dernier_avis =
      last(diag_p),

    diag_p_2_dernier_avis =
      last(diag_p_2),

    diag_a_2_dernier_avis =
      list(last(diag_a_2)),

    # Tous les diagnostics associés du passage

    diag_t_2_passage =
      list(

        unlist(diag_t_2) |>
          na.omit() |>
          unique()

      ),

    # Dernières informations psychiatriques

    mls =
      last(mls),

    mls_f =
      last(mls_f),

    .groups =
      "drop"

  )

# ----------------------------------------------------------------------
# 3.3 Fusion avec les données PASS
# ----------------------------------------------------------------------
#
# Objectif : Enrichir les données PASS avec les informations issues des
#            avis psychiatriques agrégés.
# Entrée   : pass, avis_par_passage
# Sortie   : pass_enrichi
# Unité    : 1 ligne = 1 passage

pass <- pass |>
  select(
    -any_of(c(
      "nb_avis",
      "diag_t",
      "diag_a",
      "mls",
      "mls_f",
      "type_sejour"
    ))
  )

pass_enrichi <- pass |>

  left_join(

    avis_par_passage,

    by = "id_passage",

    relationship = "one-to-one"

  )

# Contrôle qualité ------------------------------------------------------

stopifnot(nrow(pass_enrichi) == nrow(pass))

stopifnot(
  all(pass$id_passage == pass_enrichi$id_passage)
)

message("Nombre de passages : ", nrow(pass_enrichi))

message(
  "Passages avec avis psychiatrique : ",
  sum(!is.na(pass_enrichi$nb_avis))
)

message(
  "Passages sans avis psychiatrique : ",
  sum(is.na(pass_enrichi$nb_avis))
)


# ======================================================================
# 4. Construction de la cohorte patient
# ======================================================================


# ======================================================================
# 5. Variables démographiques
# ======================================================================
# Pourquoi     : 
# Entrée       : 
# Sortie       : 
# Objet créé   : 
# Unité statistique : 


# ======================================================================
# 6. Variables de recours aux urgences
# ======================================================================
# Pourquoi     : 
# Entrée       : 
# Sortie       : 
# Objet créé   : 
# Unité statistique : 


# ======================================================================
# 7. Variables psychiatriques
# ======================================================================
# Pourquoi     : 
# Entrée       : 
# Sortie       : 
# Objet créé   : 
# Unité statistique : 
# Diagnostics
#
# Classes CIM-10
# Nombre de diagnostics
# Diagnostic majoritaire
# Pondération diagnostique
#



# ======================================================================
# 8. Variables organisationnelles
# ======================================================================
# Pourquoi     : 
# Entrée       : 
# Sortie       : 
# Objet créé   : 
# Unité statistique : 
#
# Secteur psychiatrique
# Hôpital de secteur
# Département
# Hospitalisation
# Orientation
#



# ======================================================================
# 9. Définition des Frequent Users
# ======================================================================
# Pourquoi     : 
# Entrée       : 
# Sortie       : 
# Objet créé   : 
# Unité statistique : 
#
# Définition principale
# Analyses complémentaires
# Fenêtre glissante
#



# ======================================================================
# 10. Contrôle qualité de la cohorte
# ======================================================================
# Pourquoi     : 
# Entrée       : 
# Sortie       : 
# Objet créé   : 
# Unité statistique : 
#
# Vérifications finales
# Statistiques descriptives
# Valeurs manquantes
# Cohérence des variables
#



# ======================================================================
# 11. Sauvegarde de la cohorte
# ======================================================================
# Pourquoi     : 
# Entrée       : 
# Sortie       : 
# Objet créé   : 
# Unité statistique : 



# ======================================================================
# 12. Export pour les analyses statistiques
# ======================================================================
# Pourquoi     : 
# Entrée       : 
# Sortie       : 
# Objet créé   : 
# Unité statistique : 


# ======================================================================
# Fin du script
# ======================================================================