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

# ----------------------------------------------------------------------
# 4.1 Création de la cohorte
# ----------------------------------------------------------------------
#
# Objectif : Créer une table contenant une ligne par patient.
# Entrée   : pass_enrichi (1 ligne = 1 passage)
# Sortie   : cohort (1 ligne = 1 patient)
# Unité    : 1 ligne = 1 patient

cohort <-

  pass_enrichi |>

  distinct(
    id_patient
  )



# ----------------------------------------------------------------------
# 4.2 Contrôle qualité
# ----------------------------------------------------------------------
#
# Objectif : Vérifier que la cohorte contient une ligne par patient.
# Entrée   : cohort
# Sortie   : Messages de contrôle
# Unité    : 1 ligne = 1 patient

stopifnot(

  nrow(cohort) ==

    dplyr::n_distinct(pass_enrichi$id_patient)

)

message(
  "Nombre de patients : ",
  nrow(cohort)
)


# ======================================================================
# 5. Variables démographiques
# ======================================================================

# ----------------------------------------------------------------------
# 5.1 Création des variables démographiques
# ----------------------------------------------------------------------
#
# Objectif : Résumer les caractéristiques démographiques de chaque patient.
# Entrée   : pass_enrichi (1 ligne = 1 passage)
# Sortie   : cohort enrichie
# Unité    : 1 ligne = 1 patient

cohort_demo <-

  pass_enrichi |>

  arrange(
    id_patient,
    date_arrivee
  ) |>

  group_by(
    id_patient
  ) |>

  summarise(

    sexe =
      first(sexe),

    age =
      first(age),

    code_postal =
      first(code_postal),

    departement =
      first(departement),

    secteur_94 =
    first(secteur_94),

    secteur_f =
    first(secteur_f),

    hopital_secteur =
    first(hopital_secteur),

    .groups = "drop"

  )

cohort <-

  cohort |>

  left_join(

    cohort_demo,

    by = "id_patient"

  )



# ----------------------------------------------------------------------
# 5.2 Contrôle qualité
# ----------------------------------------------------------------------
#
# Objectif : Vérifier les variables démographiques.
# Entrée   : cohort
# Sortie   : Messages de contrôle
# Unité    : 1 ligne = 1 patient

stopifnot(

  nrow(cohort) ==

    nrow(cohort_demo)

)

message(
  "Variables démographiques ajoutées."
)


# ======================================================================
# 6. Variables de recours aux urgences
# ======================================================================

# ----------------------------------------------------------------------
# 6.1 Création des variables de recours
# ----------------------------------------------------------------------
# Objectif : Résumer le recours aux urgences de chaque patient.
# Entrée   : pass_enrichi (1 ligne = 1 passage)
# Sortie   : cohort enrichie
# Unité    : 1 ligne = 1 patient

cohort_recours <-

  pass_enrichi |>

  group_by(
    id_patient
  ) |>

  summarise(

    # Suivi

    premier_passage =
      min(date_arrivee),

    dernier_passage =
      max(date_arrivee),

    duree_suivi_jours =
      as.numeric(
        max(date_arrivee) -
          min(date_arrivee),
        units = "days"
      ),

    # Activité

    nb_passages =
      n(),

    nb_avis =
      sum(nb_avis, na.rm = TRUE),

    .groups =
      "drop"

  )

cohort <-

  cohort |>

  left_join(

    cohort_recours,

    by = "id_patient"

  )

# ----------------------------------------------------------------------
# 6.2 Contrôle qualité
# ----------------------------------------------------------------------
#
# Objectif : Vérifier les variables de recours.
# Entrée   : cohort
# Sortie   : Messages de contrôle
# Unité    : 1 ligne = 1 patient

stopifnot(

  nrow(cohort) ==

    nrow(cohort_recours)

)

message(
  "Variables de recours ajoutées."
)

# ======================================================================
# 7. Variables psychiatriques
# ======================================================================
# ----------------------------------------------------------------------
# 7.1 Agrégation des diagnostics au niveau patient
# ----------------------------------------------------------------------
#
# Objectif : Agréger l'ensemble des informations diagnostiques de chaque
#            patient avant le codage selon les méthodes de Schmoll et
#            Fleury.
#
# Entrée   : pass_enrichi (1 ligne = 1 passage)
# Sortie   : cohort_diag (1 ligne = 1 patient)
# Unité    : 1 ligne = 1 patient

cohort_diag <-

  pass_enrichi |>

  arrange(
    id_patient,
    date_arrivee
  ) |>

  group_by(
    id_patient
  ) |>

  summarise(

    # Diagnostics principaux du dernier avis de chaque passage

    diag_p_2_passages =
      list(
        diag_p_2_dernier_avis
      ),

    # Ensemble des diagnostics rencontrés
    # (tous les diagnostics associés de tous les passages)

    diag_t_2_patient =
      list(

        unique(

          unlist(
            diag_t_2_passage
          )

        )

      ),

    .groups =
      "drop"

  )

# ----------------------------------------------------------------------
# 7.2 Codage des diagnostics selon Schmoll et Fleury
# ----------------------------------------------------------------------
#
# Objectif : Définir les variables diagnostiques binaires selon les
#            méthodes de Schmoll et de Fleury.
#
# Schmoll :
#   - F2, F3, F4 et AUTRES sont mutuellement exclusifs.
#   - Le diagnostic dominant est le diagnostic principal le plus fréquent
#     parmi les passages.
#   - En cas d'égalité : F2 > F3 > F4 > AUTRES.
#   - F1 et F6 sont codés présents s'ils apparaissent au moins une fois
#     parmi tous les diagnostics du patient.
#
# Fleury :
#   - F2, F3, F4 et AUTRES sont indépendants.
#   - Ils sont codés présents s'ils apparaissent au moins une fois comme
#     diagnostic principal.
#   - F1 et F6 sont codés présents s'ils apparaissent au moins une fois
#     parmi tous les diagnostics du patient.

cohort_diag <-

  cohort_diag |>

  mutate(

    # ==============================================================
    # Schmoll
    # ==============================================================

    diag_dominant_schmoll =

      sapply(

        diag_p_2_passages,

        function(x) {

          x <-

            ifelse(

              x %in% c("F2", "F3", "F4"),

              x,

              "AUTRES"

            )

          freq <-

            table(

              factor(

                x,

                levels = c(
                  "F2",
                  "F3",
                  "F4",
                  "AUTRES"
                )

              )

            )

          names(freq)[which.max(freq)]

        }

      ),

diag_F2_schmoll =
  diag_dominant_schmoll == "F2",

diag_F3_schmoll =
  diag_dominant_schmoll == "F3",

diag_F4_schmoll =
  diag_dominant_schmoll == "F4",

diag_autres_schmoll =
  diag_dominant_schmoll == "AUTRES",

diag_F1_schmoll =

  sapply(
    diag_t_2_patient,
    function(x) "F1" %in% x
  ),

diag_F6_schmoll =

  sapply(
    diag_t_2_patient,
    function(x) "F6" %in% x
  ),



# ==============================================================
# Fleury
# ==============================================================

diag_F2_fleury =

  sapply(
    diag_p_2_passages,
    function(x) "F2" %in% x
  ),

diag_F3_fleury =

  sapply(
    diag_p_2_passages,
    function(x) "F3" %in% x
  ),

diag_F4_fleury =

  sapply(
    diag_p_2_passages,
    function(x) "F4" %in% x
  ),

diag_autres_fleury =

  sapply(

    diag_p_2_passages,

    function(x)

      any(

        !(x %in% c(
          "F2",
          "F3",
          "F4"
        ))

      )

  ),

diag_F1_fleury =

  sapply(
    diag_t_2_patient,
    function(x) "F1" %in% x
  ),

diag_F6_fleury =

  sapply(
    diag_t_2_patient,
    function(x) "F6" %in% x
  )

  )
# ----------------------------------------------------------------------
# 8.1 Variables d'hospitalisation
# ----------------------------------------------------------------------
#
# Objectif : Décrire le recours à l'hospitalisation psychiatrique au
#            niveau patient sur l'ensemble du suivi.
#
# hospitalisation :
#   TRUE si au moins une hospitalisation psychiatrique.
#
# nb_hospitalisation :
#   Nombre total d'hospitalisations psychiatriques.
#
# nb_hospitalisation_ssc :
#   Nombre total d'hospitalisations sous contrainte.
#
# prop_hospitalisation :
#   Proportion des passages ayant abouti à une hospitalisation.
#
# prop_hospitalisation_ssc :
#   Proportion des hospitalisations réalisées sous contrainte.

cohort_organisation <-

  pass_enrichi |>

  group_by(
    id_patient
  ) |>

summarise(

  nb_hospitalisation =
    sum(
      orientation_finale == "HOSPIT_PSY",
      na.rm = TRUE
    ),

  nb_hospitalisation_ssc =
    sum(
      orientation_finale == "HOSPIT_PSY" &
        mls_g == "SSC",
      na.rm = TRUE
    ),

  hospitalisation =
    nb_hospitalisation > 0,

  hospitalisation_sl =
    any(
      orientation_finale == "HOSPIT_PSY" &
        mls_g == "SL",
      na.rm = TRUE
    ),

  hospitalisation_ssc =
    nb_hospitalisation_ssc > 0,

  prop_hospitalisation =
    nb_hospitalisation / n(),

  prop_hospitalisation_ssc =
    if_else(
      nb_hospitalisation > 0,
      nb_hospitalisation_ssc / nb_hospitalisation,
      NA_real_
    ),

  orientation_finale =
    if_else(
      nb_hospitalisation > 0,
      "HOSPIT_PSY",
      "NON_ADMIS"
    ),

  .groups =
    "drop"

)
# ----------------------------------------------------------------------
# 8.2 Fusion avec la cohorte
# ----------------------------------------------------------------------

cohort <-

  cohort |>

  left_join(

    cohort_organisation,

    by = "id_patient"

  )

# ----------------------------------------------------------------------
# 8.3 Contrôles qualité
# ----------------------------------------------------------------------

stopifnot(
  nrow(cohort) == n_distinct(cohort$id_patient)
)

stopifnot(
  !anyDuplicated(cohort$id_patient)
)

stopifnot(
  all(
    cohort$nb_hospitalisation <= cohort$nb_passages
  )
)

stopifnot(
  all(
    cohort$nb_hospitalisation_ssc <=
      cohort$nb_hospitalisation
  )
)

stopifnot(
  all(
    cohort$prop_hospitalisation >= 0 &
      cohort$prop_hospitalisation <= 1
  )
)

stopifnot(
  all(
    is.na(cohort$prop_hospitalisation_ssc) |
      (
        cohort$prop_hospitalisation_ssc >= 0 &
          cohort$prop_hospitalisation_ssc <= 1
      )
  )
)

message(
  "Patients avec au moins une hospitalisation : ",
  sum(cohort$hospitalisation)
)

message(
  "Nombre total d'hospitalisations : ",
  sum(cohort$nb_hospitalisation)
)

message(
  "Nombre total d'hospitalisations SSC : ",
  sum(cohort$nb_hospitalisation_ssc)
)

# ======================================================================
# 9. Définition des Frequent Users
# ======================================================================
#
# Objectif     : Identifier les patients selon deux seuils de Frequent
#                User sur une fenêtre glissante de 365 jours.
#
# Entrée       : pass_enrichi
# Sortie       : cohort
# Objet créé   : nb_passages_365j_max, FU3, FU4
# Unité        : 1 ligne = 1 patient
#
# FU3 : au moins 3 passages sur une période de 365 jours.
# FU4 : au moins 4 passages sur une période de 365 jours.
#
# Le maximum est recherché sur l'ensemble des passages de chaque patient.

# ----------------------------------------------------------------------
# 9.1 Nombre maximal de passages sur 365 jours
# ----------------------------------------------------------------------

nb_passages_365j <-

  pass_enrichi |>

  arrange(
    id_patient,
    date_arrivee
  ) |>

  group_by(
    id_patient
  ) |>

  summarise(

    nb_passages_365j_max =
      max(
        sapply(
          date_arrivee,
          \(date_debut) {

            sum(
              date_arrivee >= date_debut &
                date_arrivee <= date_debut + lubridate::days(364)
            )

          }
        )
      ),

    .groups =
      "drop"

  )

# ----------------------------------------------------------------------
# 9.2 Ajout à la cohorte
# ----------------------------------------------------------------------

cohort <-

  cohort |>

  left_join(
    nb_passages_365j,
    by = "id_patient"
  )

# ----------------------------------------------------------------------
# 9.3 Définition des Frequent Users
# ----------------------------------------------------------------------
#
# Deux seuils sont conservés afin de pouvoir les comparer ultérieurement.

cohort <-

  cohort |>

  mutate(

    # Frequent User : au moins 3 passages sur 365 jours

    FU3 =
      nb_passages_365j_max >= 3,

    # Frequent User : au moins 4 passages sur 365 jours

    FU4 =
      nb_passages_365j_max >= 4

  )

# ----------------------------------------------------------------------
# 9.4 Contrôles qualité
# ----------------------------------------------------------------------

stopifnot(
  !anyNA(cohort$nb_passages_365j_max)
)

stopifnot(
  all(
    cohort$FU4 <= cohort$FU3
  )
)

stopifnot(
  all(
    !is.na(cohort$FU3)
  )
)

stopifnot(
  all(
    !is.na(cohort$FU4)
  )
)

# Effectifs ------------------------------------------------------------

message(
  "Nombre de patients : ",
  nrow(cohort)
)

message(
  "FU3 : ",
  sum(cohort$FU3),
  " patients (",
  round(
    100 * mean(cohort$FU3),
    1
  ),
  " %)"
)

message(
  "FU4 : ",
  sum(cohort$FU4),
  " patients (",
  round(
    100 * mean(cohort$FU4),
    1
  ),
  " %)"
)

# ----------------------------------------------------------------------
# 9.5 Comparaison FU3 / FU4
# ----------------------------------------------------------------------

cohort |>

  count(
    FU3,
    FU4
  )

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