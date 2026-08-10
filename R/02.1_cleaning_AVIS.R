# ======================================================================
# FU-SAU
# 02.1_cleaning_AVIS.R
# Nettoyage de la table AVIS
# ======================================================================

# Objectif :
#   - nettoyer les données issues de la table AVIS
#   - harmoniser les variables
#   - créer les variables dérivées
#   - contrôler la qualité des données
#   - sauvegarder une table prête pour les analyses
#
# Entrée :
#   data/interim/avis_raw.rds
#
# Sortie :
#   data/processed/avis_clean.rds
# ======================================================================


# Chargement du projet ==================================================
# Charger les packages, fonctions et constantes du projet.
# Importer la table AVIS brute.

source(
  here::here(
    "R",
    "utils",
    "load_project.R"
  )
)

avis <- readRDS(
  here::here(
    "data",
    "interim",
    "avis_raw.rds"
  )
)


# Structure générale ====================================================
# Décrire rapidement la table importée :
# - dimensions
# - types des variables
# - premières vérifications globales

## Dimensions ------------------------------------------------------------
# Vérifier le nombre de lignes et de variables.

dim(avis)


## Types des variables ---------------------------------------------------
# Vérifier le nom, le type et la structure des variables.

glimpse(avis)


## Contrôle global -------------------------------------------------------
# Résumer rapidement la qualité de la table avant nettoyage.

skim(avis)


# Identifiants ==========================================================
# Nettoyer les identifiants patients et passages.
# Vérifier leur cohérence et supprimer les doublons exacts.

# Renommage -------------------------------------------------------------
# Renommer les identifiants avec des noms explicites.

avis <- avis |>
  rename(
    id_patient = idpat,
    id_passage = i_ddos
  )


# Conversion ------------------------------------------------------------
# Convertir les identifiants au format caractère.

avis <- avis |>
  mutate(
    id_patient = as.character(id_patient),
    id_passage = as.character(id_passage)
  )


# Contrôles -------------------------------------------------------------

# Nombre de patients uniques
avis |>
  summarise(
    n_patients = n_distinct(id_patient)
  )

# Nombre de passages uniques
avis |>
  summarise(
    n_passages = n_distinct(id_passage)
  )

# Un passage ne doit correspondre qu'à un seul patient
avis |>
  distinct(
    id_passage,
    id_patient
  ) |>
  count(
    id_passage,
    name = "n_patients"
  ) |>
  filter(
    n_patients > 1
  )

# Un patient peut avoir plusieurs passages
avis |>
  count(
    id_patient,
    name = "n_passages"
  ) |>
  arrange(
    desc(n_passages)
  )


# Doublons --------------------------------------------------------------
# Identifier puis supprimer les doublons exacts.

# Nombre de doublons exacts
sum(duplicated(avis))

# Suppression des doublons exacts
avis <- avis |>
  distinct()

# Vérification
sum(duplicated(avis))

# Variables temporelles =================================================
# Convertir les dates.
# Vérifier leur cohérence.
# Créer les variables utiles pour les analyses temporelles :
# - heure
# - nuit
# - week-end
# - délai
# - jour de semaine
# - etc.

# Renommage -------------------------------------------------------------
# Renommer les variables temporelles pour expliciter leur contenu.

avis <- avis |>
  rename(
    date_arrivee = debu_tt,
    date_sortie = fi_nt,
    date_demande = de_mt,
    date_avis = avi_st
  )

stopifnot(is.character(avis$date_arrivee))

# Conversion ------------------------------------------------------------
# Convertir les dates et heures au format POSIXct.

avis <- avis |>
  mutate(
    date_arrivee = parse_datetime_fr(date_arrivee),
    date_sortie = parse_datetime_fr(date_sortie),
    date_demande = parse_datetime_fr(date_demande),
    date_avis = parse_datetime_fr(date_avis)
  )


# Contrôle --------------------------------------------------------------
# Vérifier le nombre de valeurs manquantes après conversion.

avis |>
  summarise(
    across(
      c(
        date_arrivee,
        date_sortie,
        date_demande,
        date_avis
      ),
      ~ sum(is.na(.))
    )
  )


# Variables dérivées ----------------------------------------------------
# Créer les variables temporelles utiles aux analyses.

avis <- avis |>
  mutate(

    # Heure de la journée
    heure_arrivee = get_hour(date_arrivee),
    heure_avis = get_hour(date_avis),
    heure_sortie = get_hour(date_sortie),

    # Passage de nuit
    nuit_arrivee = is_night(date_arrivee),
    nuit_avis = is_night(date_avis),
    nuit_sortie = is_night(date_sortie),

    # Passage le week-end
    weekend_arrivee = is_weekend(date_arrivee),
    weekend_avis = is_weekend(date_avis),
    weekend_sortie = is_weekend(date_sortie),

    # Jour de la semaine
    jour_arrivee = weekday_label(date_arrivee),
    jour_avis = weekday_label(date_avis),
    jour_sortie = weekday_label(date_sortie),

    # Délais
    delai_avis = date_avis - date_arrivee,
    LOS = date_sortie - date_arrivee
  )

# Garde -----------------------------------------------------------------
# Identifier les avis réalisés pendant une garde.

avis <- avis |>
  mutate(
    ferie = is_holiday(date_avis),
    garde = nuit_avis | weekend_avis | ferie
  )

# Contrôle

avis |>
  count(
    garde,
    sort = TRUE
  )

avis |>
  count(
    ferie
  )


# Cohérence chronologique -----------------------------------------------
# Vérifier l'ordre logique des événements.

avis |>
  filter(date_sortie < date_arrivee)

# = 0

avis |>
  filter(date_demande < date_arrivee)

# = 6
# CàD 6 passages ou la demande d'avis précède l'arrivée.
# Probable erreur de saisie
# Peu d'impact, négligeable, enregistré pour analyse ultérieure

anomalies_demande <- avis |>
  filter(
    date_demande < date_arrivee
  )

avis |>
  filter(date_avis < date_arrivee)

# = 16
# CàD 16 avis précédant l'arrivée
# Probable erreur de saisie
# Peu d'impact, négligeable, enregistré pour analyse ultérieure

anomalies_avis_avant <- avis |>
  filter(
    date_avis < date_arrivee
  )

avis |>
  filter(date_avis > date_sortie)

# = 1068
# CàD 1068 avis édités après la sortie
# Evaluation rédigée ou corrigée après la sortie
# Pas d'impact sur la durée de séjour
# A enregistrer pour vérification (exclure comme donnée aberrant si delta trop important

anomalies_avis_apres <- avis |>
  filter(
    date_avis > date_sortie
  )


# Résumé descriptif -----------------------------------------------------
# Décrire rapidement les durées créées (en min)

summary(as.numeric(avis$delai_avis))

summary(as.numeric(avis$LOS))




# Variables démographiques ==============================================
# Nettoyer les variables décrivant les patients :
# - âge
# - sexe
# - code postal
# - département
# Vérifier les valeurs aberrantes.

# Âge -------------------------------------------------------------------
# Vérifier la distribution de l'âge et rechercher d'éventuelles valeurs
# aberrantes.

summary(avis$age)

avis |>
  ggplot(
    aes(x = age)
  ) +
  geom_histogram(
    binwidth = 5,
    fill = "#4E79A7",
    color = "white"
  ) +
  labs(
    title = "Distribution des âges",
    x = "Âge (années)",
    y = "Nombre d'avis"
  ) +
  theme_fu()


# Sexe ------------------------------------------------------------------
# Remplacer le codage PMSI par des modalités explicites.

avis <- avis |>
  mutate(
    sexe = get_sexe_label(sexe)
  )

avis |>
  count(
    sexe,
    sort = TRUE
  )


# Code postal -----------------------------------------------------------
# Renommer la variable et harmoniser son format.

avis <- avis |>
  rename(
    code_postal = cp
  ) |>
  mutate(
    code_postal = as.character(code_postal),
    code_postal = stringr::str_pad(
      code_postal,
      width = 5,
      side = "left",
      pad = "0"
    )
  )


# Département -----------------------------------------------------------
# Déduire le département de résidence à partir du code postal.

avis <- avis |>
  mutate(
    departement = get_department(code_postal)
  )


avis |>
  count(
    departement,
    sort = TRUE
  )


# Contrôles -------------------------------------------------------------
# Vérifier la cohérence des variables démographiques.

avis |>
  filter(
    age < 15 |
      age > 110
  )

avis |>
  summarise(
    across(
      c(
        age,
        sexe,
        code_postal,
        departement
      ),
      ~ sum(is.na(.))
    )
  )

# Variables psychiatriques ==============================================

# Diagnostic principal --------------------------------------------------
# Renommer et harmoniser le diagnostic principal CIM-10.

avis <- avis |>
  rename(
    diag_p = dp
  ) |>
  mutate(
    diag_p = stringr::str_trim(diag_p),
    diag_p = stringr::str_to_upper(diag_p)
  )

# Distribution des diagnostics principaux
avis |>
  count(
    diag_p,
    sort = TRUE
  )


# Diagnostics associés --------------------------------------------------
# Renommer les variables relatives aux diagnostics associés.

avis <- avis |>
  rename(
    nb_diag_a = n_das,
    diag_a = das
  ) |>
  mutate(
    diag_a = stringr::str_trim(diag_a),
    diag_a = stringr::str_to_upper(diag_a)
  )

# Distribution du nombre de diagnostics associés
avis |>
  count(
    nb_diag_a,
    sort = TRUE
  )


# Variables dérivées ----------------------------------------------------
# Créer les niveaux de codage du diagnostic principal.

avis <- avis |>
  mutate(

    # Chapitre CIM-10 (1 caractère)
    diag_p_1 = stringr::str_sub(
      diag_p,
      1,
      1
    ),

    # Classe CIM-10 (2 caractères)
    diag_p_2 = stringr::str_sub(
      diag_p,
      1,
      2
    )

  )

    # Sous-classe CIM-10 (3 caractères)
avis <- avis |>
  mutate(
    diag_p_3 = stringr::str_sub(
      diag_p,
      1,
      3
    )
  )

# Contrôles -------------------------------------------------------------
# Vérifier la qualité des diagnostics.

# Répartition des chapitres CIM-10

avis |>
  count(
    diag_p_1,
    sort = TRUE
  )

# Répartition des classes CIM-10

avis |>
  count(
    diag_p_2,
    sort = TRUE
  )

# Valeurs manquantes
avis |>
  summarise(
    across(
      c(
        diag_p,
        diag_a
      ),
      ~ sum(is.na(.))
    )
  )

# Codes CIM-10 de longueur anormale
avis |>
  filter(
    nchar(diag_p) < 3
  )

# = 0

# Diagnostics ne commençant pas par une lettre
avis |>
  filter(
    !stringr::str_detect(
      diag_p,
      "^[A-Z]"
    )
  )

# = 0

# Diagnostics associés ==================================================

# Nettoyage -------------------------------------------------------------

avis <- avis |>
  mutate(

    # Suppression des espaces multiples
    diag_a = stringr::str_squish(diag_a),

    # Chaînes vides -> NA
    diag_a = na_if(diag_a, "")

  )


# Transformation en liste -----------------------------------------------

avis <- avis |>
  mutate(

    diag_a = stringr::str_split(
      diag_a,
      pattern = "\\s+"
    )

  )


# Remplacement des NA par des listes vides ------------------------------

avis$diag_a <-
  lapply(
    avis$diag_a,
    function(x) {

      if (length(x) == 1 && is.na(x)) {

        character(0)

      } else {

        x

      }

    }
  )


# Variables dérivées ----------------------------------------------------

    # Famille CIM-10 (1 caractèrs)

avis <- avis |>
  mutate(

    diag_a_1 = lapply(
      diag_a,
      function(x) stringr::str_sub(x, 1, 1)
    ),

    # Classe CIM-10 (2 caractères)

    diag_a_2 = lapply(
      diag_a,
      function(x) stringr::str_sub(x, 1, 2)
    )

  )

    # Sous-classe CIM-10 (3 caractères)

avis <- avis |>
  mutate(

    diag_a_3 = lapply(
      diag_a,
      function(x) {

        stringr::str_sub(
          x,
          1,
          3
        )

      }

    )

  )


# Contrôles -------------------------------------------------------------

# Nombre réel de diagnostics associés

avis <- avis |>
  mutate(
    nb_diag_a_calcule = lengths(diag_a)
  )


# Comparaison avec la variable PMSI

avis |>
  count(
    nb_diag_a,
    nb_diag_a_calcule
  )


# Nombre de dossiers sans diagnostic associé

sum(
  lengths(avis$diag_a) == 0
)


# Distribution du nombre de diagnostics associés

table(
  lengths(avis$diag_a)
)


# Aperçu

avis |>
  select(
    diag_a,
    diag_a_1,
    diag_a_2
  ) |>
  slice_head(
    n = 10
  )

# Diagnostics totaux ==================================================


avis <- avis |>
  mutate(

    diag_t = purrr::map2(
      diag_p,
      diag_a,
      function(dp, das) {

        unique(
          c(
            dp,
            das
          )
        )

      }

    )

  )

avis <- avis |>
  mutate(

    diag_t_1 = lapply(
      diag_t,
      function(x) {

        stringr::str_sub(
          x,
          1,
          1
        )

      }

    )

  )

avis <- avis |>
  mutate(

    diag_t_2 = lapply(
      diag_t,
      function(x) {

        stringr::str_sub(
          x,
          1,
          2
        )

      }

    )

  )

avis <- avis |>
  mutate(

    diag_t_3 = lapply(
      diag_t,
      function(x) {

        stringr::str_sub(
          x,
          1,
          3
        )

      }

    )

  )

avis <- avis |>
  mutate(
    nb_diag_t = lengths(diag_t)
  )

avis |>
  count(
    nb_diag_t,
    sort = TRUE
  )

avis |>
  select(
    diag_p,
    diag_a,
    diag_t,
    nb_diag_t
  ) |>
  slice_head(
    n = 10
  )

# Variables organisationnelles ==========================================

# Mode légal de soins ---------------------------------------------------
# Nettoyer et vérifier la variable MLS.

avis <- avis |>
  mutate(
    mls_f = get_mls_label(mls)
  )

# Regroupement du mode légal de soins ===================================

avis <- avis |>
  mutate(

    mls_g = case_when(

      mls_f == "SL" ~ "SL",

      mls_f %in% c(
        "SPDT",
        "SPPI",
        "SPDRE",
        "OPP",
        "DETENUS",
        "PENAL"
      ) ~ "SSC",

    )

  )


# Contrôles -------------------------------------------------------------

# Distribution des modes légaux de soins
avis |>
  count(
    mls_f,
    sort = TRUE
  )

# Correspondance PMSI ↔ libellés
avis |>
  count(
    mls,
    mls_f,
    sort = TRUE
  )

# Vérification des valeurs non codées
avis |>
  filter(
    mls_f == "INCONNU"
  )

# Distribution des groupes

avis |>
  count(
    mls_g,
    sort = TRUE
  )

# Correspondance avec le codage détaillé

avis |>
  count(
    mls_f,
    mls_g,
    sort = TRUE
  )

# Vérification des valeurs non classées

avis |>
  filter(
    mls_g == "INCONNU"
  )

# Secteur psychiatrique -------------------------------------------------
# Nettoyer la variable secteur.

avis <- avis |>
  mutate(
    secteur = stringr::str_squish(secteur)
  )

# Distribution
avis |>
  count(
    secteur,
    sort = TRUE
  )

# Secteur géographique 3 variables (94, IDF hors 94, Hors IDF)

avis <- avis |>
  mutate(

    secteur_f = dplyr::case_when(

      # Secteurs du Val-de-Marne
      stringr::str_detect(secteur, "SECTEUR\\s+[0-9]{2}$") ~
        paste0(
          "94G",
          stringr::str_extract(secteur, "[0-9]{2}$")
        ),

      # IDF hors secteur 94
      stringr::str_detect(secteur, "IDF") ~
        "IDF hors 94",

      # Tout le reste
      TRUE ~
        "Hors IDF"

    )

  )






# Contrôle qualité dans utils/checks.R ======================================================
# Vérifier la qualité finale de la table :
# - dimensions
# - valeurs manquantes
# - doublons
# - cohérence des variables
# - contrôle des identifiants


# Sauvegarde ============================================================

# Création du dossier si nécessaire
dir.create(
  here::here(
    "data",
    "processed"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)

# Sauvegarde de la table nettoyée
saveRDS(
  avis,
  here::here(
    "data",
    "processed",
    "avis_clean.rds"
  )
)

# Export optionnel en CSV (utile pour vérifier les données)
# write_csv(
#   avis,
#   here::here(
#     "data",
#     "processed",
#     "avis_clean.csv"
#   )
# )


# Fin du script =========================================================

gitmessage("======================================================")
message(" Nettoyage de la table AVIS terminé avec succès")
message("------------------------------------------------------")
message(" Nombre de lignes   : ", nrow(avis))
message(" Nombre de variables: ", ncol(avis))
message(" Nombre de patients : ", n_distinct(avis$id_patient))
message(" Nombre de passages : ", n_distinct(avis$id_passage))
message(" Fichier sauvegardé : data/processed/avis_clean.rds")
message("======================================================")
