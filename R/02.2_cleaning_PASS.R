# ======================================================================
# FU-SAU
# 02.2_cleaning_PASS.R
# Nettoyage de la table PASS
# ======================================================================

# Objectif :
#   - nettoyer les données issues de la table PASS
#   - harmoniser les variables avec AVIS (mêmes noms, même logique)
#   - nettoyer les variables spécifiques à PASS (RUM, destination)
#   - contrôler la qualité des données
#   - sauvegarder une table prête pour la fusion
#
# Entrée :
#   data/interim/pass_raw.rds
#
# Sortie :
#   data/processed/pass_clean.rds
# ======================================================================


# Chargement du projet ==================================================
# Charger les packages, fonctions et constantes du projet.
# Importer la table PASS brute.

source(here::here("R", "utils", "load_project.R"))

pass <- readRDS(
  here::here("data", "interim", "pass_raw.rds")
)


# Structure générale ====================================================
# Décrire rapidement la table importée.

## Dimensions ------------------------------------------------------------

dim(pass)


## Types des variables ---------------------------------------------------

glimpse(pass)


## Contrôle global -------------------------------------------------------

skim(pass)


# Identifiants ==========================================================
# Renommer, convertir et contrôler les identifiants.

## Renommage -------------------------------------------------------------

pass <- pass |>
  rename(
    id_patient = idpat,
    id_passage = i_ddos
  )


## Conversion ------------------------------------------------------------

pass <- pass |>
  mutate(
    id_patient = as.character(id_patient),
    id_passage = as.character(id_passage)
  )


## Contrôles -------------------------------------------------------------

# Nombre de patients uniques
pass |>
  summarise(
    n_patients = n_distinct(id_patient)
  )

# Nombre de passages uniques
pass |>
  summarise(
    n_passages = n_distinct(id_passage)
  )

# Un passage ne doit correspondre qu'à un seul patient
pass |>
  distinct(id_passage, id_patient) |>
  count(id_passage, name = "n_patients") |>
  filter(n_patients > 1)

# Un patient peut avoir plusieurs passages
pass |>
  count(id_patient, name = "n_passages") |>
  arrange(desc(n_passages))


## Doublons --------------------------------------------------------------

sum(duplicated(pass))

pass <- pass |>
  distinct()

sum(duplicated(pass))


# Variables temporelles =================================================
# Renommer, convertir et créer les variables temporelles.
# PASS a deux dates d'avis : premier et dernier.

## Renommage -------------------------------------------------------------

pass <- pass |>
  rename(
    date_arrivee      = debu_tt,
    date_sortie       = fi_nt,
    date_demande      = de_mt,
    date_premier_avis = avi_st_1,
    date_dernier_avis = avi_st_n
  )

stopifnot(is.character(pass$date_arrivee))


## Conversion ------------------------------------------------------------

pass <- pass |>
  mutate(
    date_arrivee      = parse_datetime_fr(date_arrivee),
    date_sortie       = parse_datetime_fr(date_sortie),
    date_demande      = parse_datetime_fr(date_demande),
    date_premier_avis = parse_datetime_fr(date_premier_avis),
    date_dernier_avis = parse_datetime_fr(date_dernier_avis)
  )


## Contrôle --------------------------------------------------------------

pass |>
  summarise(
    across(
      c(date_arrivee, date_sortie, date_demande, date_premier_avis, date_dernier_avis),
      ~ sum(is.na(.))
    )
  )


## Variables dérivées ----------------------------------------------------

pass <- pass |>
  mutate(

    # Heure de la journée
    heure_arrivee      = get_hour(date_arrivee),
    heure_demande      = get_hour(date_demande),
    heure_premier_avis = get_hour(date_premier_avis),
    heure_dernier_avis = get_hour(date_dernier_avis),
    heure_sortie       = get_hour(date_sortie),

    # Passage de nuit
    nuit_arrivee      = is_night(date_arrivee),
    nuit_demande      = is_night(date_demande),
    nuit_premier_avis = is_night(date_premier_avis),
    nuit_dernier_avis = is_night(date_dernier_avis),
    nuit_sortie       = is_night(date_sortie),

    # Passage le week-end
    weekend_arrivee      = is_weekend(date_arrivee),
    weekend_demande      = is_weekend(date_demande),
    weekend_premier_avis = is_weekend(date_premier_avis),
    weekend_dernier_avis = is_weekend(date_dernier_avis),
    weekend_sortie       = is_weekend(date_sortie),

    # Jour de la semaine
    jour_arrivee      = weekday_label(date_arrivee),
    jour_premier_avis = weekday_label(date_premier_avis),
    jour_sortie       = weekday_label(date_sortie)

  )


## Garde -----------------------------------------------------------------
# Basée sur le premier avis (même logique que AVIS).

pass <- pass |>
  mutate(
    ferie = is_holiday(date_premier_avis),
    garde = nuit_premier_avis | weekend_premier_avis | ferie
  )

pass |>
  count(garde, sort = TRUE)


## Délais ----------------------------------------------------------------

pass <- pass |>
  mutate(
    delai_premier_avis = date_premier_avis - date_arrivee,
    LOS                = date_sortie - date_arrivee
  )


## Cohérence chronologique -----------------------------------------------

pass |>
  filter(date_sortie < date_arrivee)

pass |>
  filter(date_demande < date_arrivee)

pass |>
  filter(date_premier_avis < date_arrivee)

pass |>
  filter(date_premier_avis > date_sortie)


## Résumé descriptif -----------------------------------------------------

summary(as.numeric(pass$delai_premier_avis))

summary(as.numeric(pass$LOS))


# Variables démographiques ==============================================
# Même traitement que AVIS.

## Âge -------------------------------------------------------------------

summary(pass$age)


## Sexe ------------------------------------------------------------------

pass <- pass |>
  mutate(
    sexe = get_sexe_label(sexe)
  )

pass |>
  count(sexe, sort = TRUE)


## Code postal -----------------------------------------------------------

pass <- pass |>
  rename(code_postal = cp) |>
  mutate(
    code_postal = as.character(code_postal),
    code_postal = stringr::str_pad(
      code_postal,
      width = 5,
      side  = "left",
      pad   = "0"
    )
  )


## Département -----------------------------------------------------------

pass <- pass |>
  mutate(
    departement = get_department(code_postal)
  )

pass |>
  count(departement, sort = TRUE)


## Contrôles -------------------------------------------------------------

pass |>
  filter(age < 15 | age > 110)

pass |>
  summarise(
    across(
      c(age, sexe, code_postal, departement),
      ~ sum(is.na(.))
    )
  )


# Variables psychiatriques (1er avis) ===================================
# Les variables psychiatriques de PASS correspondent au premier avis.
# On conserve les mêmes noms que dans AVIS pour faciliter la fusion.

## Diagnostic principal --------------------------------------------------

pass <- pass |>
  rename(
    diag_p   = dp_avis1,
    nb_diag_a = n_das_avis1,
    diag_a   = das_avis1
  ) |>
  mutate(
    diag_p = stringr::str_trim(diag_p),
    diag_p = stringr::str_to_upper(diag_p)
  )

pass |>
  count(diag_p, sort = TRUE)


## Niveaux CIM-10 du diagnostic principal --------------------------------

pass <- pass |>
  mutate(
    diag_p_1 = stringr::str_sub(diag_p, 1, 1),
    diag_p_2 = stringr::str_sub(diag_p, 1, 2),
    diag_p_3 = stringr::str_sub(diag_p, 1, 3)
  )


## Diagnostics associés --------------------------------------------------

pass <- pass |>
  mutate(
    diag_a = stringr::str_squish(diag_a),
    diag_a = na_if(diag_a, ""),
    diag_a = stringr::str_split(diag_a, pattern = "\\s+")
  )

pass$diag_a <- lapply(pass$diag_a, function(x) {
  if (length(x) == 1 && is.na(x)) character(0) else x
})

pass <- pass |>
  mutate(
    diag_a_1 = lapply(diag_a, function(x) stringr::str_sub(x, 1, 1)),
    diag_a_2 = lapply(diag_a, function(x) stringr::str_sub(x, 1, 2)),
    diag_a_3 = lapply(diag_a, function(x) stringr::str_sub(x, 1, 3))
  )


## Diagnostics totaux (principal + associés) -----------------------------

pass <- pass |>
  mutate(
    diag_t = purrr::map2(diag_p, diag_a, function(dp, das) unique(c(dp, das))),
    diag_t_1 = lapply(diag_t, function(x) stringr::str_sub(x, 1, 1)),
    diag_t_2 = lapply(diag_t, function(x) stringr::str_sub(x, 1, 2)),
    diag_t_3 = lapply(diag_t, function(x) stringr::str_sub(x, 1, 3)),
    nb_diag_t = lengths(diag_t)
  )


## Contrôles -------------------------------------------------------------

pass |>
  summarise(
    across(c(diag_p, diag_a), ~ sum(is.na(.)))
  )

pass |>
  filter(nchar(diag_p) < 3)

pass |>
  filter(!stringr::str_detect(diag_p, "^[A-Z]"))


# Variables organisationnelles ==========================================
# Même traitement que AVIS.

## Mode légal de soins ---------------------------------------------------

pass <- pass |>
  mutate(
    mls_f = get_mls_label(mls),
    mls_g = case_when(
      mls_f == "SL" ~ "SL",
      mls_f %in% c("SPDT", "SPPI", "SPDRE", "OPP", "DETENUS", "PENAL") ~ "SSC",
    )
  )

pass |>
  count(mls_f, sort = TRUE)

pass |>
  count(mls_g, sort = TRUE)


## Secteur psychiatrique -------------------------------------------------

pass <-

  pass |>

  left_join(
    dictionnaire_secteurs,
    by = "code_postal"
  ) |>

  mutate(

    departement = substr(code_postal, 1, 2),

    # Secteur psychiatrique détaillé -----------------------------------

    secteur_f = case_when(

      !is.na(secteur_f) ~ secteur_f,

      departement %in% c(
        "75", "77", "78", "91", "92", "93", "95"
      ) ~ "IDF_HORS_94",

      TRUE ~ "HORS_IDF"

    ),

    # Établissement support --------------------------------------------

    hopital_secteur = case_when(

      !is.na(hopital_secteur) ~ hopital_secteur,

      TRUE ~ "AUTRES"

    ),

    # Variable simplifiée pour les analyses ----------------------------

    secteur_94 = case_when(

      stringr::str_starts(secteur_f, "94G") ~ secteur_f,

      TRUE ~ "HORS_94"

    )

  )

patients_changement_secteur <-

  pass |>

  group_by(id_patient) |>

  summarise(

    n_secteurs =
      n_distinct(secteur_f),

    .groups = "drop"

  )

patients_changement_secteur |>

  count(
    n_secteurs
  )

# Le secteur psychiatrique de résidence est resté identique pour 98,1 % des patients au cours du suivi. 
# Les changements de secteur (1,9 %) ont été considérés comme négligeables ; 
# les variables de sectorisation retenues dans la cohorte correspondent donc au secteur du premier passage.

# Contrôle qualité ------------------------------------------------------

message("\nSecteur détaillé :")

pass |>

  count(
    secteur_f,
    sort = TRUE
  ) |>

  print(n = Inf)


message("\nSecteur simplifié :")

pass |>

  count(
    secteur_94,
    sort = TRUE
  ) |>

  print(n = Inf)


message("\nÉtablissement support :")

pass |>

  count(
    hopital_secteur,
    sort = TRUE
  ) |>

  print(n = Inf)

# Variables spécifiques à PASS ==========================================
# Ces variables n'existent pas dans AVIS.

# ## Type de séjour --------------------------------------------------------
# # 9 = sans hospitalisation, 11 = avec hospitalisation.

# pass <- pass |>
#   mutate(
#     type_sejour_f = case_when(
#       type_sejour == "9"  ~ "SANS_HOSPIT",
#       type_sejour == "11" ~ "AVEC_HOSPIT",
#       TRUE                ~ "INCONNU"
#     )
#   )

# pass |>
#   count(type_sejour_f, sort = TRUE)


## Nombre d'avis par passage ---------------------------------------------

pass <- pass |>
  rename(nb_avis = nb_avis) |>
  mutate(
    nb_avis = as.integer(nb_avis),
    passage_multiple = nb_avis > 1
  )

pass |>
  count(nb_avis, sort = TRUE)

pass |>
  count(passage_multiple)


# ## RUM (Résumé d'Unité Médicale) -----------------------------------------
# # Présent uniquement pour les passages avec hospitalisation.

# pass <- pass |>
#   rename(
#     date_entree_rum = de_rt,
#     date_sortie_rum = ds_rt
#   ) |>
#   mutate(
#     date_entree_rum = parse_datetime_fr(date_entree_rum),
#     date_sortie_rum = parse_datetime_fr(date_sortie_rum)
#   )

# # Nettoyage du diagnostic principal RUM
# pass <- pass |>
#   mutate(
#     dp_rum = stringr::str_trim(dp_rum),
#     dp_rum = stringr::str_to_upper(dp_rum)
#   )

# pass |>
#   count(dp_rum, sort = TRUE) |>
#   head(15)

# pass |>
#   count(um_rum, sort = TRUE)

# summary(pass$duree_rum_urg)


## Destination de sortie -------------------------------------------------
# Recodage de SORT_LONGTEXT en catégories analytiques.

pass <- pass |>
  mutate(
    destination = get_destination_label(sort_longtext)
  )

pass |>
  count(destination, sort = TRUE)

# Correspondance avec les libellés bruts
pass |>
  count(sort_longtext, destination, sort = TRUE)

# ----------------------------------------------------------------------
# Orientation finale
# ----------------------------------------------------------------------

pass <- pass |>

  mutate(

    orientation_finale = case_when(

      sort_longtext %in% c(

        "a - transfert vers psy d'un autre hopital",
        "a - transfert vers une unite psy du meme hopital",
        "ut1 - transfert - absence de lit",
        "ut2 - transfert - absence de la specialite",
        "ut4 - transfert - volonte du patient",
        "ut5 - transfert - hors de la circonscription"

      ) ~ "HOSPIT_PSY",

      TRUE ~ "NON_ADMIS"

    )

  )

## Mode d'entrée et de sortie de l'hospitalisation ----------------------
# Nettoyage des espaces parasites dans MES et MSS.

pass <- pass |>
  rename(
    mode_entree = mes,
    mode_sortie = mss
  ) |>
  mutate(
    mode_entree = stringr::str_squish(mode_entree),
    mode_sortie = stringr::str_squish(mode_sortie)
  )

pass |>
  count(mode_entree, sort = TRUE)

pass |>
  count(mode_sortie, sort = TRUE)


# Contrôle qualité ======================================================
# Résumé final de la table nettoyée.

skim(pass)

pass |>
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  ) |>
  tidyr::pivot_longer(
    everything(),
    names_to  = "variable",
    values_to = "n_na"
  ) |>
  filter(n_na > 0) |>
  arrange(desc(n_na))


# Sauvegarde ============================================================

dir.create(
  here::here("data", "processed"),
  recursive    = TRUE,
  showWarnings = FALSE
)

saveRDS(
  pass,
  here::here("data", "processed", "pass_clean.rds")
)


# Fin du script =========================================================

gitmessage("======================================================")
message(" Nettoyage de la table PASS terminé avec succès")
message("------------------------------------------------------")
message(" Nombre de lignes   : ", nrow(pass))
message(" Nombre de variables: ", ncol(pass))
message(" Nombre de patients : ", n_distinct(pass$id_patient))
message(" Nombre de passages : ", n_distinct(pass$id_passage))
message(" Fichier sauvegardé : data/processed/pass_clean.rds")
message("======================================================")
