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

source(here::here("R", "utils", "load_project.R"))

avis <- readRDS(here::here("data", "interim", "avis_raw.rds"))


# Structure générale ====================================================

## Dimensions ------------------------------------------------------------
dim(avis)

## Types des variables ---------------------------------------------------
glimpse(avis)

## Contrôle global -------------------------------------------------------
skim(avis)


# Identifiants ==========================================================

# Renommage et Conversion -----------------------------------------------
avis <- avis |>
  rename(
    id_patient = idpat,
    id_passage = i_ddos
  ) |>
  mutate(
    id_patient = as.character(id_patient),
    id_passage = as.character(id_passage)
  )

# Contrôles -------------------------------------------------------------
avis |> summarise(n_patients = n_distinct(id_patient))
avis |> summarise(n_passages = n_distinct(id_passage))

avis |>
  distinct(id_passage, id_patient) |>
  count(id_passage, name = "n_patients") |>
  filter(n_patients > 1)

avis |>
  count(id_patient, name = "n_passages") |>
  arrange(desc(n_passages))

# Doublons --------------------------------------------------------------
sum(duplicated(avis))
avis <- avis |> distinct()
sum(duplicated(avis))


# Variables temporelles =================================================

# Renommage -------------------------------------------------------------
avis <- avis |>
  rename(
    date_arrivee = debu_tt,
    date_sortie = fi_nt,
    date_demande = de_mt,
    date_avis = avi_st
  )

stopifnot(is.character(avis$date_arrivee))

# Conversion et Variables dérivées --------------------------------------
avis <- avis |>
  mutate(
    date_arrivee = parse_datetime_fr(date_arrivee),
    date_sortie = parse_datetime_fr(date_sortie),
    date_demande = parse_datetime_fr(date_demande),
    date_avis = parse_datetime_fr(date_avis),

    heure_arrivee = get_hour(date_arrivee),
    heure_avis = get_hour(date_avis),
    heure_sortie = get_hour(date_sortie),

    nuit_arrivee = is_night(date_arrivee),
    nuit_avis = is_night(date_avis),
    nuit_sortie = is_night(date_sortie),

    weekend_arrivee = is_weekend(date_arrivee),
    weekend_avis = is_weekend(date_avis),
    weekend_sortie = is_weekend(date_sortie),

    jour_arrivee = weekday_label(date_arrivee),
    jour_avis = weekday_label(date_avis),
    jour_sortie = weekday_label(date_sortie),

    delai_avis = date_avis - date_arrivee,
    LOS = date_sortie - date_arrivee,
    
    ferie = is_holiday(date_avis),
    garde = nuit_avis | weekend_avis | ferie
  )

# Contrôles --------------------------------------------------------------
avis |> summarise(across(c(date_arrivee, date_sortie, date_demande, date_avis), ~ sum(is.na(.))))
avis |> count(garde, sort = TRUE)
avis |> count(ferie)

# Cohérence chronologique -----------------------------------------------
avis |> filter(date_sortie < date_arrivee)

anomalies_demande <- avis |> filter(date_demande < date_arrivee)
anomalies_avis_avant <- avis |> filter(date_avis < date_arrivee)
anomalies_avis_apres <- avis |> filter(date_avis > date_sortie)

# Résumé descriptif -----------------------------------------------------
summary(as.numeric(avis$delai_avis))
summary(as.numeric(avis$LOS))


# Variables démographiques ==============================================

# Âge -------------------------------------------------------------------
summary(avis$age)

avis |>
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "#4E79A7", color = "white") +
  labs(title = "Distribution des âges", x = "Âge (années)", y = "Nombre d'avis") +
  theme_fu()

# Sexe, Code postal, Département ----------------------------------------
avis <- avis |>
  rename(code_postal = cp) |>
  mutate(
    sexe = get_sexe_label(sexe),
    code_postal = as.character(code_postal),
    code_postal = stringr::str_pad(code_postal, width = 5, side = "left", pad = "0"),
    departement = get_department(code_postal)
  )

avis |> count(sexe, sort = TRUE)
avis |> count(departement, sort = TRUE)

# Contrôles -------------------------------------------------------------
avis |> filter(age < 15 | age > 110)
avis |> summarise(across(c(age, sexe, code_postal, departement), ~ sum(is.na(.))))


# Variables psychiatriques ==============================================

# Diagnostic principal --------------------------------------------------
avis <- avis |>
  rename(diag_p = dp) |>
  mutate(
    diag_p = stringr::str_trim(diag_p),
    diag_p = stringr::str_to_upper(diag_p),
    diag_p_1 = stringr::str_sub(diag_p, 1, 1),
    diag_p_2 = stringr::str_sub(diag_p, 1, 2),
    diag_p_3 = stringr::str_sub(diag_p, 1, 3),
    
    # Catégorisation du diagnostic principal
    categorie_diag_p = categoriser_cim10(diag_p),
    usage_substance_p = detecter_usage_substance(categorie_diag_p)
  )

avis |> count(diag_p, sort = TRUE)
avis |> count(categorie_diag_p, sort = TRUE)

# Diagnostics associés --------------------------------------------------
avis <- avis |>
  rename(
    nb_diag_a = n_das,
    diag_a = das
  ) |>
  mutate(
    diag_a = stringr::str_trim(diag_a),
    diag_a = stringr::str_to_upper(diag_a),
    diag_a = stringr::str_squish(diag_a),
    diag_a = na_if(diag_a, ""),
    diag_a = stringr::str_split(diag_a, pattern = "\\s+")
  )

avis$diag_a <- lapply(avis$diag_a, function(x) {
  if (length(x) == 1 && is.na(x)) character(0) else x
})

avis <- avis |>
  mutate(
    diag_a_1 = lapply(diag_a, function(x) stringr::str_sub(x, 1, 1)),
    diag_a_2 = lapply(diag_a, function(x) stringr::str_sub(x, 1, 2)),
    diag_a_3 = lapply(diag_a, function(x) stringr::str_sub(x, 1, 3)),
    nb_diag_a_calcule = lengths(diag_a)
  )

avis |> count(nb_diag_a, nb_diag_a_calcule)
sum(lengths(avis$diag_a) == 0)
table(lengths(avis$diag_a))

# Diagnostics totaux ==================================================
avis <- avis |>
  mutate(
    diag_t = purrr::map2(diag_p, diag_a, ~ unique(c(.x, .y))),
    diag_t_1 = lapply(diag_t, function(x) stringr::str_sub(x, 1, 1)),
    diag_t_2 = lapply(diag_t, function(x) stringr::str_sub(x, 1, 2)),
    diag_t_3 = lapply(diag_t, function(x) stringr::str_sub(x, 1, 3)),
    nb_diag_t = lengths(diag_t),
    
    # Dépistage transversal (tous diagnostics confondus) pour capturer les comorbidités fréquentes
    usage_substance_t = purrr::map_chr(diag_t, ~ dplyr::if_else(any(detecter_usage_substance(categoriser_cim10(.x)) == "Oui"), "Oui", "Non")),
    suicidalite_t = purrr::map_chr(diag_t, ~ dplyr::if_else(any(categoriser_cim10(.x) == "Suicidalité"), "Oui", "Non"))
  )

avis |> count(nb_diag_t, sort = TRUE)
avis |> count(usage_substance_t)
avis |> count(suicidalite_t)


# Variables organisationnelles ==========================================

# Mode légal de soins ---------------------------------------------------
avis <- avis |>
  mutate(
    mls_f = get_mls_label(mls),
    mls_g = dplyr::case_when(
      mls_f == "SL" ~ "SL",
      mls_f %in% c("SPDT", "SPPI", "SPDRE", "OPP", "DETENUS", "PENAL") ~ "SSC",
      TRUE ~ "INCONNU"
    )
  )

avis |> count(mls_f, sort = TRUE)
avis |> count(mls_g, sort = TRUE)
avis |> filter(mls_g == "INCONNU")

# Secteur psychiatrique -------------------------------------------------
avis <- avis |>
  mutate(
    secteur = stringr::str_squish(secteur),
    secteur_f = dplyr::case_when(
      stringr::str_detect(secteur, "SECTEUR\\s+[0-9]{2}$") ~ paste0("94G", stringr::str_extract(secteur, "[0-9]{2}$")),
      stringr::str_detect(secteur, "IDF") ~ "IDF hors 94",
      TRUE ~ "Hors IDF"
    )
  )

avis |> count(secteur, sort = TRUE)
avis |> count(secteur_f, sort = TRUE)


# Sauvegarde ============================================================

dir.create(here::here("data", "processed"), recursive = TRUE, showWarnings = FALSE)

saveRDS(avis, here::here("data", "processed", "avis_clean.rds"))

# write_csv(avis, here::here("data", "processed", "avis_clean.csv"))


# Fin du script =========================================================

message("======================================================")
message(" Nettoyage de la table AVIS terminé avec succès")
message("------------------------------------------------------")
message(" Nombre de lignes   : ", nrow(avis))
message(" Nombre de variables: ", ncol(avis))
message(" Nombre de patients : ", n_distinct(avis$id_patient))
message(" Nombre de passages : ", n_distinct(avis$id_passage))
message(" Fichier sauvegardé : data/processed/avis_clean.rds")
message("======================================================")