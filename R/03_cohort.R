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

source(here::here("R", "utils", "load_project.R"))

# ======================================================================
# 1. Import des données
# ======================================================================

avis <- readRDS(here::here("data", "processed", "avis_clean.rds"))
pass <- readRDS(here::here("data", "processed", "pass_clean.rds"))


# ======================================================================
# 2. Contrôle qualité des données
# ======================================================================

message("\n--- Contrôles préliminaires ---")
message("Dimensions avis_clean: ", paste(dim(avis), collapse = " x "))
message("Dimensions pass_clean: ", paste(dim(pass), collapse = " x "))

avis_sans_pass <- avis |> anti_join(pass, by = "id_passage")
message("Nombre d'avis sans correspondance dans PASS: ", nrow(avis_sans_pass))

pass_sans_avis <- pass |> anti_join(avis, by = "id_passage")
message("Nombre de passages sans correspondance dans AVIS: ", nrow(pass_sans_avis))


# ======================================================================
# 3. Fusion PASS + AVIS
# ======================================================================

# ----------------------------------------------------------------------
# 3.1 Préparation des données AVIS
# ----------------------------------------------------------------------

avis <- avis |>
  arrange(id_passage, date_avis, heure_avis)

# ----------------------------------------------------------------------
# 3.2 Agrégation des avis au niveau passage
# ----------------------------------------------------------------------

avis_par_passage <- avis |>
  group_by(id_passage) |>
  summarise(
    nb_avis = n(),
    
    date_premier_avis = first(date_avis),
    diag_p_premier_avis = first(diag_p),
    diag_p_2_premier_avis = first(diag_p_2),
    diag_a_2_premier_avis = list(first(diag_a_2)),
    
    date_dernier_avis = last(date_avis),
    diag_p_dernier_avis = last(diag_p),
    diag_p_2_dernier_avis = last(diag_p_2),
    diag_a_2_dernier_avis = list(last(diag_a_2)),
    
    diag_t_2_passage = list(unique(na.omit(unlist(diag_t_2)))),
    
    mls = last(mls),
    mls_f = last(mls_f),
    
    .groups = "drop"
  )

# ----------------------------------------------------------------------
# 3.3 Fusion avec les données PASS
# ----------------------------------------------------------------------

pass <- pass |>
  select(-any_of(c("nb_avis", "diag_t", "diag_a", "mls", "mls_f", "type_sejour")))

pass_enrichi <- pass |>
  left_join(avis_par_passage, by = "id_passage", relationship = "one-to-one")

# Contrôle qualité
stopifnot(nrow(pass_enrichi) == nrow(pass))
stopifnot(all(pass$id_passage == pass_enrichi$id_passage))

message("Nombre de passages : ", nrow(pass_enrichi))
message("Passages avec avis psychiatrique : ", sum(!is.na(pass_enrichi$nb_avis)))
message("Passages sans avis psychiatrique : ", sum(is.na(pass_enrichi$nb_avis)))

# ======================================================================
# Exclusion des passages réalisés avant l'âge de 18 ans
# ======================================================================

n_passages_avant_age <- sum(pass_enrichi$age < 18, na.rm = TRUE)

n_patients_avec_passage_avant_age <- pass_enrichi |>
  filter(age < 18) |>
  summarise(n = n_distinct(id_patient)) |>
  pull(n)

pass_enrichi <- pass_enrichi |> filter(age >= 18)

message("Passages réalisés avant l'âge de 18 ans exclus : ", n_passages_avant_age)
message("Patients ayant au moins un passage avant 18 ans : ", n_patients_avec_passage_avant_age)
message("Passages après exclusion des passages <18 ans : ", nrow(pass_enrichi))
message("Patients adultes après exclusion des passages <18 ans : ", n_distinct(pass_enrichi$id_patient))

# Contrôle
stopifnot(all(pass_enrichi$age >= 18))


# ======================================================================
# 4. Construction de la cohorte patient
# ======================================================================

cohort <- pass_enrichi |> distinct(id_patient)

stopifnot(nrow(cohort) == dplyr::n_distinct(pass_enrichi$id_patient))
message("Nombre de patients : ", nrow(cohort))


# ======================================================================
# 5. Variables démographiques
# ======================================================================

cohort_demo <- pass_enrichi |>
  arrange(id_patient, date_arrivee) |>
  group_by(id_patient) |>
  summarise(
    sexe = first(sexe),
    age = first(age),
    code_postal = first(code_postal),
    departement = first(departement),
    secteur_94 = first(secteur_94),
    secteur_f = first(secteur_f),
    hopital_secteur = first(hopital_secteur),
    .groups = "drop"
  )

cohort <- cohort |>
  left_join(cohort_demo, by = "id_patient") |>
  mutate(
    age_cat = case_when(
      age >= 18 & age <= 24 ~ "18–24 ans",
      age >= 25 & age <= 44 ~ "25–44 ans",
      age >= 45 & age <= 64 ~ "45–64 ans",
      age >= 65 ~ "≥65 ans",
      TRUE ~ NA_character_
    ),
    age_cat = factor(age_cat, levels = c("18–24 ans", "25–44 ans", "45–64 ans", "≥65 ans"))
  )

stopifnot(nrow(cohort) == nrow(cohort_demo))
message("Variables démographiques ajoutées.")


# ======================================================================
# 6. Variables de recours aux urgences
# ======================================================================

cohort_recours <- pass_enrichi |>
  group_by(id_patient) |>
  summarise(
    premier_passage = min(date_arrivee),
    dernier_passage = max(date_arrivee),
    duree_suivi_jours = as.numeric(max(date_arrivee) - min(date_arrivee), units = "days"),
    nb_passages = n(),
    nb_avis = sum(nb_avis, na.rm = TRUE),
    nb_passages_jour = sum(nuit_arrivee == FALSE, na.rm = TRUE),
    nb_passages_nuit = sum(nuit_arrivee == TRUE, na.rm = TRUE),
    .groups = "drop"
  )

cohort <- cohort |> left_join(cohort_recours, by = "id_patient")

stopifnot(nrow(cohort) == nrow(cohort_recours))
message("Variables de recours ajoutées.")


# ======================================================================
# 7. Variables psychiatriques
# ======================================================================

cohort_diag <- pass_enrichi |>
  arrange(id_patient, date_arrivee) |>
  group_by(id_patient) |>
  summarise(
    diag_p_2_passages = list(diag_p_2_dernier_avis),
    diag_t_2_patient = list(unique(na.omit(unlist(diag_t_2_passage)))),
    usage_substance_patient = dplyr::if_else(any(usage_substance_t == "Oui", na.rm = TRUE), "Oui", "Non"),
    suicidalite_patient = dplyr::if_else(any(suicidalite_t == "Oui", na.rm = TRUE), "Oui", "Non"),
    .groups = "drop"
  )

# Contrôles de structure
stopifnot(nrow(cohort_diag) == n_distinct(pass_enrichi$id_patient))
stopifnot(all(c("id_patient", "diag_p_2_passages", "diag_t_2_patient") %in% names(cohort_diag)))

# Codage Schmoll
cohort_diag <- cohort_diag |>
  mutate(
    diag_dominant_schmoll = sapply(diag_p_2_passages, function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) return(NA_character_)
      x <- ifelse(x %in% c("F2", "F3", "F4"), x, "AUTRES")
      frequences <- table(factor(x, levels = c("F2", "F3", "F4", "AUTRES")))
      names(frequences)[which.max(frequences)]
    }),
    
    diag_F2_schmoll = diag_dominant_schmoll == "F2",
    diag_F3_schmoll = diag_dominant_schmoll == "F3",
    diag_F4_schmoll = diag_dominant_schmoll == "F4",
    diag_autres_schmoll = diag_dominant_schmoll == "AUTRES",
    
    diag_F1_schmoll = sapply(diag_t_2_patient, function(x) "F1" %in% x[!is.na(x)]),
    diag_F6_schmoll = sapply(diag_t_2_patient, function(x) "F6" %in% x[!is.na(x)])
  )

# Fusion
cohort <- cohort |> left_join(cohort_diag, by = "id_patient")

# Contrôles Schmoll
stopifnot(all(rowSums(cbind(cohort_diag$diag_F2_schmoll, cohort_diag$diag_F3_schmoll, cohort_diag$diag_F4_schmoll, cohort_diag$diag_autres_schmoll), na.rm = TRUE) <= 1))

message("Diagnostics Schmoll :")
message("F2 dominant : ", sum(cohort_diag$diag_F2_schmoll, na.rm = TRUE))
message("F3 dominant : ", sum(cohort_diag$diag_F3_schmoll, na.rm = TRUE))
message("F4 dominant : ", sum(cohort_diag$diag_F4_schmoll, na.rm = TRUE))
message("Autres dominant : ", sum(cohort_diag$diag_autres_schmoll, na.rm = TRUE))
message("F1 inclusif : ", sum(cohort_diag$diag_F1_schmoll, na.rm = TRUE))
message("F6 inclusif : ", sum(cohort_diag$diag_F6_schmoll, na.rm = TRUE))


# ======================================================================
# 8. Variables d'hospitalisation
# ======================================================================

cohort_organisation <- pass_enrichi |>
  group_by(id_patient) |>
  summarise(
    nb_hospitalisation = sum(orientation_finale == "HOSPIT_PSY", na.rm = TRUE),
    nb_hospitalisation_ssc = sum(orientation_finale == "HOSPIT_PSY" & mls_g == "SSC", na.rm = TRUE),
    hospitalisation = nb_hospitalisation > 0,
    hospitalisation_sl = any(orientation_finale == "HOSPIT_PSY" & mls_g == "SL", na.rm = TRUE),
    hospitalisation_ssc = nb_hospitalisation_ssc > 0,
    prop_hospitalisation = nb_hospitalisation / n(),
    prop_hospitalisation_ssc = if_else(nb_hospitalisation > 0, nb_hospitalisation_ssc / nb_hospitalisation, NA_real_),
    orientation_finale_patient = if_else(nb_hospitalisation > 0, "HOSPIT_PSY", "NON_ADMIS"),
    .groups = "drop"
  )

cohort <- cohort |> left_join(cohort_organisation, by = "id_patient")

stopifnot(nrow(cohort) == n_distinct(cohort$id_patient))
stopifnot(!anyDuplicated(cohort$id_patient))
stopifnot(all(cohort$nb_hospitalisation <= cohort$nb_passages))
stopifnot(all(cohort$nb_hospitalisation_ssc <= cohort$nb_hospitalisation))

message("Patients avec au moins une hospitalisation : ", sum(cohort$hospitalisation))
message("Nombre total d'hospitalisations : ", sum(cohort$nb_hospitalisation))
message("Nombre total d'hospitalisations SSC : ", sum(cohort$nb_hospitalisation_ssc))


# ======================================================================
# 9. Définition des Frequent Users
# ======================================================================

nb_passages_365j <- pass_enrichi |>
  arrange(id_patient, date_arrivee) |>
  group_by(id_patient) |>
  summarise(
    nb_passages_365j_max = max(
      sapply(date_arrivee, function(date_debut) {
        sum(date_arrivee >= date_debut & date_arrivee <= date_debut + lubridate::days(364))
      })
    ),
    .groups = "drop"
  )

cohort <- cohort |>
  left_join(nb_passages_365j, by = "id_patient") |>
  mutate(
    FU3 = nb_passages_365j_max >= 3,
    FU4 = nb_passages_365j_max >= 4
  )

stopifnot(!anyNA(cohort$nb_passages_365j_max))
stopifnot(all(cohort$FU4 <= cohort$FU3))
stopifnot(all(!is.na(cohort$FU3)) && all(!is.na(cohort$FU4)))

message("Nombre de patients : ", nrow(cohort))
message("FU3 : ", sum(cohort$FU3), " patients (", round(100 * mean(cohort$FU3), 1), " %)")
message("FU4 : ", sum(cohort$FU4), " patients (", round(100 * mean(cohort$FU4), 1), " %)")


# ======================================================================
# 11. Sauvegarde et Export
# ======================================================================

saveRDS(cohort, here::here("data", "processed", "cohort.rds"))
message("Fichier cohort.rds sauvegardé avec succès dans data/processed/")

dir.create(here::here("data", "exports"), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(cohort, here::here("data", "exports", "cohort_pvalue.csv"))
message("Fichier cohort_pvalue.csv exporté avec succès dans data/exports/")

# ======================================================================
# Fin du script
# ======================================================================