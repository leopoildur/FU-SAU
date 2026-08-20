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
# data/exports/cohort_jamovi.sav
# ======================================================================

# ======================================================================
# 0. Chargement du projet
# ======================================================================

source(here::here("R", "utils", "load_project.R"))
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(haven)

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
    
    # Utilisation des codes entiers pour permettre au dictionnaire d'être précis
    diag_t_passage = list(unique(na.omit(unlist(diag_t)))),
    
    mls = last(mls),
    mls_f = last(mls_f),
    mls_g = last(mls_g),
    
    .groups = "drop"
  )

# ----------------------------------------------------------------------
# 3.3 Fusion avec les données PASS
# ----------------------------------------------------------------------

pass <- pass |>
  select(-any_of(c("nb_avis", "diag_t", "diag_a", "mls", "mls_f", "mls_g", "type_sejour")))

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


# ======================================================================
# 4. Construction de la cohorte patient
# ======================================================================

cohort <- pass_enrichi |> distinct(id_patient)


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
    age_cat = factor(age_cat, levels = c("18–24 ans", "25–44 ans", "45–64 ans", "≥65 ans")),
    residence_region = if_else(departement == "94", "Val-de-Marne (94)", "Hors Val-de-Marne"),
    residence_region = factor(residence_region, levels = c("Val-de-Marne (94)", "Hors Val-de-Marne"))
  )


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


# ======================================================================
# 7. Matrice Diagnostique et Comorbidités (Via dicocim10.R)
# ======================================================================

# 7.1. Extraction des diagnostics issus du dernier avis de chaque passage
derniers_avis <- avis |>
  filter(!is.na(date_avis)) |>
  group_by(id_passage) |>
  arrange(date_avis) |>
  slice_tail(n = 1) |>
  ungroup()

diagnostics_long <- derniers_avis |>
  select(id_patient, diag_t) |>
  mutate(diag_t = as.character(diag_t)) |>
  filter(!is.na(diag_t), str_trim(diag_t) != "") |>
  separate_longer_delim(cols = diag_t, delim = regex("\\s+")) |>
  mutate(diagnostic = str_trim(diag_t)) |>
  filter(diagnostic != "")

# 7.2. Calcul du diagnostic dominant par patient
diag_dominant_patient <- diagnostics_long |>
  mutate(famille = get_famille_psy(diagnostic)) |>
  count(id_patient, famille, name = "frequence") |>
  mutate(
    poids_hierarchie = case_when(
      famille == "F2" ~ 7,
      famille == "F3" ~ 6,
      famille == "F4" ~ 5,
      famille == "F6" ~ 4,
      famille == "F1" ~ 3,
      famille == "Autres_F" ~ 2,
      famille == "Non_Psy" ~ 1,
      TRUE ~ 0
    ),
    is_psy = if_else(famille != "Non_Psy", 1, 0)
  ) |>
  group_by(id_patient) |>
  arrange(desc(is_psy), desc(frequence), desc(poids_hierarchie)) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(id_patient, famille_dominante = famille)

# 7.3. Définition du profil majoritaire et balayage des comorbidités
cohort_diag <- pass_enrichi |>
  arrange(id_patient, date_arrivee) |>
  group_by(id_patient) |>
  summarise(
    # Extraction de l'historique complet des codes bruts pour balayage
    diag_t_patient = list(unique(na.omit(unlist(diag_t_passage)))),
    suicidalite_patient = dplyr::if_else(any(suicidalite_t == "Oui", na.rm = TRUE), "Oui", "Non"),
    .groups = "drop"
  ) |>
  left_join(diag_dominant_patient, by = "id_patient") |>
  mutate(
    diag_dominant = case_when(
      is.na(famille_dominante) ~ "Aucun diagnostic psychiatrique",
      famille_dominante == "F2" ~ "Troubles psychotiques (F2)",
      famille_dominante == "F3" ~ "Troubles de l'humeur (F3)",
      famille_dominante == "F4" ~ "Troubles anxieux/névrotiques (F4)",
      famille_dominante == "F6" ~ "Troubles de la personnalité (F6)",
      famille_dominante == "F1" ~ "Troubles liés aux substances (F1)",
      famille_dominante == "Autres_F" ~ "Autres diagnostics psychiatriques",
      famille_dominante == "Non_Psy" ~ "Aucun diagnostic psychiatrique"
    ),
    diag_dominant = factor(
      diag_dominant,
      levels = c("Troubles psychotiques (F2)", "Troubles de l'humeur (F3)", 
                 "Troubles anxieux/névrotiques (F4)", "Troubles de la personnalité (F6)", 
                 "Troubles liés aux substances (F1)", "Autres diagnostics psychiatriques", 
                 "Aucun diagnostic psychiatrique")
    ),
    
    # Balayage transversal de l'historique diagnostique via le dictionnaire
    hist_F0 = sapply(diag_t_patient, function(x) any(get_famille_psy(x) == "F0")),
    hist_addiction_global = sapply(diag_t_patient, function(x) any(is_addictif_global(x))),
    hist_alcool = sapply(diag_t_patient, function(x) any(is_alcool(x))),
    hist_cannabis = sapply(diag_t_patient, function(x) any(is_cannabis(x))),
    hist_autre_toxique = sapply(diag_t_patient, function(x) any(is_autre_toxique(x))),
    hist_F2 = sapply(diag_t_patient, function(x) any(get_famille_psy(x) == "F2")),
    hist_F3 = sapply(diag_t_patient, function(x) any(get_famille_psy(x) == "F3")),
    hist_F4 = sapply(diag_t_patient, function(x) any(get_famille_psy(x) == "F4")),
    hist_F5 = sapply(diag_t_patient, function(x) any(get_famille_psy(x) == "F5")),
    hist_F6 = sapply(diag_t_patient, function(x) any(get_famille_psy(x) == "F6")),
    hist_ND = sapply(diag_t_patient, function(x) any(get_famille_psy(x) == "F7_F9")),
    
    # Comorbidités binaires (Exclusion de la classe majoritaire correspondante)
    comorb_F0 = factor(if_else(hist_F0, "Oui", "Non"), levels = c("Non", "Oui")),
    comorb_F1 = factor(if_else(hist_addiction_global & diag_dominant != "Troubles liés aux substances (F1)", "Oui", "Non"), levels = c("Non", "Oui")),
    comorb_alcool = factor(if_else(hist_alcool & diag_dominant != "Troubles liés aux substances (F1)", "Oui", "Non"), levels = c("Non", "Oui")),
    comorb_cannabis = factor(if_else(hist_cannabis & diag_dominant != "Troubles liés aux substances (F1)", "Oui", "Non"), levels = c("Non", "Oui")),
    comorb_autre_toxique = factor(if_else(hist_autre_toxique & diag_dominant != "Troubles liés aux substances (F1)", "Oui", "Non"), levels = c("Non", "Oui")),
    comorb_F2 = factor(if_else(hist_F2 & diag_dominant != "Troubles psychotiques (F2)", "Oui", "Non"), levels = c("Non", "Oui")),
    comorb_F3 = factor(if_else(hist_F3 & diag_dominant != "Troubles de l'humeur (F3)", "Oui", "Non"), levels = c("Non", "Oui")),
    comorb_F4 = factor(if_else(hist_F4 & diag_dominant != "Troubles anxieux/névrotiques (F4)", "Oui", "Non"), levels = c("Non", "Oui")),
    comorb_F5 = factor(if_else(hist_F5, "Oui", "Non"), levels = c("Non", "Oui")),
    comorb_F6 = factor(if_else(hist_F6 & diag_dominant != "Troubles de la personnalité (F6)", "Oui", "Non"), levels = c("Non", "Oui")),
    comorb_ND = factor(if_else(hist_ND, "Oui", "Non"), levels = c("Non", "Oui"))
  ) |>
  select(id_patient, diag_dominant, starts_with("comorb_"), suicidalite_patient)

cohort <- cohort |> left_join(cohort_diag, by = "id_patient")

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


# ======================================================================
# 8.5 Variables du Passage Index (Premier passage)
# ======================================================================

passage_index <- pass_enrichi |>
  arrange(id_patient, date_arrivee) |>
  group_by(id_patient) |>
  slice_head(n = 1) |>
  summarise(
    orientation_index = if_else(orientation_finale == "HOSPIT_PSY", "Hospitalisation", "Retour Domicile / Autre"),
    ssc_index = if_else(mls_g == "SSC", "Oui", "Non"),
    .groups = "drop"
  ) |>
  mutate(
    orientation_index = factor(orientation_index, levels = c("Retour Domicile / Autre", "Hospitalisation")),
    ssc_index = factor(ssc_index, levels = c("Non", "Oui"))
  )

cohort <- cohort |> left_join(passage_index, by = "id_patient")


# ======================================================================
# 9. Variables temporelles et Durée de soins (Médianes)
# ======================================================================

cohort_parcours <- pass_enrichi |>
  group_by(id_patient) |>
  summarise(
    n_passages = n(),
    
    n_passages_nuit = sum(nuit_arrivee == TRUE | nuit_arrivee == "Oui", na.rm = TRUE),
    pct_passage_nuit = (n_passages_nuit / n_passages) * 100,
    au_moins_un_passage_nuit = factor(dplyr::if_else(n_passages_nuit > 0, "Oui", "Non"), levels = c("Non", "Oui")),
    
    n_passages_garde = sum(garde == TRUE | garde == "Oui", na.rm = TRUE),
    pct_passage_garde = (n_passages_garde / n_passages) * 100,
    au_moins_un_passage_garde = factor(dplyr::if_else(n_passages_garde > 0, "Oui", "Non"), levels = c("Non", "Oui")),
    
    n_passages_we = sum(weekend_arrivee == TRUE | weekend_arrivee == "Oui", na.rm = TRUE),
    pct_passage_we = (n_passages_we / n_passages) * 100,
    au_moins_un_passage_we = factor(dplyr::if_else(n_passages_we > 0, "Oui", "Non"), levels = c("Non", "Oui")),
    
    taux_hospitalisation = (sum(orientation_finale == "HOSPIT_PSY", na.rm = TRUE) / n_passages) * 100,
    
    # Passage en médianes pour conformité méthodologique
    duree_soins_mediane = median(as.numeric(LOS), na.rm = TRUE),
    duree_soins_mediane_hospit = median(as.numeric(LOS)[orientation_finale == "HOSPIT_PSY"], na.rm = TRUE),
    duree_soins_mediane_non_hospit = median(as.numeric(LOS)[orientation_finale != "HOSPIT_PSY" | is.na(orientation_finale)], na.rm = TRUE),
    
    .groups = "drop"
  ) |>
  mutate(
    across(starts_with("duree_soins"), ~ if_else(is.nan(.) | is.na(.), NA_real_, .))
  )

cohort <- cohort |> left_join(cohort_parcours, by = "id_patient")


# ======================================================================
# 10. Définition des Frequent Users
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
    FU3 = factor(if_else(nb_passages_365j_max >= 3, "Oui", "Non"), levels = c("Non", "Oui")),
    FU4 = factor(if_else(nb_passages_365j_max >= 4, "Oui", "Non"), levels = c("Non", "Oui"))
  )


# ======================================================================
# 11. Sauvegarde et Export automatisé pour Jamovi
# ======================================================================
install.packages("labelled")
library(labelled)

# Sauvegarde de la cohorte R brute
saveRDS(cohort, here::here("data", "processed", "cohort.rds"))
readr::write_csv(cohort, here::here("data", "exports", "cohort_pvalue.csv"))

# 1. Sélection et formatage des modalités (Facteurs)
data_jamovi <- cohort |>
  transmute(
    id_patient = id_patient,
    
    FU3 = factor(FU3, 
                 levels = c("Non", "Oui"), 
                 labels = c("Utilisateurs occasionnels (NFU)", "Utilisateurs fréquents (FU)")),
    
    age = age,
    age_cat = age_cat,
    sexe = sexe,
    residence_region = residence_region,
    hopital_secteur = hopital_secteur,
    diag_dominant = diag_dominant,
    
    comorb_F0 = comorb_F0,
    comorb_F1 = comorb_F1,
    comorb_F2 = comorb_F2,
    comorb_F3 = comorb_F3,
    comorb_F4 = comorb_F4,
    comorb_F5 = comorb_F5,
    comorb_F6 = comorb_F6,
    comorb_ND = comorb_ND,
    
    suicidalite_patient = suicidalite_patient,
    
    duree_soins_mediane = duree_soins_mediane,
    duree_soins_mediane_hospit = duree_soins_mediane_hospit,
    duree_soins_mediane_non_hospit = duree_soins_mediane_non_hospit,
    
    au_moins_un_passage_garde = au_moins_un_passage_garde,
    au_moins_un_passage_nuit = au_moins_un_passage_nuit,
    au_moins_un_passage_we = au_moins_un_passage_we,
    
    hospitalisation = hospitalisation,
    hospitalisation_ssc = hospitalisation_ssc,
    
    orientation_index = factor(orientation_index,
                               levels = c("Retour Domicile / Autre", "Hospitalisation"),
                               labels = c("Non admission", "Hospitalisation en psychiatrie")),
    
    ssc_index = factor(ssc_index,
                       levels = c("Non", "Oui"),
                       labels = c("Soins libres", "Soins sous contrainte"))
  ) |>
  
# 2. Ajout des métadonnées (Les étiquettes lues par Jamovi)
  set_variable_labels(
    age = "Âge",
    age_cat = "Catégories d'âge",
    sexe = "Sexe",
    residence_region = "Département de résidence",
    hopital_secteur = "Hôpital de rattachement",
    diag_dominant = "Profil diagnostique majoritaire",
    comorb_F0 = "F0",
    comorb_F1 = "Troubles liés à l'usage de substances (F1)",
    comorb_F2 = "F2",
    comorb_F3 = "F3",
    comorb_F4 = "F4",
    comorb_F5 = "F5",
    comorb_F6 = "Troubles de la personnalité (F6)",
    comorb_ND = "Troubles neurodéveloppementaux (F7-F8-F9)",
    suicidalite_patient = "Suicidalité",
    duree_soins_mediane = "Durée de soins en minutes, médiane (IQR)",
    duree_soins_mediane_hospit = "DMS médian hospitalisé",
    duree_soins_mediane_non_hospit = "DMS médian non hospitalisé",
    au_moins_un_passage_garde = "au_moins_un_passage_garde",
    au_moins_un_passage_nuit = "au_moins_un_passage_nuit",
    au_moins_un_passage_we = "au_moins_un_passage_we",
    hospitalisation = "Au moins une hospitalisation en psychiatrie",
    hospitalisation_ssc = "Au moins une hospitalisation en soins sous contrainte",
    orientation_index = "Passage Index - Orientation",
    ssc_index = "Passage Index - Mode légal de soins"
  )

# Export direct au format SPSS/Jamovi (.sav)
dir.create(here::here("data", "exports"), showWarnings = FALSE, recursive = TRUE)
haven::write_sav(data_jamovi, here::here("data", "exports", "cohort_jamovi.sav"))

message("Export Jamovi généré avec succès dans data/exports/cohort_jamovi.sav")