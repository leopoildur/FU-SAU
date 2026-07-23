# ======================================================================
# FU-SAU
# 03_merge.R
# Création de la cohorte analytique (niveau patient)
# ======================================================================

# Objectif :
#   - Construire la table analytique principale au niveau patient.
#   - Inclure un module complet 'Frequent Users' avec diverses définitions.
#   - Calculer des variables analytiques numériques pour les analyses statistiques.
#
# Entrée :
#   data/processed/avis_clean.rds
#   data/processed/pass_clean.rds
#
# Sortie :
#   data/processed/cohort.rds
#   outputs/tables/frequent_users_contribution.csv
# ======================================================================


# Chargement du projet ==================================================

source(here::here("R", "utils", "load_project.R"))

avis <- readRDS(here::here("data", "processed", "avis_clean.rds"))
pass <- readRDS(here::here("data", "processed", "pass_clean.rds"))


# Structure générale ====================================================

dim(avis)
dim(pass)


# Contrôle des identifiants avant fusion ================================

## Passages dans AVIS sans correspondance dans PASS ---------------------

avis_sans_pass <- avis |>
  anti_join(pass, by = "id_passage")

nrow(avis_sans_pass)

## Passages dans PASS sans correspondance dans AVIS ---------------------

pass_sans_avis <- pass |>
  anti_join(avis, by = "id_passage")

nrow(pass_sans_avis)

## Nombre d'avis par passage dans AVIS ----------------------------------

avis |>
  count(id_passage, name = "n_avis_avis") |>
  arrange(desc(n_avis_avis))


# Diagnostics du 1er et du dernier avis par passage (depuis AVIS) =======

diag_par_passage <- avis |>
  arrange(id_passage, date_avis) |>
  group_by(id_passage) |>
  summarise(
    diag_p_premier_avis   = first(diag_p),
    diag_p_2_premier_avis = first(diag_p_2),
    diag_a_premier_avis   = list(first(diag_a)),
    diag_p_dernier_avis   = last(diag_p),
    diag_p_2_dernier_avis = last(diag_p_2),
    diag_a_dernier_avis   = list(last(diag_a)),
    n_avis_avis = n()
  ) |>
  ungroup()


# Enrichissement de PASS avec les diagnostics AVIS ======================

pass_enrichi <- pass |>
  left_join(diag_par_passage, by = "id_passage")


# Calcul des métriques de recours par patient (avant agrégation) ========
# Nécessaire pour les fenêtres glissantes.

pass_pre_cohort <- pass_enrichi |>
  arrange(id_patient, date_arrivee) |>
  group_by(id_patient) |>
  mutate(
    # Nombre de passages dans les 365 jours précédents pour chaque passage
    nb_passages_365j_glissant = count_events_in_rolling_window(date_arrivee, 365)
  ) |>
  ungroup()


# Agrégation au niveau patient ==========================================
# 1 ligne = 1 patient.

cohort <- pass_pre_cohort |>
  arrange(id_patient, date_arrivee) |>
  group_by(id_patient) |>
  summarise(

    # ------------------------------------------------------------------
    # Variables démographiques (prises au premier passage)
    # ------------------------------------------------------------------

    sexe                = first(sexe),
    age_premier_passage = first(age),
    code_postal         = first(code_postal),
    departement         = first(departement),

    # ------------------------------------------------------------------
    # Métriques de recours globales
    # ------------------------------------------------------------------

    nb_passages_total    = n(),
    nb_avis_total        = sum(nb_avis, na.rm = TRUE),
    nb_hospitalisations  = sum(type_sejour_f == "AVEC_HOSPIT", na.rm = TRUE),
    date_premier_passage = first(date_arrivee),
    date_dernier_passage = last(date_arrivee),
    duree_suivi_jours    = as.numeric(
      difftime(last(date_arrivee), first(date_arrivee), units = "days")
    ),
    frequence_passages_par_an = nb_passages_total / (duree_suivi_jours / 365.25),

    # ------------------------------------------------------------------
    # Diagnostics du premier passage (1er et dernier avis)
    # ------------------------------------------------------------------

    diag_p_premier_passage   = first(diag_p_premier_avis),
    diag_p_2_premier_passage = first(diag_p_2_premier_avis),
    diag_p_dernier_avis_premier_passage   = first(diag_p_dernier_avis),
    diag_p_2_dernier_avis_premier_passage = first(diag_p_2_dernier_avis),

    # ------------------------------------------------------------------
    # Diagnostics du dernier passage (1er et dernier avis)
    # ------------------------------------------------------------------

    diag_p_premier_avis_dernier_passage   = last(diag_p_premier_avis),
    diag_p_2_premier_avis_dernier_passage = last(diag_p_2_premier_avis),
    diag_p_dernier_passage   = last(diag_p_dernier_avis),
    diag_p_2_dernier_passage = last(diag_p_2_dernier_avis),

    # ------------------------------------------------------------------
    # Secteur et hôpital (premier passage)
    # ------------------------------------------------------------------

    secteur_f_premier       = first(secteur_f),
    hopital_secteur_premier = first(hopital_secteur),

    # ------------------------------------------------------------------
    # Destinations de sortie (tous passages)
    # ------------------------------------------------------------------

    nb_retour_domicile = sum(destination == "DOMICILE",      na.rm = TRUE),
    nb_transfert_psy   = sum(destination == "TRANSFERT_PSY", na.rm = TRUE),
    nb_transfert_mco   = sum(destination == "TRANSFERT_MCO", na.rm = TRUE),
    nb_non_admis       = sum(destination == "NON_ADMIS",     na.rm = TRUE),

    # ------------------------------------------------------------------
    # Mode légal de soins (premier passage)
    # ------------------------------------------------------------------

    mls_f_premier = first(mls_f),
    mls_g_premier = first(mls_g),

    # ------------------------------------------------------------------
    # Module "Frequent Users" - Définitions fixes (fenêtre glissante)
    # ------------------------------------------------------------------

    nb_passages_365j_max = max(nb_passages_365j_glissant, na.rm = TRUE),
    FU_n_2 = any(nb_passages_365j_glissant >= 2, na.rm = TRUE),
    FU_n_3 = any(nb_passages_365j_glissant >= 3, na.rm = TRUE),
    FU_n_4 = any(nb_passages_365j_glissant >= 4, na.rm = TRUE),
    FU_n_5 = any(nb_passages_365j_glissant >= 5, na.rm = TRUE),
    FU_n_10 = any(nb_passages_365j_glissant >= 10, na.rm = TRUE)

  ) |>
  ungroup()


# Module "Frequent Users" - Définitions par percentiles =================

# Calcul des percentiles sur le nombre total de passages
quantiles_total <- quantile(cohort$nb_passages_total, probs = c(0.8, 0.9, 0.95, 0.99), na.rm = TRUE)

cohort <- cohort |>
  mutate(
    FU_top_20_total = nb_passages_total >= quantiles_total["80%"],
    FU_top_10_total = nb_passages_total >= quantiles_total["90%"],
    FU_top_5_total  = nb_passages_total >= quantiles_total["95%"],
    FU_top_1_total  = nb_passages_total >= quantiles_total["99%"]
  )

# Calcul des percentiles sur le nombre maximal de passages en 365 jours
quantiles_365j_max <- quantile(cohort$nb_passages_365j_max, probs = c(0.8, 0.9, 0.95, 0.99), na.rm = TRUE)

cohort <- cohort |>
  mutate(
    FU_top_20_365j_max = nb_passages_365j_max >= quantiles_365j_max["80%"],
    FU_top_10_365j_max = nb_passages_365j_max >= quantiles_365j_max["90%"],
    FU_top_5_365j_max  = nb_passages_365j_max >= quantiles_365j_max["95%"],
    FU_top_1_365j_max  = nb_passages_365j_max >= quantiles_365j_max["99%"]
  )


# Contrôle qualité ======================================================

dim(cohort)

cohort |>
  count(nb_passages_total, sort = TRUE)

cohort |>
  count(nb_passages_365j_max, sort = TRUE)

cohort |>
  count(FU_n_4)

cohort |>
  count(FU_top_5_total)

cohort |>
  summarise(
    across(everything(), ~ sum(is.na(.)))
  ) |>
  tidyr::pivot_longer(
    everything(),
    names_to  = "variable",
    values_to = "n_na"
  ) |>
  filter(n_na > 0) |>
  arrange(desc(n_na))


# Génération de la table de contribution à l'activité ===================

# Définitions des FU à évaluer
fu_definitions <- list(
  "FU_n_3" = quote(FU_n_3),
  "FU_n_4" = quote(FU_n_4),
  "FU_n_5" = quote(FU_n_5),
  "FU_top_1_total" = quote(FU_top_1_total),
  "FU_top_5_total" = quote(FU_top_5_total),
  "FU_top_1_365j_max" = quote(FU_top_1_365j_max),
  "FU_top_5_365j_max" = quote(FU_top_5_365j_max)
)

contribution_table <- tibble(
  Definition = character(),
  Patients = numeric(),
  `% Patients` = character(),
  Passages = numeric(),
  `% Activité Passages` = character(),
  `Avis Psychiatriques` = numeric(),
  `% Activité Avis` = character()
)

total_patients <- nrow(cohort)
total_passages <- sum(cohort$nb_passages_total, na.rm = TRUE)
total_avis     <- sum(cohort$nb_avis_total, na.rm = TRUE)

for (def_name in names(fu_definitions)) {
  def_expr <- fu_definitions[[def_name]]
  
  fu_cohort <- cohort |>
    filter(!!def_expr)
  
  n_fu_patients <- nrow(fu_cohort)
  prop_fu_patients <- n_fu_patients / total_patients
  
  n_fu_passages <- sum(fu_cohort$nb_passages_total, na.rm = TRUE)
  prop_fu_passages <- n_fu_passages / total_passages
  
  n_fu_avis <- sum(fu_cohort$nb_avis_total, na.rm = TRUE)
  prop_fu_avis <- n_fu_avis / total_avis
  
  contribution_table <- contribution_table |>
    add_row(
      Definition = def_name,
      Patients = n_fu_patients,
      `% Patients` = scales::percent(prop_fu_patients, accuracy = 0.1),
      Passages = n_fu_passages,
      `% Activité Passages` = scales::percent(prop_fu_passages, accuracy = 0.1),
      `Avis Psychiatriques` = n_fu_avis,
      `% Activité Avis` = scales::percent(prop_fu_avis, accuracy = 0.1)
    )
}

# Sauvegarde de la table de contribution
dir.create(here::here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(contribution_table, here::here("outputs", "tables", "frequent_users_contribution.csv"))


# Sauvegarde de la cohorte finale =======================================

dir.create(
  here::here("data", "processed"),
  recursive    = TRUE,
  showWarnings = FALSE
)

saveRDS(
  cohort,
  here::here("data", "processed", "cohort.rds")
)


# Fin du script =========================================================

gitmessage("======================================================")
message(" Création de la cohorte patient terminée avec succès")
message("------------------------------------------------------")
message(" Nombre de patients (lignes) : ", nrow(cohort))
message(" Nombre de variables         : ", ncol(cohort))
message(" Frequent attenders (FU_n_4) : ", sum(cohort$FU_n_4, na.rm = TRUE))
message(" Fichier sauvegardé          : data/processed/cohort.rds")
message(" Table de contribution FU    : outputs/tables/frequent_users_contribution.csv")
message("======================================================")
