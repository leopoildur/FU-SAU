# ======================================================================
# FU-SAU
# R/tables_figures/02_tbl1_population_fu_vs_nonfu.R
# Tableau 1 : Caractéristiques de la population (FU vs Non-FU)
# ======================================================================

# ======================================================================
# 0. Packages et configuration
# ======================================================================

library(tidyverse)
library(gtsummary)
library(gt)

theme_gtsummary_language(
  language = "fr",
  decimal.mark = ",",
  big.mark = " "
)

# ======================================================================
# 1. Préparation des données
# ======================================================================

duree_soins_globale <- pass_enrichi |>
  group_by(id_patient) |>
  summarise(
    duree_soins_moyenne = dplyr::if_else(
      all(is.na(LOS)),
      NA_real_,
      mean(as.numeric(LOS), na.rm = TRUE)
    ),
    .groups = "drop"
  )

tableau1_data <- cohort |>
  left_join(duree_soins_globale, by = "id_patient") |>
  mutate(
    # --- Sociodémographie ---
    residence_region = case_when(
      departement == "94" ~ "Val-de-Marne (94)",
      departement %in% c("75", "77", "78", "91", "92", "93", "95") ~ "Île-de-France hors Val-de-Marne",
      TRUE ~ "Hors Île-de-France"
    ),
    residence_region = factor(residence_region, levels = c("Val-de-Marne (94)", "Île-de-France hors Val-de-Marne", "Hors Île-de-France")),
    age_cat = factor(age_cat, levels = c("18–24 ans", "25–44 ans", "45–64 ans", "≥65 ans")),
    sexe = factor(sexe, levels = c("F", "M"), labels = c("Femme", "Homme")),
    FU3 = factor(FU3, levels = c(TRUE, FALSE), labels = c("FU", "Non-FU")),

    # --- Diagnostics ---
    diag_dominant = na_if(as.character(diag_dominant), "Aucun diagnostic codé"),
    diag_dominant = factor(
      diag_dominant,
      levels = c(
        "Troubles psychotiques (F2)", 
        "Troubles de l'humeur (F3)", 
        "Troubles liés aux substances (F1)", 
        "Troubles de la personnalité (F6)", 
        "Troubles anxieux/névrotiques (F4)", 
        "Autres diagnostics"
      )
    ),
    suicidalite_patient = factor(suicidalite_patient, levels = c("Non", "Oui")),
    
    # Variable composite pour la p-value globale "Diagnostics associés"
    has_diag_associe = dplyr::if_else(
      diag_F6 == "Oui" | diag_F1 == "Oui" | suicidalite_patient == "Oui", 
      "Oui", "Non"
    ),
    has_diag_associe = factor(has_diag_associe, levels = c("Non", "Oui")),

    # --- Temporel et Hospitalisation ---
    au_moins_un_passage_nuit = factor(au_moins_un_passage_nuit, levels = c("Non", "Oui")),
    au_moins_un_passage_we = factor(au_moins_un_passage_we, levels = c("Non", "Oui")),
    au_moins_un_passage_garde = factor(au_moins_un_passage_garde, levels = c("Non", "Oui")),
    
    hospitalisation = factor(hospitalisation, levels = c(FALSE, TRUE), labels = c("Non", "Oui")),
    hospitalisation_sl = factor(hospitalisation_sl, levels = c(FALSE, TRUE), labels = c("Non", "Oui")),
    hospitalisation_ssc = factor(hospitalisation_ssc, levels = c(FALSE, TRUE), labels = c("Non", "Oui"))
  )

# ======================================================================
# 2. Construction des sous-tableaux
# ======================================================================

# ----------------------------------------------------------------------
# 2.1 Caractéristiques sociodémographiques
# ----------------------------------------------------------------------
tableau1_socio <- tableau1_data |>
  select(age, age_cat, sexe, residence_region, FU3) |>
  tbl_summary(
    by = FU3,
    statistic = list(age ~ "{median} [{p25}–{p75}]", all_categorical() ~ "{n} ({p} %)"),
    digits = list(age ~ 1, all_categorical() ~ c(0, 1)),
    missing = "no",
    label = list(
      age ~ "Âge, ans",
      age_cat ~ "Catégorie d'âge",
      sexe ~ "Sexe",
      residence_region ~ "Résidence"
    )
  ) |>
  add_overall(last = TRUE) |> add_p()

# ----------------------------------------------------------------------
# 2.2 Diagnostics
# ----------------------------------------------------------------------
tableau1_diagnostics <- tableau1_data |>
  select(
    diag_dominant, 
    has_diag_associe,
    diag_F6, diag_F1, diag_F1_alcool_seul, diag_F1_toxiques, suicidalite_patient, 
    FU3
  ) |>
  tbl_summary(
    by = FU3,
    type = list(
      has_diag_associe ~ "dichotomous",
      diag_F6 ~ "dichotomous",
      diag_F1 ~ "dichotomous",
      diag_F1_alcool_seul ~ "dichotomous",
      diag_F1_toxiques ~ "dichotomous",
      suicidalite_patient ~ "dichotomous"
    ),
    value = list(
      has_diag_associe ~ "Oui",
      diag_F6 ~ "Oui",
      diag_F1 ~ "Oui",
      diag_F1_alcool_seul ~ "Oui",
      diag_F1_toxiques ~ "Oui",
      suicidalite_patient ~ "Oui"
    ),
    statistic = list(all_categorical() ~ "{n} ({p} %)"),
    digits = list(all_categorical() ~ c(0, 1)),
    missing = "no",
    label = list(
      diag_dominant ~ "Diagnostic principal",
      has_diag_associe ~ "Diagnostics associés",
      diag_F6 ~ "\u00A0\u00A0\u00A0\u00A0Trouble de la personnalité (F6)",
      diag_F1 ~ "\u00A0\u00A0\u00A0\u00A0Trouble de l'usage de substances (F1)",
      diag_F1_alcool_seul ~ "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0Alcool uniquement",
      diag_F1_toxiques ~ "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0Toxiques hors alcool et tabac",
      suicidalite_patient ~ "\u00A0\u00A0\u00A0\u00A0Suicidalité"
    )
  ) |>
  add_overall(last = TRUE) |> 
  add_p(include = c(diag_dominant, has_diag_associe)) |>
  # Suppression des statistiques pour "has_diag_associe" (conserve uniquement la p-value)
  modify_table_body(function(df) {
    df |>
      mutate(
        across(starts_with("stat_"), ~ ifelse(variable == "has_diag_associe", NA_character_, .))
      )
  })

# ----------------------------------------------------------------------
# 2.3 Recours aux urgences
# ----------------------------------------------------------------------
tableau1_recours <- tableau1_data |>
  select(
    duree_soins_moyenne, duree_soins_moyenne_hospit, duree_soins_moyenne_non_hospit,
    au_moins_un_passage_garde, au_moins_un_passage_nuit, au_moins_un_passage_we, 
    FU3
  ) |>
  tbl_summary(
    by = FU3,
    type = list(
      au_moins_un_passage_garde ~ "dichotomous",
      au_moins_un_passage_nuit ~ "dichotomous",
      au_moins_un_passage_we ~ "dichotomous"
    ),
    value = list(
      au_moins_un_passage_garde ~ "Oui",
      au_moins_un_passage_nuit ~ "Oui",
      au_moins_un_passage_we ~ "Oui"
    ),
    statistic = list(
      all_continuous() ~ "{median} [{p25}–{p75}]",
      all_categorical() ~ "{n} ({p} %)"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing = "no",
    label = list(
      duree_soins_moyenne ~ "Durée médiane de soins par passage, min",
      duree_soins_moyenne_hospit ~ "\u00A0\u00A0\u00A0\u00A0Chez les patients hospitalisés",
      duree_soins_moyenne_non_hospit ~ "\u00A0\u00A0\u00A0\u00A0Chez les patients non hospitalisés",
      au_moins_un_passage_garde ~ "≥ 1 arrivée pendant la garde",
      au_moins_un_passage_nuit ~ "\u00A0\u00A0\u00A0\u00A0≥ 1 arrivée la nuit",
      au_moins_un_passage_we ~ "\u00A0\u00A0\u00A0\u00A0≥ 1 arrivée le week-end"
    )
  ) |>
  add_overall(last = TRUE) |> 
  add_p(include = c(duree_soins_moyenne, au_moins_un_passage_garde))

# ----------------------------------------------------------------------
# 2.4 Hospitalisations
# ----------------------------------------------------------------------
tableau1_hospit <- tableau1_data |>
  select(
    nb_hospitalisation, taux_hospitalisation,
    hospitalisation, hospitalisation_sl, hospitalisation_ssc, 
    FU3
  ) |>
  tbl_summary(
    by = FU3,
    type = list(
      hospitalisation ~ "dichotomous",
      hospitalisation_sl ~ "dichotomous",
      hospitalisation_ssc ~ "dichotomous"
    ),
    value = list(
      hospitalisation ~ "Oui",
      hospitalisation_sl ~ "Oui",
      hospitalisation_ssc ~ "Oui"
    ),
    statistic = list(
      nb_hospitalisation ~ "{median} [{p25}–{p75}]",
      taux_hospitalisation ~ "{median} [{p25}–{p75}]",
      all_categorical() ~ "{n} ({p} %)"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing = "no",
    label = list(
      nb_hospitalisation ~ "Nombre d'hospitalisations par patient",
      taux_hospitalisation ~ "Taux d'hospitalisation par patient, %",
      hospitalisation ~ "≥ 1 Hospitalisation en psychiatrie",
      hospitalisation_sl ~ "\u00A0\u00A0\u00A0\u00A0 ≥ 1 En soins libres (SL)",
      hospitalisation_ssc ~ "\u00A0\u00A0\u00A0\u00A0 ≥ 1 En soins sous contrainte (SSC)"
    )
  ) |>
  add_overall(last = TRUE) |> 
  add_p(include = c(nb_hospitalisation, taux_hospitalisation, hospitalisation))

# ======================================================================
# 3. Assemblage principal (tbl_stack)
# ======================================================================
tableau1 <- tbl_stack(
  tbls = list(
    tableau1_socio,
    tableau1_diagnostics,
    tableau1_recours,
    tableau1_hospit
  ),
  group_header = c(
    "Caractéristiques sociodémographiques",
    "Diagnostics",
    "Recours aux urgences",
    "Hospitalisations post-urgence"
  )
) |>
  bold_labels()

# ======================================================================
# 4. Mise en forme finale (gt)
# ======================================================================
tableau1_gt <- tableau1 |>
  modify_caption("**Tableau 1. Caractéristiques de la population selon le statut d'Utilisateur Fréquent (FU)**") |>
  modify_header(
    label = "**Caractéristiques**",
    all_stat_cols() ~ "**{level}**<br>*(n = {n})*",
    p.value = "**p**"
  ) |>
  as_gt() |>
  tab_style(
    style = cell_borders(sides = "top", color = "gray60", weight = px(1)),
    locations = cells_body(
      rows = label %in% c("Diagnostics associés", "≥ 1 arrivée pendant la garde")
    )
  ) |>
  tab_options(
    table.font.size = 11,
    heading.title.font.size = 12,
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold",
    data_row.padding = px(3),
    table.border.top.color = "black",
    table.border.top.width = px(1.5),
    table.border.bottom.color = "black",
    table.border.bottom.width = px(1.5),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(1),
    table_body.hlines.color = "transparent"
  ) |>
  tab_source_note(
    source_note = md(
      paste0(
        "**Note :** Données présentées en n (%) ou médiane [Q1–Q3]. ",
        "Les p-values correspondent à la comparaison entre les groupes FU et Non-FU (test de Wilcoxon, Chi-2 ou exact de Fisher). ",
        "FU : Frequent User (≥ 3 passages sur 12 mois) ; SL : soins libres ; SSC : soins sans consentement. ",
        "Le diagnostic principal repose sur le diagnostic majoritaire avec hiérarchie (F2 > F3 > F1 > F6 > F4) en cas d'égalité."
      )
    )
  )

# ======================================================================
# 5. Affichage
# ======================================================================
tableau1_gt