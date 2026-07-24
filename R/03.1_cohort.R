# ======================================================================
# FU-SAU
# 03_build_cohort.R
# Construction de la cohorte analytique (niveau patient)
# ======================================================================

# 0. Chargement du projet ===============================================

source(here::here("R", "utils", "load_project.R"))


# 1. Import des données nettoyées =======================================

avis <- readRDS(
  here::here("data", "processed", "avis_clean.rds")
)

pass <- readRDS(
  here::here("data", "processed", "pass_clean.rds")
)


# 2. Contrôles préliminaires ============================================

message("======================================================")
message("Création de la cohorte patient")
message("======================================================")

message("Dimensions AVIS : ", paste(dim(avis), collapse = " x "))
message("Dimensions PASS : ", paste(dim(pass), collapse = " x "))

# Avis sans passage correspondant

avis_sans_pass <- avis |>
  anti_join(pass, by = "id_passage")

message(
  "Avis sans passage correspondant : ",
  nrow(avis_sans_pass)
)

# Passages sans avis

pass_sans_avis <- pass |>
  anti_join(avis, by = "id_passage")

message(
  "Passages sans avis psychiatrique : ",
  nrow(pass_sans_avis)
)

# Distribution du nombre d'avis

message("Top 5 du nombre d'avis par passage")

avis |>
  count(
    id_passage,
    name = "n_avis"
  ) |>
  arrange(desc(n_avis)) |>
  slice_head(n = 5) |>
  print()



# ======================================================================
# 3. Fusion PASS + AVIS
# Une ligne = un passage
# ======================================================================

diag_par_passage <- avis |>

  arrange(
    id_passage,
    date_avis
  ) |>

  group_by(id_passage) |>

  summarise(

    # Nombre d'avis psychiatriques

    n_avis_passage =
      n(),

    # Tous les diagnostics du passage

    diag_t =
      list(

        unlist(diag_t) |>
          na.omit()

      ),

    # Dernier mode légal de soins

    mls =
      last(mls),

    mls_f =
      last(mls_f),

    # Dernier type de séjour psychiatrique

    type_sejour =
      last(type_sejour),

    # Dates du premier et du dernier avis

    date_premier_avis =
      first(date_avis),

    date_dernier_avis =
      last(date_avis),

    .groups = "drop"

  )


# Ajout des informations psychiatriques à PASS

pass_enrichi <- pass |>

  left_join(

    diag_par_passage,

    by = "id_passage"

  )

# ======================================================================
# 4. Variables dérivées au niveau passage
# ======================================================================

pass_pre_cohort <- pass_enrichi |>

  arrange(
    id_patient,
    date_arrivee
  ) |>

  group_by(id_patient) |>

  mutate(

    # Nombre de passages sur 365 jours glissants

    nb_passages_365j_glissant =
      count_events_in_rolling_window(
        date_arrivee,
        window_days = 365
      ),

    # Délai depuis le passage précédent

    delai_interpassage_jours =
      c(
        NA,
        as.numeric(diff(date_arrivee))
      )

  ) |>

  ungroup()

message("Préparation des passages terminée.")

# ======================================================================
# 5. Agrégation au niveau patient
# Une ligne = un patient
# ======================================================================

cohort <- pass_pre_cohort |>

  arrange(
    id_patient,
    date_arrivee
  ) |>

  group_by(id_patient) |>

  summarise(

    # ------------------------------------------------------------------
    # Démographie
    # ------------------------------------------------------------------

    sexe =
      first(sexe),

    age_premier_passage =
      first(age),

    age_dernier_passage =
      last(age),

    age_moyen =
      mean(age, na.rm = TRUE),

    code_postal =
      first(code_postal),

    departement =
      first(departement),

    secteur_psy =
      first(secteur_f),

    hopital =
      first(hopital_secteur),

    # ------------------------------------------------------------------
    # Suivi
    # ------------------------------------------------------------------

    premier_passage =
      first(date_arrivee),

    dernier_passage =
      last(date_arrivee),

    duree_suivi_jours =
      as.numeric(
        difftime(
          last(date_arrivee),
          first(date_arrivee),
          units = "days"
        )
      ),

    # ------------------------------------------------------------------
    # Activité
    # ------------------------------------------------------------------

    nb_passages =
      n(),

    nb_avis =
      sum(
        n_avis_passage,
        na.rm = TRUE
      ),

    duree_totale_sau =
      sum(
        duree_totale,
        na.rm = TRUE
      ),

    duree_moyenne_sau =
      mean(
        duree_totale,
        na.rm = TRUE
      ),

        # ------------------------------------------------------------------
    # Parcours de soins
    # ------------------------------------------------------------------

    nb_transfert_psy =

      sum(
        destination == "TRANSFERT_PSY",
        na.rm = TRUE
      ),

    nb_transfert_mco =

      sum(
        destination == "TRANSFERT_MCO",
        na.rm = TRUE
      ),

    nb_hospitalisations =

      sum(
        destination %in% c(
          "TRANSFERT_PSY",
          "TRANSFERT_MCO"
        ),
        na.rm = TRUE
      ),

    nb_retour_domicile =

      sum(
        destination == "DOMICILE",
        na.rm = TRUE
      ),

    nb_non_admis =

      sum(
        destination == "NON_ADMIS",
        na.rm = TRUE
      ),

    nb_rad_na =

      sum(
        destination %in% c(
          "DOMICILE",
          "NON_ADMIS"
        ),
        na.rm = TRUE
      ),

    nb_autres =

      sum(
        destination == "AUTRE",
        na.rm = TRUE
      ),

    prop_hospitalisation =

      mean(
        destination %in% c(
          "TRANSFERT_PSY",
          "TRANSFERT_MCO"
        ),
        na.rm = TRUE
      ),

    prop_transfert_psy =

      mean(
        destination == "TRANSFERT_PSY",
        na.rm = TRUE
      ),

    prop_transfert_mco =

      mean(
        destination == "TRANSFERT_MCO",
        na.rm = TRUE
      ),

# ======================================================================
# 6. Variables dérivées
# ======================================================================

cohort <- cohort |>

  mutate(

    # Durée de suivi en années

    duree_suivi_annees =
      duree_suivi_jours / 365.25,

    # Nombre moyen de passages par an

    frequence_annuelle =
      if_else(
        duree_suivi_annees > 0,
        nb_passages / duree_suivi_annees,
        nb_passages
      ),

    # Au moins une hospitalisation

    hospitalise =
      nb_hospitalisations > 0,

    # Au moins un transfert psychiatrique

    transfert_psy =
      nb_transfert_psy > 0,

    # Au moins un transfert MCO

    transfert_mco =
      nb_transfert_mco > 0

  ),

    # ------------------------------------------------------------------
    # Variables temporelles
    # ------------------------------------------------------------------

    delai_moyen_interpassages =
      mean(
        delai_interpassage_jours,
        na.rm = TRUE
      ),

    delai_median_interpassages =
      median(
        delai_interpassage_jours,
        na.rm = TRUE
      ),

    # ------------------------------------------------------------------
    # Diagnostics
    # ------------------------------------------------------------------

    all_diags_patient =
      list(

        unlist(diag_t) |>
          na.omit()

      ),

    # ------------------------------------------------------------------
    # Frequent Users
    # ------------------------------------------------------------------

    nb_passages_365j_glissant =
      list(
        nb_passages_365j_glissant
      ),

    .groups = "drop"

  )

## ======================================================================
# 6. Variables dérivées
# ======================================================================

cohort <- cohort |>

  mutate(

    # ------------------------------------------------------------------
    # Durée de suivi
    # ------------------------------------------------------------------

    duree_suivi_annees =
      duree_suivi_jours /
      365.25,

    # ------------------------------------------------------------------
    # Fréquence annuelle de recours
    # ------------------------------------------------------------------

    frequence_annuelle =

      if_else(

        duree_suivi_annees > 0,

        nb_passages /
          duree_suivi_annees,

        nb_passages

      ),

    # ------------------------------------------------------------------
    # Retour sans hospitalisation
    # ------------------------------------------------------------------

    prop_rad_na =

      nb_rad_na /
      nb_passages,

    hospitalise =

      nb_hospitalisations > 0

  )

# ======================================================================
# 7. Variables psychiatriques
# ======================================================================

cohort <- cohort |>

  mutate(

    # ------------------------------------------------------------------
    # Diagnostics uniques
    # ------------------------------------------------------------------

    all_diags_uniques =
      lapply(
        all_diags_patient,
        unique
      ),

    nb_diag_total =
      sapply(
        all_diags_patient,
        length
      ),

    nb_diag_uniques =
      sapply(
        all_diags_uniques,
        length
      ),

    nb_classes_cim10 =
      sapply(

        all_diags_uniques,

        \(x)

          n_distinct(

            str_sub(
              x,
              1,
              3
            )

          )

      ),

    # ------------------------------------------------------------------
    # Présence d'un groupe diagnostique
    # ------------------------------------------------------------------

    dx_psychose =
      sapply(
        all_diags_uniques,
        \(x) any(str_detect(x, "^F2"))
      ),

    dx_humeur =
      sapply(
        all_diags_uniques,
        \(x) any(str_detect(x, "^F3"))
      ),

    dx_addiction =
      sapply(
        all_diags_uniques,
        \(x) any(str_detect(x, "^F1"))
      ),

    dx_anxieux =
      sapply(
        all_diags_uniques,
        \(x) any(str_detect(x, "^F4"))
      ),

    dx_personnalite =
      sapply(
        all_diags_uniques,
        \(x) any(str_detect(x, "^F6"))
      ),

    dx_neurodeveloppement =
      sapply(
        all_diags_uniques,
        \(x) any(str_detect(x, "^F8"))
      ),

    dx_autres =
      sapply(

        all_diags_uniques,

        \(x)

          any(

            !str_detect(
              x,
              "^F(1|2|3|4|6|8)"
            )

          )

      )

  )
# ----------------------------------------------------------------------
# Profil diagnostique
# ----------------------------------------------------------------------

cohort <- cohort |>

  rowwise() |>

  mutate(

    freq_diag = list(

      prop.table(

        table(

          str_sub(
            all_diags_patient,
            1,
            2
          )

        )

      )

    ),

    diag_majoritaire =

      names(freq_diag)[which.max(freq_diag)],

    prop_F0 =
      ifelse("F0" %in% names(freq_diag), freq_diag[["F0"]], 0),

    prop_F1 =
      ifelse("F1" %in% names(freq_diag), freq_diag[["F1"]], 0),

    prop_F2 =
      ifelse("F2" %in% names(freq_diag), freq_diag[["F2"]], 0),

    prop_F3 =
      ifelse("F3" %in% names(freq_diag), freq_diag[["F3"]], 0),

    prop_F4 =
      ifelse("F4" %in% names(freq_diag), freq_diag[["F4"]], 0),

    prop_F5 =
      ifelse("F5" %in% names(freq_diag), freq_diag[["F5"]], 0),

    prop_F6 =
      ifelse("F6" %in% names(freq_diag), freq_diag[["F6"]], 0),

    prop_F7 =
      ifelse("F7" %in% names(freq_diag), freq_diag[["F7"]], 0),

    prop_F8 =
      ifelse("F8" %in% names(freq_diag), freq_diag[["F8"]], 0),

    prop_F9 =
      ifelse("F9" %in% names(freq_diag), freq_diag[["F9"]], 0)

  ) |>

  ungroup()

# ======================================================================
# 8. Frequent Users
# ======================================================================

cohort <- cohort |>

  mutate(

    # ------------------------------------------------------------------
    # Maximum de passages observés sur 365 jours glissants
    # ------------------------------------------------------------------

    nb_passages_365j_max =
      sapply(
        nb_passages_365j_glissant,
        max,
        na.rm = TRUE
      ),

    # ------------------------------------------------------------------
    # Définitions classiques des Frequent Users
    # ------------------------------------------------------------------

    FU_n_2 =
      nb_passages_365j_max >= 2,

    FU_n_3 =
      nb_passages_365j_max >= 3,

    FU_n_4 =
      nb_passages_365j_max >= 4,

    FU_n_5 =
      nb_passages_365j_max >= 5,

    FU_n_10 =
      nb_passages_365j_max >= 10

  )

#======================================================================
#9. Percentiles d'activité
#======================================================================

# Patients classés selon leur nombre total de passages

cohort <- cohort |>

  arrange(
    desc(nb_passages)
  ) |>

  mutate(

    rang_activite =
      row_number(),

    percentile_activite =
      percent_rank(nb_passages)

  )

#======================================================================
#10. Contribution des patients à l'activité totale
#======================================================================

n_patients <- nrow(cohort)

contribution_activite <- tibble(

  groupe = c(
    "Top 1 %",
    "Top 5 %",
    "Top 10 %",
    "Top 20 %"
  ),

  proportion_patients = c(
    0.01,
    0.05,
    0.10,
    0.20
  )

) |>

  rowwise() |>

  mutate(

    n_patients =

      ceiling(
        proportion_patients *
          n_patients
      ),

    passages =

      sum(

        cohort |>

          arrange(desc(nb_passages)) |>

          slice_head(n = n_patients) |>

          pull(nb_passages)

      ),

    part_activite =

      passages /

      sum(cohort$nb_passages)

  ) |>

  ungroup()

#======================================================================
#11. Contrôle qualité de la cohorte
#======================================================================

message("======================================================")
message("Contrôle qualité")
message("======================================================")

message("Patients : ", nrow(cohort))

message("Passages : ", sum(cohort$nb_passages))

message("Passages moyens : ", round(mean(cohort$nb_passages), 2))

message("Frequent Users (≥4) : ", sum(cohort$FU_n_4))

message(
  "Hospitalisés au moins une fois : ",
  sum(cohort$hospitalise)
)

summary(cohort$nb_passages)
summary(cohort$age_moyen)
summary(cohort$duree_suivi_jours)

#======================================================================
#13. Sauvegarde de la cohorte et des résultats
#======================================================================

saveRDS(

  cohort,

  here::here(
    "data",
    "processed",
    "cohort.rds"
  )

)

write_csv(

  contribution_activite,

  here::here(
    "results",
    "tables",
    "contribution_activite.csv"
  )

)

message("======================================================")
message("Cohorte créée avec succès")
message("======================================================")

# ======================================================================
# Export pour pvalue.io
# ======================================================================

message("Préparation de l'export pour pvalue.io...")

cohort_export <- cohort |>

  mutate(

    # --------------------------------------------------------------
    # Dates au format ISO
    # --------------------------------------------------------------

    across(

      where(lubridate::is.POSIXct),

      ~ format(
        .x,
        "%Y-%m-%d %H:%M:%S"
      )

    ),

    across(

      where(inherits, "Date"),

      ~ format(
        .x,
        "%Y-%m-%d"
      )

    ),

    # --------------------------------------------------------------
    # Variables logiques
    # --------------------------------------------------------------

    across(

      where(is.logical),

      ~ as.integer(.x)

    )

  )

# --------------------------------------------------------------
# Conversion des list-columns
# --------------------------------------------------------------

list_cols <-
  names(cohort_export)[
    sapply(cohort_export, is.list)
  ]

for(col in list_cols){

  cohort_export[[col]] <-

    sapply(

      cohort_export[[col]],

      function(x){

        if(length(x) == 0 || all(is.na(x))){

          return("")

        }

        paste(unique(x), collapse = ";")

      }

    )

}

# --------------------------------------------------------------
# Export
# --------------------------------------------------------------

readr::write_delim(

  cohort_export,

  here::here(
    "results",
    "cohort_pvalue.txt"
  ),

  delim = "\t",

  na = ""

)

message("Export pvalue.io terminé.")