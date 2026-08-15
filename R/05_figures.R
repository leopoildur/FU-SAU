# ======================================================================
# FU-SAU
# 05_figures.R
# Tableaux et figures
# ======================================================================
#
# Objectif :
# Construire le Tableau 1 décrivant la population selon le statut
# Frequent User (FU3).
#
# Ordre des sections :
#   1. Préparation des données
#   2. Caractéristiques sociodémographiques
#   3. Diagnostics selon Schmoll
#   4. Hospitalisations
#   5. Recours aux urgences
#   6. Assemblage du Tableau 1
#   7. Mise en forme
#
# Colonnes du tableau :
#   FU | Non-FU | Population totale | p
#
# ======================================================================


# ======================================================================
# 0. Packages
# ======================================================================

library(tidyverse)
library(gtsummary)
library(gt)


# ======================================================================
# 1. Préparation des données
# ======================================================================


# ----------------------------------------------------------------------
# 1.1 Durée moyenne de soins par patient
# ----------------------------------------------------------------------
#
# LOS est une variable de durée exprimée en minutes.
#
# Pour chaque patient, on calcule :
#   → la durée moyenne de ses passages aux urgences
#
# Cette variable sera ensuite présentée dans le Tableau 1 sous la forme :
#   médiane [Q1–Q3]
#
# ----------------------------------------------------------------------

duree_soins_patient <-

  pass_enrichi |>

  group_by(id_patient) |>

  summarise(

    duree_soins_moyenne =

      if (
        all(is.na(LOS))
      ) {

        NA_real_

      } else {

        mean(
          as.numeric(LOS),
          na.rm = TRUE
        )

      },

    .groups = "drop"

  )


# ----------------------------------------------------------------------
# 1.2 Vérification rapide de la durée de soins
# ----------------------------------------------------------------------

message(
  "Nombre de patients avec une durée moyenne de soins disponible : ",
  sum(!is.na(duree_soins_patient$duree_soins_moyenne))
)

message(
  "Nombre de patients sans durée de soins disponible : ",
  sum(is.na(duree_soins_patient$duree_soins_moyenne))
)


# ----------------------------------------------------------------------
# 1.3 Construction de la table utilisée pour le Tableau 1
# ----------------------------------------------------------------------

tableau1_data <-

  cohort |>

  # Ajout des passages de jour et de nuit
  left_join(
    cohort_tableau1_recours,
    by = "id_patient"
  ) |>

  # Ajout des diagnostics selon Schmoll
  left_join(

    cohort_diag |>

      select(

        id_patient,

        diag_F1_schmoll,
        diag_F6_schmoll,
        diag_autres_schmoll,

        diag_dominant_schmoll

      ),

    by = "id_patient"

  ) |>

  # Ajout de la durée moyenne de soins
  left_join(
    duree_soins_patient,
    by = "id_patient"
  ) |>

  mutate(

    # ==============================================================
    # Résidence
    # ==============================================================

    residence_region =

      case_when(

        departement == "94" ~

          "Val-de-Marne (94)",

        departement %in% c(
          "75",
          "77",
          "78",
          "91",
          "92",
          "93",
          "95"
        ) ~

          "Île-de-France hors Val-de-Marne",

        TRUE ~

          "Hors Île-de-France"

      ),

    residence_region =

      factor(

        residence_region,

        levels = c(

          "Val-de-Marne (94)",

          "Île-de-France hors Val-de-Marne",

          "Hors Île-de-France"

        )

      ),


    # ==============================================================
    # Sexe
    # ==============================================================

    sexe =

      factor(

        sexe,

        levels = c(
          "F",
          "M"
        ),

        labels = c(
          "Femme",
          "Homme"
        )

      ),


    # ==============================================================
    # Hospitalisation
    # ==============================================================
    #
    # Les variables sont binaires.
    #
    # Dans le tableau, seule la modalité "Oui" sera affichée.
    #
    # ==============================================================

    hospitalisation =

      factor(

        hospitalisation,

        levels = c(
          FALSE,
          TRUE
        ),

        labels = c(
          "Non",
          "Oui"
        )

      ),

    hospitalisation_sl =

      factor(

        hospitalisation_sl,

        levels = c(
          FALSE,
          TRUE
        ),

        labels = c(
          "Non",
          "Oui"
        )

      ),

    hospitalisation_ssc =

      factor(

        hospitalisation_ssc,

        levels = c(
          FALSE,
          TRUE
        ),

        labels = c(
          "Non",
          "Oui"
        )

      ),


    # ==============================================================
    # Diagnostics selon Schmoll
    # ==============================================================
    #
    # F1, F6 et "Autres" sont des catégories inclusives.
    #
    # F2, F3 et F4 correspondent au diagnostic dominant.
    #
    # ==============================================================

    diag_F1_schmoll =

      factor(

        diag_F1_schmoll,

        levels = c(
          FALSE,
          TRUE
        ),

        labels = c(
          "Non",
          "Oui"
        )

      ),

    diag_F6_schmoll =

      factor(

        diag_F6_schmoll,

        levels = c(
          FALSE,
          TRUE
        ),

        labels = c(
          "Non",
          "Oui"
        )

      ),

    diag_autres_schmoll =

      factor(

        diag_autres_schmoll,

        levels = c(
          FALSE,
          TRUE
        ),

        labels = c(
          "Non",
          "Oui"
        )

      ),


    # ==============================================================
    # Diagnostic dominant F2 / F3 / F4
    # ==============================================================

    diag_dominant_schmoll =

      factor(

        diag_dominant_schmoll,

        levels = c(
          "F2",
          "F3",
          "F4"
        )

      ),


    # ==============================================================
    # Statut Frequent User
    # ==============================================================
    #
    # TRUE  = FU
    # FALSE = Non-FU
    #
    # L'ordre est volontaire :
    # FU | Non-FU
    #
    # ==============================================================

    FU3 =

      factor(

        FU3,

        levels = c(
          TRUE,
          FALSE
        ),

        labels = c(
          "FU",
          "Non-FU"
        )

      )

  )


# ======================================================================
# 2. Caractéristiques sociodémographiques
# ======================================================================


tableau1_socio <-

  tableau1_data |>

  select(

    age,
    sexe,
    residence_region,
    FU3

  ) |>

  tbl_summary(

    by = FU3,

    statistic = list(

      # Variables quantitatives
      # → médiane [Q1–Q3]

      all_continuous() ~
        "{median} [{p25}, {p75}]",

      # Variables qualitatives
      # → n (%)

      all_categorical() ~
        "{n} ({p}%)"

    ),

    digits = list(

      all_continuous() ~ 1,

      all_categorical() ~
        c(0, 1)

    ),

    missing = "no",

    label = list(

      age ~
        "Âge, ans",

      sexe ~
        "Sexe",

      residence_region ~
        "Résidence"

    )

  ) |>

  add_overall(
    last = TRUE
  ) |>

  add_p()


# ======================================================================
# 3. Diagnostics selon Schmoll
# ======================================================================


tableau1_diagnostics <-

  tableau1_data |>

  select(

    diag_dominant_schmoll,

    diag_F1_schmoll,
    diag_F6_schmoll,
    diag_autres_schmoll,

    FU3

  ) |>

  tbl_summary(

    by = FU3,

    type = list(

      diag_F1_schmoll ~
        "dichotomous",

      diag_F6_schmoll ~
        "dichotomous",

      diag_autres_schmoll ~
        "dichotomous"

    ),

    value = list(

      diag_F1_schmoll ~
        "Oui",

      diag_F6_schmoll ~
        "Oui",

      diag_autres_schmoll ~
        "Oui"

    ),

    statistic = list(

      # Diagnostic dominant
      # → F2 / F3 / F4

      diag_dominant_schmoll ~
        "{n} ({p}%)",

      # Variables binaires
      # → uniquement "Oui"

      all_dichotomous() ~
        "{n} ({p}%)"

    ),

    digits = list(

      all_categorical() ~
        c(0, 1)

    ),

    missing = "no",

    label = list(

      diag_dominant_schmoll ~
        "Diagnostic dominant",

      diag_F1_schmoll ~
        "F1",

      diag_F6_schmoll ~
        "F6",

      diag_autres_schmoll ~
        "Autres diagnostics"

    )

  ) |>

  add_overall(
    last = TRUE
  ) |>

  add_p()


# ======================================================================
# 4. Hospitalisations
# ======================================================================


tableau1_hospitalisation <-

  tableau1_data |>

  select(

    hospitalisation,
    hospitalisation_sl,
    hospitalisation_ssc,

    FU3

  ) |>

  tbl_summary(

    by = FU3,

    # Les trois variables sont binaires.
    # On affichera uniquement la modalité "Oui".

    type = list(

      hospitalisation ~
        "dichotomous",

      hospitalisation_sl ~
        "dichotomous",

      hospitalisation_ssc ~
        "dichotomous"

    ),

    value = list(

      hospitalisation ~
        "Oui",

      hospitalisation_sl ~
        "Oui",

      hospitalisation_ssc ~
        "Oui"

    ),

    statistic =

      all_dichotomous() ~
      "{n} ({p}%)",

    digits =

      all_dichotomous() ~
      c(0, 1),

    missing = "no",

    label = list(

      hospitalisation ~
        "≥1 hospitalisation",

      hospitalisation_sl ~
        "≥1 hospitalisation en SL",

      hospitalisation_ssc ~
        "≥1 hospitalisation en SSC"

    )

  ) |>

  add_overall(
    last = TRUE
  ) |>

  add_p()


# ======================================================================
# 5. Recours aux urgences
# ======================================================================


tableau1_recours <-

  tableau1_data |>

  select(

    duree_soins_moyenne,

    FU3

  ) |>

  tbl_summary(

    by = FU3,

    statistic =

      all_continuous() ~
      "{median} [{p25}, {p75}]",

    digits =

      all_continuous() ~
      1,

    missing = "no",

    label = list(

      duree_soins_moyenne ~
        "Durée moyenne de soins par passage, min"

    )

  ) |>

  add_overall(
    last = TRUE
  ) |>

  add_p()


# ======================================================================
# 6. Assemblage du Tableau 1
# ======================================================================


tableau1 <-

  tbl_stack(

    tbls = list(

      tableau1_socio,

      tableau1_diagnostics,

      tableau1_hospitalisation,

      tableau1_recours

    ),

    group_header = c(

      "Caractéristiques sociodémographiques",

      "Diagnostics selon Schmoll",

      "Hospitalisations",

      "Recours aux urgences"

    )

  ) |>

  bold_labels()


# ======================================================================
# 7. Mise en forme du tableau
# ======================================================================


tableau1_gt <-

  tableau1 |>

  modify_caption(

    "**Tableau 1. Caractéristiques des patients selon le statut de Frequent User**"

  ) |>

  modify_header(

    label ~
      "**Caractéristiques**",

    stat_1 ~
      "**FU**",

    stat_2 ~
      "**Non-FU**",

    stat_0 ~
      "**Population totale**",

    p.value ~
      "**p**"

  ) |>

  as_gt() |>

  tab_options(

    table.font.size = 11,

    heading.title.font.size = 12,

    column_labels.font.weight = "bold",

    row_group.font.weight = "bold",

    data_row.padding = px(3)

  )


# ======================================================================
# 8. Note de bas de tableau
# ======================================================================


tableau1_gt <-

  tableau1_gt |>

  tab_source_note(

    source_note =

      md(

        paste0(

          "**Données présentées en n (%) ou médiane [Q1–Q3].** ",

          "Les p-values correspondent à la comparaison entre les groupes FU et non-FU. ",

          "FU : Frequent User ; ",

          "Q1–Q3 : premier et troisième quartiles ; ",

          "SL : soins libres ; ",

          "SSC : soins sans consentement. ",

          "Le diagnostic dominant F2/F3/F4 est défini selon la méthode de Schmoll, ",

          "avec priorité F2 > F3 > F4 en cas d'égalité. ",

          "F1, F6 et les autres diagnostics sont des catégories inclusives ",

          "et peuvent coexister avec le diagnostic dominant. ",

          "La durée moyenne de soins est calculée pour chaque patient à partir ",

          "de la durée de séjour (LOS) de ses passages."

        )

      )

  )


# ======================================================================
# 9. Affichage
# ======================================================================


tableau1_gtk