# ======================================================================
# FU-SAU
# 04_figure1_flowchart.R
# Figure 1 - Flowchart de sélection de la cohorte
# ======================================================================

# ======================================================================
# 0. Packages
# ======================================================================

library(tidyverse)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(here)


# ======================================================================
# 1. Chargement des données
# ======================================================================

# Données brutes/nettoyées
pass <- readRDS(
  here::here(
    "data",
    "processed",
    "pass_clean.rds"
  )
)

# Cohorte analytique
cohort <- readRDS(
  here::here(
    "data",
    "processed",
    "cohort.rds"
  )
)


# ======================================================================
# 2. Calcul des effectifs
# ======================================================================

# ----------------------------------------------------------------------
# 2.1 Population initiale
# ----------------------------------------------------------------------

n_passages_initial <-

  nrow(pass)


n_patients_initial <-

  n_distinct(
    pass$id_patient
  )


# ----------------------------------------------------------------------
# 2.2 Exclusion des patients mineurs
# ----------------------------------------------------------------------

# Dans ton protocole, l'exclusion porte sur les patients dont
# le premier passage survient avant 18 ans.
#
# On reproduit donc ici la même logique que dans 03_build_cohort.R.

patients_adultes <-

  pass |>

  arrange(
    id_patient,
    date_arrivee
  ) |>

  group_by(
    id_patient
  ) |>

  summarise(

    age_premier_passage =
      first(age),

    .groups =
      "drop"

  ) |>

  filter(
    age_premier_passage >= 18
  )


# Nombre de patients exclus

n_patients_mineurs <-

  n_patients_initial -
  nrow(patients_adultes)


# Nombre de passages correspondant aux patients exclus

pass_patients_mineurs <-

  pass |>

  semi_join(
    patients_adultes |>
      select(id_patient),
    by = "id_patient"
  )


# Correction :
# On récupère directement les patients mineurs.

patients_mineurs <-

  pass |>

  arrange(
    id_patient,
    date_arrivee
  ) |>

  group_by(
    id_patient
  ) |>

  summarise(

    age_premier_passage =
      first(age),

    .groups =
      "drop"

  ) |>

  filter(
    age_premier_passage < 18
  )


n_passages_mineurs <-

  pass |>

  semi_join(
    patients_mineurs,
    by = "id_patient"
  ) |>

  nrow()


# ----------------------------------------------------------------------
# 2.3 Population adulte éligible
# ----------------------------------------------------------------------

pass_adultes <-

  pass |>

  semi_join(
    patients_adultes,
    by = "id_patient"
  )


n_passages_adultes <-

  nrow(pass_adultes)


n_patients_adultes <-

  n_distinct(
    pass_adultes$id_patient
  )


# ----------------------------------------------------------------------
# 2.4 Cohorte analytique
# ----------------------------------------------------------------------

n_patients_cohorte <-

  nrow(cohort)


# ----------------------------------------------------------------------
# 2.5 Frequent Users
# ----------------------------------------------------------------------

n_fu <-

  sum(
    cohort$FU3,
    na.rm = TRUE
  )


n_non_fu <-

  sum(
    !cohort$FU3,
    na.rm = TRUE
  )


prop_fu <-

  100 *
  n_fu /
  n_patients_cohorte


prop_non_fu <-

  100 *
  n_non_fu /
  n_patients_cohorte


# ======================================================================
# 3. Fonction pour formater les effectifs
# ======================================================================

format_n <- function(x) {

  format(
    x,
    big.mark = " ",
    scientific = FALSE
  )

}


# ======================================================================
# 4. Création des textes
# ======================================================================

texte_initial <-

  paste0(
    "Population initiale\\n",
    format_n(n_passages_initial),
    " passages\\n",
    format_n(n_patients_initial),
    " patients"
  )


texte_exclusion <-

  paste0(
    "Exclusion des patients mineurs\\n",
    format_n(n_patients_mineurs),
    " patients\\n",
    format_n(n_passages_mineurs),
    " passages"
  )


texte_adultes <-

  paste0(
    "Patients adultes éligibles\\n",
    "Âge ≥ 18 ans\\n",
    format_n(n_passages_adultes),
    " passages\\n",
    format_n(n_patients_adultes),
    " patients"
  )


texte_cohorte <-

  paste0(
    "Cohorte analytique\\n",
    "Une ligne = un patient\\n",
    format_n(n_patients_cohorte),
    " patients"
  )


texte_fu <-

  paste0(
    "Frequent Users (FU3)\\n",
    "≥ 3 passages / 365 jours\\n",
    "n = ",
    format_n(n_fu),
    " (",
    sprintf("%.1f", prop_fu),
    " %)"
  )


texte_non_fu <-

  paste0(
    "Non-Frequent Users\\n",
    "< 3 passages / 365 jours\\n",
    "n = ",
    format_n(n_non_fu),
    " (",
    sprintf("%.1f", prop_non_fu),
    " %)"
  )


# ======================================================================
# 5. Création du flowchart
# ======================================================================

flowchart <-

  DiagrammeR::grViz(

    paste0(

      "
      digraph flowchart {

        graph [
          layout = dot,
          rankdir = TB,
          bgcolor = white,
          nodesep = 0.45,
          ranksep = 0.60,
          margin = 0.20
        ]

        node [
          shape = box,
          style = 'rounded',
          fontname = 'Arial',
          fontsize = 12,
          color = '#333333',
          fontcolor = '#111111',
          penwidth = 1.2,
          margin = 0.20
        ]

        edge [
          color = '#555555',
          penwidth = 1.2,
          arrowsize = 0.7
        ]


        initial [
          label = '",
      texte_initial,
      "',
          width = 4.5
        ]


        adultes [
          label = '",
      texte_adultes,
      "',
          width = 4.5
        ]


        cohorte [
          label = '",
      texte_cohorte,
      "',
          width = 4.5
        ]


        fu [
          label = '",
      texte_fu,
      "',
          width = 3.2
        ]


        nonfu [
          label = '",
      texte_non_fu,
      "',
          width = 3.2
        ]


        exclusion [
          label = '",
      texte_exclusion,
      "',
          shape = box,
          style = 'rounded,dashed',
          color = '#777777',
          fontcolor = '#444444',
          width = 3.5
        ]


        initial -> adultes

        adultes -> cohorte

        cohorte -> fu

        cohorte -> nonfu


        exclusion [
          pos = '0,0!'
        ]

      }
      "

    )

  )


# ======================================================================
# 6. Affichage
# ======================================================================

flowchart


# ======================================================================
# 7. Export SVG
# ======================================================================

svg_code <-

  DiagrammeRsvg::export_svg(
    flowchart
  )


writeLines(
  svg_code,
  here::here(
    "data",
    "exports",
    "figure1_flowchart.svg"
  )
)


# ======================================================================
# 8. Export PNG haute résolution
# ======================================================================

rsvg::rsvg_png(

  charToRaw(svg_code),

  file = here::here(
    "data",
    "exports",
    "figure1_flowchart.png"
  ),

  width = 2400,

  height = 1800

)


# ======================================================================
# 9. Contrôle des effectifs
# ======================================================================

message(
  "\n--- Effectifs Figure 1 ---"
)

message(
  "Population initiale : ",
  format_n(n_patients_initial),
  " patients / ",
  format_n(n_passages_initial),
  " passages"
)

message(
  "Patients mineurs exclus : ",
  format_n(n_patients_mineurs),
  " patients / ",
  format_n(n_passages_mineurs),
  " passages"
)

message(
  "Population adulte : ",
  format_n(n_patients_adultes),
  " patients / ",
  format_n(n_passages_adultes),
  " passages"
)

message(
  "Cohorte analytique : ",
  format_n(n_patients_cohorte),
  " patients"
)

message(
  "FU3 : ",
  format_n(n_fu),
  " (",
  sprintf("%.1f", prop_fu),
  " %)"
)

message(
  "Non-FU3 : ",
  format_n(n_non_fu),
  " (",
  sprintf("%.1f", prop_non_fu),
  " %)"
)

message(
  "\nFigure exportée dans data/exports/"
)