# ======================================================================
# FU-SAU
# 04_figure1_flowchart.R
# Figure 1 - Flowchart de sélection de la cohorte
# ======================================================================

library(tidyverse)
library(DiagrammeR)
library(here)

# ======================================================================
# 1. Chargement des données
# ======================================================================

pass <- readRDS(here::here("data", "processed", "pass_clean.rds"))
cohort <- readRDS(here::here("data", "processed", "cohort.rds"))

# ======================================================================
# 2. Calcul des effectifs
# ======================================================================

n_passages_initial <- nrow(pass)
n_patients_initial <- n_distinct(pass$id_patient)

patients_adultes <- pass |>
  arrange(id_patient, date_arrivee) |>
  group_by(id_patient) |>
  summarise(age_premier_passage = first(age), .groups = "drop") |>
  filter(age_premier_passage >= 18)

patients_mineurs <- pass |>
  arrange(id_patient, date_arrivee) |>
  group_by(id_patient) |>
  summarise(age_premier_passage = first(age), .groups = "drop") |>
  filter(age_premier_passage < 18)

n_patients_mineurs <- nrow(patients_mineurs)
n_passages_mineurs <- pass |> semi_join(patients_mineurs, by = "id_patient") |> nrow()

pass_adultes <- pass |> semi_join(patients_adultes, by = "id_patient")
n_passages_adultes <- nrow(pass_adultes)
n_patients_adultes <- n_distinct(pass_adultes$id_patient)

n_patients_cohorte <- nrow(cohort)

n_fu <- sum(cohort$FU3, na.rm = TRUE)
n_non_fu <- sum(!cohort$FU3, na.rm = TRUE)
prop_fu <- 100 * n_fu / n_patients_cohorte
prop_non_fu <- 100 * n_non_fu / n_patients_cohorte

# ======================================================================
# 3. Fonction pour formater les effectifs
# ======================================================================

format_n <- function(x) {
  format(x, big.mark = " ", scientific = FALSE)
}

# ======================================================================
# 4. Création des textes (Format scientifique, alignement à gauche)
# ======================================================================

# L'utilisation de \l dans Graphviz permet d'aligner le texte à gauche.
# En R, il faut l'échapper : \\l

texte_initial <- paste0(
  "Initial population\\l",
  "n = ", format_n(n_passages_initial), " visits\\l",
  "n = ", format_n(n_patients_initial), " patients\\l"
)

texte_exclusion <- paste0(
  "Excluded (age < 18 years)\\l",
  "n = ", format_n(n_passages_mineurs), " visits\\l",
  "n = ", format_n(n_patients_mineurs), " patients\\l"
)

texte_adultes <- paste0(
  "Eligible adult patients\\l",
  "n = ", format_n(n_passages_adultes), " visits\\l",
  "n = ", format_n(n_patients_adultes), " patients\\l"
)

texte_cohorte <- paste0(
  "Analytic cohort\\l",
  "n = ", format_n(n_patients_cohorte), " patients\\l"
)

texte_fu <- paste0(
  "Frequent Users (FU3)\\l",
  "(\u2265 3 visits / 365 days)\\l",
  "n = ", format_n(n_fu), " (", sprintf("%.1f", prop_fu), "%)\\l"
)

texte_non_fu <- paste0(
  "Non-Frequent Users\\l",
  "(< 3 visits / 365 days)\\l",
  "n = ", format_n(n_non_fu), " (", sprintf("%.1f", prop_non_fu), "%)\\l"
)

# ======================================================================
# 5. Création du flowchart
# ======================================================================

flowchart <- DiagrammeR::grViz(
  paste0(
    "
    digraph flowchart {
      graph [layout = dot, rankdir = TB, splines = ortho, nodesep = 0.5, ranksep = 0.5]
      
      node [shape = box, fontname = 'Helvetica', fontsize = 10, 
            color = '#000000', fontcolor = '#000000', penwidth = 0.8, margin = '0.2,0.1']
      
      edge [color = '#000000', penwidth = 0.8, arrowsize = 0.6]

      # Noeuds principaux
      initial [label = '", texte_initial, "']
      adultes [label = '", texte_adultes, "']
      cohorte [label = '", texte_cohorte, "']
      exclusion [label = '", texte_exclusion, "']
      fu [label = '", texte_fu, "']
      nonfu [label = '", texte_non_fu, "']

      # Noeud d'embranchement invisible
      dummy [shape = point, width = 0, height = 0]

      # Flux principal et exclusion
      initial -> dummy [arrowhead = none]
      dummy -> adultes
      dummy -> exclusion
      
      { rank = same; dummy; exclusion }

      adultes -> cohorte
      
      # Groupement des noeuds finaux pour l'alignement
      cohorte -> fu
      cohorte -> nonfu
      
      { rank = same; fu; nonfu }
    }
    "
  )
)

# ======================================================================
# 6. Affichage 
# ======================================================================

flowchart