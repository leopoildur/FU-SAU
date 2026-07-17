# Projet FU-SAU ---------------------------------------------------------

# Ce script est à utiliser uniquement :
# - lors de la création du projet ;
# - lorsqu'un nouveau package est ajouté.

# Installation de renv --------------------------------------------------

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Packages du projet ----------------------------------------------------

packages <- c(
  "tidyverse",
  "readxl",
  "janitor",
  "here",
  "skimr",
  "lubridate",
  "naniar",
  "gtsummary",
  "gt"
  "timeDate"
)

renv::install(packages)

# Mise à jour du lockfile ------------------------------------------------

renv::snapshot()
