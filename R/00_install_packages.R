# Projet FU-SAU ---------------------------------------------------------

# Ce script est à utiliser uniquement :
# - lors de la création du projet ;
# - lorsqu'un nouveau package est ajouté.

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
  "ggplot2",
  "gt",
  "slider",
  "timeDate"
)
install.packages(setdiff(packages, rownames(installed.packages())))