# Installation des packages du projet FU-SAU ----
options(download.file.method = "wininet")
# Installation des packages via renv ----

renv::install(
  c(
    "tidyverse",
    "readxl",
    "janitor",
    "here",
    "skimr",
    "lubridate",
    "naniar",
    "gtsummary",
    "gt"
  )
)

# Sauvegarde de l'environnement reproductible ----

renv::snapshot()