# ======================================================================
# FU-SAU
# 01_import.R
# ======================================================================
#
# Objectif :
#   - importer les tables Excel
#   - contrôler l'importation
#   - sauvegarder les données brutes au format RDS
#
# Entrée :
#   data/raw/FU-SAU_DATA.xlsx
#
# Sortie :
#   data/interim/
#     - avis_raw.rds
#     - pass_raw.rds
#     - legend.rds
#
# ======================================================================
# ======================================================================
# FU-SAU
# 01_import.R
# Importation des données
# ======================================================================

# Chargement de l'environnement -----------------------------------------

source(here::here("R", "utils", "load_project.R"))

# Vérification des fichiers ---------------------------------------------

stopifnot(
  file.exists(excel_file)
)

# Import des tables ------------------------------------------------------

legend <- read_sheet("LEGEND")

pass <- read_sheet("PASS")

avis <- read_sheet("AVIS")

# Contrôle structure -----------------------------------------------------

check_structure(legend)


check_structure(pass)

check_structure(avis)

# Contrôle des identifiants ---------------------------------------------

check_id(pass, i_ddos)

check_id(pass, idpat)

check_id(avis, i_ddos)

# Contrôle relationnel ---------------------------------------------------

avis_sans_pass <- avis |>
  anti_join(
    pass,
    by = "i_ddos"
  )

nrow(avis_sans_pass)

# Nombre d'avis par passage ---------------------------------------------

avis |>
  count(
    i_ddos,
    name = "n_avis"
  ) |>
  arrange(
    desc(n_avis)
  )

# Sauvegarde -------------------------------------------------------------

fs::dir_create(here::here("data", "interim"))

saveRDS(
  legend,
  here(
    "data",
    "interim",
    "legend_raw.rds"
  )
)

saveRDS(
  pass,
  here(
    "data",
    "interim",
    "pass_raw.rds"
  )
)

saveRDS(
  avis,
  here(
    "data",
    "interim",
    "avis_raw.rds"
  )
)

message(
  "Import terminé."
)
