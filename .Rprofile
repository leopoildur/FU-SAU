# ======================================================================
# Configuration R du projet FU-SAU
# ======================================================================

# Bibliothèque utilisateur correspondant à la version majeure/mineure de R
r_version <- paste(
  R.version$major,
  strsplit(R.version$minor, "\\.")[[1]][1],
  sep = "."
)

user_library <- file.path(
  Sys.getenv("LOCALAPPDATA"),
  "R",
  "win-library",
  r_version
)

# Crée le dossier s'il n'existe pas
if (!dir.exists(user_library)) {
  dir.create(
    user_library,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

# Utilise la bibliothèque utilisateur en priorité
.libPaths(
  c(
    user_library,
    .Library.site,
    .Library
  )
)

local({
  # 1. Détection des cœurs matériels (On garde 1 cœur libre pour la stabilité du système)
  n_cores <- max(1, parallel::detectCores(logical = FALSE) - 1)
  
  # 2. Configuration pour les fonctions de base (ex: mclapply)
  options(mc.cores = n_cores)
  
  # 3. Configuration pour le framework 'future' (utilisé par furrr pour le tidyverse)
  if (requireNamespace("future", quietly = TRUE)) {
    future::plan(future::multisession, workers = n_cores)
  }
  
  # 4. Configuration pour data.table (si vous l'utilisez)
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::setDTthreads(n_cores)
  }
})