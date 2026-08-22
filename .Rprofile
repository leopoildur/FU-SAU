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
