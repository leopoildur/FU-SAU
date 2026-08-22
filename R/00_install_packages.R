# ======================================================================
# Projet FU-SAU
# 00_install_packages.R
# ======================================================================
#
# Ce script est à utiliser :
# - lors de la création du projet ;
# - lorsqu'un nouveau package est ajouté.
#
# Les packages sont installés uniquement s'ils ne sont pas déjà présents.
# ======================================================================


# ======================================================================
# 1. Packages du projet
# ======================================================================

packages <- c(
  "tidyverse",
  "readxl",
  "janitor",
  "here",
  "skimr",
  "lubridate",
  "naniar",
  "gtsummary",
  "gt",
  "slider",
  "timeDate",
  "DiagrammeR",
  "igraph",
  "labelled"
)


# ======================================================================
# 2. Installation des packages manquants
# ======================================================================

packages_installes <-

  rownames(
    installed.packages()
  )


packages_manquants <-

  setdiff(
    packages,
    packages_installes
  )


if (length(packages_manquants) > 0) {

  install.packages(
    packages_manquants,
    repos = "https://cloud.r-project.org"
  )

} else {

  message(
    "Tous les packages nécessaires sont déjà installés."
  )

}


# ======================================================================
# 3. Vérification
# ======================================================================

packages_non_charges <-

  packages[
    !vapply(
      packages,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]


if (length(packages_non_charges) > 0) {

  warning(
    "Les packages suivants n'ont pas pu être installés : ",
    paste(
      packages_non_charges,
      collapse = ", "
    )
  )

} else {

  message(
    "Tous les packages du projet sont disponibles."
  )

}