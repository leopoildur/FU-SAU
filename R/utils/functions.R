# ======================================================================
# FU-SAU
# utils/functions.R
# Fonctions utilitaires du projet
# ======================================================================

# ======================================================================
# Importation
# ======================================================================

# Lire une feuille Excel et harmoniser les noms des variables
read_sheet <- function(sheet) {

  readxl::read_excel(
    path = excel_file,
    sheet = sheet
  ) |>
    janitor::clean_names()

}


# Sauvegarder un objet au format RDS (intermédiaire)
save_interim <- function(data, file_name) {

  saveRDS(
    object = data,
    file = here::here(
      "data",
      "interim",
      paste0(file_name, ".rds")
    )
  )

}


# Sauvegarder un objet nettoyé
save_processed <- function(data, file_name) {

  saveRDS(
    object = data,
    file = here::here(
      "data",
      "processed",
      paste0(file_name, ".rds")
    )
  )

}


# ======================================================================
# Variables temporelles
# ======================================================================

# Conversion d'une date française au format POSIXct ---------------------

parse_datetime_fr <- function(x) {

  lubridate::dmy_hm(
    x,
    tz = "Europe/Paris"
  )

}


# Heure de la journée
get_hour <- function(x) {

  lubridate::hour(x)

}


# Passage nocturne
is_night <- function(x) {

  lubridate::hour(x) >= night_start |
    lubridate::hour(x) < night_end

}


# Week-end
is_weekend <- function(x) {

  lubridate::wday(x) %in% weekend_days

}


# Jour de la semaine
weekday_label <- function(x) {

  lubridate::wday(
    x,
    label = TRUE,
    abbr = FALSE
  )

}

# Jour férié français ---------------------------------------------------

is_holiday <- function(x) {

  as.Date(x) %in% jours_feries

}

# ======================================================================
# Variables géographiques
# ======================================================================

# Département à partir du code postal
get_department <- function(cp) {

  dplyr::case_when(

    stringr::str_starts(cp, "99") ~ "ETRANGER",

    stringr::str_sub(cp, 1, 3) %in% dom_codes ~
      stringr::str_sub(cp, 1, 3),

    TRUE ~
      stringr::str_sub(cp, 1, 2)

  )

}


# ======================================================================
# Diagnostics CIM-10
# ======================================================================

# Préfixe CIM-10
get_cim10_prefix <- function(code) {

  stringr::str_sub(
    code,
    1,
    2
  )

}


# classe CIM-10
get_cim10_class <- function(code) {

  dplyr::recode(
    get_cim10_prefix(code),
    !!!cim10_dictionary,
    .default = NA_character_
  )

}


# ======================================================================
# Variables catégorielles
# ======================================================================

# MLS
get_mls_label <- function(x) {

  dplyr::recode(
    x,
    !!!mls_dictionary,
    .default = "Inconnu"
  )

}


# Sexe
get_sexe_label <- function(x) {

  dplyr::case_when(

    x == "1" ~ "M",

    x == "2" ~ "F",

    TRUE ~ NA_character_

  )

}

# Sauvegarder un graphique (PDF + PNG)

save_plot <- function(
    plot,
    file_name,
    width = 12,
    height = 8
) {

  # PDF (vectoriel)
  ggsave(
    filename = here::here(
      "outputs",
      "figures",
      paste0(file_name, ".pdf")
    ),
    plot = plot,
    width = width,
    height = height,
    units = "in"
  )

  # PNG haute résolution
  ggsave(
    filename = here::here(
      "outputs",
      "figures",
      paste0(file_name, ".png")
    ),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 600,
    bg = "white"
  )

}
