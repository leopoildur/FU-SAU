# ======================================================================
# dicocim10.R
# Dictionnaire centralisé des codes CIM-10
# Synthèse Armoon et al. / Gentil et al.
# ======================================================================

library(stringr)
library(dplyr)

#' Nettoyer un vecteur de codes CIM-10
clean_cim10_code <- function(codes) {
  codes |>
    as.character() |>
    str_to_upper() |>
    str_replace_all("[\\.\\s]", "")
}

#' Détecter les troubles et complications liés à l'alcool
is_alcool <- function(codes) {
  c_codes <- clean_cim10_code(codes)
  pattern <- "^(F10|K70|G621|I426|K292|K852|K860|E244|G312|G721|O354|T51)"
  str_detect(c_codes, pattern)
}

#' Détecter les troubles et intoxications liés au cannabis
is_cannabis <- function(codes) {
  c_codes <- clean_cim10_code(codes)
  pattern <- "^(F12|T407)"
  str_detect(c_codes, pattern)
}

#' Détecter les troubles et intoxications liés aux autres toxiques (hors alcool et cannabis)
is_autre_toxique <- function(codes) {
  c_codes <- clean_cim10_code(codes)
  pattern <- "^(F1[1345689]|F55|T40[0-689])"
  str_detect(c_codes, pattern)
}

#' Détecter TOUT trouble lié à l'usage de substance (Variable globale : Alcool + Cannabis + Autres)
is_addictif_global <- function(codes) {
  is_alcool(codes) | is_cannabis(codes) | is_autre_toxique(codes)
}

#' Détecter la suicidalité (idées, auto-lésions, tentatives de suicide)
is_suicidaire <- function(codes) {
  c_codes <- clean_cim10_code(codes)
  pattern <- "^(R458|X6[0-9]|X7[0-9]|X8[0-4]|Z915|T42[3467]|T43[5689]|T509|T52[89])"
  str_detect(c_codes, pattern)
}

#' Extraire la famille psychiatrique principale (Diagnostic dominant)
get_famille_psy <- function(codes) {
  c_codes <- clean_cim10_code(codes)
  
  case_when(
    is_addictif_global(c_codes) ~ "F1",
    str_starts(c_codes, "F0") ~ "F0",
    str_starts(c_codes, "F2") ~ "F2",
    str_starts(c_codes, "F3") ~ "F3",
    str_starts(c_codes, "F4") ~ "F4",
    str_starts(c_codes, "F5") ~ "F5",
    str_starts(c_codes, "F6") ~ "F6",
    str_detect(c_codes, "^(F7|F8|F9)") ~ "F7_F9",
    str_starts(c_codes, "F") ~ "Autres_F",
    TRUE ~ "Non_Psy"
  )
}