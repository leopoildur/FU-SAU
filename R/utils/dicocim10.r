#' Catégoriser les codes CIM-10 psychiatriques et toxicologiques
#' @param codes Vecteur contenant les codes CIM-10
#' @return Vecteur de catégories
categoriser_cim10 <- function(codes) {
  dplyr::case_when(
    stringr::str_starts(codes, "R458|X6[0-9]|X7[0-9]|X8[0-4]|Z915") ~ "Suicidalité",
    stringr::str_starts(codes, "F10|T51|X65") ~ "1_Alcool",
    stringr::str_starts(codes, "F11|F12|F13|F14|F15|F16|F18|F19|T40") ~ "1_Toxiques",
    stringr::str_starts(codes, "F2") ~ "2",
    stringr::str_starts(codes, "F3") ~ "3",
    stringr::str_starts(codes, "F4") ~ "4",
    stringr::str_starts(codes, "F6") ~ "6",
    TRUE ~ "Autre"
  )
}

#' Détecter l'usage de substance selon la catégorie
#' @param categories Vecteur de catégories généré par categoriser_cim10
#' @return "Oui" ou "Non"
detecter_usage_substance <- function(categories) {
  dplyr::if_else(categories %in% c("1_Alcool", "1_Toxiques"), "Oui", "Non")
}