mls_dictionary <- c(

  "1" = "SL",
  "3" = "SPDRE",
  "4" = "PENAL",
  "5" = "OPP",
  "6" = "DETENUS",
  "7" = "SPDT",
  "8" = "SPPI"

)

cim10_dictionary <- tibble::tribble(
  ~classe_cim10, ~libelle,

  "F0", "Troubles mentaux organiques",
  "F1", "Troubles liés aux substances",
  "F2", "Schizophrénie et troubles délirants",
  "F3", "Troubles de l'humeur",
  "F4", "Troubles anxieux",
  "F5", "Troubles du comportement alimentaire",
  "F6", "Troubles de la personnalité",
  "F7", "Déficience intellectuelle",
  "F8", "Troubles du développement",
  "F9", "Troubles du comportement de l'enfant"
)
