# ======================================================================
# FU-SAU
# Dictionnaire ICD-10 selon Gentil
# ======================================================================
#
# Codage utilisé dans la base :
#   1 lettre + 3 chiffres
#
# Exemples :
#   F20   -> F200
#   F31.0 -> F310
#   F31.9 -> F319
#
# Les troubles de l'adaptation sont intégrés aux troubles anxieux.
#
# ======================================================================


library(tidyverse)


# ======================================================================
# Fonction de création des codes
# ======================================================================

codes <- function(prefix, chiffres) {
  
  paste0(
    prefix,
    chiffres
  )
  
}


# ======================================================================
# 1. Dictionnaire Gentil
# ======================================================================

dictionnaire_icd10 <- tribble(

  ~categorie, ~code,


  # ====================================================================
  # Schizophrénie et autres troubles psychotiques
  # ====================================================================

  "Troubles schizophréniques et autres troubles psychotiques",
  "F200",

  "Troubles schizophréniques et autres troubles psychotiques",
  "F210",

  "Troubles schizophréniques et autres troubles psychotiques",
  "F220",

  "Troubles schizophréniques et autres troubles psychotiques",
  "F230",

  "Troubles schizophréniques et autres troubles psychotiques",
  "F240",

  "Troubles schizophréniques et autres troubles psychotiques",
  "F250",

  "Troubles schizophréniques et autres troubles psychotiques",
  "F280",

  "Troubles schizophréniques et autres troubles psychotiques",
  "F290",

  "Troubles schizophréniques et autres troubles psychotiques",
  "F323",

  "Troubles schizophréniques et autres troubles psychotiques",
  "F333",

  "Troubles schizophréniques et autres troubles psychotiques",
  "F448",


  # ====================================================================
  # Troubles bipolaires
  # ====================================================================

  "Troubles bipolaires",
  "F300",

  "Troubles bipolaires",
  "F301",

  "Troubles bipolaires",
  "F302",

  "Troubles bipolaires",
  "F308",

  "Troubles bipolaires",
  "F309",

  "Troubles bipolaires",
  "F310",

  "Troubles bipolaires",
  "F311",

  "Troubles bipolaires",
  "F312",

  "Troubles bipolaires",
  "F313",

  "Troubles bipolaires",
  "F314",

  "Troubles bipolaires",
  "F315",

  "Troubles bipolaires",
  "F316",

  "Troubles bipolaires",
  "F317",

  "Troubles bipolaires",
  "F318",

  "Troubles bipolaires",
  "F319",


  # ====================================================================
  # Troubles dépressifs
  # ====================================================================

  "Troubles dépressifs",
  "F320",

  "Troubles dépressifs",
  "F321",

  "Troubles dépressifs",
  "F322",

  "Troubles dépressifs",
  "F323",

  "Troubles dépressifs",
  "F328",

  "Troubles dépressifs",
  "F329",

  "Troubles dépressifs",
  "F330",

  "Troubles dépressifs",
  "F331",

  "Troubles dépressifs",
  "F332",

  "Troubles dépressifs",
  "F333",

  "Troubles dépressifs",
  "F338",

  "Troubles dépressifs",
  "F339",

  "Troubles dépressifs",
  "F348",

  "Troubles dépressifs",
  "F349",

  "Troubles dépressifs",
  "F380",

  "Troubles dépressifs",
  "F381",

  "Troubles dépressifs",
  "F388",

  "Troubles dépressifs",
  "F390",

  "Troubles dépressifs",
  "F412",


  # ====================================================================
  # Troubles anxieux
  #
  # Gentil : F40-F48 et F68
  #
  # Les troubles de l'adaptation sont inclus ici :
  # F43.0, F43.1, F43.2, F43.8, F43.9
  # ====================================================================

  "Troubles anxieux",
  "F400",

  "Troubles anxieux",
  "F410",

  "Troubles anxieux",
  "F420",

  "Troubles anxieux",
  "F430",

  "Troubles anxieux",
  "F440",

  "Troubles anxieux",
  "F450",

  "Troubles anxieux",
  "F460",

  "Troubles anxieux",
  "F470",

  "Troubles anxieux",
  "F480",

  "Troubles anxieux",
  "F680",


  # ====================================================================
  # Troubles de la personnalité
  # ====================================================================

  "Troubles de la personnalité",
  "F600",

  "Troubles de la personnalité",
  "F070",

  "Troubles de la personnalité",
  "F340",

  "Troubles de la personnalité",
  "F341",

  "Troubles de la personnalité",
  "F488",

  "Troubles de la personnalité",
  "F610",

  "Troubles de la personnalité",
  "F69",



  # ====================================================================
  # Troubles liés à l'alcool
  # ====================================================================

  "Troubles liés à l'alcool",
  "F100",

  "Troubles liés à l'alcool",
  "F101",

  "Troubles liés à l'alcool",
  "F102",

  "Troubles liés à l'alcool",
  "F103",

  "Troubles liés à l'alcool",
  "F104",

  "Troubles liés à l'alcool",
  "F105",

  "Troubles liés à l'alcool",
  "F106",

  "Troubles liés à l'alcool",
  "F107",

  "Troubles liés à l'alcool",
  "F108",

  "Troubles liés à l'alcool",
  "F109",

  "Troubles liés à l'alcool",
  "K700",

  "Troubles liés à l'alcool",
  "K701",

  "Troubles liés à l'alcool",
  "K702",

  "Troubles liés à l'alcool",
  "K703",

  "Troubles liés à l'alcool",
  "K704",

  "Troubles liés à l'alcool",
  "K709",

  "Troubles liés à l'alcool",
  "G621",

  "Troubles liés à l'alcool",
  "I426",

  "Troubles liés à l'alcool",
  "K292",

  "Troubles liés à l'alcool",
  "K852",

  "Troubles liés à l'alcool",
  "K860",

  "Troubles liés à l'alcool",
  "E244",

  "Troubles liés à l'alcool",
  "G312",

  "Troubles liés à l'alcool",
  "G721",

  "Troubles liés à l'alcool",
  "O354",

  "Troubles liés à l'alcool",
  "T510",

  "Troubles liés à l'alcool",
  "T511",

  "Troubles liés à l'alcool",
  "T518",

  "Troubles liés à l'alcool",
  "T519",


  # ====================================================================
  # Troubles liés aux drogues
  # ====================================================================

  "Troubles liés aux drogues",
  "F110",

  "Troubles liés aux drogues",
  "F111",

  "Troubles liés aux drogues",
  "F112",

  "Troubles liés aux drogues",
  "F113",

  "Troubles liés aux drogues",
  "F114",

  "Troubles liés aux drogues",
  "F115",

  "Troubles liés aux drogues",
  "F116",

  "Troubles liés aux drogues",
  "F117",

  "Troubles liés aux drogues",
  "F118",

  "Troubles liés aux drogues",
  "F119",

  "Troubles liés aux drogues",
  "F120",

  "Troubles liés aux drogues",
  "F121",

  "Troubles liés aux drogues",
  "F122",

  "Troubles liés aux drogues",
  "F123",

  "Troubles liés aux drogues",
  "F124",

  "Troubles liés aux drogues",
  "F125",

  "Troubles liés aux drogues",
  "F126",

  "Troubles liés aux drogues",
  "F127",

  "Troubles liés aux drogues",
  "F128",

  "Troubles liés aux drogues",
  "F129",

  "Troubles liés aux drogues",
  "F130",

  "Troubles liés aux drogues",
  "F131",

  "Troubles liés aux drogues",
  "F132",

  "Troubles liés aux drogues",
  "F133",

  "Troubles liés aux drogues",
  "F134",

  "Troubles liés aux drogues",
  "F135",

  "Troubles liés aux drogues",
  "F136",

  "Troubles liés aux drogues",
  "F137",

  "Troubles liés aux drogues",
  "F138",

  "Troubles liés aux drogues",
  "F139",

  "Troubles liés aux drogues",
  "F140",

  "Troubles liés aux drogues",
  "F141",

  "Troubles liés aux drogues",
  "F142",

  "Troubles liés aux drogues",
  "F143",

  "Troubles liés aux drogues",
  "F144",

  "Troubles liés aux drogues",
  "F145",

  "Troubles liés aux drogues",
  "F146",

  "Troubles liés aux drogues",
  "F147",

  "Troubles liés aux drogues",
  "F148",

  "Troubles liés aux drogues",
  "F149",

  "Troubles liés aux drogues",
  "F150",

  "Troubles liés aux drogues",
  "F151",

  "Troubles liés aux drogues",
  "F152",

  "Troubles liés aux drogues",
  "F153",

  "Troubles liés aux drogues",
  "F154",

  "Troubles liés aux drogues",
  "F155",

  "Troubles liés aux drogues",
  "F156",

  "Troubles liés aux drogues",
  "F157",

  "Troubles liés aux drogues",
  "F158",

  "Troubles liés aux drogues",
  "F159",

  "Troubles liés aux drogues",
  "F160",

  "Troubles liés aux drogues",
  "F161",

  "Troubles liés aux drogues",
  "F162",

  "Troubles liés aux drogues",
  "F163",

  "Troubles liés aux drogues",
  "F164",

  "Troubles liés aux drogues",
  "F165",

  "Troubles liés aux drogues",
  "F166",

  "Troubles liés aux drogues",
  "F167",

  "Troubles liés aux drogues",
  "F168",

  "Troubles liés aux drogues",
  "F169",

  "Troubles liés aux drogues",
  "F180",

  "Troubles liés aux drogues",
  "F181",

  "Troubles liés aux drogues",
  "F182",

  "Troubles liés aux drogues",
  "F183",

  "Troubles liés aux drogues",
  "F184",

  "Troubles liés aux drogues",
  "F185",

  "Troubles liés aux drogues",
  "F186",

  "Troubles liés aux drogues",
  "F187",

  "Troubles liés aux drogues",
  "F188",

  "Troubles liés aux drogues",
  "F189",

  "Troubles liés aux drogues",
  "F190",

  "Troubles liés aux drogues",
  "F191",

  "Troubles liés aux drogues",
  "F192",

  "Troubles liés aux drogues",
  "F193",

  "Troubles liés aux drogues",
  "F194",

  "Troubles liés aux drogues",
  "F195",

  "Troubles liés aux drogues",
  "F196",

  "Troubles liés aux drogues",
  "F197",

  "Troubles liés aux drogues",
  "F198",

  "Troubles liés aux drogues",
  "F199",

  "Troubles liés aux drogues",
  "F550",

  "Troubles liés aux drogues",
  "T400",

  "Troubles liés aux drogues",
  "T401",

  "Troubles liés aux drogues",
  "T402",

  "Troubles liés aux drogues",
  "T403",

  "Troubles liés aux drogues",
  "T404",

  "Troubles liés aux drogues",
  "T405",

  "Troubles liés aux drogues",
  "T406",

  "Troubles liés aux drogues",
  "T407",

  "Troubles liés aux drogues",
  "T408",

  "Troubles liés aux drogues",
  "T409",

  "Troubles liés aux drogues",
  "T423",

  "Troubles liés aux drogues",
  "T424",

  "Troubles liés aux drogues",
  "T426",

  "Troubles liés aux drogues",
  "T427",

  "Troubles liés aux drogues",
  "T435",

  "Troubles liés aux drogues",
  "T436",

  "Troubles liés aux drogues",
  "T438",

  "Troubles liés aux drogues",
  "T439",

  "Troubles liés aux drogues",
  "T509",

  "Troubles liés aux drogues",
  "T528",

  "Troubles liés aux drogues",
  "T529"

)


# ======================================================================
# 2. Catégorie combinée : Troubles de l'usage de substances
# ======================================================================

dictionnaire_usage_substances <-

  dictionnaire_icd10 |>

  filter(
    categorie %in% c(
      "Troubles liés à l'alcool",
      "Troubles liés aux drogues"
    )
  ) |>

  mutate(
    categorie = "Troubles de l'usage de substances"
  )


# ======================================================================
# 3. Dictionnaire final
# ======================================================================

dictionnaire_icd10 <-

  bind_rows(
    dictionnaire_icd10,
    dictionnaire_usage_substances
  ) |>

  distinct()


# ======================================================================
# 4. Vérifications
# ======================================================================

dictionnaire_icd10 |>

  count(categorie)