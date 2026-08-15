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

# ======================================================================
# Secteurs psychiatriques du Val-de-Marne
# ======================================================================

dictionnaire_secteurs <- tibble::tribble(

  ~code_postal, ~departement, ~secteur_f, ~hopital_secteur,

  # 94G01 --------------------------------------------------------------
  "94120", "94", "94G01", "CH les Murets",
  "94300", "94", "94G01", "CH les Murets",

  # 94G02 --------------------------------------------------------------
  "94360", "94", "94G02", "CH les Murets",
  "94170", "94", "94G02", "CH les Murets",
  "94130", "94", "94G02", "CH les Murets",

  # 94G03 --------------------------------------------------------------
  "94500", "94", "94G03", "CH les Murets",

  # 94G04 --------------------------------------------------------------
  "94430", "94", "94G04", "CH les Murets",
  "94510", "94", "94G04", "CH les Murets",
  "94420", "94", "94G04", "CH les Murets",
  "94880", "94", "94G04", "CH les Murets",
  "94490", "94", "94G04", "CH les Murets",
  "94350", "94", "94G04", "CH les Murets",

  # 94G05 --------------------------------------------------------------
  "94340", "94", "94G05", "CH les Murets",
  "94100", "94", "94G05", "CH les Murets",
  "94210", "94", "94G05", "CH les Murets",

  # 94G06 --------------------------------------------------------------
  "94700", "94", "94G06", "CHU Albert Chenevier",

  # 94G07 --------------------------------------------------------------
  "94000", "94", "94G07", "CHU Albert Chenevier",
  "94010", "94", "94G07", "CHU Albert Chenevier",

  # 94G08 --------------------------------------------------------------
  "94470", "94", "94G08", "CHU Albert Chenevier",
  "94380", "94", "94G08", "CHU Albert Chenevier",
  "94440", "94", "94G08", "CHU Albert Chenevier",
  "94370", "94", "94G08", "CHU Albert Chenevier",

  # 94G09 --------------------------------------------------------------
  "94450", "94", "94G09", "CHI Villeneuve-Saint-Georges",
  "94520", "94", "94G09", "CHI Villeneuve-Saint-Georges",
  "94460", "94", "94G09", "CHI Villeneuve-Saint-Georges",
  "94190", "94", "94G09", "CHI Villeneuve-Saint-Georges",

  # 94G10 --------------------------------------------------------------
  "94480", "94", "94G10", "GH Paul Guiraud",
  "94600", "94", "94G10", "GH Paul Guiraud",
  "94310", "94", "94G10", "GH Paul Guiraud",
  "94290", "94", "94G10", "GH Paul Guiraud",

  # 94G11 --------------------------------------------------------------
  "94400", "94", "94G11", "GH Paul Guiraud",

  # 94G12 --------------------------------------------------------------
  "94200", "94", "94G12", "Hôpital Paul Brousse",

  # 94G13 --------------------------------------------------------------
  "94240", "94", "94G13", "GH Paul Guiraud",
  "94800", "94", "94G13", "GH Paul Guiraud",

  # 94G15 --------------------------------------------------------------
  "94110", "94", "94G15", "GH Paul Guiraud",
  "94230", "94", "94G15", "GH Paul Guiraud",
  "94250", "94", "94G15", "GH Paul Guiraud",
  "94270", "94", "94G15", "GH Paul Guiraud",

  # 94G16 --------------------------------------------------------------
  "94140", "94", "94G16", "Hôpitaux de Saint-Maurice",
  "94220", "94", "94G16", "Hôpitaux de Saint-Maurice",
  "94160", "94", "94G16", "Hôpitaux de Saint-Maurice",
  "94410", "94", "94G16", "Hôpitaux de Saint-Maurice",

  # 94G17 --------------------------------------------------------------
  "94550", "94", "94G17", "GH Paul Guiraud",
  "94260", "94", "94G17", "GH Paul Guiraud",
  "94150", "94", "94G17", "GH Paul Guiraud",
  "94320", "94", "94G17", "GH Paul Guiraud",

# ======================================================================
# Paris (75)
# ======================================================================

# 75G01-02 -------------------------------------------------------------
"75001", "75", "75G01-02", "Hôpitaux de Saint-Maurice",
"75002", "75", "75G01-02", "Hôpitaux de Saint-Maurice",
"75003", "75", "75G01-02", "Hôpitaux de Saint-Maurice",
"75004", "75", "75G01-02", "Hôpitaux de Saint-Maurice",

# 75G08-09 --------------------------------------------------------------
"75011", "75", "75G08-09", "Hôpitaux de Saint-Maurice",

# 75G10-11 --------------------------------------------------------------
"75012", "75", "75G10-11", "Hôpitaux de Saint-Maurice",

# ======================================================================
# Hauts-de-Seine (92)
# ======================================================================

# 92G13 --------------------------------------------------------------
"92380", "92", "92G13", "GH Paul Guiraud",
"92430", "92", "92G13", "GH Paul Guiraud",
"92210", "92", "92G13", "GH Paul Guiraud",
"92420", "92", "92G13", "GH Paul Guiraud",
"92410", "92", "92G13", "GH Paul Guiraud",
"92310", "92", "92G13", "GH Paul Guiraud",

# 92G16 --------------------------------------------------------------
"92370", "92", "92G16", "GH Paul Guiraud",
"92190", "92", "92G16", "GH Paul Guiraud",

# 92G17 --------------------------------------------------------------
"92140", "92", "92G17", "GH Paul Guiraud",
"92350", "92", "92G17", "GH Paul Guiraud",

# 92G18 --------------------------------------------------------------
"92240", "92", "92G18", "GH Paul Guiraud",
"92120", "92", "92G18", "GH Paul Guiraud",

# 92G19 --------------------------------------------------------------
"92220", "92", "92G19", "GH Paul Guiraud",
"92320", "92", "92G19", "GH Paul Guiraud",

# 92G29 --------------------------------------------------------------
"92100", "92", "92G29", "GH Paul Guiraud"
)

# ======================================================================
# Dictionnaire ICD-10 selon Gentil
# Codes au format utilisé dans la base : lettre + 3 chiffres
# ======================================================================

library(tidyverse)


# ======================================================================
# 1. Dictionnaire principal
# ======================================================================

dictionnaire_icd10 <- tribble(

  ~categorie, ~code,

  # --------------------------------------------------------------------
  # Schizophrénie et autres troubles psychotiques
  # --------------------------------------------------------------------

  "Troubles schizophréniques et autres troubles psychotiques", "F200",
  "Troubles schizophréniques et autres troubles psychotiques", "F210",
  "Troubles schizophréniques et autres troubles psychotiques", "F220",
  "Troubles schizophréniques et autres troubles psychotiques", "F230",
  "Troubles schizophréniques et autres troubles psychotiques", "F240",
  "Troubles schizophréniques et autres troubles psychotiques", "F250",
  "Troubles schizophréniques et autres troubles psychotiques", "F280",
  "Troubles schizophréniques et autres troubles psychotiques", "F290",
  "Troubles schizophréniques et autres troubles psychotiques", "F323",
  "Troubles schizophréniques et autres troubles psychotiques", "F333",
  "Troubles schizophréniques et autres troubles psychotiques", "F448",

  # --------------------------------------------------------------------
  # Troubles bipolaires
  # --------------------------------------------------------------------

  "Troubles bipolaires", "F300",
  "Troubles bipolaires", "F301",
  "Troubles bipolaires", "F302",
  "Troubles bipolaires", "F308",
  "Troubles bipolaires", "F309",

  "Troubles bipolaires", "F310",
  "Troubles bipolaires", "F311",
  "Troubles bipolaires", "F312",
  "Troubles bipolaires", "F313",
  "Troubles bipolaires", "F314",
  "Troubles bipolaires", "F315",
  "Troubles bipolaires", "F316",
  "Troubles bipolaires", "F317",
  "Troubles bipolaires", "F318",
  "Troubles bipolaires", "F319",

  # --------------------------------------------------------------------
  # Troubles dépressifs
  # --------------------------------------------------------------------

  "Troubles dépressifs", "F320",
  "Troubles dépressifs", "F321",
  "Troubles dépressifs", "F322",
  "Troubles dépressifs", "F323",
  "Troubles dépressifs", "F328",
  "Troubles dépressifs", "F329",

  "Troubles dépressifs", "F330",
  "Troubles dépressifs", "F331",
  "Troubles dépressifs", "F332",
  "Troubles dépressifs", "F333",
  "Troubles dépressifs", "F338",
  "Troubles dépressifs", "F339",

  "Troubles dépressifs", "F348",
  "Troubles dépressifs", "F349",
  "Troubles dépressifs", "F380",
  "Troubles dépressifs", "F381",
  "Troubles dépressifs", "F388",
  "Troubles dépressifs", "F390",
  "Troubles dépressifs", "F412",

  # --------------------------------------------------------------------
  # Troubles anxieux
  #
  # Les troubles de l'adaptation sont volontairement intégrés ici.
  # --------------------------------------------------------------------

  "Troubles anxieux", "F400",
  "Troubles anxieux", "F410",
  "Troubles anxieux", "F420",
  "Troubles anxieux", "F430",
  "Troubles anxieux", "F440",
  "Troubles anxieux", "F450",
  "Troubles anxieux", "F460",
  "Troubles anxieux", "F470",
  "Troubles anxieux", "F480",

  "Troubles anxieux", "F680",

  # --------------------------------------------------------------------
  # Troubles de la personnalité
  # --------------------------------------------------------------------

  "Troubles de la personnalité", "F600",
  "Troubles de la personnalité", "F070",
  "Troubles de la personnalité", "F340",
  "Troubles de la personnalité", "F341",
  "Troubles de la personnalité", "F488",
  "Troubles de la personnalité", "F610",

  # --------------------------------------------------------------------
  # Troubles liés à l'alcool
  # --------------------------------------------------------------------

  "Troubles liés à l'alcool", "F100",
  "Troubles liés à l'alcool", "F101",
  "Troubles liés à l'alcool", "F102",
  "Troubles liés à l'alcool", "F103",
  "Troubles liés à l'alcool", "F104",
  "Troubles liés à l'alcool", "F105",
  "Troubles liés à l'alcool", "F106",
  "Troubles liés à l'alcool", "F107",
  "Troubles liés à l'alcool", "F108",
  "Troubles liés à l'alcool", "F109",

  "Troubles liés à l'alcool", "K700",
  "Troubles liés à l'alcool", "K701",
  "Troubles liés à l'alcool", "K702",
  "Troubles liés à l'alcool", "K703",
  "Troubles liés à l'alcool", "K704",
  "Troubles liés à l'alcool", "K709",

  "Troubles liés à l'alcool", "G621",
  "Troubles liés à l'alcool", "I426",
  "Troubles liés à l'alcool", "K292",
  "Troubles liés à l'alcool", "K852",
  "Troubles liés à l'alcool", "K860",
  "Troubles liés à l'alcool", "E244",
  "Troubles liés à l'alcool", "G312",
  "Troubles liés à l'alcool", "G721",
  "Troubles liés à l'alcool", "O354",

  "Troubles liés à l'alcool", "T510",
  "Troubles liés à l'alcool", "T511",
  "Troubles liés à l'alcool", "T518",
  "Troubles liés à l'alcool", "T519",

  # --------------------------------------------------------------------
  # Troubles liés aux drogues
  # --------------------------------------------------------------------

  "Troubles liés aux drogues", "F110",
  "Troubles liés aux drogues", "F111",
  "Troubles liés aux drogues", "F112",
  "Troubles liés aux drogues", "F113",
  "Troubles liés aux drogues", "F114",
  "Troubles liés aux drogues", "F115",
  "Troubles liés aux drogues", "F116",
  "Troubles liés aux drogues", "F117",
  "Troubles liés aux drogues", "F118",
  "Troubles liés aux drogues", "F119",

  "Troubles liés aux drogues", "F120",
  "Troubles liés aux drogues", "F121",
  "Troubles liés aux drogues", "F122",
  "Troubles liés aux drogues", "F123",
  "Troubles liés aux drogues", "F124",
  "Troubles liés aux drogues", "F125",
  "Troubles liés aux drogues", "F126",
  "Troubles liés aux drogues", "F127",
  "Troubles liés aux drogues", "F128",
  "Troubles liés aux drogues", "F129",

  "Troubles liés aux drogues", "F130",
  "Troubles liés aux drogues", "F131",
  "Troubles liés aux drogues", "F132",
  "Troubles liés aux drogues", "F133",
  "Troubles liés aux drogues", "F134",
  "Troubles liés aux drogues", "F135",
  "Troubles liés aux drogues", "F136",
  "Troubles liés aux drogues", "F137",
  "Troubles liés aux drogues", "F138",
  "Troubles liés aux drogues", "F139",

  "Troubles liés aux drogues", "F140",
  "Troubles liés aux drogues", "F141",
  "Troubles liés aux drogues", "F142",
  "Troubles liés aux drogues", "F143",
  "Troubles liés aux drogues", "F144",
  "Troubles liés aux drogues", "F145",
  "Troubles liés aux drogues", "F146",
  "Troubles liés aux drogues", "F147",
  "Troubles liés aux drogues", "F148",
  "Troubles liés aux drogues", "F149",

  "Troubles liés aux drogues", "F150",
  "Troubles liés aux drogues", "F151",
  "Troubles liés aux drogues", "F152",
  "Troubles liés aux drogues", "F153",
  "Troubles liés aux drogues", "F154",
  "Troubles liés aux drogues", "F155",
  "Troubles liés aux drogues", "F156",
  "Troubles liés aux drogues", "F157",
  "Troubles liés aux drogues", "F158",
  "Troubles liés aux drogues", "F159",

  "Troubles liés aux drogues", "F160",
  "Troubles liés aux drogues", "F161",
  "Troubles liés aux drogues", "F162",
  "Troubles liés aux drogues", "F163",
  "Troubles liés aux drogues", "F164",
  "Troubles liés aux drogues", "F165",
  "Troubles liés aux drogues", "F166",
  "Troubles liés aux drogues", "F167",
  "Troubles liés aux drogues", "F168",
  "Troubles liés aux drogues", "F169",

  "Troubles liés aux drogues", "F180",
  "Troubles liés aux drogues", "F181",
  "Troubles liés aux drogues", "F182",
  "Troubles liés aux drogues", "F183",
  "Troubles liés aux drogues", "F184",
  "Troubles liés aux drogues", "F185",
  "Troubles liés aux drogues", "F186",
  "Troubles liés aux drogues", "F187",
  "Troubles liés aux drogues", "F188",
  "Troubles liés aux drogues", "F189",

  "Troubles liés aux drogues", "F190",
  "Troubles liés aux drogues", "F191",
  "Troubles liés aux drogues", "F192",
  "Troubles liés aux drogues", "F193",
  "Troubles liés aux drogues", "F194",
  "Troubles liés aux drogues", "F195",
  "Troubles liés aux drogues", "F196",
  "Troubles liés aux drogues", "F197",
  "Troubles liés aux drogues", "F198",
  "Troubles liés aux drogues", "F199",

  "Troubles liés aux drogues", "F550",

  "Troubles liés aux drogues", "T400",
  "Troubles liés aux drogues", "T401",
  "Troubles liés aux drogues", "T402",
  "Troubles liés aux drogues", "T403",
  "Troubles liés aux drogues", "T404",
  "Troubles liés aux drogues", "T405",
  "Troubles liés aux drogues", "T406",
  "Troubles liés aux drogues", "T407",
  "Troubles liés aux drogues", "T408",
  "Troubles liés aux drogues", "T409",

  "Troubles liés aux drogues", "T423",
  "Troubles liés aux drogues", "T424",
  "Troubles liés aux drogues", "T426",
  "Troubles liés aux drogues", "T427",
  "Troubles liés aux drogues", "T435",
  "Troubles liés aux drogues", "T436",
  "Troubles liés aux drogues", "T438",
  "Troubles liés aux drogues", "T439",
  "Troubles liés aux drogues", "T509",
  "Troubles liés aux drogues", "T528",
  "Troubles liés aux drogues", "T529"
)


# ======================================================================
# 2. Catégorie combinée : troubles de l'usage de substances
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
# 4. Vérification rapide
# ======================================================================

dictionnaire_icd10 |>
  count(categorie)