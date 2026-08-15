# Installation des packages nécessaires si absent
install.packages(c("xml2", "dplyr", "purrr"))

library(xml2)
library(dplyr)
library(purrr)

# 1. Chargement du fichier XML de l'ATIH (à adapter avec le chemin exact)
fichier_xml <- "C:/Recherche/Thèse/03_Code/FU-SAU/data/cim/cim10_fr_2025.xml"
cim_xml <- read_xml(fichier_xml)

# 2. Extraction de tous les noeuds 'Class' qui contiennent les codes
classes <- xml_find_all(cim_xml, ".//Class")

# 3. Construction du dictionnaire de codage
dictionnaire_cim10 <- tibble(
  # Récupération de l'attribut 'code' (ex: F321)
  diag_t = xml_attr(classes, "code"),
  
  # Récupération du texte dans la balise Label, sous la Rubric 'preferred'
  libelle = map_chr(classes, ~ {
    noeud_label <- xml_find_first(.x, './/Rubric[@kind="preferred"]/Label')
    xml_text(noeud_label)
  })
) %>%
  # Suppression des éventuelles lignes sans code
  filter(!is.na(diag_t))

# 4. Sauvegarde au format RDS (optimisé pour R) ou CSV
saveRDS(dictionnaire_cim10, "dictionnaire_cim10.rds")
# write_csv(dictionnaire_cim10, "dictionnaire_cim10.csv")