# ======================================================================
# 5. Export des données pour jamovi
# ======================================================================

library(dplyr)
library(haven)

data_jamovi <- tableau1_data |>
  # 1. On supprime l'ancienne variable si elle était déjà là pour éviter le conflit .x et .y
  select(-any_of("hopital_secteur")) |>
  
  # 2. On colle la bonne variable venant de "cohort"
  left_join(
    cohort |> select(id_patient, hopital_secteur), 
    by = "id_patient"
  ) |>
  
  # 3. On fait notre sélection finale propre
  select(
    # Identifiant & Groupe
    id_patient, FU3,
    
    # Sociodémographie & Organisation
    age, age_cat, sexe, residence_region, hopital_secteur,
    
    # Diagnostics
    diag_dominant, has_diag_associe, diag_F6, diag_F1, 
    diag_F1_alcool_seul, diag_F1_toxiques, suicidalite_patient,
    
    # Recours aux urgences
    duree_soins_moyenne, duree_soins_moyenne_hospit, duree_soins_moyenne_non_hospit,
    au_moins_un_passage_garde, au_moins_un_passage_nuit, au_moins_un_passage_we,
    
    # Hospitalisations
    nb_hospitalisation, taux_hospitalisation, hospitalisation, 
    hospitalisation_sl, hospitalisation_ssc
  )

# Export au format SPSS (.sav)
haven::write_sav(
  data_jamovi, 
  here::here("data", "exports", "data_tableau1_jamovi.sav")
)

print("Succès : Le conflit de noms a été évité et le fichier est exporté !")