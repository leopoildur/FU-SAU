# ======================================================================
# 5. Export des données pour jamovi
# ======================================================================

# Sélection stricte des variables du Tableau 1
data_jamovi <- tableau1_data |>
  select(
    # Identifiant & Groupe
    id_patient, FU3,
    
    # Sociodémographie
    age, age_cat, sexe, residence_region,
    
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

# Export au format SPSS (.sav) lisible par jamovi (conserve les types et niveaux)
if (!requireNamespace("haven", quietly = TRUE)) install.packages("haven")

haven::write_sav(
  data_jamovi, 
  here::here("data", "exports", "data_tableau1_jamovi.sav")
)