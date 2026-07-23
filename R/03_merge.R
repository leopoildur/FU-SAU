# ======================================================================
# FU-SAU
# 03_merge.R
# Création de la cohorte analytique (niveau patient)
# ======================================================================

# Objectif :
#   - Construire la table analytique principale
#   - Unité statistique : le patient (1 ligne = 1 patient)
#   - Agréger les passages depuis PASS
#   - Récupérer les diagnostics du 1er et du dernier avis de chaque passage
#     depuis AVIS
#   - Calculer les variables de récurrence
#   - Définir les frequent attenders (>= 4 passages sur 12 mois glissants)
#
# Entrée :
#   data/processed/avis_clean.rds
#   data/processed/pass_clean.rds
#
# Sortie :
#   data/processed/cohort.rds
# ======================================================================


# Chargement du projet ==================================================

source(here::here("R", "utils", "load_project.R"))

avis <- readRDS(here::here("data", "processed", "avis_clean.rds"))
pass <- readRDS(here::here("data", "processed", "pass_clean.rds"))


# Structure générale ====================================================

dim(avis)
dim(pass)


# Contrôle des identifiants avant fusion ================================

## Passages dans AVIS sans correspondance dans PASS ---------------------

avis_sans_pass <- avis |>
  anti_join(pass, by = "id_passage")

nrow(avis_sans_pass)

## Passages dans PASS sans correspondance dans AVIS ---------------------

pass_sans_avis <- pass |>
  anti_join(avis, by = "id_passage")

nrow(pass_sans_avis)

## Nombre d'avis par passage dans AVIS ----------------------------------

avis |>
  count(id_passage, name = "n_avis_avis") |>
  arrange(desc(n_avis_avis))


# Diagnostics du 1er et du dernier avis par passage (depuis AVIS) =======
# Pour chaque passage, on récupère le diagnostic principal et les
# diagnostics associés du 1er avis et du dernier avis.
# Le tri par date_avis garantit l'ordre chronologique.

diag_par_passage <- avis |>
  arrange(id_passage, date_avis) |>
  group_by(id_passage) |>
  summarise(

    # Premier avis du passage
    diag_p_premier_avis   = first(diag_p),
    diag_p_2_premier_avis = first(diag_p_2),
    diag_a_premier_avis   = list(first(diag_a)),

    # Dernier avis du passage
    diag_p_dernier_avis   = last(diag_p),
    diag_p_2_dernier_avis = last(diag_p_2),
    diag_a_dernier_avis   = list(last(diag_a)),

    # Nombre réel d'avis dans AVIS pour ce passage
    n_avis_avis = n()

  ) |>
  ungroup()


# Enrichissement de PASS avec les diagnostics AVIS ======================
# Les passages sans correspondance dans AVIS conservent les diagnostics
# déjà présents dans PASS (1er avis).

pass_enrichi <- pass |>
  left_join(diag_par_passage, by = "id_passage")


# Agrégation au niveau patient ==========================================
# 1 ligne = 1 patient.

cohort <- pass_enrichi |>
  arrange(id_patient, date_arrivee) |>
  group_by(id_patient) |>
  summarise(

    # ------------------------------------------------------------------
    # Variables démographiques (prises au premier passage)
    # ------------------------------------------------------------------

    sexe                = first(sexe),
    age_premier_passage = first(age),
    code_postal         = first(code_postal),
    departement         = first(departement),

    # ------------------------------------------------------------------
    # Récurrence des passages
    # ------------------------------------------------------------------

    nb_passages          = n(),
    date_premier_passage = first(date_arrivee),
    date_dernier_passage = last(date_arrivee),

    duree_suivi_jours = as.numeric(
      difftime(last(date_arrivee), first(date_arrivee), units = "days")
    ),

    # ------------------------------------------------------------------
    # Diagnostics du premier passage (1er et dernier avis)
    # ------------------------------------------------------------------

    diag_p_premier_passage   = first(diag_p_premier_avis),
    diag_p_2_premier_passage = first(diag_p_2_premier_avis),
    diag_p_dernier_avis_premier_passage   = first(diag_p_dernier_avis),
    diag_p_2_dernier_avis_premier_passage = first(diag_p_2_dernier_avis),

    # ------------------------------------------------------------------
    # Diagnostics du dernier passage (1er et dernier avis)
    # ------------------------------------------------------------------

    diag_p_premier_avis_dernier_passage   = last(diag_p_premier_avis),
    diag_p_2_premier_avis_dernier_passage = last(diag_p_2_premier_avis),
    diag_p_dernier_passage   = last(diag_p_dernier_avis),
    diag_p_2_dernier_passage = last(diag_p_2_dernier_avis),

    # ------------------------------------------------------------------
    # Secteur et hôpital (premier passage)
    # ------------------------------------------------------------------

    secteur_f_premier       = first(secteur_f),
    hopital_secteur_premier = first(hopital_secteur),

    # ------------------------------------------------------------------
    # Hospitalisations
    # ------------------------------------------------------------------

    nb_passages_hospit = sum(type_sejour_f == "AVEC_HOSPIT", na.rm = TRUE),

    # ------------------------------------------------------------------
    # Destinations de sortie (tous passages)
    # ------------------------------------------------------------------

    nb_retour_domicile = sum(destination == "DOMICILE",      na.rm = TRUE),
    nb_transfert_psy   = sum(destination == "TRANSFERT_PSY", na.rm = TRUE),
    nb_transfert_mco   = sum(destination == "TRANSFERT_MCO", na.rm = TRUE),
    nb_non_admis       = sum(destination == "NON_ADMIS",     na.rm = TRUE),

    # ------------------------------------------------------------------
    # Mode légal de soins (premier passage)
    # ------------------------------------------------------------------

    mls_f_premier = first(mls_f),
    mls_g_premier = first(mls_g),

    # ------------------------------------------------------------------
    # Définition des frequent attenders
    # >= 4 passages sur une fenêtre glissante de 365 jours.
    # On compare la date du passage n avec la date du passage n-3.
    # ------------------------------------------------------------------

    frequent_attender = {
      dates <- date_arrivee
      n <- length(dates)
      if (n >= 4) {
        any(
          as.numeric(
            difftime(dates[4:n], dates[1:(n - 3)], units = "days")
          ) <= 365
        )
      } else {
        FALSE
      }
    }

  ) |>
  ungroup()


# Variables dérivées ====================================================

cohort <- cohort |>
  mutate(
    taux_hospit           = nb_passages_hospit / nb_passages,
    annee_premier_passage = lubridate::year(date_premier_passage)
  )


# Contrôle qualité ======================================================

dim(cohort)

cohort |>
  count(frequent_attender)

cohort |>
  summarise(
    across(everything(), ~ sum(is.na(.)))
  ) |>
  tidyr::pivot_longer(
    everything(),
    names_to  = "variable",
    values_to = "n_na"
  ) |>
  filter(n_na > 0) |>
  arrange(desc(n_na))

summary(cohort$nb_passages)

cohort |>
  count(nb_passages, sort = TRUE)


# Sauvegarde ============================================================

dir.create(
  here::here("data", "processed"),
  recursive    = TRUE,
  showWarnings = FALSE
)

saveRDS(
  cohort,
  here::here("data", "processed", "cohort.rds")
)


# Fin du script =========================================================

gitmessage("======================================================")
message(" Création de la cohorte patient terminée avec succès")
message("------------------------------------------------------")
message(" Nombre de patients (lignes) : ", nrow(cohort))
message(" Nombre de variables         : ", ncol(cohort))
message(" Frequent attenders          : ", sum(cohort$frequent_attender, na.rm = TRUE))
message(" Fichier sauvegardé          : data/processed/cohort.rds")
message("======================================================")
