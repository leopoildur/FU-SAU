# ======================================================================
# FU-SAU
# 02.2 - Nettoyage de la table PASS
# ======================================================================

# Chargement ============================================================

source(here("R","utils","load_project.R"))

source(here("R", "01_import.R"))

# Structure générale ============================================================

dim(pass)

glimpse(pass)

skim(pass)

colSums(is.na(pass))

names(pass)

# Identifiants ============================================================

pass <- pass |>
  rename(

    id_patient = idpat,
    id_passage = i_ddos
  ) |>
  mutate(
    id_patient = as.character(id_patient),
    id_passage = as.character(id_passage)

  )

# Variables temporelles ============================================================

# Renommer les variables ================================================

pass <- pass |>
  rename(

    date_arrivee = debu_tt,
    date_sortie = fi_nt,
    date_demande = de_mt,
    date_premier_avis = avi_st_1,
    date_dernier_avis = avi_st_n

  )


# Conversion des dates ==================================================

pass <- pass |>
  mutate(

    date_arrivee = parse_datetime_fr(date_arrivee),
    date_sortie = parse_datetime_fr(date_sortie),
    date_demande = parse_datetime_fr(date_demande),
    date_premier_avis = parse_datetime_fr(date_premier_avis),
    date_dernier_avis = parse_datetime_fr(date_dernier_avis)

  )

# Variables dérivées ====================================================

pass <- pass |>
  mutate(

    # Heure
    heure_arrivee = get_hour(date_arrivee),
    heure_demande = get_hour(date_demande),
    heure_premier_avis = get_hour(date_avis),
    heure_dernier_avis = get_hour(date_dernier_avis),
    heure_sortie = get_hour(date_sortie),

    # Nuit
    nuit_arrivee = is_night(date_arrivee),
    nuit_demande = is_night(date_demande),
    nuit_premier_avis = is_night(date_avis),
    nuit_dernier_avis = is_night(date_dernier_avis),
    nuit_sortie = is_night(date_sortie),

    # Week-end
    weekend_arrivee = is_weekend(date_arrivee),
    weekend_demande = is_weekend(date_demande),
    weekend_premier_avis = is_weekend(date_avis),
    weekend_dernier_avis = is_weekend(date_dernier_avis),
    weekend_sortie = is_weekend(date_sortie),

    # Jour férié
    ferie = is_holiday(date_avis),

    # Garde
    garde =
      nuit_premier_avis |
      weekend_premier_avis |
      ferie,

    # Délais
    delai_premier_avis = date_premier_avis - date_arrivee,
    LOS = date_sortie - date_arrivee

  )


# Variables démographiques ============================================================


# Variables psychiatriques ============================================================


# Variables organisationnelles ============================================================


# Variables spécifiques à PASS ============================================================


# Contrôle qualité ============================================================


# Sauvegarde et export ============================================================

