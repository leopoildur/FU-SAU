# ======================================================================
# Contrôles qualité
# ======================================================================

check_duplicates <- function(data) {

  sum(duplicated(data))

}


check_id <- function(data, id) {

  data |>
    summarise(
      n = n(),
      uniques = n_distinct({{ id }})
    )

}

check_structure <- function(data) {

  cat("\n")
  print(dim(data))
  cat("\n")

  glimpse(data)

}

# Contrôle 02.1_cleaning_avis.R ======================================================

# Dimensions ------------------------------------------------------------
# Vérifier le nombre de lignes et de variables.

dim(avis)

glimpse(avis)


# Valeurs manquantes ----------------------------------------------------
# Nombre de valeurs manquantes par variable.

avis |>
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  ) |>
  tidyr::pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "n_na"
  ) |>
  arrange(
    desc(n_na)
  )


# Doublons --------------------------------------------------------------
# Vérifier les doublons exacts.

sum(
  duplicated(avis)
)

# Vérifier les doublons d'identifiants (même patient, même passage, même avis)

avis |>
  count(
    id_patient,
    id_passage
  ) |>
  filter(
    n > 1
  )


# Contrôle des identifiants ---------------------------------------------

# Nombre de patients

n_distinct(
  avis$id_patient
)

# Nombre de passages

n_distinct(
  avis$id_passage
)


# Cohérence des dates ---------------------------------------------------

# Sortie avant arrivée

avis |>
  filter(
    date_sortie < date_arrivee
  )

# Demande avant arrivée

avis |>
  filter(
    date_demande < date_arrivee
  )

# Avis avant arrivée

avis |>
  filter(
    date_avis < date_arrivee
  )

# Avis après sortie

avis |>
  filter(
    date_avis > date_sortie
  )


# Cohérence des délais --------------------------------------------------

summary(
  as.numeric(avis$delai_avis)
)

summary(
  as.numeric(avis$LOS)
)


# Cohérence des diagnostics ---------------------------------------------

# Diagnostic principal manquant

sum(
  is.na(avis$diag_p)
)

# Nombre de diagnostics

avis |>
  count(
    nb_diag_t,
    sort = TRUE
  )


# Cohérence des variables catégorielles --------------------------------

avis |>
  count(
    sexe
  )

avis |>
  count(
    mls_f
  )

avis |>
  count(
    mls_g
  )

avis |>
  count(
    secteur_f
  )

avis |>
  count(
    hopital_secteur
  )


# Résumé final ----------------------------------------------------------

skimr::skim(
  avis
)
