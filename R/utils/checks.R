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
