#' @title Build a measurement key for cross-provider duplicates
#' @description
#' Combine trait domain, species name and value into one key, so that the
#' same measurement held by two providers gets the same key.
#' @param trait_domain
#' Character vector of VegVault trait domains.
#' @param species
#' Character vector of species names, the same length as `trait_domain`.
#' @param trait_value
#' Numeric vector of values in the unit-contract unit, the same length.
#' @return
#' A character vector `<domain>|<species>|<value>`, with the value written
#' to three significant digits (`sprintf("%.3g")`).
#' @details
#' Three significant digits absorb the rounding differences between
#' providers. Missing inputs give a missing key, which matches nothing.
#' @seealso [resolve_bien_try_duplicates()], [summarise_bien_try_twins()]
#' @export
build_trait_measurement_key <- function(
  trait_domain,
  species,
  trait_value
) {
  assertthat::assert_that(
    base::is.numeric(trait_value),
    msg = "'trait_value' must be numeric."
  )

  assertthat::assert_that(
    base::length(trait_domain) == base::length(trait_value),
    base::length(species) == base::length(trait_value),
    msg = stringr::str_c(
      "'trait_domain', 'species' and 'trait_value' must have the same",
      " length."
    )
  )

  vec_value_text <-
    dplyr::if_else(
      base::is.na(trait_value),
      NA_character_,
      base::sprintf("%.3g", trait_value)
    )

  res <-
    stringr::str_c(trait_domain, "|", species, "|", vec_value_text)

  return(res)
}
