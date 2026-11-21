#' @title Check whether BIEN records lack a species name
#' @description
#' Flag the BIEN records whose scrubbed species name is missing, so that
#' they can be dropped and counted before the unit contract.
#' @param vec_species
#' Character vector. BIEN scrubbed species names
#' (`scrubbed_species_binomial`).
#' @return
#' A logical vector of the same length as `vec_species`: `TRUE` where the
#' name is `NA`, empty or only white space, otherwise `FALSE`. It is never
#' `NA`.
#' @details
#' A record without a species name cannot be attached to a taxon in
#' VegVault.
#' @seealso [prepare_bien_contract_input()], [summarise_bien_dropped_counts()]
#' @export
is_bien_species_missing <- function(vec_species) {
  assertthat::assert_that(
    base::is.character(vec_species),
    msg = "'vec_species' must be a character vector."
  )

  res <-
    base::is.na(vec_species) |
    !base::nzchar(base::trimws(vec_species))

  return(res)
}
