#' @title Summarise BIEN records dropped before the unit contract
#' @description
#' Count, per trait, the BIEN records without a species name, which are
#' removed before the unit contract is applied.
#' @param data_bien_raw
#' A data frame of raw BIEN records with the columns `trait_name` and
#' `scrubbed_species_binomial`.
#' @return
#' A tibble with one row per trait that has such records and the columns
#' `source_trait_id` (the trait name), `n_records` (integer),
#' `source_provider` (`"BIEN"`) and `disposition_reason`
#' (`"species_name_missing"`), sorted by `source_trait_id`.
#' @details
#' A species name is missing as defined by [is_bien_species_missing()].
#' @seealso [prepare_bien_contract_input()],
#'   [validate_trait_unit_reconciliation()]
#' @export
summarise_bien_dropped_counts <- function(data_bien_raw) {
  assertthat::assert_that(
    base::is.data.frame(data_bien_raw),
    msg = "'data_bien_raw' must be a data frame."
  )

  vec_columns_missing <-
    base::setdiff(
      c("trait_name", "scrubbed_species_binomial"),
      base::names(data_bien_raw)
    )

  assertthat::assert_that(
    base::length(vec_columns_missing) == 0L,
    msg = stringr::str_c(
      "'data_bien_raw' lacks the columns: ",
      stringr::str_c(vec_columns_missing, collapse = ", "),
      "."
    )
  )

  res <-
    data_bien_raw |>
    dplyr::filter(
      is_bien_species_missing(vec_species = .data$scrubbed_species_binomial)
    ) |>
    dplyr::count(source_trait_id = .data$trait_name, name = "n_records") |>
    dplyr::mutate(
      source_provider = "BIEN",
      disposition_reason = "species_name_missing"
    )

  return(res)
}
