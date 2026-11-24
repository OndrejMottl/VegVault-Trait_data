#' @title Summarise raw trait records per trait
#' @description
#' Count the raw records of one provider per trait, the base of the
#' reconciliation of published, quarantined and dropped records.
#' @param data_source
#' A data frame of raw trait records of one provider.
#' @param col_trait_id
#' Character scalar. Column of `data_source` that identifies the trait, for
#' example `"source_trait_id"` (TRY) or `"trait_name"` (BIEN).
#' @param source_provider
#' Character scalar. The provider, for example `"TRY"`.
#' @return
#' A tibble with one row per trait and the columns `source_trait_id` (the
#' values of `col_trait_id`), `n_raw` (integer) and `source_provider`,
#' sorted by `source_trait_id`.
#' @details
#' Every row counts, including records that later rules drop. A missing
#' trait identifier forms its own row.
#' @seealso [validate_trait_unit_reconciliation()]
#' @export
summarise_trait_raw_counts <- function(
  data_source,
  col_trait_id,
  source_provider
) {
  assertthat::assert_that(
    base::is.data.frame(data_source),
    msg = "'data_source' must be a data frame."
  )

  assertthat::assert_that(
    base::is.character(col_trait_id),
    base::length(col_trait_id) == 1L,
    col_trait_id %in% base::names(data_source),
    msg = "'col_trait_id' must name one column of 'data_source'."
  )

  assertthat::assert_that(
    base::is.character(source_provider),
    base::length(source_provider) == 1L,
    !base::is.na(source_provider),
    msg = "'source_provider' must be one character string."
  )

  res <-
    data_source |>
    dplyr::count(
      source_trait_id = .data[[col_trait_id]],
      name = "n_raw"
    ) |>
    dplyr::mutate(source_provider = source_provider)

  return(res)
}
