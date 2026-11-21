#' @title Summarise the DataIDs of the TRY trait records
#' @description
#' Count the TRY trait records per trait and DataID, for reviewing which
#' kinds of record each selected trait holds and which DataIDs to exclude.
#' @param data_trait_rows
#' A data frame of TRY trait records with the columns `TraitID`, `DataID`
#' and `DataName`, such as `data_trait_rows` of [load_try_records()].
#' @return
#' A tibble with one row per combination of trait, DataID and DataName and
#' the columns `trait_id`, `data_id`, `data_name` and `n_records` (integer),
#' sorted by `trait_id`, then `data_id`, then `data_name`.
#' @details
#' Missing values form their own group. The input is not changed.
#' @export
summarise_try_data_ids <- function(data_trait_rows) {
  assertthat::assert_that(
    base::is.data.frame(data_trait_rows),
    msg = "'data_trait_rows' must be a data frame."
  )

  vec_columns_missing <-
    base::setdiff(
      c("TraitID", "DataID", "DataName"),
      base::names(data_trait_rows)
    )

  assertthat::assert_that(
    base::length(vec_columns_missing) == 0L,
    msg = stringr::str_c(
      "'data_trait_rows' lacks the columns: ",
      stringr::str_c(vec_columns_missing, collapse = ", "),
      "."
    )
  )

  res <-
    data_trait_rows |>
    tibble::as_tibble() |>
    dplyr::count(
      trait_id = .data$TraitID,
      data_id = .data$DataID,
      data_name = .data$DataName,
      name = "n_records"
    ) |>
    dplyr::arrange(.data$trait_id, .data$data_id, .data$data_name)

  return(res)
}
