#' @title Summarise TRY records dropped before the unit contract
#' @description
#' Count, per trait and reason, the TRY trait records that are removed
#' before the unit contract is applied.
#' @param data_trait_rows
#' A data frame of TRY trait records with the columns `source_trait_id`,
#' `DataID` and `OrigValueStr`, such as `data_trait_rows` of
#' [load_try_records()].
#' @param vec_data_ids_excluded
#' Character vector. The excluded DataIDs. It may be empty.
#' @return
#' A tibble with one row per trait and reason and the columns
#' `source_trait_id`, `disposition_reason`, `n_records` (integer) and
#' `source_provider` (`"TRY"`), sorted by `source_trait_id`, then
#' `disposition_reason`. Traits without dropped records have no row.
#' @details
#' A record of an excluded DataID has the reason `"data_id_excluded"`.
#' Otherwise a record without an original value (`OrigValueStr` missing)
#' has the reason `"original_value_missing"`. Each record has at most one
#' reason, in this order. DataIDs are compared as text.
#' @seealso [prepare_try_contract_input()],
#'   [validate_trait_unit_reconciliation()]
#' @export
summarise_try_dropped_counts <- function(
  data_trait_rows,
  vec_data_ids_excluded
) {
  vec_columns_required <-
    c("source_trait_id", "DataID", "OrigValueStr")

  assertthat::assert_that(
    base::is.data.frame(data_trait_rows),
    base::all(vec_columns_required %in% base::names(data_trait_rows)),
    msg = stringr::str_c(
      "'data_trait_rows' must be a data frame with the columns: ",
      stringr::str_c(vec_columns_required, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    base::is.character(vec_data_ids_excluded),
    msg = "'vec_data_ids_excluded' must be a character vector."
  )

  res <-
    data_trait_rows |>
    dplyr::mutate(
      disposition_reason = dplyr::case_when(
        base::as.character(.data$DataID) %in% vec_data_ids_excluded ~
          "data_id_excluded",
        base::is.na(.data$OrigValueStr) ~ "original_value_missing",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!base::is.na(.data$disposition_reason)) |>
    dplyr::count(
      .data$source_trait_id,
      .data$disposition_reason,
      name = "n_records"
    ) |>
    dplyr::mutate(source_provider = "TRY")

  return(res)
}
