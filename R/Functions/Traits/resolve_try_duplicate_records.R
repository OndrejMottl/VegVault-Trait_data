#' @title Resolve TRY records flagged as duplicates
#' @description
#' Mark published TRY records that TRY flags as copies of another published
#' record as dropped, so each measurement is published once.
#' @param data_source
#' A data frame of TRY records after [resolve_trait_unit_contract()] (and
#' usually [resolve_repeated_try_values()]), with the columns
#' `disposition`, `disposition_reason`, `obs_data_id` (TRY record
#' identifier) and `orig_obs_data_id` (TRY's duplicate flag: the
#' identifier of the original record, missing for originals).
#' @return
#' `data_source` with the same rows and columns. Published flagged records
#' whose original is published get `disposition = "dropped"` and
#' `disposition_reason = "duplicate_of_try_record"`.
#' @details
#' TRY marks a record contributed again in another dataset with
#' `OrigObsDataID`, pointing to the original record. The original is kept.
#' A flagged record is kept when its original is not published
#' (quarantined, dropped or absent), so the measurement is not lost. TRY
#' points directly to originals; a flagged record whose original is itself
#' flagged (a chain or cycle) is an error, because keeping exactly one copy
#' would then depend on the order of evaluation.
#' @seealso [resolve_repeated_try_values()]
#' @export
resolve_try_duplicate_records <- function(data_source) {
  vec_required_columns <-
    c(
      "disposition",
      "disposition_reason",
      "obs_data_id",
      "orig_obs_data_id"
    )

  vec_missing_columns <-
    base::setdiff(vec_required_columns, base::names(data_source))

  assertthat::assert_that(
    base::is.data.frame(data_source),
    base::length(vec_missing_columns) == 0L,
    msg = stringr::str_c(
      "'data_source' must be a data frame with the columns: ",
      stringr::str_c(vec_required_columns, collapse = ", "),
      "."
    )
  )

  vec_flagged_ids <-
    data_source |>
    dplyr::filter(!base::is.na(.data$orig_obs_data_id)) |>
    dplyr::pull("obs_data_id")

  vec_flagged_targets <-
    data_source |>
    dplyr::filter(!base::is.na(.data$orig_obs_data_id)) |>
    dplyr::pull("orig_obs_data_id")

  if (
    base::any(vec_flagged_targets %in% vec_flagged_ids)
  ) {
    cli::cli_abort(
      c(
        "TRY duplicate flags form a chain or cycle.",
        "x" = stringr::str_c(
          "{base::sum(vec_flagged_targets %in% vec_flagged_ids)} flagged",
          " record{?s} point to another flagged record."
        )
      )
    )
  }

  vec_published_ids <-
    data_source |>
    dplyr::filter(.data$disposition == "published") |>
    dplyr::pull("obs_data_id")

  res <-
    data_source |>
    dplyr::mutate(
      flag_duplicate = .data$disposition == "published" &
        !base::is.na(.data$orig_obs_data_id) &
        .data$orig_obs_data_id %in% vec_published_ids,
      disposition = dplyr::if_else(
        .data$flag_duplicate,
        "dropped",
        .data$disposition
      ),
      disposition_reason = dplyr::if_else(
        .data$flag_duplicate,
        "duplicate_of_try_record",
        .data$disposition_reason
      )
    ) |>
    dplyr::select(
      dplyr::all_of(base::names(data_source))
    )

  return(res)
}
