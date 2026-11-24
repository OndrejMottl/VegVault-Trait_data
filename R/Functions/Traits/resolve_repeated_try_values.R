#' @title Resolve repeated TRY values within one observation
#' @description
#' Keep one published value per TRY observation and trait, and mark the
#' other published values of that observation and trait as dropped.
#' @param data_source
#' A data frame returned by [resolve_trait_unit_contract()] for TRY records,
#' with the columns `source_provider`, `source_record_id` (the TRY
#' observation), `source_trait_id`, `disposition`, `disposition_reason`,
#' `value_kind_name`, `orig_obs_data_id` and `obs_data_id`.
#' @return
#' `data_source` with the same rows and columns. Published values that are
#' not kept get `disposition = "dropped"` and
#' `disposition_reason = "repeated_in_observation"`.
#' @details
#' A TRY observation can report one trait several times: replicates, or a
#' mean together with its minimum and maximum. VegVault stores one value per
#' observation and trait, so one published record is kept, chosen in this
#' order:
#' 1. Value kind: `"Single"` first; then central values (`"Mean"`,
#'    `"Best estimate"`, `"Median"`, `"Site specific mean"`,
#'    `"Species mean"`, `"Class mean"`); then a missing or empty kind; then
#'    every other kind, such as `"Maximum"`, `"Minimum"`, `"High"`, `"Low"`
#'    and percentiles.
#' 2. A record that TRY does not flag as a duplicate (`orig_obs_data_id`
#'    missing) before a flagged one.
#' 3. The lowest TRY record identifier (`obs_data_id`).
#'
#' Quarantined records are not changed and do not compete for selection.
#' @export
resolve_repeated_try_values <- function(data_source) {
  vec_required_columns <-
    c(
      "source_provider",
      "source_record_id",
      "source_trait_id",
      "disposition",
      "disposition_reason",
      "value_kind_name",
      "orig_obs_data_id",
      "obs_data_id"
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

  vec_central_kinds <-
    c(
      "Mean",
      "Best estimate",
      "Median",
      "Site specific mean",
      "Species mean",
      "Class mean"
    )

  data_ranked <-
    data_source |>
    dplyr::mutate(
      repeat_row_id = dplyr::row_number(),
      value_kind_rank = dplyr::case_when(
        .data$value_kind_name %in% "Single" ~ 1L,
        .data$value_kind_name %in% vec_central_kinds ~ 2L,
        base::is.na(.data$value_kind_name) |
          !base::nzchar(base::trimws(.data$value_kind_name)) ~ 3L,
        TRUE ~ 4L
      ),
      duplicate_rank = dplyr::if_else(
        base::is.na(.data$orig_obs_data_id),
        1L,
        2L
      )
    )

  vec_dropped_row_ids <-
    data_ranked |>
    dplyr::filter(.data$disposition == "published") |>
    dplyr::arrange(
      .data$value_kind_rank,
      .data$duplicate_rank,
      .data$obs_data_id
    ) |>
    dplyr::group_by(
      .data$source_provider,
      .data$source_record_id,
      .data$source_trait_id
    ) |>
    dplyr::filter(dplyr::row_number() > 1L) |>
    dplyr::ungroup() |>
    dplyr::pull("repeat_row_id")

  res <-
    data_ranked |>
    dplyr::mutate(
      flag_dropped = .data$repeat_row_id %in% vec_dropped_row_ids,
      disposition = dplyr::if_else(
        .data$flag_dropped,
        "dropped",
        .data$disposition
      ),
      disposition_reason = dplyr::if_else(
        .data$flag_dropped,
        "repeated_in_observation",
        .data$disposition_reason
      )
    ) |>
    dplyr::select(
      dplyr::all_of(base::names(data_source))
    )

  return(res)
}
