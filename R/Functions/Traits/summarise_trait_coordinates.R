#' @title Summarise the coordinates of trait records
#' @description
#' Count, per provider and combination of coordinate sources, how many
#' trait records and datasets there are, and whether their coordinates are
#' kept.
#' @param data_source
#' A data frame of trait records with the columns `source_provider`,
#' `source_dataset_key`, `latitude`, `longitude`, `latitude_source` and
#' `longitude_source`, as written by [prepare_try_coordinates()] or
#' [prepare_bien_coordinates()].
#' @return
#' A tibble with one row per `source_provider`, `latitude_source`,
#' `longitude_source` and `coordinates_kept`, sorted by these four, and the
#' columns `source_provider`, `latitude_source`, `longitude_source`,
#' `coordinates_kept`, `n_records` and `n_datasets`.
#' @details
#' `coordinates_kept` is `TRUE` when both `latitude` and `longitude` of the
#' record are present. `n_datasets` counts the distinct values of
#' `source_dataset_key` in the group, a missing key counting as one value.
#' Missing sources form their own group; no record is left out, so
#' `n_records` sums to the number of rows of `data_source`. The table holds
#' counts only, so it can be published with the other aggregate tables.
#' @seealso [summarise_trait_reference_completeness()]
#' @export
summarise_trait_coordinates <- function(data_source) {
  vec_required_columns <-
    c(
      "source_provider",
      "source_dataset_key",
      "latitude",
      "longitude",
      "latitude_source",
      "longitude_source"
    )

  assertthat::assert_that(
    base::is.data.frame(data_source),
    base::all(vec_required_columns %in% base::names(data_source)),
    msg = stringr::str_c(
      "'data_source' must be a data frame with the columns: ",
      stringr::str_c(vec_required_columns, collapse = ", "),
      "."
    )
  )

  # A narrow table first, so a large input is not copied whole and other
  #   columns cannot interfere
  res <-
    data_source |>
    dplyr::select(dplyr::all_of(vec_required_columns)) |>
    dplyr::mutate(
      coordinates_kept = !base::is.na(.data$latitude) &
        !base::is.na(.data$longitude)
    ) |>
    dplyr::group_by(
      .data$source_provider,
      .data$latitude_source,
      .data$longitude_source,
      .data$coordinates_kept
    ) |>
    dplyr::summarise(
      n_records = dplyr::n(),
      n_datasets = dplyr::n_distinct(.data$source_dataset_key),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      .data$source_provider,
      .data$latitude_source,
      .data$longitude_source,
      .data$coordinates_kept
    )

  return(res)
}
