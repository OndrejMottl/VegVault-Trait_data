#' @title Prepare numeric TRY coordinates
#' @description
#' Turn the TRY latitude and longitude rows of each observation into one
#' row of numeric decimal degrees with a status per coordinate.
#' @param data_coordinate_rows
#' A data frame of TRY ancillary rows with the columns `dataset`,
#' `acc_species_name`, `observation_id`, `data_name`, `orig_value_str` and
#' `std_value`. Rows whose `data_name` is not `"Latitude"` or `"Longitude"`
#' are ignored.
#' @return
#' A tibble with one row per `dataset`, `acc_species_name` and
#' `observation_id`, and the columns `latitude`, `longitude` (numeric,
#' WGS84 decimal degrees, missing when unusable), `latitude_source` and
#' `longitude_source`.
#' @details
#' Each coordinate takes TRY's standardised value (`std_value`) when it is
#' finite and within range (latitude -90 to 90, longitude -180 to 180).
#' Otherwise the original value is used when it is a plain number within
#' range. The source column is `"standardized"`, `"original"`,
#' `"out_of_range"` or `"not_parsable"`. A location needs both coordinates:
#' when one is unusable or absent, both are set missing and the usable one
#' gets the source `"incomplete_pair"`. Two rows for the same coordinate of
#' one observation are an error.
#' @seealso [prepare_trait_numeric()]
#' @export
prepare_try_coordinates <- function(data_coordinate_rows) {
  vec_required_columns <-
    c(
      "dataset",
      "acc_species_name",
      "observation_id",
      "data_name",
      "orig_value_str",
      "std_value"
    )

  vec_missing_columns <-
    base::setdiff(vec_required_columns, base::names(data_coordinate_rows))

  assertthat::assert_that(
    base::is.data.frame(data_coordinate_rows),
    base::length(vec_missing_columns) == 0L,
    msg = stringr::str_c(
      "'data_coordinate_rows' must be a data frame with the columns: ",
      stringr::str_c(vec_required_columns, collapse = ", "),
      "."
    )
  )

  vec_key_columns <-
    c("dataset", "acc_species_name", "observation_id")

  data_values <-
    data_coordinate_rows |>
    dplyr::filter(.data$data_name %in% c("Latitude", "Longitude")) |>
    dplyr::mutate(
      coordinate = stringr::str_to_lower(.data$data_name),
      limit = dplyr::if_else(.data$coordinate == "latitude", 90, 180),
      value_standardized = base::suppressWarnings(
        base::as.numeric(.data$std_value)
      ),
      value_original = prepare_trait_numeric(.data$orig_value_str),
      flag_standardized_ok = base::is.finite(.data$value_standardized) &
        base::abs(.data$value_standardized) <= .data$limit,
      flag_original_ok = base::is.finite(.data$value_original) &
        base::abs(.data$value_original) <= .data$limit,
      value = dplyr::case_when(
        .data$flag_standardized_ok ~ .data$value_standardized,
        .data$flag_original_ok ~ .data$value_original,
        TRUE ~ NA_real_
      ),
      source = dplyr::case_when(
        .data$flag_standardized_ok ~ "standardized",
        .data$flag_original_ok ~ "original",
        base::is.finite(.data$value_standardized) |
          base::is.finite(.data$value_original) ~ "out_of_range",
        TRUE ~ "not_parsable"
      )
    )

  data_repeated <-
    data_values |>
    dplyr::count(
      dplyr::across(dplyr::all_of(c(vec_key_columns, "coordinate"))),
      name = "n_rows"
    ) |>
    dplyr::filter(.data$n_rows > 1L)

  if (
    base::nrow(data_repeated) > 0L
  ) {
    cli::cli_abort(
      c(
        "A TRY observation has more than one row for a coordinate.",
        "x" = "{base::nrow(data_repeated)} observation-coordinate pair{?s}."
      )
    )
  }

  data_template <-
    tibble::tibble(
      latitude_value = base::numeric(0),
      longitude_value = base::numeric(0),
      latitude_source = base::character(0),
      longitude_source = base::character(0)
    )

  data_wide <-
    data_values |>
    dplyr::select(
      dplyr::all_of(c(vec_key_columns, "coordinate", "value", "source"))
    ) |>
    tidyr::pivot_wider(
      names_from = "coordinate",
      values_from = c("value", "source"),
      names_glue = "{coordinate}_{.value}"
    )

  res <-
    dplyr::bind_rows(data_template, data_wide) |>
    dplyr::mutate(
      flag_pair_complete = !base::is.na(.data$latitude_value) &
        !base::is.na(.data$longitude_value),
      latitude = dplyr::if_else(
        .data$flag_pair_complete,
        .data$latitude_value,
        NA_real_
      ),
      longitude = dplyr::if_else(
        .data$flag_pair_complete,
        .data$longitude_value,
        NA_real_
      ),
      latitude_source = dplyr::if_else(
        !.data$flag_pair_complete & !base::is.na(.data$latitude_value),
        "incomplete_pair",
        .data$latitude_source
      ),
      longitude_source = dplyr::if_else(
        !.data$flag_pair_complete & !base::is.na(.data$longitude_value),
        "incomplete_pair",
        .data$longitude_source
      )
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(
          vec_key_columns,
          "latitude",
          "longitude",
          "latitude_source",
          "longitude_source"
        )
      )
    )

  return(res)
}
