#' @title Prepare numeric BIEN coordinates
#' @description
#' Set BIEN coordinates that cannot form a location to missing, and record
#' a status per coordinate, following the same rule as
#' [prepare_try_coordinates()].
#' @param data_source
#' A data frame of BIEN trait records with the numeric columns `latitude`
#' and `longitude` (WGS84 decimal degrees, missing when not given). A
#' logical column that holds only missing values is accepted as a column
#' without any coordinate.
#' @return
#' A tibble with the rows and columns of `data_source` in the same order,
#' where `latitude` and `longitude` are double and missing when unusable,
#' followed by the new character columns `latitude_source` and
#' `longitude_source`.
#' @details
#' A coordinate is usable when it is finite and within range: latitude -90
#' to 90 and longitude -180 to 180, bounds included. Its source is then
#' `"original"`. A coordinate that is given but not usable (out of range,
#' `Inf` or `-Inf`) is set missing with the source `"out_of_range"`. A
#' location needs both coordinates: when one is unusable or not given, both
#' are set missing and the usable one gets the source `"incomplete_pair"`.
#' A coordinate that is not given (`NA` or `NaN`) keeps a missing source.
#' Other columns are returned unchanged. Columns `latitude_source` or
#' `longitude_source` already present in `data_source` are an error.
#' @seealso [prepare_try_coordinates()], [summarise_trait_coordinates()]
#' @export
prepare_bien_coordinates <- function(data_source) {
  assertthat::assert_that(
    base::is.data.frame(data_source),
    base::all(c("latitude", "longitude") %in% base::names(data_source)),
    msg = stringr::str_c(
      "'data_source' must be a data frame with the numeric columns ",
      "'latitude' and 'longitude'."
    )
  )

  vec_latitude_input <-
    dplyr::pull(data_source, "latitude")

  vec_longitude_input <-
    dplyr::pull(data_source, "longitude")

  assertthat::assert_that(
    base::is.numeric(vec_latitude_input) ||
      (
        base::is.logical(vec_latitude_input) &&
          base::all(base::is.na(vec_latitude_input))
      ),
    base::is.numeric(vec_longitude_input) ||
      (
        base::is.logical(vec_longitude_input) &&
          base::all(base::is.na(vec_longitude_input))
      ),
    msg = stringr::str_c(
      "'latitude' and 'longitude' must be numeric (or logical with only ",
      "missing values)."
    )
  )

  assertthat::assert_that(
    !base::any(
      c("latitude_source", "longitude_source") %in% base::names(data_source)
    ),
    msg = stringr::str_c(
      "'data_source' must not contain 'latitude_source' or ",
      "'longitude_source'."
    )
  )

  # The flags are kept as vectors, not columns, so no column of
  #   `data_source` can be overwritten or dropped by them
  vec_latitude <-
    base::as.double(vec_latitude_input)

  vec_longitude <-
    base::as.double(vec_longitude_input)

  vec_latitude_ok <-
    base::is.finite(vec_latitude) & base::abs(vec_latitude) <= 90

  vec_longitude_ok <-
    base::is.finite(vec_longitude) & base::abs(vec_longitude) <= 180

  vec_pair_complete <-
    vec_latitude_ok & vec_longitude_ok

  vec_latitude_source <-
    dplyr::case_when(
      vec_pair_complete ~ "original",
      vec_latitude_ok ~ "incomplete_pair",
      !base::is.na(vec_latitude) ~ "out_of_range",
      .default = NA_character_
    )

  vec_longitude_source <-
    dplyr::case_when(
      vec_pair_complete ~ "original",
      vec_longitude_ok ~ "incomplete_pair",
      !base::is.na(vec_longitude) ~ "out_of_range",
      .default = NA_character_
    )

  res <-
    data_source |>
    tibble::as_tibble() |>
    dplyr::mutate(
      latitude = dplyr::if_else(vec_pair_complete, vec_latitude, NA_real_),
      longitude = dplyr::if_else(vec_pair_complete, vec_longitude, NA_real_),
      latitude_source = vec_latitude_source,
      longitude_source = vec_longitude_source
    )

  return(res)
}
