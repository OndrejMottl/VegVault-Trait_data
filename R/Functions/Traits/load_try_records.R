#' @title Load the TRY records of the selected traits
#' @description
#' Read the TRY request file once and keep the records of the selected
#' traits and the selected ancillary records of their observations.
#' @param path_file
#' Character scalar. Path of the TRY request file (tab-separated, Latin-1).
#' @param vec_columns
#' Character vector. TRY columns to read; it must include `TraitID`,
#' `DataID`, `DataName` and `ObservationID`.
#' @param vec_trait_ids
#' Character vector. TRY trait IDs (`TraitID`) of the selected traits.
#' @param vec_ancillary_names
#' Character vector. TRY names (`DataName`) of the ancillary variables to
#' keep, for example `"Latitude"`.
#' @param vec_data_ids_excluded
#' Character vector. DataIDs whose ancillary records are not kept. It may be
#' empty.
#' @return
#' A list with two tibbles:
#' - `data_trait_rows`: the records whose `TraitID` is selected, with the
#'   columns of `vec_columns` and `source_trait_id` (`TraitID` as
#'   character), in file order.
#' - `data_ancillary`: records without a `TraitID` whose `DataName` is in
#'   `vec_ancillary_names`, whose DataID is not excluded and whose
#'   observation has a record of a selected trait, with the columns of
#'   `vec_columns`, in file order.
#' @details
#' The file is read with the settings of `rtry::rtry_import()` (tab
#' separator, `.` decimal mark, Latin-1, no quoting) and only the columns
#' in `vec_columns`, because the full request (about 17 million rows) must
#' not be held in memory more than once. Trait records of excluded DataIDs
#' are kept here, so that they can be counted as dropped. Trait IDs and
#' DataIDs are compared as text.
#' @export
load_try_records <- function(
  path_file,
  vec_columns,
  vec_trait_ids,
  vec_ancillary_names,
  vec_data_ids_excluded
) {
  assertthat::assert_that(
    base::is.character(path_file),
    base::length(path_file) == 1L,
    !base::is.na(path_file),
    base::file.exists(path_file),
    msg = "'path_file' must be the path of one existing TRY request file."
  )

  vec_columns_used <-
    c("TraitID", "DataID", "DataName", "ObservationID")

  assertthat::assert_that(
    base::is.character(vec_columns),
    base::all(vec_columns_used %in% vec_columns),
    msg = stringr::str_c(
      "'vec_columns' must be a character vector that includes 'TraitID',",
      " 'DataID', 'DataName' and 'ObservationID'."
    )
  )

  assertthat::assert_that(
    base::is.character(vec_trait_ids),
    base::length(vec_trait_ids) > 0L,
    msg = "'vec_trait_ids' must be a non-empty character vector."
  )

  assertthat::assert_that(
    base::is.character(vec_ancillary_names),
    msg = "'vec_ancillary_names' must be a character vector."
  )

  assertthat::assert_that(
    base::is.character(vec_data_ids_excluded),
    msg = "'vec_data_ids_excluded' must be a character vector."
  )

  # Read only the header first, so that a missing column stops the run
  #   instead of being skipped with a warning
  vec_columns_file <-
    data.table::fread(
      file = path_file,
      header = TRUE,
      sep = "\t",
      quote = "",
      nrows = 0L,
      showProgress = FALSE
    ) |>
    base::names()

  vec_columns_missing <-
    base::setdiff(vec_columns, vec_columns_file)

  if (
    base::length(vec_columns_missing) > 0L
  ) {
    cli::cli_abort(
      c(
        "The TRY request file lacks selected columns.",
        "x" = "Missing: {.val {vec_columns_missing}}"
      )
    )
  }

  data_raw <-
    data.table::fread(
      file = path_file,
      header = TRUE,
      sep = "\t",
      dec = ".",
      encoding = "Latin-1",
      quote = "",
      select = vec_columns,
      showProgress = FALSE
    )

  vec_trait_id_raw <-
    base::as.character(data_raw[["TraitID"]])

  vec_is_trait_row <-
    vec_trait_id_raw %in% vec_trait_ids

  data_trait_rows <-
    data_raw[vec_is_trait_row] |>
    tibble::as_tibble() |>
    dplyr::mutate(
      source_trait_id = base::as.character(.data$TraitID)
    )

  vec_observation_ids <-
    data_trait_rows |>
    dplyr::distinct(.data$ObservationID) |>
    dplyr::pull("ObservationID")

  vec_is_ancillary_row <-
    base::is.na(data_raw[["TraitID"]]) &
    data_raw[["DataName"]] %in% vec_ancillary_names &
    !base::as.character(data_raw[["DataID"]]) %in% vec_data_ids_excluded &
    data_raw[["ObservationID"]] %in% vec_observation_ids

  data_ancillary <-
    data_raw[vec_is_ancillary_row] |>
    tibble::as_tibble()

  res <-
    list(
      data_trait_rows = data_trait_rows,
      data_ancillary = data_ancillary
    )

  return(res)
}
