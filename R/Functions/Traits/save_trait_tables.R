#' @title Save the aggregate trait tables
#' @description
#' Save each table of a named list as a dated CSV file, the aggregate
#' tables and manifests of the trait export.
#' @param list_tables
#' A named list of data frames; each name is the file name of its table,
#' for example `"trait_unit_reconciliation_try"`.
#' @param dir_output
#' Character scalar. The output folder; it is created when it does not
#' exist.
#' @return
#' A character vector with the normalised path (forward slashes) of each
#' table's file, in the order of `list_tables`, so that the function can be
#' the command of a `format = "file"` target.
#' @details
#' Each table is written by `RUtilpol::save_latest_file()` as
#' `<name>_<YYYY-MM-DD>.csv`. When the newest file of that name holds the
#' same table, nothing is written and that file is returned; a table of
#' the same day is replaced, and files of earlier days are kept. The
#' tables are aggregate data only; record-level records never go through
#' this function.
#' @seealso [save_unpublished_trait_records()]
#' @export
save_trait_tables <- function(
  list_tables,
  dir_output
) {
  vec_table_names <-
    base::names(list_tables)

  assertthat::assert_that(
    base::is.list(list_tables),
    base::length(list_tables) > 0L,
    !base::is.null(vec_table_names),
    base::all(!base::is.na(vec_table_names)),
    base::all(base::nzchar(vec_table_names)),
    base::anyDuplicated(vec_table_names) == 0L,
    base::all(purrr::map_lgl(.x = list_tables, .f = base::is.data.frame)),
    msg = stringr::str_c(
      "'list_tables' must be a non-empty list of data frames with unique,",
      " non-empty names."
    )
  )

  assertthat::assert_that(
    base::is.character(dir_output),
    base::length(dir_output) == 1L,
    !base::is.na(dir_output),
    base::nzchar(dir_output),
    msg = "'dir_output' must be one non-empty character string."
  )

  base::dir.create(
    path = dir_output,
    recursive = TRUE,
    showWarnings = FALSE
  )

  # RUtilpol builds and parses R code from the path, so a Windows path
  #   must use forward slashes
  dir_output_normalised <-
    base::normalizePath(
      dir_output,
      winslash = "/",
      mustWork = TRUE
    )

  purrr::iwalk(
    .x = list_tables,
    .f = ~ RUtilpol::save_latest_file(
      object_to_save = .x,
      file_name = .y,
      dir = dir_output_normalised,
      prefered_format = "csv",
      verbose = FALSE
    )
  )

  res <-
    purrr::map_chr(
      .x = vec_table_names,
      .f = ~ base::file.path(
        dir_output_normalised,
        RUtilpol::get_latest_file_name(
          file_name = .x,
          dir = dir_output_normalised,
          verbose = FALSE
        )
      )
    )

  return(res)
}
