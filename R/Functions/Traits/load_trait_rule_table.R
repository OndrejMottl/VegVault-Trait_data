#' @title Load a tracked trait rule table
#' @description
#' Read one of the tracked CSV tables that steer the trait export (trait
#' selection, DataID exclusions, source quarantines, shared BIEN sources)
#' with every column as character.
#' @param path_file
#' Character scalar. Path of an existing CSV file with a header row.
#' @param vec_columns_required
#' Character vector. Columns the table must contain.
#' @return
#' A tibble with every column of the file as character, in file order.
#' Empty fields and `"NA"` are missing values, and surrounding white space
#' is removed, as by `readr::read_csv()` defaults.
#' @details
#' Reading everything as character keeps identifiers such as TRY trait and
#' DataID numbers as text, which is how the unit contract and the rule
#' functions compare them. A table without rows is returned as it is,
#' because an empty rule list is valid. The function aborts when a required
#' column is missing.
#' @export
load_trait_rule_table <- function(
  path_file,
  vec_columns_required
) {
  assertthat::assert_that(
    base::is.character(path_file),
    base::length(path_file) == 1L,
    !base::is.na(path_file),
    base::file.exists(path_file),
    msg = "'path_file' must be the path of one existing CSV file."
  )

  assertthat::assert_that(
    base::is.character(vec_columns_required),
    base::length(vec_columns_required) > 0L,
    base::all(!base::is.na(vec_columns_required)),
    msg = "'vec_columns_required' must be a non-empty character vector."
  )

  data_rules <-
    readr::read_csv(
      file = path_file,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE,
      progress = FALSE
    )

  vec_columns_missing <-
    base::setdiff(vec_columns_required, base::names(data_rules))

  if (
    base::length(vec_columns_missing) > 0L
  ) {
    cli::cli_abort(
      c(
        "The rule table {.file {path_file}} lacks required columns.",
        "x" = "Missing: {.val {vec_columns_missing}}"
      )
    )
  }

  res <- data_rules

  return(res)
}
