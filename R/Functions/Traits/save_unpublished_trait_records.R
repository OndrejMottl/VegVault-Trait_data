#' @title Save the unpublished trait records for audit
#' @description
#' Save the quarantined and dropped records of one provider to the private
#' audit folder.
#' @param data_source
#' Trait records with their final `disposition`, such as the resolved
#' records of one provider.
#' @param file_name
#' Character scalar. File name without extension, for example
#' `"trait_unit_unpublished_try"`.
#' @param dir_output
#' Character scalar. The audit folder; it is created when it does not
#' exist. It must be git-ignored.
#' @return
#' Character scalar. The path of the saved file,
#' `<dir_output>/<file_name>.qs2`.
#' @details
#' The records whose `disposition` is not `"published"` are saved with
#' `qs2::qs_save()`, replacing an earlier file of the same name. They are
#' record-level licensed data (TRY and BIEN), so the folder must never be
#' committed.
#' @export
save_unpublished_trait_records <- function(
  data_source,
  file_name,
  dir_output
) {
  assertthat::assert_that(
    base::is.data.frame(data_source),
    "disposition" %in% base::names(data_source),
    msg = "'data_source' must be a data frame with the column 'disposition'."
  )

  assertthat::assert_that(
    base::is.character(file_name),
    base::length(file_name) == 1L,
    !base::is.na(file_name),
    base::nzchar(file_name),
    msg = "'file_name' must be one non-empty character string."
  )

  assertthat::assert_that(
    base::is.character(dir_output),
    base::length(dir_output) == 1L,
    !base::is.na(dir_output),
    base::nzchar(dir_output),
    msg = "'dir_output' must be one non-empty character string."
  )

  data_unpublished <-
    data_source |>
    dplyr::filter(.data$disposition != "published")

  base::dir.create(
    path = dir_output,
    recursive = TRUE,
    showWarnings = FALSE
  )

  res <-
    base::file.path(
      dir_output,
      stringr::str_c(file_name, ".qs2")
    )

  qs2::qs_save(
    object = data_unpublished,
    file = res
  )

  return(res)
}
