#' @title Save a trait export artefact
#' @description
#' Save the published records of one provider as the artefact that
#' VegVault reads, creating the output folder when needed.
#' @param data_source
#' The published records to save.
#' @param file_name
#' Character scalar. The artefact stem, for example `"data_trait_try"`.
#' @param dir_output
#' Character scalar. The output folder; it is created when it does not
#' exist.
#' @return
#' Character scalar. The path of the artefact, as returned by
#' [save_trait_artifact()].
#' @details
#' The file is named `<file_name>_<YYYY-MM-DD>__<hash>__.qs` and written by
#' [save_trait_artifact()] in `qs2` format; an existing file of that name
#' holds the same object and is not rewritten, and earlier versions are
#' never removed. The artefact is not checked here: [validate_trait_export()]
#' checks it against its VegVault contract.
#' @seealso [save_trait_export_parts()], [validate_trait_export()]
#' @export
save_trait_export <- function(
  data_source,
  file_name,
  dir_output
) {
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

  base::dir.create(
    path = dir_output,
    recursive = TRUE,
    showWarnings = FALSE
  )

  res <-
    save_trait_artifact(
      object_to_save = data_source,
      file_name = file_name,
      dir = dir_output
    )

  return(res)
}
