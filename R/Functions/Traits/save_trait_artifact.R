#' @title Save a trait artifact and return its path
#' @description
#' Save an R object with `qs2` under the producer naming convention
#' `<file_name>_<date>__<hash>__.qs` and return the exact path written, so
#' that manifests list the files a run produced instead of searching a
#' folder.
#' @param object_to_save
#' The R object to save.
#' @param file_name
#' Character scalar. The artifact stem, for example `"data_trait_try"`.
#' @param dir
#' Character scalar. An existing directory to save into.
#' @param save_date
#' Date scalar used in the file name. Defaults to `base::Sys.Date()`.
#' @return
#' Character scalar. The normalised path of the saved file.
#' @details
#' The hash is `rlang::hash()` of the object, as written by
#' `RUtilpol::save_latest_file()`, so the file name identifies the content.
#' The file is written in `qs2` format; the `.qs` extension is kept because
#' the producer artifact contract names the files that way. When a file of
#' the same name already exists it holds the same object, so it is not
#' rewritten. Older versions of the artifact are never removed.
#' @seealso [make_trait_artifact_manifest()]
#' @export
save_trait_artifact <- function(
  object_to_save,
  file_name,
  dir,
  save_date = base::Sys.Date()
) {
  assertthat::assert_that(
    base::is.character(file_name),
    base::length(file_name) == 1L,
    !base::is.na(file_name),
    base::nzchar(file_name),
    msg = "'file_name' must be one non-empty character string."
  )

  assertthat::assert_that(
    base::is.character(dir),
    base::length(dir) == 1L,
    base::dir.exists(dir),
    msg = "'dir' must be one existing directory."
  )

  assertthat::assert_that(
    base::inherits(save_date, "Date"),
    base::length(save_date) == 1L,
    !base::is.na(save_date),
    msg = "'save_date' must be one non-missing Date."
  )

  object_hash <-
    rlang::hash(object_to_save)

  path_artifact <-
    base::file.path(
      dir,
      stringr::str_glue(
        "{file_name}_{base::format(save_date, '%Y-%m-%d')}",
        "__{object_hash}__.qs"
      )
    ) |>
    base::normalizePath(
      winslash = "/",
      mustWork = FALSE
    )

  if (
    !base::file.exists(path_artifact)
  ) {
    qs2::qs_save(
      object = object_to_save,
      file = path_artifact
    )
  }

  res <- path_artifact

  return(res)
}
