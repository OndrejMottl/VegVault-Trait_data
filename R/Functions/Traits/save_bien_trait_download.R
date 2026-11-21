#' @title Download and save the records of one BIEN trait
#' @description
#' Download every record of one trait from the BIEN database and save them
#' as a raw file in the BIEN input folder.
#' @param trait_name
#' Character scalar. BIEN trait name, for example `"seed mass"`.
#' @param dir_raw
#' Character scalar. Existing directory of the raw BIEN files.
#' @return
#' Character scalar. The path of the saved raw file.
#' @details
#' The records come from `BIEN::BIEN_trait_trait()` with
#' `all.taxonomy = FALSE` and `source.citation = TRUE`, as a tibble. They
#' are saved by [save_trait_artifact()] as
#' `<trait_name>_<YYYY-MM-DD>__<hash>__.qs` (`qs2` format), the name
#' [resolve_bien_trait_raw_file()] looks for. Earlier files are never
#' removed. The function aborts, without saving, when BIEN returns no data
#' frame or no rows. It contacts the BIEN database, so it is called only
#' when downloading is enabled.
#' @seealso [resolve_bien_trait_raw_file()]
#' @export
save_bien_trait_download <- function(
  trait_name,
  dir_raw
) {
  assertthat::assert_that(
    base::is.character(trait_name),
    base::length(trait_name) == 1L,
    !base::is.na(trait_name),
    base::nzchar(trait_name),
    msg = "'trait_name' must be one non-empty character string."
  )

  assertthat::assert_that(
    base::is.character(dir_raw),
    base::length(dir_raw) == 1L,
    !base::is.na(dir_raw),
    base::dir.exists(dir_raw),
    msg = "'dir_raw' must be the path of one existing directory."
  )

  data_download <-
    BIEN::BIEN_trait_trait(
      trait = trait_name,
      all.taxonomy = FALSE,
      source.citation = TRUE
    )

  if (
    isFALSE(base::is.data.frame(data_download))
  ) {
    cli::cli_abort(
      "BIEN returned no data frame for the trait {.val {trait_name}}."
    )
  }

  if (
    base::nrow(data_download) == 0L
  ) {
    cli::cli_abort(
      "BIEN returned no records for the trait {.val {trait_name}}."
    )
  }

  data_download_tibble <-
    tibble::as_tibble(data_download)

  res <-
    save_trait_artifact(
      object_to_save = data_download_tibble,
      file_name = trait_name,
      dir = dir_raw
    )

  return(res)
}
