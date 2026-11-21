#' @title Resolve the raw file of one BIEN trait
#' @description
#' Choose the raw file of one BIEN trait: the newest file in the BIEN input
#' folder, or a new download when downloading is enabled.
#' @param dir_raw
#' Character scalar. Existing directory of the raw BIEN files.
#' @param trait_name
#' Character scalar. BIEN trait name, for example `"seed mass"`.
#' @param flag_download
#' Logical scalar. `TRUE` downloads the trait again with
#' [save_bien_trait_download()] and returns the new file; `FALSE` only
#' reads the folder.
#' @return
#' Character scalar. The path of the chosen raw file.
#' @details
#' A raw file is named `<trait_name>_<YYYY-MM-DD>__<32 hex>__.qs`. The name
#' must match exactly, so a trait whose name contains this one (`"leaf
#' area"` and `"leaf area per leaf dry mass"`), sub-folders and other files
#' are never picked. Of the matching files, the one with the latest date in
#' its name is chosen; the modification time is never used. The function
#' aborts when two files share the latest date, and, with
#' `flag_download = FALSE`, when no file matches, naming the flag that
#' enables the download.
#' @seealso [save_bien_trait_download()], [load_bien_trait_raw_files()]
#' @export
resolve_bien_trait_raw_file <- function(
  dir_raw,
  trait_name,
  flag_download
) {
  assertthat::assert_that(
    base::is.character(dir_raw),
    base::length(dir_raw) == 1L,
    !base::is.na(dir_raw),
    base::dir.exists(dir_raw),
    msg = "'dir_raw' must be the path of one existing directory."
  )

  assertthat::assert_that(
    base::is.character(trait_name),
    base::length(trait_name) == 1L,
    !base::is.na(trait_name),
    base::nzchar(trait_name),
    msg = "'trait_name' must be one non-empty character string."
  )

  assertthat::assert_that(
    assertthat::is.flag(flag_download),
    !base::is.na(flag_download),
    msg = "'flag_download' must be TRUE or FALSE."
  )

  if (
    isTRUE(flag_download)
  ) {
    res_download <-
      save_bien_trait_download(
        trait_name = trait_name,
        dir_raw = dir_raw
      )

    return(res_download)
  }

  pattern_raw_file <-
    stringr::str_c(
      "^",
      stringr::str_escape(trait_name),
      "_([0-9]{4}-[0-9]{2}-[0-9]{2})__[0-9a-f]{32}__[.]qs$"
    )

  vec_paths_listed <-
    base::list.files(
      path = dir_raw,
      full.names = TRUE
    )

  vec_paths_matching <-
    vec_paths_listed[
      stringr::str_detect(base::basename(vec_paths_listed), pattern_raw_file) &
        !base::dir.exists(vec_paths_listed)
    ]

  if (
    base::length(vec_paths_matching) == 0L
  ) {
    cli::cli_abort(
      c(
        "BIEN trait {.val {trait_name}} has no raw file in {.path {dir_raw}}.",
        "i" = stringr::str_c(
          "Set {.code bien: flag_download: true} in {.file config.yml} to",
          " download it, and back to false afterwards."
        )
      )
    )
  }

  vec_dates <-
    stringr::str_match(
      base::basename(vec_paths_matching),
      pattern_raw_file
    )[, 2]

  date_latest <-
    base::max(vec_dates)

  vec_paths_latest <-
    vec_paths_matching[vec_dates == date_latest]

  if (
    base::length(vec_paths_latest) > 1L
  ) {
    cli::cli_abort(
      c(
        "BIEN trait {.val {trait_name}} has several files of {date_latest}.",
        "x" = "{.file {base::basename(vec_paths_latest)}}",
        "i" = "Keep one file per date; the pipeline never chooses between them."
      )
    )
  }

  res <- vec_paths_latest

  return(res)
}
