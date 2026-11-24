#' @title Validate the TRY raw file
#' @description
#' Check that the manually downloaded TRY data request exists and is the
#' recorded version, identified by its SHA-256 checksum.
#' @param path_file
#' Character scalar. Path of the TRY request file (tab-separated text).
#' @param sha256_expected
#' Character scalar. Expected SHA-256 checksum of the file (64 lower-case
#' hexadecimal characters).
#' @param version_label
#' Character scalar. Human-readable name of the expected version, used in
#' error messages.
#' @return
#' The input `path_file`, unchanged, so that the function can be the
#' command of a `format = "file"` target.
#' @details
#' A TRY data request is a manual download from the TRY database; the
#' pipeline never downloads it. The function aborts with a message that
#' names the manual download and the expected version when the file is
#' missing, and with both checksums when the file differs from the
#' recorded version. It never substitutes another file. The checksum is
#' computed with `tools::sha256sum()`, as in the trait manifests.
#' @export
validate_try_raw_file <- function(
  path_file,
  sha256_expected,
  version_label
) {
  assertthat::assert_that(
    base::is.character(path_file),
    base::length(path_file) == 1L,
    !base::is.na(path_file),
    base::nzchar(path_file),
    msg = "'path_file' must be one non-empty character string."
  )

  assertthat::assert_that(
    base::is.character(sha256_expected),
    base::length(sha256_expected) == 1L,
    !base::is.na(sha256_expected),
    stringr::str_detect(sha256_expected, "^[0-9a-f]{64}$"),
    msg = stringr::str_c(
      "'sha256_expected' must be a SHA-256 checksum",
      " (64 lower-case hexadecimal characters)."
    )
  )

  assertthat::assert_that(
    base::is.character(version_label),
    base::length(version_label) == 1L,
    !base::is.na(version_label),
    base::nzchar(version_label),
    msg = "'version_label' must be one non-empty character string."
  )

  if (
    isFALSE(base::file.exists(path_file))
  ) {
    cli::cli_abort(
      c(
        "The TRY raw file does not exist: {.path {path_file}}",
        "i" = stringr::str_c(
          "A TRY data request is a manual download from the TRY database;",
          " the pipeline never downloads it."
        ),
        "i" = "Place {version_label} at this path."
      )
    )
  }

  sha256_file <-
    base::unname(tools::sha256sum(path_file))

  if (
    isFALSE(base::identical(sha256_file, sha256_expected))
  ) {
    cli::cli_abort(
      c(
        "The TRY raw file is not the recorded version ({version_label}).",
        "x" = "Its SHA-256 checksum is {sha256_file}.",
        "i" = "The recorded checksum is {sha256_expected}.",
        "i" = stringr::str_c(
          "Restore the recorded file, or change the version and checksum",
          " in {.file config.yml} deliberately."
        )
      )
    )
  }

  res <- path_file

  return(res)
}
