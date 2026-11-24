#' @title Validate a partitioned trait artifact
#' @description
#' Check that the parts written for one partitioned trait artifact form a
#' complete, correctly named set whose row counts add up.
#' @param data_parts
#' A data frame with one row per written part and the columns `part`
#' (integer part number), `path` (as returned by [save_trait_artifact()])
#' and `n_rows` (rows written to that part).
#' @param file_stem
#' Character scalar. The artifact stem before the part number, for example
#' `"data_traits_bien"`.
#' @param n_parts_expected
#' Integer scalar. The number of parts that must exist.
#' @param n_rows_expected
#' Integer scalar. The total number of rows the parts must hold.
#' @param flag_read_back
#' Logical. If `TRUE` (default), every part is read with `qs2` and its row
#' count compared with `n_rows`.
#' @return
#' `data_parts`, invisibly, when every check passes.
#' @details
#' Parts are numbered from 0 without gaps or repeats, and every file name
#' must match `<file_stem>_<part>_<YYYY-MM-DD>__<32 hex>__.qs`. A stale file
#' cannot enter the set because the paths come from the save step, not from
#' a folder listing.
#' @seealso [save_trait_artifact()], [build_trait_artifact_manifest()]
#' @export
validate_trait_artifact_parts <- function(
  data_parts,
  file_stem,
  n_parts_expected,
  n_rows_expected,
  flag_read_back = TRUE
) {
  assertthat::assert_that(
    base::is.data.frame(data_parts),
    base::all(c("part", "path", "n_rows") %in% base::names(data_parts)),
    msg = "'data_parts' must be a data frame with part, path and n_rows."
  )

  assertthat::assert_that(
    base::is.character(file_stem),
    base::length(file_stem) == 1L,
    base::nzchar(file_stem),
    msg = "'file_stem' must be one non-empty character string."
  )

  assertthat::assert_that(
    assertthat::is.count(n_parts_expected),
    assertthat::is.count(n_rows_expected) || n_rows_expected == 0,
    assertthat::is.flag(flag_read_back),
    msg = stringr::str_c(
      "'n_parts_expected' must be a positive count, 'n_rows_expected' a",
      " non-negative count and 'flag_read_back' TRUE or FALSE."
    )
  )

  vec_parts <-
    dplyr::pull(data_parts, "part")

  vec_paths <-
    dplyr::pull(data_parts, "path")

  vec_rows <-
    dplyr::pull(data_parts, "n_rows")

  if (
    base::length(vec_parts) != n_parts_expected
  ) {
    cli::cli_abort(
      "Expected {n_parts_expected} parts, found {base::length(vec_parts)}."
    )
  }

  if (
    !base::identical(
      base::sort(base::as.integer(vec_parts)),
      base::seq_len(n_parts_expected) - 1L
    )
  ) {
    cli::cli_abort(
      "Artifact parts must be numbered from 0 without gaps or repeats."
    )
  }

  vec_missing_paths <-
    vec_paths[!base::file.exists(vec_paths)]

  if (
    base::length(vec_missing_paths) > 0L ||
      base::anyDuplicated(vec_paths) > 0L
  ) {
    cli::cli_abort(
      c(
        "Every artifact part must exist as its own file.",
        "x" = "Missing: {.file {vec_missing_paths}}"
      )
    )
  }

  vec_expected_patterns <-
    stringr::str_glue(
      "^{file_stem}_{vec_parts}_[0-9]{{4}}-[0-9]{{2}}-[0-9]{{2}}",
      "__[0-9a-f]{{32}}__[.]qs$"
    )

  vec_name_matches <-
    purrr::map2_lgl(
      .x = base::basename(vec_paths),
      .y = vec_expected_patterns,
      .f = ~ stringr::str_detect(.x, .y)
    )

  if (
    !base::all(vec_name_matches)
  ) {
    cli::cli_abort(
      c(
        "Artifact part file name does not match its part number and stem.",
        "x" = "{.file {base::basename(vec_paths[!vec_name_matches])}}"
      )
    )
  }

  if (
    base::sum(vec_rows) != n_rows_expected
  ) {
    cli::cli_abort(
      "Artifact parts hold {base::sum(vec_rows)} rows, not {n_rows_expected}."
    )
  }

  if (
    base::isTRUE(flag_read_back)
  ) {
    vec_rows_read <-
      purrr::map_int(
        .x = vec_paths,
        .f = ~ base::nrow(qs2::qs_read(.x))
      )

    if (
      !base::identical(vec_rows_read, base::as.integer(vec_rows))
    ) {
      cli::cli_abort(
        "Rows read back from the artifact parts differ from rows written."
      )
    }
  }

  res <- data_parts

  return(base::invisible(res))
}
