#' @title Save a trait export artefact in numbered parts
#' @description
#' Split the published records of one provider, in their row order, into
#' numbered parts and save each part as an artefact file.
#' @param data_source
#' A data frame of published records, already in the export row order.
#' @param file_stem
#' Character scalar. The artefact stem before the part number, for example
#' `"data_traits_bien"`.
#' @param n_parts
#' Integer scalar. The number of parts, at least 1.
#' @param dir_output
#' Character scalar. The output folder; it is created when it does not
#' exist.
#' @return
#' A tibble with one row per part and the columns `part` (integer, from 0),
#' `path` (as returned by [save_trait_artifact()]) and `n_rows` (integer).
#' @details
#' Consecutive rows form a part: `base::cut()` splits the row numbers into
#' `n_parts` intervals of nearly equal length. Part `<p>` is saved as
#' `<file_stem>_<p>_<YYYY-MM-DD>__<hash>__.qs` by [save_trait_artifact()].
#' The set is then checked by [validate_trait_artifact_parts()], which
#' reads every part back. The function aborts when `data_source` has fewer
#' rows than `n_parts`, so that no part is empty. The parts are not checked
#' against the VegVault contract here: [validate_trait_export()] does that.
#' @seealso [save_trait_export()], [validate_trait_export()]
#' @export
save_trait_export_parts <- function(
  data_source,
  file_stem,
  n_parts,
  dir_output
) {
  assertthat::assert_that(
    base::is.data.frame(data_source),
    msg = "'data_source' must be a data frame."
  )

  assertthat::assert_that(
    base::is.character(file_stem),
    base::length(file_stem) == 1L,
    !base::is.na(file_stem),
    base::nzchar(file_stem),
    msg = "'file_stem' must be one non-empty character string."
  )

  assertthat::assert_that(
    assertthat::is.count(n_parts),
    msg = "'n_parts' must be one positive whole number."
  )

  assertthat::assert_that(
    base::is.character(dir_output),
    base::length(dir_output) == 1L,
    !base::is.na(dir_output),
    base::nzchar(dir_output),
    msg = "'dir_output' must be one non-empty character string."
  )

  n_rows_source <-
    base::nrow(data_source)

  if (
    n_rows_source < n_parts
  ) {
    cli::cli_abort(
      c(
        "The records have fewer rows than the {n_parts} parts.",
        "x" = "Rows: {n_rows_source}."
      )
    )
  }

  base::dir.create(
    path = dir_output,
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Consecutive rows form a part, numbered from 0. base::cut() refuses a
  #   single interval, so one part is assigned directly.
  vec_part <-
    if (
      n_parts == 1L
    ) {
      base::rep(0L, n_rows_source)
    } else {
      base::cut(
        base::seq_len(n_rows_source),
        breaks = n_parts,
        labels = FALSE
      ) - 1L
    }

  data_parts <-
    (base::seq_len(n_parts) - 1L) |>
    purrr::map(
      .f = ~ {
        data_part <-
          dplyr::filter(data_source, vec_part == .x)

        path_part <-
          save_trait_artifact(
            object_to_save = data_part,
            file_name = stringr::str_c(file_stem, "_", .x),
            dir = dir_output
          )

        tibble::tibble(
          part = base::as.integer(.x),
          path = path_part,
          n_rows = base::nrow(data_part)
        )
      }
    ) |>
    purrr::list_rbind()

  validate_trait_artifact_parts(
    data_parts = data_parts,
    file_stem = file_stem,
    n_parts_expected = n_parts,
    n_rows_expected = n_rows_source
  )

  res <- data_parts

  return(res)
}
