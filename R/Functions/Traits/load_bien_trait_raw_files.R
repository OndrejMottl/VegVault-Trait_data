#' @title Load the raw BIEN trait files
#' @description
#' Read the raw files of the BIEN traits and combine their records into
#' one table with clean column names.
#' @param vec_paths
#' Character vector. Paths of existing raw files written with `qs2`, one
#' per trait, such as those of [resolve_bien_trait_raw_file()].
#' @return
#' A tibble with the records of every file, in the order of `vec_paths`,
#' and column names cleaned by `janitor::clean_names()`.
#' @details
#' Every file must hold a data frame. A file without rows is left out with
#' a warning that names it, so that an empty download is visible but does
#' not stop the run. Columns missing from one file are missing values in
#' its rows.
#' @seealso [resolve_bien_trait_raw_file()]
#' @export
load_bien_trait_raw_files <- function(vec_paths) {
  assertthat::assert_that(
    base::is.character(vec_paths),
    base::length(vec_paths) > 0L,
    base::all(base::file.exists(vec_paths)),
    msg = "'vec_paths' must be the paths of existing raw files."
  )

  list_raw <-
    purrr::map(
      .x = vec_paths,
      .f = ~ qs2::qs_read(file = .x)
    )

  vec_is_table <-
    purrr::map_lgl(
      .x = list_raw,
      .f = ~ base::is.data.frame(.x)
    )

  if (
    !base::all(vec_is_table)
  ) {
    cli::cli_abort(
      c(
        "Every raw BIEN file must hold a data frame.",
        "x" = "{.file {vec_paths[!vec_is_table]}}"
      )
    )
  }

  vec_is_empty <-
    purrr::map_lgl(
      .x = list_raw,
      .f = ~ base::nrow(.x) == 0L
    )

  if (
    base::any(vec_is_empty)
  ) {
    cli::cli_warn(
      c(
        "Raw BIEN files without records are left out.",
        "!" = "{.file {base::basename(vec_paths[vec_is_empty])}}"
      )
    )
  }

  res <-
    list_raw[!vec_is_empty] |>
    dplyr::bind_rows() |>
    tibble::as_tibble() |>
    janitor::clean_names()

  return(res)
}
