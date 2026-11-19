#' @title Make a trait-artifact manifest
#' @description
#' Record the inputs, unit registry files and outputs of one trait export
#' with their sizes, checksums and row counts.
#' @param input_paths
#' Character vector. The exact source files the export read.
#' @param registry_directory
#' Character scalar. The directory with the unit registry CSV files.
#' @param output_paths
#' Character vector. The exact artifact files the export wrote, as returned
#' by [save_trait_artifact()].
#' @param output_rows
#' Optional integer vector, one non-negative row count per output path.
#' @param data_contract
#' Optional data whose row count and column names are recorded.
#' @return
#' A tibble with one row per distinct file and the columns `artifact_role`
#' (`"input"`, `"registry"` or `"output"`), `path`, `bytes`, `md5`,
#' `sha256`, `n_rows` (outputs only), `contract_rows`, `contract_schema`
#' and `recorded_at_utc`.
#' @details
#' Every listed file must exist; a missing file is an error, so the
#' manifest never silently omits an artifact. Files are not changed.
#' @export
make_trait_artifact_manifest <- function(
  input_paths,
  registry_directory = here::here("config", "trait_units"),
  output_paths = character(0),
  output_rows = NULL,
  data_contract = NULL
) {
  assertthat::assert_that(
    base::is.character(input_paths),
    base::is.character(output_paths),
    msg = "'input_paths' and 'output_paths' must be character vectors."
  )

  assertthat::assert_that(
    base::is.null(output_rows) ||
      (
        base::is.numeric(output_rows) &&
          base::length(output_rows) == base::length(output_paths) &&
          base::all(!base::is.na(output_rows)) &&
          base::all(output_rows >= 0)
      ),
    msg = stringr::str_c(
      "'output_rows' must be NULL or one non-negative count per output",
      " path."
    )
  )

  vec_listed_paths <-
    c(input_paths, output_paths)

  vec_missing_paths <-
    vec_listed_paths[!base::file.exists(vec_listed_paths)]

  if (
    base::length(vec_missing_paths) > 0L
  ) {
    cli::cli_abort(
      c(
        "Every artifact listed in a manifest must exist.",
        "x" = "Missing: {.file {vec_missing_paths}}"
      )
    )
  }

  vec_registry_paths <-
    base::list.files(
      registry_directory,
      full.names = TRUE
    )

  vec_output_rows <-
    if (
      base::is.null(output_rows)
    ) {
      base::rep(NA_integer_, base::length(output_paths))
    } else {
      base::as.integer(output_rows)
    }

  data_requested_artifacts <-
    tibble::tibble(
      artifact_role = c(
        base::rep("input", base::length(input_paths)),
        base::rep("registry", base::length(vec_registry_paths)),
        base::rep("output", base::length(output_paths))
      ),
      path = base::normalizePath(
        c(input_paths, vec_registry_paths, output_paths),
        winslash = "/",
        mustWork = TRUE
      ),
      n_rows = c(
        base::rep(NA_integer_, base::length(input_paths)),
        base::rep(NA_integer_, base::length(vec_registry_paths)),
        vec_output_rows
      )
    ) |>
    dplyr::distinct(.data$path, .keep_all = TRUE)

  n_contract_rows <-
    if (
      base::is.null(data_contract)
    ) {
      NA_integer_
    } else {
      base::nrow(data_contract)
    }

  text_contract_schema <-
    if (
      base::is.null(data_contract)
    ) {
      NA_character_
    } else {
      stringr::str_c(base::names(data_contract), collapse = "|")
    }

  res <-
    data_requested_artifacts |>
    dplyr::mutate(
      bytes = base::file.size(.data$path),
      md5 = base::unname(tools::md5sum(.data$path)),
      sha256 = base::unname(tools::sha256sum(.data$path)),
      contract_rows = n_contract_rows,
      contract_schema = text_contract_schema,
      recorded_at_utc = base::format(
        base::Sys.time(),
        tz = "UTC",
        usetz = TRUE
      )
    ) |>
    dplyr::relocate(
      "n_rows",
      .after = "sha256"
    )

  return(res)
}
