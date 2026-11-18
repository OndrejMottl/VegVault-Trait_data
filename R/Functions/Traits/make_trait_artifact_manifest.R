#' @title Make a trait-artifact manifest
#' @description Record paths, sizes, hashes, and optional contract metadata.
#' @param input_paths A character vector of source artifact paths.
#' @param registry_directory The directory containing registry CSV files.
#' @param output_paths A character vector of produced artifact paths.
#' @param data_contract Optional data whose row count and schema are recorded.
#' @return A tibble with one row per distinct artifact that currently exists.
#' @details Missing paths are omitted; existing files are not changed.
#' @export
make_trait_artifact_manifest <- function(
    input_paths,
    registry_directory = here::here("config", "trait_units"),
    output_paths = character(0),
    data_contract = NULL) {
  vec_registry_paths <-
    base::list.files(registry_directory, full.names = TRUE)

  data_requested_artifacts <-
    tibble::tibble(
      artifact_role = c(
        rep("input", length(input_paths)),
        rep("registry", length(vec_registry_paths)),
        rep("output", length(output_paths))
      ),
      path = base::normalizePath(
        c(input_paths, vec_registry_paths, output_paths),
        winslash = "/",
        mustWork = FALSE
      )
    ) %>%
    dplyr::distinct(.data$path, .keep_all = TRUE)

  data_existing_artifacts <-
    data_requested_artifacts %>%
    dplyr::filter(base::file.exists(.data$path))

  n_contract_rows <-
    if (is.null(data_contract)) NA_integer_ else nrow(data_contract)

  text_contract_schema <-
    if (is.null(data_contract)) NA_character_ else
      paste(names(data_contract), collapse = "|")

  data_manifest <-
    data_existing_artifacts %>%
    dplyr::mutate(
      bytes = base::file.info(.data$path)$size,
      md5 = base::unname(tools::md5sum(.data$path)),
      contract_rows = n_contract_rows,
      contract_schema = text_contract_schema,
      recorded_at_utc = base::format(
        base::Sys.time(),
        tz = "UTC",
        usetz = TRUE
      )
    )

  return(data_manifest)
}
