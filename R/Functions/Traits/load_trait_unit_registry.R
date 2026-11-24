#' @title Load the trait-unit registry
#' @description Load and validate alias, conversion, and derivation rules.
#' @param registry_directory The directory containing the three registry CSVs.
#' @return A named list with alias, conversion, and derivation tibbles.
#' @details Missing files and duplicate rule keys cause an error.
#' @export
load_trait_unit_registry <- function(
    registry_directory = here::here("config", "trait_units")) {
  vec_registry_files <-
    c(
      aliases = "unit_aliases.csv",
      conversions = "unit_conversions.csv",
      derivations = "trait_derivations.csv"
    )

  vec_registry_paths <-
    base::file.path(
      registry_directory,
      vec_registry_files
    ) |>
    rlang::set_names(base::names(vec_registry_files))

  if (
    !base::all(base::file.exists(vec_registry_paths))
  ) {
    cli::cli_abort(
      c(
        "The trait-unit registry is incomplete.",
        "x" = "Missing: {vec_registry_paths[!file.exists(vec_registry_paths)]}"
      )
    )
  }

  list_registry <-
    purrr::map(
      vec_registry_paths,
      ~ readr::read_csv(
        file = .x,
        col_types = readr::cols(.default = readr::col_character()),
        na = c("", "NA"),
        show_col_types = FALSE,
        progress = FALSE
      )
    )

  data_aliases <-
    list_registry$aliases

  data_conversions <-
    list_registry$conversions |>
    dplyr::mutate(
      multiplier = base::as.numeric(.data$multiplier),
      verification_absolute_tolerance =
        base::as.numeric(.data$verification_absolute_tolerance),
      verify_source_standardized =
        .data$verify_source_standardized == "TRUE"
    )

  data_derivations <-
    list_registry$derivations |>
    dplyr::mutate(
      multiplier = base::as.numeric(.data$multiplier)
    )

  if (
    base::anyDuplicated(data_aliases$original_unit) > 0L
  ) {
    cli::cli_abort("Unit aliases must be unique by {.field original_unit}.")
  }

  vec_conversion_keys <-
    c(
      "source_provider",
      "source_trait_id",
      "source_dataset_key",
      "normalized_original_unit"
    )

  data_duplicate_conversions <-
    data_conversions |>
    dplyr::count(
      dplyr::across(dplyr::all_of(vec_conversion_keys)),
      name = "n_rules"
    ) |>
    dplyr::filter(.data$n_rules > 1L)

  if (
    base::nrow(data_duplicate_conversions) > 0L
  ) {
    cli::cli_abort("Trait-unit conversion registry keys must be unique.")
  }

  if (
    base::anyDuplicated(data_derivations$derivation_rule_key) > 0L
  ) {
    cli::cli_abort("Derivation rule keys must be unique.")
  }

  res <-
    list(
      aliases = data_aliases,
      conversions = data_conversions,
      derivations = data_derivations
    )

  return(res)
}
