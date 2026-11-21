#' @title Prepare BIEN records for the unit contract
#' @description
#' Repair the mis-encoded BIEN text, drop the records without a species
#' name, check the coordinates, and add the trait selection and the
#' canonical source columns that [resolve_trait_unit_contract()] reads.
#' @param data_bien_raw
#' A data frame of raw BIEN records with the columns `trait_name`,
#' `trait_value`, `unit`, `id`, `latitude`, `longitude` and
#' `scrubbed_species_binomial`, such as the output of
#' [load_bien_trait_raw_files()].
#' @param data_trait_selection
#' A data frame with one row per selected trait and the character columns
#' `trait_name`, `trait_domain`, `trait_full_name` and
#' `derivation_rule_key`.
#' @param vec_columns_text_repaired
#' Character vector. Text columns repaired by [normalise_text_encoding()];
#' those absent or not character are skipped.
#' @param vec_columns_selected
#' Character vector. Columns of `data_bien_raw` that are kept, in this
#' order; those absent are skipped.
#' @return
#' A tibble with one row per record that has a species name, in input
#' order, and these columns in this order: the selected columns, with `id`
#' as text and `latitude` and `longitude` as returned by
#' [prepare_bien_coordinates()]; `latitude_source` and `longitude_source`;
#' and the canonical columns `source_provider` (`"BIEN"`),
#' `source_dataset_key`, `source_record_id` (`id`), `source_trait_id` and
#' `source_trait_name` (both the trait name), `original_value_string`
#' (`trait_value` as text), `original_unit` (`unit`),
#' `source_standardized_value` (missing, as a number),
#' `source_standardized_unit` (missing), `trait_domain`,
#' `trait_full_name` and `derivation_rule_key` (from the selection).
#' @details
#' The text is repaired before any source key is built from it, so that
#' the tracked shared-source and quarantine lists hold repaired text.
#' `source_dataset_key` is the first of `project_pi`, `source_citation` and
#' `url_source` that is not missing, or `"BIEN"`. A trait that is not in
#' the selection gets missing trait columns. A species name is missing as
#' defined by [is_bien_species_missing()]; [summarise_bien_dropped_counts()]
#' counts those records.
#' @seealso [prepare_bien_coordinates()], [resolve_trait_unit_contract()]
#' @export
prepare_bien_contract_input <- function(
  data_bien_raw,
  data_trait_selection,
  vec_columns_text_repaired,
  vec_columns_selected
) {
  vec_raw_columns <-
    c(
      "trait_name", "trait_value", "unit", "id", "latitude", "longitude",
      "scrubbed_species_binomial"
    )

  vec_selection_columns <-
    c("trait_name", "trait_domain", "trait_full_name", "derivation_rule_key")

  assertthat::assert_that(
    base::is.data.frame(data_bien_raw),
    base::all(vec_raw_columns %in% base::names(data_bien_raw)),
    msg = stringr::str_c(
      "'data_bien_raw' must be a data frame with the columns: ",
      stringr::str_c(vec_raw_columns, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    base::is.data.frame(data_trait_selection),
    base::all(vec_selection_columns %in% base::names(data_trait_selection)),
    msg = stringr::str_c(
      "'data_trait_selection' must be a data frame with the columns: ",
      stringr::str_c(vec_selection_columns, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    base::is.character(vec_columns_text_repaired),
    msg = "'vec_columns_text_repaired' must be a character vector."
  )

  assertthat::assert_that(
    base::is.character(vec_columns_selected),
    base::length(vec_columns_selected) > 0L,
    msg = "'vec_columns_selected' must be a non-empty character vector."
  )

  # BIEN delivers part of its citation, PI and method text mis-encoded
  #   (UTF-8 read as Latin-1 or Windows-1252, once or twice). The text is
  #   repaired before any source key is built from it.
  data_bien_text_repaired <-
    data_bien_raw |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(vec_columns_text_repaired) &
          dplyr::where(base::is.character),
        .fns = ~ normalise_text_encoding(vec_text = .x)
      )
    )

  # The trait columns are renamed for the join and added last, as new
  #   canonical columns
  data_selection <-
    data_trait_selection |>
    dplyr::select(
      "trait_name",
      selection_trait_domain = "trait_domain",
      selection_trait_full_name = "trait_full_name",
      selection_derivation_rule_key = "derivation_rule_key"
    )

  res <-
    data_bien_text_repaired |>
    dplyr::filter(
      !is_bien_species_missing(vec_species = .data$scrubbed_species_binomial)
    ) |>
    dplyr::select(dplyr::any_of(vec_columns_selected)) |>
    # Coordinates out of range, and a coordinate without its pair, become
    #   missing, as in the TRY export; the record itself is kept
    prepare_bien_coordinates() |>
    dplyr::left_join(
      data_selection,
      by = "trait_name",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      id = base::as.character(.data$id),
      source_provider = "BIEN",
      source_dataset_key = dplyr::coalesce(
        .data$project_pi,
        .data$source_citation,
        .data$url_source,
        "BIEN"
      ),
      source_record_id = .data$id,
      source_trait_id = .data$trait_name,
      source_trait_name = .data$trait_name,
      original_value_string = base::as.character(.data$trait_value),
      original_unit = .data$unit,
      source_standardized_value = NA_real_,
      source_standardized_unit = NA_character_,
      trait_domain = .data$selection_trait_domain,
      trait_full_name = .data$selection_trait_full_name,
      derivation_rule_key = .data$selection_derivation_rule_key
    ) |>
    dplyr::select(-dplyr::starts_with("selection_"))

  return(res)
}
