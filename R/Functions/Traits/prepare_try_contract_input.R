#' @title Prepare TRY records for the unit contract
#' @description
#' Combine the TRY trait records with the references and coordinates of
#' their observations and with the trait selection, and add the canonical
#' source columns that [resolve_trait_unit_contract()] reads.
#' @param data_trait_rows
#' A data frame of TRY trait records with the TRY columns `Dataset`,
#' `AccSpeciesName`, `ObservationID`, `ObsDataID`, `DataID`,
#' `OrigValueStr`, `OrigUnitStr`, `ValueKindName`, `OrigObsDataID`,
#' `StdValue`, `UnitName` and `Reference`, and `source_trait_id`, such as
#' `data_trait_rows` of [load_try_records()].
#' @param data_ancillary
#' A data frame of TRY ancillary records with the columns `Dataset`,
#' `AccSpeciesName`, `ObservationID`, `DataName`, `OrigValueStr` and
#' `StdValue`, such as `data_ancillary` of [load_try_records()].
#' @param data_trait_selection
#' A data frame with one row per selected trait and the character columns
#' `trait_id`, `trait_domain`, `trait_full_name` and `derivation_rule_key`.
#' @param vec_data_ids_excluded
#' Character vector. The excluded DataIDs. It may be empty.
#' @param vec_reference_names
#' Character vector. TRY names (`DataName`) of the reference variables,
#' each given once per observation, for example `"Reference / source"`.
#' @param vec_coordinate_names
#' Character vector. TRY names of the coordinate variables, `"Latitude"`
#' and `"Longitude"`.
#' @return
#' A tibble with one row per kept trait record, in input order, and these
#' columns in this order: `name` (the trait ID), `dataset`,
#' `acc_species_name`, `observation_id`, `obs_data_id`, `orig_value_str`,
#' `orig_unit_str`, `value_kind_name`, `orig_obs_data_id`, `std_value`,
#' `unit_name`, `record_reference`, one column per reference variable
#' (names cleaned by `janitor::clean_names()`, for example
#' `reference_source`), `trait_domain`, `trait_full_name`, the columns of
#' [prepare_try_coordinates()] (`latitude`, `longitude`, `latitude_source`,
#' `longitude_source`), and the canonical columns `source_provider`
#' (`"TRY"`), `source_dataset_key` (the dataset), `source_record_id` (the
#' observation ID as text), `source_trait_id`, `source_trait_name` (the
#' TRY trait name), `original_value_string`, `original_unit`,
#' `source_standardized_value` (TRY's `StdValue` as a number),
#' `source_standardized_unit` and `derivation_rule_key`.
#' @details
#' Records of an excluded DataID and records without an original value are
#' removed; [summarise_try_dropped_counts()] counts them. Repeated values of
#' one trait in an observation are kept; they are resolved after the unit
#' contract. A reference variable given twice for an observation keeps its
#' first value. Observations without references or coordinates keep their
#' records, with those columns missing. An empty `Reference` is missing.
#' `record_reference` is TRY's `Reference`, the citation of the publication
#' behind each record.
#' @seealso [prepare_try_coordinates()], [resolve_trait_unit_contract()]
#' @export
prepare_try_contract_input <- function(
  data_trait_rows,
  data_ancillary,
  data_trait_selection,
  vec_data_ids_excluded,
  vec_reference_names,
  vec_coordinate_names
) {
  vec_trait_columns <-
    c(
      "Dataset", "AccSpeciesName", "ObservationID", "ObsDataID", "DataID",
      "OrigValueStr", "OrigUnitStr", "ValueKindName", "OrigObsDataID",
      "StdValue", "UnitName", "Reference", "source_trait_id"
    )

  vec_ancillary_columns <-
    c(
      "Dataset", "AccSpeciesName", "ObservationID", "DataName",
      "OrigValueStr", "StdValue"
    )

  vec_selection_columns <-
    c("trait_id", "trait_domain", "trait_full_name", "derivation_rule_key")

  list_inputs <-
    list(
      data_trait_rows = list(data_trait_rows, vec_trait_columns),
      data_ancillary = list(data_ancillary, vec_ancillary_columns),
      data_trait_selection = list(data_trait_selection, vec_selection_columns)
    )

  purrr::iwalk(
    .x = list_inputs,
    .f = ~ assertthat::assert_that(
      base::is.data.frame(.x[[1]]),
      base::all(.x[[2]] %in% base::names(.x[[1]])),
      msg = stringr::str_c(
        "'", .y, "' must be a data frame with the columns: ",
        stringr::str_c(.x[[2]], collapse = ", "),
        "."
      )
    )
  )

  assertthat::assert_that(
    base::is.character(vec_data_ids_excluded),
    msg = "'vec_data_ids_excluded' must be a character vector."
  )

  assertthat::assert_that(
    base::is.character(vec_reference_names),
    base::length(vec_reference_names) > 0L,
    msg = "'vec_reference_names' must be a non-empty character vector."
  )

  assertthat::assert_that(
    base::is.character(vec_coordinate_names),
    base::length(vec_coordinate_names) > 0L,
    msg = "'vec_coordinate_names' must be a non-empty character vector."
  )

  # Repeated values of one trait within an observation are kept here and
  #   resolved after the unit contract (resolve_repeated_try_values())
  data_trait_values <-
    data_trait_rows |>
    dplyr::filter(
      !base::as.character(.data$DataID) %in% vec_data_ids_excluded,
      !base::is.na(.data$OrigValueStr)
    ) |>
    dplyr::mutate(name = .data$source_trait_id) |>
    dplyr::select(
      "name",
      "Dataset",
      "AccSpeciesName",
      "ObservationID",
      "ObsDataID",
      "OrigValueStr",
      "OrigUnitStr",
      "ValueKindName",
      "OrigObsDataID",
      "StdValue",
      "UnitName",
      record_reference = "Reference"
    )

  # Every reference column exists even when no observation has a value
  data_reference_template <-
    vec_reference_names |>
    rlang::set_names() |>
    purrr::map(.f = ~ base::character(0)) |>
    tibble::as_tibble()

  # One row of references per observation; the first value is kept when a
  #   reference is given twice
  data_trait_references <-
    dplyr::bind_rows(
      data_reference_template,
      data_ancillary |>
        dplyr::filter(
          .data$DataName %in% vec_reference_names,
          !base::is.na(.data$OrigValueStr)
        ) |>
        dplyr::distinct(
          .data$Dataset,
          .data$AccSpeciesName,
          .data$ObservationID,
          .data$DataName,
          .keep_all = TRUE
        ) |>
        dplyr::select(
          "Dataset", "AccSpeciesName", "ObservationID", "DataName",
          "OrigValueStr"
        ) |>
        tidyr::pivot_wider(
          names_from = "DataName",
          values_from = "OrigValueStr"
        )
    )

  # Observations without any ancillary data (no location or reference) are
  #   kept; their ancillary columns stay missing
  data_trait_merged <-
    dplyr::left_join(
      data_trait_values,
      data_trait_references,
      by = c("Dataset", "AccSpeciesName", "ObservationID"),
      relationship = "many-to-one"
    )

  # Numeric WGS84 coordinates, preferring TRY's standardised values
  data_trait_coordinates <-
    data_ancillary |>
    dplyr::filter(.data$DataName %in% vec_coordinate_names) |>
    janitor::clean_names() |>
    prepare_try_coordinates()

  # The derivation rule is renamed for the join and added last, as a new
  #   canonical column
  data_selection <-
    data_trait_selection |>
    dplyr::select(
      "trait_id",
      "trait_domain",
      "trait_full_name",
      selection_derivation_rule_key = "derivation_rule_key"
    )

  res <-
    data_trait_merged |>
    dplyr::left_join(
      data_selection,
      by = c("name" = "trait_id")
    ) |>
    janitor::clean_names() |>
    dplyr::select(-dplyr::any_of(c("latitude", "longitude"))) |>
    dplyr::left_join(
      data_trait_coordinates,
      by = c("dataset", "acc_species_name", "observation_id"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      source_provider = "TRY",
      source_dataset_key = .data$dataset,
      source_record_id = base::as.character(.data$observation_id),
      source_trait_id = base::as.character(.data$name),
      source_trait_name = .data$trait_full_name,
      original_value_string = .data$orig_value_str,
      original_unit = .data$orig_unit_str,
      source_standardized_value = prepare_trait_numeric(.data$std_value),
      source_standardized_unit = .data$unit_name,
      record_reference = dplyr::na_if(.data$record_reference, ""),
      derivation_rule_key = .data$selection_derivation_rule_key
    ) |>
    dplyr::select(-"selection_derivation_rule_key")

  return(res)
}
