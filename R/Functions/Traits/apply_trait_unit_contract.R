#' @title Apply the canonical trait-unit contract
#' @description Standardise source values and derive registered trait values.
#' @param data_source A data frame containing source trait records and IDs.
#' @param registry A list of alias, conversion, and derivation registries.
#' @return A tibble with source fields, final values, units, and audit data.
#'   Columns the contract does not use, such as provider references, are
#'   returned unchanged.
#' @details Rules fail closed; invalid or unexplained records are quarantined.
#' @export
apply_trait_unit_contract <- function(
    data_source,
    registry = read_trait_unit_registry()) {
  vec_required_columns <-
    c(
      "source_provider",
      "source_dataset_key",
      "source_record_id",
      "source_trait_id",
      "source_trait_name",
      "original_value_string",
      "original_unit",
      "source_standardized_value",
      "source_standardized_unit",
      "trait_domain",
      "trait_full_name",
      "derivation_rule_key"
    )

  vec_missing_columns <-
    base::setdiff(vec_required_columns, base::names(data_source))

  if (
    base::length(vec_missing_columns) > 0L
  ) {
    cli::cli_abort(
      "Canonical trait input is missing: {vec_missing_columns}."
    )
  }

  if (
    "trait_unit_contract_applied" %in% base::names(data_source) &&
    base::any(data_source$trait_unit_contract_applied, na.rm = TRUE)
  ) {
    cli::cli_abort("The trait-unit contract cannot be applied twice.")
  }

  data_prepared <-
    data_source |>
    dplyr::mutate(
      trait_unit_row_id = dplyr::row_number(),
      source_provider = base::as.character(.data$source_provider),
      source_dataset_key = base::as.character(.data$source_dataset_key),
      source_record_id = base::as.character(.data$source_record_id),
      source_trait_id = base::as.character(.data$source_trait_id),
      original_value = parse_trait_numeric(.data$original_value_string)
    ) |>
    dplyr::left_join(
      registry$aliases,
      by = dplyr::join_by(original_unit)
    )

  data_specific_rules <-
    registry$conversions |>
    dplyr::filter(!base::is.na(.data$source_dataset_key))

  data_general_rules <-
    registry$conversions |>
    dplyr::filter(base::is.na(.data$source_dataset_key)) |>
    dplyr::select(-"source_dataset_key")

  data_specific_matches <-
    data_prepared |>
    dplyr::left_join(
      data_specific_rules,
      by = dplyr::join_by(
        source_provider,
        source_trait_id,
        source_dataset_key,
        normalized_original_unit
      )
    )

  data_unmatched <-
    data_specific_matches |>
    dplyr::filter(base::is.na(.data$conversion_rule_key)) |>
    dplyr::select(
      -dplyr::any_of(
        base::setdiff(
          base::names(registry$conversions),
          c(
            "source_provider",
            "source_trait_id",
            "source_dataset_key",
            "normalized_original_unit"
          )
        )
      )
    ) |>
    dplyr::left_join(
      data_general_rules,
      by = dplyr::join_by(
        source_provider,
        source_trait_id,
        normalized_original_unit
      )
    )

  data_matched <-
    dplyr::bind_rows(
      data_specific_matches |>
        dplyr::filter(!base::is.na(.data$conversion_rule_key)),
      data_unmatched
    ) |>
    dplyr::arrange(.data$trait_unit_row_id)

  data_converted <-
    data_matched |>
    dplyr::mutate(
      converted_value = dplyr::case_when(
        .data$operation == "identity" ~ .data$original_value,
        .data$operation == "multiply" ~
          .data$original_value * .data$multiplier,
        .data$operation == "reciprocal" & .data$original_value != 0 ~
          .data$multiplier / .data$original_value,
        TRUE ~ NA_real_
      ),
      source_value_matches = dplyr::case_when(
        base::is.na(.data$source_standardized_value) ~ TRUE,
        !.data$verify_source_standardized ~ TRUE,
        base::is.na(.data$converted_value) ~ FALSE,
        TRUE ~
          base::abs(
            .data$converted_value - .data$source_standardized_value
          ) <= .data$verification_absolute_tolerance
      ),
      source_unit_matches = dplyr::case_when(
        base::is.na(.data$source_standardized_unit) ~ TRUE,
        !.data$verify_source_standardized ~ TRUE,
        TRUE ~
          base::trimws(.data$source_standardized_unit) ==
          .data$standardized_unit
      ),
      conversion_status = dplyr::case_when(
        base::is.na(.data$original_value) ~ "failed",
        base::is.na(.data$original_unit) |
          !base::nzchar(base::trimws(.data$original_unit)) ~ "ambiguous",
        base::is.na(.data$normalized_original_unit) |
          base::is.na(.data$conversion_rule_key) ~ "unsupported",
        base::is.na(.data$converted_value) |
          !base::is.finite(.data$converted_value) ~ "failed",
        !.data$source_value_matches | !.data$source_unit_matches ~ "ambiguous",
        .data$operation == "identity" |
          (.data$operation == "multiply" & .data$multiplier == 1) ~
          "identity",
        TRUE ~ "converted"
      )
    )

  data_derivation_registry <-
    registry$derivations |>
    dplyr::rename(
      derivation_rule_version = "rule_version",
      derivation_input_unit = "input_unit",
      derivation_output_unit = "output_unit",
      derivation_operation = "operation",
      derivation_multiplier = "multiplier",
      derivation_dimension = "trait_dimension",
      derivation_evidence = "evidence"
    )

  data_derived <-
    data_converted |>
    dplyr::left_join(
      data_derivation_registry,
      by = dplyr::join_by(derivation_rule_key)
    ) |>
    dplyr::mutate(
      derivation_requested = !base::is.na(.data$derivation_rule_key),
      derivation_valid = dplyr::case_when(
        !.data$derivation_requested ~ TRUE,
        base::is.na(.data$derivation_operation) ~ FALSE,
        .data$derivation_input_unit != .data$standardized_unit ~ FALSE,
        !base::is.finite(.data$converted_value) |
          .data$converted_value <= 0 ~ FALSE,
        TRUE ~ TRUE
      ),
      trait_value = dplyr::case_when(
        !.data$derivation_requested ~ .data$converted_value,
        .data$derivation_valid & .data$derivation_operation == "reciprocal" ~
          .data$derivation_multiplier / .data$converted_value,
        TRUE ~ NA_real_
      ),
      trait_unit = dplyr::if_else(
        .data$derivation_requested,
        .data$derivation_output_unit,
        .data$standardized_unit
      ),
      conversion_status = dplyr::case_when(
        .data$conversion_status %in% c(
          "ambiguous",
          "unsupported",
          "failed"
        ) ~ .data$conversion_status,
        !.data$derivation_valid ~ "failed",
        .data$derivation_requested ~ "converted",
        TRUE ~ .data$conversion_status
      ),
      disposition = dplyr::if_else(
        .data$conversion_status %in% c("identity", "converted") &
          base::is.finite(.data$trait_value),
        "published",
        "quarantined"
      ),
      disposition_reason = dplyr::case_when(
        .data$disposition == "published" ~ NA_character_,
        base::is.na(.data$original_value) ~ "original_value_not_numeric",
        base::is.na(.data$original_unit) |
          !base::nzchar(base::trimws(.data$original_unit)) ~
          "original_unit_missing",
        base::is.na(.data$normalized_original_unit) ~
          "original_unit_not_registered",
        base::is.na(.data$conversion_rule_key) ~
          "conversion_rule_not_registered",
        !.data$source_value_matches | !.data$source_unit_matches ~
          "source_standardized_value_mismatch",
        !.data$derivation_valid ~ "derived_value_invalid",
        TRUE ~ "conversion_failed"
      ),
      trait_unit_contract_applied = TRUE
    ) |>
    dplyr::select(-"trait_unit_row_id")

  return(data_derived)
}
