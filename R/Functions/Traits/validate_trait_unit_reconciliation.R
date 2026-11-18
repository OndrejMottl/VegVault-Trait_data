#' @title Validate and reconcile canonical trait records
#' @description Enforce publication rules and count records by audit status.
#' @param data_source A canonical trait data frame containing audit fields.
#' @return A tibble of counts by provider, trait, status, and disposition.
#' @details Published source record and trait identifiers must be unique.
#' @export
validate_trait_unit_reconciliation <- function(data_source) {
  vec_required_columns <-
    c(
      "source_provider",
      "source_record_id",
      "source_trait_id",
      "conversion_status",
      "disposition",
      "trait_value",
      "trait_unit",
      "conversion_rule_key"
    )

  if (
    !base::all(vec_required_columns %in% base::names(data_source))
  ) {
    cli::cli_abort("Trait-unit reconciliation received an invalid contract.")
  }

  if (
    base::any(base::is.na(data_source$disposition))
  ) {
    cli::cli_abort("Every trait record must have exactly one disposition.")
  }

  data_published <-
    data_source |>
    dplyr::filter(.data$disposition == "published")

  if (
    base::any(!base::is.finite(data_published$trait_value)) ||
    base::any(base::is.na(data_published$trait_unit)) ||
    base::any(base::is.na(data_published$conversion_rule_key))
  ) {
    cli::cli_abort(
      "Published trait records require finite values, units, and rules."
    )
  }

  data_duplicates <-
    data_published |>
    dplyr::count(
      .data$source_provider,
      .data$source_record_id,
      .data$source_trait_id,
      name = "n_records"
    ) |>
    dplyr::filter(.data$n_records > 1L)

  if (
    base::nrow(data_duplicates) > 0L
  ) {
    cli::cli_abort("Published source trait records must be unique.")
  }

  res <-
    data_source |>
    dplyr::count(
      .data$source_provider,
      .data$source_trait_id,
      .data$disposition,
      .data$conversion_status,
      name = "n_records"
    ) |>
    dplyr::arrange(
      .data$source_provider,
      .data$source_trait_id,
      .data$disposition,
      .data$conversion_status
    )

  return(res)
}
