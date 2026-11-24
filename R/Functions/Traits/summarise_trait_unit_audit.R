#' @title Summarise the trait-unit audit
#' @description Count records and summarise final values by audit fields.
#' @param data_source A canonical trait data frame containing audit fields.
#' @return An ungrouped tibble of counts and distribution summaries.
#' @details All-missing groups receive missing distribution summaries.
#' @export
summarise_trait_unit_audit <- function(data_source) {
  res <-
    data_source |>
    dplyr::group_by(
      .data$source_provider,
      .data$source_dataset_key,
      .data$source_trait_id,
      .data$source_trait_name,
      .data$original_unit,
      .data$normalized_original_unit,
      .data$conversion_rule_key,
      .data$derivation_rule_key,
      .data$trait_unit,
      .data$conversion_status,
      .data$disposition,
      .data$disposition_reason
    ) |>
    dplyr::summarise(
      n_records = dplyr::n(),
      value_min = if (
        base::all(base::is.na(.data$trait_value))
      ) {
        NA_real_
      } else {
        base::min(.data$trait_value, na.rm = TRUE)
      },
      value_q25 = if (
        base::all(base::is.na(.data$trait_value))
      ) {
        NA_real_
      } else {
        stats::quantile(
          .data$trait_value,
          probs = 0.25,
          na.rm = TRUE,
          names = FALSE
        )
      },
      value_median = if (
        base::all(base::is.na(.data$trait_value))
      ) {
        NA_real_
      } else {
        stats::median(.data$trait_value, na.rm = TRUE)
      },
      value_q75 = if (
        base::all(base::is.na(.data$trait_value))
      ) {
        NA_real_
      } else {
        stats::quantile(
          .data$trait_value,
          probs = 0.75,
          na.rm = TRUE,
          names = FALSE
        )
      },
      value_max = if (
        base::all(base::is.na(.data$trait_value))
      ) {
        NA_real_
      } else {
        base::max(.data$trait_value, na.rm = TRUE)
      },
      .groups = "drop"
    )

  return(res)
}
