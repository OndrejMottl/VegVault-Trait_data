#' @title Validate known TRY Leaf Area corrections
#' @description Check the fixed baseline of dataset-specific Leaf Area records.
#' @param data_source A canonical TRY trait data frame after unit conversion.
#' @return Invisibly returns `TRUE` when every baseline check succeeds.
#' @details Exactly 3,632 records must be published in mm2 after one conversion.
#' @export
validate_known_try_leaf_area <- function(data_source) {
  vec_known_rules <-
    c("try_leaf_area_below_cm2", "try_leaf_area_alpine_cm2")
  data_known <-
    data_source %>%
    dplyr::filter(.data$conversion_rule_key %in% vec_known_rules)

  if (nrow(data_known) != 3632L) {
    cli::cli_abort("Known TRY Leaf Area corrections must contain 3,632 rows.")
  }
  if (!all(data_known$disposition == "published")) {
    cli::cli_abort("Known TRY Leaf Area corrections must all be published.")
  }
  if (!all(data_known$trait_unit == "mm2")) {
    cli::cli_abort("Known TRY Leaf Area corrections must end in mm2.")
  }
  if (!isTRUE(all.equal(
    data_known$trait_value,
    data_known$original_value * 100,
    tolerance = 0
  ))) {
    cli::cli_abort(
      "Known TRY Leaf Area values were not converted exactly once."
    )
  }

  res <- TRUE

  return(base::invisible(res))
}
