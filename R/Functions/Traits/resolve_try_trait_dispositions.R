#' @title Resolve the dispositions of TRY records after the unit contract
#' @description
#' Apply the TRY rules that follow the unit contract, in their fixed order:
#' the check of the known Leaf Area corrections, the reviewed source
#' quarantines, one value per observation and trait, and the removal of
#' records that TRY flags as copies.
#' @param data_contract
#' TRY records from [resolve_trait_unit_contract()].
#' @param data_quarantine
#' The reviewed source quarantine list, with `source_provider`,
#' `source_trait_id`, `source_dataset_key` and `disposition_reason`.
#' @return
#' `data_contract` with the same rows and columns, and the `disposition`
#' and `disposition_reason` that the rules set.
#' @details
#' The steps are [validate_known_try_leaf_area()] (on the contract output,
#' aborting when the known corrections are not found),
#' [resolve_source_quarantines()], [resolve_repeated_try_values()] and
#' [resolve_try_duplicate_records()]. The order matters: a quarantined value
#' is not a candidate for the repeated-value rule, and a record is a copy
#' only of a published original.
#' @export
resolve_try_trait_dispositions <- function(
  data_contract,
  data_quarantine
) {
  assertthat::assert_that(
    base::is.data.frame(data_contract),
    msg = "'data_contract' must be a data frame."
  )

  assertthat::assert_that(
    base::is.data.frame(data_quarantine),
    msg = "'data_quarantine' must be a data frame."
  )

  # The known Leaf Area corrections are checked on the contract output,
  #   before any rule changes a disposition
  validate_known_try_leaf_area(data_source = data_contract)

  res <-
    data_contract |>
    resolve_source_quarantines(data_quarantine = data_quarantine) |>
    resolve_repeated_try_values() |>
    resolve_try_duplicate_records()

  return(res)
}
