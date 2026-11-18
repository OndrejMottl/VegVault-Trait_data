data_reconciliation_fixture <-
  tibble::tibble(
    source_provider = c("TRY", "TRY"),
    source_record_id = c("record-1", "record-2"),
    source_trait_id = c("3114", "3114"),
    conversion_status = c("identity", "unsupported"),
    disposition = c("published", "quarantined"),
    trait_value = c(1, NA_real_),
    trait_unit = c("mm2", NA_character_),
    conversion_rule_key = c("identity-mm2", NA_character_)
  )

testthat::test_that("valid trait records reconcile by status and disposition", {
  data_reconciliation_observed <-
    validate_trait_unit_reconciliation(data_reconciliation_fixture)

  testthat::expect_equal(sum(data_reconciliation_observed$n_records), 2L)
  testthat::expect_setequal(
    data_reconciliation_observed$disposition,
    c("published", "quarantined")
  )
})

testthat::test_that("invalid reconciliation contracts fail closed", {
  data_reconciliation_missing_column <-
    data_reconciliation_fixture |>
    dplyr::select(-"trait_unit")
  testthat::expect_error(
    validate_trait_unit_reconciliation(data_reconciliation_missing_column),
    "invalid contract"
  )

  data_reconciliation_missing_disposition <-
    data_reconciliation_fixture |>
    dplyr::mutate(disposition = NA_character_)
  testthat::expect_error(
    validate_trait_unit_reconciliation(data_reconciliation_missing_disposition),
    "exactly one disposition"
  )

  data_reconciliation_invalid_published <-
    data_reconciliation_fixture |>
    dplyr::mutate(
      trait_value = dplyr::if_else(
        .data$disposition == "published",
        NA_real_,
        .data$trait_value
      )
    )
  testthat::expect_error(
    validate_trait_unit_reconciliation(data_reconciliation_invalid_published),
    "finite values"
  )

  data_reconciliation_duplicate <-
    data_reconciliation_fixture |>
    dplyr::slice(c(1L, 1L))
  testthat::expect_error(
    validate_trait_unit_reconciliation(data_reconciliation_duplicate),
    "must be unique"
  )
})
