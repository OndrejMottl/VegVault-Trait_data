data_reconciliation_fixture <-
  tibble::tibble(
    source_provider = c("TRY", "TRY", "TRY"),
    source_record_id = c("record-1", "record-2", "record-1"),
    source_trait_id = c("3114", "3114", "3114"),
    conversion_status = c("identity", "unsupported", "identity"),
    disposition = c("published", "quarantined", "dropped"),
    disposition_reason = c(
      NA_character_,
      "original_unit_not_registered",
      "repeated_in_observation"
    ),
    trait_value = c(1, NA_real_, 2),
    trait_unit = c("mm2", NA_character_, "mm2"),
    conversion_rule_key = c("identity-mm2", NA_character_, "identity-mm2")
  )

data_dropped_counts_fixture <-
  tibble::tibble(
    source_provider = "TRY",
    source_trait_id = "3114",
    disposition_reason = "data_id_excluded",
    n_records = 4L
  )

testthat::test_that("trait records reconcile by status and disposition", {
  res <-
    validate_trait_unit_reconciliation(data_reconciliation_fixture)

  testthat::expect_named(
    res,
    c(
      "source_provider", "source_trait_id", "disposition",
      "conversion_status", "disposition_reason", "n_records"
    )
  )
  testthat::expect_equal(base::sum(dplyr::pull(res, n_records)), 3L)
  testthat::expect_setequal(
    dplyr::pull(res, disposition),
    c("published", "quarantined", "dropped")
  )
  testthat::expect_setequal(
    dplyr::pull(res, disposition_reason),
    c(
      NA_character_,
      "original_unit_not_registered",
      "repeated_in_observation"
    )
  )
})

testthat::test_that("invalid reconciliation contracts fail closed", {
  data_missing_column <-
    data_reconciliation_fixture |>
    dplyr::select(-"trait_unit")
  testthat::expect_error(
    validate_trait_unit_reconciliation(data_missing_column),
    "invalid contract"
  )

  data_missing_reason <-
    data_reconciliation_fixture |>
    dplyr::select(-"disposition_reason")
  testthat::expect_error(
    validate_trait_unit_reconciliation(data_missing_reason),
    "invalid contract"
  )

  data_missing_disposition <-
    data_reconciliation_fixture |>
    dplyr::mutate(disposition = NA_character_)
  testthat::expect_error(
    validate_trait_unit_reconciliation(data_missing_disposition),
    "exactly one disposition"
  )

  data_unknown_disposition <-
    data_reconciliation_fixture |>
    dplyr::mutate(disposition = "kept")
  testthat::expect_error(
    validate_trait_unit_reconciliation(data_unknown_disposition),
    "published, quarantined or dropped"
  )

  data_invalid_published <-
    data_reconciliation_fixture |>
    dplyr::mutate(
      trait_value = dplyr::if_else(
        .data$disposition == "published",
        NA_real_,
        .data$trait_value
      )
    )
  testthat::expect_error(
    validate_trait_unit_reconciliation(data_invalid_published),
    "finite values"
  )

  data_duplicate <-
    data_reconciliation_fixture |>
    dplyr::slice(c(1L, 1L))
  testthat::expect_error(
    validate_trait_unit_reconciliation(data_duplicate),
    "must be unique"
  )
})

testthat::test_that("records dropped before the contract are counted", {
  res <-
    validate_trait_unit_reconciliation(
      data_source = data_reconciliation_fixture,
      data_dropped_counts = data_dropped_counts_fixture
    )

  data_dropped_before <-
    res |>
    dplyr::filter(.data$disposition_reason == "data_id_excluded")

  testthat::expect_equal(base::sum(dplyr::pull(res, n_records)), 7L)
  testthat::expect_equal(dplyr::pull(data_dropped_before, n_records), 4L)
  testthat::expect_equal(
    dplyr::pull(data_dropped_before, disposition),
    "dropped"
  )
  testthat::expect_true(
    base::is.na(dplyr::pull(data_dropped_before, conversion_status))
  )

  testthat::expect_error(
    validate_trait_unit_reconciliation(
      data_source = data_reconciliation_fixture,
      data_dropped_counts = dplyr::select(
        data_dropped_counts_fixture,
        -"disposition_reason"
      )
    ),
    "data_dropped_counts"
  )
})

testthat::test_that("raw counts must balance the reconciliation", {
  data_raw_counts_balanced <-
    tibble::tibble(
      source_provider = "TRY",
      source_trait_id = "3114",
      n_raw = 7L
    )

  res <-
    validate_trait_unit_reconciliation(
      data_source = data_reconciliation_fixture,
      data_raw_counts = data_raw_counts_balanced,
      data_dropped_counts = data_dropped_counts_fixture
    )

  testthat::expect_equal(base::sum(dplyr::pull(res, n_records)), 7L)

  testthat::expect_error(
    validate_trait_unit_reconciliation(
      data_source = data_reconciliation_fixture,
      data_raw_counts = dplyr::mutate(data_raw_counts_balanced, n_raw = 8L),
      data_dropped_counts = data_dropped_counts_fixture
    ),
    "do not balance"
  )

  testthat::expect_error(
    validate_trait_unit_reconciliation(
      data_source = data_reconciliation_fixture,
      data_raw_counts = dplyr::bind_rows(
        data_raw_counts_balanced,
        tibble::tibble(
          source_provider = "TRY",
          source_trait_id = "26",
          n_raw = 1L
        )
      ),
      data_dropped_counts = data_dropped_counts_fixture
    ),
    "do not balance"
  )

  testthat::expect_error(
    validate_trait_unit_reconciliation(
      data_source = data_reconciliation_fixture,
      data_raw_counts = dplyr::select(data_raw_counts_balanced, -"n_raw")
    ),
    "data_raw_counts"
  )
})
