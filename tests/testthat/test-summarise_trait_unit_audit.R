testthat::test_that("audit summaries retain distributions and all-NA groups", {
  data_audit_fixture <-
    tibble::tibble(
      source_provider = "TRY",
      source_dataset_key = c("published", "published", "quarantine"),
      source_trait_id = "3114",
      source_trait_name = "Leaf area",
      original_unit = c("mm2", "mm2", "unknown"),
      normalized_original_unit = c("mm2", "mm2", NA_character_),
      conversion_rule_key = c("identity", "identity", NA_character_),
      derivation_rule_key = NA_character_,
      trait_unit = c("mm2", "mm2", NA_character_),
      conversion_status = c("identity", "identity", "unsupported"),
      disposition = c("published", "published", "quarantined"),
      disposition_reason = c(NA_character_, NA_character_, "unsupported"),
      trait_value = c(1, 3, NA_real_)
    )

  data_audit_summary <-
    testthat::expect_no_warning(
      summarise_trait_unit_audit(data_audit_fixture)
    )

  data_published_summary <-
    data_audit_summary |>
    dplyr::filter(.data$disposition == "published")
  data_quarantine_summary <-
    data_audit_summary |>
    dplyr::filter(.data$disposition == "quarantined")

  testthat::expect_equal(data_published_summary$n_records, 2L)
  testthat::expect_equal(data_published_summary$value_min, 1)
  testthat::expect_equal(data_published_summary$value_median, 2)
  testthat::expect_equal(data_published_summary$value_max, 3)
  testthat::expect_true(is.na(data_quarantine_summary$value_min))
  testthat::expect_true(is.na(data_quarantine_summary$value_median))
  testthat::expect_true(is.na(data_quarantine_summary$value_max))
})
