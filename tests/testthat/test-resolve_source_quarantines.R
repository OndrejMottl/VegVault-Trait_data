data_source_quarantine_fixture <-
  tibble::tibble(
    source_provider = c("BIEN", "BIEN", "BIEN", "TRY", "BIEN"),
    source_trait_id = c(
      "stem wood density", "stem wood density", "whole plant height",
      "stem wood density", "stem wood density"
    ),
    source_dataset_key = c(
      "El-Kassaby YA", "Other PI", "El-Kassaby YA", "El-Kassaby YA",
      "El-Kassaby YA"
    ),
    disposition = c(
      "published", "published", "published", "published", "dropped"
    ),
    disposition_reason = c(
      NA_character_, NA_character_, NA_character_, NA_character_,
      "species_name_missing"
    )
  )

data_quarantine_fixture <-
  tibble::tibble(
    source_provider = "BIEN",
    source_trait_id = "stem wood density",
    source_dataset_key = "El-Kassaby YA",
    disposition_reason = "source_scale_implausible"
  )

testthat::test_that("resolve_source_quarantines() checks columns", {
  testthat::expect_error(
    resolve_source_quarantines(
      data_source = dplyr::select(
        data_source_quarantine_fixture,
        -"source_dataset_key"
      ),
      data_quarantine = data_quarantine_fixture
    ),
    "source_dataset_key"
  )
  testthat::expect_error(
    resolve_source_quarantines(
      data_source = data_source_quarantine_fixture,
      data_quarantine = dplyr::select(
        data_quarantine_fixture,
        -"disposition_reason"
      )
    ),
    "disposition_reason"
  )
})

testthat::test_that("resolve_source_quarantines() matches all three keys", {
  res <-
    resolve_source_quarantines(
      data_source = data_source_quarantine_fixture,
      data_quarantine = data_quarantine_fixture
    )

  testthat::expect_named(res, base::names(data_source_quarantine_fixture))
  testthat::expect_equal(base::nrow(res), 5L)
  testthat::expect_equal(
    dplyr::pull(res, disposition),
    c("quarantined", "published", "published", "published", "dropped")
  )
  testthat::expect_equal(
    dplyr::pull(res, disposition_reason),
    c(
      "source_scale_implausible", NA_character_, NA_character_,
      NA_character_, "species_name_missing"
    )
  )
})

testthat::test_that("resolve_source_quarantines() accepts an empty list", {
  res <-
    resolve_source_quarantines(
      data_source = data_source_quarantine_fixture,
      data_quarantine = dplyr::slice(data_quarantine_fixture, 0L)
    )

  testthat::expect_equal(res, data_source_quarantine_fixture)
})
