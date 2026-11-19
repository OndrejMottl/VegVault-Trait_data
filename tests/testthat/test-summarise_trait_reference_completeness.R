data_reference_fixture <-
  tibble::tibble(
    source_provider = "TRY",
    trait_domain = c("Leaf Area", "Leaf Area", "Leaf Area", "Seed", "Seed"),
    record_reference = c("Ref A", NA, "  ", "Ref B", "Ref B"),
    reference_source = c(NA, NA, NA, "Src 1", "Src 2")
  )

testthat::test_that("summarise_trait_reference_completeness() checks input", {
  testthat::expect_error(
    summarise_trait_reference_completeness(
      data_source = "not a data frame",
      reference_columns = "record_reference"
    ),
    "data frame"
  )

  testthat::expect_error(
    summarise_trait_reference_completeness(
      data_source = data_reference_fixture,
      reference_columns = "missing_reference"
    ),
    "reference_columns"
  )

  testthat::expect_error(
    summarise_trait_reference_completeness(
      data_source = dplyr::select(data_reference_fixture, -"trait_domain"),
      reference_columns = "record_reference"
    ),
    "trait_domain"
  )

  testthat::expect_error(
    summarise_trait_reference_completeness(
      data_source = data_reference_fixture,
      reference_columns = base::character(0)
    ),
    "reference_columns"
  )
})

testthat::test_that("summarise_trait_reference_completeness() returns table", {
  res <-
    summarise_trait_reference_completeness(
      data_source = data_reference_fixture,
      reference_columns = c("record_reference", "reference_source")
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c(
      "source_provider", "trait_domain", "reference_column", "n_records",
      "n_present", "share_present", "n_distinct_present"
    )
  )
  testthat::expect_equal(base::nrow(res), 4L)
})

testthat::test_that("summarise_trait_reference_completeness() counts values", {
  res <-
    summarise_trait_reference_completeness(
      data_source = data_reference_fixture,
      reference_columns = c("record_reference", "reference_source")
    )

  testthat::expect_equal(
    dplyr::pull(res, reference_column),
    c("record_reference", "reference_source", "record_reference",
      "reference_source")
  )
  testthat::expect_equal(
    dplyr::pull(res, trait_domain),
    c("Leaf Area", "Leaf Area", "Seed", "Seed")
  )
  testthat::expect_equal(dplyr::pull(res, n_records), c(3L, 3L, 2L, 2L))
  testthat::expect_equal(dplyr::pull(res, n_present), c(1L, 0L, 2L, 2L))
  testthat::expect_equal(
    dplyr::pull(res, share_present),
    c(1 / 3, 0, 1, 1),
    tolerance = 1e-8
  )
  testthat::expect_equal(
    dplyr::pull(res, n_distinct_present),
    c(1L, 0L, 1L, 2L)
  )
})

testthat::test_that("summarise_trait_reference_completeness() keeps 0 rows", {
  res <-
    summarise_trait_reference_completeness(
      data_source = dplyr::slice(data_reference_fixture, 0),
      reference_columns = "record_reference"
    )

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(
    res,
    c(
      "source_provider", "trait_domain", "reference_column", "n_records",
      "n_present", "share_present", "n_distinct_present"
    )
  )
})
