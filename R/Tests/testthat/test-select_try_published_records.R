testthat::test_that("select_try_published_records() validates its input", {
  testthat::expect_error(
    select_try_published_records(data_resolved = "records"),
    "data_resolved"
  )
  testthat::expect_error(
    select_try_published_records(
      data_resolved = tibble::tibble(disposition = "published")
    ),
    "trait_value"
  )
})

testthat::test_that("select_try_published_records() keeps published rows", {
  data_resolved <-
    tibble::tibble(
      name = c("4", "4", "26"),
      dataset = c("D1", "D2", "D3"),
      observation_id = c(1L, 2L, 3L),
      orig_value_str = c("0.5", "0.7", "3"),
      trait_value = c(0.5, 0.7, 3),
      trait_unit = c("g/cm3", "g/cm3", "mg"),
      trait_domain = c("SSD", "SSD", "Diaspore mass"),
      trait_full_name = c("Wood", "Wood", "Seed"),
      disposition = c("published", "quarantined", "published")
    )

  res <-
    select_try_published_records(data_resolved = data_resolved)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c(
      "dataset", "trait_value", "trait_unit", "trait_domain",
      "trait_full_name", "observation_id", "orig_value_str", "disposition"
    )
  )
  testthat::expect_identical(dplyr::pull(res, "observation_id"), c(1L, 3L))
  testthat::expect_identical(
    base::unique(dplyr::pull(res, "disposition")),
    "published"
  )
})

testthat::test_that("select_try_published_records() handles no published", {
  data_resolved <-
    tibble::tibble(
      name = "4",
      observation_id = 1L,
      trait_value = 0.5,
      trait_unit = "g/cm3",
      trait_domain = "SSD",
      trait_full_name = "Wood",
      disposition = "dropped"
    )

  res <-
    select_try_published_records(data_resolved = data_resolved)

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_false("name" %in% base::names(res))
})
