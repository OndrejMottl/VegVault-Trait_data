testthat::test_that("summarise_trait_raw_counts() validates its arguments", {
  data_source <-
    tibble::tibble(trait_name = c("seed mass", "leaf area"))

  testthat::expect_error(
    summarise_trait_raw_counts(
      data_source = "seed mass",
      col_trait_id = "trait_name",
      source_provider = "BIEN"
    ),
    "data_source"
  )
  testthat::expect_error(
    summarise_trait_raw_counts(
      data_source = data_source,
      col_trait_id = "source_trait_id",
      source_provider = "BIEN"
    ),
    "col_trait_id"
  )
  testthat::expect_error(
    summarise_trait_raw_counts(
      data_source = data_source,
      col_trait_id = "trait_name",
      source_provider = c("BIEN", "TRY")
    ),
    "source_provider"
  )
})

testthat::test_that("summarise_trait_raw_counts() counts records per trait", {
  data_source <-
    tibble::tibble(
      trait_name = c("seed mass", "leaf area", "seed mass", NA),
      trait_value = c("1", "2", "3", "4")
    )

  res <-
    summarise_trait_raw_counts(
      data_source = data_source,
      col_trait_id = "trait_name",
      source_provider = "BIEN"
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, c("source_trait_id", "n_raw", "source_provider"))
  testthat::expect_identical(
    dplyr::pull(res, "source_trait_id"),
    c("leaf area", "seed mass", NA)
  )
  testthat::expect_identical(dplyr::pull(res, "n_raw"), c(1L, 2L, 1L))
  testthat::expect_identical(
    dplyr::pull(res, "source_provider"),
    base::rep("BIEN", 3L)
  )
  testthat::expect_equal(
    base::sum(dplyr::pull(res, "n_raw")),
    base::nrow(data_source)
  )
})

testthat::test_that("summarise_trait_raw_counts() handles no records", {
  res <-
    summarise_trait_raw_counts(
      data_source = tibble::tibble(source_trait_id = base::character(0)),
      col_trait_id = "source_trait_id",
      source_provider = "TRY"
    )

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(res, c("source_trait_id", "n_raw", "source_provider"))
})
