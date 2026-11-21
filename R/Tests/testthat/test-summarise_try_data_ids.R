testthat::test_that("summarise_try_data_ids() validates its input", {
  testthat::expect_error(
    summarise_try_data_ids(data_trait_rows = "TraitID"),
    "data_trait_rows"
  )
  testthat::expect_error(
    summarise_try_data_ids(
      data_trait_rows = tibble::tibble(TraitID = 4L, DataID = 10L)
    ),
    "DataName"
  )
})

testthat::test_that("summarise_try_data_ids() counts records per DataID", {
  data_trait_rows <-
    tibble::tibble(
      TraitID = c(3106L, 4L, 3106L, 3106L, 4L),
      DataID = c(2221L, 10L, 6L, 2221L, 10L),
      DataName = c(
        "height at 15 days",
        "Wood density",
        "Plant height",
        "height at 15 days",
        "Wood density"
      ),
      OrigValueStr = c("1", "0.5", "2", "1.1", "0.6")
    )

  res <-
    summarise_try_data_ids(data_trait_rows = data_trait_rows)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("trait_id", "data_id", "data_name", "n_records")
  )
  testthat::expect_identical(dplyr::pull(res, "trait_id"), c(4L, 3106L, 3106L))
  testthat::expect_identical(dplyr::pull(res, "data_id"), c(10L, 6L, 2221L))
  testthat::expect_identical(
    dplyr::pull(res, "data_name"),
    c("Wood density", "Plant height", "height at 15 days")
  )
  testthat::expect_identical(dplyr::pull(res, "n_records"), c(2L, 1L, 2L))
  testthat::expect_equal(
    base::sum(dplyr::pull(res, "n_records")),
    base::nrow(data_trait_rows)
  )
})

testthat::test_that("summarise_try_data_ids() keeps missing values", {
  data_trait_rows <-
    tibble::tibble(
      TraitID = c(4L, 4L),
      DataID = c(10L, NA),
      DataName = c("Wood density", NA)
    )

  res <-
    summarise_try_data_ids(data_trait_rows = data_trait_rows)

  testthat::expect_equal(base::nrow(res), 2L)
  testthat::expect_identical(dplyr::pull(res, "data_id"), c(10L, NA))
  testthat::expect_identical(dplyr::pull(res, "n_records"), c(1L, 1L))
})

testthat::test_that("summarise_try_data_ids() handles a table without rows", {
  data_trait_rows <-
    tibble::tibble(
      TraitID = base::integer(0),
      DataID = base::integer(0),
      DataName = base::character(0)
    )

  res <-
    summarise_try_data_ids(data_trait_rows = data_trait_rows)

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(
    res,
    c("trait_id", "data_id", "data_name", "n_records")
  )
})
