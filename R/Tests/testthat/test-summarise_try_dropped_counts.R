testthat::test_that("summarise_try_dropped_counts() validates its arguments", {
  data_trait_rows <-
    tibble::tibble(
      source_trait_id = "4",
      DataID = 10L,
      OrigValueStr = "0.5"
    )

  testthat::expect_error(
    summarise_try_dropped_counts(
      data_trait_rows = dplyr::select(data_trait_rows, -"DataID"),
      vec_data_ids_excluded = "2221"
    ),
    "DataID"
  )
  testthat::expect_error(
    summarise_try_dropped_counts(
      data_trait_rows = data_trait_rows,
      vec_data_ids_excluded = 2221
    ),
    "vec_data_ids_excluded"
  )
})

testthat::test_that("summarise_try_dropped_counts() counts by reason", {
  data_trait_rows <-
    tibble::tibble(
      source_trait_id = c("4", "4", "3106", "3106", "3106", "26"),
      DataID = c(10L, 2221L, 2221L, 6L, 6L, 3698L),
      OrigValueStr = c("0.5", NA, "1", NA, "2", "3")
    )

  res <-
    summarise_try_dropped_counts(
      data_trait_rows = data_trait_rows,
      vec_data_ids_excluded = c("2221", "3698")
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("source_trait_id", "disposition_reason", "n_records", "source_provider")
  )
  testthat::expect_identical(
    dplyr::pull(res, "source_trait_id"),
    c("26", "3106", "3106", "4")
  )
  testthat::expect_identical(
    dplyr::pull(res, "disposition_reason"),
    c(
      "data_id_excluded",
      "data_id_excluded",
      "original_value_missing",
      "data_id_excluded"
    )
  )
  testthat::expect_identical(
    dplyr::pull(res, "n_records"),
    c(1L, 1L, 1L, 1L)
  )
  testthat::expect_identical(
    base::unique(dplyr::pull(res, "source_provider")),
    "TRY"
  )
})

testthat::test_that("summarise_try_dropped_counts() handles no exclusions", {
  data_trait_rows <-
    tibble::tibble(
      source_trait_id = c("4", "4"),
      DataID = c(10L, 2221L),
      OrigValueStr = c("0.5", "1")
    )

  res <-
    summarise_try_dropped_counts(
      data_trait_rows = data_trait_rows,
      vec_data_ids_excluded = character(0)
    )

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(
    res,
    c("source_trait_id", "disposition_reason", "n_records", "source_provider")
  )
})
