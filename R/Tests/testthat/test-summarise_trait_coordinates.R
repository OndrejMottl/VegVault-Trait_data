testthat::test_that("summarise_trait_coordinates() checks its input", {
  data_without_source <-
    tibble::tibble(
      source_provider = "TRY",
      source_dataset_key = "a",
      latitude = 45,
      longitude = 10,
      latitude_source = "standardized"
    )

  testthat::expect_error(
    summarise_trait_coordinates(data_without_source),
    "longitude_source"
  )

  testthat::expect_error(
    summarise_trait_coordinates(base::list(source_provider = "TRY")),
    "data frame"
  )
})

testthat::test_that("summarise_trait_coordinates() counts each group", {
  data_fixture <-
    tibble::tibble(
      source_provider = c(
        "TRY", "TRY", "TRY", "TRY", "TRY", "TRY", "BIEN", "BIEN"
      ),
      source_dataset_key = c("a", "b", "b", "c", "c", "d", "e", NA),
      latitude = c(45, 46, 47, NA, NA, NA, 10, NA),
      longitude = c(10, 11, 12, NA, NA, NA, 20, NA),
      latitude_source = c(
        "standardized", "standardized", "original", "out_of_range",
        "out_of_range", NA, "original", NA
      ),
      longitude_source = c(
        "standardized", "standardized", "original", "incomplete_pair",
        "incomplete_pair", NA, "original", NA
      )
    )

  res <-
    summarise_trait_coordinates(data_fixture)

  data_expected <-
    tibble::tibble(
      source_provider = c("BIEN", "BIEN", "TRY", "TRY", "TRY", "TRY"),
      latitude_source = c(
        "original", NA, "original", "out_of_range", "standardized", NA
      ),
      longitude_source = c(
        "original", NA, "original", "incomplete_pair", "standardized", NA
      ),
      coordinates_kept = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
      n_records = c(1L, 1L, 1L, 2L, 2L, 1L),
      n_datasets = c(1L, 1L, 1L, 1L, 2L, 1L)
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(res, data_expected)
  testthat::expect_equal(
    base::sum(dplyr::pull(res, n_records)),
    base::nrow(data_fixture)
  )
})

testthat::test_that("summarise_trait_coordinates() reads kept values", {
  data_fixture <-
    tibble::tibble(
      source_provider = c("BIEN", "BIEN"),
      source_dataset_key = c("a", "a"),
      latitude = c(45, NA),
      longitude = c(10, 10),
      latitude_source = c("original", "original"),
      longitude_source = c("original", "original")
    )

  res <-
    summarise_trait_coordinates(data_fixture)

  testthat::expect_equal(base::nrow(res), 2L)
  testthat::expect_equal(
    dplyr::pull(res, coordinates_kept),
    c(FALSE, TRUE)
  )
  testthat::expect_equal(dplyr::pull(res, n_records), c(1L, 1L))
})

testthat::test_that("summarise_trait_coordinates() accepts zero rows", {
  data_fixture <-
    tibble::tibble(
      source_provider = base::character(0),
      source_dataset_key = base::character(0),
      latitude = base::numeric(0),
      longitude = base::numeric(0),
      latitude_source = base::character(0),
      longitude_source = base::character(0)
    )

  res <-
    summarise_trait_coordinates(data_fixture)

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(
    res,
    c(
      "source_provider", "latitude_source", "longitude_source",
      "coordinates_kept", "n_records", "n_datasets"
    )
  )
})

testthat::test_that("summarise_trait_coordinates() ignores other columns", {
  data_fixture <-
    tibble::tibble(
      source_provider = "TRY",
      source_dataset_key = "a",
      latitude = 45,
      longitude = 10,
      latitude_source = "original",
      longitude_source = "original",
      trait_value = 1.5,
      coordinates_kept = "not a flag"
    )

  res <-
    summarise_trait_coordinates(data_fixture)

  testthat::expect_named(
    res,
    c(
      "source_provider", "latitude_source", "longitude_source",
      "coordinates_kept", "n_records", "n_datasets"
    )
  )
  testthat::expect_equal(dplyr::pull(res, coordinates_kept), TRUE)
})
