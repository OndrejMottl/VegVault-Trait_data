testthat::test_that("prepare_bien_coordinates() checks its input", {
  data_without_longitude <-
    tibble::tibble(
      id = "1",
      latitude = 45
    )

  testthat::expect_error(
    prepare_bien_coordinates(data_without_longitude),
    "longitude"
  )

  testthat::expect_error(
    prepare_bien_coordinates(base::list(latitude = 45, longitude = 10)),
    "data frame"
  )

  data_text <-
    tibble::tibble(
      latitude = "45",
      longitude = "10"
    )

  testthat::expect_error(
    prepare_bien_coordinates(data_text),
    "numeric"
  )

  data_logical <-
    tibble::tibble(
      latitude = TRUE,
      longitude = 10
    )

  testthat::expect_error(
    prepare_bien_coordinates(data_logical),
    "numeric"
  )

  data_with_source <-
    tibble::tibble(
      latitude = 45,
      longitude = 10,
      latitude_source = "original"
    )

  testthat::expect_error(
    prepare_bien_coordinates(data_with_source),
    "latitude_source"
  )
})

testthat::test_that("prepare_bien_coordinates() keeps rows and columns", {
  data_fixture <-
    tibble::tibble(
      id = c("3", "1", "2"),
      latitude = c(45, NA, 91),
      trait_value = c(1.5, 2, 3),
      longitude = c(10, 20, 30)
    )

  res <-
    prepare_bien_coordinates(data_fixture)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c(
      "id", "latitude", "trait_value", "longitude", "latitude_source",
      "longitude_source"
    )
  )
  testthat::expect_equal(base::nrow(res), 3L)
  testthat::expect_equal(dplyr::pull(res, id), c("3", "1", "2"))
  testthat::expect_equal(dplyr::pull(res, trait_value), c(1.5, 2, 3))
  testthat::expect_type(dplyr::pull(res, latitude_source), "character")
  testthat::expect_type(dplyr::pull(res, longitude_source), "character")
})

testthat::test_that("prepare_bien_coordinates() keeps values in range", {
  data_fixture <-
    tibble::tibble(
      latitude = c(-90, 90, 0, 45.25),
      longitude = c(-180, 180, 0, -122.5)
    )

  res <-
    prepare_bien_coordinates(data_fixture)

  testthat::expect_equal(dplyr::pull(res, latitude), c(-90, 90, 0, 45.25))
  testthat::expect_equal(dplyr::pull(res, longitude), c(-180, 180, 0, -122.5))
  testthat::expect_equal(
    dplyr::pull(res, latitude_source),
    base::rep("original", 4L)
  )
  testthat::expect_equal(
    dplyr::pull(res, longitude_source),
    base::rep("original", 4L)
  )
})

testthat::test_that("prepare_bien_coordinates() sets out of range missing", {
  data_fixture <-
    tibble::tibble(
      latitude = c(91, 45, Inf, -90.5),
      longitude = c(10, -180.5, 10, -Inf)
    )

  res <-
    prepare_bien_coordinates(data_fixture)

  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, latitude))))
  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, longitude))))
  testthat::expect_equal(
    dplyr::pull(res, latitude_source),
    c("out_of_range", "incomplete_pair", "out_of_range", "out_of_range")
  )
  testthat::expect_equal(
    dplyr::pull(res, longitude_source),
    c("incomplete_pair", "out_of_range", "incomplete_pair", "out_of_range")
  )
})

testthat::test_that("prepare_bien_coordinates() needs both coordinates", {
  data_fixture <-
    tibble::tibble(
      latitude = c(45, NA),
      longitude = c(NA, 10)
    )

  res <-
    prepare_bien_coordinates(data_fixture)

  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, latitude))))
  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, longitude))))
  testthat::expect_equal(
    dplyr::pull(res, latitude_source),
    c("incomplete_pair", NA)
  )
  testthat::expect_equal(
    dplyr::pull(res, longitude_source),
    c(NA, "incomplete_pair")
  )
})

testthat::test_that("prepare_bien_coordinates() keeps absent ones missing", {
  data_fixture <-
    tibble::tibble(
      latitude = c(NA, NaN),
      longitude = c(NA, NA)
    )

  res <-
    prepare_bien_coordinates(data_fixture)

  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, latitude))))
  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, longitude))))
  testthat::expect_equal(
    dplyr::pull(res, latitude_source),
    c(NA_character_, NA_character_)
  )
  testthat::expect_equal(
    dplyr::pull(res, longitude_source),
    c(NA_character_, NA_character_)
  )
})

testthat::test_that("prepare_bien_coordinates() returns double values", {
  data_fixture <-
    tibble::tibble(
      latitude = c(45L, 100L),
      longitude = c(10L, 10L)
    )

  res <-
    prepare_bien_coordinates(data_fixture)

  testthat::expect_type(dplyr::pull(res, latitude), "double")
  testthat::expect_type(dplyr::pull(res, longitude), "double")
  testthat::expect_equal(dplyr::pull(res, latitude), c(45, NA))
  testthat::expect_equal(dplyr::pull(res, longitude), c(10, NA))
})

testthat::test_that("prepare_bien_coordinates() accepts zero rows", {
  data_fixture <-
    tibble::tibble(
      latitude = base::numeric(0),
      longitude = base::numeric(0)
    )

  res <-
    prepare_bien_coordinates(data_fixture)

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(
    res,
    c("latitude", "longitude", "latitude_source", "longitude_source")
  )
  testthat::expect_type(dplyr::pull(res, latitude_source), "character")
})

testthat::test_that("prepare_bien_coordinates() handles many records", {
  base::set.seed(900723)

  data_fixture <-
    tibble::tibble(
      latitude = stats::runif(n = 10000, min = -100, max = 100),
      longitude = stats::runif(n = 10000, min = -200, max = 200)
    )

  res <-
    prepare_bien_coordinates(data_fixture)

  vec_kept <-
    base::abs(dplyr::pull(data_fixture, latitude)) <= 90 &
    base::abs(dplyr::pull(data_fixture, longitude)) <= 180

  testthat::expect_equal(base::nrow(res), 10000L)
  testthat::expect_equal(!base::is.na(dplyr::pull(res, latitude)), vec_kept)
  testthat::expect_equal(!base::is.na(dplyr::pull(res, longitude)), vec_kept)
  testthat::expect_equal(
    base::sum(dplyr::pull(res, latitude_source) %in% "original"),
    base::sum(vec_kept)
  )
})
