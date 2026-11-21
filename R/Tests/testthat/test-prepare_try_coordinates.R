build_try_coordinate_fixture <- function(
  observation_id,
  data_name,
  orig_value_str,
  std_value
) {
  res <-
    tibble::tibble(
      dataset = "dataset",
      acc_species_name = "Species name",
      observation_id = observation_id,
      data_name = data_name,
      orig_value_str = orig_value_str,
      std_value = std_value
    )

  return(res)
}

testthat::test_that("prepare_try_coordinates() checks columns", {
  data_fixture <-
    build_try_coordinate_fixture(
      observation_id = 1,
      data_name = "Latitude",
      orig_value_str = "45",
      std_value = 45
    ) |>
    dplyr::select(-"std_value")

  testthat::expect_error(
    prepare_try_coordinates(data_fixture),
    "std_value"
  )
})

testthat::test_that("prepare_try_coordinates() returns one row per key", {
  data_fixture <-
    build_try_coordinate_fixture(
      observation_id = c(1, 1, 2, 2, 2),
      data_name = c(
        "Latitude", "Longitude", "Latitude", "Longitude", "Altitude"
      ),
      orig_value_str = c("45", "10", "-12.5", "130", "300"),
      std_value = c(45, 10, -12.5, 130, 300)
    )

  res <-
    prepare_try_coordinates(data_fixture)

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_named(
    res,
    c(
      "dataset", "acc_species_name", "observation_id", "latitude",
      "longitude", "latitude_source", "longitude_source"
    )
  )
  testthat::expect_equal(base::nrow(res), 2L)
  testthat::expect_type(dplyr::pull(res, latitude), "double")
  testthat::expect_equal(dplyr::pull(res, latitude), c(45, -12.5))
  testthat::expect_equal(dplyr::pull(res, longitude), c(10, 130))
})

testthat::test_that("prepare_try_coordinates() prefers TRY standard", {
  data_fixture <-
    build_try_coordinate_fixture(
      observation_id = c(1, 1, 2, 2),
      data_name = c("Latitude", "Longitude", "Latitude", "Longitude"),
      orig_value_str = c("45 30 N", "4552", "48.2", "16.4"),
      std_value = c(45.5, 45.8667, NA, NA)
    )

  res <-
    prepare_try_coordinates(data_fixture)

  testthat::expect_equal(dplyr::pull(res, latitude), c(45.5, 48.2))
  testthat::expect_equal(dplyr::pull(res, longitude), c(45.8667, 16.4))
  testthat::expect_equal(
    dplyr::pull(res, latitude_source),
    c("standardized", "original")
  )
})

testthat::test_that("prepare_try_coordinates() rejects unusable values", {
  data_fixture <-
    build_try_coordinate_fixture(
      observation_id = c(1, 1, 2, 2),
      data_name = c("Latitude", "Longitude", "Latitude", "Longitude"),
      orig_value_str = c("4530", "10", "45 N", "10"),
      std_value = c(NA, NA, NA, NA)
    )

  res <-
    prepare_try_coordinates(data_fixture)

  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, latitude))))
  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, longitude))))
  testthat::expect_equal(
    dplyr::pull(res, latitude_source),
    c("out_of_range", "not_parsable")
  )
  testthat::expect_equal(
    dplyr::pull(res, longitude_source),
    c("incomplete_pair", "incomplete_pair")
  )
})

testthat::test_that("prepare_try_coordinates() needs both coordinates", {
  data_fixture <-
    build_try_coordinate_fixture(
      observation_id = 1,
      data_name = "Latitude",
      orig_value_str = "45",
      std_value = 45
    )

  res <-
    prepare_try_coordinates(data_fixture)

  testthat::expect_true(base::is.na(dplyr::pull(res, latitude)))
  testthat::expect_true(base::is.na(dplyr::pull(res, longitude)))
  testthat::expect_equal(dplyr::pull(res, latitude_source), "incomplete_pair")
  testthat::expect_true(base::is.na(dplyr::pull(res, longitude_source)))
})

testthat::test_that("prepare_try_coordinates() rejects repeated rows", {
  data_fixture <-
    build_try_coordinate_fixture(
      observation_id = c(1, 1, 1),
      data_name = c("Latitude", "Latitude", "Longitude"),
      orig_value_str = c("45", "46", "10"),
      std_value = c(45, 46, 10)
    )

  testthat::expect_error(
    prepare_try_coordinates(data_fixture),
    "more than one"
  )
})
