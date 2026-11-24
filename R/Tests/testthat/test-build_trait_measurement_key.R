testthat::test_that("build_trait_measurement_key() validates lengths", {
  testthat::expect_error(
    build_trait_measurement_key(
      trait_domain = c("Leaf Area", "Leaf Area"),
      species = "Abies alba",
      trait_value = c(1, 2)
    ),
    "same length"
  )
  testthat::expect_error(
    build_trait_measurement_key(
      trait_domain = "Leaf Area",
      species = "Abies alba",
      trait_value = "1"
    ),
    "numeric"
  )
})

testthat::test_that("build_trait_measurement_key() rounds to three digits", {
  res <-
    build_trait_measurement_key(
      trait_domain = c("Diaspore mass", "Diaspore mass", "Plant heigh"),
      species = c("Abies alba", "Abies alba", "Abies alba"),
      trait_value = c(12.34, 12.3449, 0.0012345)
    )

  testthat::expect_type(res, "character")
  testthat::expect_equal(
    res,
    c(
      "Diaspore mass|Abies alba|12.3",
      "Diaspore mass|Abies alba|12.3",
      "Plant heigh|Abies alba|0.00123"
    )
  )
})

testthat::test_that("build_trait_measurement_key() keeps missing keys", {
  res <-
    build_trait_measurement_key(
      trait_domain = c("Leaf Area", "Leaf Area"),
      species = c(NA_character_, "Abies alba"),
      trait_value = c(1, NA_real_)
    )

  testthat::expect_length(res, 2L)
  testthat::expect_true(base::all(base::is.na(res)))
})
