testthat::test_that("is_bien_species_missing() validates its input", {
  testthat::expect_error(
    is_bien_species_missing(vec_species = 1),
    "vec_species"
  )
  testthat::expect_error(
    is_bien_species_missing(vec_species = NULL),
    "vec_species"
  )
})

testthat::test_that("is_bien_species_missing() flags missing names", {
  res <-
    is_bien_species_missing(
      vec_species = c("Abies alba", NA, "", "   ", "Betula pendula")
    )

  testthat::expect_type(res, "logical")
  testthat::expect_identical(res, c(FALSE, TRUE, TRUE, TRUE, FALSE))
})

testthat::test_that("is_bien_species_missing() keeps the input length", {
  testthat::expect_identical(
    is_bien_species_missing(vec_species = character(0)),
    logical(0)
  )
  testthat::expect_identical(
    is_bien_species_missing(vec_species = NA_character_),
    TRUE
  )
})
