testthat::test_that("the VegVault contract copy is intact and current", {
  testthat::expect_no_error(
    validate_contract_copy(
      path_producer = here::here(),
      path_vegvault = here::here("..", "VegVault"),
      verbose = FALSE
    )
  )
})
