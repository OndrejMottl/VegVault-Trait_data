testthat::test_that("known TRY Leaf Area validation fails closed", {
  fixture <-
    tibble::tibble(
      conversion_rule_key = rep("try_leaf_area_below_cm2", 3632L),
      disposition = "published",
      trait_unit = "mm2",
      original_value = seq_len(3632L),
      trait_value = seq_len(3632L) * 100
    )

  testthat::expect_invisible(validate_known_try_leaf_area(fixture))
  testthat::expect_error(
    validate_known_try_leaf_area(dplyr::slice_head(fixture, n = 3631L)),
    "3,632"
  )
})
