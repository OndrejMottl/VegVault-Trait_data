testthat::test_that("trait numeric parsing is strict", {
  observed <-
    prepare_trait_numeric(
      c("1", " -1.5 ", ".25", "1e-3", "1-2", "<5", "5 cm", "")
    )

  testthat::expect_equal(
    observed[1:4],
    c(1, -1.5, 0.25, 0.001)
  )
  testthat::expect_true(all(is.na(observed[5:8])))
})
