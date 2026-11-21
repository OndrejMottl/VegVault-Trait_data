testthat::test_that("diagnose_bien_shared_sources() validates its inputs", {
  data_twins <-
    tibble::tibble(
      bien_source_key = "a",
      qualifies = TRUE,
      listed = TRUE
    )

  testthat::expect_error(
    diagnose_bien_shared_sources(
      data_twins = dplyr::select(data_twins, -"listed"),
      data_shared_sources = tibble::tibble(bien_source_key = "a")
    ),
    "listed"
  )
  testthat::expect_error(
    diagnose_bien_shared_sources(
      data_twins = data_twins,
      data_shared_sources = tibble::tibble(source = "a")
    ),
    "bien_source_key"
  )
})

testthat::test_that("diagnose_bien_shared_sources() names the differences", {
  data_twins <-
    tibble::tibble(
      bien_source_key = c("a", "b", "c", "e"),
      qualifies = c(TRUE, TRUE, FALSE, FALSE),
      listed = c(TRUE, FALSE, TRUE, FALSE)
    )

  data_shared_sources <-
    tibble::tibble(bien_source_key = c("a", " c ", "d"))

  testthat::expect_warning(
    res <-
      diagnose_bien_shared_sources(
        data_twins = data_twins,
        data_shared_sources = data_shared_sources
      ),
    "differs"
  )
  testthat::expect_identical(res, c("b", "c", "d"))
})

testthat::test_that("diagnose_bien_shared_sources() is quiet when in step", {
  data_twins <-
    tibble::tibble(
      bien_source_key = c("a", "b"),
      qualifies = c(TRUE, FALSE),
      listed = c(TRUE, FALSE)
    )

  testthat::expect_no_warning(
    res <-
      diagnose_bien_shared_sources(
        data_twins = data_twins,
        data_shared_sources = tibble::tibble(bien_source_key = "a")
      )
  )
  testthat::expect_identical(res, character(0))
})
