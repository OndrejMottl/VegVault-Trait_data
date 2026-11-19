testthat::test_that("build_bien_source_key() validates lengths", {
  testthat::expect_error(
    build_bien_source_key(
      url_source = c("a", "b"),
      source_citation = NA_character_,
      project_pi = NA_character_
    ),
    "same length"
  )
})

testthat::test_that("build_bien_source_key() takes the specific field", {
  res <-
    build_bien_source_key(
      url_source = c("http://www.kew.org/data/sid ", NA, NA, NA),
      source_citation = c("Citation", "Mazer S. (1989)", NA, NA),
      project_pi = c("Liu K", "Mazer S", " Chacon E", NA)
    )

  testthat::expect_type(res, "character")
  testthat::expect_equal(
    res,
    c("http://www.kew.org/data/sid", "Mazer S. (1989)", "Chacon E", "BIEN")
  )
})
