testthat::test_that("normalise_text_encoding() validates its arguments", {
  testthat::expect_error(
    normalise_text_encoding(vec_text = 1:3),
    "character"
  )

  testthat::expect_error(
    normalise_text_encoding(
      vec_text = "a",
      max_rounds = 0L
    ),
    "max_rounds"
  )

  testthat::expect_error(
    normalise_text_encoding(
      vec_text = "a",
      verbose = NA
    ),
    "verbose"
  )
})

testthat::test_that("normalise_text_encoding() keeps length, type and NA", {
  res <-
    normalise_text_encoding(
      vec_text = c("Kleyer M", NA_character_, "D\u00c3\u00adaz S", ""),
      verbose = FALSE
    )

  testthat::expect_type(res, "character")
  testthat::expect_length(res, 4L)
  testthat::expect_equal(res, c("Kleyer M", NA_character_, "D\u00edaz S", ""))

  res_empty <-
    normalise_text_encoding(
      vec_text = base::character(0),
      verbose = FALSE
    )

  testthat::expect_equal(res_empty, base::character(0))
})

testthat::test_that("normalise_text_encoding() leaves correct text as is", {
  vec_correct <-
    c(
      "Kleyer M",
      "D\u00edaz S",
      "Chac\u00f3n E",
      "Darwin\u2019s conundrum",
      "pp. 487\u2013496",
      "Niinemets \u00dc"
    )

  res <-
    normalise_text_encoding(
      vec_text = vec_correct,
      verbose = FALSE
    )

  testthat::expect_equal(res, vec_correct)
})

testthat::test_that("normalise_text_encoding() accepts stray soft hyphens", {
  vec_soft_hyphen <- "Ram\u00ed\u00adrez-Valiente JA"

  testthat::expect_no_warning(
    res <-
      normalise_text_encoding(
        vec_text = vec_soft_hyphen,
        verbose = FALSE
      )
  )

  testthat::expect_equal(res, vec_soft_hyphen)
})

testthat::test_that("normalise_text_encoding() reverses one round", {
  res <-
    normalise_text_encoding(
      vec_text = c(
        "D\u00c3\u00adaz S",
        "Niinemets \u00c3\u009c",
        "Darwin\u00e2\u0080\u0099s",
        "Darwin\u00e2\u20ac\u2122s",
        "487\u00e2\u20ac\u201c496"
      ),
      verbose = FALSE
    )

  testthat::expect_equal(
    res,
    c(
      "D\u00edaz S",
      "Niinemets \u00dc",
      "Darwin\u2019s",
      "Darwin\u2019s",
      "487\u2013496"
    )
  )
})

testthat::test_that("normalise_text_encoding() reverses two rounds", {
  res <-
    normalise_text_encoding(
      vec_text = c(
        "580\u00c3\u00a2\u00c2\u0080\u00c2\u0093582",
        "Chac\u00c3\u0083\u00c2\u00b3n E",
        "Darwin\u00c3\u00a2\u00e2\u0082\u00ac\u00e2\u0084\u00a2s"
      ),
      verbose = FALSE
    )

  testthat::expect_equal(
    res,
    c("580\u2013582", "Chac\u00f3n E", "Darwin\u2019s")
  )

  testthat::expect_warning(
    res_one_round <-
      normalise_text_encoding(
        vec_text = "Chac\u00c3\u0083\u00c2\u00b3n E",
        max_rounds = 1L,
        verbose = FALSE
      ),
    "mis-encoded"
  )

  testthat::expect_equal(res_one_round, "Chac\u00c3\u00b3n E")
})

testthat::test_that("normalise_text_encoding() maps C1 controls to cp1252", {
  res <-
    normalise_text_encoding(
      vec_text = c(
        "pp. 487\u00c2\u0096496",
        "Darwin\u0092s",
        "1205\u00c2\u00961215"
      ),
      verbose = FALSE
    )

  testthat::expect_equal(
    res,
    c("pp. 487\u2013496", "Darwin\u2019s", "1205\u20131215")
  )
})

testthat::test_that("normalise_text_encoding() warns on unrepairable text", {
  vec_mixed <- "Gal\u00e9s J, D\u00c3\u00adaz S"

  testthat::expect_warning(
    res <-
      normalise_text_encoding(
        vec_text = vec_mixed,
        verbose = FALSE
      ),
    "mis-encoded"
  )

  testthat::expect_equal(res, vec_mixed)
})

testthat::test_that("normalise_text_encoding() reports repairs if verbose", {
  testthat::expect_message(
    normalise_text_encoding(
      vec_text = c("D\u00c3\u00adaz S", "D\u00c3\u00adaz S", "Kleyer M"),
      verbose = TRUE
    ),
    "Repaired 1 of 2 distinct"
  )

  testthat::expect_no_message(
    normalise_text_encoding(
      vec_text = c("D\u00c3\u00adaz S", "Kleyer M"),
      verbose = FALSE
    )
  )
})

testthat::test_that("normalise_text_encoding() handles repeated values", {
  base::set.seed(900723)

  vec_input <-
    base::sample(
      x = c("D\u00c3\u00adaz S", "Kleyer M", NA_character_),
      size = 10000L,
      replace = TRUE
    )

  res <-
    normalise_text_encoding(
      vec_text = vec_input,
      verbose = FALSE
    )

  testthat::expect_length(res, 10000L)
  testthat::expect_equal(
    base::is.na(res),
    base::is.na(vec_input)
  )
  testthat::expect_equal(
    base::unique(res[vec_input %in% "D\u00c3\u00adaz S"]),
    "D\u00edaz S"
  )
})
