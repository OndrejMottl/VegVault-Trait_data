testthat::test_that("load_trait_rule_table() validates its arguments", {
  path_file <-
    withr::local_tempfile(fileext = ".csv")

  base::writeLines(c("trait_id,trait_domain", "4,Stem"), con = path_file)

  testthat::expect_error(
    load_trait_rule_table(
      path_file = base::file.path(base::tempdir(), "no_such_table.csv"),
      vec_columns_required = "trait_id"
    ),
    "path_file"
  )
  testthat::expect_error(
    load_trait_rule_table(
      path_file = c(path_file, path_file),
      vec_columns_required = "trait_id"
    ),
    "path_file"
  )
  testthat::expect_error(
    load_trait_rule_table(
      path_file = path_file,
      vec_columns_required = character(0)
    ),
    "vec_columns_required"
  )
  testthat::expect_error(
    load_trait_rule_table(
      path_file = path_file,
      vec_columns_required = 1
    ),
    "vec_columns_required"
  )
})

testthat::test_that("load_trait_rule_table() reads every column as text", {
  path_file <-
    withr::local_tempfile(fileext = ".csv")

  base::writeLines(
    c(
      "trait_id,trait_domain,derivation_rule_key",
      "4,Stem specific density,",
      "3116, Leaf mass per area ,sla_to_lma",
      "26,NA,"
    ),
    con = path_file
  )

  res <-
    load_trait_rule_table(
      path_file = path_file,
      vec_columns_required = c("trait_id", "trait_domain")
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("trait_id", "trait_domain", "derivation_rule_key")
  )
  testthat::expect_equal(base::nrow(res), 3L)
  testthat::expect_type(dplyr::pull(res, "trait_id"), "character")
  testthat::expect_identical(
    dplyr::pull(res, "trait_id"),
    c("4", "3116", "26")
  )
  testthat::expect_identical(
    dplyr::pull(res, "trait_domain"),
    c("Stem specific density", "Leaf mass per area", NA)
  )
  testthat::expect_identical(
    dplyr::pull(res, "derivation_rule_key"),
    c(NA, "sla_to_lma", NA)
  )
})

testthat::test_that("load_trait_rule_table() keeps a table without rows", {
  path_file <-
    withr::local_tempfile(fileext = ".csv")

  base::writeLines("bien_source_key,trait_domains", con = path_file)

  res <-
    load_trait_rule_table(
      path_file = path_file,
      vec_columns_required = "bien_source_key"
    )

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(res, c("bien_source_key", "trait_domains"))
  testthat::expect_type(dplyr::pull(res, "trait_domains"), "character")
})

testthat::test_that("load_trait_rule_table() rejects a missing column", {
  path_file <-
    withr::local_tempfile(fileext = ".csv")

  base::writeLines(c("trait_id,trait_domain", "4,Stem"), con = path_file)

  testthat::expect_error(
    load_trait_rule_table(
      path_file = path_file,
      vec_columns_required = c("trait_id", "derivation_rule_key")
    ),
    "derivation_rule_key"
  )
})
