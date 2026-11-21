testthat::test_that("load_bien_trait_raw_files() validates its input", {
  path_dir <-
    withr::local_tempdir()

  testthat::expect_error(
    load_bien_trait_raw_files(vec_paths = character(0)),
    "vec_paths"
  )
  testthat::expect_error(
    load_bien_trait_raw_files(vec_paths = 1),
    "vec_paths"
  )
  testthat::expect_error(
    load_bien_trait_raw_files(
      vec_paths = base::file.path(path_dir, "missing.qs")
    ),
    "vec_paths"
  )
})

testthat::test_that("load_bien_trait_raw_files() combines files in order", {
  path_dir <-
    withr::local_tempdir()

  path_leaf <-
    base::file.path(path_dir, "leaf area.qs")

  path_seed <-
    base::file.path(path_dir, "seed mass.qs")

  qs2::qs_save(
    object = tibble::tibble(
      trait_name = "leaf area",
      trait_value = c("10", "20"),
      URL_Source = c("a", "b")
    ),
    file = path_leaf
  )

  qs2::qs_save(
    object = base::data.frame(
      trait_name = "seed mass",
      trait_value = "3",
      project_pi = "Doe J"
    ),
    file = path_seed
  )

  res <-
    load_bien_trait_raw_files(vec_paths = c(path_seed, path_leaf))

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("trait_name", "trait_value", "project_pi", "url_source")
  )
  testthat::expect_identical(
    dplyr::pull(res, "trait_name"),
    c("seed mass", "leaf area", "leaf area")
  )
  testthat::expect_identical(
    dplyr::pull(res, "url_source"),
    c(NA, "a", "b")
  )
})

testthat::test_that("load_bien_trait_raw_files() warns about an empty file", {
  path_dir <-
    withr::local_tempdir()

  path_leaf <-
    base::file.path(path_dir, "leaf area.qs")

  path_seed <-
    base::file.path(path_dir, "seed mass.qs")

  qs2::qs_save(
    object = tibble::tibble(trait_name = "leaf area", trait_value = "10"),
    file = path_leaf
  )

  qs2::qs_save(
    object = tibble::tibble(
      trait_name = base::character(0),
      trait_value = base::character(0)
    ),
    file = path_seed
  )

  testthat::expect_warning(
    res <-
      load_bien_trait_raw_files(vec_paths = c(path_leaf, path_seed)),
    "seed mass"
  )
  testthat::expect_equal(base::nrow(res), 1L)
})

testthat::test_that("load_bien_trait_raw_files() rejects a non-table file", {
  path_dir <-
    withr::local_tempdir()

  path_file <-
    base::file.path(path_dir, "seed mass.qs")

  qs2::qs_save(object = base::letters, file = path_file)

  testthat::expect_error(
    load_bien_trait_raw_files(vec_paths = path_file),
    "data frame"
  )
})
