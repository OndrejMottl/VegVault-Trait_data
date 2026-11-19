testthat::test_that("save_trait_artifact() validates its arguments", {
  path_dir <-
    withr::local_tempdir()

  data_object <-
    tibble::tibble(value = 1:3)

  testthat::expect_error(
    save_trait_artifact(
      object_to_save = data_object,
      file_name = c("a", "b"),
      dir = path_dir
    ),
    "file_name"
  )
  testthat::expect_error(
    save_trait_artifact(
      object_to_save = data_object,
      file_name = "",
      dir = path_dir
    ),
    "file_name"
  )
  testthat::expect_error(
    save_trait_artifact(
      object_to_save = data_object,
      file_name = "data_trait_try",
      dir = base::file.path(path_dir, "missing")
    ),
    "dir"
  )
  testthat::expect_error(
    save_trait_artifact(
      object_to_save = data_object,
      file_name = "data_trait_try",
      dir = path_dir,
      save_date = "2026-01-01"
    ),
    "save_date"
  )
})

testthat::test_that("save_trait_artifact() writes the named qs2 file", {
  path_dir <-
    withr::local_tempdir()

  data_object <-
    tibble::tibble(value = 1:3)

  res <-
    save_trait_artifact(
      object_to_save = data_object,
      file_name = "data_trait_try",
      dir = path_dir,
      save_date = base::as.Date("2026-01-02")
    )

  testthat::expect_type(res, "character")
  testthat::expect_length(res, 1L)
  testthat::expect_true(base::file.exists(res))
  testthat::expect_equal(
    base::basename(res),
    stringr::str_glue(
      "data_trait_try_2026-01-02__{rlang::hash(data_object)}__.qs"
    ) |>
      base::as.character()
  )
  testthat::expect_equal(qs2::qs_read(res), data_object)
})

testthat::test_that("save_trait_artifact() names files by content", {
  path_dir <-
    withr::local_tempdir()

  res_first <-
    save_trait_artifact(
      object_to_save = tibble::tibble(value = 1:3),
      file_name = "data_traits_bien_0",
      dir = path_dir,
      save_date = base::as.Date("2026-01-02")
    )

  res_repeat <-
    save_trait_artifact(
      object_to_save = tibble::tibble(value = 1:3),
      file_name = "data_traits_bien_0",
      dir = path_dir,
      save_date = base::as.Date("2026-01-02")
    )

  res_other <-
    save_trait_artifact(
      object_to_save = tibble::tibble(value = 4:6),
      file_name = "data_traits_bien_0",
      dir = path_dir,
      save_date = base::as.Date("2026-01-02")
    )

  testthat::expect_equal(res_repeat, res_first)
  testthat::expect_false(res_other == res_first)
  testthat::expect_length(base::list.files(path_dir), 2L)
})
