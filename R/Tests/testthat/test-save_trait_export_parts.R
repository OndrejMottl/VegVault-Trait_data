testthat::test_that("save_trait_export_parts() validates its arguments", {
  dir_output <-
    withr::local_tempdir()

  data_source <-
    tibble::tibble(source_record_id = base::as.character(1:10))

  testthat::expect_error(
    save_trait_export_parts(
      data_source = "records",
      file_stem = "data_traits_bien",
      n_parts = 4L,
      dir_output = dir_output
    ),
    "data_source"
  )
  testthat::expect_error(
    save_trait_export_parts(
      data_source = data_source,
      file_stem = "",
      n_parts = 4L,
      dir_output = dir_output
    ),
    "file_stem"
  )
  testthat::expect_error(
    save_trait_export_parts(
      data_source = data_source,
      file_stem = "data_traits_bien",
      n_parts = 0L,
      dir_output = dir_output
    ),
    "n_parts"
  )
  testthat::expect_error(
    save_trait_export_parts(
      data_source = data_source,
      file_stem = "data_traits_bien",
      n_parts = 2.5,
      dir_output = dir_output
    ),
    "n_parts"
  )
})

testthat::test_that("save_trait_export_parts() rejects empty parts", {
  dir_output <-
    withr::local_tempdir()

  testthat::expect_error(
    save_trait_export_parts(
      data_source = tibble::tibble(source_record_id = c("1", "2", "3")),
      file_stem = "data_traits_bien",
      n_parts = 4L,
      dir_output = dir_output
    ),
    "fewer rows"
  )
  testthat::expect_length(base::list.files(dir_output), 0L)
})

testthat::test_that("save_trait_export_parts() splits rows in order", {
  dir_output <-
    base::file.path(withr::local_tempdir(), "Data")

  data_source <-
    tibble::tibble(
      source_record_id = base::as.character(1:10),
      trait_value = base::as.numeric(1:10)
    )

  res <-
    save_trait_export_parts(
      data_source = data_source,
      file_stem = "data_traits_bien",
      n_parts = 4L,
      dir_output = dir_output
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, c("part", "path", "n_rows"))
  testthat::expect_identical(dplyr::pull(res, "part"), 0:3)
  testthat::expect_identical(dplyr::pull(res, "n_rows"), c(3L, 2L, 2L, 3L))
  testthat::expect_true(base::all(base::file.exists(dplyr::pull(res, "path"))))
  testthat::expect_match(
    base::basename(dplyr::pull(res, "path")),
    "^data_traits_bien_[0-3]_[0-9]{4}-[0-9]{2}-[0-9]{2}__[0-9a-f]{32}__[.]qs$"
  )

  data_read_back <-
    dplyr::pull(res, "path") |>
    purrr::map(.f = ~ qs2::qs_read(file = .x)) |>
    dplyr::bind_rows()

  testthat::expect_identical(data_read_back, data_source)
})

testthat::test_that("save_trait_export_parts() writes one part when asked", {
  dir_output <-
    withr::local_tempdir()

  res <-
    save_trait_export_parts(
      data_source = tibble::tibble(source_record_id = c("1", "2")),
      file_stem = "data_traits_bien",
      n_parts = 1L,
      dir_output = dir_output
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_identical(dplyr::pull(res, "n_rows"), 2L)
})
