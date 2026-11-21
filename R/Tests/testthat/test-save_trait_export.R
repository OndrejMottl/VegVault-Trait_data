testthat::test_that("save_trait_export() validates its arguments", {
  dir_output <-
    withr::local_tempdir()

  data_source <-
    tibble::tibble(trait_value = c(1, 2))

  testthat::expect_error(
    save_trait_export(
      data_source = data_source,
      file_name = "",
      dir_output = dir_output
    ),
    "file_name"
  )
  testthat::expect_error(
    save_trait_export(
      data_source = data_source,
      file_name = "data_trait_try",
      dir_output = c(dir_output, dir_output)
    ),
    "dir_output"
  )
})

testthat::test_that("save_trait_export() saves into a new folder", {
  dir_output <-
    base::file.path(withr::local_tempdir(), "Outputs", "Data_temp")

  data_source <-
    tibble::tibble(trait_value = c(1, 2), trait_unit = "mm2")

  res <-
    save_trait_export(
      data_source = data_source,
      file_name = "data_trait_try",
      dir_output = dir_output
    )

  testthat::expect_type(res, "character")
  testthat::expect_length(res, 1L)
  testthat::expect_true(base::file.exists(res))
  testthat::expect_match(
    base::basename(res),
    "^data_trait_try_[0-9]{4}-[0-9]{2}-[0-9]{2}__[0-9a-f]{32}__[.]qs$"
  )
  testthat::expect_identical(qs2::qs_read(file = res), data_source)
})

testthat::test_that("save_trait_export() writes one file per content", {
  dir_output <-
    withr::local_tempdir()

  data_source <-
    tibble::tibble(trait_value = c(1, 2))

  res_first <-
    save_trait_export(
      data_source = data_source,
      file_name = "data_trait_try",
      dir_output = dir_output
    )

  res_second <-
    save_trait_export(
      data_source = data_source,
      file_name = "data_trait_try",
      dir_output = dir_output
    )

  testthat::expect_identical(res_first, res_second)
  testthat::expect_length(base::list.files(dir_output), 1L)
})
