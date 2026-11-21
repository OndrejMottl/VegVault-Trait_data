testthat::test_that("save_unpublished_trait_records() validates arguments", {
  dir_output <-
    withr::local_tempdir()

  testthat::expect_error(
    save_unpublished_trait_records(
      data_source = tibble::tibble(trait_value = 1),
      file_name = "trait_unit_unpublished_try",
      dir_output = dir_output
    ),
    "disposition"
  )
  testthat::expect_error(
    save_unpublished_trait_records(
      data_source = tibble::tibble(disposition = "dropped"),
      file_name = "",
      dir_output = dir_output
    ),
    "file_name"
  )
  testthat::expect_error(
    save_unpublished_trait_records(
      data_source = tibble::tibble(disposition = "dropped"),
      file_name = "trait_unit_unpublished_try",
      dir_output = NA_character_
    ),
    "dir_output"
  )
})

testthat::test_that("save_unpublished_trait_records() keeps unpublished", {
  dir_output <-
    base::file.path(withr::local_tempdir(), "Audit")

  data_source <-
    tibble::tibble(
      source_record_id = c("1", "2", "3", "4"),
      disposition = c("published", "quarantined", "dropped", "published")
    )

  res <-
    save_unpublished_trait_records(
      data_source = data_source,
      file_name = "trait_unit_unpublished_try",
      dir_output = dir_output
    )

  testthat::expect_identical(
    res,
    base::file.path(dir_output, "trait_unit_unpublished_try.qs2")
  )
  testthat::expect_true(base::file.exists(res))

  data_saved <-
    qs2::qs_read(file = res)

  testthat::expect_identical(
    dplyr::pull(data_saved, "source_record_id"),
    c("2", "3")
  )
})

testthat::test_that("save_unpublished_trait_records() replaces the file", {
  dir_output <-
    withr::local_tempdir()

  res_first <-
    save_unpublished_trait_records(
      data_source = tibble::tibble(disposition = c("dropped", "dropped")),
      file_name = "trait_unit_unpublished_bien",
      dir_output = dir_output
    )

  res_second <-
    save_unpublished_trait_records(
      data_source = tibble::tibble(disposition = "quarantined"),
      file_name = "trait_unit_unpublished_bien",
      dir_output = dir_output
    )

  testthat::expect_identical(res_first, res_second)
  testthat::expect_equal(base::nrow(qs2::qs_read(file = res_second)), 1L)
})
