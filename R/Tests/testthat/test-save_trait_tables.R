testthat::test_that("save_trait_tables() validates its arguments", {
  dir_output <-
    withr::local_tempdir()

  testthat::expect_error(
    save_trait_tables(
      list_tables = list(),
      dir_output = dir_output
    ),
    "list_tables"
  )
  testthat::expect_error(
    save_trait_tables(
      list_tables = list(tibble::tibble(a = 1)),
      dir_output = dir_output
    ),
    "list_tables"
  )
  testthat::expect_error(
    save_trait_tables(
      list_tables = list(table_a = "not a table"),
      dir_output = dir_output
    ),
    "list_tables"
  )
  testthat::expect_error(
    save_trait_tables(
      list_tables = list(table_a = tibble::tibble(a = 1)),
      dir_output = 1
    ),
    "dir_output"
  )
})

testthat::test_that("save_trait_tables() writes dated CSV files", {
  dir_output <-
    base::file.path(withr::local_tempdir(), "Tables")

  list_tables <-
    list(
      trait_unit_reconciliation_try = tibble::tibble(
        source_trait_id = c("4", "26"),
        n_records = c(10L, 5L)
      ),
      trait_coordinate_summary_try = tibble::tibble(n_records = 15L)
    )

  res <-
    save_trait_tables(
      list_tables = list_tables,
      dir_output = dir_output
    )

  name_date <-
    base::format(base::Sys.Date(), "%Y-%m-%d")

  testthat::expect_type(res, "character")
  testthat::expect_identical(
    base::basename(res),
    c(
      stringr::str_c("trait_unit_reconciliation_try_", name_date, ".csv"),
      stringr::str_c("trait_coordinate_summary_try_", name_date, ".csv")
    )
  )
  testthat::expect_true(base::all(base::file.exists(res)))

  data_read_back <-
    readr::read_csv(
      file = res[[1]],
      col_types = readr::cols(.default = readr::col_character())
    )

  testthat::expect_identical(
    dplyr::pull(data_read_back, "source_trait_id"),
    c("4", "26")
  )
})

testthat::test_that("save_trait_tables() keeps one file per table and day", {
  dir_output <-
    withr::local_tempdir()

  list_tables <-
    list(trait_coordinate_summary_bien = tibble::tibble(n_records = 15L))

  res_first <-
    save_trait_tables(
      list_tables = list_tables,
      dir_output = dir_output
    )

  res_second <-
    save_trait_tables(
      list_tables = list(
        trait_coordinate_summary_bien = tibble::tibble(n_records = 16L)
      ),
      dir_output = dir_output
    )

  testthat::expect_identical(res_first, res_second)
  testthat::expect_length(base::list.files(dir_output), 1L)
  testthat::expect_identical(
    dplyr::pull(readr::read_csv(res_second, show_col_types = FALSE)),
    16
  )
})
