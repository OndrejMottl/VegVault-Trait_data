#----------------------------------------------------------#
# Fixture -----
#----------------------------------------------------------#

# A small TRY request: two selected traits (4, 3106), one other trait (99),
#   ancillary records of selected and unselected observations, one
#   unselected ancillary variable and one excluded ancillary DataID. The
#   contributor column is never read. Written as Latin-1, like TRY files;
#   the species name of observation 3 holds a non-ASCII letter (e acute).
name_try_fixture_accented <-
  stringr::str_c("Carex caf", base::intToUtf8(233L), "a")

vec_try_fixture_lines <-
  c(
    stringr::str_c(
      c(
        "LastName", "Dataset", "AccSpeciesName", "ObservationID", "TraitID",
        "DataID", "DataName", "OrigValueStr"
      ),
      collapse = "\t"
    ),
    "Smith\tD1\tAbies alba\t1\t4\t10\tWood density\t0.5",
    "Smith\tD1\tAbies alba\t1\t\t59\tLatitude\t50.1",
    "Smith\tD1\tAbies alba\t1\t\t60\tLongitude\t14.2",
    "Smith\tD1\tAbies alba\t1\t\t61\tAltitude\t300",
    "Smith\tD2\tBetula pendula\t2\t99\t11\tOther trait\tlow",
    "Smith\tD2\tBetula pendula\t2\t\t59\tLatitude\t48.0",
    stringr::str_c(
      "Jones\tD3\t", name_try_fixture_accented,
      "\t3\t3106\t2221\theight at 15 days\t1.2"
    ),
    stringr::str_c(
      "Jones\tD3\t", name_try_fixture_accented, "\t3\t\t2222\tLatitude\t45"
    ),
    stringr::str_c(
      "Jones\tD3\t", name_try_fixture_accented, "\t3\t\t59\tLatitude\t45.5"
    )
  )

vec_try_fixture_columns <-
  c(
    "Dataset", "AccSpeciesName", "ObservationID", "TraitID", "DataID",
    "DataName", "OrigValueStr"
  )


#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("load_try_records() validates its arguments", {
  path_file <-
    withr::local_tempfile(fileext = ".txt")

  base::writeLines(
    base::iconv(vec_try_fixture_lines, from = "UTF-8", to = "latin1"),
    con = path_file,
    useBytes = TRUE
  )

  testthat::expect_error(
    load_try_records(
      path_file = base::file.path(base::tempdir(), "no_such_request.txt"),
      vec_columns = vec_try_fixture_columns,
      vec_trait_ids = "4",
      vec_ancillary_names = "Latitude",
      vec_data_ids_excluded = character(0)
    ),
    "path_file"
  )
  testthat::expect_error(
    load_try_records(
      path_file = path_file,
      vec_columns = base::setdiff(vec_try_fixture_columns, "TraitID"),
      vec_trait_ids = "4",
      vec_ancillary_names = "Latitude",
      vec_data_ids_excluded = character(0)
    ),
    "vec_columns"
  )
  testthat::expect_error(
    load_try_records(
      path_file = path_file,
      vec_columns = vec_try_fixture_columns,
      vec_trait_ids = 4,
      vec_ancillary_names = "Latitude",
      vec_data_ids_excluded = character(0)
    ),
    "vec_trait_ids"
  )
  testthat::expect_error(
    load_try_records(
      path_file = path_file,
      vec_columns = vec_try_fixture_columns,
      vec_trait_ids = "4",
      vec_ancillary_names = 1,
      vec_data_ids_excluded = character(0)
    ),
    "vec_ancillary_names"
  )
  testthat::expect_error(
    load_try_records(
      path_file = path_file,
      vec_columns = vec_try_fixture_columns,
      vec_trait_ids = "4",
      vec_ancillary_names = "Latitude",
      vec_data_ids_excluded = 2221
    ),
    "vec_data_ids_excluded"
  )
})

testthat::test_that("load_try_records() rejects a column the file lacks", {
  path_file <-
    withr::local_tempfile(fileext = ".txt")

  base::writeLines(
    base::iconv(vec_try_fixture_lines, from = "UTF-8", to = "latin1"),
    con = path_file,
    useBytes = TRUE
  )

  testthat::expect_error(
    load_try_records(
      path_file = path_file,
      vec_columns = c(vec_try_fixture_columns, "StdValue"),
      vec_trait_ids = "4",
      vec_ancillary_names = "Latitude",
      vec_data_ids_excluded = character(0)
    ),
    "StdValue"
  )
})


#----------------------------------------------------------#
# Outputs and functionality -----
#----------------------------------------------------------#

testthat::test_that("load_try_records() keeps the selected trait records", {
  path_file <-
    withr::local_tempfile(fileext = ".txt")

  base::writeLines(
    base::iconv(vec_try_fixture_lines, from = "UTF-8", to = "latin1"),
    con = path_file,
    useBytes = TRUE
  )

  res <-
    load_try_records(
      path_file = path_file,
      vec_columns = vec_try_fixture_columns,
      vec_trait_ids = c("4", "3106"),
      vec_ancillary_names = c("Latitude", "Longitude"),
      vec_data_ids_excluded = c("2221", "2222")
    )

  testthat::expect_type(res, "list")
  testthat::expect_named(res, c("data_trait_rows", "data_ancillary"))

  data_trait_rows <-
    purrr::chuck(res, "data_trait_rows")

  testthat::expect_s3_class(data_trait_rows, "tbl_df")
  testthat::expect_named(
    data_trait_rows,
    c(vec_try_fixture_columns, "source_trait_id")
  )
  testthat::expect_identical(
    dplyr::pull(data_trait_rows, "source_trait_id"),
    c("4", "3106")
  )
  testthat::expect_identical(
    dplyr::pull(data_trait_rows, "OrigValueStr"),
    c("0.5", "1.2")
  )
  testthat::expect_identical(
    dplyr::pull(data_trait_rows, "AccSpeciesName"),
    c("Abies alba", name_try_fixture_accented)
  )
})

testthat::test_that("load_try_records() keeps the selected ancillary rows", {
  path_file <-
    withr::local_tempfile(fileext = ".txt")

  base::writeLines(
    base::iconv(vec_try_fixture_lines, from = "UTF-8", to = "latin1"),
    con = path_file,
    useBytes = TRUE
  )

  res <-
    load_try_records(
      path_file = path_file,
      vec_columns = vec_try_fixture_columns,
      vec_trait_ids = c("4", "3106"),
      vec_ancillary_names = c("Latitude", "Longitude"),
      vec_data_ids_excluded = c("2221", "2222")
    )

  data_ancillary <-
    purrr::chuck(res, "data_ancillary")

  testthat::expect_s3_class(data_ancillary, "tbl_df")
  testthat::expect_named(data_ancillary, vec_try_fixture_columns)
  testthat::expect_identical(
    base::as.integer(dplyr::pull(data_ancillary, "ObservationID")),
    c(1L, 1L, 3L)
  )
  testthat::expect_identical(
    dplyr::pull(data_ancillary, "DataName"),
    c("Latitude", "Longitude", "Latitude")
  )
  testthat::expect_identical(
    dplyr::pull(data_ancillary, "OrigValueStr"),
    c("50.1", "14.2", "45.5")
  )
})

testthat::test_that("load_try_records() never reads unselected columns", {
  path_file <-
    withr::local_tempfile(fileext = ".txt")

  base::writeLines(
    base::iconv(vec_try_fixture_lines, from = "UTF-8", to = "latin1"),
    con = path_file,
    useBytes = TRUE
  )

  res <-
    load_try_records(
      path_file = path_file,
      vec_columns = vec_try_fixture_columns,
      vec_trait_ids = c("4", "3106"),
      vec_ancillary_names = c("Latitude", "Longitude"),
      vec_data_ids_excluded = c("2221", "2222")
    )

  testthat::expect_false(
    "LastName" %in% base::names(purrr::chuck(res, "data_trait_rows"))
  )
  testthat::expect_false(
    "LastName" %in% base::names(purrr::chuck(res, "data_ancillary"))
  )
})

testthat::test_that("load_try_records() keeps every DataID if none excluded", {
  path_file <-
    withr::local_tempfile(fileext = ".txt")

  base::writeLines(
    base::iconv(vec_try_fixture_lines, from = "UTF-8", to = "latin1"),
    con = path_file,
    useBytes = TRUE
  )

  res <-
    load_try_records(
      path_file = path_file,
      vec_columns = vec_try_fixture_columns,
      vec_trait_ids = c("4", "3106"),
      vec_ancillary_names = "Latitude",
      vec_data_ids_excluded = character(0)
    )

  testthat::expect_identical(
    base::as.integer(
      dplyr::pull(purrr::chuck(res, "data_ancillary"), "DataID")
    ),
    c(59L, 2222L, 59L)
  )
})
