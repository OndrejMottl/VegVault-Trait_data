build_trait_parts_fixture <- function(
  path_dir,
  vec_part_rows = c(3L, 2L),
  file_stem = "data_traits_bien"
) {
  vec_parts <-
    base::seq_along(vec_part_rows) - 1L

  vec_paths <-
    purrr::map2_chr(
      .x = vec_parts,
      .y = vec_part_rows,
      .f = ~ save_trait_artifact(
        object_to_save = tibble::tibble(
          part = .x,
          value = base::seq_len(.y)
        ),
        file_name = stringr::str_glue("{file_stem}_{.x}"),
        dir = path_dir,
        save_date = base::as.Date("2026-01-02")
      )
    )

  res <-
    tibble::tibble(
      part = vec_parts,
      path = vec_paths,
      n_rows = vec_part_rows
    )

  return(res)
}

testthat::test_that("validate_trait_artifact_parts() accepts a full set", {
  path_dir <-
    withr::local_tempdir()

  data_parts <-
    build_trait_parts_fixture(path_dir)

  res <-
    validate_trait_artifact_parts(
      data_parts = data_parts,
      file_stem = "data_traits_bien",
      n_parts_expected = 2L,
      n_rows_expected = 5L
    )

  testthat::expect_equal(res, data_parts)
})

testthat::test_that("validate_trait_artifact_parts() needs every part", {
  path_dir <-
    withr::local_tempdir()

  data_parts <-
    build_trait_parts_fixture(path_dir)

  testthat::expect_error(
    validate_trait_artifact_parts(
      data_parts = data_parts,
      file_stem = "data_traits_bien",
      n_parts_expected = 3L,
      n_rows_expected = 5L
    ),
    "parts"
  )
  testthat::expect_error(
    validate_trait_artifact_parts(
      data_parts = dplyr::mutate(data_parts, part = c(0L, 2L)),
      file_stem = "data_traits_bien",
      n_parts_expected = 2L,
      n_rows_expected = 5L
    ),
    "numbered"
  )
})

testthat::test_that("validate_trait_artifact_parts() checks file names", {
  path_dir <-
    withr::local_tempdir()

  data_parts <-
    build_trait_parts_fixture(path_dir)

  testthat::expect_error(
    validate_trait_artifact_parts(
      data_parts = dplyr::mutate(
        data_parts,
        path = base::rev(.data$path)
      ),
      file_stem = "data_traits_bien",
      n_parts_expected = 2L,
      n_rows_expected = 5L
    ),
    "name"
  )
  testthat::expect_error(
    validate_trait_artifact_parts(
      data_parts = data_parts,
      file_stem = "data_trait_try",
      n_parts_expected = 2L,
      n_rows_expected = 5L
    ),
    "name"
  )
})

testthat::test_that("validate_trait_artifact_parts() checks rows", {
  path_dir <-
    withr::local_tempdir()

  data_parts <-
    build_trait_parts_fixture(path_dir)

  testthat::expect_error(
    validate_trait_artifact_parts(
      data_parts = data_parts,
      file_stem = "data_traits_bien",
      n_parts_expected = 2L,
      n_rows_expected = 6L
    ),
    "rows"
  )
  testthat::expect_error(
    validate_trait_artifact_parts(
      data_parts = dplyr::mutate(data_parts, n_rows = c(4L, 1L)),
      file_stem = "data_traits_bien",
      n_parts_expected = 2L,
      n_rows_expected = 5L
    ),
    "read back"
  )
  data_parts_unread <-
    dplyr::mutate(data_parts, n_rows = c(4L, 1L))

  res_unread <-
    validate_trait_artifact_parts(
      data_parts = data_parts_unread,
      file_stem = "data_traits_bien",
      n_parts_expected = 2L,
      n_rows_expected = 5L,
      flag_read_back = FALSE
    )

  testthat::expect_equal(res_unread, data_parts_unread)
})

testthat::test_that("validate_trait_artifact_parts() needs the files", {
  path_dir <-
    withr::local_tempdir()

  data_parts <-
    build_trait_parts_fixture(path_dir)

  base::file.remove(dplyr::pull(data_parts, path)[2])

  testthat::expect_error(
    validate_trait_artifact_parts(
      data_parts = data_parts,
      file_stem = "data_traits_bien",
      n_parts_expected = 2L,
      n_rows_expected = 5L
    ),
    "exist"
  )
})
