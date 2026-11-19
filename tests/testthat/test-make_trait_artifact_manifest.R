testthat::test_that("make_trait_artifact_manifest() rejects missing files", {
  path_fixture <-
    withr::local_tempdir()

  path_input <-
    base::file.path(path_fixture, "input.txt")

  path_output <-
    base::file.path(path_fixture, "output.qs")

  base::writeLines("input", path_input)

  base::writeLines("output", path_output)

  testthat::expect_error(
    make_trait_artifact_manifest(
      input_paths = c(path_input, base::file.path(path_fixture, "gone.txt")),
      output_paths = path_output
    ),
    "gone.txt"
  )
  testthat::expect_error(
    make_trait_artifact_manifest(
      input_paths = path_input,
      output_paths = base::file.path(path_fixture, "gone.qs")
    ),
    "gone.qs"
  )
})

testthat::test_that("make_trait_artifact_manifest() validates row counts", {
  path_fixture <-
    withr::local_tempdir()

  path_input <-
    base::file.path(path_fixture, "input.txt")

  path_output <-
    base::file.path(path_fixture, "output.qs")

  base::writeLines("input", path_input)

  base::writeLines("output", path_output)

  testthat::expect_error(
    make_trait_artifact_manifest(
      input_paths = path_input,
      output_paths = path_output,
      output_rows = c(1L, 2L)
    ),
    "output_rows"
  )
  testthat::expect_error(
    make_trait_artifact_manifest(
      input_paths = path_input,
      output_paths = path_output,
      output_rows = -1L
    ),
    "output_rows"
  )
})

testthat::test_that("make_trait_artifact_manifest() records every file", {
  path_fixture <-
    withr::local_tempdir()

  path_input <-
    base::file.path(path_fixture, "input.txt")

  vec_output_paths <-
    base::file.path(path_fixture, c("part_0.qs", "part_1.qs"))

  base::writeLines("input", path_input)

  base::writeLines("output zero", vec_output_paths[1])

  base::writeLines("output one", vec_output_paths[2])

  data_contract_fixture <-
    tibble::tibble(record_id = c("a", "b"), trait_value = c(1, 2))

  res <-
    make_trait_artifact_manifest(
      input_paths = path_input,
      output_paths = vec_output_paths,
      output_rows = c(3L, 4L),
      data_contract = data_contract_fixture
    )

  data_outputs <-
    res |>
    dplyr::filter(.data$artifact_role == "output")

  testthat::expect_named(
    res,
    c(
      "artifact_role", "path", "bytes", "md5", "sha256", "n_rows",
      "contract_rows", "contract_schema", "recorded_at_utc"
    )
  )
  testthat::expect_setequal(
    dplyr::pull(res, artifact_role),
    c("input", "registry", "output")
  )
  testthat::expect_equal(
    dplyr::pull(data_outputs, sha256),
    base::unname(tools::sha256sum(vec_output_paths))
  )
  testthat::expect_equal(dplyr::pull(data_outputs, n_rows), c(3L, 4L))
  testthat::expect_true(
    base::all(
      base::is.na(
        res |>
          dplyr::filter(.data$artifact_role != "output") |>
          dplyr::pull(n_rows)
      )
    )
  )
  testthat::expect_true(base::all(base::nchar(dplyr::pull(res, md5)) == 32L))
  testthat::expect_true(base::all(dplyr::pull(res, contract_rows) == 2L))
  testthat::expect_true(
    base::all(
      dplyr::pull(res, contract_schema) == "record_id|trait_value"
    )
  )
})

testthat::test_that("make_trait_artifact_manifest() works without data", {
  path_fixture <-
    withr::local_tempdir()

  path_input <-
    base::file.path(path_fixture, "input.txt")

  base::writeLines("input", path_input)

  res <-
    make_trait_artifact_manifest(input_paths = path_input)

  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, n_rows))))
  testthat::expect_true(
    base::all(base::is.na(dplyr::pull(res, contract_rows)))
  )
  testthat::expect_true(
    base::all(base::is.na(dplyr::pull(res, contract_schema)))
  )
})
