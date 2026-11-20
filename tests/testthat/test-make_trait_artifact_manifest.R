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

testthat::test_that("make_trait_artifact_manifest() checks root_directory", {
  path_fixture <-
    withr::local_tempdir()

  path_input <-
    base::file.path(path_fixture, "input.txt")

  base::writeLines("input", path_input)

  testthat::expect_error(
    make_trait_artifact_manifest(
      input_paths = path_input,
      registry_directory = path_fixture,
      root_directory = base::file.path(path_fixture, "missing_folder")
    ),
    "'root_directory' must"
  )
  testthat::expect_error(
    make_trait_artifact_manifest(
      input_paths = path_input,
      registry_directory = path_fixture,
      root_directory = c(path_fixture, path_fixture)
    ),
    "'root_directory' must"
  )
})

testthat::test_that("make_trait_artifact_manifest() records relative paths", {
  path_fixture <-
    withr::local_tempdir()

  vec_folders <-
    base::file.path(
      path_fixture,
      c("Data", "config/trait_units", "Outputs/Data")
    )

  purrr::walk(
    .x = vec_folders,
    .f = ~ base::dir.create(.x, recursive = TRUE)
  )

  base::writeLines("input", base::file.path(vec_folders[1], "input.txt"))

  base::writeLines("alias", base::file.path(vec_folders[2], "aliases.csv"))

  base::writeLines("output", base::file.path(vec_folders[3], "part_0.qs"))

  res <-
    make_trait_artifact_manifest(
      input_paths = base::file.path(vec_folders[1], "input.txt"),
      registry_directory = vec_folders[2],
      output_paths = base::file.path(vec_folders[3], "part_0.qs"),
      root_directory = path_fixture
    )

  testthat::expect_equal(
    dplyr::pull(res, path),
    c(
      "Data/input.txt",
      "config/trait_units/aliases.csv",
      "Outputs/Data/part_0.qs"
    )
  )
  testthat::expect_equal(
    dplyr::pull(res, artifact_role),
    c("input", "registry", "output")
  )
})

testthat::test_that("make_trait_artifact_manifest() rejects outside files", {
  path_fixture <-
    withr::local_tempdir()

  path_elsewhere <-
    withr::local_tempdir()

  path_registry <-
    base::file.path(path_fixture, "registry")

  base::dir.create(path_registry)

  base::writeLines("alias", base::file.path(path_registry, "aliases.csv"))

  path_inside <-
    base::file.path(path_fixture, "input.txt")

  path_outside <-
    base::file.path(path_elsewhere, "outside.txt")

  base::writeLines("input", path_inside)

  base::writeLines("input", path_outside)

  testthat::expect_error(
    make_trait_artifact_manifest(
      input_paths = c(path_inside, path_outside),
      registry_directory = path_registry,
      root_directory = path_fixture
    ),
    "outside.txt"
  )
  testthat::expect_error(
    make_trait_artifact_manifest(
      input_paths = path_inside,
      registry_directory = path_elsewhere,
      root_directory = path_fixture
    ),
    "outside.txt"
  )
})

testthat::test_that("make_trait_artifact_manifest() records every file", {
  path_fixture <-
    withr::local_tempdir()

  path_registry <-
    base::file.path(path_fixture, "registry")

  base::dir.create(path_registry)

  base::writeLines("alias", base::file.path(path_registry, "aliases.csv"))

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
      registry_directory = path_registry,
      output_paths = vec_output_paths,
      output_rows = c(3L, 4L),
      data_contract = data_contract_fixture,
      root_directory = path_fixture
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
    dplyr::pull(data_outputs, path),
    c("part_0.qs", "part_1.qs")
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

  path_registry <-
    base::file.path(path_fixture, "registry")

  base::dir.create(path_registry)

  base::writeLines("alias", base::file.path(path_registry, "aliases.csv"))

  path_input <-
    base::file.path(path_fixture, "input.txt")

  base::writeLines("input", path_input)

  res <-
    make_trait_artifact_manifest(
      input_paths = path_input,
      registry_directory = path_registry,
      root_directory = path_fixture
    )

  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, n_rows))))
  testthat::expect_true(
    base::all(base::is.na(dplyr::pull(res, contract_rows)))
  )
  testthat::expect_true(
    base::all(base::is.na(dplyr::pull(res, contract_schema)))
  )
})
