testthat::test_that("manifests retain only existing immutable inputs", {
  path_manifest_fixture <- withr::local_tempdir()
  path_manifest_input <- file.path(path_manifest_fixture, "input.txt")
  path_manifest_output <- file.path(path_manifest_fixture, "output.txt")
  path_manifest_missing <- file.path(path_manifest_fixture, "missing.txt")
  writeLines("input", path_manifest_input)
  writeLines("output", path_manifest_output)

  data_contract_fixture <-
    tibble::tibble(record_id = c("a", "b"), trait_value = c(1, 2))

  data_manifest_observed <-
    make_trait_artifact_manifest(
      input_paths = c(path_manifest_input, path_manifest_missing),
      output_paths = path_manifest_output,
      data_contract = data_contract_fixture
    )

  testthat::expect_false(path_manifest_missing %in% data_manifest_observed$path)
  testthat::expect_true(all(file.exists(data_manifest_observed$path)))
  testthat::expect_true(all(nchar(data_manifest_observed$md5) == 32L))
  testthat::expect_true(all(data_manifest_observed$contract_rows == 2L))
  testthat::expect_true(
    all(data_manifest_observed$contract_schema == "record_id|trait_value")
  )
  testthat::expect_setequal(
    data_manifest_observed$artifact_role,
    c("input", "registry", "output")
  )
})
