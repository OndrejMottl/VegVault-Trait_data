testthat::test_that("the tracked trait-unit registry is valid and typed", {
  list_registry_observed <-
    read_trait_unit_registry()

  testthat::expect_named(
    list_registry_observed,
    c("aliases", "conversions", "derivations")
  )
  testthat::expect_type(
    dplyr::pull(list_registry_observed$conversions, "multiplier"),
    "double"
  )
  testthat::expect_type(
    dplyr::pull(
      list_registry_observed$conversions,
      "verify_source_standardized"
    ),
    "logical"
  )
  testthat::expect_false(
    base::anyDuplicated(
      dplyr::pull(list_registry_observed$aliases, "original_unit")
    ) > 0L
  )
})

testthat::test_that("incomplete and duplicate registries fail closed", {
  path_registry_incomplete <-
    withr::local_tempdir()
  testthat::expect_error(
    read_trait_unit_registry(path_registry_incomplete),
    "incomplete"
  )

  path_registry_duplicate_alias <-
    withr::local_tempdir()
  vec_alias_source_paths <-
    base::list.files(
      here::here("config", "trait_units"),
      full.names = TRUE
    )
  vec_alias_fixture_paths <-
    base::file.path(
      path_registry_duplicate_alias,
      base::basename(vec_alias_source_paths)
    )
  vec_alias_files_copied <-
    base::file.copy(
      from = vec_alias_source_paths,
      to = vec_alias_fixture_paths
    )
  testthat::expect_true(base::all(vec_alias_files_copied))
  path_alias_file <-
    base::file.path(path_registry_duplicate_alias, "unit_aliases.csv")
  text_alias_registry <-
    base::readLines(path_alias_file)
  text_alias_registry_duplicate <-
    c(text_alias_registry, text_alias_registry[[2L]])
  base::writeLines(text_alias_registry_duplicate, path_alias_file)

  testthat::expect_error(
    read_trait_unit_registry(path_registry_duplicate_alias),
    "aliases must be unique"
  )

  path_registry_duplicate_conversion <-
    withr::local_tempdir()
  vec_conversion_source_paths <-
    base::list.files(
      here::here("config", "trait_units"),
      full.names = TRUE
    )
  vec_conversion_fixture_paths <-
    base::file.path(
      path_registry_duplicate_conversion,
      base::basename(vec_conversion_source_paths)
    )
  vec_conversion_files_copied <-
    base::file.copy(
      from = vec_conversion_source_paths,
      to = vec_conversion_fixture_paths
    )
  testthat::expect_true(base::all(vec_conversion_files_copied))
  path_conversion_file <-
    base::file.path(
      path_registry_duplicate_conversion,
      "unit_conversions.csv"
    )
  text_conversion_registry <-
    base::readLines(path_conversion_file)
  text_conversion_registry_duplicate <-
    c(text_conversion_registry, text_conversion_registry[[2L]])
  base::writeLines(
    text_conversion_registry_duplicate,
    path_conversion_file
  )

  testthat::expect_error(
    read_trait_unit_registry(path_registry_duplicate_conversion),
    "conversion registry keys must be unique"
  )
})
