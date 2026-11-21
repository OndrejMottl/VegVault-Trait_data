testthat::test_that("validate_trait_export() validates its arguments", {
  dir_output <-
    withr::local_tempdir()

  testthat::expect_error(
    validate_trait_export(
      path_artefact = base::file.path(dir_output, "missing.qs"),
      path_contract = here::here("Data", "Contracts", "try_traits.yml")
    ),
    "path_artefact"
  )
  testthat::expect_error(
    validate_trait_export(
      path_artefact = character(0),
      path_contract = here::here("Data", "Contracts", "try_traits.yml")
    ),
    "path_artefact"
  )

  path_artefact <-
    base::file.path(dir_output, "data_trait_try.qs")

  qs2::qs_save(object = tibble::tibble(a = 1), file = path_artefact)

  testthat::expect_error(
    validate_trait_export(
      path_artefact = path_artefact,
      path_contract = base::file.path(dir_output, "missing.yml")
    ),
    "path_contract"
  )
})

testthat::test_that("validate_trait_export() passes a valid TRY artefact", {
  dir_output <-
    withr::local_tempdir()

  list_fixture <-
    build_try_traits_fixture()

  path_artefact <-
    base::file.path(dir_output, base::names(list_fixture))

  qs2::qs_save(object = list_fixture[[1]], file = path_artefact)

  res <-
    validate_trait_export(
      path_artefact = path_artefact,
      path_contract = here::here("Data", "Contracts", "try_traits.yml")
    )

  testthat::expect_identical(res, path_artefact)
})

testthat::test_that("validate_trait_export() checks BIEN parts together", {
  dir_output <-
    withr::local_tempdir()

  list_fixture <-
    build_bien_traits_fixture()

  vec_paths <-
    base::file.path(dir_output, base::names(list_fixture))

  purrr::walk2(
    .x = list_fixture,
    .y = vec_paths,
    .f = ~ qs2::qs_save(object = .x, file = .y)
  )

  res <-
    validate_trait_export(
      path_artefact = vec_paths,
      path_contract = here::here("Data", "Contracts", "bien_traits.yml")
    )

  testthat::expect_identical(res, vec_paths)

  list_broken <-
    build_bien_traits_fixture(flag_broken = TRUE)

  vec_paths_broken <-
    base::file.path(withr::local_tempdir(), base::names(list_broken))

  purrr::walk2(
    .x = list_broken,
    .y = vec_paths_broken,
    .f = ~ qs2::qs_save(object = .x, file = .y)
  )

  testthat::expect_error(
    validate_trait_export(
      path_artefact = vec_paths_broken,
      path_contract = here::here("Data", "Contracts", "bien_traits.yml")
    ),
    class = "producer_contract_error"
  )
})

testthat::test_that("validate_trait_export() rejects a broken artefact", {
  dir_output <-
    withr::local_tempdir()

  list_fixture <-
    build_try_traits_fixture(flag_broken = TRUE)

  path_artefact <-
    base::file.path(dir_output, base::names(list_fixture))

  qs2::qs_save(object = list_fixture[[1]], file = path_artefact)

  testthat::expect_error(
    validate_trait_export(
      path_artefact = path_artefact,
      path_contract = here::here("Data", "Contracts", "try_traits.yml")
    ),
    class = "producer_contract_error"
  )
})

testthat::test_that("validate_trait_export() rejects a wrong file name", {
  dir_output <-
    withr::local_tempdir()

  list_fixture <-
    build_try_traits_fixture()

  path_artefact <-
    base::file.path(dir_output, "data_trait_try_latest.qs")

  qs2::qs_save(object = list_fixture[[1]], file = path_artefact)

  testthat::expect_error(
    validate_trait_export(
      path_artefact = path_artefact,
      path_contract = here::here("Data", "Contracts", "try_traits.yml")
    ),
    class = "producer_contract_error"
  )
})
