testthat::test_that("resolve_bien_trait_raw_file() validates its arguments", {
  path_dir <-
    withr::local_tempdir()

  testthat::expect_error(
    resolve_bien_trait_raw_file(
      dir_raw = base::file.path(path_dir, "missing"),
      trait_name = "seed mass",
      flag_download = FALSE
    ),
    "dir_raw"
  )
  testthat::expect_error(
    resolve_bien_trait_raw_file(
      dir_raw = path_dir,
      trait_name = "",
      flag_download = FALSE
    ),
    "trait_name"
  )
  testthat::expect_error(
    resolve_bien_trait_raw_file(
      dir_raw = path_dir,
      trait_name = "seed mass",
      flag_download = NA
    ),
    "flag_download"
  )
})

testthat::test_that("resolve_bien_trait_raw_file() picks the latest date", {
  path_dir <-
    withr::local_tempdir()

  vec_files <-
    c(
      stringr::str_c(
        "leaf area_2025-08-01__", base::strrep("a", 32), "__.qs"
      ),
      stringr::str_c(
        "leaf area_2026-11-19__", base::strrep("b", 32), "__.qs"
      ),
      stringr::str_c(
        "leaf area per leaf dry mass_2027-01-01__",
        base::strrep("c", 32),
        "__.qs"
      ),
      "leaf area_2027-02-01.csv"
    )

  purrr::walk(
    .x = base::file.path(path_dir, vec_files),
    .f = ~ base::writeLines("raw", con = .x)
  )

  base::dir.create(
    base::file.path(
      path_dir,
      stringr::str_c(
        "leaf area_2028-01-01__", base::strrep("d", 32), "__.qs"
      )
    )
  )

  res <-
    resolve_bien_trait_raw_file(
      dir_raw = path_dir,
      trait_name = "leaf area",
      flag_download = FALSE
    )

  testthat::expect_type(res, "character")
  testthat::expect_length(res, 1L)
  testthat::expect_identical(
    base::basename(res),
    stringr::str_c("leaf area_2026-11-19__", base::strrep("b", 32), "__.qs")
  )
  testthat::expect_true(base::file.exists(res))
})

testthat::test_that("resolve_bien_trait_raw_file() rejects a tied date", {
  path_dir <-
    withr::local_tempdir()

  vec_files <-
    c(
      stringr::str_c(
        "seed mass_2026-11-19__", base::strrep("a", 32), "__.qs"
      ),
      stringr::str_c(
        "seed mass_2026-11-19__", base::strrep("b", 32), "__.qs"
      )
    )

  purrr::walk(
    .x = base::file.path(path_dir, vec_files),
    .f = ~ base::writeLines("raw", con = .x)
  )

  testthat::expect_error(
    resolve_bien_trait_raw_file(
      dir_raw = path_dir,
      trait_name = "seed mass",
      flag_download = FALSE
    ),
    "2026-11-19"
  )
})

testthat::test_that("resolve_bien_trait_raw_file() names the download flag", {
  path_dir <-
    withr::local_tempdir()

  base::writeLines(
    "raw",
    con = base::file.path(
      path_dir,
      stringr::str_c(
        "leaf area per leaf dry mass_2026-11-19__",
        base::strrep("a", 32),
        "__.qs"
      )
    )
  )

  testthat::expect_error(
    resolve_bien_trait_raw_file(
      dir_raw = path_dir,
      trait_name = "leaf area",
      flag_download = FALSE
    ),
    "flag_download"
  )
})

testthat::test_that("resolve_bien_trait_raw_file() downloads when enabled", {
  path_dir <-
    withr::local_tempdir()

  path_old <-
    base::file.path(
      path_dir,
      stringr::str_c(
        "seed mass_2025-08-01__", base::strrep("a", 32), "__.qs"
      )
    )

  base::writeLines("raw", con = path_old)

  testthat::local_mocked_bindings(
    BIEN_trait_trait = function(trait, all.taxonomy, source.citation) {
      res <-
        base::data.frame(trait_name = trait, trait_value = "1.5")

      return(res)
    },
    .package = "BIEN"
  )

  res <-
    resolve_bien_trait_raw_file(
      dir_raw = path_dir,
      trait_name = "seed mass",
      flag_download = TRUE
    )

  testthat::expect_true(base::file.exists(res))
  testthat::expect_false(base::identical(res, path_old))
  testthat::expect_match(
    base::basename(res),
    stringr::str_c(
      "^seed mass_",
      base::format(base::Sys.Date(), "%Y-%m-%d"),
      "__[0-9a-f]{32}__[.]qs$"
    )
  )
  testthat::expect_true(base::file.exists(path_old))
})
