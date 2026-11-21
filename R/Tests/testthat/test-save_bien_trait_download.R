testthat::test_that("save_bien_trait_download() validates its arguments", {
  path_dir <-
    withr::local_tempdir()

  testthat::expect_error(
    save_bien_trait_download(
      trait_name = c("seed mass", "leaf area"),
      dir_raw = path_dir
    ),
    "trait_name"
  )
  testthat::expect_error(
    save_bien_trait_download(
      trait_name = "",
      dir_raw = path_dir
    ),
    "trait_name"
  )
  testthat::expect_error(
    save_bien_trait_download(
      trait_name = "seed mass",
      dir_raw = base::file.path(path_dir, "missing")
    ),
    "dir_raw"
  )
})

testthat::test_that("save_bien_trait_download() saves the BIEN records", {
  path_dir <-
    withr::local_tempdir()

  list_calls <- list()

  testthat::local_mocked_bindings(
    BIEN_trait_trait = function(trait, all.taxonomy, source.citation) {
      list_calls <<-
        list(
          trait = trait,
          all.taxonomy = all.taxonomy,
          source.citation = source.citation
        )

      res <-
        base::data.frame(
          trait_name = trait,
          trait_value = c("1.5", "2"),
          id = c(11L, 12L)
        )

      return(res)
    },
    .package = "BIEN"
  )

  res <-
    save_bien_trait_download(
      trait_name = "seed mass",
      dir_raw = path_dir
    )

  testthat::expect_type(res, "character")
  testthat::expect_length(res, 1L)
  testthat::expect_true(base::file.exists(res))
  testthat::expect_match(
    base::basename(res),
    "^seed mass_[0-9]{4}-[0-9]{2}-[0-9]{2}__[0-9a-f]{32}__[.]qs$"
  )
  testthat::expect_identical(
    list_calls,
    list(trait = "seed mass", all.taxonomy = FALSE, source.citation = TRUE)
  )

  data_saved <-
    qs2::qs_read(file = res)

  testthat::expect_s3_class(data_saved, "tbl_df")
  testthat::expect_identical(
    dplyr::pull(data_saved, "trait_value"),
    c("1.5", "2")
  )
})

testthat::test_that("save_bien_trait_download() saves nothing for no rows", {
  path_dir <-
    withr::local_tempdir()

  testthat::local_mocked_bindings(
    BIEN_trait_trait = function(trait, all.taxonomy, source.citation) {
      res <-
        base::data.frame(trait_name = base::character(0))

      return(res)
    },
    .package = "BIEN"
  )

  testthat::expect_error(
    save_bien_trait_download(
      trait_name = "seed mass",
      dir_raw = path_dir
    ),
    "no records"
  )
  testthat::expect_length(base::list.files(path_dir), 0L)
})

testthat::test_that("save_bien_trait_download() rejects a non-table result", {
  path_dir <-
    withr::local_tempdir()

  testthat::local_mocked_bindings(
    BIEN_trait_trait = function(trait, all.taxonomy, source.citation) {
      return(NULL)
    },
    .package = "BIEN"
  )

  testthat::expect_error(
    save_bien_trait_download(
      trait_name = "seed mass",
      dir_raw = path_dir
    ),
    "data frame"
  )
  testthat::expect_length(base::list.files(path_dir), 0L)
})
