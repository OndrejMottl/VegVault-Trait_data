testthat::test_that("summarise_bien_dropped_counts() validates its input", {
  testthat::expect_error(
    summarise_bien_dropped_counts(data_bien_raw = "records"),
    "data_bien_raw"
  )
  testthat::expect_error(
    summarise_bien_dropped_counts(
      data_bien_raw = tibble::tibble(trait_name = "seed mass")
    ),
    "scrubbed_species_binomial"
  )
})

testthat::test_that("summarise_bien_dropped_counts() counts missing names", {
  data_bien_raw <-
    tibble::tibble(
      trait_name = c(
        "seed mass", "seed mass", "leaf area", "seed mass", "leaf area"
      ),
      scrubbed_species_binomial = c(NA, "", "Abies alba", " ", NA)
    )

  res <-
    summarise_bien_dropped_counts(data_bien_raw = data_bien_raw)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("source_trait_id", "n_records", "source_provider", "disposition_reason")
  )
  testthat::expect_identical(
    dplyr::pull(res, "source_trait_id"),
    c("leaf area", "seed mass")
  )
  testthat::expect_identical(dplyr::pull(res, "n_records"), c(1L, 3L))
  testthat::expect_identical(
    base::unique(dplyr::pull(res, "source_provider")),
    "BIEN"
  )
  testthat::expect_identical(
    base::unique(dplyr::pull(res, "disposition_reason")),
    "species_name_missing"
  )
})

testthat::test_that("summarise_bien_dropped_counts() handles no drops", {
  res <-
    summarise_bien_dropped_counts(
      data_bien_raw = tibble::tibble(
        trait_name = "seed mass",
        scrubbed_species_binomial = "Abies alba"
      )
    )

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(
    res,
    c("source_trait_id", "n_records", "source_provider", "disposition_reason")
  )
})
