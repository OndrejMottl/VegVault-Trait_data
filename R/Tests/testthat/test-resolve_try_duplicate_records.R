build_try_duplicate_fixture <- function(
  obs_data_id,
  orig_obs_data_id,
  disposition = "published"
) {
  res <-
    tibble::tibble(
      source_provider = "TRY",
      obs_data_id = obs_data_id,
      orig_obs_data_id = orig_obs_data_id,
      disposition = disposition,
      disposition_reason = dplyr::if_else(
        disposition == "published",
        NA_character_,
        "original_unit_not_registered"
      )
    )

  return(res)
}

testthat::test_that("resolve_try_duplicate_records() checks columns", {
  data_fixture <-
    build_try_duplicate_fixture(
      obs_data_id = 1,
      orig_obs_data_id = NA_real_
    ) |>
    dplyr::select(-"orig_obs_data_id")

  testthat::expect_error(
    resolve_try_duplicate_records(data_fixture),
    "orig_obs_data_id"
  )
})

testthat::test_that("resolve_try_duplicate_records() keeps the original", {
  data_fixture <-
    build_try_duplicate_fixture(
      obs_data_id = c(1, 2, 3),
      orig_obs_data_id = c(NA_real_, 1, NA_real_)
    )

  res <-
    resolve_try_duplicate_records(data_fixture)

  testthat::expect_named(res, base::names(data_fixture))
  testthat::expect_equal(base::nrow(res), 3L)
  testthat::expect_equal(
    dplyr::pull(res, disposition),
    c("published", "dropped", "published")
  )
  testthat::expect_equal(
    dplyr::pull(res, disposition_reason),
    c(NA_character_, "duplicate_of_try_record", NA_character_)
  )
})

testthat::test_that("resolve_try_duplicate_records() keeps one copy", {
  res_quarantined_original <-
    build_try_duplicate_fixture(
      obs_data_id = c(1, 2),
      orig_obs_data_id = c(NA_real_, 1),
      disposition = c("quarantined", "published")
    ) |>
    resolve_try_duplicate_records()

  testthat::expect_equal(
    dplyr::pull(res_quarantined_original, disposition),
    c("quarantined", "published")
  )

  res_absent_original <-
    build_try_duplicate_fixture(
      obs_data_id = c(2, 3),
      orig_obs_data_id = c(99, 99)
    ) |>
    resolve_try_duplicate_records()

  testthat::expect_equal(
    dplyr::pull(res_absent_original, disposition),
    c("published", "published")
  )
})

testthat::test_that("resolve_try_duplicate_records() drops all copies", {
  res <-
    build_try_duplicate_fixture(
      obs_data_id = c(1, 2, 3, 4),
      orig_obs_data_id = c(NA_real_, 1, 1, 1),
      disposition = c("published", "published", "published", "quarantined")
    ) |>
    resolve_try_duplicate_records()

  testthat::expect_equal(
    dplyr::pull(res, disposition),
    c("published", "dropped", "dropped", "quarantined")
  )
})

testthat::test_that("resolve_try_duplicate_records() rejects chains", {
  data_fixture <-
    build_try_duplicate_fixture(
      obs_data_id = c(1, 2, 3),
      orig_obs_data_id = c(NA_real_, 1, 2)
    )

  testthat::expect_error(
    resolve_try_duplicate_records(data_fixture),
    "chain"
  )
})
