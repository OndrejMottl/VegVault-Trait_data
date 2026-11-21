build_repeated_try_fixture <- function(
  value_kind_name,
  orig_obs_data_id = NA_real_,
  obs_data_id = base::seq_along(value_kind_name),
  disposition = "published",
  source_record_id = "obs-1",
  source_trait_id = "3106"
) {
  res <-
    tibble::tibble(
      source_provider = "TRY",
      source_record_id = source_record_id,
      source_trait_id = source_trait_id,
      value_kind_name = value_kind_name,
      orig_obs_data_id = orig_obs_data_id,
      obs_data_id = obs_data_id,
      disposition = disposition,
      disposition_reason = dplyr::if_else(
        disposition == "published",
        NA_character_,
        "original_value_not_numeric"
      )
    )

  return(res)
}

testthat::test_that("resolve_repeated_try_values() checks columns", {
  data_fixture <-
    build_repeated_try_fixture(value_kind_name = "Single") |>
    dplyr::select(-"value_kind_name")

  testthat::expect_error(
    resolve_repeated_try_values(data_fixture),
    "value_kind_name"
  )
})

testthat::test_that("resolve_repeated_try_values() keeps rows and columns", {
  data_fixture <-
    build_repeated_try_fixture(
      value_kind_name = c("Maximum", "Single", "Minimum")
    )

  res <-
    resolve_repeated_try_values(data_fixture)

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_named(res, base::names(data_fixture))
  testthat::expect_equal(base::nrow(res), 3L)
  testthat::expect_equal(
    dplyr::pull(res, obs_data_id),
    dplyr::pull(data_fixture, obs_data_id)
  )
})

testthat::test_that("resolve_repeated_try_values() prefers single values", {
  res <-
    build_repeated_try_fixture(
      value_kind_name = c("Maximum", "Single")
    ) |>
    resolve_repeated_try_values()

  testthat::expect_equal(
    dplyr::pull(res, disposition),
    c("dropped", "published")
  )
  testthat::expect_equal(
    dplyr::pull(res, disposition_reason),
    c("repeated_in_observation", NA_character_)
  )
})

testthat::test_that("resolve_repeated_try_values() prefers central values", {
  res <-
    build_repeated_try_fixture(
      value_kind_name = c("Maximum", "Minimum", "Mean")
    ) |>
    resolve_repeated_try_values()

  testthat::expect_equal(
    dplyr::pull(res, disposition),
    c("dropped", "dropped", "published")
  )

  res_missing_kind <-
    build_repeated_try_fixture(
      value_kind_name = c("High", "")
    ) |>
    resolve_repeated_try_values()

  testthat::expect_equal(
    dplyr::pull(res_missing_kind, disposition),
    c("dropped", "published")
  )
})

testthat::test_that("resolve_repeated_try_values() avoids TRY duplicates", {
  res <-
    build_repeated_try_fixture(
      value_kind_name = c("Single", "Single"),
      orig_obs_data_id = c(99, NA_real_)
    ) |>
    resolve_repeated_try_values()

  testthat::expect_equal(
    dplyr::pull(res, disposition),
    c("dropped", "published")
  )
})

testthat::test_that("resolve_repeated_try_values() breaks ties by record", {
  res <-
    build_repeated_try_fixture(
      value_kind_name = c("Single", "Single", "Single"),
      obs_data_id = c(30, 10, 20)
    ) |>
    resolve_repeated_try_values()

  testthat::expect_equal(
    dplyr::pull(res, disposition),
    c("dropped", "published", "dropped")
  )
})

testthat::test_that("resolve_repeated_try_values() skips quarantined rows", {
  res <-
    build_repeated_try_fixture(
      value_kind_name = c("Single", "Mean"),
      disposition = c("quarantined", "published")
    ) |>
    resolve_repeated_try_values()

  testthat::expect_equal(
    dplyr::pull(res, disposition),
    c("quarantined", "published")
  )
  testthat::expect_equal(
    dplyr::pull(res, disposition_reason),
    c("original_value_not_numeric", NA_character_)
  )
})

testthat::test_that("resolve_repeated_try_values() groups by key", {
  data_fixture <-
    dplyr::bind_rows(
      build_repeated_try_fixture(
        value_kind_name = "Maximum",
        source_record_id = "obs-1"
      ),
      build_repeated_try_fixture(
        value_kind_name = "Single",
        source_record_id = "obs-2"
      ),
      build_repeated_try_fixture(
        value_kind_name = "Single",
        source_record_id = "obs-1",
        source_trait_id = "26"
      )
    )

  res <-
    resolve_repeated_try_values(data_fixture)

  testthat::expect_equal(
    dplyr::pull(res, disposition),
    base::rep("published", 3L)
  )
})

testthat::test_that("resolve_repeated_try_values() leaves one per key", {
  base::set.seed(900723)

  data_fixture <-
    build_repeated_try_fixture(
      value_kind_name = base::sample(
        c("Single", "Mean", "Maximum", "Minimum", ""),
        size = 2000L,
        replace = TRUE
      ),
      orig_obs_data_id = base::sample(
        c(NA_real_, 1),
        size = 2000L,
        replace = TRUE
      ),
      source_record_id = base::sample(
        base::as.character(1:300),
        size = 2000L,
        replace = TRUE
      )
    )

  res <-
    resolve_repeated_try_values(data_fixture)

  data_published <-
    res |>
    dplyr::filter(.data$disposition == "published")

  testthat::expect_equal(
    base::nrow(data_published),
    dplyr::n_distinct(dplyr::pull(data_fixture, source_record_id))
  )
  testthat::expect_equal(
    base::nrow(res |> dplyr::filter(.data$disposition == "dropped")),
    2000L - base::nrow(data_published)
  )
})
