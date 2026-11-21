data_bien_twins_fixture <-
  tibble::tibble(
    disposition = c(
      "published", "published", "published", "published", "quarantined",
      "published", "published"
    ),
    trait_domain = "Diaspore mass",
    scrubbed_species_binomial = "Abies alba",
    trait_value = c(1, 2, 3, 10, 1, 1, 50),
    url_source = c(
      "http://www.kew.org/data/sid", "http://www.kew.org/data/sid",
      "http://www.kew.org/data/sid", "http://www.kew.org/data/sid",
      "http://www.kew.org/data/sid", NA, NA
    ),
    source_citation = NA_character_,
    project_pi = c(NA, NA, NA, NA, NA, "Plot PI", "Plot PI")
  )

data_try_twins_fixture <-
  tibble::tibble(
    trait_domain = "Diaspore mass",
    acc_species_name = "Abies alba",
    trait_value = c(1, 2, 3, 10.5)
  )

testthat::test_that("summarise_bien_try_twins() checks its inputs", {
  testthat::expect_error(
    summarise_bien_try_twins(
      data_bien = dplyr::select(data_bien_twins_fixture, -"project_pi"),
      data_try_published = data_try_twins_fixture
    ),
    "project_pi"
  )
  testthat::expect_error(
    summarise_bien_try_twins(
      data_bien = data_bien_twins_fixture,
      data_try_published = data_try_twins_fixture,
      min_share = 2
    ),
    "min_share"
  )
})

testthat::test_that("summarise_bien_try_twins() counts twins by source", {
  res <-
    summarise_bien_try_twins(
      data_bien = data_bien_twins_fixture,
      data_try_published = data_try_twins_fixture,
      min_twins = 2L
    )

  testthat::expect_named(
    res,
    c(
      "bien_source_key", "trait_domains", "n_bien", "n_twin", "n_chance",
      "share_twin", "share_chance", "qualifies", "listed"
    )
  )
  testthat::expect_equal(
    dplyr::pull(res, bien_source_key),
    c("http://www.kew.org/data/sid", "Plot PI")
  )
  testthat::expect_equal(dplyr::pull(res, n_bien), c(4L, 2L))
  testthat::expect_equal(dplyr::pull(res, n_twin), c(3L, 1L))
  testthat::expect_equal(dplyr::pull(res, n_chance), c(1L, 0L))
  testthat::expect_equal(dplyr::pull(res, share_twin), c(0.75, 0.5))
  testthat::expect_equal(dplyr::pull(res, qualifies), c(TRUE, FALSE))
  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, listed))))
})

testthat::test_that("summarise_bien_try_twins() applies the thresholds", {
  res_strict <-
    summarise_bien_try_twins(
      data_bien = data_bien_twins_fixture,
      data_try_published = data_try_twins_fixture,
      min_twins = 4L
    )

  res_excess <-
    summarise_bien_try_twins(
      data_bien = data_bien_twins_fixture,
      data_try_published = data_try_twins_fixture,
      min_twins = 2L,
      min_excess = 0.6
    )

  testthat::expect_equal(dplyr::pull(res_strict, qualifies), c(FALSE, FALSE))
  testthat::expect_equal(dplyr::pull(res_excess, qualifies), c(FALSE, FALSE))
})

testthat::test_that("summarise_bien_try_twins() marks listed sources", {
  res <-
    summarise_bien_try_twins(
      data_bien = data_bien_twins_fixture,
      data_try_published = data_try_twins_fixture,
      data_shared_sources = tibble::tibble(
        bien_source_key = "http://www.kew.org/data/sid"
      )
    )

  testthat::expect_equal(dplyr::pull(res, listed), c(TRUE, FALSE))
})
