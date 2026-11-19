build_bien_duplicate_fixture <- function(
  trait_value,
  url_source = "http://www.kew.org/data/sid",
  species = "Abies alba",
  trait_domain = "Diaspore mass",
  disposition = "published"
) {
  res <-
    tibble::tibble(
      source_provider = "BIEN",
      trait_domain = trait_domain,
      scrubbed_species_binomial = species,
      trait_value = trait_value,
      url_source = url_source,
      source_citation = NA_character_,
      project_pi = "Someone",
      disposition = disposition,
      disposition_reason = dplyr::if_else(
        disposition == "published",
        NA_character_,
        "original_value_not_numeric"
      )
    )

  return(res)
}

data_try_published_fixture <-
  tibble::tibble(
    trait_domain = c("Diaspore mass", "Diaspore mass"),
    acc_species_name = c("Abies alba", "Pinus sylvestris"),
    trait_value = c(12.34, 7)
  )

data_shared_sources_fixture <-
  tibble::tibble(
    bien_source_key = c("http://www.kew.org/data/sid", "Chacon E")
  )

testthat::test_that("resolve_bien_try_duplicates() checks columns", {
  testthat::expect_error(
    resolve_bien_try_duplicates(
      data_bien = dplyr::select(
        build_bien_duplicate_fixture(trait_value = 1),
        -"url_source"
      ),
      data_try_published = data_try_published_fixture,
      data_shared_sources = data_shared_sources_fixture
    ),
    "url_source"
  )
  testthat::expect_error(
    resolve_bien_try_duplicates(
      data_bien = build_bien_duplicate_fixture(trait_value = 1),
      data_try_published = dplyr::select(
        data_try_published_fixture,
        -"acc_species_name"
      ),
      data_shared_sources = data_shared_sources_fixture
    ),
    "acc_species_name"
  )
  testthat::expect_error(
    resolve_bien_try_duplicates(
      data_bien = build_bien_duplicate_fixture(trait_value = 1),
      data_try_published = data_try_published_fixture,
      data_shared_sources = tibble::tibble(source = "x")
    ),
    "bien_source_key"
  )
})

testthat::test_that("resolve_bien_try_duplicates() drops listed twins", {
  data_bien <-
    build_bien_duplicate_fixture(trait_value = c(12.3449, 12.5, 7))

  res <-
    resolve_bien_try_duplicates(
      data_bien = data_bien,
      data_try_published = data_try_published_fixture,
      data_shared_sources = data_shared_sources_fixture
    )

  testthat::expect_named(res, base::names(data_bien))
  testthat::expect_equal(base::nrow(res), 3L)
  testthat::expect_equal(
    dplyr::pull(res, disposition),
    c("dropped", "published", "published")
  )
  testthat::expect_equal(
    dplyr::pull(res, disposition_reason),
    c("duplicate_of_try_record", NA_character_, NA_character_)
  )
})

testthat::test_that("resolve_bien_try_duplicates() needs a listed source", {
  res_unlisted <-
    build_bien_duplicate_fixture(
      trait_value = 12.34,
      url_source = "http://example.org/plots"
    ) |>
    resolve_bien_try_duplicates(
      data_try_published = data_try_published_fixture,
      data_shared_sources = data_shared_sources_fixture
    )

  testthat::expect_equal(dplyr::pull(res_unlisted, disposition), "published")

  res_fallback_key <-
    build_bien_duplicate_fixture(
      trait_value = 12.34,
      url_source = NA_character_
    ) |>
    dplyr::mutate(project_pi = " Chacon E ") |>
    resolve_bien_try_duplicates(
      data_try_published = data_try_published_fixture,
      data_shared_sources = data_shared_sources_fixture
    )

  testthat::expect_equal(
    dplyr::pull(res_fallback_key, disposition),
    "dropped"
  )
})

testthat::test_that("resolve_bien_try_duplicates() matches the full key", {
  res_other_domain <-
    build_bien_duplicate_fixture(
      trait_value = 12.34,
      trait_domain = "Leaf Area"
    ) |>
    resolve_bien_try_duplicates(
      data_try_published = data_try_published_fixture,
      data_shared_sources = data_shared_sources_fixture
    )

  res_other_species <-
    build_bien_duplicate_fixture(
      trait_value = 12.34,
      species = "Abies grandis"
    ) |>
    resolve_bien_try_duplicates(
      data_try_published = data_try_published_fixture,
      data_shared_sources = data_shared_sources_fixture
    )

  res_quarantined <-
    build_bien_duplicate_fixture(
      trait_value = 12.34,
      disposition = "quarantined"
    ) |>
    resolve_bien_try_duplicates(
      data_try_published = data_try_published_fixture,
      data_shared_sources = data_shared_sources_fixture
    )

  testthat::expect_equal(
    dplyr::pull(res_other_domain, disposition),
    "published"
  )
  testthat::expect_equal(
    dplyr::pull(res_other_species, disposition),
    "published"
  )
  testthat::expect_equal(
    dplyr::pull(res_quarantined, disposition),
    "quarantined"
  )
})
