data_trait_contract_fixture <-
  tibble::tibble(
    source_provider = "TRY",
    source_dataset_key = "general",
    source_record_id = "record-1",
    source_trait_id = "3114",
    source_trait_name = "source trait",
    original_value_string = "2",
    original_unit = "cm2",
    source_standardized_value = 200,
    source_standardized_unit = "mm2",
    trait_domain = "Leaf Area",
    trait_full_name = "Leaf area",
    derivation_rule_key = NA_character_
  )

testthat::test_that("registered leaf-area values are converted once", {
  data_observed_leaf_area <-
    resolve_trait_unit_contract(data_trait_contract_fixture)

  testthat::expect_equal(data_observed_leaf_area$trait_value, 200)
  testthat::expect_equal(data_observed_leaf_area$trait_unit, "mm2")
  testthat::expect_equal(data_observed_leaf_area$conversion_status, "converted")
  testthat::expect_equal(data_observed_leaf_area$disposition, "published")

  testthat::expect_error(
    resolve_trait_unit_contract(data_observed_leaf_area),
    "cannot be applied twice"
  )
})

testthat::test_that("known source defects use explicit overrides", {
  data_trait_contract_override <-
    data_trait_contract_fixture |>
    dplyr::mutate(
      source_dataset_key = "AlpinePlants_Austria",
      source_standardized_value = 2
    )
  data_observed_override <-
    resolve_trait_unit_contract(data_trait_contract_override)

  testthat::expect_equal(data_observed_override$trait_value, 200)
  testthat::expect_equal(
    data_observed_override$conversion_rule_key,
    "try_leaf_area_alpine_cm2"
  )
  testthat::expect_equal(data_observed_override$disposition, "published")
})

testthat::test_that("SLA is standardized before LMA is derived", {
  data_trait_contract_lma <-
    data_trait_contract_fixture |>
    dplyr::mutate(
      source_trait_id = "3116",
      original_value_string = "20",
      original_unit = "cm2/g",
      source_standardized_value = 2,
      source_standardized_unit = "mm2 mg-1",
      derivation_rule_key = "sla_to_lma"
    )
  data_observed_lma <-
    resolve_trait_unit_contract(data_trait_contract_lma)

  testthat::expect_equal(data_observed_lma$converted_value, 2)
  testthat::expect_equal(data_observed_lma$trait_value, 0.5)
  testthat::expect_equal(data_observed_lma$trait_unit, "mg/mm2")
})

testthat::test_that("unsupported and invalid inputs fail closed", {
  data_trait_contract_unsupported <-
    data_trait_contract_fixture |>
    dplyr::mutate(original_unit = "furlong2")
  data_observed_unsupported <-
    resolve_trait_unit_contract(data_trait_contract_unsupported)

  data_trait_contract_invalid <-
    data_trait_contract_fixture |>
    dplyr::mutate(original_value_string = "1-2")
  data_observed_invalid <-
    resolve_trait_unit_contract(data_trait_contract_invalid)

  testthat::expect_equal(
    data_observed_unsupported$conversion_status,
    "unsupported"
  )
  testthat::expect_equal(
    data_observed_unsupported$disposition,
    "quarantined"
  )
  testthat::expect_equal(data_observed_invalid$conversion_status, "failed")
  testthat::expect_equal(data_observed_invalid$disposition, "quarantined")
})

testthat::test_that("missing and blank units are quarantined", {
  data_trait_contract_no_unit <-
    dplyr::bind_rows(
      dplyr::mutate(data_trait_contract_fixture, original_unit = NA),
      dplyr::mutate(
        data_trait_contract_fixture,
        source_record_id = "record-2",
        original_unit = "  "
      )
    )

  data_observed_no_unit <-
    resolve_trait_unit_contract(data_trait_contract_no_unit)

  testthat::expect_equal(
    dplyr::pull(data_observed_no_unit, conversion_status),
    c("ambiguous", "ambiguous")
  )
  testthat::expect_equal(
    dplyr::pull(data_observed_no_unit, disposition),
    c("quarantined", "quarantined")
  )
  testthat::expect_equal(
    dplyr::pull(data_observed_no_unit, disposition_reason),
    c("original_unit_missing", "original_unit_missing")
  )
})

testthat::test_that("TRY unit notation variants are published", {
  # Notation variants observed in TRY request 28498: middle dot,
  #    superscript two, Windows en dash (read as U+0096) and micro sign
  data_try_variants <-
    tibble::tibble(
      source_trait_id = c(
        "4", "4", "14", "14", "3110", "3114",
        "3116", "3116", "3117", "3117", "3117", "3117"
      ),
      original_unit = c(
        "g\u00b7cm-3", "g_cm3-1", "g\u00b7kg-1", "\u00b5g mg-1",
        "mm\u00b2", "mm\u00b2", "m\u00b2/kg", "cm-2 g", "m\u00b2/kg",
        "g m\u00962", "mm2 mg\u00961", "mm\u00b2 mg-1"
      ),
      source_standardized_value = c(
        2, 2, 2, 2, 2, 2, 2, 0.05, 2, 500, 2, 2
      ),
      source_standardized_unit = c(
        "g/cm3", "g/cm3", "mg/g", "mg/g", "mm2", "mm2",
        "mm2 mg-1", "mm2 mg-1", "mm2 mg-1", "mm2 mg-1", "mm2 mg-1",
        "mm2 mg-1"
      )
    ) |>
    dplyr::mutate(
      source_provider = "TRY",
      source_dataset_key = "dataset",
      source_record_id = base::as.character(dplyr::row_number()),
      source_trait_name = .data$source_trait_id,
      original_value_string = "2",
      trait_domain = "domain",
      trait_full_name = .data$source_trait_id,
      derivation_rule_key = dplyr::if_else(
        .data$source_trait_id %in% c("3116", "3117"),
        "sla_to_lma",
        NA_character_
      )
    )

  data_try_contract <-
    resolve_trait_unit_contract(data_try_variants)

  testthat::expect_equal(
    dplyr::pull(data_try_contract, disposition),
    base::rep("published", 12L)
  )
  testthat::expect_equal(
    dplyr::pull(data_try_contract, trait_value),
    c(2, 2, 2, 2, 2, 2, 0.5, 20, 0.5, 0.002, 0.5, 0.5)
  )
})

testthat::test_that("observed BIEN unit strings are published", {
  # Unit strings returned by BIEN database 4.2.8 (R package BIEN 1.2.8)
  #    for the six traits
  data_bien_units <-
    tibble::tibble(
      source_trait_id = c(
        "leaf area",
        "leaf area per leaf dry mass",
        "leaf nitrogen content per leaf dry mass",
        "seed mass",
        "stem wood density",
        "whole plant height"
      ),
      original_unit = c("mm2", "m2.kg-1", "mg.g-1", "mg", "g.cm-3", "m")
    ) |>
    dplyr::mutate(
      source_provider = "BIEN",
      source_dataset_key = "BIEN",
      source_record_id = base::as.character(dplyr::row_number()),
      source_trait_name = .data$source_trait_id,
      original_value_string = "2",
      source_standardized_value = NA_real_,
      source_standardized_unit = NA_character_,
      trait_domain = "domain",
      trait_full_name = .data$source_trait_id,
      derivation_rule_key = dplyr::if_else(
        .data$source_trait_id == "leaf area per leaf dry mass",
        "sla_to_lma",
        NA_character_
      )
    )

  data_bien_contract <-
    resolve_trait_unit_contract(data_bien_units)

  testthat::expect_equal(
    dplyr::pull(data_bien_contract, disposition),
    base::rep("published", 6L)
  )
  testthat::expect_equal(
    dplyr::pull(data_bien_contract, trait_unit),
    c("mm2", "mg/mm2", "mg/g", "mg", "g/cm3", "m")
  )
  testthat::expect_equal(
    dplyr::pull(data_bien_contract, trait_value),
    c(2, 0.5, 2, 2, 2, 2)
  )
})

testthat::test_that("provider reference columns pass through unchanged", {
  vec_reference <-
    c("D\u00edaz S (2016) Leaf traits. Ecology 97: 1-9.", NA)

  data_trait_contract_reference <-
    dplyr::bind_rows(
      data_trait_contract_fixture,
      dplyr::mutate(
        data_trait_contract_fixture,
        source_record_id = "record-2",
        original_unit = NA
      )
    ) |>
    dplyr::mutate(record_reference = vec_reference)

  data_observed_reference <-
    resolve_trait_unit_contract(data_trait_contract_reference) |>
    dplyr::arrange(source_record_id)

  testthat::expect_equal(base::nrow(data_observed_reference), 2L)
  testthat::expect_equal(
    dplyr::pull(data_observed_reference, disposition),
    c("published", "quarantined")
  )
  testthat::expect_identical(
    dplyr::pull(data_observed_reference, record_reference),
    vec_reference
  )
})
