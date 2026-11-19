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
    apply_trait_unit_contract(data_trait_contract_fixture)

  testthat::expect_equal(data_observed_leaf_area$trait_value, 200)
  testthat::expect_equal(data_observed_leaf_area$trait_unit, "mm2")
  testthat::expect_equal(data_observed_leaf_area$conversion_status, "converted")
  testthat::expect_equal(data_observed_leaf_area$disposition, "published")

  testthat::expect_error(
    apply_trait_unit_contract(data_observed_leaf_area),
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
    apply_trait_unit_contract(data_trait_contract_override)

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
    apply_trait_unit_contract(data_trait_contract_lma)

  testthat::expect_equal(data_observed_lma$converted_value, 2)
  testthat::expect_equal(data_observed_lma$trait_value, 0.5)
  testthat::expect_equal(data_observed_lma$trait_unit, "mg/mm2")
})

testthat::test_that("unsupported and invalid inputs fail closed", {
  data_trait_contract_unsupported <-
    data_trait_contract_fixture |>
    dplyr::mutate(original_unit = "furlong2")
  data_observed_unsupported <-
    apply_trait_unit_contract(data_trait_contract_unsupported)

  data_trait_contract_invalid <-
    data_trait_contract_fixture |>
    dplyr::mutate(original_value_string = "1-2")
  data_observed_invalid <-
    apply_trait_unit_contract(data_trait_contract_invalid)

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
    apply_trait_unit_contract(data_bien_units)

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
