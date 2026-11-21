#----------------------------------------------------------#
# Fixture -----
#----------------------------------------------------------#

# The name "Diaz" with an i acute, as BIEN delivers it mis-encoded (UTF-8
#   read as Latin-1) and as it reads after the repair
name_bien_fixture_misencoded <-
  stringr::str_c("D", base::intToUtf8(c(195L, 173L)), "az S")

name_bien_fixture_repaired <-
  stringr::str_c("D", base::intToUtf8(237L), "az S")

# Four BIEN records: a seed mass with a mis-encoded PI, a leaf area per dry
#   mass with an impossible latitude and only a URL source, a record
#   without a species name, and a trait outside the selection
data_bien_fixture_raw <-
  tibble::tibble(
    trait_name = c(
      "seed mass", "leaf area per leaf dry mass", "seed mass", "other trait"
    ),
    trait_value = c("1.5", "10", "2", "3"),
    unit = c("mg", "mm2 mg-1", "mg", "x"),
    id = c(11L, 12L, 13L, 14L),
    latitude = c(50, 100, 45, NA),
    longitude = c(14, 14, 10, NA),
    method = c("weighed", NA, NA, NA),
    url_source = c(NA, "http://x", NA, NA),
    source_citation = c("Cite A", NA, NA, NA),
    project_pi = c(name_bien_fixture_misencoded, NA, NA, NA),
    scrubbed_species_binomial = c("Abies alba", "Betula pendula", NA, "Carex"),
    access = "public",
    extra_column = "not kept"
  )

data_bien_fixture_selection <-
  tibble::tibble(
    trait_name = c("leaf area per leaf dry mass", "seed mass"),
    trait_domain = c("Leaf mass per area", "Diaspore mass"),
    trait_full_name = c("leaf mass per area", "seed mass"),
    derivation_rule_key = c("sla_to_lma", NA)
  )

vec_bien_fixture_text <-
  c("method", "url_source", "source_citation", "project_pi")

vec_bien_fixture_selected <-
  c(
    "trait_name", "trait_value", "unit", "id", "longitude", "latitude",
    "method", "url_source", "source_citation", "project_pi",
    "scrubbed_species_binomial", "access"
  )

vec_bien_fixture_canonical <-
  c(
    "source_provider", "source_dataset_key", "source_record_id",
    "source_trait_id", "source_trait_name", "original_value_string",
    "original_unit", "source_standardized_value", "source_standardized_unit",
    "trait_domain", "trait_full_name", "derivation_rule_key"
  )


#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("prepare_bien_contract_input() validates its inputs", {
  testthat::expect_error(
    prepare_bien_contract_input(
      data_bien_raw = dplyr::select(
        data_bien_fixture_raw,
        -"scrubbed_species_binomial"
      ),
      data_trait_selection = data_bien_fixture_selection,
      vec_columns_text_repaired = vec_bien_fixture_text,
      vec_columns_selected = vec_bien_fixture_selected
    ),
    "scrubbed_species_binomial"
  )
  testthat::expect_error(
    prepare_bien_contract_input(
      data_bien_raw = data_bien_fixture_raw,
      data_trait_selection = dplyr::select(
        data_bien_fixture_selection,
        -"trait_domain"
      ),
      vec_columns_text_repaired = vec_bien_fixture_text,
      vec_columns_selected = vec_bien_fixture_selected
    ),
    "trait_domain"
  )
  testthat::expect_error(
    prepare_bien_contract_input(
      data_bien_raw = data_bien_fixture_raw,
      data_trait_selection = data_bien_fixture_selection,
      vec_columns_text_repaired = 1,
      vec_columns_selected = vec_bien_fixture_selected
    ),
    "vec_columns_text_repaired"
  )
  testthat::expect_error(
    prepare_bien_contract_input(
      data_bien_raw = data_bien_fixture_raw,
      data_trait_selection = data_bien_fixture_selection,
      vec_columns_text_repaired = vec_bien_fixture_text,
      vec_columns_selected = character(0)
    ),
    "vec_columns_selected"
  )
})


#----------------------------------------------------------#
# Outputs and functionality -----
#----------------------------------------------------------#

testthat::test_that("prepare_bien_contract_input() returns the input", {
  res <-
    prepare_bien_contract_input(
      data_bien_raw = data_bien_fixture_raw,
      data_trait_selection = data_bien_fixture_selection,
      vec_columns_text_repaired = vec_bien_fixture_text,
      vec_columns_selected = vec_bien_fixture_selected
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c(
      vec_bien_fixture_selected,
      "latitude_source",
      "longitude_source",
      vec_bien_fixture_canonical
    )
  )
  testthat::expect_identical(dplyr::pull(res, "id"), c("11", "12", "14"))
  testthat::expect_identical(
    dplyr::pull(res, "source_record_id"),
    c("11", "12", "14")
  )
  testthat::expect_identical(
    base::unique(dplyr::pull(res, "source_provider")),
    "BIEN"
  )
  testthat::expect_identical(
    dplyr::pull(res, "original_value_string"),
    c("1.5", "10", "3")
  )
  testthat::expect_identical(
    dplyr::pull(res, "source_standardized_value"),
    base::rep(NA_real_, 3L)
  )
})

testthat::test_that("prepare_bien_contract_input() repairs text first", {
  res <-
    prepare_bien_contract_input(
      data_bien_raw = data_bien_fixture_raw,
      data_trait_selection = data_bien_fixture_selection,
      vec_columns_text_repaired = vec_bien_fixture_text,
      vec_columns_selected = vec_bien_fixture_selected
    )

  testthat::expect_identical(
    dplyr::pull(res, "project_pi"),
    c(name_bien_fixture_repaired, NA, NA)
  )
  testthat::expect_identical(
    dplyr::pull(res, "source_dataset_key"),
    c(name_bien_fixture_repaired, "http://x", "BIEN")
  )
})

testthat::test_that("prepare_bien_contract_input() maps the trait selection", {
  res <-
    prepare_bien_contract_input(
      data_bien_raw = data_bien_fixture_raw,
      data_trait_selection = data_bien_fixture_selection,
      vec_columns_text_repaired = vec_bien_fixture_text,
      vec_columns_selected = vec_bien_fixture_selected
    )

  testthat::expect_identical(
    dplyr::pull(res, "trait_domain"),
    c("Diaspore mass", "Leaf mass per area", NA)
  )
  testthat::expect_identical(
    dplyr::pull(res, "trait_full_name"),
    c("seed mass", "leaf mass per area", NA)
  )
  testthat::expect_identical(
    dplyr::pull(res, "derivation_rule_key"),
    c(NA, "sla_to_lma", NA)
  )
  testthat::expect_identical(
    dplyr::pull(res, "source_trait_name"),
    c("seed mass", "leaf area per leaf dry mass", "other trait")
  )
})

testthat::test_that("prepare_bien_contract_input() checks coordinates", {
  res <-
    prepare_bien_contract_input(
      data_bien_raw = data_bien_fixture_raw,
      data_trait_selection = data_bien_fixture_selection,
      vec_columns_text_repaired = vec_bien_fixture_text,
      vec_columns_selected = vec_bien_fixture_selected
    )

  testthat::expect_equal(dplyr::pull(res, "latitude"), c(50, NA, NA))
  testthat::expect_equal(dplyr::pull(res, "longitude"), c(14, NA, NA))
})
