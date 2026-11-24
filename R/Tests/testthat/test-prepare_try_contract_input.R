#----------------------------------------------------------#
# Fixture -----
#----------------------------------------------------------#

# Five TRY trait records: two of observation 1 (with coordinates and both
#   references, one reference given twice), one of an excluded DataID, one
#   without a value, and one of observation 3, which has no ancillary data.
data_try_fixture_traits <-
  tibble::tibble(
    Dataset = c("D1", "D1", "D2", "D2", "D3"),
    AccSpeciesName = c(
      "Abies alba", "Abies alba", "Betula pendula", "Betula pendula", "Carex"
    ),
    ObservationID = c(1L, 1L, 2L, 2L, 3L),
    ObsDataID = c(101L, 102L, 103L, 104L, 105L),
    TraitID = c(4L, 3116L, 4L, 4L, 4L),
    DataID = c(10L, 20L, 2221L, 10L, 10L),
    DataName = c("Wood density", "SLA", "Excluded", "Wood density", "WD"),
    OrigValueStr = c("0.5", "10", "1", NA, "0.7"),
    OrigUnitStr = c("g/cm3", "mm2 mg-1", "g/cm3", "g/cm3", "g/cm3"),
    ValueKindName = c("Single", "Mean", "Single", "Single", "Single"),
    OrigObsDataID = c(NA, NA, NA, NA, NA),
    StdValue = c(0.5, 10, 1, NA, NA),
    UnitName = c("g/cm3", "mm2 mg-1", "g/cm3", "g/cm3", "g/cm3"),
    Reference = c("Ref A", "", "Ref B", "Ref B", "Ref C"),
    source_trait_id = c("4", "3116", "4", "4", "4")
  )

data_try_fixture_ancillary <-
  tibble::tibble(
    Dataset = "D1",
    AccSpeciesName = "Abies alba",
    ObservationID = 1L,
    DataName = c(
      "Latitude",
      "Longitude",
      "Reference / source",
      "Reference / source",
      "Dataset reference (citation)"
    ),
    OrigValueStr = c("50.1", "14.2", "Source X", "Source Y", "Cite Z"),
    StdValue = c(50.1, 14.2, NA, NA, NA)
  )

data_try_fixture_selection <-
  tibble::tibble(
    trait_id = c("4", "3116"),
    trait_domain = c("Stem specific density", "Leaf mass per area"),
    trait_full_name = c("Wood density name", "SLA name"),
    derivation_rule_key = c(NA, "sla_to_lma")
  )

vec_try_fixture_references <-
  c("Reference / source", "Dataset reference (citation)")

vec_try_fixture_output_columns <-
  c(
    "name", "dataset", "acc_species_name", "observation_id", "obs_data_id",
    "orig_value_str", "orig_unit_str", "value_kind_name", "orig_obs_data_id",
    "std_value", "unit_name", "record_reference", "reference_source",
    "dataset_reference_citation", "trait_domain", "trait_full_name",
    "latitude", "longitude", "latitude_source", "longitude_source",
    "source_provider", "source_dataset_key", "source_record_id",
    "source_trait_id", "source_trait_name", "original_value_string",
    "original_unit", "source_standardized_value", "source_standardized_unit",
    "derivation_rule_key"
  )


#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("prepare_try_contract_input() validates its inputs", {
  testthat::expect_error(
    prepare_try_contract_input(
      data_trait_rows = dplyr::select(data_try_fixture_traits, -"Reference"),
      data_ancillary = data_try_fixture_ancillary,
      data_trait_selection = data_try_fixture_selection,
      vec_data_ids_excluded = "2221",
      vec_reference_names = vec_try_fixture_references,
      vec_coordinate_names = c("Latitude", "Longitude")
    ),
    "Reference"
  )
  testthat::expect_error(
    prepare_try_contract_input(
      data_trait_rows = data_try_fixture_traits,
      data_ancillary = dplyr::select(data_try_fixture_ancillary, -"DataName"),
      data_trait_selection = data_try_fixture_selection,
      vec_data_ids_excluded = "2221",
      vec_reference_names = vec_try_fixture_references,
      vec_coordinate_names = c("Latitude", "Longitude")
    ),
    "DataName"
  )
  testthat::expect_error(
    prepare_try_contract_input(
      data_trait_rows = data_try_fixture_traits,
      data_ancillary = data_try_fixture_ancillary,
      data_trait_selection = dplyr::select(
        data_try_fixture_selection,
        -"derivation_rule_key"
      ),
      vec_data_ids_excluded = "2221",
      vec_reference_names = vec_try_fixture_references,
      vec_coordinate_names = c("Latitude", "Longitude")
    ),
    "derivation_rule_key"
  )
  testthat::expect_error(
    prepare_try_contract_input(
      data_trait_rows = data_try_fixture_traits,
      data_ancillary = data_try_fixture_ancillary,
      data_trait_selection = data_try_fixture_selection,
      vec_data_ids_excluded = 2221,
      vec_reference_names = vec_try_fixture_references,
      vec_coordinate_names = c("Latitude", "Longitude")
    ),
    "vec_data_ids_excluded"
  )
})


#----------------------------------------------------------#
# Outputs and functionality -----
#----------------------------------------------------------#

testthat::test_that("prepare_try_contract_input() returns the contract input", {
  res <-
    prepare_try_contract_input(
      data_trait_rows = data_try_fixture_traits,
      data_ancillary = data_try_fixture_ancillary,
      data_trait_selection = data_try_fixture_selection,
      vec_data_ids_excluded = "2221",
      vec_reference_names = vec_try_fixture_references,
      vec_coordinate_names = c("Latitude", "Longitude")
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, vec_try_fixture_output_columns)
  testthat::expect_identical(
    dplyr::pull(res, "obs_data_id"),
    c(101L, 102L, 105L)
  )
  testthat::expect_identical(dplyr::pull(res, "name"), c("4", "3116", "4"))
  testthat::expect_identical(
    dplyr::pull(res, "source_record_id"),
    c("1", "1", "3")
  )
  testthat::expect_identical(
    base::unique(dplyr::pull(res, "source_provider")),
    "TRY"
  )
  testthat::expect_identical(
    dplyr::pull(res, "source_dataset_key"),
    c("D1", "D1", "D3")
  )
})

testthat::test_that("prepare_try_contract_input() maps the trait selection", {
  res <-
    prepare_try_contract_input(
      data_trait_rows = data_try_fixture_traits,
      data_ancillary = data_try_fixture_ancillary,
      data_trait_selection = data_try_fixture_selection,
      vec_data_ids_excluded = "2221",
      vec_reference_names = vec_try_fixture_references,
      vec_coordinate_names = c("Latitude", "Longitude")
    )

  testthat::expect_identical(
    dplyr::pull(res, "trait_domain"),
    c("Stem specific density", "Leaf mass per area", "Stem specific density")
  )
  testthat::expect_identical(
    dplyr::pull(res, "source_trait_name"),
    c("Wood density name", "SLA name", "Wood density name")
  )
  testthat::expect_identical(
    dplyr::pull(res, "derivation_rule_key"),
    c(NA, "sla_to_lma", NA)
  )
  testthat::expect_identical(
    dplyr::pull(res, "original_value_string"),
    c("0.5", "10", "0.7")
  )
  testthat::expect_equal(
    dplyr::pull(res, "source_standardized_value"),
    c(0.5, 10, NA)
  )
})

testthat::test_that("prepare_try_contract_input() joins references", {
  res <-
    prepare_try_contract_input(
      data_trait_rows = data_try_fixture_traits,
      data_ancillary = data_try_fixture_ancillary,
      data_trait_selection = data_try_fixture_selection,
      vec_data_ids_excluded = "2221",
      vec_reference_names = vec_try_fixture_references,
      vec_coordinate_names = c("Latitude", "Longitude")
    )

  testthat::expect_identical(
    dplyr::pull(res, "reference_source"),
    c("Source X", "Source X", NA)
  )
  testthat::expect_identical(
    dplyr::pull(res, "dataset_reference_citation"),
    c("Cite Z", "Cite Z", NA)
  )
  testthat::expect_identical(
    dplyr::pull(res, "record_reference"),
    c("Ref A", NA, "Ref C")
  )
})

testthat::test_that("prepare_try_contract_input() joins coordinates", {
  res <-
    prepare_try_contract_input(
      data_trait_rows = data_try_fixture_traits,
      data_ancillary = data_try_fixture_ancillary,
      data_trait_selection = data_try_fixture_selection,
      vec_data_ids_excluded = "2221",
      vec_reference_names = vec_try_fixture_references,
      vec_coordinate_names = c("Latitude", "Longitude")
    )

  testthat::expect_equal(dplyr::pull(res, "latitude"), c(50.1, 50.1, NA))
  testthat::expect_equal(dplyr::pull(res, "longitude"), c(14.2, 14.2, NA))
})

testthat::test_that("prepare_try_contract_input() works without references", {
  res <-
    prepare_try_contract_input(
      data_trait_rows = data_try_fixture_traits,
      data_ancillary = dplyr::filter(
        data_try_fixture_ancillary,
        .data$DataName %in% c("Latitude", "Longitude")
      ),
      data_trait_selection = data_try_fixture_selection,
      vec_data_ids_excluded = character(0),
      vec_reference_names = vec_try_fixture_references,
      vec_coordinate_names = c("Latitude", "Longitude")
    )

  testthat::expect_named(res, vec_try_fixture_output_columns)
  testthat::expect_equal(base::nrow(res), 4L)
  testthat::expect_true(
    base::all(base::is.na(dplyr::pull(res, "reference_source")))
  )
  testthat::expect_type(
    dplyr::pull(res, "dataset_reference_citation"),
    "character"
  )
})
