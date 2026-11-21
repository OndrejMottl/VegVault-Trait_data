#----------------------------------------------------------#
# Fixture -----
#----------------------------------------------------------#

# The 3,632 known Leaf Area corrections, each in its own observation, and
#   five other records: one of a quarantined source, a repeated value
#   (Single and Maximum in observation 20) and a copy of a published
#   record (obs_data_id 32 flags 31).
data_try_fixture_leaf_area <-
  tibble::tibble(
    source_provider = "TRY",
    source_trait_id = "3108",
    source_dataset_key = "LEAF",
    source_record_id = base::as.character(base::seq_len(3632L) + 1000L),
    disposition = "published",
    disposition_reason = NA_character_,
    value_kind_name = "Single",
    orig_obs_data_id = NA_real_,
    obs_data_id = base::as.numeric(base::seq_len(3632L) + 1000L),
    conversion_rule_key = "try_leaf_area_below_cm2",
    trait_unit = "mm2",
    original_value = base::as.numeric(base::seq_len(3632L)),
    trait_value = base::as.numeric(base::seq_len(3632L)) * 100
  )

data_try_fixture_others <-
  tibble::tibble(
    source_provider = "TRY",
    source_trait_id = "4",
    source_dataset_key = c("DQ", "D2", "D2", "D3", "D4"),
    source_record_id = c("10", "20", "20", "30", "40"),
    disposition = "published",
    disposition_reason = NA_character_,
    value_kind_name = c("Single", "Single", "Maximum", "Single", "Single"),
    orig_obs_data_id = c(NA, NA, NA, NA, 31),
    obs_data_id = c(11, 21, 22, 31, 32),
    conversion_rule_key = "try_density",
    trait_unit = "g/cm3",
    original_value = 0.5,
    trait_value = 0.5
  )

data_try_fixture_contract <-
  dplyr::bind_rows(data_try_fixture_others, data_try_fixture_leaf_area)

data_try_fixture_quarantine <-
  tibble::tibble(
    source_provider = "TRY",
    source_trait_id = "4",
    source_dataset_key = "DQ",
    disposition_reason = "source_scale_implausible"
  )


#----------------------------------------------------------#
# Tests -----
#----------------------------------------------------------#

testthat::test_that("resolve_try_trait_dispositions() validates inputs", {
  testthat::expect_error(
    resolve_try_trait_dispositions(
      data_contract = "records",
      data_quarantine = data_try_fixture_quarantine
    ),
    "data_contract"
  )
  testthat::expect_error(
    resolve_try_trait_dispositions(
      data_contract = data_try_fixture_contract,
      data_quarantine = "quarantine"
    ),
    "data_quarantine"
  )
})

testthat::test_that("resolve_try_trait_dispositions() checks Leaf Area", {
  testthat::expect_error(
    resolve_try_trait_dispositions(
      data_contract = dplyr::slice_head(
        data_try_fixture_contract,
        n = base::nrow(data_try_fixture_contract) - 1L
      ),
      data_quarantine = data_try_fixture_quarantine
    ),
    "3,632"
  )
})

testthat::test_that("resolve_try_trait_dispositions() applies every rule", {
  res <-
    resolve_try_trait_dispositions(
      data_contract = data_try_fixture_contract,
      data_quarantine = data_try_fixture_quarantine
    )

  testthat::expect_named(res, base::names(data_try_fixture_contract))
  testthat::expect_equal(
    base::nrow(res),
    base::nrow(data_try_fixture_contract)
  )

  data_others <-
    dplyr::slice_head(res, n = 5L)

  testthat::expect_identical(
    dplyr::pull(data_others, "disposition"),
    c("quarantined", "published", "dropped", "published", "dropped")
  )
  testthat::expect_identical(
    dplyr::pull(data_others, "disposition_reason"),
    c(
      "source_scale_implausible",
      NA,
      "repeated_in_observation",
      NA,
      "duplicate_of_try_record"
    )
  )
  testthat::expect_true(
    base::all(dplyr::pull(dplyr::slice(res, -(1:5)), "disposition") ==
      "published")
  )
})
