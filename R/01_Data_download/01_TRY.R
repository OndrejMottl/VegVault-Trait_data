#----------------------------------------------------------#
#
#
#                     Trait data
#
#                 Load and plit data
#
#
#                       O. Mottl
#                         2023
#
#----------------------------------------------------------#
# Load data donwloaded from TRY, split by individual traits, and save
#   as `qs` with high compression to reduce file size


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

# Locations. Without arguments the script reads the TRY request and writes
#   the release outputs. A validation run on a subset passes an input file
#   and an output folder, which then receives Data/, Tables/ and Audit/:
#   Rscript R/01_Data_download/01_TRY.R <input file> <output folder>
vec_script_args <-
  base::commandArgs(trailingOnly = TRUE)

if (
  base::length(vec_script_args) == 2L
) {
  path_try_input <- vec_script_args[[1]]

  path_output_data <-
    base::file.path(vec_script_args[[2]], "Data")

  path_output_tables <-
    base::file.path(vec_script_args[[2]], "Tables")

  path_private_audit <-
    base::file.path(vec_script_args[[2]], "Audit")
} else if (
  base::length(vec_script_args) == 0L
) {
  path_try_input <-
    here::here("Data", "Input", "TRY", "Raw", "28498.txt")

  path_output_data <-
    here::here("Outputs", "Data")

  path_output_tables <-
    here::here("Outputs", "Tables")

  path_private_audit <-
    here::here("Data", "Temp", "trait_unit_audit")
} else {
  cli::cli_abort(
    "Pass no arguments, or an input file and an output folder."
  )
}

purrr::walk(
  .x = c(path_output_data, path_output_tables, path_private_audit),
  .f = ~ base::dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

#----------------------------------------------------------#
# 2. Load data  -----
#----------------------------------------------------------#

# Data request 28498
# Trait List: 3106, 4, 3108, 3110, 3112, 3114, 3116, 3117, 14, 26

# Units
# 4 =  g/cm3
# 14 =  mg/g
# 26 = mg
# 3106 =  m
# 3108 = mm2
# 3110 = mm2
# 3112 = mm2
# 3114 = mm2
# 3116 = mm2 mg-1
# 3117 = mm2 mg-1

library(rtry)

data_trait_raw <-
  rtry::rtry_import(path_try_input)

#----------------------------------------------------------#
# 3. Data wrangling  -----
#----------------------------------------------------------#

# trait_id_value corespond to TRY trait ID
# trait_domain corespond to trait selection by Diaz et al. (2016)
# trait_full_name corespond to trait name in TRY
trait_name_translation_table <-
  tibble::tribble(
    ~trait_id_value, ~trait_domain, ~trait_full_name,
    4, "Stem specific density", "Stem specific density (SSD, stem dry mass per stem fresh volume) or wood density",
    14, "Leaf nitrogen content per unit mass", "Leaf nitrogen (N) content per leaf dry mass",
    26, "Diaspore mass", "Seed dry mass",
    3106, "Plant heigh", "Plant height vegetative",
    3108, "Leaf Area", "Leaf area (in case of compound leaves: leaf, petiole excluded)",
    3110, "Leaf Area", "Leaf area (in case of compound leaves: leaf, petiole included)",
    3112, "Leaf Area", "Leaf area (in case of compound leaves: leaf, undefined if petiole in- or excluded)",
    3114, "Leaf Area", "Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or exluded)",
    3116, "Leaf mass per area", "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included",
    3117, "Leaf mass per area", "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded)"
  ) %>%
  dplyr::mutate(
    trait_id_value = as.character(trait_id_value)
  )

data_trait_tibble <-
  tibble::as_tibble(data_trait_raw)

data_trait_dummy <-
  data_trait_tibble %>%
  dplyr::distinct(TraitID) %>%
  tidyr::drop_na() %>%
  purrr::chuck("TraitID") %>%
  rlang::set_names() %>%
  tibble::enframe() %>%
  dplyr::arrange(value)

data_trait_observations <-
  data_trait_dummy %>%
  dplyr::mutate(
    sel_observation = purrr::map(
      .progress = TRUE,
      .x = name,
      .f = ~ {
        vec_sel_trait_observation_id <-
          data_trait_tibble %>%
          dplyr::filter(TraitID == .x) %>%
          dplyr::distinct(ObservationID) %>%
          tidyr::drop_na() %>%
          purrr::chuck("ObservationID")

        data_trait_dummy_negate <-
          data_trait_dummy %>%
          dplyr::filter(name != .x)

        data_trait_tibble %>%
          dplyr::filter(
            ObservationID %in% vec_sel_trait_observation_id
          ) %>%
          dplyr::filter(
            !TraitID %in% data_trait_dummy_negate$value
          ) %>%
          return()
      }
    )
  )

data_trait_data_id <-
  data_trait_observations %>%
  dplyr::mutate(
    sel_data_id_overview = purrr::map2(
      .progress = TRUE,
      .x = name,
      .y = sel_observation,
      .f = ~ .y %>%
        dplyr::filter(TraitID == .x) %>%
        dplyr::distinct(DataID, DataName)
    )
  )

if (
  base::interactive()
) {
  data_trait_data_id %>%
    dplyr::select(-sel_observation) %>%
    tidyr::unnest(sel_data_id_overview) %>%
    View()
}

vec_data_id_to_remove <-
  c(
    2221:2225, 3646, 3647, 3698, 3699, 3727, 3728, 3730, 3731, 3849, 3850,
    4029, 4030
  )

# Raw TRY trait records per trait, and the records removed before the unit
#   contract is applied, counted by reason for the reconciliation
vec_try_trait_ids <-
  trait_name_translation_table |>
  dplyr::pull(trait_id_value)

data_trait_raw_rows <-
  data_trait_tibble |>
  dplyr::mutate(
    source_trait_id = base::as.character(.data$TraitID)
  ) |>
  dplyr::filter(.data$source_trait_id %in% vec_try_trait_ids)

data_trait_raw_counts <-
  data_trait_raw_rows |>
  dplyr::count(.data$source_trait_id, name = "n_raw") |>
  dplyr::mutate(source_provider = "TRY")

data_trait_dropped_counts <-
  data_trait_raw_rows |>
  dplyr::mutate(
    disposition_reason = dplyr::case_when(
      .data$DataID %in% vec_data_id_to_remove ~ "data_id_excluded",
      base::is.na(.data$OrigValueStr) ~ "original_value_missing",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!base::is.na(.data$disposition_reason)) |>
  dplyr::count(
    .data$source_trait_id,
    .data$disposition_reason,
    name = "n_records"
  ) |>
  dplyr::mutate(source_provider = "TRY")

vec_trait_observation_ids <-
  data_trait_raw_rows |>
  dplyr::distinct(.data$ObservationID) |>
  dplyr::pull(.data$ObservationID)

data_trait_sel_data_id <-
  data_trait_data_id %>%
  dplyr::select(-sel_observation) %>%
  tidyr::unnest(sel_data_id_overview) %>%
  dplyr::filter(!DataID %in% vec_data_id_to_remove)


data_trait_observations$sel_observation[[1]] %>%
  dplyr::glimpse()

data_trait_values <-
  data_trait_observations %>%
  dplyr::mutate(
    sel_data_trait_values = purrr::map(
      .progress = TRUE,
      .x = sel_observation,
      .f = ~ .x %>%
        dplyr::filter(DataID %in% data_trait_sel_data_id$DataID) %>%
        dplyr::select(
          Dataset,
          AccSpeciesName,
          ObservationID,
          ObsDataID,
          OrigValueStr,
          OrigUnitStr,
          ValueKindName,
          OrigObsDataID,
          StdValue,
          UnitName
        ) %>%
        # Repeated values of one trait within an observation are kept here
        #   and resolved after the unit contract
        #   (resolve_repeated_try_values())
        dplyr::filter(is.na(OrigValueStr) == FALSE)
    )
  )

data_trait_values$sel_data_trait_values[[1]] %>%
  dplyr::glimpse()

data_trait_covariates <-
  data_trait_values %>%
  dplyr::mutate(
    sel_data_covariates = purrr::map(
      .progress = TRUE,
      .x = sel_observation,
      .f = ~ .x %>%
        dplyr::filter(is.na(TraitID)) %>%
        dplyr::filter(!DataID %in% vec_data_id_to_remove) %>%
        dplyr::filter(!DataID %in% data_trait_sel_data_id$DataID) %>%
        dplyr::select(
          Dataset, AccSpeciesName, ObservationID, DataName, OrigValueStr
        ) %>%
        dplyr::distinct(
          Dataset, AccSpeciesName, ObservationID, DataName,
          .keep_all = TRUE
        ) %>%
        dplyr::filter(is.na(OrigValueStr) == FALSE) %>%
        dplyr::group_by(Dataset, AccSpeciesName, ObservationID) %>%
        tidyr::pivot_wider(
          names_from = DataName, values_from = OrigValueStr
        ) %>%
        dplyr::ungroup()
    )
  )

data_trait_covariates$sel_data_covariates[[1]] %>%
  dplyr::glimpse()

# Observations without any ancillary data (no location or reference) are
#   kept; their ancillary columns stay missing
data_trait_merged <-
  dplyr::left_join(
    data_trait_covariates %>%
      dplyr::select(
        name, sel_data_trait_values
      ) %>%
      tidyr::unnest(sel_data_trait_values),
    data_trait_covariates %>%
      dplyr::select(
        name, sel_data_covariates
      ) %>%
      tidyr::unnest(sel_data_covariates),
    by = c("name", "Dataset", "AccSpeciesName", "ObservationID"),
    relationship = "many-to-one"
  )

# Numeric WGS84 coordinates, preferring TRY's standardised values; they
#   replace the text coordinates of the ancillary data
data_trait_coordinates <-
  data_trait_tibble |>
  dplyr::filter(
    base::is.na(.data$TraitID),
    .data$DataName %in% c("Latitude", "Longitude"),
    !.data$DataID %in% vec_data_id_to_remove,
    .data$ObservationID %in% vec_trait_observation_ids
  ) |>
  janitor::clean_names() |>
  prepare_try_coordinates()

data_trait_contract <-
  data_trait_merged %>%
  dplyr::left_join(
    trait_name_translation_table,
    by = c("name" = "trait_id_value")
  ) %>%
  janitor::clean_names() %>%
  dplyr::select(-dplyr::any_of(c("latitude", "longitude"))) %>%
  dplyr::left_join(
    data_trait_coordinates,
    by = c("dataset", "acc_species_name", "observation_id"),
    relationship = "many-to-one"
  ) %>%
  dplyr::mutate(
    source_provider = "TRY",
    source_dataset_key = .data$dataset,
    source_record_id = base::as.character(.data$observation_id),
    source_trait_id = base::as.character(.data$name),
    source_trait_name = .data$trait_full_name,
    original_value_string = .data$orig_value_str,
    original_unit = .data$orig_unit_str,
    source_standardized_value = parse_trait_numeric(.data$std_value),
    source_standardized_unit = .data$unit_name,
    derivation_rule_key = dplyr::if_else(
      .data$trait_domain == "Leaf mass per area",
      "sla_to_lma",
      NA_character_
    )
  ) %>%
  apply_trait_unit_contract()

# The known Leaf Area corrections are checked on the contract output,
#   before repeated values are resolved
validate_known_try_leaf_area(data_trait_contract)

# One published value per observation and trait, by the documented rule in
#   resolve_repeated_try_values(); then records that TRY flags as copies of
#   a published original are dropped (resolve_try_duplicate_records())
data_trait_resolved <-
  data_trait_contract |>
  resolve_repeated_try_values() |>
  resolve_try_duplicate_records()

data_trait_reconciliation <-
  validate_trait_unit_reconciliation(
    data_source = data_trait_resolved,
    data_raw_counts = data_trait_raw_counts,
    data_dropped_counts = data_trait_dropped_counts
  )

data_trait_processed <-
  data_trait_resolved %>%
  dplyr::filter(.data$disposition == "published") %>%
  dplyr::select(-"name") %>%
  dplyr::relocate(
    trait_value,
    trait_unit,
    trait_domain,
    trait_full_name,
    .before = observation_id
  )

data_trait_unpublished <-
  data_trait_resolved %>%
  dplyr::filter(.data$disposition != "published")

data_trait_unit_audit_summary <-
  summarise_trait_unit_audit(data_trait_resolved)

#----------------------------------------------------------#
# 4. Save  -----
#----------------------------------------------------------#

path_try_artifact <-
  save_trait_artifact(
    object_to_save = data_trait_processed,
    file_name = "data_trait_try",
    dir = path_output_data
  )

RUtilpol::save_latest_file(
  object_to_save = data_trait_unit_audit_summary,
  file_name = "trait_unit_audit_summary_try",
  dir = path_output_tables,
  prefered_format = "csv"
)

RUtilpol::save_latest_file(
  object_to_save = data_trait_reconciliation,
  file_name = "trait_unit_reconciliation_try",
  dir = path_output_tables,
  prefered_format = "csv"
)

data_try_manifest <-
  make_trait_artifact_manifest(
    input_paths = path_try_input,
    output_paths = path_try_artifact,
    output_rows = base::nrow(data_trait_processed),
    data_contract = data_trait_resolved
  )

RUtilpol::save_latest_file(
  object_to_save = data_try_manifest,
  file_name = "trait_artifact_manifest_try",
  dir = path_output_tables,
  prefered_format = "csv"
)

# Quarantined and dropped records stay private (TRY licence)
qs2::qs_save(
  object = data_trait_unpublished,
  file = base::file.path(
    path_private_audit,
    "trait_unit_unpublished_try.qs2"
  )
)

#----------------------------------------------------------#
# 5. (Optional) Check the distribution  -----
#----------------------------------------------------------#

data_trait_summary <-
  data_trait_processed %>%
  dplyr::select(trait_domain, trait_value, acc_species_name) %>%
  dplyr::group_by(trait_domain, acc_species_name) %>%
  dplyr::summarise(
    .groups = "drop",
    n = n(),
    mean = mean(trait_value, na.rm = TRUE),
    sd = sd(trait_value, na.rm = TRUE),
    min = min(trait_value, na.rm = TRUE),
    max = max(trait_value, na.rm = TRUE)
  )

data_trait_summary %>%
  dplyr::filter(n > 50) %>%
  split(.$trait_domain) %>%
  purrr::map(
    .f = ~ summary(.x)
  )
