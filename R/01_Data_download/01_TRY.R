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

# Read only the columns the export needs, with the read settings of
#   rtry::rtry_import(). The full request (about 17 million rows) must not be
#   held in memory more than once.
vec_try_columns <-
  c(
    "Dataset", "AccSpeciesName", "ObservationID", "ObsDataID", "TraitID",
    "DataID", "DataName", "OrigValueStr", "OrigUnitStr", "ValueKindName",
    "OrigObsDataID", "StdValue", "UnitName", "Reference"
  )

data_trait_raw <-
  data.table::fread(
    file = path_try_input,
    header = TRUE,
    sep = "\t",
    dec = ".",
    encoding = "Latin-1",
    quote = "",
    select = vec_try_columns,
    showProgress = FALSE
  )

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

vec_data_id_to_remove <-
  c(
    2221:2225, 3646, 3647, 3698, 3699, 3727, 3728, 3730, 3731, 3849, 3850,
    4029, 4030
  )

vec_try_trait_ids <-
  trait_name_translation_table |>
  dplyr::pull(trait_id_value)

# Trait records of the requested traits
data_trait_raw_rows <-
  data_trait_raw[
    base::as.character(TraitID) %in% vec_try_trait_ids
  ] |>
  tibble::as_tibble() |>
  dplyr::mutate(
    source_trait_id = base::as.character(.data$TraitID)
  )

vec_trait_observation_ids <-
  data_trait_raw_rows |>
  dplyr::distinct(.data$ObservationID) |>
  dplyr::pull(.data$ObservationID)

# Ancillary records that VegVault reads: coordinates and references. Other
#   ancillary variables (altitude, climate, site descriptions, ...) are not
#   exported.
vec_ancillary_names <-
  c(
    "Latitude",
    "Longitude",
    "Reference / source",
    "Dataset reference (citation)"
  )

data_trait_ancillary <-
  data_trait_raw[
    base::is.na(TraitID) &
      DataName %in% vec_ancillary_names &
      !DataID %in% vec_data_id_to_remove &
      ObservationID %in% vec_trait_observation_ids
  ] |>
  tibble::as_tibble()

base::rm(data_trait_raw)

base::invisible(base::gc())

if (
  base::interactive()
) {
  data_trait_raw_rows %>%
    dplyr::distinct(TraitID, DataID, DataName) %>%
    View()
}

# Raw TRY trait records per trait, and the records removed before the unit
#   contract is applied, counted by reason for the reconciliation
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

# Repeated values of one trait within an observation are kept here and
#   resolved after the unit contract (resolve_repeated_try_values()).
#   `Reference` is TRY's citation of the publication behind each record;
#   it is exported as `record_reference`.
data_trait_values <-
  data_trait_raw_rows |>
  dplyr::filter(
    !.data$DataID %in% vec_data_id_to_remove,
    !base::is.na(.data$OrigValueStr)
  ) |>
  dplyr::mutate(name = .data$source_trait_id) |>
  dplyr::select(
    "name",
    "Dataset",
    "AccSpeciesName",
    "ObservationID",
    "ObsDataID",
    "OrigValueStr",
    "OrigUnitStr",
    "ValueKindName",
    "OrigObsDataID",
    "StdValue",
    "UnitName",
    record_reference = "Reference"
  )

base::rm(data_trait_raw_rows)

base::invisible(base::gc())

# One row of references per observation; the first value is kept when a
#   reference is given twice
data_trait_references <-
  dplyr::bind_rows(
    tibble::tibble(
      `Reference / source` = base::character(0),
      `Dataset reference (citation)` = base::character(0)
    ),
    data_trait_ancillary |>
      dplyr::filter(
        .data$DataName %in% c(
          "Reference / source",
          "Dataset reference (citation)"
        ),
        !base::is.na(.data$OrigValueStr)
      ) |>
      dplyr::distinct(
        .data$Dataset,
        .data$AccSpeciesName,
        .data$ObservationID,
        .data$DataName,
        .keep_all = TRUE
      ) |>
      dplyr::select(
        "Dataset", "AccSpeciesName", "ObservationID", "DataName",
        "OrigValueStr"
      ) |>
      tidyr::pivot_wider(
        names_from = "DataName",
        values_from = "OrigValueStr"
      )
  )

# Observations without any ancillary data (no location or reference) are
#   kept; their ancillary columns stay missing
data_trait_merged <-
  dplyr::left_join(
    data_trait_values,
    data_trait_references,
    by = c("Dataset", "AccSpeciesName", "ObservationID"),
    relationship = "many-to-one"
  )

# Numeric WGS84 coordinates, preferring TRY's standardised values
data_trait_coordinates <-
  data_trait_ancillary |>
  dplyr::filter(.data$DataName %in% c("Latitude", "Longitude")) |>
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
    record_reference = dplyr::na_if(.data$record_reference, ""),
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

# Reviewed source-level quarantines (values on an impossible scale) are
#   applied first. Then one published value per observation and trait is
#   kept (resolve_repeated_try_values()), and records that TRY flags as
#   copies of a published original are dropped
#   (resolve_try_duplicate_records())
path_source_quarantine <-
  here::here("config", "trait_quarantine", "source_quarantine.csv")

data_source_quarantine <-
  readr::read_csv(
    file = path_source_quarantine,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )

data_trait_resolved <-
  data_trait_contract |>
  resolve_source_quarantines(data_quarantine = data_source_quarantine) |>
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

data_trait_reference_completeness <-
  summarise_trait_reference_completeness(
    data_source = data_trait_processed,
    reference_columns = c(
      "record_reference",
      "reference_source",
      "dataset_reference_citation"
    )
  )

# Published records and datasets per coordinate source, including those
#   whose coordinates were set missing (prepare_try_coordinates())
data_trait_coordinate_summary <-
  summarise_trait_coordinates(data_source = data_trait_processed)

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

RUtilpol::save_latest_file(
  object_to_save = data_trait_reference_completeness,
  file_name = "trait_reference_completeness_try",
  dir = path_output_tables,
  prefered_format = "csv"
)

RUtilpol::save_latest_file(
  object_to_save = data_trait_coordinate_summary,
  file_name = "trait_coordinate_summary_try",
  dir = path_output_tables,
  prefered_format = "csv"
)

data_try_manifest <-
  make_trait_artifact_manifest(
    input_paths = c(path_try_input, path_source_quarantine),
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
