#----------------------------------------------------------#
#
#
#                     Trait data
#
#                Process the BIEN data
#
#
#                      O. Mottl
#                         2023
#
#----------------------------------------------------------#
# Process the BIEN to save as one file with relevant data


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

# Locations. BIEN records duplicated in TRY are dropped, so the published
#   TRY artifact written by 01_TRY.R is an explicit input. A release run
#   passes only that file and writes the release outputs:
#   Rscript R/02_Process_data/01_BIEN_process.R <TRY artifact>
#   A validation run on a subset also passes an input folder and an output
#   folder, which then receives Data/, Tables/ and Audit/:
#   Rscript R/02_Process_data/01_BIEN_process.R <TRY artifact> <input>
#     <output>
vec_script_args <-
  base::commandArgs(trailingOnly = TRUE)

if (
  !base::length(vec_script_args) %in% c(1L, 3L)
) {
  cli::cli_abort(
    stringr::str_c(
      "Pass the TRY artifact, optionally followed by an input folder and",
      " an output folder."
    )
  )
}

path_try_artifact <- vec_script_args[[1]]

path_shared_sources <-
  here::here(
    "config",
    "trait_duplicates",
    "bien_sources_shared_with_try.csv"
  )

if (
  base::length(vec_script_args) == 3L
) {
  path_bien_input <- vec_script_args[[2]]

  path_output_data <-
    base::file.path(vec_script_args[[3]], "Data")

  path_output_tables <-
    base::file.path(vec_script_args[[3]], "Tables")

  path_private_audit <-
    base::file.path(vec_script_args[[3]], "Audit")
} else {
  path_bien_input <-
    here::here("Data", "Input", "BIEN")

  path_output_data <-
    here::here("Outputs", "Data")

  path_output_tables <-
    here::here("Outputs", "Tables")

  path_private_audit <-
    here::here("Data", "Temp", "trait_unit_audit")
}

purrr::walk(
  .x = c(path_output_data, path_output_tables, path_private_audit),
  .f = ~ base::dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

#----------------------------------------------------------#
# 2. Load data  -----
#----------------------------------------------------------#

data_bien_files <-
  path_bien_input %>%
  list.files() %>%
  purrr::map(
    .f = ~ RUtilpol::get_clean_name(.x)
  ) %>%
  unique() %>%
  rlang::set_names() %>%
  purrr::map(
    .f = ~ RUtilpol::get_latest_file(
      file_name = .x,
      dir = path_bien_input
    )
  )

# The exact input files read above, for the manifest
vec_bien_input_files <-
  base::names(data_bien_files) |>
  purrr::map_chr(
    .f = ~ RUtilpol::get_latest_file_name(
      file_name = .x,
      dir = path_bien_input,
      verbose = FALSE
    )
  ) |>
  purrr::discard(.p = base::is.na)

vec_bien_input_paths <-
  base::file.path(
    path_bien_input,
    vec_bien_input_files
  )

#----------------------------------------------------------#
# 3. Process data  -----
#----------------------------------------------------------#

data_bien_raw <-
  data_bien_files %>%
  # discard the NA
  purrr::discard(
    .p = ~ is.na(.x) %>% all()
  ) %>%
  # discard the empty
  purrr::discard(
    .p = ~ is.null(.x)
  ) %>%
  # discard the 0 row dataframe
  purrr::discard(
    .p = ~ nrow(.x) < 1
  ) %>%
  # merge the files
  dplyr::bind_rows() %>%
  tibble::as_tibble() %>%
  janitor::clean_names()

data_bien_raw %>%
  dplyr::slice(1:1e3) %>%
  names()

# Raw BIEN trait records per trait, the base of the reconciliation
data_bien_raw_counts <-
  data_bien_raw |>
  dplyr::count(source_trait_id = .data$trait_name, name = "n_raw") |>
  dplyr::mutate(source_provider = "BIEN")

# BIEN delivers part of its citation, PI and method text mis-encoded (UTF-8
#   read as Latin-1 or Windows-1252, once or twice). The text is repaired
#   before any source key is built from it, so the shared-source and
#   quarantine lists hold repaired text.
data_bien_text_repaired <-
  data_bien_raw |>
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::any_of(
        c("method", "url_source", "source_citation", "project_pi")
      ) &
        dplyr::where(base::is.character),
      .fns = ~ repair_mis_encoded_text(vec_text = .x)
    )
  )

# Records without a scrubbed species name cannot be attached to a taxon in
#   VegVault, so they are dropped before the contract and counted
data_bien_raw_flagged <-
  data_bien_text_repaired |>
  dplyr::mutate(
    flag_species_missing = base::is.na(.data$scrubbed_species_binomial) |
      !base::nzchar(base::trimws(.data$scrubbed_species_binomial))
  )

data_bien_dropped_counts <-
  data_bien_raw_flagged |>
  dplyr::filter(.data$flag_species_missing) |>
  dplyr::count(source_trait_id = .data$trait_name, name = "n_records") |>
  dplyr::mutate(
    source_provider = "BIEN",
    disposition_reason = "species_name_missing"
  )

data_bien_contract <-
  data_bien_raw_flagged %>%
  dplyr::filter(!.data$flag_species_missing) %>%
  dplyr::select(
    dplyr::any_of(
      c(
        "trait_name",
        "trait_value",
        "unit",
        "id",
        "longitude",
        "latitude",
        "method",
        "url_source",
        "source_citation",
        "project_pi",
        "scrubbed_species_binomial",
        "access"
      )
    )
  ) %>%
  # Coordinates out of range, and a coordinate without its pair, become
  #   missing, as in the TRY export; the record itself is kept
  prepare_bien_coordinates() %>%
  dplyr::mutate(
    id = as.character(id),
    source_provider = "BIEN",
    source_dataset_key = dplyr::coalesce(
      .data$project_pi,
      .data$source_citation,
      .data$url_source,
      "BIEN"
    ),
    source_record_id = .data$id,
    source_trait_id = .data$trait_name,
    source_trait_name = .data$trait_name,
    original_value_string = base::as.character(.data$trait_value),
    original_unit = .data$unit,
    source_standardized_value = NA_real_,
    source_standardized_unit = NA_character_,
    trait_domain = dplyr::case_when(
      .data$trait_name == "stem wood density" ~
        "Stem specific density",
      .data$trait_name == "leaf nitrogen content per leaf dry mass" ~
        "Leaf nitrogen content per unit mass",
      .data$trait_name == "seed mass" ~ "Diaspore mass",
      .data$trait_name == "whole plant height" ~ "Plant heigh",
      .data$trait_name == "leaf area" ~ "Leaf Area",
      .data$trait_name == "leaf area per leaf dry mass" ~
        "Leaf mass per area",
      TRUE ~ NA_character_
    ),
    trait_full_name = dplyr::if_else(
      .data$trait_name == "leaf area per leaf dry mass",
      "leaf mass per area",
      .data$trait_name
    ),
    derivation_rule_key = dplyr::if_else(
      .data$trait_name == "leaf area per leaf dry mass",
      "sla_to_lma",
      NA_character_
    )
  ) %>%
  apply_trait_unit_contract()

# BIEN records that copy a compilation also held by TRY are dropped when the
#   published TRY artifact holds the same measurement; the TRY copy is kept
#   (resolve_bien_try_duplicates())
data_try_published <-
  qs2::qs_read(path_try_artifact)

data_shared_sources <-
  readr::read_csv(
    file = path_shared_sources,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )

# Reviewed source-level quarantines (values on an impossible scale) are
#   applied before duplicates are resolved
path_source_quarantine <-
  here::here("config", "trait_quarantine", "source_quarantine.csv")

data_bien_quarantined <-
  resolve_source_quarantines(
    data_source = data_bien_contract,
    data_quarantine = readr::read_csv(
      file = path_source_quarantine,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE,
      progress = FALSE
    )
  )

data_bien_resolved <-
  resolve_bien_try_duplicates(
    data_bien = data_bien_quarantined,
    data_try_published = data_try_published,
    data_shared_sources = data_shared_sources
  )

# Recheck the list of shared sources against the current data: a source
#   that qualifies but is not listed, or the reverse, needs a review of the
#   list before release
data_bien_try_twins <-
  summarise_bien_try_twins(
    data_bien = data_bien_quarantined,
    data_try_published = data_try_published,
    data_shared_sources = data_shared_sources
  )

vec_shared_sources_to_review <-
  c(
    data_bien_try_twins |>
      dplyr::filter(.data$qualifies != .data$listed) |>
      dplyr::pull("bien_source_key"),
    base::setdiff(
      base::trimws(dplyr::pull(data_shared_sources, "bien_source_key")),
      dplyr::pull(data_bien_try_twins, "bien_source_key")
    )
  )

if (
  base::length(vec_shared_sources_to_review) > 0L
) {
  cli::cli_warn(
    c(
      "The list of BIEN sources shared with TRY differs from the data.",
      "i" = "Review {.file {path_shared_sources}}.",
      "x" = "Sources: {vec_shared_sources_to_review}"
    )
  )
}

data_bien_reconciliation <-
  validate_trait_unit_reconciliation(
    data_source = data_bien_resolved,
    data_raw_counts = data_bien_raw_counts,
    data_dropped_counts = data_bien_dropped_counts
  )

data_bien_unnest <-
  data_bien_resolved %>%
  dplyr::filter(.data$disposition == "published")

data_bien_unpublished <-
  data_bien_resolved %>%
  dplyr::filter(.data$disposition != "published")

data_bien_unit_audit_summary <-
  summarise_trait_unit_audit(data_bien_resolved)

# BIEN gives its references once per source, not per measurement
data_bien_reference_completeness <-
  summarise_trait_reference_completeness(
    data_source = data_bien_unnest,
    reference_columns = c("url_source", "source_citation", "project_pi")
  )

# Published records and datasets per coordinate source, including those
#   whose coordinates were set missing
data_bien_coordinate_summary <-
  summarise_trait_coordinates(data_source = data_bien_unnest)

dplyr::glimpse(data_bien_unnest)

# check the percentage of NAs in each column
(data_bien_unnest %>%
  is.na() %>%
  colSums() / nrow(data_bien_unnest)) * 100


#----------------------------------------------------------#
# 4. Save  -----
#----------------------------------------------------------#

# Split the published rows by row order into numbered parts (from 0), each
#   saved to an exact path that the manifest records
n_bien_parts <- 4L

vec_bien_part <-
  base::cut(
    base::seq_len(base::nrow(data_bien_unnest)),
    breaks = n_bien_parts,
    labels = FALSE
  ) - 1L

data_bien_parts <-
  (base::seq_len(n_bien_parts) - 1L) |>
  purrr::map(
    .f = ~ {
      data_part <-
        dplyr::filter(data_bien_unnest, vec_bien_part == .x)

      tibble::tibble(
        part = .x,
        path = save_trait_artifact(
          object_to_save = data_part,
          file_name = stringr::str_glue("data_traits_bien_{.x}"),
          dir = path_output_data
        ),
        n_rows = base::nrow(data_part)
      )
    }
  ) |>
  purrr::list_rbind()

validate_trait_artifact_parts(
  data_parts = data_bien_parts,
  file_stem = "data_traits_bien",
  n_parts_expected = n_bien_parts,
  n_rows_expected = base::nrow(data_bien_unnest)
)

RUtilpol::save_latest_file(
  object_to_save = data_bien_unit_audit_summary,
  file_name = "trait_unit_audit_summary_bien",
  dir = path_output_tables,
  prefered_format = "csv"
)

RUtilpol::save_latest_file(
  object_to_save = data_bien_reconciliation,
  file_name = "trait_unit_reconciliation_bien",
  dir = path_output_tables,
  prefered_format = "csv"
)

RUtilpol::save_latest_file(
  object_to_save = data_bien_try_twins,
  file_name = "trait_bien_try_twins_by_source",
  dir = path_output_tables,
  prefered_format = "csv"
)

RUtilpol::save_latest_file(
  object_to_save = data_bien_reference_completeness,
  file_name = "trait_reference_completeness_bien",
  dir = path_output_tables,
  prefered_format = "csv"
)

RUtilpol::save_latest_file(
  object_to_save = data_bien_coordinate_summary,
  file_name = "trait_coordinate_summary_bien",
  dir = path_output_tables,
  prefered_format = "csv"
)

data_bien_manifest <-
  make_trait_artifact_manifest(
    input_paths = c(
      vec_bien_input_paths,
      path_try_artifact,
      path_shared_sources,
      path_source_quarantine
    ),
    output_paths = dplyr::pull(data_bien_parts, path),
    output_rows = dplyr::pull(data_bien_parts, n_rows),
    data_contract = data_bien_resolved
  )

RUtilpol::save_latest_file(
  object_to_save = data_bien_manifest,
  file_name = "trait_artifact_manifest_bien",
  dir = path_output_tables,
  prefered_format = "csv"
)

# Quarantined and dropped records stay private (BIEN licence)
qs2::qs_save(
  object = data_bien_unpublished,
  file = base::file.path(
    path_private_audit,
    "trait_unit_unpublished_bien.qs2"
  )
)
