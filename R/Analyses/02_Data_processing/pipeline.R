#----------------------------------------------------------#
#
#
#                     Trait data
#
#                Data processing pipeline
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Defines the data processing target graph: the TRY and BIEN records
#   under the trait-unit contract, with one disposition per raw record
#   (published, quarantined or dropped), the reconciliation that checks
#   they add up, the published records, and the aggregate tables.
# Run with:
#   R/Analyses/02_Data_processing/00_run.R
# Sourcing this script only declares targets; it does not execute them.
# BIEN records duplicated in TRY are dropped against the published TRY
#   records, so the TRY chain runs first.


#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)

# The BIEN records hold about 10 million rows, so every target is stored
#   with `qs2` (format "qs") and dropped from memory once its dependants
#   have run
targets::tar_option_set(
  seed = config_seed,
  format = "qs",
  memory = "transient",
  garbage_collection = 1L
)

vec_try_reference_names <-
  load_config_value(value = c("try", "ancillary", "references")) |>
  base::unlist()

vec_try_coordinate_names <-
  load_config_value(value = c("try", "ancillary", "coordinates")) |>
  base::unlist()

vec_bien_columns_text_repaired <-
  load_config_value(value = c("bien", "columns_text_repaired")) |>
  base::unlist()

vec_bien_columns_selected <-
  load_config_value(value = c("bien", "columns_selected")) |>
  base::unlist()

dir_trait_unit_registry <-
  here::here(
    load_config_value(value = c("trait_rules", "unit_registry_dir"))
  )

path_source_quarantine_file <-
  here::here(
    load_config_value(value = c("trait_rules", "source_quarantine"))
  )

path_bien_shared_sources_file <-
  here::here(
    load_config_value(value = c("trait_rules", "bien_shared_sources"))
  )

# Reference columns of the published records whose completeness is
#   summarised; they are columns of the VegVault contracts
vec_try_reference_columns <-
  c("record_reference", "reference_source", "dataset_reference_citation")

vec_bien_reference_columns <-
  c("url_source", "source_citation", "project_pi")


#----------------------------------------------------------#
# 1. Upstream contract -----
#----------------------------------------------------------#

# Only these public targets of the upstream store are imported
vec_public_targets_data_acquisition <-
  c(
    "data_try_trait_selection",
    "data_try_data_id_exclusions",
    "data_try_trait_rows",
    "data_try_ancillary",
    "data_bien_trait_selection",
    "data_bien_raw"
  )

store_data_acquisition <-
  resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = "data_acquisition"
  )

runner_data_acquisition <- "R/Analyses/01_Data_acquisition/00_run.R"


#----------------------------------------------------------#
# 2. Define targets -----
#----------------------------------------------------------#

list(
  # Fingerprint the upstream public targets so that a change upstream
  #   invalidates the imports below; the check itself is cheap metadata.
  targets::tar_target(
    name = "fingerprint_data_acquisition",
    command = compute_target_store_fingerprint(
      store = store_data_acquisition,
      target_names = vec_public_targets_data_acquisition,
      runner = runner_data_acquisition
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Import the TRY trait selection; reruns only when the fingerprint
  #   changes.
  targets::tar_target(
    name = "data_try_trait_selection",
    command = {
      fingerprint_data_acquisition

      load_target_store_value(
        store = store_data_acquisition,
        target_name = "data_try_trait_selection",
        runner = runner_data_acquisition
      )
    }
  ),
  # Import the excluded TRY DataIDs.
  targets::tar_target(
    name = "data_try_data_id_exclusions",
    command = {
      fingerprint_data_acquisition

      load_target_store_value(
        store = store_data_acquisition,
        target_name = "data_try_data_id_exclusions",
        runner = runner_data_acquisition
      )
    }
  ),
  # Import the TRY records of the selected traits.
  targets::tar_target(
    name = "data_try_trait_rows",
    command = {
      fingerprint_data_acquisition

      load_target_store_value(
        store = store_data_acquisition,
        target_name = "data_try_trait_rows",
        runner = runner_data_acquisition
      )
    }
  ),
  # Import the TRY coordinate and reference records.
  targets::tar_target(
    name = "data_try_ancillary",
    command = {
      fingerprint_data_acquisition

      load_target_store_value(
        store = store_data_acquisition,
        target_name = "data_try_ancillary",
        runner = runner_data_acquisition
      )
    }
  ),
  # Import the BIEN trait selection.
  targets::tar_target(
    name = "data_bien_trait_selection",
    command = {
      fingerprint_data_acquisition

      load_target_store_value(
        store = store_data_acquisition,
        target_name = "data_bien_trait_selection",
        runner = runner_data_acquisition
      )
    }
  ),
  # Import the BIEN records.
  targets::tar_target(
    name = "data_bien_raw",
    command = {
      fingerprint_data_acquisition

      load_target_store_value(
        store = store_data_acquisition,
        target_name = "data_bien_raw",
        runner = runner_data_acquisition
      )
    }
  ),
  # The unit registry folder, tracked as a file so that an edited rule
  #   reruns the contract.
  targets::tar_target(
    name = "path_trait_unit_registry",
    command = dir_trait_unit_registry,
    format = "file"
  ),
  # The unit aliases, conversions and derivations.
  targets::tar_target(
    name = "list_trait_unit_registry",
    command = load_trait_unit_registry(
      registry_directory = path_trait_unit_registry
    )
  ),
  # The reviewed source quarantines, tracked as a file.
  targets::tar_target(
    name = "path_source_quarantine",
    command = path_source_quarantine_file,
    format = "file"
  ),
  # Sources whose values are on an impossible scale, with the evidence.
  targets::tar_target(
    name = "data_source_quarantine",
    command = load_trait_rule_table(
      path_file = path_source_quarantine,
      vec_columns_required = c(
        "source_provider",
        "source_trait_id",
        "source_dataset_key",
        "disposition_reason"
      )
    )
  ),
  # The BIEN sources that copy compilations held by TRY, tracked as a file.
  targets::tar_target(
    name = "path_bien_shared_sources",
    command = path_bien_shared_sources_file,
    format = "file"
  ),
  # The listed BIEN sources shared with TRY.
  targets::tar_target(
    name = "data_bien_shared_sources",
    command = load_trait_rule_table(
      path_file = path_bien_shared_sources,
      vec_columns_required = "bien_source_key"
    )
  ),
  # The excluded TRY DataIDs as a vector.
  targets::tar_target(
    name = "vec_try_data_ids_excluded",
    command = dplyr::pull(data_try_data_id_exclusions, "data_id")
  ),
  # Raw TRY records per trait, the base of the reconciliation.
  targets::tar_target(
    name = "data_try_raw_counts",
    command = summarise_trait_raw_counts(
      data_source = data_try_trait_rows,
      col_trait_id = "source_trait_id",
      source_provider = "TRY"
    )
  ),
  # TRY records removed before the unit contract, counted by reason.
  targets::tar_target(
    name = "data_try_dropped_counts",
    command = summarise_try_dropped_counts(
      data_trait_rows = data_try_trait_rows,
      vec_data_ids_excluded = vec_try_data_ids_excluded
    )
  ),
  # TRY records with their references, coordinates and trait selection,
  #   in the form the unit contract reads.
  targets::tar_target(
    name = "data_try_contract_input",
    command = prepare_try_contract_input(
      data_trait_rows = data_try_trait_rows,
      data_ancillary = data_try_ancillary,
      data_trait_selection = data_try_trait_selection,
      vec_data_ids_excluded = vec_try_data_ids_excluded,
      vec_reference_names = vec_try_reference_names,
      vec_coordinate_names = vec_try_coordinate_names
    )
  ),
  # TRY records under the unit contract: converted, verified against TRY's
  #   standardised value, and published or quarantined.
  targets::tar_target(
    name = "data_try_contract",
    command = resolve_trait_unit_contract(
      data_source = data_try_contract_input,
      registry = list_trait_unit_registry
    )
  ),
  # Public target: every TRY record with its final disposition, after the
  #   Leaf Area check, the source quarantines, one value per observation
  #   and trait, and the removal of TRY copies.
  targets::tar_target(
    name = "data_try_resolved",
    command = resolve_try_trait_dispositions(
      data_contract = data_try_contract,
      data_quarantine = data_source_quarantine
    )
  ),
  # TRY records per trait and disposition; stops the run unless published,
  #   quarantined and dropped records add up to the raw records.
  targets::tar_target(
    name = "data_try_reconciliation",
    command = validate_trait_unit_reconciliation(
      data_source = data_try_resolved,
      data_raw_counts = data_try_raw_counts,
      data_dropped_counts = data_try_dropped_counts
    )
  ),
  # Public target: the published TRY records in the column order of the
  #   artefact.
  targets::tar_target(
    name = "data_try_published",
    command = select_try_published_records(data_resolved = data_try_resolved)
  ),
  # TRY counts and value distributions per source, trait and rule.
  targets::tar_target(
    name = "data_try_unit_audit_summary",
    command = summarise_trait_unit_audit(data_source = data_try_resolved)
  ),
  # Completeness of the TRY reference columns of the published records.
  targets::tar_target(
    name = "data_try_reference_completeness",
    command = summarise_trait_reference_completeness(
      data_source = data_try_published,
      reference_columns = vec_try_reference_columns
    )
  ),
  # Published TRY records and datasets per coordinate source, including
  #   those whose coordinates were set missing.
  targets::tar_target(
    name = "data_try_coordinate_summary",
    command = summarise_trait_coordinates(data_source = data_try_published)
  ),
  # Public target: the TRY aggregate tables, named by their file names.
  targets::tar_target(
    name = "list_try_tables",
    command = list(
      trait_unit_audit_summary_try = data_try_unit_audit_summary,
      trait_unit_reconciliation_try = data_try_reconciliation,
      trait_reference_completeness_try = data_try_reference_completeness,
      trait_coordinate_summary_try = data_try_coordinate_summary
    )
  ),
  # Raw BIEN records per trait, the base of the reconciliation.
  targets::tar_target(
    name = "data_bien_raw_counts",
    command = summarise_trait_raw_counts(
      data_source = data_bien_raw,
      col_trait_id = "trait_name",
      source_provider = "BIEN"
    )
  ),
  # BIEN records without a species name, removed before the contract.
  targets::tar_target(
    name = "data_bien_dropped_counts",
    command = summarise_bien_dropped_counts(data_bien_raw = data_bien_raw)
  ),
  # BIEN records with repaired text, checked coordinates and the trait
  #   selection, in the form the unit contract reads.
  targets::tar_target(
    name = "data_bien_contract_input",
    command = prepare_bien_contract_input(
      data_bien_raw = data_bien_raw,
      data_trait_selection = data_bien_trait_selection,
      vec_columns_text_repaired = vec_bien_columns_text_repaired,
      vec_columns_selected = vec_bien_columns_selected
    )
  ),
  # BIEN records under the unit contract.
  targets::tar_target(
    name = "data_bien_contract",
    command = resolve_trait_unit_contract(
      data_source = data_bien_contract_input,
      registry = list_trait_unit_registry
    )
  ),
  # BIEN records after the reviewed source quarantines.
  targets::tar_target(
    name = "data_bien_quarantined",
    command = resolve_source_quarantines(
      data_source = data_bien_contract,
      data_quarantine = data_source_quarantine
    )
  ),
  # Public target: every BIEN record with its final disposition. A record
  #   of a listed shared source with the same measurement in the published
  #   TRY records is dropped; the TRY copy is kept.
  targets::tar_target(
    name = "data_bien_resolved",
    command = resolve_bien_try_duplicates(
      data_bien = data_bien_quarantined,
      data_try_published = data_try_published,
      data_shared_sources = data_bien_shared_sources
    )
  ),
  # BIEN records with a TRY twin per BIEN source, recomputing the rule that
  #   selected the list of shared sources.
  targets::tar_target(
    name = "data_bien_try_twins",
    command = summarise_bien_try_twins(
      data_bien = data_bien_quarantined,
      data_try_published = data_try_published,
      data_shared_sources = data_bien_shared_sources
    )
  ),
  # Sources where the list of shared sources and the data disagree; a
  #   warning names them, and the list needs a review before a release.
  targets::tar_target(
    name = "vec_bien_shared_sources_to_review",
    command = diagnose_bien_shared_sources(
      data_twins = data_bien_try_twins,
      data_shared_sources = data_bien_shared_sources
    )
  ),
  # BIEN records per trait and disposition; stops the run unless they add
  #   up to the raw records.
  targets::tar_target(
    name = "data_bien_reconciliation",
    command = validate_trait_unit_reconciliation(
      data_source = data_bien_resolved,
      data_raw_counts = data_bien_raw_counts,
      data_dropped_counts = data_bien_dropped_counts
    )
  ),
  # Public target: the published BIEN records in record-ID order.
  targets::tar_target(
    name = "data_bien_published",
    command = select_bien_published_records(data_resolved = data_bien_resolved)
  ),
  # BIEN counts and value distributions per source, trait and rule.
  targets::tar_target(
    name = "data_bien_unit_audit_summary",
    command = summarise_trait_unit_audit(data_source = data_bien_resolved)
  ),
  # Completeness of the BIEN reference columns, which BIEN gives once per
  #   source, not per measurement.
  targets::tar_target(
    name = "data_bien_reference_completeness",
    command = summarise_trait_reference_completeness(
      data_source = data_bien_published,
      reference_columns = vec_bien_reference_columns
    )
  ),
  # Published BIEN records and datasets per coordinate source.
  targets::tar_target(
    name = "data_bien_coordinate_summary",
    command = summarise_trait_coordinates(data_source = data_bien_published)
  ),
  # Public target: the BIEN aggregate tables, named by their file names.
  targets::tar_target(
    name = "list_bien_tables",
    command = list(
      trait_unit_audit_summary_bien = data_bien_unit_audit_summary,
      trait_unit_reconciliation_bien = data_bien_reconciliation,
      trait_bien_try_twins_by_source = data_bien_try_twins,
      trait_reference_completeness_bien = data_bien_reference_completeness,
      trait_coordinate_summary_bien = data_bien_coordinate_summary
    )
  )
)
