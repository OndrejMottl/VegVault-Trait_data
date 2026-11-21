#----------------------------------------------------------#
#
#
#                     Trait data
#
#                  Data export pipeline
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Defines the data export target graph: the TRY artefact and the BIEN
#   artefact in numbered parts, each checked against its VegVault contract,
#   then the manifests and aggregate tables, and the private audit files of
#   the unpublished records.
# Run with:
#   R/Analyses/03_Data_export/00_run.R
# Sourcing this script only declares targets; it does not execute them.
# The `dev` profile (R_CONFIG_ACTIVE=dev) writes every export to the
#   git-ignored `Outputs/Data_temp/` instead of the tracked outputs.


#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)

# The published BIEN records hold about 10 million rows, so every target
#   is stored with `qs2` (format "qs") and dropped from memory once its
#   dependants have run
targets::tar_option_set(
  seed = config_seed,
  format = "qs",
  memory = "transient",
  garbage_collection = 1L
)

dir_export_data <-
  here::here(
    load_config_value(value = c("export", "dir_data"))
  )

dir_export_tables <-
  here::here(
    load_config_value(value = c("export", "dir_tables"))
  )

dir_export_audit <-
  here::here(
    load_config_value(value = c("export", "dir_audit"))
  )

n_bien_parts <-
  load_config_value(value = c("export", "n_bien_parts"))

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

# The project's copy of the VegVault contracts (see
#   Data/Contracts/contract_copy.yml)
path_contract_try_file <-
  here::here("Data", "Contracts", "try_traits.yml")

path_contract_bien_file <-
  here::here("Data", "Contracts", "bien_traits.yml")


#----------------------------------------------------------#
# 1. Upstream contract -----
#----------------------------------------------------------#

# Only these public targets of the upstream stores are imported. The raw
#   files come from the acquisition store, for the manifests.
vec_public_targets_data_acquisition <-
  c(
    "path_try_raw_file",
    "path_bien_raw_file"
  )

vec_public_targets_data_processing <-
  c(
    "data_try_published",
    "data_try_resolved",
    "list_try_tables",
    "data_bien_published",
    "data_bien_resolved",
    "list_bien_tables"
  )

store_data_acquisition <-
  resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = "data_acquisition"
  )

store_data_processing <-
  resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = "data_processing"
  )

runner_data_acquisition <- "R/Analyses/01_Data_acquisition/00_run.R"

runner_data_processing <- "R/Analyses/02_Data_processing/00_run.R"


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
  targets::tar_target(
    name = "fingerprint_data_processing",
    command = compute_target_store_fingerprint(
      store = store_data_processing,
      target_names = vec_public_targets_data_processing,
      runner = runner_data_processing
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Import the path of the TRY request; reruns only when the fingerprint
  #   changes.
  targets::tar_target(
    name = "path_try_raw_file",
    command = {
      fingerprint_data_acquisition

      load_target_store_value(
        store = store_data_acquisition,
        target_name = "path_try_raw_file",
        runner = runner_data_acquisition
      )
    }
  ),
  # Import the paths of the BIEN raw files.
  targets::tar_target(
    name = "path_bien_raw_file",
    command = {
      fingerprint_data_acquisition

      load_target_store_value(
        store = store_data_acquisition,
        target_name = "path_bien_raw_file",
        runner = runner_data_acquisition
      )
    }
  ),
  # Import the published TRY records.
  targets::tar_target(
    name = "data_try_published",
    command = {
      fingerprint_data_processing

      load_target_store_value(
        store = store_data_processing,
        target_name = "data_try_published",
        runner = runner_data_processing
      )
    }
  ),
  # Import every TRY record with its disposition.
  targets::tar_target(
    name = "data_try_resolved",
    command = {
      fingerprint_data_processing

      load_target_store_value(
        store = store_data_processing,
        target_name = "data_try_resolved",
        runner = runner_data_processing
      )
    }
  ),
  # Import the TRY aggregate tables.
  targets::tar_target(
    name = "list_try_tables",
    command = {
      fingerprint_data_processing

      load_target_store_value(
        store = store_data_processing,
        target_name = "list_try_tables",
        runner = runner_data_processing
      )
    }
  ),
  # Import the published BIEN records.
  targets::tar_target(
    name = "data_bien_published",
    command = {
      fingerprint_data_processing

      load_target_store_value(
        store = store_data_processing,
        target_name = "data_bien_published",
        runner = runner_data_processing
      )
    }
  ),
  # Import every BIEN record with its disposition.
  targets::tar_target(
    name = "data_bien_resolved",
    command = {
      fingerprint_data_processing

      load_target_store_value(
        store = store_data_processing,
        target_name = "data_bien_resolved",
        runner = runner_data_processing
      )
    }
  ),
  # Import the BIEN aggregate tables.
  targets::tar_target(
    name = "list_bien_tables",
    command = {
      fingerprint_data_processing

      load_target_store_value(
        store = store_data_processing,
        target_name = "list_bien_tables",
        runner = runner_data_processing
      )
    }
  ),
  # The TRY contract, tracked as a file so that a new copy of the
  #   contracts reruns the check below.
  targets::tar_target(
    name = "path_contract_try",
    command = path_contract_try_file,
    format = "file"
  ),
  # The BIEN contract, tracked as a file for the same reason.
  targets::tar_target(
    name = "path_contract_bien",
    command = path_contract_bien_file,
    format = "file"
  ),
  # The rule files listed in the manifests, tracked as files.
  targets::tar_target(
    name = "path_trait_unit_registry",
    command = dir_trait_unit_registry,
    format = "file"
  ),
  targets::tar_target(
    name = "path_source_quarantine",
    command = path_source_quarantine_file,
    format = "file"
  ),
  targets::tar_target(
    name = "path_bien_shared_sources",
    command = path_bien_shared_sources_file,
    format = "file"
  ),
  # The TRY artefact for VegVault, tracked as a file.
  targets::tar_target(
    name = "path_try_export_saved",
    command = save_trait_export(
      data_source = data_try_published,
      file_name = "data_trait_try",
      dir_output = dir_export_data
    ),
    format = "file"
  ),
  # Public target: the TRY artefact once it passes its VegVault contract;
  #   a violation stops the run before any table is written.
  targets::tar_target(
    name = "path_try_export",
    command = validate_trait_export(
      path_artefact = path_try_export_saved,
      path_contract = path_contract_try
    )
  ),
  # The exact inputs, rule files and output of the TRY export, with
  #   checksums and row counts.
  targets::tar_target(
    name = "data_try_manifest",
    command = build_trait_artifact_manifest(
      input_paths = c(path_try_raw_file, path_source_quarantine),
      registry_directory = path_trait_unit_registry,
      output_paths = path_try_export,
      output_rows = base::nrow(data_try_published),
      data_contract = data_try_resolved
    )
  ),
  # Public target: the TRY aggregate tables and the manifest, written only
  #   after the contract check.
  targets::tar_target(
    name = "path_try_tables",
    command = save_trait_tables(
      list_tables = c(
        list_try_tables,
        list(trait_artifact_manifest_try = data_try_manifest)
      ),
      dir_output = dir_export_tables
    ),
    format = "file"
  ),
  # Public target: the quarantined and dropped TRY records, kept only in
  #   the git-ignored audit folder (TRY licence).
  targets::tar_target(
    name = "path_try_unpublished",
    command = save_unpublished_trait_records(
      data_source = data_try_resolved,
      file_name = "trait_unit_unpublished_try",
      dir_output = dir_export_audit
    ),
    format = "file"
  ),
  # The BIEN artefact in numbered parts, in record-ID order, with the part
  #   check. A deleted part is noticed below; rerun this target with
  #   targets::tar_invalidate("data_bien_parts").
  targets::tar_target(
    name = "data_bien_parts",
    command = save_trait_export_parts(
      data_source = data_bien_published,
      file_stem = "data_traits_bien",
      n_parts = n_bien_parts,
      dir_output = dir_export_data
    )
  ),
  # Public target: the BIEN parts once they pass their VegVault contract,
  #   checked together and tracked as files. The published records are
  #   not in memory during the check.
  targets::tar_target(
    name = "path_bien_export",
    command = validate_trait_export(
      path_artefact = dplyr::pull(data_bien_parts, "path"),
      path_contract = path_contract_bien
    ),
    format = "file"
  ),
  # The exact inputs, rule files and outputs of the BIEN export. The TRY
  #   artefact is an input, because BIEN records duplicated in it are
  #   dropped.
  targets::tar_target(
    name = "data_bien_manifest",
    command = build_trait_artifact_manifest(
      input_paths = c(
        path_bien_raw_file,
        path_try_export,
        path_bien_shared_sources,
        path_source_quarantine
      ),
      registry_directory = path_trait_unit_registry,
      output_paths = path_bien_export,
      output_rows = dplyr::pull(data_bien_parts, "n_rows"),
      data_contract = data_bien_resolved
    )
  ),
  # Public target: the BIEN aggregate tables and the manifest, written only
  #   after the contract check.
  targets::tar_target(
    name = "path_bien_tables",
    command = save_trait_tables(
      list_tables = c(
        list_bien_tables,
        list(trait_artifact_manifest_bien = data_bien_manifest)
      ),
      dir_output = dir_export_tables
    ),
    format = "file"
  ),
  # Public target: the quarantined and dropped BIEN records, kept only in
  #   the git-ignored audit folder (BIEN licence).
  targets::tar_target(
    name = "path_bien_unpublished",
    command = save_unpublished_trait_records(
      data_source = data_bien_resolved,
      file_name = "trait_unit_unpublished_bien",
      dir_output = dir_export_audit
    ),
    format = "file"
  )
)
