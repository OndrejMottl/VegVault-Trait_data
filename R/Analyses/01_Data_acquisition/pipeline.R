#----------------------------------------------------------#
#
#
#                     Trait data
#
#                Data acquisition pipeline
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Defines the data acquisition target graph: the manually downloaded TRY
#   request with the records of the selected traits, the tracked trait
#   selection tables, and one raw file per BIEN trait.
# Run with:
#   R/Analyses/01_Data_acquisition/00_run.R
# Sourcing this script only declares targets; it does not execute them.
# BIEN is downloaded only when `bien: flag_download` is true in config.yml.


#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)

# The TRY records take several gigabytes in memory, so every target is
#   stored with `qs2` (format "qs") and dropped from memory once its
#   dependants have run
targets::tar_option_set(
  seed = config_seed,
  format = "qs",
  memory = "transient",
  garbage_collection = 1L
)

path_try_raw <-
  base::file.path(
    path_data_storage,
    load_config_value(value = c("try", "raw_file"))
  )

sha256_try_raw <-
  load_config_value(value = c("try", "raw_file_sha256"))

version_try_raw <-
  load_config_value(value = c("try", "raw_file_version"))

vec_try_columns <-
  load_config_value(value = c("try", "columns_selected")) |>
  base::unlist()

vec_try_ancillary_names <-
  load_config_value(value = c("try", "ancillary")) |>
  base::unlist(use.names = FALSE)

path_try_traits_file <-
  here::here(
    load_config_value(value = c("trait_rules", "try_traits"))
  )

path_try_data_id_exclusions_file <-
  here::here(
    load_config_value(value = c("trait_rules", "try_data_id_exclusions"))
  )

path_bien_traits_file <-
  here::here(
    load_config_value(value = c("trait_rules", "bien_traits"))
  )

dir_bien_raw <-
  base::file.path(
    path_data_storage,
    load_config_value(value = c("bien", "raw_dir"))
  )

flag_download_bien <-
  load_config_value(value = c("bien", "flag_download"))

vec_trait_selection_columns <-
  c("trait_domain", "trait_full_name", "derivation_rule_key")


#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # The TRY request, checked against the recorded version. As a file
  #   target, a changed or missing file reruns the check and its readers.
  targets::tar_target(
    name = "path_try_raw_file",
    command = validate_try_raw_file(
      path_file = path_try_raw,
      sha256_expected = sha256_try_raw,
      version_label = version_try_raw
    ),
    format = "file"
  ),
  # The tracked TRY trait selection, tracked as a file so that an edit
  #   reruns the selection.
  targets::tar_target(
    name = "path_try_traits",
    command = path_try_traits_file,
    format = "file"
  ),
  # Public target: the selected TRY traits with their trait domain, TRY
  #   name and derivation rule.
  targets::tar_target(
    name = "data_try_trait_selection",
    command = load_trait_rule_table(
      path_file = path_try_traits,
      vec_columns_required = c("trait_id", vec_trait_selection_columns)
    )
  ),
  # The tracked list of excluded TRY DataIDs, tracked as a file.
  targets::tar_target(
    name = "path_try_data_id_exclusions",
    command = path_try_data_id_exclusions_file,
    format = "file"
  ),
  # Public target: the excluded TRY DataIDs.
  targets::tar_target(
    name = "data_try_data_id_exclusions",
    command = load_trait_rule_table(
      path_file = path_try_data_id_exclusions,
      vec_columns_required = "data_id"
    )
  ),
  # Both TRY tables from one read of the request file, which must not be
  #   held in memory twice.
  targets::tar_target(
    name = "list_try_records",
    command = load_try_records(
      path_file = path_try_raw_file,
      vec_columns = vec_try_columns,
      vec_trait_ids = dplyr::pull(data_try_trait_selection, "trait_id"),
      vec_ancillary_names = vec_try_ancillary_names,
      vec_data_ids_excluded = dplyr::pull(
        data_try_data_id_exclusions,
        "data_id"
      )
    )
  ),
  # Public target: the TRY records of the selected traits, including those
  #   of excluded DataIDs, which are counted as dropped.
  targets::tar_target(
    name = "data_try_trait_rows",
    command = purrr::chuck(list_try_records, "data_trait_rows")
  ),
  # Public target: the coordinate and reference records of the
  #   observations of the selected traits.
  targets::tar_target(
    name = "data_try_ancillary",
    command = purrr::chuck(list_try_records, "data_ancillary")
  ),
  # Public target: TRY trait records per trait and DataID, for reviewing
  #   the DataID exclusions.
  targets::tar_target(
    name = "data_try_data_ids",
    command = summarise_try_data_ids(data_trait_rows = data_try_trait_rows)
  ),
  # The tracked BIEN trait selection, tracked as a file.
  targets::tar_target(
    name = "path_bien_traits",
    command = path_bien_traits_file,
    format = "file"
  ),
  # Public target: the selected BIEN traits with their trait domain, name
  #   and derivation rule.
  targets::tar_target(
    name = "data_bien_trait_selection",
    command = load_trait_rule_table(
      path_file = path_bien_traits,
      vec_columns_required = c("trait_name", vec_trait_selection_columns)
    )
  ),
  # The BIEN trait names, one raw file each.
  targets::tar_target(
    name = "vec_bien_trait_names",
    command = dplyr::pull(data_bien_trait_selection, "trait_name")
  ),
  # Public target: the raw file of each BIEN trait, the newest in the input
  #   folder, or a new download with the download flag. A file placed in
  #   the folder by hand is used only after this target is invalidated.
  targets::tar_target(
    name = "path_bien_raw_file",
    command = resolve_bien_trait_raw_file(
      dir_raw = dir_bien_raw,
      trait_name = vec_bien_trait_names,
      flag_download = flag_download_bien
    ),
    # {targets} parses patterns itself and rejects a namespaced `map()`
    pattern = map(vec_bien_trait_names),
    format = "file"
  ),
  # Public target: the records of every BIEN trait, in the order of the
  #   selection table.
  targets::tar_target(
    name = "data_bien_raw",
    command = load_bien_trait_raw_files(vec_paths = path_bien_raw_file)
  )
)
