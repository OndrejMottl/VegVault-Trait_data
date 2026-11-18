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

#----------------------------------------------------------#
# 2. Load data  -----
#----------------------------------------------------------#

data_bien_files <-
  here::here(
    "Data/Input/BIEN/"
  ) %>%
  list.files() %>%
  purrr::map(
    .f = ~ RUtilpol::get_clean_name(.x)
  ) %>%
  unique() %>%
  rlang::set_names() %>%
  purrr::map(
    .f = ~ RUtilpol::get_latest_file(
      file_name = .x,
      dir = here::here(
        "Data/Input/BIEN/"
      )
    )
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

data_bien_contract <-
  data_bien_raw %>%
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

data_bien_reconciliation <-
  validate_trait_unit_reconciliation(data_bien_contract)

data_bien_unnest <-
  data_bien_contract %>%
  dplyr::filter(.data$disposition == "published")

data_bien_quarantine <-
  data_bien_contract %>%
  dplyr::filter(.data$disposition == "quarantined")

data_bien_unit_audit_summary <-
  summarise_trait_unit_audit(data_bien_contract)

dplyr::glimpse(data_bien_unnest)

# check the percentage of NAs in each column
(data_bien_unnest %>%
  is.na() %>%
  colSums() / nrow(data_bien_unnest)) * 100


#----------------------------------------------------------#
# 4. Save  -----
#----------------------------------------------------------#

total_rows <- nrow(data_bien_unnest)
n_chunks <- 4L
list_chunk_rows <-
  split(
    seq_len(total_rows),
    cut(
      seq_len(total_rows),
      breaks = n_chunks,
      labels = FALSE
    )
  )

list_chunk_rows %>%
  purrr::iwalk(
    .f = ~ data_bien_unnest %>%
      dplyr::slice(.x) %>%
      RUtilpol::save_latest_file(
        object_to_save = .,
        file_name = paste0(
          "data_traits_bien_",
          base::as.integer(.y) - 1L
        ),
        dir = here::here(
          "Outputs/Data/"
        ),
        prefered_format = "qs"
      )
  )

RUtilpol::save_latest_file(
  object_to_save = data_bien_unit_audit_summary,
  file_name = "trait_unit_audit_summary_bien",
  dir = here::here("Outputs", "Tables"),
  prefered_format = "csv"
)

RUtilpol::save_latest_file(
  object_to_save = data_bien_reconciliation,
  file_name = "trait_unit_reconciliation_bien",
  dir = here::here("Outputs", "Tables"),
  prefered_format = "csv"
)

vec_bien_output_path_inventory <-
  list.files(
    here::here("Outputs", "Data"),
    pattern = "^data_traits_bien_[0-9]+_.*[.]qs$",
    full.names = TRUE
  )
vec_bien_output_paths_latest <-
  vec_bien_output_path_inventory[
    order(file.info(vec_bien_output_path_inventory)$mtime, decreasing = TRUE)
  ] %>%
  head(4L)

data_bien_manifest <-
  make_trait_artifact_manifest(
    input_paths = list.files(
      here::here("Data", "Input", "BIEN"),
      full.names = TRUE
    ),
    output_paths = vec_bien_output_paths_latest,
    data_contract = data_bien_contract
  )

RUtilpol::save_latest_file(
  object_to_save = data_bien_manifest,
  file_name = "trait_artifact_manifest_bien",
  dir = here::here("Outputs", "Tables"),
  prefered_format = "csv"
)

path_private_audit <-
  here::here("Data", "Temp", "trait_unit_audit")

base::dir.create(
  path_private_audit,
  recursive = TRUE,
  showWarnings = FALSE
)

qs2::qs_save(
  object = data_bien_quarantine,
  file = base::file.path(
    path_private_audit,
    "trait_unit_quarantine_bien.qs"
  )
)

stopifnot(
  sum(purrr::map_int(list_chunk_rows, length)) == total_rows
)
