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

#----------------------------------------------------------#
# 2. Load data  -----
#----------------------------------------------------------#

# Data request 28498
# Trait List: 3106, 4, 3108, 3110, 3112, 3114, 3116, 3117, 14, 26

library(rtry)

data_trait_raw <-
  rtry::rtry_import(
    here::here(
      "Data/Input/Raw/28498.txt"
    )
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
    3106, "Plant heigh", "Plant height vegetative	",
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

        data_trait_tibble %>%
          dplyr::filter(
            ObservationID %in% vec_sel_trait_observation_id
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

data_trait_data_id %>%
  dplyr::select(-sel_observation) %>%
  tidyr::unnest(sel_data_id_overview) %>%
  View()

vec_data_id_to_remove <-
  c(
    2221:2225, 3646, 3647, 3698, 3699, 3727, 3728, 3730, 3731, 3849, 3850,
    4029, 4030
  )

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
        dplyr::select(Dataset, AccSpeciesName, ObservationID, OrigValueStr) %>%
        dplyr::distinct(
          Dataset, AccSpeciesName, ObservationID,
          .keep_all = TRUE
        ) %>%
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

data_trait_merged <-
  dplyr::full_join(
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
    by = c("name", "Dataset", "AccSpeciesName", "ObservationID")
  )

data_trait_processed <-
  data_trait_merged %>%
  dplyr::mutate(
    trait_value = as.double(OrigValueStr)
  ) %>%
  dplyr::left_join(
    trait_name_translation_table,
    by = c("name" = "trait_id_value")
  ) %>%
  janitor::clean_names() %>%
  dplyr::select(-name) %>%
  dplyr::relocate(
    trait_value, trait_domain, trait_full_name,
    .before = observation_id
  )

#----------------------------------------------------------#
# 4. Save  -----
#----------------------------------------------------------#

RUtilpol::save_latest_file(
  object_to_save = data_trait_processed,
  file_name = "data_trait_try",
  dir = here::here(
    "Outputs/Data/"
  ),
  prefered_format = "qs",
  preset = "high"
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
