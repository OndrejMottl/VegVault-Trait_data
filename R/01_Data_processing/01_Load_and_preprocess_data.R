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
# Trait List: 3106, 4, 3108, 3110, 3112, 3114, 3114, 3116, 3117, 14, 26

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

data_trait_tibble <-
  tibble::as_tibble(data_trait_raw)

data_trait_dummy <-
  data_trait_tibble %>%
  dplyr::distinct(TraitID) %>%
  tidyr::drop_na() %>%
  purrr::chuck("TraitID") %>%
  rlang::set_names() %>%
  tibble::enframe()

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

data_trait_values <-
  data_trait_observations %>%
  dplyr::mutate(
    sel_data_trait_values = purrr::map(
      .progress = TRUE,
      .x = sel_observation,
      .f = ~ .x %>%
        dplyr::filter(DataID %in% data_trait_sel_data_id$DataID) %>%
        dplyr::select(DatasetID, AccSpeciesID, ObservationID, OrigValueStr) %>%
        dplyr::distinct(
          DatasetID, AccSpeciesID, ObservationID,
          .keep_all = TRUE
        ) %>%
        dplyr::filter(is.na(OrigValueStr) == FALSE)
    )
  )

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
          DatasetID, AccSpeciesID, ObservationID, DataName, OrigValueStr
        ) %>%
        dplyr::distinct(
          DatasetID, AccSpeciesID, ObservationID, DataName,
          .keep_all = TRUE
        ) %>%
        dplyr::filter(is.na(OrigValueStr) == FALSE) %>%
        dplyr::group_by(DatasetID, AccSpeciesID, ObservationID) %>%
        tidyr::pivot_wider(
          names_from = DataName, values_from = OrigValueStr
        ) %>%
        dplyr::ungroup()
    )
  )

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
    by = c("name", "DatasetID", "AccSpeciesID", "ObservationID")
  )

data_specis_names <-
  data_trait_tibble %>%
  dplyr::distinct(AccSpeciesID, AccSpeciesName)

data_trait_processed <-
  dplyr::left_join(
    data_trait_merged,
    data_specis_names,
    by = "AccSpeciesID"
  ) %>%
  dplyr::mutate(
    trait_value = as.double(OrigValueStr)
  ) %>%
  janitor::clean_names()

#----------------------------------------------------------#
# 4. Save  -----
#----------------------------------------------------------#

RUtilpol::save_latest_file(
  object_to_save = data_trait_processed,
  dir = here::here(
    "Data/Processed"
  ),
  prefered_format = "qs",
  preset = "high"
)

#----------------------------------------------------------#
# 5. (Optional) Check the distribution  -----
#----------------------------------------------------------#


data_trait_processed %>%
  dplyr::select(name, trait_value) %>%
  dplyr::slice_sample(n = 10e3) %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = trait_value
    )
  ) +
  ggplot2::facet_grid(1 ~ name, scales = "free") +
  ggplot2::geom_density(
    mapping = ggplot2::aes(
      fill = name
    )
  ) +
  ggplot2::guides(
    fill = "none"
  )
