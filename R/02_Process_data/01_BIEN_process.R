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

data_bien <-
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
    trait_value = as.numeric(trait_value)
  ) %>%
  dplyr::group_by(
    trait_name
  ) %>%
  tidyr::nest(
    species_data = c(
      id,
      scrubbed_species_binomial,
      trait_value,
      unit,
      longitude,
      latitude,
      method,
      url_source,
      source_citation,
      project_pi,
      access,
    )
  ) %>%
  dplyr::ungroup()

# data check -----

data_bien_unnest <-
  data_bien %>%
  tidyr::unnest(species_data)

dplyr::glimpse(data_bien_unnest)

data_bien_unnest %>%
  dplyr::mutate(
    dplyr::across(
      tidyselect::where(is.character),
      as.factor
    )
  ) %>%
  summary()

# check the percentage of NAs in each column
(data_bien_unnest %>%
  is.na() %>%
  colSums() / nrow(data_bien_unnest)) * 100


#----------------------------------------------------------#
# 4. Save  -----
#----------------------------------------------------------#

total_rows <- nrow(data_bien_unnest)
n_chunks <- 4
chunk_size <- total_rows / n_chunks

c(
  0:(n_chunks - 1)
) %>%
  rlang::set_names() %>%
  purrr::map(
    .f = ~ as.integer((.x * chunk_size + 1):((.x + 1) * chunk_size))
  ) %>%
  purrr::iwalk(
    .f = ~ data_bien_unnest %>%
      dplyr::slice(.x) %>%
      RUtilpol::save_latest_file(
        object_to_save = .,
        file_name = paste0(
          "data_traits_bien_",
          .y
        ),
        dir = here::here(
          "Outputs/Data/"
        ),
        prefered_format = "qs",
        preset = "archive"
      )
  )
