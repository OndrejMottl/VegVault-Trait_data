#----------------------------------------------------------#
#
#
#                     Trait data
#
#                  Get the BIEN trait data
#
#
#                      O. Mottl
#                         2023
#
#----------------------------------------------------------#
# Download trait data using rBIEN package


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

download_de_novo <- TRUE

#----------------------------------------------------------#
# 2. Downloa data and save as individual files  -----
#----------------------------------------------------------#

# Get all the avaibale protocols

vec_traits <-
  BIEN::BIEN_trait_list()

View(vec_traits)


# Following Diaz et al 2016, select the 6 traits
sel_traits <-
  c(
    "whole plant height",
    "stem wood density",
    "leaf area",
    "leaf area per leaf dry mass",
    "leaf nitrogen content per leaf dry mass",
    "seed mass"
  )

# path to the directory
sel_path <-
  here::here(
    "Data/Input/BIEN/"
  )

# download each sampling protocol and save it as individual file
purrr::walk(
  .progress = TRUE,
  .x = sel_traits,
  .f = ~ {
    sel_trait <- .x

    message(sel_trait)

    # check if the file is present
    is_present <-
      list.files(sel_path) %>%
      purrr::map_lgl(
        .f = ~ stringr::str_detect(.x, sel_trait)
      ) %>%
      any()

    if (
      isFALSE(is_present) | isTRUE(download_de_novo)
    ) {
      # download
      data_download <-
        BIEN::BIEN_trait_trait(
          trait = sel_trait,
          all.taxonomy = FALSE,
          source.citation = TRUE
        ) %>%
        tibble::as_tibble()

      # save
      RUtilpol::save_latest_file(
        object_to_save = data_download,
        file_name = sel_trait,
        dir = sel_path,
        prefered_format = "qs",
        preset = "archive"
      )
    }
  }
)
