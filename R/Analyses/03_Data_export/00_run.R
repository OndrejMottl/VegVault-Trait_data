#----------------------------------------------------------#
#
#
#                     Trait data
#
#               Run stage 03: data export
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Builds the data export pipeline in its own store under
#   `<data root>/Targets_data/`.


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)

flag_visualise_pipeline <- FALSE


#----------------------------------------------------------#
# 1. Run pipelines -----
#----------------------------------------------------------#

run_target_pipeline(
  script = here::here("R/Analyses/03_Data_export/pipeline.R"),
  store = resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = "data_export"
  ),
  visualise = flag_visualise_pipeline
)
