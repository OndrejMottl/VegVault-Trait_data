#----------------------------------------------------------#
#
#
#                     Trait data
#
#             Run stage 02: data processing
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Builds the data processing pipeline in its own store under
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
  script = here::here("R/Analyses/02_Data_processing/pipeline.R"),
  store = resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = "data_processing"
  ),
  visualise = flag_visualise_pipeline
)
