
library(here)
here::here()

set.seed(14042020)
# Set up paths for panel and wave specific analysis
scripts_path <- here("inst", "scripts")

base_data_path <- here("inst", "data")
base_outputs_path <- here("inst", "outputs")
survey_sub_path <- file.path("panel_b", "wave_1")


data_path <- file.path(base_data_path, survey_sub_path)
matrices_path <- file.path(data_path, "contact_matrices")
online_matrices_path <- file.path(outputs_path, "contact_matrices_online")
outputs_path <- file.path(base_outputs_path, survey_sub_path)

# Set TEST to TRUE to run 200 bootstrap samples, FALSE to run 5000
TEST = FALSE

# Create all contact matrices
source(here::here("inst", "scripts", "aaa_create_contact_matrices.R"))
source(here::here("inst", "scripts", "aaa_scale_contact_matrices.R"))
source(here::here("inst", "scripts", "aaa_scale_boot_matrices.R"))

# Generate eigen values and R estimates
source(here::here("inst", "scripts", "001_create_eigen_values.R"))

# Plot results
source(here::here("inst", "scripts", "002_analysis_figures.R"))
