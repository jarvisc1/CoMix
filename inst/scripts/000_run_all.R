library(CoMix)
library(here)
here::here()
set.seed(14042020)

## SET PANEL AND WAVE PATH for wave specific analysis - the following data
## should be stored in and rds file in the corresponding data folder:
##  - participants data.table
##  - contacts data.table
##  - participants and contacts combined data.table

panel <- "panel_a"
wave <- "wave_1"
survey_sub_path <- file.path(panel, wave)
age_limits <- c(0, 5, 18, 30, 40, 50, 60, 70)
age_limits_sym <- c(18, 30, 40, 50, 60, 70)

scripts_path <- here("inst", "scripts")
base_data_path <- here("inst", "data")
base_outputs_path <- here("inst", "outputs")

data_path <- file.path(base_data_path, survey_sub_path)

outputs_path <- file.path(base_outputs_path, survey_sub_path)
matrices_path <- file.path(outputs_path, "contact_matrices")
online_matrices_path <- file.path(outputs_path, "contact_matrices_online")

# Will throw an error if the data path does not include necessary data
source(file.path(scripts_path, "add_analysis_directories.R"))


# Set TEST to TRUE to run 200 bootstrap samples, FALSE to run 5000
TEST = FALSE

# Create all contact matrices
source(file.path(scripts_path, "aaa_create_contact_matrices.R"))
source(file.path(scripts_path, "aaa_scale_contact_matrices.R"))
source(file.path(scripts_path, "aaa_scale_boot_matrices.R"))

# Generate eigen values and R estimates
source(file.path(scripts_path, "001_create_eigen_values.R"))

# Plot results
source(file.path(scripts_path, "002_analysis_figures.R"))
source(file.path(scripts_path, "003_analysis_figures.R"))
source(file.path(scripts_path, "004_analysis_figures.R"))
source(file.path(scripts_path, "005_analysis_figures.R"))





## Remove variables to prevent carry-over errors
rm(scripts_path)
rm(base_data_path)
rm(base_outputs_path)
rm(data_path)
rm(matrices_path)
rm(outputs_path)
rm(online_matrices_path)
rm(age_limits)
rm(age_limits_sym)

