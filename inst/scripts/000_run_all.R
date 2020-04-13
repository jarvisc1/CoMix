# Create all contact matrices
source(here::here("inst", "scripts", "aaa_create_contact_matrices.R"))
source(here::here("inst", "scripts", "aaa_scale_contact_matrices.R"))
source(here::here("inst", "scripts", "aaa_scale_boot_matrices.R"))

# Generate eigen values and R estimates
source(here::here("inst", "scripts", "001_create_eigen_values.R"))

# Plot results
source(here::here("inst", "scripts", "002_analysis_figures.R"))
