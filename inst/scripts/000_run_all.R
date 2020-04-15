
library(here)
here::here()

## SET PANEL AND WAVE PATH - the following data should be stored in and rds file
## in the corresponding data folder:
##  - participants data.table
##  - contacts data.table
##  - participants and contacts combined data.table
survey_sub_path <- file.path("panel_b", "wave_1")

set.seed(14042020)
# Set up paths for panel and wave specific analysis

scripts_path <- here("inst", "scripts")

base_data_path <- here("inst", "data")
base_outputs_path <- here("inst", "outputs")

data_path <- file.path(base_data_path, survey_sub_path)
matrices_path <- file.path(data_path, "contact_matrices")
outputs_path <- file.path(base_outputs_path, survey_sub_path)
online_matrices_path <- file.path(outputs_path, "contact_matrices_online")

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


# Summarize results
# Will break out into functions
#
outputs_files <- list.files(base_outputs_directory, recursive = TRUE)
r_summary_paths <- grep("r_summary_table.csv", outputs_files, value = TRUE)
r_summary_paths <- grep("interim", r_summary_paths,
                        value = TRUE, invert = TRUE)

r_summaries <- list()
for (s_path in r_summary_paths) {
  split_path <- stringr::str_split(s_path, "/|\\\\")[[1]]
  df <- as.data.table(read.csv(file.path(base_outputs_path, s_path),
                 stringsAsFactors = FALSE, row.names = ))
  df[ , panel := toupper(gsub("panel_", "", split_path[1]))]
  df[ , wave := gsub("wave_", "", split_path[2])]
  df[ , wave_id := paste(panel, wave)]
  df <- df[, list(panel, wave, wave_id, contact, mean, ci_low, ci_high)]
  r_summaries[[length(r_summaries) + 1]] <- df
}

r_summary <- do.call("rbind", r_summaries)
r_summary <- r_summary[contact %in% c("all scaled", "phsyical scaled")]
r_summary <- r_summary[, wave_id := as.factor(wave_id)]

write.csv(r_summary, file.path(base_outputs_path, "combined", "r_summary.csv"),
          row.names = FALSE)

r0_errrorbar <- ggplot(r_summary,
                       aes(x = factor(wave_id), y = mean, color = contact)) +
  geom_errorbar(aes(ymin = ci_low,
                    ymax = ci_high),
                width = 0.05,
                size  = 1,
                position = position_dodge(width = 0.9)) +
  geom_point(shape = 15, size  = 5, position = position_dodge(width = 0.9)) +
  xlab("Wave") +
  ylab("R0") +
  ggtitle("Mean R0 Estimates by Wave and Contact Type")

r0_errrorbar

ggsave(plot = r0_errrorbar,
       filename = file.path(base_outputs_path, "combined", "r_summary.png"))


# Remove variables to prevent carry-over errors
rm(scripts_path)
rm(base_data_path)
rm(base_outputs_path)
rm(data_path)
rm(matrices_path)
rm(outputs_path)
rm(online_matrices_path)
rm(age_limits)
rm(age_limits_sym)
