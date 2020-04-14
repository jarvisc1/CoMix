## Setup contact matrix data

library(socialmixr)
library(data.table)
library(ggplot2)
library(CoMix)
here::here()

source(here::here("R/create_cm.R"))
source(here::here("R/matrix_functions.R"))
source(here::here("R/imputation_functions.R"))
source(here::here("R/create_scaling_matrices.R"))
source(here::here("R/create_scaling_matrices_phys.R"))
source(here::here("R/create_scaling_matrices_observed.R"))


## Requires participants and contacts data
part <- readRDS(file.path(data_path, "clean_participants.rds"))
table(part$date, useNA = "always")
table(part$panel, useNA = "always")
table(part$wave)
contacts <- readRDS(file.path(data_path, "clean_contacts.rds"))
table(contacts$date, useNA = "always")
table(contacts$panel, useNA = "always")
table(contacts$wave)
## Create survey object

## Reduce the number of variables going into the data
contacts_m <- contacts[, .(
  part_id,
  cont_id,
  cnt_gender,
  cnt_age_est_min,
  cnt_age_est_max,
  phys_contact,
  cnt_home,
  cnt_work,
  cnt_school,
  cnt_supermarket,
  cnt_shop,
  cnt_inside,
  cnt_outside,
  # hhm_pregnant,
  hhm_isolate,
  hhm_quarantine,
  hhm_limit_work,
  hhm_limit_school
)
]

part_m <- part[, .(
  date,
  country,
  part_id,
  part_gender,
  part_age,
  part_pregnant,
  part_isolate,
  part_quarantine,
  part_limit_work,
  part_limit_school,
  part_work_closed,
  part_covid_test_result,
  part_covid_contact
)
]


comix_survey <- survey(part_m, contacts_m)
comix_cm <- create_cm(comix_survey)

polymod_part <- readRDS(file.path(base_data_path, "polymod_participants.rds"))
polymod_contacts <- readRDS(file.path(base_data_path, "polymod_contacts.rds"))
polymod_survey <- survey(polymod_part, polymod_contacts)
polymod_cm <- create_cm(polymod_survey)


## Save the survey and contact matrix objects
saveRDS(comix_survey, file = file.path(matrices_path, "comix_survey.rds"))
saveRDS(comix_survey, file = file.path(matrices_path, "polymod_survey.rds"))
saveRDS(comix_cm, file = file.path(matrices_path, "comix_cm.rds"))
saveRDS(polymod_cm, file = file.path(matrices_path,"polymod_cm.rds"))

# source(here::here("R/matrix_functions.R"))


## STEP 1: Set up OBSERVED matrices - single
# Create contact matrices for each survey and contact location - single
nboots <- 1
file_name <- "comix_cms.RData"
create_scaling_matrices_observed(comix_survey, polymod_survey, nboots,
                        matrices_path, file_name)

## STEP 2: Set up PHYSICAL CONTACT matrices - single
# Create contact matrices for each survey and contact location
nboots <- 1
file_name <- "comix_phys_cms.RData"
create_scaling_matrices(comix_survey, polymod_survey, nboots,
                        matrices_path, file_name)


## STEP 3: Set up SYMMETRICAL OBSERVED matrices - bootstrapped
# Create contact matrices for each survey and contact location
nboots <- 5000
if(TEST) nboots <- 200
file_name <- "boots_cms.RData"
create_scaling_matrices(comix_survey, polymod_survey, nboots,
                        matrices_path, file_name)

## STEP 4: Set up PHYSICAL CONTACT matrices - bootstrapped
# Create contact matrices for each survey and contact location
nboots <- 5000
if(TEST) nboots <- 200
file_name <- "boots_phys_cms.RData"
create_scaling_matrices_phys(comix_survey, polymod_survey, nboots,
                        matrices_path, file_name)



