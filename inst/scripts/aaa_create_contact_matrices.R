## Setup contact matrix data
TEST = TRUE

library(socialmixr)
library(data.table)
library(ggplot2)
library(CoMix)
here::here()

source(here::here("R/create_cm.R"))
source(here::here("R/matrix_functions.R"))
source(here::here("R/imputation_functions.R"))


## Requires participants and contacts data
part <- readRDS(here::here("inst", "data", "clean_participants.rds"))
table(part$date, useNA = "always")
table(part$panel, useNA = "always")
table(part$wave)
contacts <- readRDS(here::here("inst", "data", "clean_contacts.rds"))
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
  part_id,
  part_gender,
  part_age,
  date,
  country,
  part_isolate,
  part_quarantine,
  part_limit_work,
  part_limit_school,
  part_work_closed,
  part_covid_test_result,
  part_covid_contact
)
]


comix_survey <- socialmixr::survey(part_m, contacts_m)
polymod_survey <- polymod
comix_cm <- create_cm(comix_survey)
polymod_cm <- create_cm(polymod_survey)


## Save the survey objects
saveRDS(comix_survey, file = here::here("inst", "data", "contact_matrices",
                                        "comix_survey.rds"))
saveRDS(comix_cm, file = here::here("inst", "data", "contact_matrices",
                                    "comix_contact_matrix.rds"))

# source(here::here("R/matrix_functions.R"))


## STEP 1:
# Create matrices for each survey and contact location
source(here::here("inst/scripts/create_scaling_matrices.R"))

## STEP 2:
# Create bootstrapped contat matrices for each survey and contact location
source(here::here("inst/scripts/create_scaling_matrices.R"))

## STEP 2:
# Create bootstrapped pysical contact matrices for each survey and contact location
source(here::here("inst/scripts/create_scaling_matrices_phys.R"))


# source(here::here('inst/scripts/create_survey_contact_matrices.R'))
# saveRDS(comix_cm_phys, file = "data/contact_matrices/comix_cm_phys.rds")
# saveRDS(polymod_cm_phys, file = "data/contact_matrices/polymod_cm_phys.rds")
