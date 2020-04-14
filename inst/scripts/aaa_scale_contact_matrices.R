source('R/matrix_functions.R')
source('R/imputation_functions.R')

### We need to impute the missing age contacts in the H2020 data.

## This script will take a long time to run it create 10,000 bootstrapped contact
## Matrices for each location for Polymod and H2020

library(data.table)

## Set up the observed contact matrix

load(file.path(matrices_path, "comix_cms.RData"))

comix_cm_imputed <- scale_matrix_single(polymod_cm, comix_cm, polymod_cm)

image(comix_cm_imputed)

saveRDS(comix_cm_imputed,
        file = file.path(matrices_path, "comix_cm_imputed.rds"))



write.csv(comix_cm_home,
          file.path(online_matrices_path, "comix_contact_matrix_home.csv"))
write.csv(comix_cm_work,
          file.path(online_matrices_path, "CoMix_contact_matrix_work.csv"))
write.csv(comix_cm_school,
          file.path(online_matrices_path, "CoMix_contact_matrix_school.csv"))
write.csv(comix_cm_other,
          file.path(online_matrices_path, "CoMix_contact_matrix_other.csv"))

remove(list = ls()[(grepl("comix_cm_", ls()))])
remove(list = ls()[(grepl("polymod_cm_", ls()))])


load(file.path(matrices_path, "comix_phys_cms.RData"))


comix_cm_phys_imputed <- scale_matrix_single(polymod_cm, comix_cm, polymod_cm)
image(comix_cm_phys_imputed)

saveRDS(comix_cm_phys_imputed,
        file = file.path(matrices_path, "comix_cm_phys_imputed.rds"))

## Of each matrix
remove(list = ls()[(grepl("comix_cm_", ls()))])
remove(list = ls()[(grepl("polymod_cm_", ls()))])

