### We need to impute the missing age contacts in the H2020 data.

## This script will take a long time to run it create 10,000 bootstrapped contact
## Matrices for each location for Polymod and H2020

library(data.table)

## Set up the observed contact matrix

load('data/contact_matrices/comix_cms.RData')
source('r/functions/matrix_scaling.r')


comix_cm_imputed <- scale_matrix_single(polymod_cm, comix_cm, polymod_cm)

image(comix_cm_imputed)

saveRDS(comix_cm_imputed, file = 'data/contact_matrices/comix_cm_imputed.rds')



write.csv(comix_cm_home, "outputs/contact_matrices_online/comix_contact_matrix_home.csv")
write.csv(comix_cm_work, "outputs/contact_matrices_online/CoMix_contact_matrix_work.csv")
write.csv(comix_cm_school, "outputs/contact_matrices_online/CoMix_contact_matrix_school.csv")
write.csv(comix_cm_other, "outputs/contact_matrices_online/CoMix_contact_matrix_other.csv")


load('data/contact_matrices/comix_phys_cms.RData')


comix_cm <- comix_cm_phys
comix_cm_school <- comix_cm_school_phys
comix_cm_home <- comix_cm_home_phys
comix_cm_work <- comix_cm_work_phys
comix_cm_other <- comix_cm_other_phys
polymod_cm <- polymod_cm_phys
polymod_cm_school <- polymod_cm_school_phys
polymod_cm_home <- polymod_cm_home_phys
polymod_cm_work <- polymod_cm_work_phys
polymod_cm_other <- polymod_cm_other_phys




comix_cm_phys_imputed <- scale_matrix_single(polymod_cm_phys, comix_cm_phys, polymod_cm_phys)


saveRDS(comix_cm_phys_imputed, file = 'data/contact_matrices/comix_cm_phys_imputed.rds')

## Of each matrix

