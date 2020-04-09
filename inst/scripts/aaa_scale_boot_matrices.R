source(here::here("R/create_cm.R"))
source(here::here("R/matrix_functions.R"))
source(here::here("R/imputation_functions.R"))


load("inst/data/contact_matrices/boots_cms.RData")

boots <- length(comix_cm)
# boots <- 10
eigen_scale <- numeric(boots)

for (i in 1:boots){
  ## Scaling for each matrix
  eigen_scale[i] <- scale_factor_R(comix_cm, polymod_cm, i = i)

}

saveRDS(eigen_scale, file = here::here("inst/data/contact_matrices/eigen_all.rds"))




remove(list = ls()[(grepl("_cm_", ls()))])

load(here::here("inst/data/contact_matrices/boots_phys_cms.RData"))
boots <- length(comix_cm)
eigen_scale_phys <- numeric(boots)

## Scale Bootstrapped matrices

for (i in 1:boots){
  ## Scaling for each matrix
  eigen_scale_phys[i] <- scale_factor_R(comix_cm, polymod_cm, i = i)

}

saveRDS(eigen_scale_phys,
        file = here::here("inst/data/contact_matrices/eigen_physical.rds"))


