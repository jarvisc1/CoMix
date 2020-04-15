
load(file.path(matrices_path, "boots_cms.RData"))

boots <- length(comix_cm)
eigen_scale <- numeric(boots)

for (i in 1:boots){
  ## Scaling for each matrix
  eigen_scale[i] <- scale_factor_R(comix_cm, polymod_cm, i = i)

}

saveRDS(eigen_scale, file = file.path(matrices_path, "eigen_all.rds"))




remove(list = ls()[(grepl("comix_cm_", ls()))])
remove(list = ls()[(grepl("polymod_cm_", ls()))])


load(file.path(matrices_path, "boots_phys_cms.RData"))
boots <- length(comix_cm)
eigen_scale_phys <- numeric(boots)

## Scale Bootstrapped matrices

for (i in 1:boots){
  ## Scaling for each matrix
  eigen_scale_phys[i] <- scale_factor_R(comix_cm, polymod_cm, i = i)

}

saveRDS(eigen_scale_phys,
        file = file.path(matrices_path, "eigen_physical.rds"))

remove(list = ls()[(grepl("comix_cm_", ls()))])
remove(list = ls()[(grepl("polymod_cm_", ls()))])



### Reduce contacts for 5-18 by 50%

## Changed to scale all by 50%
load(file.path(matrices_path, "boots_cms.RData"))
polymod_cm <- lapply(polymod_cm, scale_list)
polymod_cm_home <- lapply(polymod_cm_home, scale_list)
polymod_cm_work <- lapply(polymod_cm_work, scale_list)
polymod_cm_other <- lapply(polymod_cm_other, scale_list)


boots <- length(comix_cm)
eigen_scale_scaled <- numeric(boots)

for (i in 1:boots){
  ## Scaling for each matrix
  eigen_scale_scaled[i] <- scale_factor_R(comix_cm, polymod_cm, i = i)

}

saveRDS(eigen_scale_scaled,
        file = file.path(matrices_path, "eigen_all_polymod_scaled.rds"))

remove(list = ls()[(grepl("comix_cm", ls()))])
remove(list = ls()[(grepl("polymod_cm", ls()))])



## Repeat for physical
load(file.path(matrices_path, "boots_phys_cms.RData"))

polymod_cm <- lapply(polymod_cm, scale_list)
polymod_cm_home <- lapply(polymod_cm_home, scale_list)
polymod_cm_work <- lapply(polymod_cm_work, scale_list)
polymod_cm_other <- lapply(polymod_cm_other, scale_list)

boots <- length(comix_cm)
eigen_scale_phys_scaled <- numeric(boots)

## Scale Bootstrapped matrices

for (i in 1:boots){
  ## Scaling for each matrix
  eigen_scale_phys_scaled[i] <- scale_factor_R(comix_cm, polymod_cm, i = i)
}

saveRDS(eigen_scale_phys_scaled,
        file = file.path(matrices_path, "eigen_physical_polymod_scaled.rds"))

