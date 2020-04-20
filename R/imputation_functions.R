#' Impute values of comix based on POLYMOD
#'
#' Tale comix contacts matrix and impute the values based on the scale of the
#' dominant eigenvalues between comix and polymod
#'
#' @param i number of bootstraps, if zero then represent a single matrix
#' @param home logical default TRUE to include home for imputation
#' @param work logical default TRUE to include work for imputation
#' @param other logical default TRUE to include other for imputation
#' @param school ogical default FALSE to include school for imputation
#' @param home logical default TRUE to include home for imputation
#' @export
#'
#'
#'

impute_values <- function(i = 0,
                          home = TRUE,
                          work = TRUE,
                          other = TRUE,
                          school = FALSE,
                          ...){
  imputed_values <- 0

  if(home){
    home_impute <- scale_matrix(
      polymod_cm_home,
      comix_cm_home,
      i = i,
      ...
    )
    imputed_values <- home_impute
  }

  if(work){
    work_impute <-  scale_matrix(
      polymod_cm_work,
      comix_cm_work,
      i = i,
      ...
    )
    imputed_values <- imputed_values + work_impute

  }

  if(other){
    other_impute <- scale_matrix(
      polymod_cm_other,
      comix_cm_other,
      i = i,
      ...
    )
    imputed_values <- imputed_values + other_impute
  }

  if(school){
    school_impute <- scale_matrix(
      polymod_cm_school,
      comix_cm_school,
      i = i,
      ...
    )
    imputed_values <- imputed_values + school_impute
  }
  return(imputed_values)
}



#' Impute only the required values of the contact matrix
#'
#' Only impute the required rows.
#' Will add documentation
#' @export
#'
#'
#'
update_cm <- function(
  mat1, mat2,
  impute_rows = 1:3, impute_cols = 1:9,
  i = 1,
  observed_rows = 4:9,
  observed_col = 1:9,
  reciprocol = TRUE,
  school = FALSE,
  ...
){
  mat1 <- mat1[[i]]$matrix
  mat2 <- mat2[[i]]$matrix
  update_mat <- matrix(0, nrow = nrow(mat1), ncol = ncol(mat1))
  row.names(update_mat) <- colnames(mat1)
  colnames(update_mat)  <- colnames(mat1)

  #non_imputed_values <- mat1[row, col]
  update_mat <- mat1

  imputed_values <- impute_values(i = i, school = school, ... )[impute_rows, impute_cols]
  update_mat[impute_rows, impute_cols] <- imputed_values

  symm_mat(update_mat)
}


## Specific for this analysis get imputed values for CoMix matrix
## #' Will add documentation
#' @export
impute_values_single <- function(i = 1, school = FALSE, ...){

  imputed_values <- scale_matrix_single(
    polymod_cm_home,
    comix_cm_home,
    polymod_cm_home,
    i = i,
    ...
  ) +
    scale_matrix_single(
      polymod_cm_work,
      comix_cm_work,
      polymod_cm_work,
      i = i,
      ...
    ) +
    scale_matrix_single(
      polymod_cm_other,
      comix_cm_other,
      polymod_cm_other,
      i = i,
      ...
    )

  if(school){
    ## Schools were closed so can not scale by this so just add
    imputed_values <- imputed_values +
      polymod_cm_schoolmatrix
  }
  return(imputed_values)

}




#' Will add documentation
#' @export
scale_list <- function(mat_list, row = 2, col = 2, scale = 0.5) {
  mat_list$matrix[row, col] <- mat_list$matrix[row, col]*scale
  mat_list
}
