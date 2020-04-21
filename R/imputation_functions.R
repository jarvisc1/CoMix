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
#'
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



#' Impute Comix matrix based on Polymod
#'
#' @param comix
#' @param polymod
#' @param i
#' @param impute_rows
#' @param impute_cols
#' @param observed_rows
#' @param observed_rows
#' @param observed_cols
#' @param reciprocol
#' @export
#'
#'
#'
impute_cm <- function(
  comix, polymod,
  i = 0,
  impute_rows = 1:3,
  impute_cols = 1:3,
  observed_rows = 4:9,
  observed_cols = 1:9,
  reciprocol = TRUE,
  ...
){
  if(i > 0 ){
    comix <- comix[[i]]$matrix
    polymod <- polymod[[i]]$matrix
  }

  update_mat <- matrix(0, nrow = nrow(comix), ncol = ncol(comix))
  row.names(update_mat) <- colnames(comix)
  colnames(update_mat)  <- colnames(comix)

  if(reciprocol){
    update_mat[impute_cols, observed_rows] <-  t(comix)[impute_cols, observed_rows]
  }

  non_imputed_values <- comix[observed_rows, observed_cols]
  update_mat[observed_rows, observed_cols] <- non_imputed_values

  imputed_values <- impute_values(i = i, ... )[impute_rows, impute_cols]
  update_mat[impute_rows, impute_cols] <- imputed_values

  update_mat <- symm_mat(update_mat)
  update_mat
}




