

### NEED DOCUMENTATION
###
#' Will add documentation
#' @export
update_cm <- function(
  mat1, mat2,
  impute_rows = 1:3, impute_cols = 1:9,
  i = 1,
  observed_rows = 4:9,
  observed_col = 1:9,
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

### Repeat these for a single matrix objects.

##################
#' Will add documentation
#' @export
scale_factor_single <- function(mat1, mat2, ... ){
  x1 <- split_cm_single(mat1, ...)
  # browser()
  x2 <- split_cm_single(mat2, ...)
  x1 <- symm_mat(x1)
  x2 <- symm_mat(x2)
  max_eigen_ratio(x1, x2)
}

#' Will add documentation / may refactor
#' @export
split_cm_single <- function(mat, i = 1, row = 4:9, col = 4:9, ...){
  mat[row,col]
}


# Take a full matrix and scale it based on ratio of mat1 and mat2
#' Will add documentation
#' @export
scale_matrix_single <- function(fullmat, mat1, mat2, i = 1, ...){
  mat1 <- mat1

  fullmat * scale_factor_single(mat1, mat2, i = i, ...)

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


## Specific for this analysis get imputed values for CoMix matrix
## Will add documentation
#' @export
impute_values <- function(i = 1, school = FALSE, ...){
  imputed_values <- scale_matrix(
    polymod_cm_home,
    comix_cm_home,
    polymod_cm_home,
    i = i,
    ...
  ) +
    scale_matrix(
      polymod_cm_work,
      comix_cm_work,
      polymod_cm_work,
      i = i,
      ...
    ) +
    scale_matrix(
      polymod_cm_other,
      comix_cm_other,
      polymod_cm_other,
      i = i,
      ...
    )

  if(school){
    ## Schools were closed so can not scale by this so just add
    imputed_values <- imputed_values +
      polymod_boot_cm_school[[i]]$matrix
  }
  return(imputed_values)

}

#' Will add documentation
#' @export
scale_list <- function(mat_list, row = 2, col = 2, scale = 0.5) {
  mat_list$matrix[row, col] <- mat_list$matrix[row, col]*scale
  mat_list
}
