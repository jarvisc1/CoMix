
#'  Scale by eigenvalue ratio
#'
#' Take two matrices split them if required to be of the same size and square
#' Then take the ratio of the maximum eigenvalues
#'
#' @param mat1 Matrix
#' @param mat2 Matrix
#' @param split1 logical parameter, defaulted to FALSE, for splitting the first matrix
#' @param split2 logical parameter, defaulted to FALSE, for splitting the second matrix
#' @param ... parameters to pass to the `split_cm` function
#' @export
#'
# Take two matrices and calculate the scale factor for them
## Repeats the above functions to get the ratio of the max eigen values
## Of each matrix
# scale_factor <- function(mat1, mat2, split1 = FALSE, split2 = FALSE, ... ){
#   if (split1){
#     mat1 <- split_cm(mat1, ...)
#   }
#   if (split2){
#     mat2 <- split_cm(mat2, ...)
#   }
#
#   x1 <- symm_mat(mat1)
#   x2 <- symm_mat(mat2)
#   max_eigen_ratio(x1, x2)
# }

scale_factor <- function(mat1, mat2, ... ){
  x2 <- split_cm(mat2, ...)
  x1 <- symm_mat(mat1)
  x2 <- symm_mat(x2)
  max_eigen_ratio(x1, x2)
}


#'  Scale by matrix by eigenvalue ratio
#'
#' Take two matrices split them if required to be of the same size and square
#' Then take the ratio of the maximum eigenvalues
#'
#' @param scale_matrix Full matrix which which is used for Matrix
#' @param missing_matrix Matrix with missing value that need imputing
#' @param split1 whether to split the first matrix
#' @param split2 whether to split the second matrix
#'
#' @export
#'

# Take a full matrix and scale it based on ratio of mat1 and mat2
scale_matrix <- function(fullmat, mat1, mat2, i = 1, ...){
  mat1 <- mat1[[i]]$matrix

  fullmat[[i]]$matrix * scale_factor(mat1, mat2, i = i, ...)

}

# scale_matrix <- function(scale_matrix, missing_matrix, i = 0, ...){
#
#   if(i == 0 ){
#     full_matrix <- scale_matrix
#   } else {
#     scale_matrix <- scale_matrix[[i]]$matrix
#     missing_matrix <- missing_matrix[[i]]$matrix
#     full_matrix <- scale_matrix
#   }
#
#
#   full_matrix * scale_factor(scale_matrix, missing_matrix, i = 0, ...)
#
# }



### Final function which calcualte the scaling factors for R.

scale_factor_R <- function(mat1, mat2, i = 1, ...){
  # browser()
  x1 <- update_cm(mat1, mat2, i = i, ...)
  x2 <- mat2[[i]]$matrix
  x1 <- symm_mat(x1)
  x2 <- symm_mat(x2)
  max_eigen_ratio(x1, x2)
}


### NEED DOCUMENTATION



update_cm <- function(
  mat1, mat2,
  impute_rows = 1:2, impute_cols = 1:8,
  i = 1,
  observed_rows = 2:8,
  observed_col = 1:8,
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

scale_factor_single <- function(mat1, mat2, ... ){
  x1 <- split_cm_single(mat1, ...)
  # browser()
  x2 <- split_cm_single(mat2, ...)
  x1 <- symm_mat(x1)
  x2 <- symm_mat(x2)
  max_eigen_ratio(x1, x2)
}

split_cm_single <- function(mat, i = 1, row = 3:8, col = 3:8, ...){
  mat[row,col]
}


# Take a full matrix and scale it based on ratio of mat1 and mat2
scale_matrix_single <- function(fullmat, mat1, mat2, i = 1, ...){
  mat1 <- mat1

  fullmat * scale_factor_single(mat1, mat2, i = i, ...)

}

## Specific for this analysis get imputed values for CoMix matrix
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


scale_list <- function(mat_list, row = 2, col = 2, scale = 0.5) {
  mat_list$matrix[row, col] <- mat_list$matrix[row, col]*scale
  mat_list
}
