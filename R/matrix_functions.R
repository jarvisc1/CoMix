#' Splits a matrix by rows and columns
#'
#' Take a matrix or list of matrices and split then based on the row and column values provided
#'
#' @param mat matrix
#' @param i the element in the list to use if mat is a list of matrices
#' @param row a value or range 1:3 of the rows to filter by
#' @param column a value or range 1:3 of the rows to filter by
#'
#' @export
#'


split_cm <- function(mat, i = 0, row = 3:8, col = 3:8, ...){
  if(i == 0 ){
    mat[row,col]
  } else {
    mat[[i]]$matrix[row,col]
  }
}


#' Makes a square matrix symmetric
#'
#' Take a matrix ded
#'
#' @param x matrix
#'
#' @export
#'


symm_mat <- function(x){
  if(nrow(x) != ncol(x)){
    simpleError("Matrix must be square")
  } else {
    (x + t(x))/ 2
  }
}


#'  Eigenvalue ratio
#'
#' Take the ratio of the max eigenvalues of two matrices
#'
#' @param x square matrix with the same dimentions as y
#' @param y square matrix with the same dimentions as y
#'
#' @export
#'

max_eigen_ratio <- function(x, y){
  max(eigen(x, only.values = TRUE)$values)/max(eigen(y, only.values = TRUE)$values)

}


#'  Scale by eigenvalue ratio
#'
#' Take two matrices split them if required to be of the same size and square
#' Then take the ratio of the maximum eigenvalues
#'
#' @param mat1 Matrix
#' @param mat2 Matrix
#' @param split1 whether to split the first matrix
#' @param split2 whether to split the second matrix
#'
#' @export
#'



# Take two matrices and calculate the scale factor for them
## Repeats the above functions to get the ratio of the max eigen values
## Of each matrix
scale_factor <- function(mat1, mat2, split1 = FALSE, split2 = FALSE, ... ){

  if (split1){
    x1 <- split_cm(mat1, ...)
  } else{
    x1 <- mat1
  }
  if (split2){
    x2 <- split_cm(mat1, ...)
  } else{
    x2 <- mat1
  }

  x1 <- symm_mat(x1)
  x2 <- symm_mat(x2)
  max_eigen_ratio(x1, x2)
}
