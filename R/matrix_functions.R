#' Splits a matrix by rows and columns
#'
#' Take a matrix or list of matrices and split then based on the row and column
#' values provided
#'
#' @param mat matrix or list of matrices
#' @param i the element in the list to use if mat is a list of matrices, defaults
#' to false for a single matrix
#' @param row a value or range (example, 1:3) of the rows to include
#' @param column a value or range (example, 1:3) of the rows to include
#'
#' @export
#
split_cm <- function(mat, i = FALSE, row = 3:8, col = 3:8, ...){

    mat[[i]]$matrix[row,col]

}


#' Makes a square matrix symmetric
#'
#' Take a matrix and
#'
#' @param x a square matrix
#'
#' @export
#

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



#' #'  Scales contact matrices
#' #'
#' #' Take the ratio of the max eigenvalues of two matrices
#' #'
#' #' @param x square matrix with the same dimentions as y
#' #' @param y square matrix with the same dimentions as y
#' #'
#' #' @export
#' #'


#' Create a contact matrix with filters
#'
#' @param survey a socialmixR survey object
#' @param countries a character vector passed to socialmixR::contact_matrix
#' @param age.limit a numeric vectors used to create breaks of ages, passed to
#' socialmixR::contact_matrix
#' @param filter_text = a list of text to filter the survey, passed to
#' socialmixR::contact_matrix
#' @param boots a numerical, defaulted to 1, indicating the number of bootsraps,
#' passed to socialmixR::contact_matrix
#' #' @param return_matrix a logical, defaulted to true, to return a socialmixR
#' contact matrix object. Returns a socialmixR survey object if set to FALSE
cm_filter <- function(
  survey,
  countries = "United Kingdom",
  age_limits = c(0, 5, 18, 30, 40, 50, 60, 70),
  symmetric = TRUE,
  filter_text = list(),
  boots = 1,
  return_matrix = TRUE
) {

  x <- contact_matrix(
    survey,
    countries = countries,
    age.limits = age_limits,
    symmetric = symmetric,
    filter = filter_text,
    n = boots
  )
  if(return_matrix){
    if(boots == 1){
      return(x$matrix)
    } else {
      return(x$matrices)
    }
  } else {
    return(x)
  }
}
