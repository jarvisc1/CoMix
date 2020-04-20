#' Create a contacts matrix using socialmixr
#'
#' Wrapper function for socialmix which helps quickly create filtered contacts matrices
#' and reshape and returns multiple objects with the prefix table
#' which relate to different questions asked in the survey
#'
#'
#' @param survey a survey object from socialmixr
#' @param export_var_names Whether to export the variable names into a seperate file
#' @param matrix_output return just the matrix or the matrix and participants default = TRUE
#' @param boots number of bootstrap sample
#' @param country limit to one or more countries; if not given, will use all countries in the survey; these can be given as country names or 2-letter (ISO Alpha-2) country codes
#' @param age_limits lower limits of the age groups over which to construct the matrix
#' @param symmetric whether to make matrix symmetric
#' @param filter list object for filtering the contacts matrix to work
#' @return Either contact matrix or contact matrix and with participants information
#'
#' @export



### Create a contact matrix with filters

## Put filter = TRUE
## Then put the value to filter by in the various filter variables

create_cm <- function(
  survey,
  countries = "United Kingdom",
  age_limits,
  symmetric = FALSE,
  filter_text = list(),
  boots = 1,
  return_matrix = TRUE,
  ...
){
  x <- socialmixr::contact_matrix(
    survey,
    countries = countries,
    age.limits = age_limits,
    symmetric = symmetric,
    filter = filter_text,
    n = boots,
    ...
  )

  if (return_matrix) {
    if(boots == 1) {
      return(x$matrix)
    } else {
      return(x$matrices)
    }
  } else {
    return(x)
  }
}
