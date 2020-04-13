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
#' @param filter whether to filter the data default is FALSE, need to be TRUE for filter variables to work
#' @param phys_contact filter by physical or non-phyisical contact
#' @param home  filter by contacts in the home
#' @param other_house filter to contacts in other homes
#' @param work filter by contacts at work
#' @param school fitler by contacts at school
#' @param gender filter by gender
#' @param hhm_quarantine filter by contacts with household members in quarantine
#' @param hhm_isolate filter by contacts with household members in isolation
#' @param hhm_limit_work filter by contacts with household members who have been asked to  limit work
#' @param hhm_limit_school filter by contacts with household members who have have been asked to limit school
#' @param hhm_work_closed filter by contacts with household members who have had their work closed
#' @param part_quarantine filter by participants in quarantine
#' @param part_isolate filter by participants in isolation
#' @param part_limit_work filter by participants who have been asked to limit work
#' @param part_limit_school filter by participants who have been asked to limit school
#' @param part_work_closed filter by participants who have had their work closed
#' @param part_covid_test_results filter by participants with a positive covid test result
#' @param part_covid_contact filter by participants who have had contact with a someone who has had covid
#' @return Either contact matrix or contact matrix and with participants information
#'
#' @export



### Create a contact matrix with filters

## Put filter = TRUE
## Then put the value to filter by in the various filter variables

create_cm <- function(
  survey,
  countries = "United Kingdom",
  age_limits = c(0, 5, 18, 30, 40, 50, 60, 70),
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
