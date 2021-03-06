#'  Creates matrices for scaling CoMix by POLYMOD
#'
#' Take two matrices split them if required to be of the same size and square
#' Then take the ratio of the maximum eigenvalues
#'
#' @param comix_survey Survey object for CoMix.
#' @param polymod_survey Survey object for POLYMOD.
#' @param matrices_path File path where the matrices should be saved.
#' @param file_name Name of file for multiple matrices to be save into.
#' @param split2 Logical parameter, defaulted to FALSE, for splitting the second matrix.
#' @param observed Logical parameter, defaulted to FALSE, for saving the observed contact matrices function.
#' @param phys Logical parameter, defaulted to false, if true then only physical contacts are returned.
#' @param ... passed onto `create_cm`phys Logical parameter, defaulted to false, if true then only physical contacts are returned.
#' @export
#' @export
#'
create_scaling_matrices <- function(comix_survey, polymod_survey, nboots,
                                    matrices_path, file_name,
                                    age_limits = c(0, 5, 18, 30, 40, 50, 60, 70),
                                    age_limits_sym = c(18, 30, 40, 50, 60, 70),
                                    observed = FALSE,
                                    phys = FALSE,
                                    ...) {

  ## Create location specific contact matrices
  ## Use age_limits_sym for COMIX matrices to match POLYMOD dimensions

  if(phys){

    ## We do not have data on participants below 18 so need to have lower limit
    ## Of 18 to do the inputed and have a symmetric matrix
    comix_cm <- create_cm(comix_survey, age_limits = age_limits,
                          symmetric = FALSE, boots = nboots,
                          filter_text = list(phys_contact = 1), ...
    )
    polymod_cm <- create_cm(polymod_survey,  age_limits = age_limits,
                            symmetric = TRUE, boots = nboots,
                            filter_text = list(phys_contact = 2), ...
    )


    filter_comix <- list(list(phys_contact = 1, cnt_school = "Yes"),
                         list(phys_contact = 1, cnt_home = "Yes"),
                         list(phys_contact = 1, cnt_work = "Yes"),
                         list(phys_contact = 1, cnt_work = "No",
                              cnt_school = "No",
                              cnt_home = "No"))

    filter_polymod <- list(list(phys_contact = 2, cnt_school = 1),
                           list(phys_contact = 2, cnt_home = 1),
                           list(phys_contact = 2, cnt_work = 1),
                           list(phys_contact = 2, cnt_work = 0,
                                cnt_school = 0,
                                cnt_home = 0))
  } else{
    comix_cm <- create_cm(comix_survey, age_limits = age_limits,
                          symmetric = FALSE, boots = nboots, ...
    )
    polymod_cm <- create_cm(polymod_survey,  age_limits = age_limits,
                            symmetric = TRUE, boots = nboots, ...
    )
   filter_comix <- list(list(cnt_school = "Yes"),
                        list(cnt_home = "Yes"),
                        list(cnt_work = "Yes"),
                        list(cnt_work = "No",
                             cnt_school = "No",
                             cnt_home = "No"))
   filter_polymod <- list(list(cnt_school = 1),
                          list(cnt_home = 1),
                          list(cnt_work = 1),
                          list(cnt_work = 0,
                               cnt_school = 0,
                               cnt_home = 0))
  }


  comix_names <- list("comix_cm_school",
                      "comix_cm_home",
                      "comix_cm_work",
                      "comix_cm_other"
                      )
  polymod_names <- list("polymod_cm_school",
                       "polymod_cm_home",
                       "polymod_cm_work",
                       "polymod_cm_other"
                      )

  for (i in 1:length(filter_comix)){

    assign(comix_names[[i]], create_cm(survey = comix_survey, age_limits = age_limits_sym,
                                    symmetric = TRUE, boots = nboots ,
                                    filter_text = filter_comix[[i]], ...)
    )
    assign(polymod_names[[i]], create_cm(survey = polymod_survey, age_limits = age_limits,
                                    symmetric = TRUE, boots = nboots ,
                                    filter_text = filter_polymod[[i]], weigh.dayofweek = TRUE,
                                    ...)
    )
  }


  save(
      comix_cm,
      comix_cm_school,
      comix_cm_home,
      comix_cm_work,
      comix_cm_other,
      polymod_cm,
      polymod_cm_school,
      polymod_cm_home,
      polymod_cm_work,
      polymod_cm_other,
      file = file.path(matrices_path, file_name))

  if (observed) {
    saveRDS(comix_cm, file = file.path(matrices_path, "comix_cm.rds"))
    saveRDS(polymod_cm, file = file.path(matrices_path, "polymod_cm.rds"))
  }


}
