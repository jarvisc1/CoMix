
create_scaling_matrices <- function(comix_survey, polymod_survey, nboots,
                                    matrices_path, file_name,
                                    age_limits = c(0, 5, 18, 30, 40, 50, 60, 70),
                                    age_limits_sym = c(18, 30, 40, 50, 60, 70)) {

  ## We do not have data on participants below 18 so need to have lower limit
  ## Of 18 to do the inputed and have a symmetric matrix
  comix_cm <- cm_filter(comix_survey, symmetric = FALSE, boots = nboots)
  polymod_cm <- cm_filter(polymod_survey, symmetric = TRUE, boots = nboots)


  ## Create location specific contact matrices
  ## Use age_limits_sym for COMIX matrices to match POLYMOD dimensions
  comix_cm_school <- cm_filter(survey = comix_survey, age_limits = age_limits_sym,
                             symmetric = TRUE, boots = nboots ,
                             filter_text = list(cnt_school = "Yes"))

  comix_cm_home <- cm_filter(survey = comix_survey, age_limits = age_limits_sym,
                             symmetric = TRUE, boots = nboots,
                             filter_text = list(cnt_home = "Yes"))

  comix_cm_work <- cm_filter(survey = comix_survey, age_limits = age_limits_sym,
                             symmetric = TRUE, boots = nboots,
                             filter_text = list(cnt_work = "Yes"))

  comix_cm_other <- cm_filter(survey = comix_survey, age_limits = age_limits_sym,
                              symmetric = TRUE, boots = nboots,
                              filter_text = list(cnt_work = "No", cnt_school = "No",
                                                 cnt_home = "No"))

  ## Repeat for Polymod
  ## Use age_limits for POLYMOD matrices
  polymod_cm_school <- cm_filter(survey = polymod_survey, age_limits = age_limits,
                                 symmetric = TRUE, boots = nboots,
                                 filter_text = list(cnt_school = 1))
  polymod_cm_home <- cm_filter(survey = polymod_survey, age_limits = age_limits,
                                 symmetric = TRUE, boots = nboots,
                                 filter_text = list(cnt_home = 1))
  polymod_cm_work <- cm_filter(survey = polymod_survey, age_limits = age_limits,
                               symmetric = TRUE, boots = nboots,
                               filter_text = list(cnt_work = 1))
  polymod_cm_other <- cm_filter(survey = polymod_survey, age_limits = age_limits,
                                symmetric = TRUE, boots = nboots,
                                filter_text = list(cnt_work = 0, cnt_school = 0,
                                                   cnt_home = 0))

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
}
