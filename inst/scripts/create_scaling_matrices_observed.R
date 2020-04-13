#
# nboots <- 1
# file_name <- "comix_cms.RData"

## We do not have data on participants below 18 so need to have lower limit
## Of 18 to do the inputed and have a symmetric matrix

comix_cm <- cm_filter(comix_survey, symmetric = FALSE, boots = nboots)
polymod_cm <- cm_filter(polymod_survey, symmetric = TRUE, boots = nboots)


## Create location specific contact matrices
# Age lims for asymmetric CoMix matrices to match the polymod matrix dimensions
age_limits <- c(0, 5, 18, 30, 40, 50, 60, 70)
comix_cm_school <- cm_filter(survey = comix_survey, age_limits = age_limits,
                             symmetric = FALSE, boots = nboots,
                             filter_text = list(cnt_school = "Yes"))
comix_cm_home <- cm_filter(survey = comix_survey, age_limits = age_limits,
                           symmetric = FALSE, boots = nboots,
                           filter_text = list(cnt_home = "Yes"))
comix_cm_work <- cm_filter(survey = comix_survey, age_limits = age_limits,
                           symmetric = FALSE, boots = nboots,
                           filter_text = list(cnt_work = "Yes"))
comix_cm_other <- cm_filter(survey = comix_survey, age_limits = age_limits,
                            symmetric = FALSE, boots = nboots,
                            filter_text = list(cnt_work = "No", cnt_school = "No",
                                               cnt_home = "No"))

## Repeat for Polymod
# Age limits for POLYMOD matrices
age_limits <- c(0, 5, 18, 30, 40, 50, 60, 70)

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

if (nboots == 1) {

}

rm(nboots)
rm(file_name)
remove(list = ls()[(grepl("comix_cm", ls()))])
remove(list = ls()[(grepl("polymod_cm", ls()))])
