
context("create_cm() tests")


part_dt <- readRDS(here::here("tests", "data", "participants_base.rds"))
cont_dt <- readRDS(here::here("tests", "data", "contacts_base.rds"))



survey <- socialmixr::survey(participants = part_dt, contacts = cont_dt)
age_limits <- c(0, 5, 13, 18, 30, 40, 50, 60, 70)


# cm <- CoMix::create_cm(
#   survey,
#   countries = "United Kingdom",
#   age_limits = age_limits,
#   symmetric = FALSE,
#   # filter_text = list(),
#   boots = 1,
#   return_matrix = TRUE
# )

test_that("calling create_cm with (...) args are passed through to socialmixr::contact_matrix", {

  # Check that ... parameters are passed through
  expect_message(CoMix::create_cm(
    survey,
    countries = "United Kingdom",
    age_limits = age_limits,
    symmetric = FALSE,
    # filter_text = list(),
    boots = 1,
    return_matrix = TRUE
  ),
  "Removing participants that have contacts without age information. To change this behaviour, set the 'missing.contact.age' option")

  m <- capture_messages(CoMix::create_cm(
    survey,
    countries = "United Kingdom",
    age_limits = age_limits,
    symmetric = FALSE,
    # filter_text = list(),
    boots = 1,
    return_matrix = TRUE,
    missing.contact.age = "sample"
  ))
  testthat::equals(m, character())
})
