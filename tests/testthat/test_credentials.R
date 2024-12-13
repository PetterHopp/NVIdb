# library(NVIdb)
library(testthat)

test_that("test remove_credentials", {
  # skip if no connection to 'FAG' have been established
  # skip_if_not(isTRUE(NVIcheckmate::check_credentials("PJS")))

  keyring::key_set_with_value(service = "test", username = "bruker", password = "1234567")
  expect_true(NVIcheckmate::check_credentials("test"))

  remove_credentials("test")
  expect_false(isTRUE(NVIcheckmate::check_credentials("test")))
})


test_that("Errors or warnings for set_credentials", {
  linewidth <- options("width")
  options(width = 80)

  expect_error(set_credentials(),
               regexpr = "argument 'dbservice' is missing, with no default")

  options(width = unlist(linewidth))
})

test_that("Errors or warnings for remove_credentials", {
  linewidth <- options("width")
  options(width = 80)

  expect_error(remove_credentials(),
               regexpr = "argument 'dbservice' is missing, with no default")

  options(width = unlist(linewidth))
})
