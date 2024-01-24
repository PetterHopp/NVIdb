library(NVIdb)
library(testthat)


test_that("Errors or warnings for login_by_input", {
  linewidth <- options("width")
  options(width = 80)

  expect_error(login_by_input("PHS"),
               regexpr = "Variable 'dbservice': Must be element of")

  expect_error(login_by_input(dbservice = "PJS",
                       dbdriver = 1,
                       db = 123L,
                       dbserver = 23,
                       dbport = NA,
                       dbprotocol = 1234,
                       dbinterface = "x"),
                 regexpr = "Variable 'dbdriver': Must be of type 'character'")

  options(width = unlist(linewidth))
})
