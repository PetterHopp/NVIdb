library(NVIdb)
library(testthat)
context("PJS build_sql_select_year")

test_that("build_sql_select_code", {

  expect_equivalent(build_sql_select_year(year = 2020, varname = "aar"),
                    "aar = 2020")

  expect_equivalent(build_sql_select_year(year = c(2018, 2020), varname = "aar"),
                    "aar >= 2018 AND aar <= 2020")

  expect_equivalent(build_sql_select_year(year = c(2018:2020), varname = "sak.aar"),
                    "sak.aar >= 2018 AND sak.aar <= 2020")

  this_year <- format(Sys.Date(), "%Y")

  expect_equivalent(build_sql_select_year(year = c(2018:this_year), varname = "aar"),
                    "aar >= 2018")
})


test_that("errors for build_sql_select_year", {

expect_error(build_sql_select_year(year = NA, varname = "aar", db = "PJS"),
  regexp = "Contains missing values")

expect_error(build_sql_select_year(year = "", varname = "aar", db = "PJS"),
  regexp = "Variable 'year': Must be of type 'integerish', not 'character'")

expect_error(build_sql_select_year(year = "2020", varname = "aar", db = "PJS"),
  regexp = "Variable 'year': Must be of type 'integerish', not 'character'")

expect_error(build_sql_select_year(year = 2020, varname = NA, db = "PJS"),
             regexp = "Contains missing values")

expect_error(build_sql_select_year(year = 2020, varname = "", db = "PJS"),
             regexp = "All elements must have at least 1 characters")

expect_error(build_sql_select_year(year = 2020, varname = c("aar", "sak.aar"), db = "PJS"),
             regexp = "Must have length 1, but has length")

expect_error(build_sql_select_year(year = 2020, varname = "aar", db = "EOS"),
             regexp = "Must be element of set \\{'PJS'\\}, but is")

})
