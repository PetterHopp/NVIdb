library(NVIdb)
library(testthat)

test_that("build_sql_select_code", {

  query <- build_sql_select_code(values = NULL, varname = "hensiktkode", db = "PJS")
  expect_equivalent(query, "")

  query <- build_sql_select_code(values = "0100101", varname = "hensiktkode", db = "PJS")
  expect_equivalent(query, "hensiktkode = '0100101'")

  query <- build_sql_select_code(values = "0100101%", varname = "hensiktkode", db = "PJS")
  expect_equivalent(query, "hensiktkode LIKE '0100101%'")

  query <- build_sql_select_code(values = c("0100101", "0100102"), varname = "hensiktkode", db = "PJS")
  expect_equivalent(query, "hensiktkode IN ('0100101', '0100102')")

  query <- build_sql_select_code(values = c("0100101%", "0100102%"), varname = "hensiktkode", db = "PJS")
  expect_equivalent(query, "hensiktkode LIKE '0100101%' OR hensiktkode LIKE '0100102%'")

  query <- build_sql_select_code(values = c("0100101", "0100102%"), varname = "hensiktkode", db = "PJS")
  expect_equivalent(query, "hensiktkode = '0100101' OR hensiktkode LIKE '0100102%'")

  query <- build_sql_select_code(values = c("0100101", "0100101007", "0100102%", "0100202%"), varname = "hensiktkode", db = "PJS")
  expect_equivalent(query, "hensiktkode IN ('0100101', '0100101007') OR hensiktkode LIKE '0100102%' OR hensiktkode LIKE '0100202%'")

  query <- build_sql_select_code(values = c("0100101", "0100102", NA), varname = "hensiktkode", db = "PJS")
  expect_equivalent(query, "hensiktkode IS NULL OR hensiktkode IN ('0100101', '0100102')")

  query <- build_sql_select_code(values = c("0100101%", "0100102%", NA), varname = "hensiktkode", db = "PJS")
  expect_equivalent(query, "hensiktkode IS NULL OR  hensiktkode LIKE '0100101%' OR hensiktkode LIKE '0100102%'")

  query <- build_sql_select_code(values = c(NA, "0100101", "0100102%", NA), varname = "hensiktkode", db = "PJS")
  expect_equivalent(query, "hensiktkode IS NULL OR hensiktkode = '0100101' OR hensiktkode LIKE '0100102%'")

})


test_that("errors for build_sql_select_code", {

  expect_error(build_sql_select_code(values = NA, varname = "hensiktkode", db = "PJS"),
               regexp = "Contains only missing values")

  expect_error(build_sql_select_code(values = c(NA, NA), varname = "hensiktkode", db = "PJS"),
               regexp = "Contains only missing values")

  expect_error(build_sql_select_code(values = "", varname = "hensiktkode", db = "PJS"),
               regexp = "All elements must have at least 1 characters")

  expect_error(build_sql_select_code(values = " ", varname = "hensiktkode", db = "PJS"),
               regexp = "All elements must have at least 1 characters")

  expect_error(build_sql_select_code(values = c(" ", " "), varname = "hensiktkode", db = "PJS"),
               regexp = "All elements must have at least 1 characters")

})
