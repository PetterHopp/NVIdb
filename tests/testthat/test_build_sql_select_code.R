library(NVIdb)
library(testthat)
context("PJS build_sql_select_code")

test_that("build_sql_select_code", {
  
  query <- build_sql_select_code(values = NA, varname = "hensiktkode", db = "PJS") 
  expect_equivalent(query, "")
  
  query <- build_sql_select_code(values = NULL, varname = "hensiktkode", db = "PJS") 
  expect_equivalent(query, "")
  
  query <- build_sql_select_code(values = "", varname = "hensiktkode", db = "PJS") 
  expect_equivalent(query, "")
  
  query <- build_sql_select_code(values = " ", varname = "hensiktkode", db = "PJS") 
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
  
  
})

