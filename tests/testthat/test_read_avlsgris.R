# library(NVIdb)
library(testthat)
library(checkmate)

test_that("read_avlsgris", {
  # Test if access to NVI internal files
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  avlsgris <- read_avlsgris(wanted_year = 2020, wanted_month = "11")
  expect_data_frame(avlsgris, nrows = 78)

  avlsgris <- read_avlsgris(wanted_year = 2021, wanted_month = "09")
  expect_data_frame(avlsgris, nrows = 78)

  avlsgris <- read_avlsgris(wanted_date = as.Date("2024-12-01"))
  expect_data_frame(avlsgris, nrows = 61)
})


test_that("Argument testing in read_avlsgris", {
  linewidth <- options("width")
  options(width = 80)

  expect_error(read_avlsgris(wanted_date = NULL, wanted_year = 2020, wanted_month = "13"),
               regexp = "Variable 'wanted_month': Must be element of")

  options(width = unlist(linewidth))
})


test_that("Errors in read_avlsgris", {
  # Test if access to NVI internal files
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  linewidth <- options("width")
  options(width = 80)

  expect_error(read_avlsgris(wanted_date = NULL, wanted_year = 1999, wanted_month = "06"),
               regexp = "Must have at least 1 rows, but has 0 rows")

  options(width = unlist(linewidth))
})
