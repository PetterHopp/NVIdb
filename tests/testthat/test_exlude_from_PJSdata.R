library(NVIdb)
library(testthat)

test_that("Exlude records from abroad or quality assessment from PJS data", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  PJStest <- readRDS(file.path(".", "PJS_testdata.rds"))
  # PJStest <- readRDS("./tests/testthat/PJS_testdata.rds")

  # Standardisere kolonnenavn
  PJStest <- standardize_columns(data = PJStest, property = "colnames")
  n_records <- dim(PJStest)[1]
  # test
  expect_identical(dim(exclude_from_PJSdata(PJSdata = PJStest, abroad = "exclude", quality = "exclude"))[1], n_records - 2L)

  expect_identical(dim(exclude_from_PJSdata(PJSdata = PJStest, abroad = "exclude", quality = "include"))[1], n_records - 1L)

  expect_identical(dim(exclude_from_PJSdata(PJSdata = PJStest, abroad = "include", quality = "exclude"))[1], n_records - 1L)

})

# ktr <- exclude_from_PJSdata(PJSdata = PJStest, abroad = "include", quality = "exclude")
