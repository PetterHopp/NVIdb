context("set_dir_NVI")
library(NVIdb)
library(testthat)


test_that("Preset paths exists", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  for (i in names(NVIconfig::path_NVI)) {
    expect_true(dir.exists(set_dir_NVI(i)))
  }

  # Testing if some upper case letters changes the result
  expect_true(dir.exists(set_dir_NVI("LevReG")))
  expect_true(dir.exists(set_dir_NVI("PRodtilskudd")))
  expect_true(dir.exists(set_dir_NVI("prodregister")))
  expect_true(dir.exists(set_dir_NVI("GrunnDataLand")))
  expect_true(dir.exists(set_dir_NVI("Provedata_rapportering")))
  expect_true(dir.exists(set_dir_NVI("OkProgrammer")))

})

