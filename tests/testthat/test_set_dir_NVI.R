# library(NVIdb)
library(testthat)


test_that("Preset paths exists", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  for (i in names(NVIconfig:::path_NVI)) {
    expect_true(dir.exists(set_dir_NVI(i)))
  }

})

test_that("set_dir_NVI with input ignoring case", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  expect_true(dir.exists(set_dir_NVI("LevReG")))
  expect_true(dir.exists(set_dir_NVI("PRodtilskudd")))
  expect_true(dir.exists(set_dir_NVI("prodregister")))
  expect_true(dir.exists(set_dir_NVI("GrunnDataLand")))
  expect_true(dir.exists(set_dir_NVI("Provedata_rapportering")))
  expect_true(dir.exists(set_dir_NVI("OkProgrammer")))

})

test_that("set_dir_NVI with slash = FALSE", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  expect_true(dir.exists(set_dir_NVI("LevReg", slash = FALSE)))
  expect_true(dir.exists(set_dir_NVI("PRodtilskudd", slash = FALSE)))
  expect_true(dir.exists(set_dir_NVI("prodregister", slash = FALSE)))
  expect_true(dir.exists(set_dir_NVI("GrunnDataLand", slash = FALSE)))
  expect_true(dir.exists(set_dir_NVI("Provedata_rapportering", slash = FALSE)))
  expect_true(dir.exists(set_dir_NVI("OkProgrammer", slash = FALSE)))

})

test_that("set_dir_NVI using abbreviated input", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  expect_true(dir.exists(set_dir_NVI("Lev")))
  expect_true(dir.exists(set_dir_NVI("Eksterned")))
  expect_true(dir.exists(set_dir_NVI("akv")))
  expect_true(dir.exists(set_dir_NVI("GrunnData")))
  expect_true(dir.exists(set_dir_NVI("Ok")))

})

test_that("set_dir_NVI error testing", {
  linewidth <- options("width")
  options(width = 120)

  expect_error(set_dir_NVI("X"),
               regexp = "but is 'X'.$")

  expect_error(set_dir_NVI("p"),
               regexp = "but is 'p'.  Abbreviated arguments can only be matched to one single value among the possible arguments.$")

  expect_error(set_dir_NVI("FAG", slash = "FALSE"),
               regexp = "Variable 'slash': Must be of type 'logical flag'")

  options(width = unlist(linewidth))
})
