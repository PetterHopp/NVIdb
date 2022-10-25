library(NVIdb)
library(testthat)


test_that("read leveransereg", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Read leveransereg
  levreg <- read_leveransereg(filename = "LevReg_201801_201812.csv",
                              from_path = paste0(set_dir_NVI("LevReg"), "FormaterteData/"),
                              nrow = 100)

  expect_equal(dim(levreg)[1], 100L)
})


test_that("errors for read_leveransereg", {

  linewidth <- options("width")
  options(width = 80)

  expect_error(read_leveransereg(filename = NA, from_path = tempdir()),
               regexp = "File does not exist:")

  expect_error(read_leveransereg(filename = "filename.csv", from_path = tempdir()),
               regexp = "File does not exist:")

  options(width = unlist(linewidth))
})
