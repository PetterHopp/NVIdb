library(NVIdb)
library(testthat)
library(checkmate)

test_that("Read varekode", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # read file
  varekoder <- read_varekode(filename = "varekoder.csv",
                             data_source = "formatted",
                             year = c(2020:2021))

  expect_subset(x = c("leveranseaar", "varekode", "vare", "dyreslag", "varetype", "vareart", "dyrekategori", "varekategorikode"),
                choices = colnames(varekoder))

  expect_equal(unique(varekoder$leveranseaar),
               c(2020, 2021))

  varekoder <- read_varekode(filename = "varekoder.csv",
                             data_source = "formatted",
                             year = c("2020", "2021"))

  expect_equal(unique(varekoder$leveranseaar),
               c(2020, 2021))

  varekoder <- read_varekode(year = 2020, data_source = "raw")

  expect_subset(x = c("varekode", "vare", "dyreslag"),
                choices = colnames(varekoder))

  expect_subset(x = c("STORFE", "GRIS", "HEST", "VILT", "ULL", "H\u00D8NS"),
                choices = varekoder$dyreslag)

  # Test of source file in UTF-8
  varekoder <- read_varekode(year = 2019, data_source = "raw")

  expect_subset(x = c("STORFE", "GRIS", "HEST", "VILT", "ULL", "H\u00D8NS"),
                choices = varekoder$dyreslag)


})

test_that("errors for read_varekode", {

  linewidth <- options("width")
  options(width = 80)

  expect_error(read_varekode(filename = NULL,
                             from_path = set_dir_NVI("LevReg"),
                             data_source = "formatted"),
               regexp = "Variable 'filename': Must be of type 'character', not 'NULL'",
               fixed = TRUE)

  expect_error(read_varekode(filename = "filename.csv",
                             from_path = set_dir_NVI("LevReg"),
                             data_source = "formatted"),
               regexp = "File\n * does not exist:",
               fixed = TRUE)

  expect_error(read_varekode(filename = "varekoder.csv",
                             from_path = set_dir_NVI("LevReg"),
                             data_source = "prepared",
                             year = "2020"),
               regexp = "Variable 'data_source': Must be a subset of {'formatted','raw'}",
               fixed = TRUE)

  expect_error(read_varekode(filename = "varekoder.csv",
                             from_path = set_dir_NVI("LevReg"),
                             data_source = "formatted",
                             year = 1990),
               regexp = "Element 1 is not >= 1995",
               fixed = TRUE)

  expect_error(read_varekode(filename = "filename.csv",
                             from_path = set_dir_NVI("LevReg"),
                             data_source = "formatted",
                             year = "first"),
               regexp = "Must be element of set {'last'}",
               fixed = TRUE)

  options(width = unlist(linewidth))
})
