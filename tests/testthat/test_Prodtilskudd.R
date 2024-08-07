library(NVIdb)
library(testthat)

td <- tempdir()

test_that("Copy Prodtilskudd", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # copy_Prodtilskudd
  copy_Prodtilskudd(Pkode_year = c("2019"), Pkode_month = "both", to_path = td)

  expect_identical(list.files(td, pattern = "Pkode"),
                   c("Pkode20190301 Uttrekk per 20200624 UTF8.csv", "Pkode20191001 Uttrekk per 20200624 UTF8.csv"))

})

test_that("Read Prodtilskudd", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Reads data
  Pkoder <- read_Prodtilskudd(Pkode_year = "2019", Pkode_month = "03")

  Pkoder$Telledato <- as.Date(Pkoder$Telledato)
  # check type data.frame
  expect_identical(class(Pkoder), "data.frame")
  # check number of rows and columns
  expect_equal(dim(Pkoder), c(26936, 68))

  # check correct version
  # expect_equal(as.vector(unique(Pkoder$`Søknadsår`)), 2019)
  # expect_equal(as.vector(unique(Pkoder$Telledato)), as.integer(as.Date("2019-03-01")))
  correct_result <- as.data.frame(cbind("S\u00F8knads\u00E5r" = 2019, "Telledato" = as.Date("2019-03-01")))
  # correct_result$`Søknadsår` <- as.numeric(correct_result$`Søknadsår`)
  expect_equal(as.character(unique(Pkoder[, c("S\u00F8knads\u00E5r", "Telledato")])), as.character(correct_result))

  # Reads data
  Pkoder <- read_Prodtilskudd(Pkode_year = "2019", Pkode_month = "both", from_path = td)

  # check type data.frame
  expect_identical(class(Pkoder), "data.frame")
  # check number of rows and columns
  expect_equal(dim(Pkoder), c(55803, 89))
  # check correct version
  expect_equal(as.vector(unique(Pkoder[, "S\u00F8knads\u00E5r"])), 2019)
  expect_equal(as.vector(as.integer(as.Date(unique(Pkoder$Telledato)))), as.integer(as.Date(c("2019-10-01", "2019-03-01"))))

})

test_that("Read Prodtilskudd, extracted_date", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Reads data
  Pkoder <- read_Prodtilskudd(Pkode_year = "2019", Pkode_month = "03", extracted_date = "2020-01-13")

  Pkoder$Telledato <- as.Date(Pkoder$Telledato)
  # check type data.frame
  expect_identical(class(Pkoder), "data.frame")
  # check number of rows and columns
  expect_equal(dim(Pkoder), c(26921, 67))

  # check correct version
  # expect_equal(as.vector(unique(Pkoder$`Søknadsår`)), 2019)
  # expect_equal(as.vector(unique(Pkoder$Telledato)), as.integer(as.Date("2019-03-01")))
  correct_result <- as.data.frame(cbind("S\u00F8knads\u00E5r" = 2019, "Telledato" = as.Date("2019-03-01")))
  # correct_result$`Søknadsår` <- as.numeric(correct_result$`Søknadsår`)
  expect_equal(as.character(unique(Pkoder[, c("S\u00F8knads\u00E5r", "Telledato")])), as.character(correct_result))

})

test_that("errors for copy_Prodtilskudd", {

  linewidth <- options("width")
  options(width = 80)

  expect_error(copy_Prodtilskudd(from_path = "wrong_path", to_path = "./", Pkode_year = "last", Pkode_month = "10"),
               regexp = "Directory 'wrong_path' does not exist.",
               fixed = TRUE)

  expect_error(copy_Prodtilskudd(from_path = tempdir(), to_path = "wrong_path", Pkode_year = "last", Pkode_month = "10"),
               regexp = "Directory 'wrong_path' does not exist.")

  expect_error(copy_Prodtilskudd(from_path = tempdir(), to_path = "./", Pkode_year = "1990", Pkode_month = "10"),
               regexp = "Element 1 is not >= 1995",
               fixed = TRUE)

  expect_error(copy_Prodtilskudd(from_path = tempdir(), to_path = "./", Pkode_year = "first", Pkode_month = "10"),
               regexp = "{'last'}, but is 'first'",
               fixed = TRUE)

  expect_error(copy_Prodtilskudd(from_path = tempdir(), to_path = "./", Pkode_year = NULL, Pkode_month = "10"),
               regexp = "Must be a subset of {'last'}",
               fixed = TRUE)

  expect_error(copy_Prodtilskudd(from_path = tempdir(), to_path = "./", Pkode_year = 2020, Pkode_month = "xx"),
               regexp = "Variable 'Pkode_month': Must be a subset of",
               fixed = TRUE)

  options(width = unlist(linewidth))
})

test_that("errors for copy_Prodtilskudd with extracted_date", {

  linewidth <- options("width")
  options(width = 80)

  expect_error(copy_Prodtilskudd(from_path = tempdir(), to_path = "./", Pkode_year = "last",
                                 Pkode_month = "10", extracted_date = "2023-03-31"),
               regexp = "Contains only missing values. The input 'last' is",
               fixed = TRUE)

  expect_error(copy_Prodtilskudd(from_path = tempdir(), to_path = "./", Pkode_year = 2020,
                                 Pkode_month = "both", extracted_date = "2023-03-31"),
               regexp = "The inputs 'both' and 'last' are not accepted when",
               fixed = TRUE)

  options(width = unlist(linewidth))
})



test_that("errors for read_Prodtilskudd", {

  linewidth <- options("width")
  options(width = 80)

  expect_error(read_Prodtilskudd(from_path = file.path(tempdir(), "rubbish")),
               regexp = "rubbish' does not",
               fixed = TRUE)

  expect_error(read_Prodtilskudd(from_path = paste0(set_dir_NVI("Prodtilskudd"), "FormaterteData/"),
                                 Pkode_year = 1990,
                                 Pkode_month = "both"),
               regexp = "Element 1 is not >= 1995",
               fixed = TRUE)

  expect_error(read_Prodtilskudd(from_path = paste0(set_dir_NVI("Prodtilskudd"), "FormaterteData/"),
                                 Pkode_year = "first",
                                 Pkode_month = "both"),
               regexp = "{'last'}, but is 'first'",
               fixed = TRUE)

  expect_error(read_Prodtilskudd(from_path = paste0(set_dir_NVI("Prodtilskudd"), "FormaterteData/"),
                                 Pkode_year = NULL,
                                 Pkode_month = "both"),
               regexp = "Must be a subset of {'last'}",
               fixed = TRUE)

  expect_error(read_Prodtilskudd(from_path = paste0(set_dir_NVI("Prodtilskudd"), "FormaterteData/"),
                                 Pkode_year = 2020,
                                 Pkode_month = "xx"),
               regexp = "Variable 'Pkode_month': Must be a subset of",
               fixed = TRUE)
  
  expect_error(read_Prodtilskudd(from_path = paste0(set_dir_NVI("Prodtilskudd"), "FormaterteData/"),
                                 Pkode_year = 2023,
                                 Pkode_month = "12"),
               regexp = "No versions of Produksjonstilskudd available for",
               fixed = TRUE)
  
  options(width = unlist(linewidth))
})
