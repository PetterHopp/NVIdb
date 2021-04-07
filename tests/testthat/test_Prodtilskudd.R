context("Prodtilskudd")
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
  expect_equal(as.vector(unique(Pkoder$Telledato)), as.integer(as.Date(c("2019-10-01", "2019-03-01"))))

})
