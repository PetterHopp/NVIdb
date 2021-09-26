library(NVIdb)
library(testthat)
context("poststed")

# Assigns temporary dir to td
td <- tempdir()

test_that("Copy poststed", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # copy_kommune_fylke
  copy_poststed(to_path = td)

  expect_true(file.exists(file.path(td, "Poststed_UTF8.csv")))

})

test_that("Correct merging of poststed og komnr", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Load translation table for poststed
  poststed <- read_poststed()

  # Make a dataframe with postnr that should be translated
  poststeder <- as.data.frame(c("0468", "1343", "4550", "7800"))
  colnames(poststeder) <- "postnr"

  # Make a dataframe with the correct result
  correct_result <- cbind(poststeder,
                          as.data.frame(c("OSLO", "EIKSMARKA", "FARSUND", "NAMSOS"), stringsAsFactors = FALSE),
                          as.data.frame(c("0301", "0219", "4206", "5007"), stringsAsFactors = FALSE))
  colnames(correct_result) <- c("postnr", "poststed", "komnr")

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_identical(add_poststed(poststeder,
                                translation_table = poststed,
                                code_column = "postnr",
                                new_column = c("poststed", "komnr"),
                                position = "last"),
                   correct_result)

})


test_that("Correct result when using overwrite and keep", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Load translation table for poststed
  poststed <- read_poststed()

  # Make a dataframe with postnr that should be translated
  poststeder <- as.data.frame(c("0468", "1343", "4550", "7800"))
  colnames(poststeder) <- "postnr"

  # Make a dataframe with the correct result
  correct_result <- cbind(poststeder,
                          as.data.frame(c("OSLO", "EIKSMARKA", "FARSUND", "NAMSOS"), stringsAsFactors = FALSE),
                          as.data.frame(c("0301", "0219", "4206", "5007"), stringsAsFactors = FALSE))
  colnames(correct_result) <- c("postnr", "poststed", "komnr")

  poststeder <- add_poststed(poststeder,
                             translation_table = poststed,
                             code_column = "postnr",
                             new_column = c("poststed", "komnr"))

  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_identical(poststeder,
                   correct_result)

  poststeder <- add_poststed(poststeder,
                             translation_table = poststed,
                             code_column = "postnr",
                             new_column = c("poststed", "komnr"),
                             position = "first",
                             overwrite = TRUE)


  expect_identical(poststeder,
                   correct_result[, c("poststed", "komnr", "postnr")])

  # Compare Add kommune, with overwrite = TRUE, keep position
  poststeder <- add_poststed(poststeder,
                             translation_table = poststed,
                             code_column = "postnr",
                             new_column = c("poststed", "komnr"),
                             position = "keep",
                             overwrite = TRUE)


  expect_identical(poststeder,
                   correct_result[, c("poststed", "komnr", "postnr")])



})

