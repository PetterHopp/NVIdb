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


test_that("add_poststed argument checking", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))
  
  # Load translation table for poststed
  poststed <- read_poststed()
  
  # Make a dataframe with postnr that should be translated
  poststeder <- as.data.frame(c("0468", "1343", "4550", "7800"))
  colnames(poststeder) <- "postnr"
  
  expect_error(add_poststed(data = poststederX, translation_table = poststed, code_column = "postnr",
                            new_column = "poststed", position = "left", overwrite = FALSE),
               regexp = "object 'poststederX' not found" )
  
  expect_error(add_poststed(data = poststeder, translation_table = poststedX, code_column = "postnr",
                            new_column = "poststed", position = "right", overwrite = FALSE),
               regexp = "object 'poststedX' not found" )
  
  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = c("postnr"="Postnr"),
                            new_column = "poststed", position = "first", overwrite = FALSE),
               regexp = "in the translation table 'poststed' but 'Postnr' is not a column name in" )

  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = c("Postnr"="postnr"),
                            new_column = "poststed", position = "last", overwrite = FALSE),
               regexp = "the data 'poststeder' but 'Postnr' is not a column in the data. You" )

  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = c("postnr"="postnr"),
                            new_column = c("poststed" = "Poststed"), position = "left", overwrite = FALSE),
               regexp = "new_column must be column names in the translation table 'poststed' but 'Poststed' are" )

  poststeder <- add_poststed(data = poststeder, translation_table = poststed, code_column = c("postnr"="postnr"),
                             new_column = c("poststed" = "poststed"), position = "right", overwrite = FALSE)
  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = c("postnr"="postnr"),
                            new_column = c("poststed" = "poststed"), position = "left", overwrite = FALSE),
               regexp = "Either give new column name\\(s) for the column\\(s) called 'poststed' or specify overwrite = TRUE" )

  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = c("postnr"="postnr"),
                            new_column = c("postnr" = "poststed"), position = "left", overwrite = FALSE),
               regexp = "You cannot give the new column the same name as the" )

  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = "postnr",
                            new_column = "poststed", position = "over", overwrite = TRUE),
               regexp = "Variable 'position': Must be element of set \\{'first','left','right','last','keep'}, but is 'over'" )
})


