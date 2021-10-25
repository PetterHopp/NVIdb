library(NVIdb)
library(testthat)
context("assert_arguments")


test_that("assert_add_function by add_poststed", {
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
               regexp = "Variable 'unname\\(code_column)': Must be a subset of set " )

  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = c("Postnr"="postnr"),
                            new_column = "poststed", position = "last", overwrite = FALSE),
               regexp = "Variable 'names\\(code_column)': Must be a subset of set \\{postnr}" )
  
  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = c("postnr"="postnr"),
                            new_column = c("poststed" = "Poststed"), position = "left", overwrite = FALSE),
               regexp = "new_column must be column name\\(s) in the translation table, but 'Poststed' are" )
  
  poststeder <- add_poststed(data = poststeder, translation_table = poststed, code_column = c("postnr"="postnr"),
                             new_column = c("poststed" = "poststed"), position = "right", overwrite = FALSE)
  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = c("postnr"="postnr"),
                            new_column = c("poststed" = "poststed"), position = "left", overwrite = FALSE),
               regexp = "Variable 'names\\(new_column)': Must be disjunct from \\(poststed). The column name\\(s): 'poststed'" )
  
  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = c("postnr"="postnr"),
                            new_column = c("postnr" = "poststed"), position = "left", overwrite = FALSE),
               regexp = "You cannot give any of the new column\\(s) the same" )
  
  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = "postnr",
                            new_column = "poststed", position = "over", overwrite = TRUE),
               regexp = "Variable 'position': Must be element of set \\{'first','left','right','last','keep'}, but is 'over'" )
})


