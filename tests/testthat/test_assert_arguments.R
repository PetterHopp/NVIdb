library(NVIdb)
library(testthat)

test_that("assert_add_function by add_poststed", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Load translation table for poststed
  poststed <- read_poststed()

  # Make a dataframe with postnr that should be translated
  poststeder <- as.data.frame(c("0468", "1343", "4550", "7800"))
  colnames(poststeder) <- "postnr"

  linewidth <- options("width")
  options(width = 80)

  expect_error(add_poststed(data = poststederX, translation_table = poststed, code_column = "postnr",
                            new_column = "poststed", position = "left", overwrite = FALSE),
               regexp = "object 'poststederX' not found")

  expect_error(add_poststed(data = poststeder, translation_table = poststedX, code_column = "postnr",
                            new_column = "poststed", position = "right", overwrite = FALSE),
               regexp = "object 'poststedX' not found")

  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = c("postnr" = "Postnr"),
                            new_column = "poststed", position = "first", overwrite = FALSE),
               regexp = "Variable 'unname\\(code_column)': Names must be a subset of")

  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = c("Postnr" = "postnr"),
                            new_column = "poststed", position = "last", overwrite = FALSE),
               regexp = "Variable 'names\\(code_column)': Names must be a subset of \\{'postnr'},")

  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = c("postnr" = "postnr"),
                            new_column = c("poststed" = "Poststed"), position = "left", overwrite = FALSE),
               regexp = "\\{'kategoritype','postnr','poststed','komnr','utgatt_dato'\\}")

  poststeder <- add_poststed(data = poststeder, translation_table = poststed, code_column = c("postnr" = "postnr"),
                             new_column = c("poststed" = "poststed"), position = "right", overwrite = FALSE)
  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = c("postnr" = "postnr"),
                            new_column = c("poststed" = "poststed"), position = "left", overwrite = FALSE),
               regexp = "Variable 'names\\(new_column)': Names must be disjunct from")

  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = c("postnr" = "postnr"),
                            new_column = c("postnr" = "poststed"), position = "left", overwrite = FALSE),
               regexp = "Variable 'names\\(new_column)': Names must be disjunct from")

  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = "postnr",
                            new_column = "poststed", position = "over", overwrite = TRUE),
               regexp = "\\{'first','left','right','last','keep'}")

  expect_error(add_poststed(data = poststeder, translation_table = poststed, code_column = "postnr",
                            new_column = "poststed", position = "l", overwrite = TRUE),
               regexp = "\\{'first','left','right','last','keep'}, but has additional elements")

  options(width = unlist(linewidth))
})


test_that("assert_read_functions", {
  linewidth <- options("width")
  options(width = 80)

  filename <- "noname.csv"
  from_path <- tempdir()
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  assert_read_functions(filename = filename, from_path = from_path, add = checks)
  # Report check-results
  expect_error(checkmate::reportAssertions(checks),
               regexp = "File does not exist")

  filename <- NA
  from_path <- tempdir()
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  assert_read_functions(filename = filename, from_path = from_path, add = checks)
  # Report check-results
  expect_error(checkmate::reportAssertions(checks),
               regexp = "Contains missing values")

  options(width = unlist(linewidth))
})
