library(NVIdb)
library(testthat)

# Assigns temporary dir to td
td <- tempdir()

# if (!dir.exists(file.path (td, "data"))) {
#   dir.create(file.path (td, "data"))
# }
filenames <- c("Prodnr2GjeldendeProdnr.csv", "Prodnr2Koordinater.csv")
for (i in 1:length(filenames)) {
  if (file.exists(file.path(td, filenames[i]))) {
    file.remove(file.path(td, filenames[i]))
  }
}

test_that("Copy produsent properties registers", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # copy file
  copy_prodnr_2_current_prodnr(to_path = td)
  expect_true(file.exists(file.path(td, filenames[1])))

  # copy_produsent(from_path = td, to_path =  file.path (td, "data"))
  # expect_true(file.exists(file.path(td, "data", filenames[1])))

})


test_that("Correct merging of produsent og komnr", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # From add_produsent_help
  # CURRENT PRODNR8
  # Reading from the temporary directory
  prodnr_2_gjeldende_prodnr <- read_prodnr_2_current_prodnr(from_path = td)

  prodnr8 <- c("09140087", "14260856", "17020818", "50060129")
  produsenter <- as.data.frame(prodnr8)

  # Make a dataframe with the correct result
  correct_result <- cbind(as.data.frame(c("42130063", "46440597", "50060129", "50060129"), stringsAsFactors = FALSE),
                          produsenter)
  colnames(correct_result) <- c("gjeldende_prodnr8", "prodnr8")


  # Compare Add fylke, current fylkenr and current fylke with correct result
  produsenter <- add_produsent_properties(produsenter,
                                          translation_table = prodnr_2_gjeldende_prodnr,
                                          code_column = "prodnr8",
                                          new_column = "gjeldende_prodnr8",
                                          position = "left",
                                          impute_old_when_missing = TRUE)
  expect_identical(produsenter, correct_result)


  # COORDINATES
  # Reading from standard directory at NVI's network
  prodnr_2_koordinater <- read_prodnr_2_coordinates()

  # Make a dataframe with the correct result
  correct_result <- cbind(produsenter,
                          as.data.frame(c("8.78182485813634", "7.04026092117579", "11.64981213930787", "11.64981213930787"), stringsAsFactors = FALSE),
                          as.data.frame(c("58.63237920843709", "61.47787583826138", "64.13066637524832", "64.13066637524832"), stringsAsFactors = FALSE)
  )
  colnames(correct_result) <- c("gjeldende_prodnr8", "prodnr8", "longitude", "latitude")

  produsenter <- add_produsent_properties(produsenter,
                                          translation_table = prodnr_2_koordinater,
                                          code_column = c("gjeldende_prodnr8" = "prodnr8"),
                                          new_column = c("longitude" = "geo_eu89_o", "latitude" = "geo_eu89_n"),
                                          position = "last")
  expect_identical(produsenter, correct_result)
})



test_that("errors for add_produsent", {

  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  linewidth <- options("width")
  options(width = 80)

  # Load translation table for produsent
  prodnr_2_gjeldende_prodnr <- read_prodnr_2_current_prodnr()

  expect_error(add_produsent_properties(data = "no_data",
                                        translation_table = "prodnr_2_gjeldende_prodnr",
                                        code_column = "prodnr8",
                                        new_column = "gjeldende_prodnr8"),
               regexp = "Variable 'data': Must be of type 'data.frame', not 'character'.")


  expect_error(add_produsent_properties(data = "no_data",
                                        translation_table = "prodnr_2_gjeldende_prodnr",
                                        code_column = "prodnr8",
                                        new_column = "gjeldende_prodnr8",
                                        position = "before"),
               regexp = "Variable 'position': Must be a subset of")

  expect_error(add_produsent_properties(data = "no_data",
                                        translation_table = "prodnr_2_gjeldende_prodnr",
                                        code_column = "prodnr8",
                                        new_column = "gjeldende_prodnr8",
                                        overwrite = 1),
               regexp = "Variable 'overwrite': Must be of type 'logical', not 'double'.")

  options(width = unlist(linewidth))
})

test_that("errors for copy_produsent", {

  linewidth <- options("width")
  options(width = 80)

  # # skip if no connection to 'FAG' have been established
  # skip_if_not(dir.exists(set_dir_NVI("FAG")))
  #
  # # Load translation table for produsent
  # produsent <- read_produsent()
  #
  expect_error(copy_prodnr_2_current_prodnr(filename = NULL, from_path = tempdir(), to_path = "./"),
               regexp = "(filename): Must be of type 'character', *\n * not 'NULL'",
               fixed = TRUE)

  expect_error(copy_prodnr_2_current_prodnr(filename = "filename.csv", from_path = tempdir(), to_path = "./"),
               regexp = "File does not exist:")

  expect_error(copy_prodnr_2_current_prodnr(filename = "filename.csv", from_path = tempdir(), to_path = "filepath_does_not_exist"),
               regexp = "Directory 'filepath_does_not_exist' does not\n * exist.",
               fixed = TRUE)

  options(width = unlist(linewidth))
})

test_that("errors for read_produsent", {

  linewidth <- options("width")
  options(width = 80)

  expect_error(read_prodnr_2_current_prodnr(filename = NULL, from_path = tempdir()),
               regexp = "(filename): Must be of type 'character', *\n * not 'NULL'",
               fixed = TRUE)

  expect_error(read_prodnr_2_current_prodnr(filename = "filename.csv", from_path = tempdir()),
               regexp = "File does not exist:")

  options(width = unlist(linewidth))
})
