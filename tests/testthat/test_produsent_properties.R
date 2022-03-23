library(NVIdb)
library(testthat)

# Assigns temporary dir to td
td <- tempdir()

if (!dir.exists(file.path (td, "test"))) {
  dir.create(file.path (td, "test")) 
} 
filenames <- "Poststed_UTF8.csv"
if (file.exists(file.path (td, "test", filenames[1]))) {
  file.remove(file.path (td, "test", filenames[1])) 
} 

test_that("Copy poststed", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))
  
  # copy file
  copy_poststed(to_path = td)
  expect_true(file.exists(file.path(td, filenames[1])))
  
  copy_poststed(from_path = td, to_path =  file.path (td, "test"))
  expect_true(file.exists(file.path(td, "test", filenames[1])))

})

# From add_produsent_help
#CURRENT PRODNR8
# Reading from standard directory at NVI's network
prodnr_2_gjeldende_prodnr <- read_prodnr_2_current_prodnr()

# Copy standard file from standard location to the subdirectory Data below the working directory
copy_prodnr_2_current_prodnr(to_path = "./Data/")

# Reading from the subdirectory Data below the working directory
prodnr_2_gjeldende_prodnr <- read_prodnr_2_current_prodnr(from_path = "./Data/")

prodnr8 <- c("09140087", "14260856", "17020818", "50060129")
olddata <- as.data.frame(prodnr8)

# Add new column with current prodnr8
newdata <- add_produsent(olddata,
                         translation_table = prodnr_2_gjeldende_prodnr,
                         code_column = "prodnr8",
                         new_column = "gjeldende_prodnr8",
                         position = "left")

# COORDINATES
# Reading from standard directory at NVI's network
prodnr_2_koordinater <- read_prodnr_2_coordinates()

newdata <- add_produsent(newdata,
                         translation_table = prodnr_2_koordinater,
                         code_column = "prodnr8",
                         new_column = c("longitude" = "geo_eu89_o", "latutude" = "geo_eu89_n"))
# End of From add_produsent_help
                         


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

test_that("errors for add_poststed", {
  
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))
  
  linewidth <- options("width")
  options(width = 80)
  
  # Load translation table for poststed
  poststed <- read_poststed()
  
  expect_error(add_poststed(data = "no_data", translation_table = "poststed") ,
               regexp = "Variable 'data': Must be of type 'data.frame', not 'character'.")
  
  
  expect_error(add_poststed(data = "no_data", translation_table = "poststed", position = "before") ,
               regexp = "Variable 'position': Must be element")
  
  expect_error(add_poststed(data = "no_data", translation_table = "poststed", overwrite = 1) ,
               regexp = "Variable 'overwrite': Must be of type 'logical', not 'double'.")
  
  options(width = unlist(linewidth))
})

test_that("errors for copy_poststed", {
  
  linewidth <- options("width")
  options(width = 80)
  
  # # skip if no connection to 'FAG' have been established
  # skip_if_not(dir.exists(set_dir_NVI("FAG")))
  # 
  # # Load translation table for poststed
  # poststed <- read_poststed()
  # 
  expect_error(copy_poststed(filename = NULL, from_path = tempdir(), to_path = "./") ,
               regexp = "(filename): Must be of type 'character', *\n * not 'NULL'",
               fixed = TRUE)
  
  expect_error(copy_poststed(filename = "filename.csv", from_path = tempdir(), to_path = "./") ,
               regexp = "File does not exist:")
  
  expect_error(copy_poststed(filename = "filename.csv", from_path = tempdir(), to_path = "filepath_does_not_exist") ,
               regexp = "Directory 'filepath_does_not_exist' does not\n * exists.",
               fixed = TRUE)
  
  options(width = unlist(linewidth))
})

test_that("errors for read_poststed", {
  
  linewidth <- options("width")
  options(width = 80)
  
  expect_error(read_poststed(filename = NULL, from_path = tempdir()) ,
               regexp = "(filename): Must be of type 'character', *\n * not 'NULL'",
               fixed = TRUE)
  
  expect_error(read_poststed(filename = "filename.csv", from_path = tempdir()) ,
               regexp = "File does not exist:")
  
  options(width = unlist(linewidth))
})
