library(NVIdb)
library(testthat)
context("kommune_fylke")

# Assigns temporary dir to td
td <- tempdir()

test_that("Copy kommune og fylke", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # copy_kommune_fylke
  copy_kommune_fylke(to_path = td)

  expect_true(file.exists(file.path(td, "Kommune_UTF8.csv")))
  expect_true(file.exists(file.path(td, "komnr_2_gjeldende_komnr_UTF8.csv")))
  expect_true(file.exists(file.path(td, "Fylke_UTF8.csv")))

})

test_that("Correct merging of fylke, gjeldende_fylkenr and gjeldende_fylke by fylkenr", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Load translation table for kommune_fylke
  kommune_fylke <- read_kommune_fylke()

  # Make a dataframe with fylkesnr that should be translated
  # All existing fylkenr should be included
  fylker <- as.data.frame(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11",
                            "12", "14", "15", "16", "17", "18", "19", "20",
                            "30", "34", "38", "42", "46", "50", "54"))
  colnames(fylker) <- "fylkenr"

  # Make a dataframe with the correct result
  correct_result <- cbind(as.data.frame(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11",
                                          "12", "14", "15", "16", "17", "18", "19", "20",
                                          "30", "34", "38", "42", "46", "50", "54"), stringsAsFactors = FALSE),
                          as.data.frame(c("Østfold", "Akershus", "Oslo", "Hedmark", "Oppland", "Buskerud",
                                          "Vestfold", "Telemark", "Aust-Agder", "Vest-Agder", "Rogaland",
                                          "Hordaland", "Sogn og Fjordane", "Møre og Romsdal", "Sør-Trøndelag",
                                          "Nord-Trøndelag", "Nordland", "Troms", "Finnmark",
                                          "Viken", "Innlandet", "Vestfold og Telemark", "Agder", "Vestland", "Trøndelag", "Troms og Finnmark"), stringsAsFactors = FALSE),
                          as.data.frame(c("30", "30", "03", "34", "34", "30", "38", "38", "42", "42", "11",
                                          "46", "46", "15", "50", "50", "18", "54", "54",
                                          "30", "34", "38", "42", "46", "50", "54"), stringsAsFactors = FALSE),
                          as.data.frame(c("Viken", "Viken", "Oslo", "Innlandet", "Innlandet", "Viken",
                                          "Vestfold og Telemark", "Vestfold og Telemark", "Agder", "Agder", "Rogaland",
                                          "Vestland", "Vestland", "Møre og Romsdal", "Trøndelag",
                                          "Trøndelag", "Nordland", "Troms og Finnmark", "Troms og Finnmark",
                                          "Viken", "Innlandet", "Vestfold og Telemark", "Agder", "Vestland", "Trøndelag", "Troms og Finnmark"), stringsAsFactors = FALSE))
  colnames(correct_result) <- c("fylkenr", "fylke", "gjeldende_fylkenr", "gjeldende_fylke")
  
  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_identical(add_kommune_fylke(data = fylker,
                                     translation_table = kommune_fylke,
                                     code_column = "fylkenr",
                                     new_column = c("fylke", "gjeldende_fylkenr", "gjeldende_fylke"),
                                     position = "right"),
                   correct_result)
  
  # Make a dataframe with the correct result
  correct_result <- cbind(as.data.frame(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11",
                                          "12", "14", "15", "16", "17", "18", "19", "20",
                                          "30", "34", "38", "42", "46", "50", "54"), stringsAsFactors = FALSE),
                          as.data.frame(c("30", "30", "03", "34", "34", "30", "38", "38", "42", "42", "11",
                                          "46", "46", "15", "50", "50", "18", "54", "54",
                                          "30", "34", "38", "42", "46", "50", "54"), stringsAsFactors = FALSE),
                          as.data.frame(c("Viken", "Viken", "Oslo", "Innlandet", "Innlandet", "Viken",
                                          "Vestfold og Telemark", "Vestfold og Telemark", "Agder", "Agder", "Rogaland",
                                          "Vestland", "Vestland", "Møre og Romsdal", "Trøndelag",
                                          "Trøndelag", "Nordland", "Troms og Finnmark", "Troms og Finnmark",
                                          "Viken", "Innlandet", "Vestfold og Telemark", "Agder", "Vestland", "Trøndelag", "Troms og Finnmark"), stringsAsFactors = FALSE))
  colnames(correct_result) <- c("fylkenr", "gjeldende_fylkenr", "gjeldende_fylke")
  
  # Compare Add fylke, current fylkenr and current fylke with correct result
  expect_identical(add_kommune_fylke(fylker,
                                     translation_table = kommune_fylke,
                                     code_column = "fylkenr",
                                     new_column = c("gjeldende_fylkenr", "gjeldende_fylke"),
                                     position = "right"),
                   correct_result)
  
  # Test of adding fylke only
  # Make a dataframe with the correct result
  correct_result <- cbind(as.data.frame(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11",
                                          "12", "14", "15", "16", "17", "18", "19", "20",
                                          "30", "34", "38", "42", "46", "50", "54"), stringsAsFactors = FALSE),
                          as.data.frame(c("Østfold", "Akershus", "Oslo", "Hedmark", "Oppland", "Buskerud",
                                          "Vestfold", "Telemark", "Aust-Agder", "Vest-Agder", "Rogaland",
                                          "Hordaland", "Sogn og Fjordane", "Møre og Romsdal", "Sør-Trøndelag",
                                          "Nord-Trøndelag", "Nordland", "Troms", "Finnmark",
                                          "Viken", "Innlandet", "Vestfold og Telemark", "Agder", "Vestland", "Trøndelag", "Troms og Finnmark"), stringsAsFactors = FALSE))
  colnames(correct_result) <- c("fylkenr", "fylke")


  # Compare Add fylke with correct result
  expect_identical(add_kommune_fylke(fylker,
                                     translation_table = kommune_fylke,
                                     code_column = "fylkenr",
                                     new_column = "fylke",
                                     position = "right"),
                   correct_result)

})



test_that("Correct merging of kommune, gjeldende_komnr and gjeldende_kommune by komnr", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Load translation table for kommune_fylke
  kommune_fylke <- read_kommune_fylke()

  # Make a dataframe with komnr that should be translated
  kommuner <- as.data.frame(c("0123", "0301", "1101", "1601"))
  colnames(kommuner) <- "komnr"

  # Make a dataframe with the correct result
  correct_result <- cbind(as.data.frame(c("0123", "0301", "1101", "1601"), stringsAsFactors = FALSE),
                          as.data.frame(c("Spydeberg", "Oslo", "Eigersund", "Trondheim"), stringsAsFactors = FALSE),
                          as.data.frame(c("3014", "0301", "1101", "5001"), stringsAsFactors = FALSE),
                          as.data.frame(c("Indre Østfold", "Oslo", "Eigersund", "Trondheim"), stringsAsFactors = FALSE))
  colnames(correct_result) <- c("komnr", "kommune", "gjeldende_komnr", "gjeldende_kommune")


  # Compare Add kommune, current komnr and current kommune with correct result
  expect_identical(add_kommune_fylke(kommuner,
                                     translation_table = kommune_fylke,
                                     code_column = "komnr",
                                     new_column = c("kommune", "gjeldende_komnr", "gjeldende_kommune"),
                                     position = "right"),
                   correct_result)

})

test_that("Correct position of new variables", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Load translation table for kommune_fylke
  kommune_fylke <- read_kommune_fylke()

  # Make a dataframe with komnr that should be translated
  kommuner <- as.data.frame(c("0123", "0301", "1101", "1601"), stringsAsFactors = FALSE)
  colnames(kommuner) <- "komnr"

  # Make a dataframe with the correct result
  correct_result <- as.data.frame(c("Spydeberg", "Oslo", "Eigersund", "Trondheim"), stringsAsFactors = FALSE)
  correct_result <- cbind(correct_result,
                          correct_result,
                          kommuner,
                          correct_result,
                          correct_result)
  colnames(correct_result) <- c("kommune_first", "kommune_left", "komnr", "kommune_right", "kommune_last" )


  # Compare Add kommune, current komnr and current kommune with correct result
  kommuner <-  add_kommune_fylke(kommuner,
                                 translation_table = kommune_fylke,
                                 code_column = "komnr",
                                 new_column = c("kommune_first" = "kommune"),
                                 position = "first")
  kommuner <-  add_kommune_fylke(kommuner,
                                 translation_table = kommune_fylke,
                                 code_column = "komnr",
                                 new_column = c("kommune_last" = "kommune"),
                                 position = "last")
  kommuner <-  add_kommune_fylke(kommuner,
                                 translation_table = kommune_fylke,
                                 code_column = "komnr",
                                 new_column = c("kommune_left" = "kommune"),
                                 position = "left")
  kommuner <-  add_kommune_fylke(kommuner,
                                 translation_table = kommune_fylke,
                                 code_column = "komnr",
                                 new_column = c("kommune_right" = "kommune"),
                                 position = "right")

  expect_identical(kommuner,
                   correct_result)
})

test_that("Correct result when using overwrite and keep", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Load translation table for kommune_fylke
  kommune_fylke <- read_kommune_fylke()

  # Make a dataframe with komnr that should be translated
  kommuner <- as.data.frame(c("0123", "0301", "1101", "1601"), stringsAsFactors = FALSE)
  colnames(kommuner) <- "komnr"

  # Make a dataframe with the correct result
  correct_result <- as.data.frame(c("Spydeberg", "Oslo", "Eigersund", "Trondheim"), stringsAsFactors = FALSE)
  correct_result <- cbind(kommuner, correct_result)
  colnames(correct_result) <- c("komnr", "kommune" )


  # Compare Add kommune, current komnr and current kommune with correct result
  kommuner <-  add_kommune_fylke(kommuner,
                                 translation_table = kommune_fylke,
                                 code_column = "komnr",
                                 new_column = c("kommune" = "kommune"),
                                 position = "right")

  expect_identical(kommuner,
                   correct_result)

  # Compare Add kommune, with overwrite = TRUE, new position
  kommuner <-  add_kommune_fylke(kommuner,
                                 translation_table = kommune_fylke,
                                 code_column = "komnr",
                                 new_column = "kommune",
                                 position = "first",
                                 overwrite = TRUE)

  expect_identical(kommuner,
                   correct_result[, c("kommune", "komnr")])

  # Compare Add kommune, with overwrite = TRUE, keep position
  kommuner <-  add_kommune_fylke(kommuner,
                                 translation_table = kommune_fylke,
                                 code_column = "komnr",
                                 new_column = "kommune",
                                 position = "keep",
                                 overwrite = TRUE)

  expect_identical(kommuner,
                   correct_result[, c("kommune", "komnr")])


})


test_that("errors for read_kommune_fylke", {
  
  linewidth <- options("width")
  options(width = 80)
  
  expect_error(read_kommune_fylke(filename = NULL, from_path = tempdir()) ,
               regexp = "Variable 'filename': Must be of type 'list', not 'NULL'.",
               fixed = TRUE)
  
  expect_error(read_kommune_fylke(filename = "filename.csv", from_path = tempdir()) ,
               regexp = "File does not exist:")
  
  options(width = unlist(linewidth))
})

