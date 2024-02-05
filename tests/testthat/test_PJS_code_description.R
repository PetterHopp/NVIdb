library(NVIdb)
library(RODBC)
library(testthat)

# Login to PJS
# Suppress warnings to avoid warnings if not on NVI network
journal_rapp <- suppressWarnings(login_by_credentials_PJS())

# Assigns temporary dir to td
td <- tempdir()

test_that("Copy PJS_codes_2_text", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # copy_PJS_codes_2_text
  copy_PJS_codes_2_text(to_path = td)

  expect_true(file.exists(file.path(td, "PJS_codes_2_text.csv")))

})

test_that("read PJS_codes_2_text", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Reads translation table for PJS-codes
  PJS_codes_2_text <- read_PJS_codes_2_text()

  # check that data.frame
  expect_identical(class(PJS_codes_2_text), "data.frame")
  # check number of columns
  expect_equal(dim(PJS_codes_2_text)[2], 4)
  # check number of rows
  expect_gt(dim(PJS_codes_2_text)[1], 16500)
  expect_lt(dim(PJS_codes_2_text)[1], 30000)

})

test_that("If translation table is updated", {
  # skip if no connection to PJS have been established
  skip_if(journal_rapp < 1)

  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Reads translation table for PJS-codes
  PJS_codes_2_text <- read_PJS_codes_2_text()

  # Test if hensikter is correctly translated
  # Read hensikter from PJS
  hensikter <- RODBC::sqlQuery(journal_rapp,
                               "select * from v_hensikt",
                               as.is = TRUE,
                               stringsAsFactors = FALSE)
  hensikter$hensiktnavn <- trimws(hensikter$hensiktnavn)

  # Add new description based on NVIdb
  hensikter <- add_PJS_code_description(hensikter,
                                        translation_table = PJS_codes_2_text,
                                        code_colname = c("hensiktkode"),
                                        PJS_variable_type = c("hensikt"),
                                        new_column = c("NVIdb_hensikt"))

  # Compare new and old code descriptions
  expect_identical(hensikter$NVIdb_hensikt,
                   hensikter$hensiktnavn)
  # expect_identical(gsub('\"', "", hensikter$NVIdb_hensikt),
  #                  gsub('\"', "", hensikter$hensiktnavn))

  # Test if driftsform is correctly translated
  # Read driftsform from PJS
  driftsform <- RODBC::sqlQuery(journal_rapp,
                                "select * from driftsform",
                                as.is = TRUE,
                                stringsAsFactors = FALSE)
  driftsform$driftsformnavn <- trimws(driftsform$driftsformnavn)

  # Add new description based on NVIdb
  driftsform <- add_PJS_code_description(driftsform,
                                         translation_table = PJS_codes_2_text,
                                         code_colname = c("driftsformkode"),
                                         PJS_variable_type = c("driftsform"),
                                         new_column = c("NVIdb_driftsform"))

  # Compare new and old code descriptions
  expect_identical(driftsform$NVIdb_driftsform,
                   driftsform$driftsformnavn)

})


test_that("Translate codes in PJS-data", {

  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Reads translation table for PJS-codes
  PJS_codes_2_text <- read_PJS_codes_2_text()

  testdata <- as.data.frame(cbind(rbind(c("hensiktkode" = "01001"), c("hensiktkode" = "01002")),
                                  rbind(c("metodekode" = "010001"), c("metodekode" = "010002")),
                                  rbind(c("ansvarlig_seksjon" = "01"), c("ansvarlig_seksjon" = "02"))))

  correct_result <- as.data.frame(cbind(rbind(c("hensiktkode" = "01001"), c("hensiktkode" = "01002")),
                                        rbind(c("hensikt" = "Oppklaring og oppfølging av helseproblem hos dyr"), c("hensikt" = "Oppklaring og oppfølging av helseproblem hos menneske")),
                                        rbind(c("metodekode" = "010001"), c("metodekode" = "010002")),
                                        rbind(c("metode" = "Obduksjon/organundersøkelse"), c("metode" = "Histopatologi")),
                                        rbind(c("ansvarlig_seksjon" = "01"), c("ansvarlig_seksjon" = "02")),
                                        rbind(c("seksjon" = "Bakteriologi - Fisk og dyr"), c("seksjon" = "Virologi"))))

  testdata <- add_PJS_code_description(testdata,
                                       translation_table = PJS_codes_2_text,
                                       code_colname = c("hensiktkode", "metodekode", "ansvarlig_seksjon"),
                                       PJS_variable_type = c("hensikt", "metode", "seksjon"),
                                       new_column = c("hensikt", "metode", "seksjon"))

  # Compare new and old code descriptions
  expect_identical(testdata, correct_result)

  testdata <- add_PJS_code_description(testdata,
                                       translation_table = PJS_codes_2_text,
                                       code_colname = c("hensiktkode", "metodekode", "ansvarlig_seksjon"),
                                       PJS_variable_type = c("hensikt", "metode", "seksjon"),
                                       new_column = c("hensikt", "metode", "seksjon"),
                                       position = "left",
                                       overwrite = TRUE)

  # Compare new and old code descriptions
  expect_identical(testdata, correct_result[, c("hensikt", "hensiktkode", "metode", "metodekode", "seksjon", "ansvarlig_seksjon")])

  testdata <- add_PJS_code_description(testdata,
                                       translation_table = PJS_codes_2_text,
                                       code_colname = c("hensiktkode", "metodekode", "ansvarlig_seksjon"),
                                       PJS_variable_type = c("hensikt", "metode", "seksjon"),
                                       new_column = c("hensikt", "metode", "seksjon"),
                                       position = c("keep", "first", "last"),
                                       overwrite = TRUE)

  # Compare new and old code descriptions
  expect_identical(testdata, correct_result[, c("metode", "hensikt", "hensiktkode", "metodekode", "ansvarlig_seksjon", "seksjon")])

})

test_that("Translate codes using 'auto'", {

  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Reads translation table for PJS-codes
  PJS_codes_2_text <- read_PJS_codes_2_text()

  testdata <- as.data.frame(cbind(rbind(c("hensiktkode" = "01001"), c("hensiktkode" = "01002")),
                                  rbind(c("metodekode" = "010001"), c("metodekode" = "010002")),
                                  rbind(c("ansvarlig_seksjon" = "01"), c("ansvarlig_seksjon" = "02"))))

  correct_result <- as.data.frame(cbind(rbind(c("hensiktkode" = "01001"), c("hensiktkode" = "01002")),
                                        rbind(c("hensikt" = "Oppklaring og oppfølging av helseproblem hos dyr"), c("hensikt" = "Oppklaring og oppfølging av helseproblem hos menneske")),
                                        rbind(c("metodekode" = "010001"), c("metodekode" = "010002")),
                                        rbind(c("metode" = "Obduksjon/organundersøkelse"), c("metode" = "Histopatologi")),
                                        rbind(c("ansvarlig_seksjon" = "01"), c("ansvarlig_seksjon" = "02")),
                                        rbind(c("ansvarlig_seksjon_navn" = "Bakteriologi - Fisk og dyr"), c("ansvarlig_seksjon_navn" = "Virologi"))))

  testdata <- add_PJS_code_description(testdata,
                                       translation_table = PJS_codes_2_text,
                                       code_colname = c("hensiktkode", "metodekode", "ansvarlig_seksjon"),
                                       PJS_variable_type = c("auto"),
                                       new_column = c("auto"))

  # Compare new and old code descriptions
  expect_identical(testdata, correct_result)

})

test_that("Backward translation from description to code", {

  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Reads translation table for PJS-codes
  PJS_codes_2_text <- read_PJS_codes_2_text()

  art <- c("Storfe", "storfe", "Pattedyr", "Laks", "Alpakka", "Mops")
  testdata <- as.data.frame(art)


  correct_result <- as.data.frame(cbind(rbind("03100202001", "03100202001", "03",
                                                "04031903001001", "03100203009002", NA_character_),
                                        testdata))
 colnames(correct_result) <- c("artkode", "art")

  testdata <- add_PJS_code_description(testdata,
                                       translation_table = PJS_codes_2_text,
                                       code_colname = c("art"),
                                       PJS_variable_type = c("art"),
                                       new_column = c("artkode"),
                                       position = "left",
                                       backward = TRUE)

  # Compare new and old code descriptions
  rownames(testdata) <- NULL
  expect_identical(testdata, correct_result)


  testdata <- add_PJS_code_description(data = testdata,
                                      translation_table = PJS_codes_2_text,
                                      PJS_variable_type = "artrase",
                                      code_colname = "art",
                                      new_column = "artkode",
                                      position = "left",
                                      overwrite = TRUE,
                                      backward = TRUE)

  correct_result[6, 1] <- "03070101002228"
  # Compare new and old code descriptions
  rownames(testdata) <- NULL
  expect_identical(testdata, correct_result)

})


# test_that("Translate codes in PJS-data", {
#   # skip if no connection to PJS have been established
#   skip_if(journal_rapp < 1)
#
#   sak_res <- sqlQuery(journal_rapp,
#                       "select * from v1_sak_m_res where aar=2020 and innsendelsesnummer == 111 and provenummer == 1",
#                       as.is = TRUE,
#                       stringsAsFactors = FALSE)
#
#   sak_res <- add_PJS_code_description(sak_res,
#                                       translation_table = PJS_codes_2_text,
#                                       code_colname = c("hensiktkode", "metodekode", "ansvarlig_seksjon"),
#                                       PJS_variable_type = c("hensikt", "metode", "seksjon"),
#                                       new_column = c("hensikt", "metode", "seksjon"))
#
#   saker <- unique(sak_res[, c("ansvarlig_seksjon", "seksjon", "hensiktkode", "hensikt")])
#   saker <- saker[order(saker$innsendelsesnummer), ]
#
#   expect_equivalent(saker$hensikt,
#                     c("", "", "", "", "", "", "", "", "", ""))
#   expect_equivalent(saker$seksjon,
#                     c("", "", "", "", "", "", "", "", "", ""))
#   # expect_equivalent(saker$utbrudd,
#   #                   c("", "", "", "", "", "", "", "", "", ""))
# })

test_that("errors for add_PJS_code_description", {

  linewidth <- options("width")
  options(width = 80)

  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Reads translation table for PJS-codes
  PJS_codes_2_text <- read_PJS_codes_2_text()

  testdata <- as.data.frame(list("hensiktkode" = c("01001", "01002"),
                                  "metodekode" = c("010001", "010002"),
                                  "ansvarlig_seksjon" = c("01", "02")))

  expect_error(add_PJS_code_description("testdata",
                                        translation_table = PJS_codes_2_text,
                                        code_colname = c("hensiktkode", "metodekode", "ansvarlig_seksjon"),
                                        PJS_variable_type = c("hensikt", "metode", "seksjon"),
                                        new_column = c("hensikt", "metode", "seksjon")),
               regexp = "Variable 'data': Must be of type 'data.frame', not 'character'.",
               fixed = TRUE)

  expect_error(add_PJS_code_description(testdata,
                                        translation_table = PJS_codes_2_text,
                                        code_colname = c("hensiktkoder"),
                                        PJS_variable_type = c("hensikt"),
                                        new_column = c("hensikt")),
               regexp = "but 'hensiktkoder' is not a column in the data.",
               fixed = TRUE)

  expect_error(add_PJS_code_description(testdata,
                                        translation_table = PJS_codes_2_text,
                                        code_colname = c("hensiktkode"),
                                        PJS_variable_type = c("hensikter"),
                                        new_column = c("hensikt")),
               regexp = "Variable 'PJS_variable_type': Must be a subset of",
               fixed = TRUE)

  expect_error(add_PJS_code_description(testdata,
                                        translation_table = PJS_codes_2_text,
                                        code_colname = c("hensiktkode"),
                                        PJS_variable_type = c("hensikt"),
                                        new_column = c("metodekode")),
               regexp = "The column name(s): 'metodekode' already exist in 'testdata`.",
               fixed = TRUE)

  options(width = unlist(linewidth))
})

RODBC::odbcCloseAll()
