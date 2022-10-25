library(NVIdb)
library(testthat)

test_that("Test Standardize PJSdata and column change column names and class", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  PJStest <- readRDS(file.path(".", "PJS_testdata.rds"))
  # PJStest <- readRDS("./tests/testthat/PJS_testdata.rds")

  # Standardisere kolonnenavn
  PJStest2 <- standardize_PJSdata(PJSdata = PJStest)

  # test name changes
  correct_result <- c("innsendelsenr", "eier_lokalitettype", "mottatt", "uttatt", "avsluttet",
                      "komnr", "annen_aktortype", "annen_aktornr", "utbruddnr", "provenr",
                      "fysiologisk_stadiumkode", "uttatt_parprove", "mottatt_parprove", "merknad_prove", "delprovenr",
                      "undnr", "und_godkjent", "und_avsluttet", "resnr", "res_analyttkode",
                      "enhetkode", "res_kjennelsekode", "konklnr", "konkl_provenr", "konkl_kjennelsekode",
                      "konkl_typekode", "eier_lokalitet", "saksnr")

  expect_identical(setdiff(colnames(PJStest2), colnames(PJStest)), correct_result)

  # test numeric columns
  correct_result <- intersect(colnames(PJStest2),
                              c("aar", "innsendelsenr", "provenr", "delprovenr", "undnr",
                                "resnr", "sens_undnr", "sensresnr", "konklnr",
                                "ant_prover", "ant_i_samleprove", "ant_delprover", "ant_i_samledelprove"))

  expect_identical(intersect(names(which(sapply(PJStest2, is.numeric))),
                             c("aar", "innsendelsenr", "provenr", "delprovenr", "undnr",
                               "resnr", "sens_undnr", "sensresnr", "konklnr",
                               "ant_prover", "ant_i_samleprove", "ant_delprover", "ant_i_samledelprove")),
                   correct_result)

  # test Date class
    expect_identical(names(which(unlist(lapply(PJStest2, class)) == "Date")),
                     intersect(colnames(PJStest2), c("mottatt", "uttatt", "avsluttet", "sak_forst_avsluttet",
                                                     "uttatt_parprove", "mottatt_parprove",
                                                     "und_godkjent", "und_avsluttet")))

})


test_that("errors for standardize_PJSdata", {

  PJStest <- readRDS(file.path(".", "PJS_testdata.rds"))
  # PJStest <- readRDS("./tests/testthat/PJS_testdata.rds")

  expect_error(standardize_PJSdata(PJSdata = PJStest, dbsource = NULL),
               regexp = "Variable 'dbsource': Must be of type 'character'")

  expect_error(standardize_PJSdata(PJSdata = "no_data"),
               regexp = "Variable 'PJSdata': Must be of type 'data.frame', not 'character'.")

})
