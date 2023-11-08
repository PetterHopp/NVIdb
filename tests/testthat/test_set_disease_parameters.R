library(NVIdb)
library(testthat)

test_that("set disease parameters by direct input", {
  parameters <- set_disease_parameters(hensikt2select = c("0100108018", "0100109003", "0100111003", "0800109"),
                                       analytt2select = c("01220104%", "1502010235"),
                                       metode2select = c("070070", "070231", "010057", "060265"))
  expect_equal(parameters,
               list("hensikt2select" = c("0100108018", "0100109003", "0100111003", "0800109"),
                    "hensikt2delete" = NULL,
                    "utbrudd2select" = NULL,
                    "metode2select" = c("070070", "070231", "010057", "060265"),
                    "analytt2select" = c("01220104%", "1502010235"),
                    "analytt2delete" = NULL,
                    "art2select" = NULL,
                    "include_missing_art" = "never"))

  parameters2 <- set_disease_parameters(selection_parameters = parameters)
  expect_equal(parameters2,
               list("hensikt2select" = c("0100108018", "0100109003", "0100111003", "0800109"),
                    "hensikt2delete" = NULL,
                    "utbrudd2select" = NULL,
                    "metode2select" = c("070070", "070231", "010057", "060265"),
                    "analytt2select" = c("01220104%", "1502010235"),
                    "analytt2delete" = NULL,
                    "art2select" = NULL,
                    "include_missing_art" = "never"))


  parameters <- set_disease_parameters(hensikt2select = c("0100108018", "0100109003", "0100111003"),
                                       hensikt2delete = c("0800109"),
                                       utbrudd2select = "22",
                                       analytt2select = c("01220104%", "1502010235"),
                                       metode2select = c("070070", "070231", "010057", "060265"))
  expect_equal(parameters,
               list("hensikt2select" = c("0100108018", "0100109003", "0100111003"),
                    "hensikt2delete" = c("0800109"),
                    "utbrudd2select" = "22",
                    "metode2select" = c("070070", "070231", "010057", "060265"),
                    "analytt2select" = c("01220104%", "1502010235"),
                    "analytt2delete" = NULL,
                    "art2select" = NULL,
                    "include_missing_art" = "never"))

  parameters2 <- set_disease_parameters(selection_parameters = parameters)
  expect_equal(parameters2,
               list("hensikt2select" = c("0100108018", "0100109003", "0100111003"),
                    "hensikt2delete" = c("0800109"),
                    "utbrudd2select" = "22",
                    "metode2select" = c("070070", "070231", "010057", "060265"),
                    "analytt2select" = c("01220104%", "1502010235"),
                    "analytt2delete" = NULL,
                    "art2select" = NULL,
                    "include_missing_art" = "never"))

  parameters <- set_disease_parameters(hensikt2select = c("0100108018", "0100109003", "0100111003"),
                                       hensikt2delete = c("0800109"),
                                       utbrudd2select = "22",
                                       metode2select = NULL,
                                       art2select = c("01%"),
                                       include_missing_art = "never")
  expect_equal(parameters,
               list("hensikt2select" = c("0100108018", "0100109003", "0100111003"),
                    "hensikt2delete" = c("0800109"),
                    "utbrudd2select" = "22",
                    "metode2select" = NULL,
                    "analytt2select" = NULL,
                    "analytt2delete" = NULL,
                    "art2select" = c("01%"),
                    "include_missing_art" = "never"))

  parameters <- set_disease_parameters(hensikt2select = c("0100108018", "0100109003", "0100111003"),
                                       hensikt2delete = c("0800109"),
                                       utbrudd2select = "22",
                                       metode2select = NULL,
                                       art2select = c("01%"),
                                       include_missing_art = NULL)
  expect_equal(parameters,
               list("hensikt2select" = c("0100108018", "0100109003", "0100111003"),
                    "hensikt2delete" = c("0800109"),
                    "utbrudd2select" = "22",
                    "metode2select" = NULL,
                    "analytt2select" = NULL,
                    "analytt2delete" = NULL,
                    "art2select" = c("01%"),
                    "include_missing_art" = "never"))

  parameters <- set_disease_parameters(hensikt2select = c("0100108018", "0100109003", "0100111003"),
                                       hensikt2delete = c("0800109"),
                                       utbrudd2select = "22",
                                       metode2select = NULL,
                                       art2select = c("01%", NA),
                                       include_missing_art = NULL)
  expect_equal(parameters,
               list("hensikt2select" = c("0100108018", "0100109003", "0100111003"),
                    "hensikt2delete" = c("0800109"),
                    "utbrudd2select" = "22",
                    "metode2select" = NULL,
                    "analytt2select" = NULL,
                    "analytt2delete" = NULL,
                    "art2select" = c("01%", NA),
                    "include_missing_art" = "always"))

  parameters2 <- set_disease_parameters(selection_parameters = parameters)
  expect_equal(parameters2,
               list("hensikt2select" = c("0100108018", "0100109003", "0100111003"),
                    "hensikt2delete" = c("0800109"),
                    "utbrudd2select" = "22",
                    "metode2select" = NULL,
                    "analytt2select" = NULL,
                    "analytt2delete" = NULL,
                    "art2select" = c("01%", NA),
                    "include_missing_art" = "always"))

})

test_that("set disease parameters using parameter file", {
  writeLines(
    c('hensikt2select <- c("0100108018", "0100109003", "0100111003", "0800109")',
      'utbrudd2select <- NULL',
      'metode2select <- c("070070", "070231", "010057", "060265")',
      'analytt2select <- c("01220104%", "1502010235")'),
    con = file.path(tempdir(), "PD.R")
  )

  parameters <- set_disease_parameters(file = file.path(tempdir(), "PD.R"))
  expect_equal(parameters,
               list("hensikt2select" = c("0100108018", "0100109003", "0100111003", "0800109"),
                    "hensikt2delete" = NULL,
                    "utbrudd2select" = NULL,
                    "metode2select" = c("070070", "070231", "010057", "060265"),
                    "analytt2select" = c("01220104%", "1502010235"),
                    "analytt2delete" = NULL,
                    "art2select" = NULL,
                    "include_missing_art" = "never"))

  parameters <- set_disease_parameters(selection_parameters = file.path(tempdir(), "PD.R"))
  expect_equal(parameters,
               list("hensikt2select" = c("0100108018", "0100109003", "0100111003", "0800109"),
                    "hensikt2delete" = NULL,
                    "utbrudd2select" = NULL,
                    "metode2select" = c("070070", "070231", "010057", "060265"),
                    "analytt2select" = c("01220104%", "1502010235"),
                    "analytt2delete" = NULL,
                    "art2select" = NULL,
                    "include_missing_art" = "never"))
})


test_that("errors for set_disease_parameters", {
  linewidth <- options("width")
  options(width = 80)

  expect_error(set_disease_parameters(hensikt2select = "0100108018",
                                      analytt2select = "0",
                                      metode2select = "070070"),
               regexp = "'analytt2select': All elements must have at least 2")

  expect_error(set_disease_parameters(hensikt2select = "0100108018",
                                      analytt2select = "012201040122010401220104",
                                      metode2select = "070070"),
               regexp = "but element 1 has 24 characters")

  expect_error(set_disease_parameters(hensikt2select = "0100108018",
                                      analytt2select = NA,
                                      metode2select = "070070"),
               regexp = "'analytt2select': Contains missing values")

  expect_error(set_disease_parameters(hensikt2select = NA,
                                      analytt2select = NA,
                                      metode2select = NA),
               regexp = "'hensikt2select': Contains missing values")

  expect_error(set_disease_parameters(hensikt2select = "0100108018",
                                      analytt2select = "01220104%",
                                      utbrudd2select = NA),
               regexp = "'utbrudd2select': Contains missing values")

  expect_error(set_disease_parameters(hensikt2select = NULL,
                                      analytt2select = NULL,
                                      utbrudd2select = NULL),
               regexp = "have input different from NULL and NA")

  expect_error(set_disease_parameters(hensikt2delete = "01001080180100108018",
                                      analytt2select = "01220104%",
                                      utbrudd2select = "2"),
               regexp = "but element 1 has 20 characters")

  # expect_error(set_disease_parameters(hensikt2delete = "0100108018",
  #                                     analytt2select = "01220104%",
  #                                     utbrudd2select = "2",
  #                                     art2select = "05%"),
  #              regexp = "Variable 'include_missing_art': Must be a subset of")

  expect_error(set_disease_parameters(hensikt2delete = "0100108018",
                                      analytt2select = "01220104%",
                                      utbrudd2select = "2",
                                      art2select = c("05%", NA),
                                      include_missing_art = "yes"),
               regexp = "Variable 'include_missing_art': Must be element of set")

  options(width = unlist(linewidth))
})
