library(NVIdb)
library(testthat)

test_that("set disease parameters by direct input", {
  parameters <- set_disease_parameters(analytt2select = c("01220104%", "1502010235"),
                                       hensikt2select = c("0100108018", "0100109003", "0100111003", "0800109"),
                                       metode2select = c("070070", "070231", "010057", "060265"))
  expect_equal(parameters,
               list("hensikt2select" = c("0100108018", "0100109003", "0100111003", "0800109"),
                    "utbrudd2select" = NULL,
                    "metode2select" = c("070070", "070231", "010057", "060265"),
                    "analytt2select" = c("01220104%", "1502010235")))
})

# test_that("set disease parameters using parameter file", {
#   td <- tempdir()
#   writeLines (
#     paste('hensikt2select <- c("0100108018", "0100109003", "0100111003", "0800109")' ,
#           'utbrudd2select <- NULL',
#           'metode2select <- c("070070", "070231", "010057", "060265")' ,
#           'analytt2select <- c("01220104%", "1502010235")',
#           "",
#           sep ="\n")
#   )
#   parameters <- set_disease_parameters( file = file.path(td, "PD.txt") )
#   expect_equal(parameters,
#                list(         "hensikt2select" =           c("0100108018", "0100109003", "0100111003", "0800109"),
#                              "metode2select" = c("070070", "070231", "010057", "060265"),
#                              "analytt2select" = c("01220104%", "1502010235")))
# })


test_that("errors for set_disease_parameters", {
  linewidth <- options("width")
  options(width = 80)
  
  expect_error(set_disease_parameters(analytt2select = "0",
                                      hensikt2select = "0100108018",
                                      metode2select = "070070"),
               regexp = "'analytt2select': All elements must have at least 2")

  expect_error(set_disease_parameters(analytt2select = NA,
                                      hensikt2select = "0100108018",
                                      metode2select = "070070"),
               regexp = "'analytt2select': Contains missing values")

  expect_error(set_disease_parameters(analytt2select = NA,
                                      hensikt2select = NA,
                                      metode2select = NA),
               regexp = "'hensikt2select': Contains missing values")

  expect_error(set_disease_parameters(analytt2select = "01220104%",
                                      hensikt2select = "0100108018",
                                      utbrudd2select = NA),
               regexp = "'utbrudd2select': Contains missing values")

  options(width = unlist(linewidth))
})
