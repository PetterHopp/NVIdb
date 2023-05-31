library(NVIdb)
library(testthat)

test_that("transform code combinations, from 2 columns to 2 columns", {
  #  A code combination of two is tranformed to another code combination of two
  data <- as.data.frame(cbind(c("Detected", "Detected", "Not detected", NA),
                              c("M. bovis", "M. kansasii", "M. bovis", NA)))
  colnames(data) <- c("kjennelse", "analytt")
  
  transform_code_combinations(data = data,
                              from_values = list(c("Detected"),
                                                 c("M. kansasii")),
                              to_values = list(c("Not detected"),
                                               c("M. bovis")),
                              from_columns = c("kjennelse", "analytt"),
                              to_columns = c("kjennelse", "analytt"),
                              impute_when_missing_from = c("kjennelse", "analytt")
  )
  
  correct_result <- as.data.frame(cbind(c("Detected", "Detected", "Not detected", NA),
                                        c("M. bovis", "M. kansasii", "M. bovis", NA)))
  
  # # examples
  # # two code values to one new varable
  # from_values <- list(c("hjort", "rein", "rein", NA), 
  #                     c("produksjonsdyr", "ville dyr", "produksjonsdyr", NA))
  # to_values <- list(c("oppdrettshjort", "villrein", "tamrein", "Ukjent"))
  # from_columns <- c("art", "driftsform")
  # to_columns <- c("art2")
  # impute_when_missing_from <- "art"
  # 
  # PJSdata <- as.data.frame(cbind(c("hjort", "rein", "rein", "elg", "hjort", "rein", "rein", NA),
  #                                c("produksjonsdyr", "ville dyr", "produksjonsdyr", "ville dyr", "ville dyr", "produksjonsdyr", "ville dyr", NA)))
  # colnames(PJSdata) <- c("art", "driftsform")
  # data <- PJSdata
  # 
  # 
  
expect_equal(cut_slash("C:/temp/"), "C:/temp")

expect_equal(cut_slash("C:\\temp\\"), "C:\\temp")

expect_equal(cut_slash(c("C:/temp/", "C:\\temp\\")), c("C:/temp", "C:\\temp"))

expect_equal(cut_slash(list("C:/temp/", "C:\\temp\\")), c("C:/temp", "C:\\temp"))
})


library(NVIdb) 
library(testthat)

test_that("transform_code_combinations: two variables to one", {
  
  # two code values to one new varable
  data <- as.data.frame(cbind(c("hjort", "rein", "rein", "elg", "hjort", "rein", "rein", NA),
                              c("produksjonsdyr", "ville dyr", "produksjonsdyr", "ville dyr", "ville dyr", "produksjonsdyr", "ville dyr", NA)))
  colnames(data) <- c("art", "driftsform")
  
  correct_result <- rbind(data, 
                          c("opdrettshjort", "villrein", "tamrein", "elg", "hjort", "tamrein", "villrein", "ukjent")) 
  
  tranform_code_combinations(data = data, 
                             from_values <- list(c("hjort", "rein", "rein", NA), 
                                                 c("produksjonsdyr", "ville dyr", "produksjonsdyr", NA)), 
                             to_values <- list(c("oppdrettshjort", "villrein", "tamrein", "ukjent")), 
                             from_columns <- c("art", "driftsform"), 
                             to_columns <- c("art2"), 
                             impute_when_missing_from <- "art") 
  
  expect_identical(data, correct_result) 
}) 
#

test_that("transform_code_combinations: one variable to three", {
  
  data <- as.data.frame(c("fixed organs", "fresh bulk milk", "blood sample", NA)) 
  colnames(data)     <- c("material")
  
  correct_result <- rbind(data, 
                          c("single sample", "bulk milk", "single sample", NA), 
                          c("fixed", "fresh", "fresh", NA), 
                          c("organs", "milk", "blood", NA)) 
  
  tranform_code_combinations(data = data, 
                             from_values <- list(c("hjort", "rein", "rein", NA), 
                                                 c("produksjonsdyr", "ville dyr", "produksjonsdyr", NA)), 
                             to_values <- list(c("oppdrettshjort", "villrein", "tamrein", "ukjent")), 
                             from_columns <- c("art", "driftsform"), 
                             to_columns <- c("art2"), 
                             impute_when_missing_from <- "art") 
  
  expect_identical(data, correct_result) 
}) 
