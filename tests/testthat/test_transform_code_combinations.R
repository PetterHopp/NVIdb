library(NVIdb)
library(testthat)

test_that("transform code combinations, from 2 columns to 2 columns", {
  #  A code combination of two is tranformed to another code combination of two in the original columns
  data <- as.data.frame(cbind(c("Detected", "Detected", "Not detected", NA),
                              c("M. bovis", "M. kansasii", "M. bovis", NA)))
  colnames(data) <- c("kjennelse", "analytt")

  data <- transform_code_combinations(data = data,
                                      from_values = list("kjennelse" = c("Detected"),
                                                         "analytt" = c("M. kansasii")),
                                      to_values = list("kjennelse" = c("Not detected"),
                                                       "analytt" = c("M. bovis")),
                                      impute_when_missing_from = c("kjennelse", "analytt"))

  correct_result <- as.data.frame(cbind(c("Detected", "Not detected", "Not detected", NA),
                                        c("M. bovis", "M. bovis", "M. bovis", NA)))
  colnames(correct_result) <- c("kjennelse", "analytt")
  expect_identical(data, correct_result)

  #  A code combination of two is transformed to another code combination of two into new columns
  data <- as.data.frame(cbind(c("Detected", "Detected", "Not detected", NA),
                              c("M. bovis", "M. kansasii", "M. bovis", NA)))
  colnames(data) <- c("kjennelse", "analytt")

  correct_result <- as.data.frame(cbind(data,
                                        c("Detected", "Not detected", "Not detected", NA),
                                        c("M. bovis", "M. bovis", "M. bovis", NA)))
  colnames(correct_result) <- c("kjennelse", "analytt", "kjennelse2", "analytt2")

  data <- transform_code_combinations(data = data,
                                      from_values = list("kjennelse" = c("Detected"),
                                                         "analytt" = c("M. kansasii")),
                                      to_values = list("kjennelse2" = c("Not detected"),
                                                       "analytt2" = c("M. bovis")),
                                      impute_when_missing_from = c("kjennelse", "analytt"))

  expect_identical(data, correct_result)
})

test_that("transform_code_combinations: two variables to one", {

  # two code values to one new variable
  data <- as.data.frame(cbind(c("hjort", "rein", "rein", "elg", "hjort", "rein", "rein", NA),
                              c("produksjonsdyr", "ville dyr", "produksjonsdyr", "ville dyr",
                                "ville dyr", "produksjonsdyr", "ville dyr", NA)))
  colnames(data) <- c("art", "driftsform")

  correct_result <- cbind(data,
                          c("oppdrettshjort", "villrein", "tamrein", "elg", "hjort", "tamrein", "villrein", "ukjent"))
  colnames(correct_result) <- c("art", "driftsform", "art2")

  data <- transform_code_combinations(data = data,
                                      from_values = list("art" = c("hjort", "rein", "rein", NA),
                                                         "driftsform" = c("produksjonsdyr", "ville dyr", "produksjonsdyr", NA)),
                                      to_values = list("art2" = c("oppdrettshjort", "villrein", "tamrein", "ukjent")),
                                      impute_when_missing_from = "art")

  expect_identical(data, correct_result)
})
#

test_that("transform_code_combinations: one variable to three", {

  data <- as.data.frame(c("fixed organs", "fresh bulk milk", "blood sample", NA))
  colnames(data) <- c("material")

  correct_result <- cbind(data,
                          c("organs", "milk", "blood", NA),
                          c("single sample", "bulk milk", "single sample", NA),
                          c("fixed", "fresh", "fresh", NA))
  colnames(correct_result) <- c("material", "material_type", "sample_type", "preparation")

  data <- transform_code_combinations(data = data,
                                      from_values = list("material" = c("fixed organs", "fresh bulk milk", "blood sample", NA)),
                                      to_values = list("material_type" = c("organs", "milk", "blood", NA),
                                                       "sample_type" = c("single sample", "bulk milk", "single sample", NA),
                                                       "preparation" = c("fixed", "fresh", "fresh", NA)),
                                      impute_when_missing_from = "material")

  expect_identical(data, correct_result)
})
