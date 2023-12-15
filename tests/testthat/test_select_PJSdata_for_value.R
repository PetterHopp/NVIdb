library(NVIdb)
library(testthat)

test_that("test select_PJSdata_for_value", {

  df <- as.data.frame(cbind(saksnr = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    code1 = c("01", "01", "01001", "01001", "02", "02", "02001", "02002", "0200201", NA),
                      code2 = c("03", "03", "03003", "03004", "04", "05", "04003", "04004", "0400403", NA)))

  correct_result <- as.data.frame(cbind(saksnr = c(1, 2),
                                        code1 = c("01", "01"),
                      code2 = c("03", "03")))

  expect_identical(select_PJSdata_for_value(data = df,
                                      code_column = "code1",
                                      value_2_check = "01",
                                      keep_selected = TRUE),
                   correct_result)

  correct_result <- as.data.frame(cbind(saksnr = c(1, 2, 3, 4),
                                        code1 = c("01", "01", "01001", "01001"),
                      code2 = c("03", "03", "03003", "03004")))

  expect_identical(select_PJSdata_for_value(data = df,
                                      code_column = "code1",
                                      value_2_check = "01%",
                                      keep_selected = TRUE),
                   correct_result)

  correct_result <- as.data.frame(cbind(saksnr = c(10),
                                        code1 = c(NA_character_),
                      code2 = c(NA_character_)))

  expect_identical(select_PJSdata_for_value(data = df,
                                      code_column = "code1",
                                      value_2_check = "NA",
                                      keep_selected = TRUE),
                   correct_result)


    correct_result <- as.data.frame(cbind(saksnr = c(1, 2, 5, 6, 7, 8, 9, 10),
                                        code1 = c("01", "01", "02", "02", "02001", "02002", "0200201", NA_character_),
                                        code2 = c("03", "03", "04", "05", "04003", "04004", "0400403", NA_character_)))

    result <- select_PJSdata_for_value(data = df,
                                      code_column = "code1",
                                      value_2_check = c("01", "02%", "NA"),
                                      keep_selected = TRUE)
    rownames(result) <- NULL
  expect_identical(result,
                   correct_result)

  correct_result <- as.data.frame(cbind(saksnr = c(3, 4, 5, 6, 7, 8, 9, 10),
                                        code1 = c("01001", "01001", "02", "02", "02001", "02002", "0200201", NA),
                                        code2 = c("03003", "03004", "04", "05", "04003", "04004", "0400403", NA)))

  result <- select_PJSdata_for_value(data = df,
                                     code_column = "code1",
                                     value_2_check = c("01"),
                                     keep_selected = FALSE)
  rownames(result) <- NULL
  expect_identical(result,
                   correct_result)

  correct_result <- as.data.frame(cbind(saksnr = c(3, 4),
                                        code1 = c("01001", "01001"),
                                        code2 = c("03003", "03004")))

  result <- select_PJSdata_for_value(data = df,
                                     code_column = "code1",
                                     value_2_check = c("01", "02%", "NA"),
                                     keep_selected = FALSE)
  rownames(result) <- NULL
  expect_identical(result,
                   correct_result)


    correct_result <- as.data.frame(cbind(saksnr = c(1, 2),
                                        code1 = c("01", "01"),
                                        code2 = c("03", "03")))

    expect_identical(select_PJSdata_for_value(data = df,
                                              code_column = c("code1", "code2"),
                                              value_2_check = "01%-03",
                                              keep_selected = TRUE),
                     correct_result)

    correct_result <- as.data.frame(cbind(saksnr = c(1, 2, 3, 4),
                                          code1 = c("01", "01", "01001", "01001"),
                                          code2 = c("03", "03", "03003", "03004")))

    expect_identical(select_PJSdata_for_value(data = df,
                                              code_column = c("code1", "code2"),
                                              value_2_check = "01%-03%",
                                              keep_selected = TRUE),
                     correct_result)


})


test_that("Errors or warnings for select_PJSdata_for_value", {
  linewidth <- options("width")
  options(width = 80)

  df <- as.data.frame(cbind(saksnr = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                            code1 = c("01", "01", "01001", "01001", "02", "02", "02001", "02002", "0200201", NA),
                            code2 = c("03", "03", "03003", "03004", "04", "05", "04003", "04004", "0400403", NA)))

  expect_error(select_PJSdata_for_value(data = "df",
                                        code_column = "code1",
                                        value_2_check = "01",
                                        keep_selected = TRUE),
               regexpr = "Variable 'data': Must be of type 'data.frame', not 'character'")

  expect_error(select_PJSdata_for_value(data = df,
                                        code_column = "code4",
                                        value_2_check = "01",
                                        keep_selected = TRUE),
               regexpr = "{'saksnr','code1','code2'}, but has additional elements {'code4'}",
               fixed = TRUE)

  expect_error(select_PJSdata_for_value(data = df,
                                        code_column = "code1",
                                        value_2_check = TRUE,
                                        keep_selected = TRUE),
               regexpr = "'value_2_check': Must be of type 'character'")

  expect_error(select_PJSdata_for_value(data = df,
                                        code_column = "code1",
                                        value_2_check = "01",
                                        keep_selected = "TRUE"),
               regexpr = "Variable 'keep_selected': Must be of type 'logical flag'")

  options(width = unlist(linewidth))
})
