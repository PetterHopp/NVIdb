# library(NVIdb)
library(testthat)

test_that("errors for defunct functions", {

  linewidth <- options("width")
  options(width = 80)

  expect_error(set_PAT(),
               regexp = "'set_PAT', 'get_PAT', and 'remove_PAT' should be replaced by",
               fixed = TRUE)

  expect_error(get_PAT(),
               regexp = "'set_PAT', 'get_PAT', and 'remove_PAT' should be replaced by",
               fixed = TRUE)

  expect_error(remove_PAT(),
               regexp = "'set_PAT', 'get_PAT', and 'remove_PAT' should be replaced by",
               fixed = TRUE)

  expect_error(set_credentials_EOS(),
               regexp = "'set_credentials_EOS' is replaced by 'set_credentials('EOS')' as",
               fixed = TRUE)

  expect_error(login_EOS(),
               regexp = "'login_EOS' is replaced by 'login('EOS')' as the wrapper",
               fixed = TRUE)

  expect_error(login_by_input_EOS(),
               regexp = "'login_by_input_EOS' is replaced by 'login_by_input('EOS')' as",
               fixed = TRUE)

  expect_error(login_by_credentials_EOS(),
               regexp = "'login_by_credentials_EOS' is replaced by 'login_by_credentials('EOS')'",
               fixed = TRUE)

  expect_error(login_PJS(),
               regexp = "'login_PJS' is replaced by 'login('PJS')' as the wrapper",
               fixed = TRUE)

  expect_error(login_by_input_PJS(),
               regexp = "'login_by_input_PJS' is replaced by 'login_by_input('PJS')' as",
               fixed = TRUE)

  expect_error(login_by_credentials_PJS(),
               regexp = "'login_by_credentials_PJS' is replaced by 'login_by_credentials('PJS')'",
               fixed = TRUE)

  expect_error(read_eos_data(),
               regexp = "Use 'NVIpjsr::read_eos_data' instead.",
               fixed = TRUE)

  expect_error(standardize_eos_data(),
               regexp = "Use 'NVIpjsr::standardize_eos_data' instead.",
               fixed = TRUE)

  expect_error(choose_PJS_levels(),
               regexp = "Use 'NVIpjsr::choose_PJS_levels' instead.",
               fixed = TRUE)

  expect_error(build_query_hensikt(),
               regexp = "Use 'NVIpjsr::build_query_hensikt' instead.",
               fixed = TRUE)

  expect_error(exclude_from_PJSdata(),
               regexp = "Use 'NVIpjsr::exclude_from_PJSdata' instead.",
               fixed = TRUE)

  expect_error(retrieve_PJSdata(),
               regexp = "Use 'NVIpjsr::retrieve_PJSdata' instead.",
               fixed = TRUE)

  expect_error(select_PJSdata_for_value(),
               regexp = "Use 'NVIpjsr::select_PJSdata_for_value' instead.",
               fixed = TRUE)

  expect_error(set_disease_parameters(),
               regexp = "Use 'NVIpjsr::set_disease_parameters' instead.",
               fixed = TRUE)

  expect_error(standardize_PJSdata(),
               regexp = "Use 'NVIpjsr::standardize_PJSdata' instead.",
               fixed = TRUE)

  expect_error(transform_code_combinations(),
               regexp = "Use 'NVIpjsr::transform_code_combinations' instead.",
               fixed = TRUE)

  options(width = unlist(linewidth))
})
