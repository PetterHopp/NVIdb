library(NVIdb)
library(testthat)
library(checkmate)


test_that("read eos_data", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # Read data with saksnr
  campylobacter <- read_eos_data(eos_table = "proveresultat_campylobacter",
                                 from_path = paste0(set_dir_NVI("EOS"), "RaData/"))
  expect_equal(min(substr(campylobacter$saksnr, 1, 4)), "2016")

  checkmate::expect_choice(as.numeric(max(substr(campylobacter$saksnr, 1, 4))),
               choices = c((as.numeric(format(Sys.Date(), "%Y")) - 1), as.numeric(format(Sys.Date(), "%Y"))))

  # Read data with Saksnr
  ila <- read_eos_data(eos_table = "proveresultat_ila",
                       from_path = paste0(set_dir_NVI("EOS"), "RaData/"),
                       year = c(2017:2019))
  expect_equal(min(substr(ila$saksnr, 1, 4)), "2017")

  expect_equal(max(substr(ila$saksnr, 1, 4)), "2019")

  ila <- read_eos_data(eos_table = "proveresultat_ila",
                       from_path = paste0(set_dir_NVI("EOS"), "RaData/"),
                       year = as.numeric(format(Sys.Date(), "%Y")) - 1)
  expect_equal(as.numeric(min(substr(ila$saksnr, 1, 4))), as.numeric(format(Sys.Date(), "%Y")) - 1)

  expect_equal(as.numeric(max(substr(ila$saksnr, 1, 4))), as.numeric(format(Sys.Date(), "%Y")) - 1)

})


test_that("errors for read_eos_data", {

  linewidth <- options("width")
  options(width = 80)

  expect_error(read_eos_data(eos_table = NA, from_path = tempdir()),
               regexp = "Variable 'eos_table': May not be NA")

  expect_error(read_eos_data(eos_table = "filename.csv", from_path = tempdir()),
               regexp = "File\n * does not exist:",
               fixed = TRUE)

  options(width = unlist(linewidth))
})
