library(NVIdb)
library(testthat)


test_that("Correct merging of produsent og produsent properties", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))
  
  # From add_produsent_help
  #CURRENT PRODNR8
  prodnr_2_gjeldende_prodnr <- read_prodnr_2_current_prodnr()

  prodnr8 <- c("09140087", "14260856", "17020818", "50060129")
  produsenter <- as.data.frame(prodnr8)

  # Make a dataframe with the correct result
  correct_result <- cbind(as.data.frame(c("42130063", "46440597", "50060129", "50060129"), stringsAsFactors = FALSE),
                          produsenter)
  colnames(correct_result) <- c("gjeldende_prodnr8", "prodnr8")


  # Compare Add fylke, current fylkenr and current fylke with correct result
  produsenter <- NVIdb::add_produsent(data = produsenter,
                               translation_table = prodnr_2_gjeldende_prodnr,
                               code_column = "prodnr8",
                               new_column = "gjeldende_prodnr8",
                               position = "left",
                               overwrite = FALSE)
  expect_identical(produsenter, correct_result)
  
  # COORDINATES
  # Reading from standard directory at NVI's network
  prodnr_2_koordinater <- read_prodnr_2_coordinates()
  
  # Make a dataframe with the correct result
  correct_result <- cbind(produsenter,
                          as.data.frame(c("8.78182485813634", "7.04026092117579", "11.64981213930787", "11.64981213930787"), stringsAsFactors = FALSE),
                          as.data.frame(c("58.63237920843709", "61.47787583826138", "64.13066637524832", "64.13066637524832"), stringsAsFactors = FALSE)
  )
  colnames(correct_result) <- c("gjeldende_prodnr8", "prodnr8", "longitude", "latitude")
  
  produsenter <- add_produsent(data = produsenter,
                               translation_table = prodnr_2_koordinater,
                               code_column = c("gjeldende_prodnr8" = "prodnr8"),
                               new_column = c("longitude" = "geo_eu89_o", "latitude" = "geo_eu89_n"),
                               position = "last")
  expect_identical(produsenter, correct_result)
})
