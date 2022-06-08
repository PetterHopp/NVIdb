library(NVIdb)
library(testthat)


test_that("Correct merging of lokalitet and sone", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # From add_produsent_help
  # CURRENT PRODNR8
  # Reading from the temporary directory
  sonetilhorighet <- read_sonetilhorighet()

  # ADD AQUACULTURE ZONE
  eier_lokalitetnr <- c("10298", "10318", "10735", "10814")
  lokaliteter <- as.data.frame(eier_lokalitetnr)

  # Make a dataframe with the correct result
  correct_result <- cbind(as.data.frame(c(3, 4, 11, 12), stringsAsFactors = FALSE),
                          lokaliteter)
  colnames(correct_result) <- c("produksjonsomraade", "eier_lokalitetnr")

  # Add new column with aquculture zone
  lokaliteter <- add_lokalitet(lokaliteter,
                               translation_table = sonetilhorighet,
                               code_column = c("eier_lokalitetnr" = "LokNr"),
                               new_column = c("produksjonsomraade" = "sone"),
                               position = "left")

  expect_equal(lokaliteter, correct_result)


  # ADD COORDINATES
  eier_lokalitetnr <- c("10298", "10318", "10735", "10814")
  lokaliteter <- as.data.frame(eier_lokalitetnr)

  # Make a dataframe with the correct result
  correct_result <- cbind(lokaliteter,
                          as.data.frame(c("5,244133012708", "5,06988301405342", "20,3290669999275", "23,2844499984564"), stringsAsFactors = FALSE),
                          as.data.frame(c("59,8665000008897", "61,758017000812", "69,6618500000011", "70,2409500000325"), stringsAsFactors = FALSE)
  )
  colnames(correct_result) <- c("eier_lokalitetnr", "longitude", "latitude")

  # Add new columns with longitude and lattitude
  lokaliteter <- add_lokalitet(lokaliteter,
                               translation_table = sonetilhorighet,
                               code_column = c("eier_lokalitetnr" = "LokNr"),
                               new_column = c("longitude" = "Longitude_WGS84",
                                              "latitude" = "Latitude_WGS84"))

  expect_identical(lokaliteter, correct_result)

})



test_that("errors for add_lokalitet", {

  linewidth <- options("width")
  options(width = 80)

  expect_error(add_lokalitet(data = "no_data",
                             translation_table = "sonetilhorighet",
                             code_column = c("eier_lokalitetnr" = "LokNr"),
                             new_column = c("produksjonsomraade" = "sone")),
               regexp = "Variable 'data': Must be of type 'data.frame', not 'character'.")


  expect_error(add_lokalitet(data = "no_data",
                             translation_table = "sonetilhorighet",
                             code_column = c("eier_lokalitetnr" = "LokNr"),
                             new_column = c("produksjonsomraade" = "sone"),
                             position = "ferst"),
               regexp = "Variable 'position': Must be a subset of")

  expect_error(add_lokalitet(data = "no_data",
                             translation_table = "sonetilhorighet",
                             code_column = c("eier_lokalitetnr" = "LokNr"),
                             new_column = c("produksjonsomraade" = "sone"),
                             overwrite = "TRUE"),
               regexp = "Variable 'overwrite': Must be of type 'logical', not 'character'.")

  options(width = unlist(linewidth))
})


test_that("errors for read_sonetilhorighet", {

  linewidth <- options("width")
  options(width = 80)

  expect_error(read_sonetilhorighet(filename = NA, from_path = tempdir()),
               regexp = "File does not exist:")

  expect_error(read_sonetilhorighet(filename = "filename.csv", from_path = tempdir()),
               regexp = "File does not exist:")

  options(width = unlist(linewidth))
})
