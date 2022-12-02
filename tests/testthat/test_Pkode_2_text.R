library(NVIdb)
library(testthat)
library(checkmate)

# Assigns temporary dir to td
td <- tempdir()
if (!dir.exists(file.path(td, "data"))) {
  dir.create(file.path(td, "data"))
}

filenames <- "Produksjonstilskuddskoder2_UTF8.csv"
for (i in 1:length(filenames)) {
  if (file.exists(file.path(td, filenames[i]))) {
    file.remove(file.path(td, filenames[i]))
  }
}

test_that("Copy Produksjonstilskuddskoder", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # copy file
  copy_Pkode_2_text(to_path = td)
  expect_true(file.exists(file.path(td, filenames[1])))

  copy_Pkode_2_text(from_path = td, to_path = file.path(td, "data"))
  expect_true(file.exists(file.path(td, "data", filenames[1])))
})

test_that("Read Produksjonstilskuddskoder", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  # read file
  Pkoder <- read_Pkode_2_text(filename = "Produksjonstilskuddskoder2_UTF8.csv")

  expect_subset(x = c("soknadaar", "soknadmnd", "telledato", "Pkode",  "Pkodetype","Pkodeart", "beskrivelse",
                                "enhet", "unike_dyr", "sortering"),
                choices = colnames(Pkoder))

  Pkoder <- read_Pkode_2_text(keep_old_names = TRUE)

  expect_subset(x = c("Søknadsår", "Telledato", "Art", "Kode", "Beskrivelse",
                      "Enhet", "Seleksjon", "Sortering"),
                choices = colnames(Pkoder))
})

test_that("errors for copy_Pkode_2_text", {

    linewidth <- options("width")
    options(width = 80)

    expect_error(copy_Pkode_2_text(filename = NULL, from_path = tempdir(), to_path = "./"),
                 regexp = "(filename): Must be of type 'character', *\n * not 'NULL'",
                 fixed = TRUE)

    expect_error(copy_Pkode_2_text(filename = "filename.csv", from_path = tempdir(), to_path = "./"),
                 regexp = "File does not exist:")

    expect_error(copy_Pkode_2_text(filename = "filename.csv", from_path = tempdir(), to_path = "filepath_does_not_exist"),
                 regexp = "Directory 'filepath_does_not_exist' does not\n * exist.",
                 fixed = TRUE)

    options(width = unlist(linewidth))
  })

test_that("errors for read_Pkode_2_text", {

    linewidth <- options("width")
    options(width = 80)

    expect_error(read_Pkode_2_text(filename = NULL, from_path = tempdir()),
                 regexp = "(filename): Must be of type 'character', *\n * not 'NULL'",
                 fixed = TRUE)

    expect_error(read_Pkode_2_text(filename = "filename.csv", from_path = tempdir()),
                 regexp = "File does not exist:")

    options(width = unlist(linewidth))
  })
