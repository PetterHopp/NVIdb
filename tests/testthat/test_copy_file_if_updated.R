library(NVIdb)
library(testthat)

# Set up environment ----
# Assigns temporary dir to td
td <- tempdir()

if (!dir.exists(file.path (td, "test"))) {
  dir.create(file.path (td, "test")) 
} 
filenames <- "PJS_levels.csv"
write.csv2(NVIdb::PJS_levels, file = file.path(td, filenames), row.names = FALSE)

if (file.exists(file.path (td, "test", filenames[1]))) {
  file.remove(file.path (td, "test", filenames[1])) 
} 


test_that("copy_file_if_updated", {
  # copy file
  NVIdb:::copy_file_if_updated(filename = filenames[1], from_path = td, to_path =  file.path (td, "test"))
  expect_true(file.exists(file.path(td, "test", filenames[1])))
  
  write.csv2(NVIdb::PJS_levels, file = file.path(td, filenames), row.names = FALSE)
  
  NVIdb:::copy_file_if_updated(filename = filenames[1], from_path =  file.path (td, "test"), to_path = td)
  expect_true(file.exists(file.path(td, "test", filenames[1])))
  
  NVIdb:::copy_file_if_updated(filename = filenames[1], from_path = td, to_path =  file.path (td, "test"))
  expect_true(file.exists(file.path(td, "test", filenames[1])))
})

test_that("copy_file_if_updated with / or \\", {
  # copy file
  if (file.exists(file.path (td, "test", filenames[1]))) {
    file.remove(file.path (td, "test", filenames[1])) 
  } 
  NVIdb:::copy_file_if_updated(filename = filenames[1], 
                               from_path = paste0(td, "/"), 
                               to_path =  paste0(file.path (td, "test"), "/"))
  expect_true(file.exists(file.path(td, "test", filenames[1])))
  
  if (file.exists(file.path (td, "test", filenames[1]))) {
    file.remove(file.path (td, "test", filenames[1])) 
  } 
  NVIdb:::copy_file_if_updated(filename = filenames[1], 
                               from_path = paste0(td, "\\"), 
                               to_path =  paste0(file.path (td, "test"), "\\"))
  expect_true(file.exists(file.path(td, "test", filenames[1])))
  
})

