library(NVIdb)
library(testthat)

test_that("cut_slash", {
expect_equal(cut_slash("C:/temp/"), "C:/temp") 

expect_equal(cut_slash("C:\\temp\\"), "C:\\temp")

expect_equal(cut_slash(c("C:/temp/", "C:\\temp\\")), c("C:/temp", "C:\\temp"))

expect_equal(cut_slash(list("C:/temp/", "C:\\temp\\")), c("C:/temp", "C:\\temp"))
})

