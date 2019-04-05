library(dplyr)
library(stringr)
library(magrittr)
library(agdesing)
library(testthat)
context("test get levels from design tab ")

#----------- type = select  --------------------------------------------------------------

test_that("number of elements of levels (as list))", {
  
fname <- rprojroot::find_testthat_root_file("userInput", "dt-get-levels-design.rds")
allinputs<- readRDS(fname)
## Get levels from design tab #############################################################
#allinputs<- readRDS("tests/testthat/userInput/table_ids.rds")
flbl<- get_factors_design(allinputs, "frcbd",duplicate = FALSE)
design <- "frcbd"
out<- get_levels_design2(allinputs = dt, factors = flbl, design = "frcbd", format= "list")
#test
n<-length(out)
expect_equal(n, 2)

})


test_that("test for adding levels with units", {
  fname <- rprojroot::find_testthat_root_file("userInput", "tbl_levels-with-units.rds")
  allinputs<- readRDS(fname)
  design <- "frcbd"
  flbl<- get_factors_design(allinputs, "frcbd")
  out<- get_levels_design2(allinputs = allinputs, factors = flbl, design = "frcbd", format= "list")
  n<-length(out)
  expect_equal(n, 4)
})  