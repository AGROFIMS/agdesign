library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
context("test exp.conditions harvest ")

#----------- type = select  --------------------------------------------------------------

test_that("Capture inputs from Multiple harvest boxes", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "crop-_residue_factor.rds")
  allinputs<- readRDS(fname)
  
  
  
  nr <- nrow(dt_harv)
  expect_equal(nc, 39)
  
  
})