library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
context("test exp.conditions intercrop planting transplanting ")

#----------- type = select  --------------------------------------------------------------

test_that("INTECROP: Capture inputs from planting and transplting boxes per crop", {
  
  #fname <- rprojroot::find_testthat_root_file("userInput", "intecrop-platra-table-1.rds")
  allinputs<-readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/intercrop-platra-table-1.rds")
  
  addId<-c("FWVOHPFF","HKWVUQWW")
  crop<- c("Cassava", "Common bean")
  dt_tp<- get_ec_plantrans_inter(allinputs, addId, crop)
  
  nc <- ncol(dt_tp)
  nr <- nrow(dt_tp)
  expect_equal(nc, 39)
  expect_equal(nr, 1)
  
})