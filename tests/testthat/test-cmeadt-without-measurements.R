library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
library(agdesign)
context("test Trials without selecting measurements/variables")

test_that("test Relay Crop Trials without any selected measurement ", {
  
  #fname <- rprojroot::find_testthat_root_file("userInput", "table_ids.rds")
  fname <- rprojroot::find_testthat_root_file("userInput", "cmea_mono_without_variables.rds")
  allinputs <- readRDS(fname)
  
  crops<- c("Cassava")
  cropId<- c("1") #Common bean
  ctype <- "monocrop"
  out <- get_dtcmea_variables(allinputs, ctype="intercrop", addId="1", cropId= cropId[1])
  
  testthat::expect_equal(nrow(out),0)
  
})

test_that("test  Relay crop without any selected measurement ", {

  #fname <- rprojroot::find_testthat_root_file("userInput", "table_ids.rds")
  fname <- rprojroot::find_testthat_root_file("userInput", "cmea_relay_without_variables.rds")
  allinputs <- readRDS(fname)

  crops<- c("Cassava","Common bean")
  cropId<- c("1")
  ctype <- "relay crop"
  out <- get_dtcmea_variables(allinputs, ctype="relay crop", addId="1", cropId= cropId[1])

  testthat::expect_equal(nrow(out),0)

})