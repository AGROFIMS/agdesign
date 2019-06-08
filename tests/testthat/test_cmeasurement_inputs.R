library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
library(agdesign)
context("test crop measurement inputs")


test_that("test crop measurement for monocrop trials", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "cmea_mono_test_inputs.rds")
  allinputs <- readRDS(fname)
  
  crops<- c("Cassava")
  cropId<- c("1") #Common bean
  addId <- c("1","2")
  ctype <- "monocrop"
  dt_measurements <- get_dtcmea_variables(allinputs, ctype="monocrop", addId=addId, crop=cropId, cropId= cropId[1])
  
  #row filtered
  testthat::expect_equal(nrow(dt_measurements), 2)
  
})


test_that("test crop measurement for intercrop trial", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "measurement_test_inputs.rds")
  allinputs <- readRDS(fname)
  
  crops<- c("Common bean","Potato")
  cropId<- c("1") #Common bean
  ctype <- "intercrop"
  out <- get_dtcmea_variables(allinputs, ctype="intercrop", addId="1", cropId= cropId[1])
  
  testthat::expect_equal(nrow(out), 1)
  
})
test_that("test intercrop 1 row selected first crop and 0 rows selected in the second crop", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "multicrop_extremecase1.rds")
  allinputs <- readRDS(fname)
  
  crops<- c("Cassava","Potato")
  cropId<- c("1","2") #Common bean
  ctype <- "intercrop"
  addId<- list( "1", character(0))
  out1 <- get_dtcmea_variables(allinputs, ctype="intercrop", addId= addId[[1]], cropId= cropId[1])
  out2 <- get_dtcmea_variables(allinputs, ctype="intercrop", addId= addId[[2]], cropId= cropId[2])
  
  testthat::expect_equal(nrow(out1), 1)
  testthat::expect_equal(nrow(out2), 0)
})



test_that("test crop measurement tables for relay crop trial", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "cmea_relay_inputs.rds")
  allinputs <- readRDS(fname)
  
  crops<- c("Cassava","Maize")
  cropId<- c("1","2") #Common bean
  ctype <- "relay crop"
  out1 <- get_dtcmea_variables(allinputs, ctype="relay crop", addId="1", cropId= cropId[1])
  out2 <- get_dtcmea_variables(allinputs, ctype="relay crop", addId="1", cropId= cropId[2])
  testthat::expect_equal(nrow(out1), 1)
  testthat::expect_equal(nrow(out2), 1)
})










test_that("test existence of measurement IDS for monocrop trials when user select values", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "cmea_mono_test_inputs.rds")
  allinputs <- readRDS(fname)
  
  crop<- c("Cassava")
  cropId<- c("1") #Common bean
  addId <- c("1","2")
  ctype <- "monocrop"
  dt_measurements <- get_dtcmea_variables(allinputs, ctype="monocrop", addId=addId, crop=crop, cropId= cropId)
  
  #Check if some measurements exist in the master crop measurement table 
  dt_cmea<- agdesign::dt_cmea
  dt_cmea<- dplyr::select(dt_cmea,-contains("Numberof"))
  
  join_cmea <- dplyr::left_join(dt_measurements, dt_cmea)
  
  #row filtered
  testthat::expect_false(anyNA(join_cmea$VariableId))
  
})

