library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
library(agdesign)
context("Test nutrient details ")

#----------- type = select  --------------------------------------------------------------



test_that("Test detail modal for 10 nutrient elements", {
  
  #setwd("~/AGROFIMS/agdesign")
  fname_table <- rprojroot::find_testthat_root_file("userInput", "test_table_nutrientDetails_2.rds")
  fname_input <- rprojroot::find_testthat_root_file("userInput", "test_input_nutrientDetails_2.rds")
  allinputs <- readRDS(fname_table)
  #input <- readRDS(fname_input)
  
  #design
  design <- "frcbd"
  #index <- c("1","2") #two factors
  index <- "2"
  #index level
  indexEspLvl <- c("frcbd_lvl_espType_1_1", "frcbd_lvl_espType_2_1","frcbd_lvl_espType_2_2","frcbd_lvl_espType_2_3")
  res<- get_nutrient_details(allinputs=allinputs, design=design, index =2, indexEspLvl = indexEspLvl)
  
  testthat::expect_equal(nrow(res),10)
  testthat::expect_equal(ncol(res),6)
  
  
})     