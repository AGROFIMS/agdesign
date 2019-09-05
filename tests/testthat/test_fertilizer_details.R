library(testthat)
library(agdesign)
context("Test fertilizer details ")

#----------- type = select  --------------------------------------------------------------



test_that("Test fertilizer details", {
  
  #setwd("~/AGROFIMS/agdesign")
  fname_table <- rprojroot::find_testthat_root_file("userInput", "fertilizer_details_1.rds")
  # fname_input <- rprojroot::find_testthat_root_file("userInput", "test_input_nutrientDetails_2.rds")
  allinputs <- readRDS(fname_table)
  #input <- readRDS(fname_input)
  source("R/utils.R")
  source("R/ec_soilfertility.R")
  source("R/experimental_design.R")
  
  #allinputs <- allinputs
  index <- "1"
  design <- "frcbd"
  
  indexEspLvl  <- c("frcbd_lvl_espType_1_1")
  #get_fertilizer_details_design(allinputs, "frcbd", "1", indexEspLvl)
  out <- get_fertilizer_details_design(allinputs, design, index="1", indexEspLvl)
  
  
  
})