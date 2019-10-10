library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
library(agdesign)
library(rowr)
context("Test nutrient details")

#----------- type = select  --------------------------------------------------------------

test_that("Test detail modal for 10 nutrient elements", {
  
  fname_table <- rprojroot::find_testthat_root_file("userInput", "nut_details_mgmt_1.rds")
  nut_details_mgmt <- readRDS(fname_table)
  
  details <- nut_details_mgmt$nut_details
  treatment <- nut_details_mgmt$treatment
  fertilizer <- nut_details_mgmt$fertilizer
  
  
  
  
})