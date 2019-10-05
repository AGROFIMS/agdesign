library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
library(agdesign)
context("test for calculating nutrient Management practices")


test_that("test calculations for product rate in Nutrient-Management-Practices", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "calc_prod_mgmt.rds")
  allinputs <- readRDS(fname)
  source("R/ec_soilfertility.R")
  out <- get_nutrient_mgmt(allinputs= allinputs, "mgp_nut_1")
  
  
})


test_that("test calculations for nutrient rate in Product-Management-Practices", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "calc_nut_mgmt.rds")
  allinputs <- readRDS(fname)
  source("R/ec_soilfertility.R")
  splitId <- "mgp_proidx_1_1"
  addId <- "mgp_pro_1"
  out <- get_prodfert_mgmt(allinputs, addId = addId, splitId= splitId)
  #out <- get_nutrient_mgmt(allinputs= allinputs, "mgp_nut_1")
  
  
})