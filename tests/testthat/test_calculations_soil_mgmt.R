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
  testthat::expect_equal(nrow(out$outrate), 1)
  
})

test_that("test calculations for two product rate per Split in Nutrient-Management-Practices ", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "calc_prod_mgmt_2.rds")
  allinputs <- readRDS(fname)
  addId <- c("mgp_nut_1","mgp_nut_2")
  source("R/ec_soilfertility.R")
  out <- get_nutrient_mgmt(allinputs= allinputs, addId)
  # out$outrate
  testthat::expect_equal(nrow(out$outrate), 3)
  testthat::expect_equal(nrow(out$fertilizers), 3)
  testthat::expect_equal(nrow(out$treatments), 2)
})


test_that("test calculations for nutrient rate in Product-Management-Practices", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "calc_nut_mgmt.rds")
  allinputs <- readRDS(fname)
  source("R/ec_soilfertility.R")
  splitId <- "mgp_proidx_1_1"
  addId <- "mgp_pro_1"
  out <- get_prodfert_mgmt(allinputs, addId = addId, splitId= splitId)
  #out <- get_nutrient_mgmt(allinputs= allinputs, "mgp_nut_1")
  outcalc  <- NutrientRates_mgmt(out$prodfert_mgmt ,out$treatment_mgmt) 
  
  testthat::expect_equal(nrow(outcalc), 1)
})