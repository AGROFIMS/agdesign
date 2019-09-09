library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
library(agdesign)
library(rowr)
context("Test nutrient details")

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
  res<- get_nutrient_details_design(allinputs=allinputs, design=design, index =2, indexEspLvl = indexEspLvl)
  
  testthat::expect_equal(nrow(res),10)
  testthat::expect_equal(ncol(res),6)
  
  
})     


test_that("Test detail 3 nutrient elements for 1 factor (modal)", {

  fname_table <- rprojroot::find_testthat_root_file("userInput", "test_table_nutrientDetails_3.rds")
  # #fname_input <- rprojroot::find_testthat_root_file("userInput", "test_input_nutrientDetails_2.rds")
  allinputs <- readRDS(fname_table)
  indexEspLvl <- c("frcbd_lvl_espType_1_1", "frcbd_lvl_espType_1_2","frcbd_lvl_espType_1_3")
  design <- "frcbd"
  index <- "1"

  out <- product_calculation(allinputs, index , indexEspLvl , design)
  
  testthat::expect_equal(nrow(out),3)
  testthat::expect_equal(ncol(out),2)
  
})


test_that("Test detail for reshape nutrient table",{
  
  nutrient_elements <- tibble::tibble(A = c("Nitrogen", "Phosphorus", "Potassium") , B= c("50","20","10"))
  treatments <- nutrient_reshape(nutrient_elements, "A", "B")
  testthat::expect_equal(ncol(treatments),3)
  testthat::expect_true(is.numeric(treatments[1,1]))
  testthat::expect_true(is.numeric(treatments[1,2]))
  testthat::expect_true(is.numeric(treatments[1,3]))
  
})


test_that("Test nutrient details with number of splits >1",{
  
  fname_table <- rprojroot::find_testthat_root_file("userInput", "nutrient_split_2.rds")
  fname_dfall <-  rprojroot::find_testthat_root_file("userInput", "dfAll_nutrient_split_2.rds")
  allinputs <- readRDS(fname_table)
  dfAll <- readRDS(fname_dfall)
  design<- "frcbd"
  indexEspLvl<- c("frcbd_lvl_espType_1_1","frcbd_lvl_espType_1_2")
  index <- "1"  
  out <- get_nutrient_details_design_test(allinputs, "frcbd", index, indexEspLvl)
  
  product_calculation_test(allinputs, dfAll, index="1", indexEspLvl= indexEspLvl , design="frcbd")
  
  testthat::expect_equal(nrow(out),8) 
  testthat::expect_equal(ncol(out),6) 
  
  #-----------------------------------------------------------------------------------
  
})


test_that("Test nutrient details with number of splits >2",{
  
  #CHANGE DATASET FOR TESTING
  
  fname_table <- rprojroot::find_testthat_root_file("userInput", "nutrient_split_2.rds")
  fname_dfall <-  rprojroot::find_testthat_root_file("userInput", "dfAll_nutrient_split_2.rds")
  allinputs <- readRDS(fname_table)
  dfAll <- readRDS(fname_dfall)
  design<- "frcbd"
  indexEspLvl<- c("frcbd_lvl_espType_1_1","frcbd_lvl_espType_1_2")
  index <- "1"  
  #out <- get_nutrient_details_design_test(allinputs, "frcbd", index, indexEspLvl)
  
  out <- product_calculation_test(allinputs, dfAll, index="1", indexEspLvl= indexEspLvl , design="frcbd")
  testthat::expect_equal(nrow(out),10) 
  #testthat::expect_equal(ncol(out),6) 
  
})
