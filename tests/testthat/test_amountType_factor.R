library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
library(agdesign)
context("test type and amount factors ")

#----------- type = select  --------------------------------------------------------------

test_that("Test type and amount factor when user delete one level (2 active 1 removed)", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "test_amtype-1.rds")
  allinputs <- readRDS(fname)
  index <- "1"
  design <- "fcrd"
  indexEspLvl <- c("frcbd_lvl_espType_1_1", "frcbd_lvl_espType_1_2" ,"frcbd_lvl_espType_1_3", "crd_lvl_espType_1_1",
                   "fcrd_lvl_espType_1_1" , "fcrd_lvl_espType_1_3" )
  out<- get_amountype_levels(allinputs, index=index, indexEspLvl=indexEspLvl, factors="Nutrient element type and amount", design=design, 
                             data_dictionary=NULL)   
  
  testthat::expect_equal(object = length(out), expected = 6)
})



