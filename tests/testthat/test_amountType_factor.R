library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
library(agdesign)
context("test type and amount factors ")

#----------- type = select  --------------------------------------------------------------

test_that("Test type and amount factor (special factor)", {
  
  #source("R/experimental_design.R")
  fname <- rprojroot::find_testthat_root_file("userInput", "crd_AmountTypefactors.rds")
  allinputs <- readRDS(fname)
  
  factors <- c("Mulch amount and type","Fertilizer type and amount")
  
  data_dictionary <- agdesign::dtfactordesign
  lvl1 <- get_amountype_levels(allinputs, index="1", factors[1], design="crd", 
                            data_dictionary=data_dictionary)
  
  lvl2 <- get_amountype_levels(allinputs, index="2", factors[2], design="crd", 
                            data_dictionary=data_dictionary)
  out<- NULL
  for(i in 1:length(factors)){
    
   out[[i]] <- get_amountype_levels(allinputs, index= i , factors[i], design="crd", 
                         data_dictionary=data_dictionary)
  }
  
  testthat::expect_equal(lvl1, c("Bark / Wood chips_a1_g/m2", "Bark / Wood chips_a2_g/m2",
                                "Bark / Wood chips_a3_g/m2", "Compost_b1_g/m2"          ,
                                "Compost_b2_g/m2","Compost_b3_g/m2")        
                         )
  testthat::expect_equal(lvl2, c("Maize_Alfalfa meal_k1_g/m2", "Maize_Alfalfa meal_k2_g/m2",
                                 "Maize_Alfalfa meal_k3_g/m2"))
  
  
  testthat::expect_equal(out[[1]], lvl1)
  testthat::expect_equal(length(out[[1]]), 6)
  testthat::expect_equal(out[[2]], lvl2)
  testthat::expect_equal(length(out[[2]]), 3)
})