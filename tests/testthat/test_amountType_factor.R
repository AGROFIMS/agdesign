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
  
  testthat::expect_equal(lvl1, c("Bark / Wood chips_a1g/m2", "Bark / Wood chips_a2g/m2",
                                "Bark / Wood chips_a3g/m2", "Compost_b1g/m2"          ,
                                "Compost_b2g/m2","Compost_b3g/m2")        
                         )
  testthat::expect_equal(lvl2, c("Maize_Alfalfa meal_k1g/m2", "Maize_Alfalfa meal_k2g/m2",
                                 "Maize_Alfalfa meal_k3g/m2"))
  
  
  testthat::expect_equal(out[[1]], lvl1)
  testthat::expect_equal(length(out[[1]]), 6)
  testthat::expect_equal(out[[2]], lvl2)
  testthat::expect_equal(length(out[[2]]), 3)
})



test_that("Test for factor named Crop residue amount", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "crop_residue_factor.rds")
  allinputs<- readRDS(fname)
  
  #a2 <- get_levels_design(allinputs, "1", factors, design="frcbd",data_dictionary=NULL,format="list")
  #a1 <- get_levels_design(allinputs, "2", factors="Crop residue amount", 
  #                        design="frcbd",data_dictionary= agdesign::dt_factordesign)
  
  factors <- c("Crop residue amount","Intercrop arrangement")
  index <- c("1","2")
  
  #index="1" ; factors="Crop residue amount" ;
  design="frcbd"; data_dictionary=agdesign::dt_factordesign; crop <- ""
  
  # lvl <- get_levels_design(allinputs, "1", factors="Crop residue amount", 
  #                         design="frcbd",data_dictionary= agdesign::dt_factordesign)
  lvl <- get_levels_design(allinputs, index, factors=factors,
                          design="frcbd",data_dictionary= agdesign::dt_factordesign)
  
  nr <- length(lvl)
  nlvl_cramount <- length(lvl[[1]])
  expect_equal(nr, 2)
  expect_equal(nlvl_cramount, 6)
})

test_that("UI test when user remove element from the first levels - first evaluation- in the second factor", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "nutrient_removelevel_test.rds")
  allinputs<- readRDS(fname)
  
  allinputs[allinputs$id=="crd_lvl_2_1",2] <- "12"
  
  
  factors <- c("Nutrient element type and amount","Nutrient element type and amount")
  index <- c("1","2")
  
  data_dictionary <- agdesign::dtfactordesign
  
  lvl<-NULL
  
  #for(i in 2){
  i <- 2
  lvl <- get_amountype_levels(allinputs, index=index[i], factors[i], design="crd", 
                                data_dictionary=data_dictionary)
  #}
  
  testthat::expect_equal(length(lvl),3)
  
  
})


