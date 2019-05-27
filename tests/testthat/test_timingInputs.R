library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
library(agdesign)
context("test exp.conditions harvest ")

#----------- type = select  --------------------------------------------------------------

test_that("Test timing factor (special factor)", {
  
  #source("R/experimental_design.R")
  fname <- rprojroot::find_testthat_root_file("userInput", "crd_Timingfactors.rds")
  allinputs <- readRDS(fname)
  
  factors <- c("Weeding timing","Irrigation timing","Fertilizer timing","Abiotic stress timing",
               "Biotic stress control timing")
  index <- c("1","2","3","4","5")
  
  data_dictionary <- agdesign::dtfactordesign
  lvl1 <- get_timing_levels(allinputs, index="1", factors[1], design="crd",case="timing", 
                           data_dictionary=data_dictionary, format="list")
  
  lvl2 <- get_timing_levels(allinputs, index="2", factors[2], design="crd",case="timing", 
                             data_dictionary=data_dictionary, format="list")
  
  lvl3 <- get_timing_levels(allinputs, index="3", factors[3], design="crd",case="timing", 
                             data_dictionary=data_dictionary, format="list")
  
  lvl4 <- get_timing_levels(allinputs, index="4", factors[4], design="crd",case="timing", 
                             data_dictionary=data_dictionary, format="list")
  
  lvl5 <- get_timing_levels(allinputs, index="5", factors[5], design="crd",case="timing", 
                             data_dictionary=data_dictionary, format="list")
  
  
  testthat::expect_equal(lvl1, c("g1", "g2", "g3"))
  
  testthat::expect_equal(lvl2, c("day1", "day2", "day3"))
  
  testthat::expect_equal(lvl3, c("fq1"))
  
  testthat::expect_equal(lvl4, c("2019-05-23", "2019-05-23" ,"2019-05-23"))
  
  testthat::expect_equal(lvl5, c("oth1","oth2"))
})