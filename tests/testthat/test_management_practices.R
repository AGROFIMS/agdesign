library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
library(agdesign)
context("Management practices")

#----------- type = select  --------------------------------------------------------------

test_that("Residue description and management test", {
  
  #source("/home/obenites/AGROFIMS/agdesign/R/ec_resmgt.R")
  input <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/test_ec_resmgmt_1.rds")
  #Incorporate residue
  input2 <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/test_ec_resmgmt_2.rds")
  
  out1 <- get_ec_resdesc(input)$dt #description 
  out2 <- get_ec_resmgt(input)$dt #management
  out3 <- get_ec_resmgt(input2)$dt #management with incorporation
  
  testthat::expect_equal(ncol(out1), 6)
  testthat::expect_equal(ncol(out2), 4)
  testthat::expect_equal(ncol(out3), 5)
})

test_that("Seedbed preparation test ", {
  
  input <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/test_ec_seedbed_1.rds")
  
  out1 <- get_ec_sblalv(input)$dt
  out2 <- get_ec_sbpud(input)$dt
  out3 <- get_ec_sbtill(input)$dt
    
  #source("/home/obenites/AGROFIMS/agdesign/R/ec_resmgt.R")
  #input <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/test_ec_resmgmt_1.rds")
  # out1 <- get_ec_resdesc(input)$dt #description 
  # out2 <- get_ec_resmgt(input)$dt #management
  # 
   testthat::expect_equal(ncol(out1), 5)
   testthat::expect_equal(ncol(out2), 6)
   testthat::expect_equal(ncol(out3), 7)

   
  
})

test_that("Mulching test", {
  
  #input <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/test_ec_mulch.rds")
  allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/test_dt_ec_mulch.rds")
  
  out<- get_ec_mulching(allinputs)$dt
  
  #source("/home/obenites/AGROFIMS/agdesign/R/ec_resmgt.R")
  #input <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/test_ec_resmgmt_1.rds")
  # out1 <- get_ec_resdesc(input)$dt #description 
  # out2 <- get_ec_resmgt(input)$dt #management
  # 
  testthat::expect_equal(ncol(out), 11)

})

test_that("Weeding test", {
  
  #input <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/test_ec_mulch.rds")
  allinputs1 <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/test_dt_ec_weeding.rds")
  allinputs2 <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/test_dt_ec_weeding_2.rds")
  
  #dt<- get_protocol_weed(allinputs2, addId="1", ctype="monocrop")
  
  out1<- get_ec_weed(allinputs1, addId="1", ctype="monocrop")$dt
  out2<- get_ec_weed(allinputs2, addId="1", ctype="monocrop")$dt
  
  testthat::expect_equal(ncol(out1), 9)
  testthat::expect_equal(ncol(out2), 9)
  
  
})