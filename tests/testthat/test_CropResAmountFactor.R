# library(dplyr)
# library(stringr)
# library(magrittr)
# library(testthat)
# context("test factor for crop residue amount ")
# 
# #----------- type = select  --------------------------------------------------------------
# 
# test_that("T", {
#   
#   fname <- rprojroot::find_testthat_root_file("userInput", "crop_residue_factor.rds")
#   allinputs<- readRDS(fname)
#   
#   #a2 <- get_levels_design(allinputs, "1", factors, design="frcbd",data_dictionary=NULL,format="list")
#   #a1 <- get_levels_design(allinputs, "2", factors="Crop residue amount", 
#   #                        design="frcbd",data_dictionary= agdesign::dt_factordesign)
#   
#   index="1" ; factors="Crop residue amount" ;
#   design="frcbd"; data_dictionary=agdesign::dt_factordesign; crop <- ""
#   
#   a1 <- get_levels_design(allinputs, "1", factors="Crop residue amount", 
#                                         design="frcbd",data_dictionary= agdesign::dt_factordesign)
#   
#   
#   #a2<- get_amountype_levels(allinputs, index="1", factors="Crop residue amount",
#   #                                      design="frcbd", data_dictionary=agdesign::dt_factordesign)
#   
#   nr <- length(a1)
#   expect_equal(6, 6)
#   
# })