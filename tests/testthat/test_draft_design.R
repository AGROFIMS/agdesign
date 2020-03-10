library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
context("test draft design ")

#----------- type = select  --------------------------------------------------------------

test_that("Test for correct capture of non-units factors", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "table_draft_design#1.rds")
  #fname <- rprojroot::find_testthat_root_file("userInput", "table_ids.rds")
  # FRCBD -----------------------------------------------------------------
  #source("R/experimental_design.R")
  #source("R/utils.R")
  allinputs<- readRDS(fname)
  design <- "frcbd"
  index <- c("1","2")
  factors <- get_factors_design(allinputs = allinputs,index = index, design, TRUE)
  flvl <- get_levels_design(allinputs, data_dictionary=dtfactordesign, index = index,
                            factors=factors, design="frcbd", format="list")
  
  testthat::expect_equal(length(factors), 2)
  testthat::expect_equal(length(flvl), 2)
})

