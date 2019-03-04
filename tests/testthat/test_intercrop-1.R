library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
context("test exp.conditions harvest in intercrop ")

#----------- type = select  --------------------------------------------------------------

test_that("Harvest table for intercrop trials", {

fname <- rprojroot::find_testthat_root_file("userInput", "intercrop-table_ids.rds")
allinputs<- readRDS(fname)
  # Harvest -----------------------------------------------------------------
#allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/intecrop-table_ids.rds")

crop <- c("crop1", "crop2")
addId <- c("NGBAKSHG", "VIITIDBD")
dt<- get_ec_harv_inter(allinputs, addId, crop)

ncrop_list<- length(dt)
expect_equal(ncrop_list, 2)
expect_equal(ncol(dt$crop1), 9)
expect_equal(ncol(dt$crop2), 9)
})

