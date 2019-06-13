library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
library(agdesign)
context("test for collect inputs in traitlist")


test_that("test collectable inputs for residue management", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "mono_collect_field.rds")
  allinputs <- readRDS(fname)
  
  get_collectable_resmgt(allinputs)
  
  
  
  
})