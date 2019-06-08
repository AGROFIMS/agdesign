library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
library(agdesign)
context("test get labels for residue management")

#----------- type = select  --------------------------------------------------------------

test_that("Test to get labels for RESIDUE management and filter them in experiment conditions", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "ec-label-inputs.rds")
  fec <- rprojroot::find_testthat_root_file("excel", "ExpConditions-05-3-2019.xlsx")
  
  # Harvest -----------------------------------------------------------------
  input <- readRDS(fname)
  ec<- readxl::read_excel(fec,sheet = "Residue management")
  
  lbl_resdesc <- get_label_resdesc(input)
  lbl_resmgt <- get_label_resmgt(input)  
    
  dt1 <- ec %>% dplyr::filter(Fieldbook_download %in% lbl_resdesc)
  dt2 <- ec %>% dplyr::filter(Fieldbook_download %in% lbl_resmgt)
  dt<-rbind(dt1,dt2)
  
  lbl<-c(lbl_resdesc,lbl_resmgt)
  
  testthat::expect_equal(nrow(dt1), length(lbl_resdesc))
  testthat::expect_equal(nrow(dt2), length(lbl_resmgt))
  testthat::expect_equal(nrow(dt), length(lbl))
  
})



test_that("Test to get labels for SEEDBED and filter then in experiment conditions", {
  
  fname <- rprojroot::find_testthat_root_file("userInput", "ec-labelinputs.rds")
  fec <- rprojroot::find_testthat_root_file("excel", "ExpConditions-05-3-2019.xlsx")
  
  # Harvest -----------------------------------------------------------------
  input <- readRDS(fname)
  ec<- readxl::read_excel(fec,sheet = "Seedbed preparation")
  
  lbl_lavl <- get_label_sblalv(input)
  lbl_pud <- get_label_sbpud(input)
  lbl <- get_ec_sbtill(input)
  
  dt1 <- ec %>% dplyr::filter(Fieldbook_download %in% lbl_resdesc)
  dt2 <- ec %>% dplyr::filter(Fieldbook_download %in% lbl_resmgt)
  dt<-rbind(dt1,dt2)
  
  lbl<-c(lbl_resdesc,lbl_resmgt)
  
  testthat::expect_equal(nrow(dt1), length(lbl_resdesc))
  testthat::expect_equal(nrow(dt2), length(lbl_resmgt))
  testthat::expect_equal(nrow(dt), length(lbl))
  
})



