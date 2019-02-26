library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
context("test exp.conditions harvest ")

#----------- type = select  --------------------------------------------------------------

test_that("Capture inputs from Multiple harvest boxes", {

fname <- rprojroot::find_testthat_root_file("userInput", "harvest-tableIds-01.rds")

# Harvest -----------------------------------------------------------------
allinputs<- readRDS(fname)
harv <- allinputs %>%  
  filter(!str_detect(id, "button")) %>%
  filter(!str_detect(id, "-selectized")) %>%
  filter(str_detect(id,"hahd"))

addId<- str_extract_all(harv$id, "[:uppercase:]{8}") %>% unlist() %>% unique()
dt_harv<- get_ec_harv(allinputs, addId) 


nc <- ncol(dt_harv)
nr <- nrow(dt_harv)
expect_equal(nc, 39)
expect_equal(nr, 1)
})

