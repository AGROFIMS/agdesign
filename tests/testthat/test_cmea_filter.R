library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
library(agdesign)
context("test to filter values from crop measurement")


#----------- type = select  --------------------------------------------------------------
# 
test_that("Test for filtering units depeding on Crop and Measurement", {

  dt<- get_dcm_values(data_dictionary=dt_cmea, attribute = "TraitUnit", crop="Rice",measurement = "Moisture content")
  testthat::expect_equal(dt[1,],"%")
#mono_mea_1_search
})


