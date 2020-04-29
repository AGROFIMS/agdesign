# library(dplyr)
# library(stringr)
# library(magrittr)
# library(testthat)
# library(agdesign)
# context("test for getting day after planting values in CM")
# 
# test_that("Get 'days afte planting' values for monocrop", {
#   
#   #fname <- rprojroot::find_testthat_root_file("userInput", "table_ids.rds")
#   fname <- rprojroot::find_testthat_root_file("userInput", "test_mono_dap_inputs.rds")
#   allinputs <- readRDS(fname)
#   lookup <- "mono"
#   cropId <- "1" 
#   addId <- c("1","2","3")
#   
#   #timValue <- allinputs %>% dplyr::filter(str_detect(id,  "_timingValue_"))        
#   timValue <- NULL
#   for(i in 1:length(addId)){
#     timValue[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"_timingValue_",addId[i],"_1","$") )) %>% dplyr::nth(2)
#   }
#   
#   dt <- get_dtcmea_variables(allinputs=allinputs, ctype="monocrop",addId=addId, crop="crop", cropId= "1")
#   
#   
#   testthat::expect_equal(length(timValue),3)
#   
#   #paste0("^",lookup, cropId,"_timingValue_",addId[i],"_1","$") 
# 
# })