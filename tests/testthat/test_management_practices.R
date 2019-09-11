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

test_that("Residue description and management test #2", {
  
  #source("/home/obenites/AGROFIMS/agdesign/R/ec_resmgt.R")
  input <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/inputs.rds")
  #Incorporate residue
  allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/table_ids.rds")
  
  dt1 <- get_ec_resdesc(input)$dt #description 
  dt2 <- get_ec_resmgt(input)$dt #management
  lbl1 <- get_ec_resdesc(input)$lbl
  lbl2 <- get_ec_resmgt(input)$lbl
  lbl<- c(lbl1,lbl2)
  #out3 <- get_ec_resmgt(input2)$dt #management with incorporation
  dt <- smart_colbind(dt1,dt2)
  
  kds_resmgt<- magmtprac$resmgt
  kds_resmgt <- kds_resmgt %>% dplyr::filter(TraitName %in% lbl)
  
  ##Evaluation Reactive expressions
  collect_resmgt <- get_collectable_resmgt(allinputs)
  kds_resmgt <- magmtprac$resmgt %>% dplyr::mutate(temp = paste0(Subgroup,"_",Measurement))
  kds_resmgt <- kds_resmgt %>% dplyr::filter(temp %in% collect_resmgt) 
  #detect headers based on collectable trait
  mpra_trait <- kds_resmgt$TraitName[!is.na(kds_resmgt$TraitName)]
  index <- which(stringr::str_detect(names(dt), paste(mpra_trait, collapse = '|'))==TRUE)
  dt <- dt[index]
  
  testthat::expect_equal(length(lbl), 11) #NUMBER OF LABELS
  testthat::expect_equal(ncol(dt), 11) #NUMBER OF COLUMN IN DF

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



test_that("Harvest test", {

  input <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/test_input_ec_mgp_1.rds")
  #Incorporate residue
  allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/test_ec_mgp_1.rds")
  
  dt <- get_ec_harv(allinputs, input, ctype="monocrop", cropId="1", addId="1")$dt
  #############SECOND CASE #######################################################################
  
  mpra_trait <- stringr::str_replace_all(names(dt) , pattern = "__[0-9]+", replacement = "")
  
  collect_harv<- c("Start date" ,                 
                   "Harvest method",    
                   "Crop component harvested"   , 
                   "Harvest cut height"       ,   
                   "Amount harvested"     ,       
                   "Notes"             ,          
                   "Traction"       ,             
                   "Type" ,                       
                   "Number of m2 units harvested",
                   "Number of plants harvested"  ,
                   "Number of rows harvested"    ,
                   "Length of rows harvested"    ,
                   "Width within rows harvested" ,
                   "Space between rows harvested",
                   "Plot area harvested"  )       
  #collect_harv <- get_collectable_harvest(allinputs, ctype="monocrop",ver="export")
  #---Filter collectable trait in Mgm. Practices (Harves sheet)
  kds_harv <- magmtprac$harv %>% dplyr::filter(Measurement %in% collect_harv)
  ##---inputs derived from harvestable area combo
  kds_harv_harea <- magmtprac$harv %>% dplyr::filter(Measurement_3 %in% collect_harv)
  kds_harv <- rbind(kds_harv, kds_harv_harea)
  ##---Look mpra_trait into kds_harv
  kds_harv <- kds_harv %>% dplyr::filter(TraitName %in% mpra_trait )
  #----Detect headers
  lgl<- grepl(pattern = paste0(kds_harv$TraitName, collapse="|"),x = names(dt))
  dt <- dt[which(lgl==TRUE)]
  
  #############SECOND CASE #######################################################################
  dt2 <- get_ec_harv(allinputs, input, ctype="monocrop", cropId="1", addId="1")$dt
  #collect_harv2 <- "Number of m2 units harvested"
  mpra_trait2 <- stringr::str_replace_all(names(dt) , pattern = "__[0-9]+", replacement = "")
  collect_harv2 <- "Number of m2 units harvested"
  #---Filter collectable trait in Mgm. Practices (Harves sheet)
  kds_harv2 <- magmtprac$harv %>% dplyr::filter(Measurement %in% collect_harv2)
  ##---inputs derived from harvestable area combo
  kds_harv_harea2 <- magmtprac$harv %>% dplyr::filter(Measurement_3 %in% collect_harv2)
  kds_harv2 <- rbind(kds_harv2, kds_harv_harea2)
  ##---Look mpra_trait into kds_harv
  kds_harv2 <- kds_harv2 %>% dplyr::filter(TraitName %in% mpra_trait2 )
  #----Detect headers
  lgl2<- grepl(pattern = paste0(kds_harv2$TraitName, collapse="|"),x = names(dt2))
  dt2 <- dt[which(lgl2==TRUE)]
  
  testthat::expect_condition(nrow(dt,15))
  testthat::expect_condition(nrow(dt2,1))
  
})


test_that("Planting transplanting",{
  
  source("R/utils.R")
  source("R/ec_plantrans.R")
  allinputs<- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/table_ids.rds")
  input <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/inputs.rds")
  
  dt<- get_ec_plantrans(allinputs, input, ctype="monocrop", cropId= "1", addId="1")$dt
  collect_platra <- get_collectable_plantrans(allinputs,ctype="monocrop",ver = "export")
  
  collect_platra <- stringr::str_replace_all(tolower(collect_platra), pattern = "_", replacement = " ")
  
  
  #Special cases
  ifelse(collect_platra=="transplanting distance between plants", 
         collect_platra <-c(collect_platra, "transplanting density distance between plants"), 
         collect_platra)
  
  ifelse(collect_platra=="transplanting distance between rows", 
         collect_platra <-c(collect_platra, "transplanting density distance between rows"), 
         collect_platra)
  
  ifelse(collect_platra=="transplanting distance between bunds", 
         collect_platra <-c(collect_platra,"transplanting density distance between bunds"), 
         collect_platra)
  
  ifelse(collect_platra=="transplanting number of rows", 
         collect_platra <-c(collect_platra,"transplanting density number of rows"), 
         collect_platra)
  
  
  #management practices
  mpra_trait <- names(dt)
  mpra_trait <- stringr::str_replace_all(tolower(mpra_trait), pattern = "_|//*", replacement = " ")
  mpra_trait <- stringr::str_replace_all(tolower(mpra_trait), pattern = "[:digit:]+", replacement = "") %>% stringr::str_trim(side="both")
  
  
  lgl<- grepl(pattern = paste0(collect_platra, collapse="|"),x = mpra_trait)
  
  dt <- dt[which(lgl==TRUE)]
  
  
  
  
  
})


