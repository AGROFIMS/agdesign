library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
library(agdesign)
context("Experiment information")

test_that("Experiment lead", {
  
  #source("/home/obenites/AGROFIMS/agdesign/R/ec_resmgt.R")
  input <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/test_input_expLead.rds")
   
  id_rand_el <- c("1","2") #getAddInputId(experimentVars$ids_EL, "EL_", "")
  pl <- map_values(input, id_chr="projLeadEnt_", id_rand_el,format = "data.frame",lbl= "Experiment, lead organization name")
  pcgiar <- map_values(input, id_chr="tLeadCenter_", id_rand_el, format = "data.frame", lbl= "Experiment, CGIAR center name")
  plc <- map_values(input, id_chr="tLeadContCRP_", id_rand_el, format = "data.frame", lbl= "Experiment, lead contributor crp")
  pel <- map_values(input, id_chr="expLead_", id_rand_el,format = "data.frame", lbl= "Experiment lead person / Primary Investigator")
  out <-rbind(pl,pcgiar,plc, pel)
  
  input2 <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/test_input_expLead_2.rds")
  
  id_rand_el <- c("1","2","3","4") #getAddInputId(experimentVars$ids_EL, "EL_", "")
  pl <- map_values(input2, id_chr="projLeadEnt_", id_rand_el,format = "data.frame",lbl= "Experiment, lead organization name")
  pcgiar <- map_values(input2, id_chr="tLeadCenter_", id_rand_el, format = "data.frame", lbl= "Experiment, CGIAR center name")
  pcgiar <- pcgiar %>% dplyr::filter(res!="")
  plc <- map_values(input2, id_chr="tLeadContCRP_", id_rand_el, format = "data.frame", lbl= "Experiment, lead contributor crp")
  pel <- map_values(input2, id_chr="expLead_", id_rand_el,format = "data.frame", lbl= "Experiment lead person / Primary Investigator")
  out2 <-rbind(pl,pcgiar,plc, pel)
  
  # input <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/inputs.rds")
  # id_rand_el <-  c("1")

  
  testthat::expect_equal(nrow(out), 8)  
  testthat::expect_equal(ncol(out), 2) 
  testthat::expect_equal(nrow(out2), 14)  
  testthat::expect_equal(ncol(out2), 2) 

})


test_check("Personnel information", {
  
  input <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/inputs.rds")
  
  id_rand_pers <-  c("1","2","3")
  
  pst <- map_values(input, id_chr="personnel_type_", id_rand_pers,format = "data.frame", lbl= "Person type")
  prfn <- map_values(input, id_chr="person_firstName_", id_rand_pers,format = "data.frame", lbl= "Person, first name")
  prsn <- map_values(input, id_chr="person_lastName_", id_rand_pers,format = "data.frame", lbl= "Person, last name")
  prmail <- map_values(input, id_chr="person_email_", id_rand_pers,format = "data.frame", lbl= "Person email")
  
  praf <- map_values(input, id_chr="person_affiliation_", id_rand_pers, format = "data.frame", lbl= "Person, affiliation")
  prafname <- map_values(input, id_chr="affiliation_name_", id_rand_pers, format = "data.frame", lbl= "Person, affiliation name")
  
  dt<- rbind(pst,prfn,prsn,prmail, praf, prafname)
  
  testthat::expect_equal(nrow(out), 18) 
  
})


test_check("Factor metadata", {
  
  allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/test_factor_metadata.rds")
  design<- "frcbd"
  index <- c("1","2")
  
  flvl <- get_levels_design(allinputs = allinputs, indexEspLvl=NULL, data_dictionary=dt_factordesign, 
                            c("1","2"), factors = c("Crop variety","Harvest start date"), design="frcbd", format="list")
  
  out<- lapply(index, function(x)  allinputs %>% dplyr::filter(str_detect(id,  paste0(design,"_note_factor_",x,"$")))  )
  out <- data.table::rbindlist(out) %>% as.data.frame(stringsAsFactors=FALSE)
  
  #Number of notes
  testthat::expect_equal(nrow(out), 2) 
  #Number of levels of factor 1
  testthat::expect_equal(  length(flvl[[1]])  , 3) 
  #Number of levels of factor 2
  testthat::expect_equal(  length(flvl[[2]])  , 2) 
  
  
})