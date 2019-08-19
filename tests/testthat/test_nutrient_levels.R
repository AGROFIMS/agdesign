library(dplyr)
library(stringr)
library(magrittr)
library(testthat)
library(agdesign)
context("test type and amount factors ")

#----------- type = select  --------------------------------------------------------------

test_that("Test type and amount factor (special factor)", {
  
  #ADVICE: NOT VECTORIZED, should be included in the main class get_levels_design
  
  source("R/experimental_design.R")
  fname <- rprojroot::find_testthat_root_file("userInput", "LevelIdAmountTypefactor.rds")
  allinputs <- readRDS(fname)
  
  index <- "2"
  indexEspLvl <- c("frcbd_lvl_espType_2_1" ,"frcbd_lvl_espType_2_2")
  design <- "frcbd"
  #lvlIndex <- get_index_espLvl_design(id = lvlIds, design = paste0(design,"_lvl_espType_")) 
  #lvlIndex <- 
  
  #Filter index from special factors and levels
  indexEspLvl <- filter_index_espLvl_design(index = index, indexEspLvl= indexEspLvl, design=design, designEspflvl="_lvl_espType_")
  indexEspLvl <- get_index_espLvl_design(indexEspLvl, paste0(design,"_lvl_espType_",index,"_"))  #"frcbd_lvl_espType_2_")
  
  #Lookup design pattern
  lookup <- paste0("^",design,"_")
  dt <- allinputs %>% dplyr::filter(!str_detect(id, "add")) %>%
    dplyr::filter(!str_detect(id, "button")) %>%
    #dplyr::filter(!str_detect(id, "unit")) %>%  ##Contemplate Unit case
    dplyr::filter(!str_detect(id, "_sel_factor_")) %>%
    dplyr::filter(!str_detect(id, "-selectized")) %>%  
    dplyr::filter(str_detect(id, lookup))
  
  ## crop --------------------------------------------------------------------------------------------
  #structure: design_factor_crop_input_index
  crop <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",design,"factor","_crop_input",index,"$") )) 
  if(nrow(crop)!=0){
    crop <- crop$values  
  }else{
    crop<- ""
  }
  
  ## number of evaluation for each element -----------------------------------------------------------
  ## input structure: design_numLevelsESP_index
  #numEval<- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",design, "_numLevelsESP_",index,"$"))) %>% nth(2)
  #numEval <- length(indexEspLvl)
  numEval <- as.integer(length(indexEspLvl))
  
  eleType <- unit <-NULL
  out <- list()
  lvl <- NULL
  for(j in seq.int(numEval)){
    
    ## level values. 
    ## input structure: design_lvl_espType_index_numEval------------------------------------------------------------------------------
    eleType <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"lvl_espType_",index,"_", indexEspLvl[j],"$"))) %>% nth(2) 
    print(eleType) 
    
    ## levels + unit -----------------------------------------------------------------------------------------------
    ## input structure:  #design_lvl_index_numEval
    lvl <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup, "lvl_espLvl_",index,"_", indexEspLvl[j],"$")))  %>% nth(2)
    lvl <-  strsplit(lvl,",")[[1]] %>% stringi::stri_trim_both()
    print(lvl)
    #unit  ----------------------------------------------------------------------------------------
    ##  input strcucture: design_lvl_unit_index_numEval 
    unit <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"lvl_espUnit_",index,"_", indexEspLvl[j],"$")))  %>% nth(2)
    print(unit[j])
    
    if(crop!=""){
      out[[j]] <- paste0(crop,"_",eleType,"_",lvl,unit)
      #print(out[j])
    } else {
      out[[j]] <- paste0(eleType,"_",lvl,unit)
    }
    #print(out[[j]])
    
  }
  out <- unlist(out)
  
})