get_soil_factor_level<- function(id_rand, allinputs){
  
  # allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/soilfer-factor-lvl-table_ids.rds")
  # input <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/soilfer-factor-lvl-table_ids.rds")
  
  #TODO : diferenciar entre full and non factorial
  #id_ff_rand <- getAddInputId(designVars$ids_FULL, "FF_", "") 
  #id_nff_rand <- getAddInputId(designVars$ids_NFULL, "NFF_", "") 
  
  #id_rand<-c("CSDKWYAW", "HIRIDBEP", "ZZSSXINS")
  
  #Remove button and selectized
  cleaninputs <- allinputs %>%  filter(!str_detect(id, "button")) %>%
    filter(!str_detect(id, "-selectized"))
  
  #Arrage table by id_rand
  sffl <- arrange_by_pattern(cleaninputs, id_rand)
  temp <- sffl %>% filter(values=="Soil fertility")
  if(nrow(temp)!=0){
    
    #Extract the random ids for soil (id_rand_soil) and arrage by id_rand_soil
    id_rand_soil <- str_extract_all(temp$id,"[:uppercase:]{8}") %>% unlist() %>% unique()
    sffl <- arrange_by_pattern(sffl, id_rand_soil)
    #sffl[sffl$values=="",] <- "-"
    
    #Get group 1, group2, and group 3 and levels   s
    
    #Group 1
    #res<- paste0("sel_factor_[:uppercase:]{8}_3")
    sel1 <- sffl %>% filter(str_detect(id, "sel_factor_[:uppercase:]{8}_1$"))
    
    #Group 2
    sel2 <-  sffl %>% filter(str_detect(id, "sel_factor_[:uppercase:]{8}_2$"))
    
    #Group 3
    sel3 <-  sffl %>% filter(str_detect(id, "sel_factor_[:uppercase:]{8}_3$"))
    
    
    #Cases: Number of fertilizer applications
    # Fertilizer product application rate
    # Nutrient element application rate
    #Biofertilizer
    #Inorganic
    #Green manure
    #Lime
    #Organic
    #Fertilizer application technique
    #Implement type
    
    
    #Inside subtab after select any case above
    
    #Number of levels of the factor  ##########################################################################
    nlvl<- sffl %>% filter(str_detect(id, "numLevels_tabSoil_[:uppercase:]{8}$"))
    ##########################################################################################################
    
    #Number of application of each tab -----------------------------------------
    napp <- sffl %>% filter(str_detect(id, "numApps_tabSoil_factor_[:uppercase:]{8}_box_"))
    napplvl<- list()
    for(i in 1:length(id_rand_soil)){
      flvl <- napp %>% filter(str_detect(id, id_rand_soil[i]))    
      napplvl[[i]] <- flvl$values
      if(all(napplvl[[i]] == "")){napplvl[[i]] <- rep("-", length(napplvl[[i]] ))} 
      
    }
    names(napplvl)<-paste("Number of fertilizer applications",id_rand_soil)    
    ############################################################################################################
    
    ############################### FERTILIZER PRODUCT ##########################################################
    
    #Fertilizer product #########################################################################################
    fpd <-  sffl %>% filter(str_detect(id, "select_product_factor_[:uppercase:]{8}_level_"))
    #fpd <-  sffl %>% filter(str_detect(id, "select_product_factor_[:uppercase:]{8}_level_[]"))
    #fpd <-  sffl %>% filter(str_detect(id, "select_product_factor_[:uppercase:]{8}_level_"))
    ################################################################################################################
    
    # Fertilizer product rate ##########################################################################################
    fpr <- sffl %>% filter(str_detect(id, "input_tabSoil_rate_product_[:uppercase:]{8}_level_[:digit:]{1,15}_app_[:digit:]{1,15}$"))
    ################################################################################################################
    
    #TOTAL FERTILIZER RATE ##########################################################################################
    ftot <- sffl %>% filter(str_detect(id, "input_product_RateTotal_factor_[:uppercase:]{8}_level_[:digit:]{1,15}$"))
    ftotlvl<- list()
    for(i in 1:length(id_rand_soil)){
      flvl <- ftot %>% filter(str_detect(id, id_rand_soil[i]))    
      ftotlvl[[i]] <- flvl$values   
      if(all(ftotlvl[[i]] == "")){ftotlvl[[i]] <- rep("-", length(ftotlvl[[i]] ))} 
    }
    names(ftotlvl)<-paste("Fertilizer product application rate",id_rand_soil)
    #################################################################################################################
    
    
    ##############################NUTRIENT ###########################################################################
    # Nutrient element
    nue <- sffl %>% filter(str_detect(id, "input_element_factor_[:uppercase:]{8}_level_[:digit:]{1,15}_app_[:digit:]{1,15}$"))
    #nue<- sffl %>%   filter(str_detect(id, "input_element_factor_[:uppercase:]{8}_level_[1-9]_app_[1-3]$"))
    #################################################################################################################
    
    #Nutrient element rate
    nuer <- sffl %>% filter(str_detect(id, "input_tabSoil_rate_element_[:uppercase:]{8}_level_[:digit:]{1,15}_app_[:digit:]{1,15}$"))
    #################################################################################################################
    
    #TOTAL NUTRIENT ELEMENT ##########################################################################################
    nutot <- sffl %>% filter(str_detect(id, "input_element_RateTotal_factor_[:uppercase:]{8}_level_[:digit:]{1,15}$"))
    nutlvl<- list()
    for(i in 1:length(id_rand_soil)){
      flvl <- nutot %>% filter(str_detect(id, id_rand_soil[i]))    
      nutlvl[[i]] <- flvl$values
      if(all(nutlvl[[i]] == "")){nutlvl[[i]] <- rep("-", length(nutlvl[[i]]))} 
    }
    names(nutlvl)<-paste("Nutrient element application rate",id_rand_soil)
    ################################################################################
    
    out<- list(napplvl, ftotlvl, nutlvl)
    out<- purrr::flatten(out)
    
    
  } else{
    out <- NULL
  }
  

  
}
