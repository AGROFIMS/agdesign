#Get measurement data from Crop Measurement Data Dictionary
#
#
get_dcm_values <- function(data_dictionary=NULL, attribute = "Subgroup", crop="Potato"){
  
  if (!is.null(data_dictionary) && !is.null(crop)) {
    
    if(attribute=="Timing"){
      out<- c("Day after planting", "Frequency","Date","Growth stage","Other")
    } else {
      out <-data_dictionary %>% filter(Crop==crop) %>% select_(attribute)  
      out<- unique(out)
    }
    
  } else{
    out <-  ""
  }
  out
} 

# allinputs : reactive table with all the shiny inputs
# ctype: croppping type
# addId: list active Ids(number) derived from user click on add button()
# cropId: vector active Ids(number) of the crop in the CROP tab
#
get_dtcmea_variables <- function(allinputs, ctype="monocrop", addId="1", crop="none", cropId="1"){
  
  if(ctype=="monocrop"){
    lookup <- "mono_mea_"
    cropId <- "1"
  } else if(ctype=="intercrop"){
    lookup <- "int_mea_"
  } else if(ctype=="relay crop"){
    lookup <- "rel_mea_"
  }
  
  dt <- allinputs %>% dplyr::filter(!str_detect(id, "add")) %>%
                      dplyr::filter(!str_detect(id, "button")) %>%
                      dplyr::filter(!str_detect(id, "_search")) %>%  ##Contemplate Unit case
                      dplyr::filter(!str_detect(id, "_sel_factor_")) %>%
                      dplyr::filter(!str_detect(id, "_closeBox_")) %>%  
                      dplyr::filter(!str_detect(id, "-selectized")) %>%  
                      dplyr::filter(str_detect(id,   paste0("^",lookup) ))
  #print("dt_cmea")
  #print(dt)
  
  if(nrow(dt)!=0 &&  length(addId)!=0 && length(cropId)!=0){
    mea<-parmea <-unit <- pseason <- pplot<- timing<- timValue <- timNumLev <- NULL
    for( i in seq.int(addId) ){
      mea[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,cropId,"_measurement_",addId[i],"$") ))  %>% dplyr::nth(2)
      parmea[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,cropId,"_parmea_",addId[i],"$") ))  %>% dplyr::nth(2)
      unit[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup, cropId,"_unit_",addId[i],"$") ))  %>% dplyr::nth(2)
      pseason[i]<- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,cropId,"_per_season_",addId[i],"$") ))  %>% dplyr::nth(2)
      pplot[i]<- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,cropId,"_per_plot_",addId[i],"$") ))  %>% dplyr::nth(2)
      timing[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,cropId,"_timing_",addId[i],"$") ))  %>% dplyr::nth(2)
      
      if(timing[i]=="Date"){
        timNumLev[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,cropId,"_timingNumLevels_",addId[i],"$") ))  %>% dplyr::nth(2)
        for(j in seq.int(as.integer(timNumLev[i]))){
          timValue[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,cropId,"_timingValue_",addId[i],"_[[:digit:]]+","$") )) %>% dplyr::nth(2) %>% paste(., collapse = ",")
        }
        #mono_mea_1_timingValue_1_1 #mono_mea_1_timingValue_1_2 #mono_mea_1_timingValue_1_3
      }else {
        timValue[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,cropId,"_timingValue_",addId[i],"_1","$") ))  %>% dplyr::nth(2)        
      }
    }
    
    dt<- tibble::tibble(crop, mea, parmea, unit, as.numeric(pseason), as.numeric(pplot), timing, timValue)
    names(dt) <- c("Crop", "Measurement", "Subgroup","TraitUnit",  "NumberofMeasurementsPerSeason", "NumberofMeasurementsPerPlot",
                   "Timing", "TimingValue")
  } 
  else {
    #Case: In case there are not any selected variable/measurement 
    dt <- data.frame()
  }
  dt
}


#Get trait data
# trait_variables: variables selected in the Crop Measurement interface 
get_dt_trait <- function(dtcmea_variables, dt_cmea){

 if(nrow(dtcmea_variables)!=0){
   dt <- dplyr::left_join(dtcmea_variables, dplyr::select(dt_cmea,-starts_with("Number")))
   dt <- dt %>% dplyr::mutate(TraitName = paste(Crop, Subgroup, Measurement, TraitUnit, sep="_"))
 } else {
   dt <- data.frame()
 }
 dt  

}

  
  
  
# cropId active Ids(number) of the crop in the CROP tab
# ctype: croppping type
#
# get_cmea_multicrop_addId <- function(cropId, ctype= "intercrop"){
#   
#   if(ctype=="intercrop"){
#     
#     
#     if( cropId=="1" ){
#       v <- getAddInputId(meaINT1$ids, "int_mea_[:digit:]+_fluidRow_","")
#     } else if (cropId=="2"){
#       v <- getAddInputId(meaINT2$ids, "int_mea_[:digit:]+_fluidRow_","")
#     } else if (cropId=="3"){
#       v <- getAddInputId(meaINT3$ids ,"int_mea_[:digit:]+_fluidRow_","")
#     } else if (cropId=="4"){
#       v <- getAddInputId(meaINT4$ids ,"int_mea_[:digit:]+_fluidRow_","")
#     } else if (cropId=="5"){
#       v <- getAddInputId(meaINT5$ids ,"int_mea_[:digit:]+_fluidRow_","")
#     } else{ 
#       v <-NULL
#     }
#   } 
#   else if(ctype=="relay crop"){
#     
#     if(cropId=="1"){#
#       v <- getAddInputId(meaREL1$ids , "rel_mea_[:digit:]+_fluidRow_","")
#     } else if (cropId=="2"){
#       v <- getAddInputId(meaREL2$ids , "rel_mea_[:digit:]+_fluidRow_","")
#     } else if (cropId=="3"){
#       v <- getAddInputId(meaREL3$ids ,"rel_mea_[:digit:]+_fluidRow_","")
#     } else if (cropId=="4"){
#       v <- getAddInputId(meaREL4$ids ,"rel_mea_[:digit:]+_fluidRow_","")
#     } else if (cropId=="5"){
#       v <- getAddInputId(meaREL5$ids , "rel_mea_[:digit:]+_fluidRow_","")
#     } else{ 
#       v <-NULL
#     }
#     
#   } 
#   else if(ctype=="rotation"){
#     
#     if( cropId=="1" ){
#       v <- getAddInputId(meaROT1$ids, "rot_mea_[:digit:]+_fluidRow_","")
#     } else if (cropId=="2"){
#       v <- getAddInputId(meaROT2$ids,"rot_mea_[:digit:]+_fluidRow_","")
#     } else if (cropId=="3"){
#       v <- getAddInputId(meaROT3$ids,"rot_mea_[:digit:]+_fluidRow_","")
#     } else if (cropId=="4"){
#       v <- getAddInputId(meaROT4$ids,"rot_mea_[:digit:]+_fluidRow_","")
#     } else if (cropId=="5"){
#       v <- getAddInputId(meaROT5$ids,"rot_mea_[:digit:]+_fluidRow_","")
#     } else{ 
#       v <-NULL
#     }
#     
#   }
#   
#   out<- v 
# }




