# allinputs : reactive table with all the shiny inputs
# ctype: croppping type
# addId: (vector) active Ids(number) derived from user click on add button()
#
get_weather_variables <- function(allinputs,addId="1"){
  
  
  lookup <- "weather_" 
  
  dt <- allinputs %>% dplyr::filter(!str_detect(id, "add")) %>%
    dplyr::filter(!str_detect(id, "button")) %>%
    dplyr::filter(!str_detect(id, "_search")) %>%  ##Contemplate Unit case
    dplyr::filter(!str_detect(id, "_sel_factor_")) %>%
    dplyr::filter(!str_detect(id, "_closeBox_")) %>%  
    dplyr::filter(!str_detect(id, "-selectized")) %>%  
    dplyr::filter(str_detect(id,   paste0("^",lookup) ))
  
  
  if(nrow(dt)!=0 && length(addId)!=0){
    crop <- ""
    group<- "Weather"
    subgroup<-""
    mea <-unit <- pseason <- pplot <-timing <-timValue<- timNumLev <- NULL
    for( i in seq.int(addId)){
      mea[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"mea_",addId[i],"$") ))  %>% dplyr::nth(2)
      unit[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"unit_",addId[i],"$") ))  %>% dplyr::nth(2)
      pseason[i]<- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"per_season_",addId[i],"$") ))  %>% dplyr::nth(2)
      pplot[i]<-  "1" #allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"per_plot_",addId[i],"$") ))  %>% dplyr::nth(2)
      timing[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"timing_",addId[i],"$") ))  %>% dplyr::nth(2)
      
      if(timing[i]=="Date"){
        timNumLev[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"timingNumLevels_",addId[i],"$") ))  %>% dplyr::nth(2)
        for(j in seq.int(as.integer(timNumLev[i]))){
          timValue[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"timingValue_",addId[i],"_[[:digit:]]+","$") )) %>% dplyr::nth(2) %>% paste(., collapse = ",")
        }
      }else {
        timValue[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"timingValue_",addId[i],"_1","$") ))  %>% dplyr::nth(2)        
      }
      
      
      #timValue[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"timingValue_",addId[i],"_1","$") ))  %>% dplyr::nth(2)
    }
    
    dt<- tibble::tibble(crop, group, subgroup, mea, unit, as.numeric(pseason), as.numeric(pplot), timing, timValue)
    names(dt) <- c("Crop", "Group","Subgroup", "Measurement", "TraitUnit",  "NumberofMeasurementsPerSeason",
                   "NumberofMeasurementsPerPlot","Timing","TimingValue")
    
    
  } 
  else {
    #Case: In case there are not any selected variable/measurement 
    dt <- data.frame()
  }
  dt
}

#
#
#
get_dt_weather <- function(weather_variables,dt_weather){
  dt_weather$Group <- "Weather"
  dt_weather$Crop <- ""
  dt_weather$Subgroup <-""
  if(nrow(weather_variables)!=0){
    dt <- dplyr::left_join(weather_variables, dplyr::select(dt_weather,-starts_with("Number")))
  } else {
    dt <- data.frame()
  }
  dt  
  
}


# Get soil variables from user interface
# allinputs : reactive table with all the shiny inputs
# ctype: croppping type
# addId: (vector) active Ids(number) derived from user click on add button()
#
get_soil_variables <- function(allinputs,addId="1"){
  
  
  lookup <- "soil_" #soilVars$num 
  
  dt <- allinputs %>% dplyr::filter(!str_detect(id, "add")) %>%
    dplyr::filter(!str_detect(id, "button")) %>%
    dplyr::filter(!str_detect(id, "_search")) %>%  ##Contemplate Unit case
    dplyr::filter(!str_detect(id, "_sel_factor_")) %>%
    dplyr::filter(!str_detect(id, "_closeBox_")) %>%  
    dplyr::filter(!str_detect(id, "-selectized")) %>%  
    dplyr::filter(str_detect(id,   paste0("^",lookup) ))
  
  
  if(nrow(dt)!=0 && length(addId)!=0){
    crop <- ""
    group<- "Soil"
    subgroup<-""
    mea <-unit <- pseason <- pplot <-depth <-depthUnit <- timing <- timValue <- timNumLev <- NULL
    for( i in seq.int(addId)){
      mea[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"mea_",addId[i],"$") ))  %>% dplyr::nth(2)
      unit[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"unit_",addId[i],"$") ))  %>% dplyr::nth(2)
      pseason[i]<- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"per_season_",addId[i],"$") ))  %>% dplyr::nth(2)
      pplot[i]<- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"per_plot_",addId[i],"$") ))  %>% dplyr::nth(2)
      depth[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"depth_",addId[i],"$") ))  %>% dplyr::nth(2)
      depthUnit[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"depthunit_",addId[i],"$") ))  %>% dplyr::nth(2)
      timing[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"timing_",addId[i],"$") ))  %>% dplyr::nth(2)
      
      if(timing[i]=="Date"){
        timNumLev[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"timingNumLevels_",addId[i],"$") ))  %>% dplyr::nth(2)
        for(j in seq.int(as.integer(timNumLev[i]))){
          timValue[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"timingValue_",addId[i],"_[[:digit:]]+","$") )) %>% dplyr::nth(2) %>% paste(., collapse = ",")
        }
      }else {
        timValue[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"timingValue_",addId[i],"_1","$") ))  %>% dplyr::nth(2)        
      }
      #timValue[i] <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",lookup,"timingValue_",addId[i],"_1","$") ))  %>% dplyr::nth(2)
      
    }
    
    dt<- tibble::tibble(crop, group, subgroup, mea, unit, as.numeric(pseason), as.numeric(pplot), depth, 
                        as.numeric(depthUnit), timing,timValue)
    names(dt) <- c("Crop", "Group","Subgroup", "Measurement","TraitUnit", "NumberofMeasurementsPerSeason", "NumberofMeasurementsPerPlot",
                   "Depth", "DepthUnit", "Timing","TimingValue")
    
  } 
  else {
    #Case: In case there are not any selected variable/measurement 
    dt <- data.frame()
  }
  dt
}


get_dt_soil <- function(soil_variables,dt_soil){
  dt_soil$Group <- "Soil"
  dt_soil$Crop <- ""
  dt_soil$Subgroup <-""
  if(nrow(soil_variables)!=0){
    dt <- dplyr::left_join(soil_variables, dplyr::select(dt_soil,-starts_with("Number")))
  } else {
    dt <- data.frame()
  }
  dt  
  
}
