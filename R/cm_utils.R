#Get measurement data from Crop Measurement Data Dictionary
#attribute: Column values to look for
#crop: crop name
#subgroup: subgroup column name
get_dcm_values <- function(data_dictionary=NULL, attribute = "Subgroup", crop="Potato", subgroup=NULL, measurement= NULL){
  
  if (!is.null(data_dictionary) && !is.null(crop)) {
    
    if(attribute=="Timing"){
      out<- c("Days after planting", "Frequency","Date","Growth stage","Other")
    } else if(is.null(measurement)) {
      out <-data_dictionary %>% dplyr::filter(Crop==crop) %>% dplyr::select_(attribute)  
      out<- unique(out)
      
    } else if(!is.null(measurement) && attribute=="Subgroup" && !is.null(crop)){
      out <- data_dictionary %>% dplyr::filter(Crop==crop) %>% 
                                 #dplyr::filter(Subgroup==subgroup) %>% 
                                 dplyr::filter(Measurement == measurement) %>%  
                                 dplyr::select_(attribute)
      #out<- out[,1] 
      
    } else if(!is.null(measurement) && attribute=="TraitUnit"){
      out <-data_dictionary %>% dplyr::filter(Crop==crop) %>% 
                                dplyr::filter(Subgroup==subgroup) %>% 
                                dplyr::filter(Measurement==measurement) %>% 
                                dplyr::select_(attribute)  
      out<- out[,1] 
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
get_trait_dt <- function(dtcmea_variables, dt_cmea){

 if(nrow(dtcmea_variables)!=0){
   trait_dt <- dplyr::left_join(dtcmea_variables, 
                          dplyr::select(dt_cmea, -c("NumberofMeasurementsPerSeason",	"NumberofMeasurementsPerPlot",	"Timing",	"TimingValue")))
   trait_dt <- trait_dt %>% dplyr::mutate(TraitName = paste(Crop, Subgroup, Measurement, TraitUnit, sep="_"))
 } else {
   trait_dt <- data.frame()
 }
  trait_dt  

}

  




