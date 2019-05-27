#Get measurement data from Crop Measurement Data Dictionary
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



#Get measurement values from crop measurement layout
#ctype: croppping type
#addId: Id of the number of measurement per crop (list of vectors `addId``)
#cropId: id number of the crop (vector of id)

get_mea_inputs <-function(ctype="monocrop",  addId=list(c("1"),c("1")), cropId= "1") {
  
  #template 
  if(cytpe=="monocrop"){
    lookup <- paste0("mono","_mea_")
    cropId<-"1"
  } 
  else if(ctype=="intercrop"){
    lookup <- paste0("int","_mea_")
    #paste0(lookup,"_mea_",cropId,"_measurement_"addId)
    #"int_mea_1_measurement_1"
  } 
  else if(ctype=="relay crop"){
    lookup <- paste0("rel","_mea_")
    #paste0(lookup,"_mea_",cropId,"_measurement_"addId)
  }
  
  mea <- allinputs %>% dplyr::filter(!str_detect(id, "button")) %>%
    dplyr::filter(!str_detect(id, "-selectized")) %>%
    dplyr::filter(str_detect(id, paste0(lookup)))
  
  
  #measurement<- parmea <- munit <- pseason<- pplot <-tim NULL
  
  for( i in 1:length(addId)){
    
    # #measurement
    # measurement <- rbind(measurement , mea  %>% filter(str_detect(id, paste0(lookup, cropId,"_measurement_"addId[i],"$"))))
    # 
    # #parameter
    # parmea <- rbind(parmea, mea %>%  filter(str_detect(id, paste0(lookup, cropId,"_parmea_"addId[i]),"$")))
    # 
    # #unit
    # munit <- rbind(munit,  mea %>% filter(str_detect(id, paste0(lookup,cropId,"_unit_"addId[i]),"$")))
    # 
    # #per season
    # pseason <- rbind(pseason,  mea %>% filter(str_detect(id, paste0(lookup, cropId,"_per_season_"addId[i]),"$")))
    # #TODO: Default = 1
    # 
    # #per plot
    # pplot<- rbind(pplot,  mea %>% filter(str_detect(id, paste0(lookup, cropId,"_per_plot_"addId[i]),"$")))
    # #mono_mea_1_per_plot_1
    # 
    # #timing
    # tim <- rbind(tim ,  mea %>% filter(str_detect(id,  paste0(lookup, cropId,"_timing_"addId[i]),"$")))
    
    ### TODO: ID de los timingValues deben ser iguales, excepto para fecha.
    #timingValue  
    #paste0(lookup,cropId,"_timingValue_"addId[i])
    #mono_timingValueText_1_1
    
  }
  
}