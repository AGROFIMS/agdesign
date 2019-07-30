## Get managements practices for harvest ########################################################
get_ec_harv <- function(allinputs, input, ctype="monocrop", cropId="1", addId="1"){
  
  #allinputs<- readRDS("tests/testthat/userInput/harvest-tableIds-01.rds")
  #allinputs<- readRDS("tests/testthat/userInput/irri-table-id-all-tech-sel.rds.rds")
  #allinputs<- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/table_ids.rds")
  
  if(ctype=="monocrop"){
    #"monocrop_hahd_"
    lookup<- "monocrop"
    addId <- "1"
    #monocrop_hahd_crop_component_harvested_1
  } 
  else if(ctype=="intercrop"){
    lookup<- paste0("int_harv_",cropId)
    addId<- addId
    #int_harv_2_hahd_harvest_method_2
  } 
  else if(ctype=="relay crop") {
    lookup<- paste0("rel_harv_",cropId) 
    addId<-addId
  }
  
  harv <- allinputs %>% dplyr::filter(!str_detect(id, "button")) %>%
    dplyr::filter(!str_detect(id, "-selectized")) %>%
    dplyr::filter(str_detect(id, paste0(lookup)))
  
  #harv_temp  <- data.frame()
  
  startD<- endD<- method <- method_other<- comph <- comph_other <-  ha_area<- NULL
  
  for( i in 1:length(addId)){
    #harvest start date
    startD <- rbind(startD , harv  %>% filter(str_detect(id, paste0(lookup,"_hahd_harvest_start_date_",addId[i],"$"))) )
    #harvest end date
    endD <- rbind(endD, harv  %>% filter(str_detect(id, paste0(lookup,"_hahd_harvest_end_date_",addId[i],"$"))))
    
    method <-  rbind(method, harv  %>% filter(str_detect(id, paste0(lookup,"_hahd_harvest_method_",addId[i],"$"))) )
    method_other <-  rbind(method_other, harv  %>% filter(str_detect(id, paste0(lookup,"_hahd_harvest_method_",addId[i],"_other$"))) )
    #method <-  dt_inputs(method, method_other)
    
    comph <- rbind(comph,  harv %>% filter(str_detect(id,  paste0(lookup,"_hahd_crop_component_harvested_",addId[i],"$"))) )
    comph_other <- rbind(comph_other ,  harv %>% filter(str_detect(id,paste0(lookup,"_hahd_crop_component_harvested_",addId[i], "_other$"))) )
    #compht <- dt_inputs(comph, comph_other)
    
    #Harvest area
    ha_area <- rbind(ha_area, harv  %>% filter(str_detect(id,paste0(lookup,"_hahd_crop_harvestable_area_",addId[i],"$"))))
    
    #lbl_ha_area <- "Harvest area"
  }
  method <-  dt_inputs(method, method_other)
  comph <- dt_inputs(comph, comph_other)
  
  
  ###SPECIAL CASE FOR SELECTING HARVEST AREA
  values_ha_area_sp <- lbl_ha_area_sp <- lbl_ha_area_num <- NULL
  #Harvesta area : m2 units
  for(i in 1:length(addId)){
    
    if(ha_area$values[i]==""){
      values_ha_area_sp <- append(values_ha_area_sp, "")
      lbl_ha_area_sp <-  append(lbl_ha_area_sp, paste("Harvest_area",addId[i],sep="_"))
      #label with numbers
      lbl_ha_area_num <- append(lbl_ha_area_num, paste("Harvest_area",i,sep="__"))
      
    } 
    else if(ha_area$values[i]=="m2 units"){
      #input$hahd_crop_component_harvested_m2_[:digit:]+
      ha_m2 <- harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_crop_component_harvested_m2_",addId[i],"$")))
      values_ha_area_sp <- append(values_ha_area_sp, ha_m2$values)
      
      #main label
      #lbl_ha_area_sp <-  append(lbl_ha_area_sp, paste("Harvestable_area_number_of_m2_units_harvested",addId[i],sep="_")) #deprecated
      lbl_ha_area_sp <-  append(lbl_ha_area_sp, paste("Number_of_m2_units_harvested",addId[i],sep="_")) 
      #label with numbers
      lbl_ha_area_num <-  append(lbl_ha_area_num, paste("Number_of_m2_units_harvested",i, sep="__"))
      
    } 
    else if(ha_area$values[i]=="Individual plants"){
      #Harvesta area : individual plants
      ha_ip <- harv  %>% filter(str_detect(id, paste0(lookup,"_hahd_crop_component_harvested_ip_",addId[i],"$")))
      values_ha_area_sp<- append(values_ha_area_sp, ha_ip$values)
      #main label
      #lbl_ha_area_sp<-  append(lbl_ha_area_sp,paste("Harvestable_area_number_of_individual_plants_harvested_ip",addId[i],sep="_"))
      lbl_ha_area_sp<-  append(lbl_ha_area_sp,paste("Number_of_individual_plants_harvested",addId[i],sep="_"))
      #label with numbers
      #lbl_ha_area_num <-  append(lbl_ha_area_num,paste("Harvestable_area_number_of_individual_plants_harvested", i, sep="__"))
      lbl_ha_area_num <-  append(lbl_ha_area_num,paste("Number_of_individual_plants_harvested", i, sep="__"))
      #hahd_crop_component_harvested_ip_1
    } 
    else if(ha_area$values[i]=="Rows"){
      #Harvesta area : rows---------------------------------------------------------
      ha_row_num <- harv  %>% filter(str_detect(id, paste0(lookup,"_hahd_crop_component_harvested_num_",addId[i],"$")))
      values_ha_area_sp <- append(values_ha_area_sp, ha_row_num$values)
      #main label
      #lbl_ha_area_sp <-  append(lbl_ha_area_sp, paste("Harvestable_area_number_of_rows_harvested",addId[i],sep="_"))
      lbl_ha_area_sp <-  append(lbl_ha_area_sp, paste("Number_of_rows_harvested",addId[i],sep="_"))
      #label with numbers
      #lbl_ha_area_num <-  append(lbl_ha_area_num, paste("Harvestable_area_number_of_rows_harvested", i ,sep="__"))
      lbl_ha_area_num <-  append(lbl_ha_area_num, paste("Number_of_rows_harvested", i ,sep="__"))
      
      #rows length
      ha_row_len <- harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_crop_component_harvested_len_",addId[i],"$")))
      values_ha_area_sp <- append(values_ha_area_sp, ha_row_len$values)
      ha_row_len_unit <- harv  %>% filter(str_detect(id, paste0("hahd_crop_component_harvested_lenunit_",addId[i],"$")))
      
      #main label
      #lbl_ha_area_sp <-  append(lbl_ha_area_sp, paste("Harvestable_area_length_of_rows_harvested",ha_row_len_unit$values, addId[i], sep="_"))
      lbl_ha_area_sp <-  append(lbl_ha_area_sp, paste("Length_of_rows_harvested",ha_row_len_unit$values, addId[i], sep="_"))
      #numeric labels
      #lbl_ha_area_num <-  append(lbl_ha_area_num, paste(paste("Harvestable_area_length_of_rows_harvested", ha_row_len_unit$values, sep="_"), i, sep="__"))
      lbl_ha_area_num <-  append(lbl_ha_area_num, paste(paste("Length_of_rows_harvested", ha_row_len_unit$values, sep="_"), i, sep="__"))
      
      #rows width
      ha_row_width <- harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_crop_component_harvested_width_",addId[i],"$")))
      values_ha_area_sp <- append(values_ha_area_sp, ha_row_width$values)
      ha_row_width_unit <- harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_crop_component_harvested_widthunit_",addId[i],"$")))
      #lbl_ha_area_sp <-  append(lbl_ha_area_sp,paste("Harvestable_area_width_within_rows_harvested", ha_row_width_unit$values, addId[i], sep="_"))
      lbl_ha_area_sp <-  append(lbl_ha_area_sp,paste("Width_within_rows_harvested", ha_row_width_unit$values, addId[i], sep="_"))
      #numeric labels
      #lbl_ha_area_num <-  append(lbl_ha_area_num, paste(paste("Harvestable_area_width_within_rows_harvested", ha_row_width_unit$values, sep="_"),i, sep="__"))
      lbl_ha_area_num <-  append(lbl_ha_area_num, paste(paste("Width_within_rows_harvested", ha_row_width_unit$values, sep="_"),i, sep="__"))
      
      
      #rows space
      ha_row_space<- harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_space_rows_harvested_",addId[i],"$")))
      values_ha_area_sp <- append(values_ha_area_sp, ha_row_space$values)
      ha_row_space_unit <- harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_crop_component_harvested_spaceunit_",addId[i],"$"))) 
      #main label
      #lbl_ha_area_sp<-  append(lbl_ha_area_sp, paste("Harvestable_area_space_between_rows_harvested",ha_row_space_unit$values,addId[i],sep="_"))
      lbl_ha_area_sp<-  append(lbl_ha_area_sp, paste("Space_between_rows_harvested",ha_row_space_unit$values,addId[i],sep="_"))
      #numeric labels
      #lbl_ha_area_num<-  append(lbl_ha_area_num, paste(paste("Harvestable_area_space_between_rows_harvested",ha_row_space_unit$values,sep="_"),i, sep="__"))     
      lbl_ha_area_num<-  append(lbl_ha_area_num, paste(paste("Space_between_rows_harvested",ha_row_space_unit$values,sep="_"),i, sep="__"))
      
      
    } 
    else if(ha_area$values[i]=="Entire plot"){
      #Harvesta area : entire plot
      ha_row_plot_area <- harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_crop_component_harvested_entire_",addId[i],"$"))) 
      values_ha_area_sp <- append(values_ha_area_sp, ha_row_plot_area$values)
      ha_row_plot_area_unit<- harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_crop_component_harvested_entireunit_",addId[i],"$"))) 
      
      #main plot
      #lbl_ha_area_sp <- append(lbl_ha_area_sp,paste("Harvestable_area_entire_plot_area_harvested",ha_row_plot_area_unit$values,addId[i],sep="_"))
      lbl_ha_area_sp <- append(lbl_ha_area_sp,paste("Entire_plot_area_harvested",ha_row_plot_area_unit$values,addId[i],sep="_"))
      #numeric labels
      #lbl_ha_area_num <- append(lbl_ha_area_num, paste(paste("Harvestable_area_entire_plot_area_harvested",ha_row_plot_area_unit$values,sep="_"),i,sep="__"))
      lbl_ha_area_num <- append(lbl_ha_area_num, paste(paste("Entire_plot_area_harvested",ha_row_plot_area_unit$values,sep="_"),i,sep="__"))
      
    } 
    else if(ha_area$values[i]=="Other"){
      #Harvesta area : other 
      ha_area_other <- harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_crop_harvestable_area_",addId[i],"_other$")))
      values_ha_area_sp <- append(values_ha_area_sp, ha_area_other$values)
      lbl_ha_area_sp<-  append(lbl_ha_area_sp,paste("Harvestable_area_other", addId[i],sep="_")) #TODO CHECK THIS `OTHER` 
      #numeric labels
      lbl_ha_area_num<-  append(lbl_ha_area_num, paste("Harvestable_area_other", i, sep="__")) #TODO CHECK THIS `OTHER`
      
    }
  }
  
  ha_area_sp <- data.frame(id=lbl_ha_area_sp, values = values_ha_area_sp,stringsAsFactors = FALSE)
  
  ##############END ESPECIAL CASE HARVEST AREA 
  
  amount<- amount_unit<-  cut <- cut_unit <-  type <-type_other <-traction<- traction_other <- notes<-NULL
  
  for( i in 1:length(addId)){
    
    #Amount
    amount <- rbind(amount, harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_amount_harvested_",addId[i],"$")))  )
    amount_unit <- rbind(amount_unit, harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_amount_harvested_unit_",addId[i],"$"))) )
    #monocrop_hahd_amount_harvested_1
    cut<-rbind(cut, harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_harvest_cut_height_",addId[i],"$"))) )
    cut_unit <-rbind(cut_unit, harv  %>% filter(str_detect(id,paste0(lookup,"_hahd_harvest_cut_height_unit_",addId[i],"$"))) )
    
    #implement
    type <-rbind(type, harv %>% filter(str_detect(id,  paste0(lookup,"_hahd_harvest_implement_",addId[i],"$"))) )
    type_other <-rbind(type_other, harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_harvest_implement_",addId[i],"_other$"))) )
    #type <- dt_inputs(type, type_other)
    
    #traction
    traction <-rbind(traction, harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_harvest_traction_",addId[i],"$"))) )
    traction_other <- rbind(traction_other, harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_harvest_traction_",addId[i],"_other$"))))
    #traction <- dt_inputs(traction, traction_other)
    
    #notes
    notes<- rbind(notes, harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_harvest_notes_",addId[i],"$"))) )
    
  }
  
  type <- dt_inputs(type, type_other)
  traction <- dt_inputs(traction, traction_other)
  #Bind tables
  dt<- rbind(startD, endD, method,comph, ha_area, ha_area_sp, amount, cut,type, traction, notes )
  print(dt)
  
  #dt<- arrange_by_pattern(dt, addId)
  
  #Create labels
  lbl_start <- paste("Harvest_start_date",1:length(addId),sep = "__")
  lbl_end <- paste("Harvest_end_date",1:length(addId),sep = "__")
  lbl_method <-  paste("Harvest_method",1:length(addId),sep = "__")
  #lbl_comph <- paste("Harvest_crop_component_harvested",1:length(addId),sep = "__") #deprectaed
  lbl_comph <- paste("Crop_component_harvested",1:length(addId),sep = "__")
  lbl_ha_area <-  paste("Harvestable_area", 1:length(addId),sep = "__")
  
  lbl_amount <- paste(paste("Amount_harvested", amount_unit$values, sep="_"),  1:length(addId),sep = "__")
  lbl_cut <- paste(paste("Harvest_cut_height", cut_unit$values, sep="_"),  1:length(addId),sep = "__")
  
  
  lbl_type<- paste("Harvest_implement_type", 1:length(addId), sep = "__")
  #lbl_traction<- paste("Harvest_implement_traction", 1:length(addId),sep = "__") #deprecated
  lbl_traction<- paste("Harvest_traction_type", 1:length(addId), sep = "__")
  
  
  lbl_notes<- paste("Harvest_notes", 1:length(addId),sep = "__")
  
  lbl_harv <- c(lbl_start, lbl_end, lbl_method, lbl_comph, lbl_ha_area,
                lbl_ha_area_num,  lbl_amount, lbl_cut, lbl_type, lbl_traction, lbl_notes)
  
  #print(lbl_harv)
  
  #Special case:
  #Swichtching id values by irrigation labels (lbl_irr)
  dt$id <- lbl_harv
  
  #Remove NoLabel or NonData rows
  #dt <- arrange_by_pattern(dt, as.character(1:length(addId)))
  
  #transpose data as rows   
  dt_harv<- t(dt$values) %>% as.data.frame(stringAsFactors=FALSE)
  names(dt_harv) <- dt$id
  #dt_harv
  
  #LABEL FOR TRAITLIST
  lbl <- str_replace_all(string = names(dt_harv), pattern = "__[:digit:]+$",replacement = "") %>% unique()
  
  #OUTPUT
  out<- list(dt=dt_harv, lbl = lbl)
  
  
}


## Get addId for multi crop trials
get_addId_multiharvest <- function(cropId, ctype= "intercrop" ){
  
  if(ctype=="intercrop"){
    if( cropId=="1" ){
      v <- getAddInputId(expconIntHARVcrop1$ids,pattern = "int_harv_[:digit:]+_","")
    } else if (cropId=="2"){
      v <- getAddInputId(expconIntHARVcrop2$ids,pattern = "int_harv_[:digit:]+_","")
    } else if (cropId=="3"){
      v <- getAddInputId(expconIntHARVcrop3$ids,pattern = "int_harv_[:digit:]+_","")
    } else if (cropId=="4"){
      v <- getAddInputId(expconIntHARVcrop4$ids,pattern = "int_harv_[:digit:]+_","")
    } else if (cropId=="5"){
      v <- getAddInputId(expconIntHARVcrop5$ids,pattern = "int_harv_[:digit:]+_","")
    } else{ 
      v <-NULL
    }
  } 
  else if(ctype=="relay crop"){
    
    if(cropId=="1"){
      v <- getAddInputId(expconRelHARVcrop1$ids, pattern = "rel_harv_[:digit:]+_","")
    } else if (cropId=="2"){
      v <- getAddInputId(expconRelHARVcrop2$ids, pattern = "rel_harv_[:digit:]+_","")
    } else if (cropId=="3"){
      v <- getAddInputId(expconRelHARVcrop3$ids, pattern = "rel_harv_[:digit:]+_","")
    } else if (cropId=="4"){
      v <- getAddInputId(expconRelHARVcrop3$ids, pattern = "rel_harv_[:digit:]+_","")
    } else if (cropId=="5"){
      v <- getAddInputId(expconRelHARVcrop4$ids, pattern = "rel_harv_[:digit:]+_","")
    } else{ 
      v <-NULL
    }
    
  } 
  else if(ctype=="rotation"){
    
    if( cropId=="1" ){
      v <- getAddInputId(expconIntHARVcrop1$ids,pattern = "int_harv_[:digit:]+_","")
    } else if (cropId=="2"){
      v <- getAddInputId(expconIntHARVcrop2$ids,pattern = "int_harv_[:digit:]+_","")
    } else if (cropId=="3"){
      v <- getAddInputId(expconIntHARVcrop3$ids,pattern = "int_harv_[:digit:]+_","")
    } else if (cropId=="4"){
      v <- getAddInputId(expconIntHARVcrop4$ids,pattern = "int_harv_[:digit:]+_","")
    } else if (cropId=="5"){
      v <- getAddInputId(expconIntHARVcrop5$ids,pattern = "int_harv_[:digit:]+_","")
    } else{ 
      v <-NULL
    }
    
  }
  
  out<- v 
}

# 
# Get protocol for harvest trials ##############################################################
get_protocol_harv <- function(allinputs, input, ctype="monocrop", cropId="1", addId="1"){
  
  out<- get_ec_harv(allinputs, input, ctype= ctype, cropId=cropId, addId= addId)$dt 
  if(nrow(out)!=0){
  names(out) <- stringr::str_replace_all(names(out),"__1","")
  out <- t(out) %>% as.data.frame(stringsAsFactors=FALSE) %>% tibble::rownames_to_column()
  out <- out %>% dplyr::filter(V1!="") %>% dplyr::filter(!stringr::str_detect(V1, "^NA$"))
  names(out) <- c("TraitName","Value")
  out <- out
  }else {
    out<- data.frame()
  }
  out
  
}


# Get Collectable inputs for Harvest ###########################################################3
get_collectable_harvest <- function(allinputs, ctype="monocrop",crop, cropId="1",ver="default"){
  
  if(ctype=="monocrop"){
    lookup<- "mono"
    cropId <- "1"
    #direct seeding
    ha <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^", lookup,"_harvest_to_collect_field","$") )) %>% dplyr::nth(2)
    out <- stringi::stri_split_regex(ha, ",")[[1]] %>% stringr::str_trim(side = "both") %>% setdiff("") 
    
    if(length(out)!=0){
      if(ver=="default"){
        out <- paste0("Harvest" ,"_", out) 
      }
      else if(ver=="export"){
        out <- out #ifelse(str_detect(string = out,pattern = "Harvestable|Harvest"), out, paste0("Harvest_",out))
      }
    }
    
  } 
  else {
    
    if(ctype=="intercrop"){
      crop_pattern <- "int"
    }
    else if(ctype=="relay crop"){
      crop_pattern <- "rel"
    }
    else if(cytpe=="rotation"){
      crop_pattern <- "rot"
    }  
      #iExample: nt_harv_2_harvest_to_collect_field_1
      ha <- lapply(X = cropId, function(x) allinputs %>% 
                     dplyr::filter(str_detect(id,  paste0("^", paste0(crop_pattern,"_harv_",x) ,"_harvest_to_collect_field_1","$") ))%>% 
                     dplyr::nth(2)) 
      
      ha <- lapply(ha, function(x)stringi::stri_split_regex(x, ",")[[1]] )
      
      for( i in seq.int(crop)){
        ha[[i]] <- ha[[i]] %>% stringr::str_trim(side = "both") %>% setdiff("")
        ha[[i]] <- paste0(crop[i],"_",ha[[i]])
      }
      out <- unlist(ha)
      if(length(out)!=0){
        if(ver=="default"){
          out <- paste0("Harvest" ,"_", out) 
        }
        else if(ver=="export"){
          out <- out #ifelse(str_detect(string = out,pattern = "Harvestable|Harvest"), out, paste0("Harvest_",out))
        }
      }
  } 
 
  out 
}



