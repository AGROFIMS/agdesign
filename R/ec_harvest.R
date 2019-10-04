## Get managements practices for harvest ########################################################

get_ec_harv <- function(allinputs, input, ctype="monocrop", cropId="1", addId="1"){
  
  #allinputs<- readRDS("tests/testthat/userInput/harvest-tableIds-01.rds")
  #allinputs<- readRDS("tests/testthat/userInput/irri-table-id-all-tech-sel.rds.rds")
  #allinputs<- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/table_ids.rds")
  
  if(ctype=="monocrop"){
    #"monocrop_hahd_"
    lookup<- "monocrop"
    addId <- addId
    #monocrop_hahd_crop_component_harvested_1
  } else if(ctype=="intercrop"){
    lookup<- paste0("int_harv_",cropId)
    addId<- addId
    #int_harv_2_hahd_harvest_method_2
  } else if(ctype=="relay crop") {
    lookup<- paste0("rel_harv_",cropId) 
    addId<-addId
  }
  
  harv <- allinputs %>% dplyr::filter(!str_detect(id, "button")) %>%
    dplyr::filter(!str_detect(id, "-selectized")) %>%
    dplyr::filter(str_detect(id, paste0(lookup))) %>% 
    dplyr::filter(str_detect(id, paste0("hahd"))) 
    #dplyr::filter(!str_detect(id, paste0("unit")))
  
  harv<- arrange_by_pattern(harv, pattern = addId) 
  
  #for( i in 1:length(addId)){
    #harvest start date
    startD <- harv  %>% dplyr::filter(str_detect(id, paste0(lookup,"_hahd_harvest_start_date_[:digit:]+","$")))  %>% dplyr::distinct()
    #harvest end date
    endD <- harv  %>% dplyr::filter(str_detect(id, paste0(lookup,"_hahd_harvest_end_date_[:digit:]+","$")))  %>% dplyr::distinct()
    
    method <-   harv  %>% dplyr::filter(str_detect(id, paste0(lookup,"_hahd_harvest_method_[:digit:]+","$")))  %>% dplyr::distinct()
    method_other <-  harv  %>% dplyr::filter(str_detect(id, paste0(lookup,"_hahd_harvest_method_[:digit:]+","_other$")))  %>% dplyr::distinct()
    method <-  dt_inputs(method, method_other)
    #method <-  dt_inputs(method, method_other)
    
    comph <-  harv %>% dplyr::filter(str_detect(id,  paste0(lookup,"_hahd_crop_component_harvested_[:digit:]+","$")))  %>% dplyr::distinct()
    comph_other <-   harv %>% dplyr::filter(str_detect(id,paste0(lookup,"_hahd_crop_component_harvested_[:digit:]+","_other$")))  %>% dplyr::distinct()
    comph <- dt_inputs(comph, comph_other)
    #compht <- dt_inputs(comph, comph_other)
    
    #Harvest area
    ha_area <-  harv %>% dplyr::filter(str_detect(id,paste0(lookup,"_hahd_crop_harvestable_area_[:digit:]+","$")))  %>% dplyr::distinct()
    #Harvestable == Other
    ha_area_other <-  harv  %>% dplyr::filter(str_detect(id,  paste0(lookup,"_hahd_crop_harvestable_area_[:digit:]+_other","$")))  %>% dplyr::distinct()
    ha_area <- dt_inputs(ha_area ,ha_area_other)
    
    
    #Amount
    amount <-  harv  %>% dplyr::filter(str_detect(id,  paste0(lookup,"_hahd_amount_harvested_[:digit:]+","$")))  %>% dplyr::distinct()
    amount_unit <-  harv  %>% dplyr::filter(str_detect(id,  paste0(lookup,"_hahd_amount_harvested_unit_[:digit:]+","$")))  %>% dplyr::distinct()
    
    #Cut height
    cut<- harv  %>% dplyr::filter(str_detect(id,  paste0(lookup,"_hahd_harvest_cut_height_[:digit:]+","$")))  %>% dplyr::distinct()
    cut_unit <- harv  %>% dplyr::filter(str_detect(id,paste0(lookup,"_hahd_harvest_cut_height_unit_[:digit:]+","$")))  %>% dplyr::distinct()
    
    #Harvestable == area m2
    ha_m2 <- harv  %>% dplyr::filter(str_detect(id,  paste0(lookup,"_hahd_crop_component_harvested_m2_[:digit:]+","$")))  %>% dplyr::distinct()
    
    #Harvestable == Indiviual plants
    ha_ip <- harv  %>% dplyr::filter(str_detect(id, paste0(lookup,"_hahd_crop_component_harvested_ip_[:digit:]+","$")))  %>% dplyr::distinct()
  
    #Harvestable == Rows
    ha_row_num <- harv  %>% dplyr::filter(str_detect(id, paste0(lookup,"_hahd_crop_component_harvested_num_[:digit:]+","$")))  %>% dplyr::distinct()
    
    ha_row_len <- harv  %>% dplyr::filter(str_detect(id,  paste0(lookup,"_hahd_crop_component_harvested_len_[:digit:]+","$")))  %>% dplyr::distinct()
    ha_row_len_unit <- harv  %>% dplyr::filter(str_detect(id, paste0("hahd_crop_component_harvested_lenunit_[:digit:]+","$")))  %>% dplyr::distinct()
    
    ha_row_width <- harv  %>% dplyr::filter(str_detect(id,  paste0(lookup,"_hahd_crop_component_harvested_width_[:digit:]+","$")))  %>% dplyr::distinct()
    ha_row_width_unit <- harv  %>% dplyr::filter(str_detect(id,  paste0(lookup,"_hahd_crop_component_harvested_widthunit_[:digit:]+","$")))  %>% dplyr::distinct()
    
    ha_row_space<- harv  %>% dplyr::filter(str_detect(id,  paste0(lookup,"_hahd_space_rows_harvested_[:digit:]+","$")))  %>% dplyr::distinct()
    ha_row_space_unit <- harv  %>% dplyr::filter(str_detect(id,  paste0(lookup,"_hahd_crop_component_harvested_spaceunit_[:digit:]+","$")))  %>% dplyr::distinct()
    
    ha_row_plot_entire <- harv  %>% dplyr::filter(str_detect(id,  paste0(lookup,"_hahd_crop_component_harvested_entire_[:digit:]+","$")))  %>% dplyr::distinct()
    ha_row_plot_entire_unit<- harv  %>% dplyr::filter(str_detect(id,  paste0(lookup,"_hahd_crop_component_harvested_entireunit_[:digit:]+","$")))  %>% dplyr::distinct()
    
     
    #notes
    notes<- harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_harvest_notes_[:digit:]+","$")))  %>% dplyr::distinct()
    
    #implement
    type <- harv %>% dplyr::filter(str_detect(id,  paste0(lookup,"_hahd_harvest_implement_[:digit:]+","$")))  %>% dplyr::distinct()
    type_other <- harv  %>% dplyr::filter(str_detect(id,  paste0(lookup,"_hahd_harvest_implement_[:digit:]+","_other$"))) %>% dplyr::distinct()
    type <- dt_inputs(type, type_other)
    
    #traction
    traction <- harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_harvest_traction_[:digit:]+","$")))  %>% dplyr::distinct()
    traction_other <- harv  %>% filter(str_detect(id,  paste0(lookup,"_hahd_harvest_traction_[:digit:]+","_other$")))  %>% dplyr::distinct()
    traction <- dt_inputs(traction, traction_other)
    
   dt<- rbind(startD, endD, method,comph,
          ha_area, 
          ha_m2,
          ha_ip,
          ha_row_num, ha_row_len, ha_row_width, ha_row_space ,
          ha_row_plot_entire,
          #ha_other,
          amount, cut,
          type, traction, notes )  
    
  #Create labels
  lbl_start <- paste("Harvest_start_date",seq.int(addId),sep = "__")
  lbl_end <- paste("Harvest_end_date",seq.int(addId),sep = "__")
  lbl_method <-  paste("Harvest_method",seq.int(addId),sep = "__")
  #lbl_comph <- paste("Harvest_crop_component_harvested",seq.int(addId),sep = "__") #deprectaed
  lbl_comph <- paste("Crop_component_harvested",seq.int(addId),sep = "__")
  
  #SPECIAL CASE#######################
  lbl_ha_area <- ha_area$values #
  lbl_ha_area <-  dplyr::case_when( lbl_ha_area %in% c("Individual plants", "Entire plot") ~ "Harvestable_area", TRUE ~ "Harvestable_area_other")
  lbl_ha_area <- paste(lbl_ha_area, seq.int(ha_area$values),sep = "__")
  #lbl_ha_area <-  paste("Harvestable_area", seq.int(addId),sep = "__")
  #####################################
  #Harvest_implement_type 
  #ha_m2
  lbl_ha_area_m2 <- paste("Number_of_m2_units_harvested",seq.int(addId),sep="__") 
  #ha_ip
  lbl_ha_area_ip<-  paste("Number_of_individual_plants_harvested",seq.int(addId),sep="__")
  # ha_row_num   # ha_row_len   # ha_row_width   # ha_row_space   # 
  lbl_ha_area_num <- paste("Number_of_rows_harvested", seq.int(addId) ,sep="__")
  lbl_ha_rows <-   paste( paste("Length_of_rows_harvested",ha_row_len_unit$values,  sep="_"),  seq.int(addId),sep = "__")
  lbl_ha_width <-  paste(paste("Width_within_rows_harvested", ha_row_width_unit$values, sep="_"),  seq.int(addId), sep = "__")
  lbl_ha_area_space<- paste(paste("Space_between_rows_harvested",ha_row_space_unit$values, sep="_"),  seq.int(addId), sep = "__")
  #ha_row_plot_area
  lbl_ha_area_entire <- paste(paste("Entire_plot_area_harvested",ha_row_plot_entire_unit$values,sep="_"),  seq.int(addId),sep = "__")
  #ha_other
  lbl_amount <- paste(paste("Amount_harvested", amount_unit$values, sep="_"),  seq.int(addId),sep = "__")
  lbl_cut <- paste(paste("Harvest_cut_height", cut_unit$values, sep="_"),  seq.int(addId),sep = "__")
  lbl_type<- paste("Harvest_implement_type", seq.int(addId), sep = "__")
  lbl_traction<- paste("Harvest_traction_type", seq.int(addId), sep = "__")
  lbl_notes<- paste("Harvest_notes", seq.int(addId),sep = "__")
  
  lbl_harv <- c(lbl_start, lbl_end, lbl_method, lbl_comph, lbl_ha_area, 
                lbl_ha_area_m2,
                lbl_ha_area_ip,
                lbl_ha_area_num,  lbl_ha_rows , lbl_ha_width, lbl_ha_area_space,
                lbl_ha_area_entire,
                lbl_amount, lbl_cut, lbl_type, lbl_traction, lbl_notes)
  
  dt$id <- lbl_harv
  
  dt <- dt %>%
    mutate(id= case_when( #Harvestable_area__1
      id == stringr::str_detect(id,"Harvestable_area") & !(values %in% c("Individual plants", "Entire plot")) ~ stringr::str_replace_all(id, "Harvestable_area","Harvestable_area_other"),
      TRUE                      ~ id)
    )
  
  
  #transpose data as rows  and create table 
  dt_harv<- t(dt$values) %>% as.data.frame(stringAsFactors=FALSE)
  names(dt_harv) <- dt$id #changes names
  
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
  #names(out) <- stringr::str_replace_all(names(out),"__1","")
  out <- t(out) %>% as.data.frame(stringsAsFactors=FALSE) %>% tibble::rownames_to_column()
  out <- out %>% dplyr::filter(V1!="") %>% dplyr::filter(!stringr::str_detect(V1, "^NA$"))
  
  #Create a column to store temporally TraitName
  out$TraitProt <- out$rowname
  #Remove numbers from traitnames
  out$rowname <- stringr::str_replace_all(out$rowname,"__[:digit:]+","")
  names(out) <- c("TraitName","Value","TraitProt")
  out <- out 
  }else {
    out<- data.frame()
  }
  out
  
}


# Get Collectable inputs for Harvest ###########################################################
# Output: Monocrop: vector of collectable inputs 
# Ouput : Multicrop: vector of collectable inputs concatenated with the respectively crop (ex. Cassava_Start_date, Maize_Start_date) 
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



