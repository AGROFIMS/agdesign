get_ec_harv <- function(allinputs, addId){
  
  #allinputs<- readRDS("tests/testthat/userInput/harvest-tableIds-01.rds")
  #allinputs<- readRDS("tests/testthat/userInput/irri-table-id-all-tech-sel.rds.rds")
  #allinputs<- readRDS("/home/obenites/AGROFIMS/agdesign/inst/table_ids.rds")
  harv <- allinputs %>%  
          filter(!str_detect(id, "button")) %>%
          filter(!str_detect(id, "-selectized")) %>%
          filter(str_detect(id,"hahd"))
  
  #addId<- str_extract_all(harv$id, "[:uppercase:]{8}") %>% unlist() %>% unique()
  
  #harvest start date
  startD <- harv  %>% filter(str_detect(id,  "hahd_harvest_start_date_[:uppercase:]{8}$"))
  #harvest end date
  endD <- harv  %>% filter(str_detect(id,  "hahd_harvest_end_date_[:uppercase:]{8}$"))
  
  method <- harv  %>% filter(str_detect(id,  "hahd_harvest_method_[:uppercase:]{8}$"))
  method_other <- harv  %>% filter(str_detect(id,  "hahd_harvest_method_[:uppercase:]{8}_other$"))
  method <-  dt_inputs(method, method_other)
  
  comph <- harv  %>% filter(str_detect(id,  "hahd_crop_component_harvested_[:uppercase:]{8}$"))
  comph_other <- harv  %>% filter(str_detect(id,  "hahd_crop_component_harvested_[:uppercase:]{8}_other$"))
  comph<- dt_inputs(comph, comph_other)
  
  #Harvest area
  ha_area <- harv  %>% filter(str_detect(id,  "hahd_crop_harvestable_area_[:uppercase:]{8}$"))
  #lbl_ha_area <- "Harvest area"
  
  ###SPECIAL CASE FOR SELECTING HARVEST AREA
  values_ha_area_sp <- lbl_ha_area_sp <- lbl_ha_area_num <- NULL
  #Harvesta area : m2 units
  for(i in 1:length(addId)){
    
    if(ha_area$values[i]==""){
      values_ha_area_sp <- append(values_ha_area_sp, "")
      lbl_ha_area_sp <-  append(lbl_ha_area_sp, paste("Harvest_area",addId[i],sep="_"))
      #label with numbers
      lbl_ha_area_num <- append(lbl_ha_area_num, paste("Harvest_area",i,sep="__"))
      
    } else if(ha_area$values[i]=="m2 units"){
      #input$hahd_crop_component_harvested_m2_[:uppercase:]{8}
      ha_m2 <- harv  %>% filter(str_detect(id, paste0("hahd_crop_component_harvested_m2_",addId[i],"$")))
      values_ha_area_sp <- append(values_ha_area_sp, ha_m2$values)
      lbl_ha_area_sp <-  append(lbl_ha_area_sp, paste("Harvest_number_of_m2_units_harvested",addId[i],sep="_"))
      #label with numbers
      lbl_ha_area_num <-  append(lbl_ha_area_num, paste("Harvest_number_of_m2_units_harvested",i, sep="__"))
      
    } else if(ha_area$values[i]=="Individual plants"){
      #Harvesta area : individual plants
      ha_ip <- harv  %>% filter(str_detect(id, paste0("hahd_crop_component_harvested_ip_",addId[i],"$")))
      values_ha_area_sp<- append(values_ha_area_sp, ha_ip$values)
      lbl_ha_area_sp<-  append(lbl_ha_area_sp,paste("Harvest_number_of_plants_harvested",addId[i],sep="_"))
      #label with numbers
      lbl_ha_area_num <-  append(lbl_ha_area_num,paste("Harvest_number_of_plants_harvested", i, sep="__"))
      
      
    } else if(ha_area$values[i]=="Rows"){
      #Harvesta area : rows---------------------------------------------------------
      ha_row_num <- harv  %>% filter(str_detect(id, paste0("hahd_crop_component_harvested_num_",addId[i],"$")))
      values_ha_area_sp <- append(values_ha_area_sp, ha_row_num$values)
      lbl_ha_area_sp <-  append(lbl_ha_area_sp, paste("Harvest_number_of_plants_harvested",addId[i],sep="_"))
      #label with numbers
      lbl_ha_area_num <-  append(lbl_ha_area_num, paste("Harvest_number_of_plants_harvested", i ,sep="__"))
      
      
      #rows length
      ha_row_len <- harv  %>% filter(str_detect(id, paste0("hahd_crop_component_harvested_len_",addId[i],"$")))
      values_ha_area_sp <- append(values_ha_area_sp, ha_row_len$values)
      ha_row_len_unit <- harv  %>% filter(str_detect(id, paste0("hahd_crop_component_harvested_lenunit_",addId[i],"$")))
      lbl_ha_area_sp <-  append(lbl_ha_area_sp, paste("Harvest_Length_of_rows_harvested",ha_row_len_unit$values, addId[i], sep="_"))
      #numeric labels
      lbl_ha_area_num <-  append(lbl_ha_area_num, paste(paste("Harvest_Length_of_rows_harvested", ha_row_len_unit$values, sep="_"), i, sep="__"))
      
      
      #rows width
      ha_row_width <- harv  %>% filter(str_detect(id, paste0("hahd_crop_component_harvested_width_",addId[i],"$")))
      values_ha_area_sp <- append(values_ha_area_sp, ha_row_width$values)
      ha_row_width_unit <- harv  %>% filter(str_detect(id, paste0("hahd_crop_component_harvested_widthunit_",addId[i],"$")))
      lbl_ha_area_sp <-  append(lbl_ha_area_sp,paste("Harvest_Width_within_rows_harvested", ha_row_width_unit$values, addId[i], sep="_"))
      #numeric labels
      lbl_ha_area_num <-  append(lbl_ha_area_num, paste(paste("Harvest_Width_within_rows_harvested", ha_row_width_unit$values, sep="_"),i, sep="__"))
      
      
      #rows space
      ha_row_space<- harv  %>% filter(str_detect(id, paste0("hahd_space_rows_harvested_",addId[i],"$")))
      values_ha_area_sp <- append(values_ha_area_sp, ha_row_space$values)
      ha_row_space_unit <- harv  %>% filter(str_detect(id, paste0("hahd_crop_component_harvested_spaceunit_",addId[i],"$"))) 
      lbl_ha_area_sp<-  append(lbl_ha_area_sp, paste("Harvest_Space_between_rows_harvested",ha_row_space_unit$values,addId[i],sep="_"))
      #numeric labels
      lbl_ha_area_num<-  append(lbl_ha_area_num, paste(paste("Harvest_Space_between_rows_harvested",ha_row_space_unit$values,sep="_"),i, sep="__"))     
      
      
    } else if(ha_area$values[i]=="Entire plot"){
      #Harvesta area : entire plot
      ha_row_plot_area <- harv  %>% filter(str_detect(id, paste0("hahd_crop_component_harvested_entire_",addId[i],"$"))) 
      values_ha_area_sp <- append(values_ha_area_sp, ha_row_plot_area$values)
      ha_row_plot_area_unit<- harv  %>% filter(str_detect(id, paste0("hahd_crop_component_harvested_entireunit_",addId[i],"$"))) 
      lbl_ha_area_sp <- append(lbl_ha_area_sp,paste("Harvest_plot_area_harvested",ha_row_plot_area_unit$values,addId[i],sep="_"))
      #numeric labels
      lbl_ha_area_num <- append(lbl_ha_area_num, paste(paste("Harvest_plot_area_harvested",ha_row_plot_area_unit$values,sep="_"),i,sep="__"))
      
    } else if(ha_area$values[i]=="Other"){
      #Harvesta area : other 
      ha_area_other <- harv  %>% filter(str_detect(id, paste0("hahd_crop_harvestable_area_",addId[i],"_other$")))
      values_ha_area_sp <- append(values_ha_area_sp, ha_area_other$values)
      lbl_ha_area_sp<-  append(lbl_ha_area_sp,paste("Harvest_area", addId[i],sep="_"))
      #numeric labels
      lbl_ha_area_num<-  append(lbl_ha_area_num, paste("Harvest_area", i, sep="__"))
      
    }
  }
  
  ha_area_sp <- data.frame(id=lbl_ha_area_sp, values = values_ha_area_sp,stringsAsFactors = FALSE)
  
  ##############END ESPECIAL CASE HARVEST AREA 
  
  #Amount
  amount <- harv  %>% filter(str_detect(id, "hahd_amount_harvested_[:uppercase:]{8}$"))
  amount_unit <-  harv  %>% filter(str_detect(id, "hahd_amount_harvested_unit_[:uppercase:]{8}$"))
  
  cut<- harv  %>% filter(str_detect(id, "hahd_harvest_cut_height_[:uppercase:]{8}$"))
  cut_unit <- harv  %>% filter(str_detect(id, "hahd_harvest_cut_height_unit_[:uppercase:]{8}$"))
  
  #implement
  implement<- harv  %>% filter(str_detect(id, "hahd_harvest_implement_[:uppercase:]{8}$"))
  implement_other <- harv  %>% filter(str_detect(id, "hahd_harvest_implement_[:uppercase:]{8}_other$"))
  implement <- dt_inputs(implement, implement_other)
  
  #traction
  traction <- harv  %>% filter(str_detect(id, "hahd_harvest_traction_[:uppercase:]{8}$"))
  traction_other <-  harv  %>% filter(str_detect(id, "hahd_harvest_traction_[:uppercase:]{8}_other$"))
  traction <- dt_inputs(traction, traction_other)
    
  notes<- harv  %>% filter(str_detect(id, "hahd_harvest_notes_[:uppercase:]{8}$"))
  
  #Bind tables
  dt<- rbind(startD, endD, method,comph, ha_area, ha_area_sp, amount, cut, notes )
  dt<- arrange_by_pattern(dt, addId)
  
  #Create labels
  lbl_start <- paste("Harvest_start_date",1:length(addId),sep = "__")
  lbl_end <- paste("Harvest_end_date",1:length(addId),sep = "__")
  lbl_method <-  paste("Harvest_method",1:length(addId),sep = "__")
  lbl_comph <- paste("Harvest_crop_component_harvested",1:length(addId),sep = "__")
  lbl_ha_area <-  paste("Harvest_harvestable_area", 1:length(addId),sep = "__")
  
  lbl_cut <- paste(paste("Harvest_amount", cut_unit$values, sep="_"),  1:length(addId),sep = "__")
  lbl_amount <- paste(paste("Harvest_amount", amount_unit$values, sep="_"),  1:length(addId),sep = "__")
  lbl_notes<- paste("Harvest_notes", 1:length(addId),sep = "__")
  
  lbl_harv <- c(lbl_start, lbl_end, lbl_method, lbl_comph, lbl_ha_area,
                lbl_ha_area_num, lbl_cut, lbl_amount, lbl_notes)
  
  
  #Special case:
  #Swichtching id values by irrigation labels (lbl_irr)
  dt$id <- lbl_harv
  
  #Remove NoLabel or NonData rows
  dt <- arrange_by_pattern(dt, as.character(1:length(addId)))
  
  #transpose data as rows   
  dt_harv<- t(dt$values) %>% as.data.frame(stringAsFactors=FALSE)
  names(dt_harv) <- dt$id
  dt_harv
  
}
