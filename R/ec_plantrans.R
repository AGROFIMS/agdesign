#allinputs<- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/table_ids.rds")
#input<- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/inputs.rds")

# Get management practices for planting and transplanting experiments ########################
get_ec_plantrans <- function(allinputs, input, ctype="monocrop", cropId, addId="1", ver = "default"){
  
  #allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/table_ids.rds")
  #input<- readRDS("/home/obenites/AGROFIMS/agdesign/inst/inputs.rds")
  
  if(ctype=="monocrop"){
    lookup<- ctype 
    cropId <- "1"
  } 
  else if(ctype=="intercrop"){
    lookup<- paste0("int_pt_",cropId)
  } 
  else if(ctype=="relay crop") {
    lookup<- paste0("rel_pt_",cropId) 
  }
  
  #Direct seeding
  if(isTRUE(input[[paste0(lookup,"_directSeeding_checkbox_",addId)]])){
    
    #direct seedling -------------------------------------------------------------
    ptdi <- allinputs %>%  dplyr::filter(!str_detect(id, "button")) %>%
                            dplyr::filter(!str_detect(id, "-selectized")) %>%
                            dplyr::filter(str_detect(id, paste0(lookup,"_ptdi")))
    
    #start date
    startD <- ptdi %>% filter(str_detect(id,  paste0(lookup,"_ptdi_planting_start_date_[:digit:]+$")  ) )
    
    #environment
    env <- ptdi %>% dplyr::filter(str_detect(id,  paste0(lookup,"_ptdi_seeding_environment_[:digit:]+$" )))
    env_other <- ptdi %>% dplyr::filter(str_detect(id,  paste0(lookup,"_ptdi_seeding_environment_[:digit:]+_other$")))
    env <- dt_inputs(env, env_other)
    
    #seeding technique
    tech<- ptdi %>% dplyr::filter(str_detect(id,  paste0(lookup,"_ptdi_seeding_technique_[:digit:]+$" )))
    #<- ptdi %>% filter(str_detect(id,  "ptdi_seeding_technique_[:digit:]+_other"))
    
    #treatment
    treat <- ptdi %>% filter(str_detect(id,  paste0(lookup,"_ptdi_seed_treatment_[:digit:]+$"  )))
    
    #implement type
    imp_type<- ptdi %>% dplyr::filter(str_detect(id,  paste0(lookup,"_ptdi_seeding_implement_type_[:digit:]+$")))
    imp_type_other <- ptdi %>% dplyr::filter(str_detect(id,  paste0(lookup,"_ptdi_seeding_implement_type_[:digit:]+_other$")))
    imp_type <- dt_inputs(imp_type, imp_type_other) 
    
    #implement traction
    imp_trac <- ptdi %>% filter(str_detect(id, paste0(lookup, "_ptdi_seeding_traction_[:digit:]+$")))
    imp_trac_other<- ptdi %>% filter(str_detect(id,  paste0(lookup,"_ptdi_seeding_traction_[:digit:]+_other")))
    imp_trac <- dt_inputs(imp_trac, imp_trac_other)
    
    #distance rows
    row<- ptdi %>% dplyr::filter(str_detect(id,  paste0(lookup, "_ptdi_distance_rows_[:digit:]+$"  ) ))
    row_unit <- ptdi %>% dplyr::filter(str_detect(id,  paste0(lookup, "_ptdi_distance_rows_unit_[:digit:]+$" )))
    
    #seeding rate
    rate <- ptdi %>% dplyr::filter(str_detect(id,  paste0(lookup,"_ptdi_seeding_rate_[:digit:]$")))
    rate_unit <- ptdi %>% dplyr::filter(str_detect(id, paste0(lookup,"_ptdi_seeding_rate_unit_[:digit:]+$" )))
    
    #distance plants
    plan<- ptdi %>% dplyr::filter(str_detect(id,  paste0(lookup, "_ptdi_distance_plants_[:digit:]+$"   ))  )
    plan_unit<- ptdi %>% dplyr::filter(str_detect(id,   paste0(lookup,"_ptdi_distance_plants_unit_[:digit:]+$")))
    
    #number of rows
    numrow <- ptdi %>% dplyr::filter(str_detect(id, paste0(lookup, "_ptdi_seeding_density_number_rows_[:digit:]+$")))
    
    #plan density
    plden<- ptdi %>% dplyr::filter(str_detect(id,  paste0(lookup,"_ptdi_seeding_plant_density_[:digit:]+$")))
    plden_unit <- ptdi %>% dplyr::filter(str_detect(id,  paste0(lookup,"_ptdi_seeding_plant_density_unit_[:digit:]+$"  )))  
    
    #bund
    bund<- ptdi %>% dplyr::filter(str_detect(id, paste0(lookup, "_ptdi_seeding_distance_bunds_[:digit:]+$")))
    bund_unit<- ptdi %>% dplyr::filter(str_detect(id, paste0(lookup, "_ptdi_seeding_distance_bunds_unit_[:digit:]+$" )))
    
    #notes
    notes <- ptdi %>% filter(str_detect(id, paste0(lookup, "_ptdi_direct_seeding_notes_[:digit:]+$")))
    
    dt_di <- rbind(startD, env, tech, treat,  imp_type, 
                   imp_trac, row, rate, plan, numrow, plden, bund, notes)
    
    # lbl_di <- c("Direct_seeding_start_date", "Direct_seeding_environment",
    #             "Direct_seeding_technique",
    #             "Direct_seeding_seed_treatment", "Direct_seeding_implement_type", 
    #             "Direct_seeding_implement_traction" ,  
    #             paste("Direct_seeding_distance_between_rows","_",row_unit$values,sep=""),
    #             paste("Direct_seeding_seed_rate","_",rate_unit$values,sep=""),  
    #             paste("Direct_seeding_distance_between_plants","_",plan_unit$values,sep=""),
    #             "Direct_seeding_number_of_rows", 
    #             paste("Direct_seeding_plant_density","_",plden_unit$values,sep=""),
    #             paste("Direct_seeding_distance_between_bunds","_",bund_unit$values,sep=""),
    #             "Direct_seeding_notes"
    # )
    lbl_di <- c("Seeding_start_date", "Seeding_environment",
                "Seeding_technique",
                "Seeding_seed_treatment", "Seeding_implement_type", 
                "Seeding_implement_traction" ,  
                paste("Seeding_density_distance_between_rows","_",row_unit$values,sep=""),
                paste("Seeding_seed_rate","_",rate_unit$values,sep=""),  
                paste("Seeding_density_distance_between_plants","/",plan_unit$values,sep=""),
                "Seeding_density_number_of_rows",  ##Check with Celine #74  Direct seeding
                paste("Seeding_plant_density","_",plden_unit$values,sep=""),
                paste("Seeding_density_distance_between_bunds","_",bund_unit$values,sep=""),
                "Seeding_notes"
    )
    
    
    lbl_di <-   lbl_dt <- paste(lbl_di, rep("1", length(lbl_di)) ,sep="__") 
    
    
    #TODO: AGREGAR END DATE "Planting_direct_seeding_end_date"
    
    dt_di <- t(dt_di$values) %>% as.data.frame(stringAsFactors=FALSE)
    names(dt_di) <- lbl_di
  }
  else {
    dt_di<- data.frame()
  }
  
  #Transplating  
  if(isTRUE(input[[paste0(lookup,"_transplanting_checkbox_",addId)]])){
    
    #planting transplanting ----------------------------------------------------
    ptta <- allinputs %>%  filter(!str_detect(id, "button")) %>%
      filter(!str_detect(id, "-selectized")) %>%
      filter(str_detect(id, paste0(lookup, "_ptta")))
    
    #Start and End date
    ta_startD<- ptta %>% filter(str_detect(id,  paste0(lookup,"_ptta_transplanting_start_date_[:digit:]+$")))
    ta_endD <- ptta %>% filter(str_detect(id, paste0(lookup,"_ptta_transplanting_end_date_[:digit:]+$")))
    
    #Age
    ta_age <- ptta %>% filter(str_detect(id, paste0(lookup,"_ptta_age_seedling_[:digit:]+$")))
    
    #Environment
    ta_env <- ptta %>% filter(str_detect(id, paste0(lookup,"_ptta_transplanting_environment_[:digit:]+$")))
    ta_env_other <- ptta %>% filter(str_detect(id, paste0(lookup,"_ptta_transplanting_environment_[:digit:]+_other$")))
    ta_env <- dt_inputs(ta_env, ta_env_other)
    
    #Technique
    ta_tech <- ptta %>% filter(str_detect(id, paste0(lookup,"_ptta_transplanting_technique_[:digit:]+$")))
    ta_tech_other<- ptta %>% filter(str_detect(id, paste0(lookup,"_ptta_transplanting_technique_[:digit:]+_other$")))
    ta_tech <- dt_inputs(ta_tech,ta_tech_other)
    
    #Treatment
    ta_treat <- ptta %>% filter(str_detect(id,paste0(lookup,"_ptta_transplanting_treatment_[:digit:]+$")))
    
    #Traction
    ta_trac <- ptta %>% filter(str_detect(id, paste0(lookup,"_ptta_trans_traction_[:digit:]+$")))
    ta_trac_other <-ptta %>% filter(str_detect(id,paste0(lookup,"_ptta_trans_traction_[:digit:]+_other$")))
    ta_trac <- dt_inputs(ta_trac, ta_trac_other)
    
    #distance between rows
    ta_drow<- ptta %>% filter(str_detect(id,paste0(lookup,"_ptta_trans_distance_rows_[:digit:]+$")))
    ta_drow_unit<- ptta %>% filter(str_detect(id,paste0(lookup,"_ptta_trans_distance_rows_unit_[:digit:]+$")))
    
    #Seeding density
    ta_sden <- ptta %>% filter(str_detect(id,paste0(lookup,"_ptta_trans_seeding_density_[:digit:]+$")))
    ta_sden_unit <-ptta %>% filter(str_detect(id,paste0(lookup,"_ptta_trans_seeding_density_unit_[:digit:]+$")))
    
    #Number of rows
    ta_numrow <- ptta %>% filter(str_detect(id,paste0(lookup,"_ptta_trans_num_rows_[:digit:]+$")))
    
    #distance plants
    ta_dplan<- ptta %>% filter(str_detect(id, paste0(lookup,"_ptta_trans_distance_plants_[:digit:]+$")))
    ta_dplan_unit <- ptta %>% filter(str_detect(id, paste0(lookup,"_ptta_trans_distance_plants_unit_[:digit:]+$")))
    
    #distance bunds
    ta_bunds<- ptta %>% filter(str_detect(id, paste0(lookup,"_ptta_trans_distance_bunds_[:digit:]+$")))
    ta_bunds_unit<- ptta %>% filter(str_detect(id,paste0(lookup,"_ptta_trans_distance_bunds_unit_[:digit:]+$")))
    
    #Notes
    ta_notes<- ptta %>% filter(str_detect(id, paste0(lookup,"_ptta_transplanting_density_notes_[:digit:]+$")))
    
    dt_ta<- rbind(ta_startD,ta_endD, ta_age, ta_env, ta_tech, ta_treat, ta_trac, 
                  ta_drow, ta_sden, ta_numrow, ta_dplan, ta_bunds, ta_notes)
    
    # lbl_ta <- c("Transplanting_start_date", "Transplanting_end_date",
    #             "Transplanting_age_of_seedling_(days)","Transplanting_seedling_environment",
    #             "Transplanting_technique","Transplanting_treatment",
    #             "Transplanting_traction",
    #             paste0("Transplanting_distance_between_rows","_",ta_drow_unit$values,sep=""),
    #             paste0("Transplanting_seedling_density","_",ta_sden_unit$values,sep=""),
    #             "Transplanting_number_of_rows",
    #             paste0("Transplanting_distance_between_plants","_",ta_dplan_unit$values,sep=""),
    #             paste0("Transplanting_distance_between_bunds","_",ta_bunds_unit$values,sep=""),
    #             "Transplanting_notes")      
    
    lbl_ta <- c("Transplanting_start_date", "Transplanting_end_date",
                "Transplanting_age_of_seedling","Transplanting_seedling_environment",
                "Transplanting_technique","Transplanting_seed_treatment",
                "Transplanting_traction",
                paste0("Transplanting_distance_between_rows","_",ta_drow_unit$values,sep=""),
                paste0("Transplanting_seedling_density","_",ta_sden_unit$values,sep=""),
                "Transplanting_density_number_of_rows", ##Check with Celine #87 Transplanting
                paste0("Transplanting_density_distance_between_plants","_",ta_dplan_unit$values,sep=""),
                paste0("Transplanting_density_distance between_bunds","_",ta_bunds_unit$values,sep=""),
                "Transplanting_notes") 
    
    
    lbl_ta <-   lbl_ta <- paste(lbl_ta, rep("1", length(lbl_ta)) ,sep="__") 
    
    
    dt_ta <- t(dt_ta$values) %>% as.data.frame(stringAsFactors=FALSE)
    names(dt_ta) <- lbl_ta
  }
  else{
    dt_ta <- data.frame()
  }
  
  #dt_plantrans <- cbind(dt_di, dt_ta)
  dt_plantrans<- smart_colbind(dt_di,dt_ta)
  
  #LABEL FOR TRAITLIST
  lbl<- str_replace_all(string = names(dt_plantrans), pattern = "__[:digit:]+$", replacement = "") %>% unique()
  
  out <- list(dt= dt_plantrans, lbl=lbl)
  
  
}


# Get protocol for planting and transplanting experiments ###################################

get_protocol_plantrans <- function(allinputs, input, ctype="monocrop", cropId, addId="1"){
  
  out<- get_ec_plantrans(allinputs, input, ctype= ctype, cropId=cropId, addId= addId)$dt
  if(nrow(out)!=0){
    names(out) <- stringr::str_replace_all(names(out),"__1","")
    out <- t(out) %>% as.data.frame(stringsAsFactors=FALSE) %>% tibble::rownames_to_column()
    out <- out %>% dplyr::filter(V1!="") %>% dplyr::filter(!stringr::str_detect(V1, "^NA$"))
    names(out) <- c("TraitName","Value")
    out <- out
  }else {
    out <- data.frame()
  }
 
  
}


# Get Collectable inputs from planting and transplanting  ###################################

#allinputs: data.frame with all the mapped inputs
#cytpe: cropping type
#cropdid: vector ids
#ver: default(Group) or export
get_collectable_plantrans <- function(allinputs, ctype="monocrop",crop, cropId="1", ver="default"){
  
  if(ctype=="monocrop"){
    lookup<- ctype
    colltup <- "_directSeeding_to_collect_field_1"
    cropId <- "1"
    #direct seeding
    ds <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^", lookup,"_directSeeding_to_collect_field_1","$") )) %>% dplyr::nth(2)
    ds <- stringi::stri_split_regex(ds, ",")[[1]] %>% stringr::str_trim(side = "both") %>% setdiff("")   
    if(length(ds)!=0){
      if(ver=="default"){
        ds <- paste0("Direct seeding" ,"_", ds)  
      }
      else if(ver=="export"){
        ds <- ifelse(str_detect(string = ds,pattern = "Seeding" ), ds, paste0("Seeding_",ds))
      }
    } else {
      ds <-NULL
    }
    
    #transplanting
    tra <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^", lookup,"_transplanting_to_collect_field_1","$") )) %>% dplyr::nth(2)
    tra <- stringi::stri_split_regex(tra, ",")[[1]]  %>% stringr::str_trim(side = "both") %>% setdiff("")
    if(length(tra)!=0){
      tra <- paste0("Transplanting" ,"_", tra)
    } else {
      tra <- NULL
    }
    out <- c(ds,tra)  
  }
  else {
    if(ctype=="intercrop"){
      crop_pattern <- "int"
    }else if(ctype=="relay crop"){
      crop_pattern <- "rel"
    }else if(cytpe=="rotation"){
      crop_pattern <- "rot"
    }  
      #direct seeding --------------------------------------------------------------------------------------------------------------------------
      ds <- lapply(X = cropId, function(x) allinputs %>% dplyr::filter(str_detect(id,  paste0("^", paste0(crop_pattern,"_pt_",x) ,"_directSeeding_to_collect_field_1","$") ))
                   %>% dplyr::nth(2) ) 
      #ds is a list
      if(all(unlist(ds)!="")){
      
        ds <- lapply(ds, function(x)stringi::stri_split_regex(x, ",")[[1]] )
        
        for( i in seq.int(crop)){
          ds[[i]] <- ds[[i]] %>% stringr::str_trim(side = "both") %>% setdiff("")
          ds[[i]] <- paste0(crop[i],"_",ds[[i]])  
        }
        ds <- unlist(ds)
        if(length(ds)!=0){
          
          if(ver=="default"){
            ds <- paste0("Direct seeding" ,"_", ds)  
          }
          else if(ver=="export"){
            ds <- ifelse(str_detect(string = ds,pattern = "Seeding" ), ds, paste0("Seeding_",ds))
            ds <- ifelse(str_detect(string = ds, pattern = crop), 
                         stringr::str_replace(string = ds, pattern = paste0(crop,"_"),replacement = "") , ds) 
          }
          
          
          #ds <- paste0("Direct seeding" ,"_", ds)
        }
          
      }
      else {
        ds <-NULL
      }
      
      
      #transplanting --------------------------------------------------------------------------------------------------------------------------
      tra <- lapply(X = cropId, function(x) allinputs %>% dplyr::filter(str_detect(id,  paste0("^",  paste0(crop_pattern,"_pt_",x) ,"_transplanting_to_collect_field_1","$") ))
                    %>% dplyr::nth(2))
      #tra is a list
      
      if(all(unlist(tra)!="")){
        tra <- lapply(tra, function(x)stringi::stri_split_regex(x, ",")[[1]] )
        
        for( i in seq.int(crop)){
          tra[[i]] <- tra[[i]] %>% stringr::str_trim(side = "both") %>% setdiff("")
          tra[[i]] <- paste0(crop[i],"_",tra[[i]])
        }
        tra <- unlist(tra)
        if(length(tra)!=0){
          
          
          if(ver=="default"){
            tra <- paste0("Transplanting" ,"_", tra)
          }
          else if(ver=="export"){
            #tra <- tra
            tra <- ifelse(str_detect(string = tra, pattern = crop), 
                         stringr::str_replace(string = tra, pattern = paste0(crop,"_"),replacement = "") , tra) 
          }
          
        }  
        
      } else {
        tra <- NULL
      }
      
      
      #final ouput
      out <- c(ds, tra) 
    } 

  out    ###  Group,"_",Crop,"_",Measurement  #######33
}

