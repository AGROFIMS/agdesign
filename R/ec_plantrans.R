
get_ec_plantrans <- function(allinputs, addId, input){
  
  
  #allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/table_ids.rds")
  #input<- readRDS("/home/obenites/AGROFIMS/agdesign/inst/inputs.rds")
  
  #Direct seeding
  if(isTRUE(input$directSeeding_checkbox_1)){
    
  #direct seedling -------------------------------------------------------------
  ptdi <- allinputs %>%  filter(!str_detect(id, "button")) %>%
          filter(!str_detect(id, "-selectized")) %>%
          filter(str_detect(id,"ptdi"))
  
  #start date
  startD <- ptdi %>% filter(str_detect(id, "ptdi_planting_start_date_[:digit:]+$"))
  
  #environment
  env <- ptdi %>% filter(str_detect(id,  "ptdi_seeding_environment_[:digit:]+$" ))
  env_other <- ptdi %>% filter(str_detect(id,  "ptdi_seeding_environment_[:digit:]+_other$"))
  env <- dt_inputs(env, env_other)
  
  #seeding technique
  tech<- ptdi %>% filter(str_detect(id,  "ptdi_seeding_technique_[:digit:]+$"           ))  
  #<- ptdi %>% filter(str_detect(id,  "ptdi_seeding_technique_[:digit:]+_other"))
  
  #treatment
  treat <- ptdi %>% filter(str_detect(id,  "ptdi_seed_treatment_[:digit:]+$"              ))
  
  #implement type
  imp_type<- ptdi %>% filter(str_detect(id,  "ptdi_seeding_implement_type_[:digit:]+$"))
  imp_type_other <- ptdi %>% filter(str_detect(id,  "ptdi_seeding_implement_type_[:digit:]+_other$"))
  imp_type <- dt_inputs(imp_type, imp_type_other) 
  
  #implement traction
  imp_trac <- ptdi %>% filter(str_detect(id,  "ptdi_seeding_traction_[:digit:]+$"))
  imp_trac_other<- ptdi %>% filter(str_detect(id,  "ptdi_seeding_traction_[:digit:]+_other"))
  imp_trac <- dt_inputs(imp_trac, imp_trac_other)
  
  #distance rows
  row<- ptdi %>% filter(str_detect(id,   "ptdi_distance_rows_[:digit:]+$"   ))
  row_unit <- ptdi %>% filter(str_detect(id,   "ptdi_distance_rows_unit_[:digit:]+$" ))
  
  #seeding rate
  rate <- ptdi %>% filter(str_detect(id,  "ptdi_seeding_rate_[:digit:]$"))
  rate_unit <- ptdi %>% filter(str_detect(id, "ptdi_seeding_rate_unit_[:digit:]+$" ))

  #distance plants
  plan<- ptdi %>% filter(str_detect(id,   "ptdi_distance_plants_[:digit:]+$"   ))  
  plan_unit<- ptdi %>% filter(str_detect(id,   "ptdi_distance_plants_unit_[:digit:]+$"))
  
  #number of rows
  numrow <- ptdi %>% filter(str_detect(id,  "ptdi_seeding_density_number_rows_[:digit:]+$"))
  
  #plan density
  plden<- ptdi %>% filter(str_detect(id,  "ptdi_seeding_plant_density_[:digit:]+$"       ))
  plden_unit <- ptdi %>% filter(str_detect(id,  "ptdi_seeding_plant_density_unit_[:digit:]+$"  ))  
  
  #bund
  bund<- ptdi %>% filter(str_detect(id,  "ptdi_seeding_distance_bunds_[:digit:]+$"      ))
  bund_unit<- ptdi %>% filter(str_detect(id,  "ptdi_seeding_distance_bunds_unit_[:digit:]+$" ))
  
  #notes
  notes <- ptdi %>% filter(str_detect(id,  "ptdi_direct_seeding_notes_[:digit:]+$"))
  
  dt_di <- rbind(startD, env, tech, treat,  imp_type, 
              imp_trac, row, rate, plan, numrow, plden, bund, notes)
  
  lbl_di <- c("Direct_seeding_start_date", "Direct_seeding_environment",
              "Direct_seeding_technique",
              "Direct_seeding_seed_treatment", "Direct_seeding_implement_type", 
              "Direct_seeding_implement_traction" ,  
               paste("Direct_seeding_distance_between_rows","_",row_unit$values,sep=""),
              paste("Direct_seeding_seed_rate","_",rate_unit$values,sep=""),  
              paste("Direct_seeding_distance_between_plants","_",plan_unit$values,sep=""),
              "Direct_seeding_number_of_rows", 
              paste("Direct_seeding_plant_density","_",plden_unit$values,sep=""),
              paste("Direct_seeding_distance_between_bunds","_",bund_unit$values,sep=""),
              "Direct_seeding_notes"
              )
  
  #TODO: AGREGAR END DATE "Planting_direct_seeding_end_date"
  
  dt_di <- t(dt_di$values) %>% as.data.frame(stringAsFactors=FALSE)
  names(dt_di) <- lbl_di
  }
  else {
    dt_di<- data.frame()
  }
  
  #Transplating  
  if(isTRUE(input$transplanting_checkbox_1)){
    
  #planting transplanting ----------------------------------------------------
  ptta <- allinputs %>%  filter(!str_detect(id, "button")) %>%
          filter(!str_detect(id, "-selectized")) %>%
          filter(str_detect(id,"ptta"))
  
  #Start and End date
  ta_startD<- ptta %>% filter(str_detect(id,"ptta_transplanting_start_date_[:digit:]+$"))
  ta_endD <- ptta %>% filter(str_detect(id,"ptta_transplanting_end_date_[:digit:]+$"))
  
  #Age
  ta_age <- ptta %>% filter(str_detect(id,"ptta_age_seedling_[:digit:]+$"))
  
  #Environment
  ta_env <- ptta %>% filter(str_detect(id,"ptta_transplanting_environment_[:digit:]+$"))
  ta_env_other <- ptta %>% filter(str_detect(id,"ptta_transplanting_environment_[:digit:]+_other$"))
  ta_env <- dt_inputs(ta_env, ta_env_other)
  
  #Technique
  ta_tech <- ptta %>% filter(str_detect(id,"ptta_transplanting_technique_[:digit:]+$"))
  ta_tech_other<- ptta %>% filter(str_detect(id,"ptta_transplanting_technique_[:digit:]+_other$"))
  ta_tech <- dt_inputs(ta_tech,ta_tech_other)
  
  #Treatment
  ta_treat <- ptta %>% filter(str_detect(id,"ptta_transplanting_treatment_[:digit:]+$"))
  
  #Traction
  ta_trac <- ptta %>% filter(str_detect(id,"ptta_trans_traction_[:digit:]+$"))
  ta_trac_other <-ptta %>% filter(str_detect(id,"ptta_trans_traction_[:digit:]+_other$"))
  ta_trac <- dt_inputs(ta_trac, ta_trac_other)
  
  #distance between rows
  ta_drow<- ptta %>% filter(str_detect(id,"ptta_trans_distance_rows_[:digit:]+$"))
  ta_drow_unit<- ptta %>% filter(str_detect(id,"ptta_trans_distance_rows_unit_[:digit:]+$"))
  
  #Seeding density
  ta_sden <- ptta %>% filter(str_detect(id,"ptta_trans_seeding_density_[:digit:]+$"))
  ta_sden_unit <-ptta %>% filter(str_detect(id,"ptta_trans_seeding_density_unit_[:digit:]+$"))
  
  #Number of rows
  ta_numrow <- ptta %>% filter(str_detect(id,"ptta_trans_num_rows_[:digit:]+$"))
  
  #distance plants
  ta_dplan<- ptta %>% filter(str_detect(id,"ptta_trans_distance_plants_[:digit:]+$"))
  ta_dplan_unit <- ptta %>% filter(str_detect(id,"ptta_trans_distance_plants_unit_[:digit:]+$"))
  
  #distance bunds
  ta_bunds<- ptta %>% filter(str_detect(id,"ptta_trans_distance_bunds_[:digit:]+$"))
  ta_bunds_unit<- ptta %>% filter(str_detect(id,"ptta_trans_distance_bunds_unit_[:digit:]+$"))
  
  #Notes
  ta_notes<- ptta %>% filter(str_detect(id,"ptta_transplanting_density_notes_[:digit:]+$"))
  
  dt_ta<- rbind(ta_startD,ta_endD, ta_age, ta_env, ta_tech, ta_treat, ta_trac, 
             ta_drow, ta_sden, ta_numrow, ta_dplan, ta_bunds, ta_notes)
     
  lbl_ta <- c("Transplanting_start_date", "Transplanting_end_date",
             "Transplanting_age_of_seedling_(days)","Transplanting_seedling_environment",
              "Transplanting_technique","Transplanting_treatment",
              "Transplanting_traction",
              paste0("Transplanting_distance_between_rows","_",ta_drow_unit$values,sep=""),
              paste0("Transplanting_seedling_density","_",ta_sden_unit$values,sep=""),
              "Transplanting_number_of_rows",
              paste0("Transplanting_distance_between_plants","_",ta_dplan_unit$values,sep=""),
              paste0("Transplanting_distance_between_bunds","_",ta_bunds_unit$values,sep=""),
              "Transplanting_notes")      
  
  dt_ta <- t(dt_ta$values) %>% as.data.frame(stringAsFactors=FALSE)
  names(dt_ta) <- lbl_ta
  }
  else{
    dt_ta <- data.frame()
  }
  
  #dt_plantrans <- cbind(dt_di, dt_ta)
  dt_plantrans<- dplyr::bind_cols(dt_di,dt_ta)
  dt_plantrans
}

#TODO: escoger checkboxed y exportar lo marcado en planting transpling Intercrop

get_ec_plantrans_inter <- function(allinputs, addId, crop){
  
  
  #allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/inst/table_ids.rds")
  #input<- readRDS("/home/obenites/AGROFIMS/agdesign/inst/inputs.rds")
  
  
  #direct seedling -------------------------------------------------------------
  ptdi <- allinputs %>%  filter(!str_detect(id, "button")) %>%
            filter(!str_detect(id, "-selectized")) %>%
            filter(str_detect(id,"ptdi"))
  
  ptdi_temp  <- data.frame()
  
  
  for( i in 1:length(addId)){
    ptdi_temp  <- rbind(ptdi_temp, ptdi %>% filter(str_detect(id, addId[i])))
  }
  ptdi <- ptdi_temp
  
  
  #start date
  startD <- ptdi %>% filter(str_detect(id, "ptdi_planting_start_date_[:uppercase:]{8}$"))
  
  #environment
  env <- ptdi %>% filter(str_detect(id,  "ptdi_seeding_environment_[:uppercase:]{8}$" ))
  env_other <- ptdi %>% filter(str_detect(id,  "ptdi_seeding_environment_[:uppercase:]{8}_other$"))
  env <- dt_inputs(env, env_other)
  
  #seeding technique
  tech<- ptdi %>% filter(str_detect(id,  "ptdi_seeding_technique_[:uppercase:]{8}$"           ))  
  #<- ptdi %>% filter(str_detect(id,  "ptdi_seeding_technique_[:digit:]+_other"))
  
  #treatment
  treat <- ptdi %>% filter(str_detect(id,  "ptdi_seed_treatment_[:uppercase:]{8}$"              ))
  
  #implement type
  imp_type<- ptdi %>% filter(str_detect(id,  "ptdi_seeding_implement_type_[:uppercase:]{8}$"))
  imp_type_other <- ptdi %>% filter(str_detect(id,  "ptdi_seeding_implement_type_[:uppercase:]{8}_other$"))
  imp_type <- dt_inputs(imp_type, imp_type_other) 
  
  #implement traction
  imp_trac <- ptdi %>% filter(str_detect(id,  "ptdi_seeding_traction_[:uppercase:]{8}$"))
  imp_trac_other<- ptdi %>% filter(str_detect(id,  "ptdi_seeding_traction_[:uppercase:]{8}_other"))
  imp_trac <- dt_inputs(imp_trac, imp_trac_other)
  
  #distance rows
  row<- ptdi %>% filter(str_detect(id,   "ptdi_distance_rows_[:uppercase:]{8}$"   ))
  row_unit <- ptdi %>% filter(str_detect(id,   "ptdi_distance_rows_unit_[:uppercase:]{8}$" ))
  
  #seeding rate
  rate <- ptdi %>% filter(str_detect(id,  "ptdi_seeding_rate_[:uppercase:]{8}$"))
  rate_unit <- ptdi %>% filter(str_detect(id, "ptdi_seeding_rate_unit_[:uppercase:]{8}$" ))
  
  #distance plants
  plan<- ptdi %>% filter(str_detect(id,   "ptdi_distance_plants_unit_[:uppercase:]{8}$"))
  plan_unit<- ptdi %>% filter(str_detect(id,   "ptdi_distance_plants_[:uppercase:]{8}$"   ))  
  
  #number of rows
  numrow <- ptdi %>% filter(str_detect(id,  "ptdi_seeding_density_number_rows_[:uppercase:]{8}$"))
  
  #plan density
  plden<- ptdi %>% filter(str_detect(id,  "ptdi_seeding_plant_density_[:uppercase:]{8}$"       ))
  plden_unit <- ptdi %>% filter(str_detect(id,  "ptdi_seeding_plant_density_unit_[:uppercase:]{8}$"  ))  
  
  #bund
  bund<- ptdi %>% filter(str_detect(id,  "ptdi_seeding_distance_bunds_[:uppercase:]{8}$"      ))
  bund_unit<- ptdi %>% filter(str_detect(id,  "ptdi_seeding_distance_bunds_unit_[:uppercase:]{8}$" ))
  
  #notes
  notes <- ptdi %>% filter(str_detect(id,  "ptdi_direct_seeding_notes_[:uppercase:]{8}$"        ))
  
  dt_di <- rbind(startD, env, tech, treat,  imp_type, 
                 imp_trac, row, rate, plan, numrow, plden, bund, notes)
  
  lbl_di <- c("Direct_seeding_start_date", "Direct_seeding_environment", "Direct_seeding_technique",
              "Direct_seeding_seed_treatment", "Direct_seeding_implement_type", 
              "Direct_seeding_traction_type" ,  "Direct_seeding_distance_between_rows",
              "Direct_seeding_seed_rate",  "Direct_seeding_distance_between_plants",
              "Direct_seeding_number_of_rows", "Direct_seeding_plant_density",
              "Direct_seeding_distance_between_bunds","Direct_seeding_notes"
  )
  
  #TODO: agregar end date: "Planting_direct_seeding_end_date"
  
  ### INTERCROP
  lbl_di_inter <- NULL
  for(i in 1:length(lbl_di)){
    lbl_di_inter[[i]] <- paste(lbl_di[i], addId, sep = "_") 
  }
  lbl_di_inter <- unlist(lbl_di_inter)
  ##
  
  dt_di <- t(dt_di$values) %>% as.data.frame(stringAsFactors=FALSE)
  names(dt_di) <- lbl_di_inter
  
  
  #planting transplanting ----------------------------------------------------
  ptta <- allinputs %>%  filter(!str_detect(id, "button")) %>%
    filter(!str_detect(id, "-selectized")) %>%
    filter(str_detect(id,"ptta"))
  
  ptta_temp  <- data.frame()
  #addId <- c("NGBAKSHG", "VIITIDBD")
  
  for( i in 1:length(addId)){
    ptta_temp  <- rbind(ptta_temp, ptta %>% filter(str_detect(id, addId[i])))
  }
  ptta <- ptta_temp
  
  
  #Start and End date
  ta_startD<- ptta %>% filter(str_detect(id,"ptta_transplanting_start_date_[:uppercase:]{8}$"))
  ta_endD <- ptta %>% filter(str_detect(id,"ptta_transplanting_end_date_[:uppercase:]{8}$"))
  
  #Age
  ta_age <- ptta %>% filter(str_detect(id,"ptta_age_seedling_[:uppercase:]{8}$"))
  
  #Environment
  ta_env <- ptta %>% filter(str_detect(id,"ptta_transplanting_environment_[:uppercase:]{8}$"))
  ta_env_other <- ptta %>% filter(str_detect(id,"ptta_transplanting_environment_[:uppercase:]{8}+_other$"))
  ta_env <- dt_inputs(ta_env, ta_env_other)
  
  #Technique
  ta_tech <- ptta %>% filter(str_detect(id,"ptta_transplanting_technique_[:uppercase:]{8}$"))
  ta_tech_other<- ptta %>% filter(str_detect(id,"ptta_transplanting_technique_[:uppercase:]{8}_other$"))
  ta_tech <- dt_inputs(ta_tech,ta_tech_other)
  
  #Treatment
  ta_treat <- ptta %>% filter(str_detect(id,"ptta_transplanting_treatment_[:uppercase:]{8}$"))
  
  #Traction
  ta_trac <- ptta %>% filter(str_detect(id,"ptta_trans_traction_[:uppercase:]{8}$"))
  ta_trac_other <-ptta %>% filter(str_detect(id,"ptta_trans_traction_[:uppercase:]{8}_other$"))
  ta_trac <- dt_inputs(ta_trac, ta_trac_other)
  
  #Number of rows
  ta_drow<- ptta %>% filter(str_detect(id,"ptta_trans_distance_rows_[:uppercase:]{8}$"))
  ta_drow_unit<- ptta %>% filter(str_detect(id,"ptta_trans_distance_rows_unit_[:uppercase:]{8}$"))
  
  #Seeding density
  ta_sden <- ptta %>% filter(str_detect(id,"ptta_trans_seeding_density_[:uppercase:]{8}$"))
  ta_sden_unit <-ptta %>% filter(str_detect(id,"ptta_trans_seeding_density_unit_[:uppercase:]{8}$"))
  
  #Number of rows
  ta_numrow <- ptta %>% filter(str_detect(id,"ptta_trans_num_rows_[:uppercase:]{8}$"))
  
  #distance plants
  ta_dplan<- ptta %>% filter(str_detect(id,"ptta_trans_distance_plants_[:uppercase:]{8}$"))
  ta_dplan_unit <- ptta %>% filter(str_detect(id,"ptta_trans_distance_plants_unit_[:uppercase:]{8}$"))
  
  #distance bunds
  ta_bunds<- ptta %>% filter(str_detect(id,"ptta_trans_distance_bunds_[:uppercase:]{8}$"))
  ta_bunds<- ptta %>% filter(str_detect(id,"ptta_trans_distance_bunds_unit_[:uppercase:]{8}$"))
  
  #Notes
  ta_notes<- ptta %>% filter(str_detect(id,"ptta_transplanting_density_notes_[:uppercase:]{8}$"))
  
  dt_ta<- rbind(ta_startD,ta_endD, ta_age, ta_env, ta_tech, ta_treat, ta_trac, 
                ta_drow, ta_sden, ta_numrow, ta_dplan, ta_bunds, ta_notes)
  
  lbl_ta <- c("Transplanting_start_date", "Transplanting_end_date",
              "Transplanting_age_of_seedling_(days)","Transplanting_seedling_environment",
              "Transplanting_technique","Transplanting_treatment",
              "Transplanting_traction","Transplanting_distance_between_rows",
              "Transplanting_seedling_density","Transplanting_number_of_rows",
              "Transplanting_distance_between_plants",
              "Transplanting_distance_between_bunds",
              "Transplanting_notes")      

  
  lbl_ta_inter <- NULL
  for(i in 1:length(lbl_di)){
    lbl_ta_inter[[i]] <- paste(lbl_ta[i], addId, sep = "_") 
  }
  lbl_ta_inter <- unlist(lbl_ta_inter)
  
  
  dt_ta <- t(dt_ta$values) %>% as.data.frame(stringAsFactors=FALSE)
  names(dt_ta) <- lbl_ta_inter
  
  dt_plantrans <- cbind(dt_di, dt_ta)
  #dt_plantrans
  
  ### Create list of data frames (exp condition tables )per crop
  dt_plantrans_inter <- NULL
  
  for(i in 1:length(addId)){
    #chr<- as.character(i)
    lgl_header_plantras <- str_detect(string = names(dt_plantrans), addId[i] )
    # replacement = addId[i] )  
    dt_plantrans_inter[[i]] <- dt_plantrans[,lgl_header_plantras] 
    names(dt_plantrans_inter[[i]]) <- str_replace_all(names(dt_plantrans_inter[[i]]),
                                                  pattern = paste0("_",addId[i]),
                                                  replacement = ""  )
  }
  
  names(dt_plantrans_inter) <- crop
  dt_plantrans_inter
  
}