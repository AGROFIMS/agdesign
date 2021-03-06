## RESIDUE DESCRIPTION SUBTAB -------------------------------------------------------------------------------------

#' Get residual description inputs
#' 
#'@description get all input values related to the experiment conditions \code{residue description management} experiment condition tab, 
#'provided by users when type in AGROFIMS.
#'@param input shiny reactiveValuesList provided by AGROFIMS
#'@param lbl \code{character} vector labels

get_ec_resdesc <- function(input, lbl){
  
  # allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/inst/table_ids.rds")
  # input<- readRDS("/home/obenites/AGROFIMS/agdesign/inst/inputs.rds")

  ### RESIDUE DESCRIPTION ----------------------------------------------------------------------------------------
  
  plantp<- map_singleform_values(input = input$rmgt_residue_plantPart, input_other = input$rmgt_residue_plantPart_other,
                             type = "select", format = "vector", label = "Factor")
  
  cmoi <- map_singleform_values(input =input$rmgt_crop_residue_moisture,
                                type = "select", format = "vector", label = "Factor")
  
  r_thick <-  map_singleform_values(input =input$rmgt_crop_residue_thick , type = "numeric", format = "vector", label = "Factor")
  r_thick_unit <- map_singleform_values(input =input$rmgt_crop_residue_thick_unit, 
                                        type = "select", format = "vector", label = "Factor") #unit
  #r_thick<- paste0(r_thick,r_thick_unit)
  r_amount <-  map_singleform_values(input =input$rmgt_crop_residue_amount_sqm, type = "numeric", format = "vector", label = "Factor")
  r_amount_unit <- map_singleform_values(input =input$rmgt_crop_residue_amount_sqm_unit, 
                                         type = "select", format = "vector", label = "Factor") #unit
  #r_thick<- paste0(r_amount,r_amount_unit)
  r_cov <-  map_singleform_values(input =input$rmgt_crop_residue_perc_cov, type = "numeric", format = "vector", label = "Factor")
  r_cov_unit <- map_singleform_values(input =input$rmgt_crop_residue_perc_cov_unit, 
                                      type = "select", format = "vector", label = "Factor")  #unit
  #r_thick<- paste0(r_cov,r_cov_unit)
  r_notes <- input$rmgt_residue_description_notes
  
  #BASE FORM OF HEADERS
  # lbl <-  c("Residue_management_plant_part",
  #           "Residue_management_crop_residue_moisture",
  #           paste0("Residue_management_crop_residue_thickness_",r_thick_unit),
  #           paste0("Residue_management_crop_residue_amount_",r_amount_unit),
  #           "Residue_management_crop_residue_percent_of_coverage_%", 
  #           "Residue_management_residue_description_notes")
  
  lbl <- c("Crop_residue_plant_part",
           "Crop_residue_moisture",
           paste0("Crop_residue_thickness_",r_thick_unit),
           paste0("Crop_residue_amount_",r_amount_unit),
           "Crop_residue_percent_coverage_%",
           "Crop_residue_notes"
           )
  
  #Label for spreadsheet and kdsmart headers
  lbl_dt <- lbl # hide for issue 236 #paste(lbl, rep("1", length(lbl)) ,sep="__") 
  
  dt <- data.frame(plantp, cmoi, r_thick, r_amount, r_cov,  r_notes, stringsAsFactors = FALSE)
  names(dt) <- lbl_dt
  #TODO: IN CASE NOT CHECKBOX else not select dt<-NULL
  
  out <- list(dt=dt, lbl= lbl)
}


## RESIDUE MANAGEMENT SUBTAB -------------------------------------------------------------------------------------

#' Get residual management inputs
#' 
#'@description get all input values related to the experiment conditions \code{residue management} experiment condition tab, 
#'provided by users when type in AGROFIMS.
#'@param input shiny reactiveValuesList provided by AGROFIMS
#'@param lbl \code{character} vector labels

get_ec_resmgt <- function(input, lbl){

  #LABEL BASE FORM
  # lbl <-   c("Residue_management_description_start_date", 
  #                  "Residue_management_description_technique",
  #                  "Residue_management_description_traction",
  #                  "Residue_management_description_notes"
  #           )
  # 
  # lbl <-   c("Residue_management_start_date", 
  #            #"Residue_management_end_date",
  #            "Residue_management_technique",
  #            "Residue_management_traction_type",
  #            "Residue_management_notes"
  # )
  
  #LABEL FOR SPREADSHEET AND KDSMART with underscore
  #lbl_dt <- paste(lbl, rep("1", length(lbl)) ,sep="__") 
  
  #rmgt_residue_technique
    r_start_date<-  map_singleform_values(input =input$rmgt_residue_start_date, type = "date",
                                          format = "vector", label = "Factor")#dates
    r_technique <-  map_singleform_values(input = input$rmgt_residue_technique, input$rmgt_residue_technique_other, 
                                          type = "select",format = "vector", label = "Factor")#dates
    
    r_traction <-  map_singleform_values(input =input$rmgt_residue_traction, input$rmgt_residue_traction_other,
                                         type = "select",format = "vector", label = "Factor")#dates
    
    r_notes <- input$rmgt_residue_management_notes
    
    
    ##SPECIAL CASE: INCORPORATION IN METHOD ####################
   # if(r_technique=="Incorporation"){
      
      #lb1 <- "Residue incorporation depth"
      rb_incordepth <-  map_singleform_values(input =input$rmgt_residue_inc_depth,
                                              type = "text", format = "vector", label = "Factor")#dates
      
      #lb2 <- "Residue incorporation depth Unit"
      rb_incordepthunit <- map_singleform_values(input =input$rmgt_residue_inc_depth_unit, 
                                                 type = "select", format = "vector", label = "Factor") #unit
      
      dt <- data.frame(r_start_date, r_technique, rb_incordepth, r_traction, r_notes, stringsAsFactors = FALSE)
      #dt[1,] <- "" #Fill the templates with empty values 
      
      lbl <-   c("Residue_management_start_date", 
                 "Residue_management_technique",
                 paste0("Residue_incorporation_depth_",rb_incordepthunit),
                 "Residue_management_traction_type",
                 "Residue_management_notes")
      
     
      #LABEL FOR SPREADSHEET AND KDSMART with underscore
      lbl_dt <-  lbl # hide for issue 236 #paste(lbl, rep("1", length(lbl)) ,sep="__") 
      names(dt) <- lbl_dt
      
      
    #} 
    #else {
      #dt <- data.frame(r_start_date, r_technique, r_traction, r_notes, stringsAsFactors = FALSE)
      #names(dt) <- lbl_dt
    #}
    
 
   out <- list(dt=dt, lbl= lbl)
    
}


## Protocols ##########################################################################################################

# Get protocol residual dsecription

get_protocol_resdesc <- function(input){
  
  out<- get_ec_resdesc(input)$dt 
  #names(out) <- stringr::str_replace_all(names(out),"__1","") # hide for issue 236
  out <- t(out) %>% as.data.frame(stringsAsFactors=FALSE) %>% tibble::rownames_to_column()
  out <- out %>% dplyr::filter(V1!="") %>% dplyr::filter(!stringr::str_detect(V1, "^NA$"))
  names(out) <- c("TraitName","Value")
  out
}

# Get protocol residual management 

get_protocol_resmgt <- function(input){

  out<- get_ec_resmgt(input)$dt
  #names(out) <- stringr::str_replace_all(names(out),"__1","") # hide for issue 236
  out <- t(out) %>% as.data.frame(stringsAsFactors=FALSE) %>% tibble::rownames_to_column()
  out <- out %>% dplyr::filter(V1!="") %>% dplyr::filter(!stringr::str_detect(V1, "^NA$"))
  names(out) <- c("TraitName","Value")
  out
}


# Get collectable inputs from Residue Management #############################################################################
# allinputs: data frame of all inputs derived from ReactiveValuesToList
get_collectable_resmgt <- function(allinputs){

  rd <- allinputs %>% dplyr::filter(stringr::str_detect(id,  paste0("^","residue_description_to_collect_field","$") )) %>% dplyr::nth(2)
  rd <- stringi::stri_split_regex(rd,",")[[1]] %>%  stringr::str_trim(side = "both") %>%  setdiff("")
  if(length(rd)!=0){
    rd <- paste0("Residue description" ,"_", rd)
  }
  
  rm <- allinputs %>% dplyr::filter(stringr::str_detect(id,  paste0("^","residue_management_to_collect_field","$") ))  %>% dplyr::nth(2)
  rm <- stringi::stri_split_regex(rm,",")[[1]] %>% stringr::str_trim(side = "both") %>% setdiff("")  
  if(length(rm)!=0){
    rm <- paste0("Residue management" ,"_", rm)
  }
  out <- c(rd, rm)
}




