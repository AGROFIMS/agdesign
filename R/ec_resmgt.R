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
  r_notes <- input$rmgt_residue_management_notes

  lbl <-  c("Plant_part",
            "Crop_residue_moisture",
            paste0("Crop_residue_thickness_",r_thick_unit),
            paste0("Crop_residue_amount_",r_amount_unit),
            "Crop residue percent of coverage", 
            "Residue description Notes")
  
  dt <- data.frame(plantp, cmoi, r_thick, r_amount, r_cov,  r_notes, stringsAsFactors = FALSE)
 
  #dt<- rbind(plantp, cmoi, r_thick, r_amount, r_cov)
  #dt <- data.frame(matrix(ncol = length(lbl), nrow = 1))
  #dt[1,] <- dt_res
  names(dt) <- lbl
  #TODO: IN CASE NOT CHECKBOX else not select dt<-NULL
  
  dt
}


## RESIDUE MANAGEMENT SUBTAB -------------------------------------------------------------------------------------

#' Get residual management inputs
#' 
#'@description get all input values related to the experiment conditions \code{residue management} experiment condition tab, 
#'provided by users when type in AGROFIMS.
#'@param input shiny reactiveValuesList provided by AGROFIMS
#'@param lbl \code{character} vector labels

get_ec_resmgt <- function(input, lbl){

  lbl <-   c("Residue_management_description_Start date", 
                   "Residue_management_description_Technique",
                   "Residue_management_description_Traction",
                   "Residue_management_description_Notes"
  )
  #rmgt_residue_technique
    r_start_date<-  map_singleform_values(input =input$rmgt_residue_start_date, type = "date",
                                          format = "vector", label = "Factor")#dates
    r_technique <-  map_singleform_values(input = input$rmgt_residue_technique, input$rmgt_residue_technique_other, 
                                          type = "select",format = "vector", label = "Factor")#dates
    
    r_traction <-  map_singleform_values(input =input$rmgt_residue_traction, input$rmgt_residue_traction_other,
                                         type = "select",format = "vector", label = "Factor")#dates
    r_notes <- input$rmgt_residue_management_notes
    
    
    if(r_technique=="Burying"){
      
      #lb1 <- "Residue incorporation depth"
      rb_incordepth <-  map_singleform_values(input =input$rmgt_residue_inc_depth,
                                              type = "text", format = "vector", label = "Factor")#dates
      
      #lb2 <- "Residue incorporation depth Unit"
      rb_incordepthunit <- map_singleform_values(input =input$rmgt_residue_inc_depth_unit, 
                                                 type = "select", format = "vector", label = "Factor") #unit
      
      dt <- data.frame(r_start_date, r_technique, rb_incordepth, r_traction, r_notes, stringsAsFactors = FALSE)
      
     lbl <-   c("Residue_management_description_Start_date", 
                 "Residue_management_description_Technique",
                 paste0("Residue_management_Incorporation_depth",rb_incordepthunit),
                 "Residue_management_description_Traction",
                 "Residue_management_description_Notes")
     
      names(dt) <- lbl
      dt
      
    } 
    else {
      dt <- data.frame(r_start_date, r_technique, r_traction, r_notes)
      names(dt) <- lbl
    }
    
    dt
    
}



# TODO: FUNCTION
# get_ec_restab<- function(dt_resdesc , dt_resmgt){
#   
#   #TODO: IN CASE NOT CHECKBOX else not select dt<-NULL
#   dt<- cbind(dt_resdesc , dt_resmgt)
#   
# }

