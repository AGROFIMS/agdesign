## RESIDUE DESCRIPTION SUBTAB -------------------------------------------------------------------------------------

#' Get residual description inputs
#' 
#'@description get all input values related to the experiment conditions \code{residue description management} experiment condition tab, 
#'provided by users when type in AGROFIMS.
#'@param input shiny reactiveValuesList provided by AGROFIMS
#'@param lbl \code{character} vector labels

get_ec_resdesc <- function(input, lbl){
  
  ### RESIDUE DESCRIPTION ----------------------------------------------------------------------------------------
  
  lbl <-  c("Plant part", "Crop residue moisture",
                "Crop residue thickness", "Crop residue thickness Unit (cm; ft; in; m)",
                "Crop residue amount", "Crop residue amount (g/ft2; g/m2; kg/ha; kf/m2: lb/ac)",
                "Crop residue percent of coverage", "Crop residue percent of coverage unit (%)",
                "Residue description Notes")
  
  
  plantp<- map_singleform_values(input = input$residue_plantPart, input_other = input$rmgt_esidue_plantPart_other,
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

  dt <- data.frame(plantp, cmoi, r_thick, r_thick_unit, r_amount, 
                       r_amount_unit, r_cov, r_cov_unit, r_notes)
 
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

  lbl <-  lbl <- c("Residue management description start date", 
                   "Residue management description technique",
                   "Residue management description traction",
                   "Residue management description Notes"
  )
  
    r_start_date<-  map_singleform_values(input =input$rmgt_residue_start_date, type = "date",
                                          format = "vector", label = "Factor")#dates
    r_technique <-  map_singleform_values(input =input$rmgt_residue_technique, input$rmgt_residue_technique_other, 
                                          type = "select",format = "vector", label = "Factor")#dates
    r_traction <-  map_singleform_values(input =input$rmgt_residue_traction, input$rmgt_residue_traction_other,
                                         type = "select",format = "vector", label = "Factor")#dates
    r_notes <- input$rmgt_residue_management_notes
    
    dt <- data.frame(r_start_date, r_technique, r_traction, r_notes)
    
    dt <- data.frame(matrix(ncol = length(lbl), nrow = 1))
    #dt[1,] <- dt_resmg
    names(dt) <- lbl
    dt

}



# TODO: FUNCTION
# get_ec_restab<- function(dt_resdesc , dt_resmgt){
#   
#   #TODO: IN CASE NOT CHECKBOX else not select dt<-NULL
#   dt<- cbind(dt_resdesc , dt_resmgt)
#   
# }

