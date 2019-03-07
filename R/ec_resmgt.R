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
  
  lbl <-  c("Residue_management_plant_part",
            "Residue_management_crop_residue_moisture",
            paste0("Residue_management_crop_residue_thickness_",r_thick_unit),
            paste0("Residue_management_crop_residue_amount_",r_amount_unit),
            "Residue_management_crop_residue_percent_of_coverage_%", 
            "Residue_management_residue_description_notes")
  
  dt <- data.frame(plantp, cmoi, r_thick, r_amount, r_cov,  r_notes, stringsAsFactors = FALSE)
 
  #dt<- rbind(plantp, cmoi, r_thick, r_amount, r_cov)
  #dt <- data.frame(matrix(ncol = length(lbl), nrow = 1))
  #dt[1,] <- dt_res
  names(dt) <- lbl
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

  lbl <-   c("Residue_management_description_start_date", 
                   "Residue_management_description_technique",
                   "Residue_management_description_traction",
                   "Residue_management_description_notes"
  )
  #rmgt_residue_technique
    r_start_date<-  map_singleform_values(input =input$rmgt_residue_start_date, type = "date",
                                          format = "vector", label = "Factor")#dates
    r_technique <-  map_singleform_values(input = input$rmgt_residue_technique, input$rmgt_residue_technique_other, 
                                          type = "select",format = "vector", label = "Factor")#dates
    
    r_traction <-  map_singleform_values(input =input$rmgt_residue_traction, input$rmgt_residue_traction_other,
                                         type = "select",format = "vector", label = "Factor")#dates
    r_notes <- input$rmgt_residue_management_notes
    
    
    if(r_technique=="Incorporation"){
      
      #lb1 <- "Residue incorporation depth"
      rb_incordepth <-  map_singleform_values(input =input$rmgt_residue_inc_depth,
                                              type = "text", format = "vector", label = "Factor")#dates
      
      #lb2 <- "Residue incorporation depth Unit"
      rb_incordepthunit <- map_singleform_values(input =input$rmgt_residue_inc_depth_unit, 
                                                 type = "select", format = "vector", label = "Factor") #unit
      
      dt <- data.frame(r_start_date, r_technique, rb_incordepth, r_traction, r_notes, stringsAsFactors = FALSE)
      
     lbl <-   c("Residue_management_description_start_date", 
                 "Residue_management_description_technique",
                 paste0("Residue_management_residue_incorporation_depth_",rb_incordepthunit),
                 "Residue_management_description_traction",
                 "Residue_management_description_notes")
     
      names(dt) <- lbl
      dt
      
    } 
    else {
      dt <- data.frame(r_start_date, r_technique, r_traction, r_notes)
      names(dt) <- lbl
    }
    
   out <- list(dt=dt, lbl= lbl)
    
}



#################################### LABEL ##################################################################

# #LABEL:  RESIDUE DESCRIPTION
# get_label_resdesc <- function(input){
#  
#     ### RESIDUE DESCRIPTION ----------------------------------------------------------------------------------------
#   
#  
#   r_thick <-  map_singleform_values(input =input$rmgt_crop_residue_thick , type = "numeric", format = "vector", label = "Factor")
#   r_thick_unit <- map_singleform_values(input =input$rmgt_crop_residue_thick_unit, 
#                                         type = "select", format = "vector", label = "Factor") #unit
#   #r_thick<- paste0(r_thick,r_thick_unit)
#   r_amount <-  map_singleform_values(input =input$rmgt_crop_residue_amount_sqm, type = "numeric", format = "vector", label = "Factor")
#   r_amount_unit <- map_singleform_values(input =input$rmgt_crop_residue_amount_sqm_unit, 
#                                          type = "select", format = "vector", label = "Factor") #unit
#   #r_thick<- paste0(r_amount,r_amount_unit)
#   r_cov <-  map_singleform_values(input =input$rmgt_crop_residue_perc_cov, type = "numeric", format = "vector", label = "Factor")
#   r_cov_unit <- map_singleform_values(input =input$rmgt_crop_residue_perc_cov_unit, 
#                                       type = "select", format = "vector", label = "Factor")  #unit
# 
#   lbl <-  c("Residue_management_plant_part",
#             "Residue_management_crop_residue_moisture",
#             paste0("Residue_management_crop_residue_thickness_",r_thick_unit),
#             paste0("Residue_management_crop_residue_amount_",r_amount_unit),
#             "Residue_management_crop_residue_percent_of_coverage_%", 
#             "Residue_management_residue_description_notes")
#   
# }
# 
# 
# #LABEL: RESIDUE MANAGEMENT
# 
# get_label_resmgt <- function(input){
#   
#   lbl <-   c("Residue_management_description_start_date", 
#              "Residue_management_description_technique",
#              "Residue_management_description_traction",
#              "Residue_management_description_notes"
#   )
#  
#   r_technique <-  map_singleform_values(input = input$rmgt_residue_technique, input$rmgt_residue_technique_other, 
#                                         type = "select",format = "vector", label = "Factor")#dates
#   
#   if(r_technique=="Incorporation"){
#     
#     #lb1 <- "Residue incorporation depth"
#     rb_incordepth <-  map_singleform_values(input =input$rmgt_residue_inc_depth,
#                                             type = "text", format = "vector", label = "Factor")#dates
#     
#     #lb2 <- "Residue incorporation depth Unit"
#     rb_incordepthunit <- map_singleform_values(input =input$rmgt_residue_inc_depth_unit, 
#                                                type = "select", format = "vector", label = "Factor") #unit
#     
#     lbl <-   c("Residue_management_description_start_date", 
#                "Residue_management_description_technique",
#                paste0("Residue_management_residue_incorporation_depth_",rb_incordepthunit),
#                "Residue_management_description_traction",
#                "Residue_management_description_notes")
#   } 
#   
#   lbl
#   
# }
# 
# # TOTAL
# 
# get_label_residue_module<- function(input){
#   
#   
#   if(isTRUE(input$residueDesc_checkbox)){
#     lbl1<- get_label_resdesc     
#   }else {
#     lbl1 <- NULL
#   }
#   
#   if(isTRUE(input$residueManag_checkbox)){
#     lbl2 <- get_label_resmgt
#   } else{
#     lbl2 <- NULL
#   }
#   
# 
#   
# }

