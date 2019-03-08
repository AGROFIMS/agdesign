#' @export
get_ec_sblalv<- function(input){

  ll_start_date <- map_singleform_values(input =input$landLeveling_start_date, type = "date", format = "vector", label = "Factor" ) #dates
  #ll_end_date <- getDateInput(input$landLeveling_end_date) #dates
  ll_npasses <- map_singleform_values(input =input$numPasses,  type = "numeric", format = "vector", 
                                      label = "Factor" )
  ll_notes <-  map_singleform_values(input =input$landLeveling_notes,  type = "text", format = "vector",
                                     label = "Factor")
  ll_type <-  map_singleform_values(input$land_impl_type, type = "select", format = "vector", label = "Factor")
  ll_traction <-  map_singleform_values(input$land_traction, input$land_traction_other, type = "select", format = "vector", label = "Factor")
  
  dt<- data.frame(ll_start_date, ll_npasses, ll_notes, ll_type, ll_traction)
  
  #BASE FORM OF HEADERS
  lbl<- c("Land_levelling_start_date_(yyyy/mm/dd)", 
              "Land_levelling_total_number_of_levelling_passes", "Land_levelling_notes",
              "Land_levelling_implement_type", "Land_levelling_implement_traction")
  
  #Label for spreadsheet and kdsmart headers
  lbl_dt <- paste(lbl, rep("1", length(lbl)) ,sep="__") 
  
  
  #TODO: AGREGAR END DATE
  names(dt)<-lbl_dt
  out <- list(dt=dt, lbl=lbl)
  
}

#' @export
get_ec_sbpud <- function(input){
  
  lp_start_date <-  map_singleform_values(input =input$puddling_start_date,
                                          type = "date", format = "vector", label = "Factor" )
  
  lp_depth_val <- map_singleform_values(input = input$puddling_depth_val, 
                                        type = "numeric", format = "vector", label = "Factor" )
  lp_depth_unit <- map_singleform_values(input = input$puddling_depth_unit, 
                                         type = "select", format = "vector", label = "Factor")
  
  lp_npasses <- map_singleform_values(input = input$puddling_total_number_puddling_passes,  
                                      type = "numeric", format = "vector", label = "Factor")
  lp_notes <- map_singleform_values(input =input$puddling_notes, 
                                    type = "text", format = "vector", label = "Factor")
  lp_type <- map_singleform_values(input = input$pud_impl_type, input$pud_impl_type_other, 
                                   type = "select", format = "vector", label = "Factor")
  lp_traction <- map_singleform_values(input$pud_traction, input$pud_traction_other, 
                                       type = "select", format = "vector", label = "Factor")
  
  dt <- data.frame(lp_start_date, lp_depth_val, lp_npasses, lp_notes, lp_type, lp_traction) 
  
  
  lbl <- c("Puddling_start_date_(yyyy/mm/dd)", 
               paste0("Puddling_depth_",lp_depth_unit),
               "Puddling_total_number_of_puddling_passes",
               "Puddling_notes", "Puddling_implement_type", "Puddling_implement_traction")
  
  #Label for spreadsheet and kdsmart headers
  lbl_dt <- paste(lbl, rep("1", length(lbl)) ,sep="__")
  
  #TODO: AGREGAR END DATE
  names(dt) <- lbl_dt
  
  #OUTPUT
  out <- list(dt=dt, lbl= lbl) 
  
}

#' @export
get_ec_sbtill <- function(input){
  
  
  lt_start_date <- map_singleform_values(input = input$tillage_start_date, type = "date", format = "vector", label = "Factor" )
  #lt_end_date  <-  getDateInput(input$tillage_end_date)
  lt_technique  <- map_singleform_values(input =input$till_technique, input$till_technique_other,
                                         type = "select", format = "vector", label = "Factor")
  #lt_depth_method  <- getAgrOper(input$till_depth_method)
  
  lt_depth  <- map_singleform_values(input = input$tillage_depth,  type = "numeric", format = "vector", label = "Factor")
  lt_depth_unit  <- map_singleform_values(input = input$tillage_depth_unit,type = "select", format = "vector", label = "Factor" )
  #lt_depth_lbl <- paste("Tillage depth", lt_depth_unit, sep="_") #label
  
  lt_npasses  <- map_singleform_values(input =input$total_number_tillage_passes, type = "numeric", format = "vector", label = "Factor")
  lt_notes  <- map_singleform_values(input$tillage_notes, type = "text", format = "vector",
                                     label = "Factor")
  lt_type  <-  map_singleform_values(input =input$till_impl_type, input$till_impl_type_other, 
                                     type = "select", format = "vector", label = "Factor")
  lt_traction <- map_singleform_values(input =input$till_traction, input$till_traction_other,
                                       type = "select", format = "vector", label = "Factor")
  
  lbl <- c("Tillage_start_date_(yyyy/mm/dd)", 
               "Tillage_technique",
               paste0("Tillage_depth_",lt_depth_unit), 
               "Tillage_total_number_of_tillage_passes", "Tillage_notes", 
               "Tillage_implement_type", "Tillage_implement_traction")
  
  dt <- data.frame(lt_start_date, lt_technique,
                     lt_depth, lt_npasses ,lt_notes, lt_type, lt_traction,stringsAsFactors = FALSE )
  
  
  #Label for spreadsheet and kdsmart headers
  lbl_dt <- paste(lbl, rep("1", length(lbl)) ,sep="__")
  
  #TODO: AGREGAR END DATE
  names(dt) <- lbl_dt
  
  out <- list(dt=dt, lbl=lbl)
  
}







