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
  
  llNames<- c("Land levelling start date (yyyy/mm/dd)", 
              "Land levelling Total number of levelling passes", "Land levelling Notes",
              "Land levelling Type", "Land levelling traction")
  names(dt)<-llNames
  dt
  
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
  
  dt <- data.frame(lp_start_date, lp_depth_val, lp_depth_unit, lp_npasses, lp_notes, lp_type, lp_traction) 
  
  
  lpNames <- c("Puddling start date (yyyy/mm/dd)", 
               "Puddling depth", "Puddling depth Unit (cm; ft; in; m)",
               "Puddling Total number of puddling passes",
               "Puddling notes", "Puddling type", "Puddling traction")
  
  names(dt) <- lpNames
  dt
  
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
  
  ltNames <- c("Tillage start date (yyyy/mm/dd)", "Tillage technique",
               "Tillage depth", "Tillage depth Unit (cm; ft; in; m)", #unit label
               "Total number of tillage passes", "Tillage Notes", "Tillage Type", "Tillage Traction")
  
  dt <- data.frame(lt_start_date, lt_technique,
                     lt_depth, lt_depth_unit, lt_npasses ,lt_notes, lt_type, lt_traction )
  
  
  names(dt) <- ltNames
  dt
  
  
}