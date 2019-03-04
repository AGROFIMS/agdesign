#' Get exp conditions , soil and fertilkity tab
#'@description get all input values related to the experiment conditions \code{soil and fertility} experiment condition tab, 
#'provided by users when type in AGROFIMS.
#'@param allinputs shiny reactiveValuesList provided by AGROFIMS
#'@param lbl \code{character} vector labels
#'@param napp \code{numeric} number of applications
#'@author Omar Benites

get_ec_sf <- function(allinputs, lbl, napp ){
  
  #allinputs$id<-as.character(allinputs$id)
  #allinputs$values<-as.character(allinputs$values)  
  
  # allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/inst/table_ids.rds")
  # input<- readRDS("/home/obenites/AGROFIMS/agdesign/inst/inputs.rds")

  napp<- as.numeric(napp)
  
  ## Fertilizer type -------------------------------------------------------------------------------------------------
  ft <-allinputs %>% filter(str_detect(id, "^select_fertilizerType_soil_table_row_")) %>%
                      filter(!str_detect(id, "selectized")) %>% arrange_("id")
  
  #product fertilizer ------------------------------------------------------------------------------------------------
  pr <-allinputs %>% filter(str_detect(id, "^select_product_soil_table_row_")) %>% 
                     filter(!str_detect(id, "selectized")) %>% filter(!str_detect(id, "other")) %>% arrange_("id")
  pr_other <-allinputs %>% filter(str_detect(id, "^select_product_soil_table_row_[0-9]+_other")) %>% arrange_("id")
  pr <- dt_inputs(pr,pr_other)
  
  
  ## product rate -----------------------------------------------------------------------------------------------------
  pra <-allinputs %>% filter(str_detect(id, "^input_productRate_soil_table_")) %>% arrange_("id")
  
  # element fertilizer ------------------------------------------------------------------------------------------------
  el <-allinputs %>% filter(str_detect(id, "^select_element_soil_table_row_")) %>% 
                      filter(!str_detect(id, "selectized")) %>% filter(!str_detect(id, "other")) %>% arrange_("id")
  el_other <-allinputs %>% filter(str_detect(id, "^select_element_soil_table_row_[0-9]+_other"))  %>% arrange_("id")
  el <- dt_inputs(el,el_other)
  
  
  ## element rate -----------------------------------------------------------------------------------------------------
  era <-allinputs %>% filter(str_detect(id, "^input_elementRate_soil_table_row_")) %>% arrange_("id")
  
  ##Start and End Date ------------------------------------------------------------------------------------------------
  startD <-allinputs %>% filter(str_detect(id, "^input_startdate_soil_table_row_")) %>% 
                          filter(!str_detect(id, "button")) %>%  arrange_("id")
  
  endD <-  allinputs %>% filter(str_detect(id, "^input_enddate_soil_table_row_")) %>% 
                          filter(!str_detect(id, "button")) %>%  arrange_("id")
  ## Technique ---------------------------------------------------------------------------------------------------------
  tech <-allinputs %>% filter(str_detect(id, "^select_techinque_soil_table_row_")) %>% 
                        filter(!str_detect(id, "selectized")) %>% filter(!str_detect(id, "other")) %>% arrange_("id")
  tech_other <-allinputs %>% filter(str_detect(id, "^select_techinque_soil_table_row_[0-9]+_other")) %>% arrange_("id")
  tech <- dt_inputs(tech,tech_other)
  
  #Text Area Soil fertility --------------------------------------------------------------------------------------------
  txtA <- allinputs %>% filter(str_detect(id, "^textArea_soil_table_row_")) %>% arrange_("id")
  
  #Total product and element soil fertility ----------------------------------------------------------------------------
  proTotal <- allinputs %>% filter(str_detect(id, "^totalApp soil_fertilizer_totalAppRate"))
  eleTotal <- allinputs %>% filter(str_detect(id, "^soil_fertilizer_totalAppRate"))
  
  dt_soil<- rbind(ft, pr, pra, el, era, startD, endD, tech, txtA, proTotal, eleTotal) 
  dt_soil <- arrange_by_pattern(dt_soil, as.character(1:napp) )
  #dt_soil <- arrange_by_pattern(dt_soil,c("1","2","3"))
  ### Label
  lbl <- c("Soil_fertility_fertilizer_type","Soil_fertility_product","Soil_fertility_product_rate_(kg/ha)", 
           "Soil_fertility_element","Soil_fertility_element_rate_(kg/ha)",
           "Soil_fertility_start_date", "Soil_fertility_end_date", "Soil_fertility_technique", "Soil_fertility_notes")
  lbl_soil <- NULL #vector(mode = "character",length = length(lbl)*napp)) 
  for(i in 1:napp){
    lbl_soil  <- append(lbl_soil , paste0(lbl,"__", i))
  }
  #lbl_soil <-c(lbl_soil, "Total product calculated application", "Total element calculated application")
  dt <- data.frame(matrix(ncol = length(lbl_soil), nrow = 1))
  names(dt) <- lbl_soil
  dt[1,]<- dt_soil[,"values"]
  dt
  
}


# dt_inputs<- function(dt, dt_other){
# 
#   for(i in 1:nrow(dt)){
#     
#     if(dt[i,"values"]=="Other" || dt[i,"values"]=="other" ){
#        dt[i,"values"] <-  dt_other[i,"values"]
#     }
#     else{
#       dt[i,"values"]<- dt[i,"values"]
#     }
#   }  
#   dt
# }
# 
# lbl <- c("Fertilizer_type","Product","Product_rate_(kg/ha)", "Element","Element_rate_(kg/ha)",
#          "Start_date", "End_date", "Technique", "Notes")
# allinputs <- 
# 


#dt<- get_ec_sf(allinputs, lbl, napp )