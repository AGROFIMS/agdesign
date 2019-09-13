#' Get exp conditions , soil and fertilkity tab
#' @description get all input values related to the experiment conditions \code{soil and fertility} experiment condition tab, 
#' provided by users when type in AGROFIMS.
#' @param allinputs shiny reactiveValuesList provided by AGROFIMS
#' @param napp \code{numeric} number of applications
#' @author Omar Benites
#'
get_ec_sf <- function(allinputs, napp ){
  
  #allinputs$id<-as.character(allinputs$id)
  #allinputs$values<-as.character(allinputs$values)  
  
  # allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/inst/table_ids.rds")
  # input<- readRDS("/home/obenites/AGROFIMS/agdesign/inst/inputs.rds")

  napp<- as.numeric(napp)
  
  ## Number of fertilizer
  # napp_vector <- rep(napp,napp) 
    
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
  
  ##LABEL FOR TRAITLIST :remove all double underscore from soil_fertility labels
  lbl<- str_replace_all(string = lbl_soil, pattern = "__[:digit:]+$",replacement = "")
  lbl <- unique(lbl)
  
  out<- list(dt=dt, lbl= lbl)
  
  
}

#' Get fertilizer product names from Fertilizer and Nutrient dataset
#' @description Get fertilizer's names from Fertilizer and Nutrient dataset
#' @param .data Fertilizer and Nutrient dataset
#' @author Omar Benites
#' 
get_fertilizer <- function(.data){
   .data$name
}


# minimize total cost of a fertilizer treatment
productApplicationRates <- function(supply, treatment, price, minCost=TRUE){
  name <- supply$name
  supply <- t(as.matrix(supply[,-1]))
  treatment <- as.matrix(treatment)
  
  result <- matrix(nrow=ncol(supply), ncol=nrow(treatment))
  price <- price * minCost
  treatment <- treatment * 100
  for (i in 1:nrow(treatment)) {
    solution <- limSolve::linp(E=supply, F=treatment[i,], Cost=price)
    if (solution$IsError) {
      result[,i] <- -99
    } else { 	
      result[,i] <- solution$X
    }
  }
  colnames(result) <- paste0("tr.", 1:ncol(result))
  data.frame(name, result)
}

# calculation of nutrient rate application
nutrientApplicationRates <- function(supply, treatment) {
  result <- matrix(nrow=ncol(supply)-1, ncol=ncol(treatment)-1)
  x <- match(treatment$name, supply$name)
  sup <- supply[x, -1] / 100
  for (i in 2:ncol(treatment)) {
    result[,i-1] <- colSums(treatment[,i] * sup)
  }
  colnames(result) <- colnames(treatment)[-1]
  data.frame(element=colnames(sup), result)
}

#Reshape nutrient data
nutrient_reshape<- function(df, name="A", values="B"){
  out<- list()
  nutrient<- unique(df[,name])
  for(i in seq.int(nutrient)   ){
    out[[i]] <- df %>% dplyr::filter(SplitNutName==nutrient[i]) %>% dplyr::select_(values) %>% nth(1) %>% as.numeric()
    out[[i]]<- as.data.frame(out[[i]],stringsAsFactors=FALSE)
    names(out[[i]]) <- nutrient[i]
  }
  if(length(out)>1){
    #https://stackoverflow.com/a/53893866/7340448
    #How to cbind a list of data.frames of different lengths
    #library(rowr)
    out <- do.call(rowr::cbind.fill, c(out, fill = NA))
  } else {
    out <- out[[1]]
  }
  out
  #out<- rbindlist(out)
}




product_calculation <- function(allinputs, dfAll, index="1", indexEspLvl=indexEspLvl , design="frcbd"){
  
  #Get labels
  indexEspLvl<- filter_index_espLvl_design(index= index, indexEspLvl=indexEspLvl, design=design, designEspflvl="_lvl_espType_")
  
  #Get especial levels
  indexEspLvl_subfix <- get_index_espLvl_design(indexEspLvl, paste0(design,"_lvl_espType_"))
  lookup <- paste0(design,"_")

  #Table of nutrient details (eletype, mNumTiming)-----------------------------------------------------------------------------------------
  nutrients_details <- get_nutrient_details_design(allinputs=allinputs, design=design, index =index, indexEspLvl = indexEspLvl)
  
  ## Split nutrient name and split number per each nutrient-------------------------------------------------------------------------------
  split_nutrient_name <- lapply(X = seq.int(dfAll$splits), function(x) rep(dfAll$eleType[x], each =as.numeric(dfAll$splits[x]))) %>% unlist() 
  split_nutrient_number <- lapply(as.integer(dfAll$splits), function(x) seq.int(x)) %>% unlist() 
  nutrients_details <- nutrients_details %>% 
                                 mutate(SplitNutName = split_nutrient_name ) %>% 
                                 mutate(splitNutNum = split_nutrient_number)
  ## 
  print("5")
  nutrient_elements <- nutrients_details[,c("SplitNutName", "nutAmount")]
  nutrient_elements[,2] <- as.double(nutrient_elements[,2]) 
  
  treatments <-  nutrient_reshape(nutrient_elements, "SplitNutName", "nutAmount")
  #ASIGNAR 0 A LOS NA
  #Ref: https://stackoverflow.com/questions/20535505/replacing-all-missing-values-in-r-data-table-with-a-value
  treatments[is.na(treatments)] = 0


  print("7")
  FertProDt<- lapply(seq.int(nutrients_details$mNutProduct), function(x) dt_fernut %>% dplyr::filter(name %in% nutrients_details$mNutProduct[x]) )
  FertProDt <- data.table::rbindlist(FertProDt,fill = TRUE,use.names = TRUE)
  names(FertProDt) <- c("group", "name", "Nitrogen","Phosphorus","Potassium","Calcium","Magnesium",  
                        "Sulfur", "Moldbenum", "Zinc", "Boron", "Copper","Iron", "Manganese", "Nickel", "Chlorine")
  FertProDt<- tibble::as_tibble(FertProDt) 
  FertProDt <- FertProDt[,c("name",unique(nutrient_elements$SplitNutName))]
  FertProDt[is.na(FertProDt)] = 0
  FertProDt <- purrr::map_at(.x = FertProDt,.at = unique(nutrient_elements$SplitNutName), .f = as.numeric) %>% as.data.frame(stringsAsFactors=FALSE)
  
  
  ##TODO:: TRANSFORMAR A NUMERICO ambas matrices
  price <- rep(1, nrow(FertProDt) )
  r <- productApplicationRates(supply = FertProDt, treatment = treatments , price =price , minCost=FALSE)
  r <- as.data.frame(r, stringsAsFactors=FALSE)
  n <- ncol(r)- 1
  names(r) <- c("name", paste0("level_",seq.int(n)))
  r
 
  #rr <- r[apply(r[,-1], 1, function(i) !all(i==0)), ]
  #print(rr)   
  
}

############################################ FERTILIZERS ####################################################

#Reshape fertilizer data
#"SplitFertName", "mProdAmount"
fertilizer_reshape<- function(df){
  
  out<- data.frame(t(as.numeric(df$mProdAmount)),stringsAsFactors = FALSE)
  names(out) <- paste(df$factorType,df$splitFertNum, 1:nrow(df),sep = "_")
  out
}


#Fertilzier calculation
fertilizer_calculation <- function(allinputs, dfAll, index="1", indexEspLvl=indexEspLvl , design="frcbd"){
  
  
  #Get labels
  indexEspLvl<- filter_index_espLvl_design(index= index, indexEspLvl=indexEspLvl, design=design, designEspflvl="_lvl_espType_")
  
  #Get especial levels
  indexEspLvl_subfix <- get_index_espLvl_design(indexEspLvl, paste0(design,"_lvl_espType_"))
  lookup <- paste0(design,"_")
  
  #Table of nutrient details (eletype, mNumTiming)-----------------------------------------------------------------------------------------
  fertilizer_details <- get_fertilizer_details_design(allinputs, design, index, indexEspLvl)
  
  
  ## Split nutrient name and split number per each nutrient-------------------------------------------------------------------------------
  split_fert_name <- lapply(X = seq.int(dfAll$splits), function(x) rep(dfAll$eleType[x], each =as.numeric(dfAll$splits[x]))) %>% unlist() 
  split_fert_number <- lapply(as.integer(dfAll$splits), function(x) seq.int(x)) %>% unlist() 
  fertilizer_details <- fertilizer_details %>% 
                          mutate(splitFertNum = split_fert_number) %>% 
                          mutate(SplitFertName = split_fert_name ) 
                          
  fertilizer_elements <- fertilizer_details[,c("SplitFertName", "mProdAmount", "splitFertNum")]
  fertilizer_elements[,2] <- as.double(fertilizer_elements[,2]) 
  
  rr <- fertilizer_reshape(df=fertilizer_elements)
  names(rr)<- paste0("split_",1:ncol(rr))
  
  ####
  ##TODO: CAMBIAR  factorType POR mFerProductName
  FertProDt<- lapply(seq.int(fertilizer_details$mProdAmount), function(x) dt_fernut %>% dplyr::filter(name %in% fertilizer_details$factorType[x]) )
  FertProDt <- data.table::rbindlist(FertProDt,fill = TRUE,use.names = TRUE)
  names(FertProDt) <- c("group", "name", "Nitrogen","Phosphorus","Potassium","Calcium","Magnesium",  
                        "Sulfur", "Moldbenum", "Zinc", "Boron", "Copper","Iron", "Manganese", "Nickel", "Chlorine")
  FertProDt<- tibble::as_tibble(FertProDt) 
  contents<- FertProDt[,-1]
  #FertProDt <- FertProDt[,c("name",unique(fertilizer_elements$ factorType))]
  #FertProDt[is.na(FertProDt)] = 0
  #FertProDt <- purrr::map_at(.x = FertProDt,.at = unique(fertilizer_elements$factorType), .f = as.numeric) %>% as.data.frame(stringsAsFactors=FALSE)
  
  out <- nutrientApplicationRates(contents, rr)   

  
}



