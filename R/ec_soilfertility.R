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


#### Simple case #########################################
getFert <- function(treats, fertilizers, selected) {
  
  treats <- treats %>% arrange_at(1)
  fertilizers <- fertilizers[c("group","name",names(treats))]
  factors <- t(as.matrix(treats))
  f <- fertilizers[fertilizers$name == selected, -c(1:2)]
  f <- as.matrix(f)/100
  x <- as.vector(na.omit(factors / as.vector(f)))
  m <- matrix(x)
  colnames(m) <-selected
  rownames(m) <- paste0("Level-", 1:nrow(m))
  m <- as.data.frame(m,stringsAsFactors=FALSE) %>% tibble::rownames_to_column()
  colnames(m)[1] <- c("Levels")
  m[,1] <- paste0("Level_",names(treats),"_",treats[,1] )
  m
}    
#### End simple case #########################################



############################################ NUTRIENT AND FERTILIZERS ####################################################

# best application rates based on nutrient inputs
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

# Reshape nutrient data
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


#Product calculation wrapper function

product_calculation <- function(allinputs, dfAll, index="1", indexEspLvl=indexEspLvl , design="frcbd"){
  
  #Get labels
  indexEspLvl<- filter_index_espLvl_design(index= index, indexEspLvl=indexEspLvl, design=design, designEspflvl="_lvl_espType_")
  
  #Get especial levels
  indexEspLvl_subfix <- get_index_espLvl_design(indexEspLvl, paste0(design,"_lvl_espType_"))
  lookup <- paste0(design,"_")
  
  #Table of nutrient details (eletype, mNumTiming)-----------------------------------------------------------------------------------------
  nutrients_details <- get_nutrient_details_design(allinputs=allinputs, design=design, index =index, indexEspLvl = indexEspLvl)
  
  #Split nutrient name and split number per each nutrient-------------------------------------------------------------------------------
  split_nutrient_name <- lapply(X = seq.int(dfAll$splits), function(x) rep(dfAll$eleType[x], each =as.numeric(dfAll$splits[x]))) %>% unlist() 
  split_nutrient_number <- lapply(as.integer(dfAll$splits), function(x) seq.int(x)) %>% unlist() 
  nutrients_details <- nutrients_details %>% 
                        mutate(SplitNutName = split_nutrient_name ) %>% 
                        mutate(splitNutNum = split_nutrient_number)
  #Customizeing nutrient_details 
  class(nutrients_details)<-"data.frame"
  nutrient_elements <- nutrients_details[,c("SplitNutName", "nutAmount")]
  nutrient_elements[,2] <- as.numeric(nutrient_elements[,2] ) 
   
  ## Get nutrient tables per each sub level #############################################
  out_nut<- vector(mode="list", nrow(dfAll))
  lookup <- paste0("^",design,"_")
  for (i in 1:nrow(dfAll)){
    
    level <- dfAll[i,1]
    type <- dfAll[i,2]
    levels <- dfAll[i,3]
    unit <- dfAll[i,4]
    index <- dfAll[i,5]
    
    #namefert <-  allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"factorType_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)
    out1 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputNutDT1_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out1 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputNutDT1_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out2 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputNutDT2_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out3 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputNutDT3_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out4 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputNutDT4_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out5 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputNutDT5_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out6 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputNutDT6_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out7 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputNutDT7_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out8 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputNutDT8_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out9 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputNutDT9_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out10 <- allinputs %>% dplyr::filter(str_detect(id, paste0(lookup,"outputNutDT10_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out11 <- allinputs %>% dplyr::filter(str_detect(id, paste0(lookup,"outputNutDT11_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out12 <- allinputs %>% dplyr::filter(str_detect(id, paste0(lookup,"outputNutDT12_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out13 <- allinputs %>% dplyr::filter(str_detect(id, paste0(lookup,"outputNutDT13_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out14 <- allinputs %>% dplyr::filter(str_detect(id, paste0(lookup,"outputNutDT14_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    
    out_nut[[i]] <-data.frame(out1,out2,out3,out4,out5,out6,out7,out8,out9,out10,out11,out12,out13,out14, stringsAsFactors = FALSE)    
    names(out_nut[[i]]) <-  c("N", "P", "K", "Ca", "Mg", "S" , "Mb" , "Zn", "B", "Cu", "Fe", "Mn", "Ni","Cl" )
    
  }
  nutfert <- data.table::rbindlist(out_nut) %>% as.data.frame()
  temp4 <- list()
  for(i in 1:nrow(dfAll)){
    splits <- as.numeric(dfAll$splits)[i]
    temp4[[i]] <- nutfert[rep(i,splits), ]
    #sapply(df, rep.int,2) %>% as.data.frame(stringsAsFactors=FALSE) method 2
  }
  nutfert <- data.table::rbindlist(temp4) %>% as.data.frame()
  nutfert$name <- paste(nutrients_details$mNutProduct)
  nutfert <- nutfert[,c(15, 1:14)]
  names(nutfert) <- c("name", "Nitrogen","Phosphorus","Potassium","Calcium","Magnesium",  
                      "Sulfur", "Moldbenum", "Zinc", "Boron", "Copper","Iron", "Manganese", "Nickel", "Chlorine")
  fertilizers <- nutfert
  ##End Get nutrient tables per each sub level ###########################################
  
  
  #Get treatments ######################################################################
  treatments <- as.list(as.numeric(nutrients_details$nutAmount))
  names(treatments) <- nutrients_details$SplitNutName
  #End get treatments##################################################################
  
  
  
  
  out <- fertilizerRates(fertilizers= fertilizers, treatments = treatments)  
  
  out <- unlist(out)
  nutSplitNames <- paste(nutrients_details$SplitNutName,nutrients_details$nutAmount,nutrients_details$splitNutNum, sep="_")
  
  fertRate <- data.frame(Product=nutrients_details$mNutProduct, Nutrient=nutSplitNames ,Rates = out)
  
  
  #temp3 <- list()
  #for(i in 1:nrow(dfAll)){
  #  splits <- as.numeric(dfAll$splits)[i]
  #  temp3[[i]] <- dfAll[rep(i,splits), ]
  #  temp3[[i]]$Psplits<- 1:splits 
  #  #sapply(df, rep.int,2) %>% as.data.frame(stringsAsFactors=FALSE) method 2
  #}
  #temp3 <- data.table::rbindlist(temp3) %>% as.data.frame(stringsAsFactors=FALSE)
  #out2 <- temp3 %>% dplyr::mutate(Nutrient = paste0(eleType,"_",levels, "_", Psplits )) 
  #out2$Rates <- unlist(out)
  #out2 <- out2[,c("Nutrient", "Rates" )]
  #names(out2) <- c("Nutrient", "Rates")
  #out2
  
}

# product_calculation <- function(allinputs, dfAll, index="1", indexEspLvl=indexEspLvl , design="frcbd"){
#   
#   #Get labels
#   indexEspLvl<- filter_index_espLvl_design(index= index, indexEspLvl=indexEspLvl, design=design, designEspflvl="_lvl_espType_")
#   
#   #Get especial levels
#   indexEspLvl_subfix <- get_index_espLvl_design(indexEspLvl, paste0(design,"_lvl_espType_"))
#   lookup <- paste0(design,"_")
# 
#   #Table of nutrient details (eletype, mNumTiming)-----------------------------------------------------------------------------------------
#   nutrients_details <- get_nutrient_details_design(allinputs=allinputs, design=design, index =index, indexEspLvl = indexEspLvl)
#   
#   ## Split nutrient name and split number per each nutrient-------------------------------------------------------------------------------
#   split_nutrient_name <- lapply(X = seq.int(dfAll$splits), function(x) rep(dfAll$eleType[x], each =as.numeric(dfAll$splits[x]))) %>% unlist() 
#   split_nutrient_number <- lapply(as.integer(dfAll$splits), function(x) seq.int(x)) %>% unlist() 
#   nutrients_details <- nutrients_details %>% 
#                                  mutate(SplitNutName = split_nutrient_name ) %>% 
#                                  mutate(splitNutNum = split_nutrient_number)
#   ## 
#   print("5")
#   class(nutrients_details)<-"data.frame"
#   nutrient_elements <- nutrients_details[,c("SplitNutName", "nutAmount")]
#   nutrient_elements[,2] <- as.numeric(nutrient_elements[,2] ) 
#   
#   treatments <-  nutrient_reshape(nutrient_elements, "SplitNutName", "nutAmount")
#   #ASIGNAR 0 A LOS NA
#   #Ref: https://stackoverflow.com/questions/20535505/replacing-all-missing-values-in-r-data-table-with-a-value
#   treatments[is.na(treatments)] = 0
# 
# 
#   print("7")
#   FertProDt<- lapply(seq.int(nutrients_details$mNutProduct), function(x) dt_fernut %>% dplyr::filter(name %in% nutrients_details$mNutProduct[x]) )
#   FertProDt <- data.table::rbindlist(FertProDt,fill = TRUE,use.names = TRUE)
#   names(FertProDt) <- c("group", "name", "Nitrogen","Phosphorus","Potassium","Calcium","Magnesium",  
#                         "Sulfur", "Moldbenum", "Zinc", "Boron", "Copper","Iron", "Manganese", "Nickel", "Chlorine")
#   FertProDt<- tibble::as_tibble(FertProDt) 
#   FertProDt <- FertProDt[,c("name",unique(nutrient_elements$SplitNutName))]
#   FertProDt[is.na(FertProDt)] = 0
#   FertProDt <- purrr::map_at(.x = FertProDt,.at = unique(nutrient_elements$SplitNutName), .f = as.numeric) %>% as.data.frame(stringsAsFactors=FALSE)
#   
#   
#   ##TODO:: TRANSFORMAR A NUMERICO ambas matrices
#   price <- rep(1, nrow(FertProDt) )
# 
#   #New function based on Robert's script
#   fertilizer_table <- dt_fernut
#   names(fertilizer_table) <- c("group", "name", "Nitrogen","Phosphorus","Potassium","Calcium","Magnesium",  
#                         "Sulfur", "Moldbenum", "Zinc", "Boron", "Copper","Iron", "Manganese", "Nickel", "Chlorine")
#   
#   r <- getFert(treats = treatments, fertilizers = fertilizer_table, selected =  unique(FertProDt$name))
#   r
# 
# }


#Reshape fertilizer data
fertilizer_reshape<- function(df){
  
  out<- data.frame(t(as.numeric(df$mProdAmount)),stringsAsFactors = FALSE)
  names(out) <- paste(df$factorType,df$splitFertNum, 1:nrow(df),sep = "_")
  out
}

#Fertilizer calculation
fertilizer_calculation<- function(allinputs, dfAll, index="1", indexEspLvl=indexEspLvl , design="frcbd"){
  
  
  #Get labels
  indexEspLvl<- filter_index_espLvl_design(index= index, indexEspLvl=indexEspLvl, design=design, designEspflvl="_lvl_espType_")
  
  #Get especial levels
  indexEspLvl_subfix <- get_index_espLvl_design(indexEspLvl, paste0(design,"_lvl_espType_"))
  #lookup <- paste0(design,"_")
  
  #Table of nutrient details (eletype, mNumTiming)-----------------------------------------------------------------------------------------
  fertilizer_details <- get_fertilizer_details_design(allinputs, design, index, indexEspLvl)
  class(fertilizer_details)<- "data.frame"
  
  ## Split nutrient name and split number per each nutrient-------------------------------------------------------------------------------
  split_fert_name <- lapply(X = seq.int(dfAll$splits), function(x) rep(dfAll$eleType[x], each =as.numeric(dfAll$splits[x]))) %>% unlist() 
  split_fert_number <- lapply(as.integer(dfAll$splits), function(x) seq.int(x)) %>% unlist() 
  fertilizer_details <- fertilizer_details %>% 
                        mutate(splitFertNum = split_fert_number) %>% 
                        mutate(SplitFertName = split_fert_name ) 
  fertilizer_details[,"mProdAmount"] <- as.numeric(fertilizer_details[,"mProdAmount"]) 
  
  ## NEW CODE: GET FERTILIZER_AVAILABLE TABLE
  #First input
  # fact <- lapply(seq.int(unique(fertilizer_details$factorType)), 
  #                function(x) fertilizer_details %>% dplyr::filter(factorType==fertilizer_details$factorType[x]) 
  #                %>% dplyr::select(mProdAmount) %>% nth(1)) 
  # names(fact) <- unique(fertilizer_details$factorType)
  ## END NEW CODE
  
  ## FACT (TREATMENT): Fertilizer Application Product
  fact<- as.list(as.numeric(fertilizer_details$mProdAmount))
  names(fact) <- paste(fertilizer_details$factorType,fertilizer_details$mProdAmount,
                       fertilizer_details$splitFertNum, sep="_")
  
  
  ## NEW CODE: GET FERTILIZER_AVAILABLE TABLE
  out_fert<- vector(mode="list", nrow(dfAll))
  lookup <- paste0("^",design,"_")
  for (i in 1:nrow(dfAll)){
    
    level <- dfAll[i,1]
    type <- dfAll[i,2]
    levels <- dfAll[i,3]
    unit <- dfAll[i,4]
    index <- dfAll[i,5]
    print(paste(level,index,i))
    
    #frcbd_factorType_1_1_1
    namefert <-  allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"factorType_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)
    out1 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputFerDT1_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out1 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputFerDT1_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out2 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputFerDT2_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out3 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputFerDT3_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out4 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputFerDT4_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out5 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputFerDT5_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out6 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputFerDT6_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out7 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputFerDT7_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out8 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputFerDT8_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out9 <- allinputs %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputFerDT9_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out10 <- allinputs %>% dplyr::filter(str_detect(id, paste0(lookup,"outputFerDT10_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out11 <- allinputs %>% dplyr::filter(str_detect(id, paste0(lookup,"outputFerDT11_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out12 <- allinputs %>% dplyr::filter(str_detect(id, paste0(lookup,"outputFerDT12_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out13 <- allinputs %>% dplyr::filter(str_detect(id, paste0(lookup,"outputFerDT13_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    out14 <- allinputs %>% dplyr::filter(str_detect(id, paste0(lookup,"outputFerDT14_",level,"_",index,"_",i,"$"))) %>% dplyr::nth(2)%>% as.numeric()
    
    out_fert[[i]] <-data.frame(namefert, out1,out2,out3,out4,out5,out6,out7,out8,out9,out10,out11,out12,out13,out14, stringsAsFactors = FALSE)    
    names(out_fert[[i]]) <-  c("name","N", "P", "K", "Ca", "Mg", "S" , "Mb" , "Zn", "B", "Cu", "Fe", "Mn", "Ni","Cl" )
    
  }
  
  fert_available<- data.table::rbindlist(out_fert) %>% as.data.frame(stringsAsFactors=FALSE)
  #fert_available$name <- paste(fert_available$name, dfAll$levels, dfAll$splits, sep = "_")
  
  temp <- list()
  for(i in 1:nrow(dfAll)){
    splits <- as.numeric(dfAll$splits)[i]
    temp[[i]] <- fert_available[rep(i,splits), ]
    #sapply(df, rep.int,2) %>% as.data.frame(stringsAsFactors=FALSE) method 2
  }
  fert_available<- data.table::rbindlist(temp) %>% as.data.frame(stringsAsFactors=FALSE)
  
  fert_available$name <- paste(fert_available$name, fertilizer_details$mProdAmount,
                               fertilizer_details$splitFertNum, sep = "_")
  ## END NEW CODE : GET FERTILIZER_AVAILABLE TABLE
  
  ### FORMER CODE
  # fert_available <- dt_fernut %>% dplyr::filter(name %in% unique(fertilizer_details$factorType)) 
  # names(fert_available) <- c("group", "name", "Nitrogen","Phosphorus","Potassium","Calcium","Magnesium",  
  #                            "Sulfur", "Moldbenum", "Zinc", "Boron", "Copper","Iron", "Manganese", "Nickel", "Chlorine")
  # fert_available <- tibble::as_tibble(fert_available) %>% dplyr::select(-group)
  ### END CODE
  
  
  #Output
  out <- nutrientRates(fert_available, fact)
  for(i in seq.int(out)){
    #rownames(out[[i]]) <- paste0(names(out)[i],"_",rownames(out[[i]]))
    rownames(out[[i]]) <- paste0(names(out)[i])
  }
  out <- lapply(seq.int(out), function(x) as.data.frame(out[[x]],stringsAsFactors=FALSE))
  out <- lapply(seq.int(out), function(x) tibble::rownames_to_column(out[[x]]) )
  out <- data.table::rbindlist(out,use.names = TRUE)
  out <- as.data.frame(out, stringsAsFactors=FALSE)
  
}


# Nutrient Rates (output)
nutrientRates <- function(fertilizers, treatments) {
  m <- match(names(treatments), fertilizers$name)
  #stopifnot(!any(is.na(m)))
  fertilizers <- fertilizers[m, ]
  f <- as.matrix(fertilizers[,-1]) # for easy multiplication
  out <- list()
  for (i in 1:length(treatments)) {
    treat <- unlist(treatments[[i]])
    r <- f[rep(i, length(treat)), ,drop=FALSE] * treat / 100
    rownames(r) <- round(treat,1)
    out[[ fertilizers$name[i] ]] <- r
  }
  out
}    

# Fertilizer Rates (output)
fertilizerRates <- function(fertilizers, treatments) {
  out <- list()
  for (i in 1:length(treatments)) {
    j <- colnames(fertilizers)  == names(treatments[i])
    #stopifnot(any(j))  # no match
    fert <- fertilizers[i,j]
    k <- which(fert > 0)
    #stopifnot(length(k)==1) # one, and only one, product must have the nutrient!
    out[[ i ]] <- unlist(treatments[i]) / (fert[k] / 100)
    if(length(out[[ i ]])==0 ){ out[[ i ]]<- "No-Value"  }
    names(out[[i]])<-NULL
  }
  names(out) <- names(treatments)
  out
}    





# Fertilizer Rates (output)
fertilizerRates_mgmt <- function(fertilizers, treatments) {
  out <- fertilizers[,-1]*treatments
}    




