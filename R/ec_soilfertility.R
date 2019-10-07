#' Get exp conditions , soil and fertilkity tab
#' @description get all input values related to the experiment conditions \code{soil and fertility} experiment condition tab, 
#' provided by users when type in AGROFIMS.
#' @param allinputs shiny reactiveValuesList provided by AGROFIMS
#' @param napp \code{numeric} number of applications
#' @author Omar Benites
#'
get_ec_sf <- function(input){
  
  if(length(input$soilfertility_to_collect_field)==0){
   
    lbl <- c("Fertilizer_type", "Fertilizer_amount_kg/ha","Nutrient_amount_kg/ha")
    dt <- data.frame()
    
  } else {
    lbl <- input$soilfertility_to_collect_field
    lbl <-  dplyr::case_when(  lbl =="Fertilizer type"~  "Fertilizer_type",
                               lbl =="Fertilizer amount"  ~ "Fertilizer_amount_kg/ha",   
                               lbl=="Nutrient amount"  ~"Nutrient_amount_kg/ha",
                               lbl=="Timing"  ~ "Fertilizer_timing",
                               lbl=="Number of fertilizer applications"  ~"Fertilizer_application_number",
                               lbl=="Nutrient type"  ~"Nutrient_type",
                               lbl=="Fertilizer application technique"	~"Fertilizer_application_technique",
                               lbl=="Fertilizer implement type"	~"Fertilizer_implement_type",
                               lbl=="Fertilizer notes"	~"Fertilizer_notes",
                               TRUE ~ lbl)
    lbl <- lbl
    
    dt <- as.list(rep("", length(lbl)))
    names(dt) <- lbl
  }
 
  out <- list(dt= dt, lbl= lbl)
  
  ##LABEL FOR TRAITLIST :remove all double underscore from soil_fertility labels
  # lbl<- str_replace_all(string = lbl_soil, pattern = "__[:digit:]+$",replacement = "")
  # lbl <- unique(lbl)
  # out<- list(dt=dt, lbl= lbl)
  # 
  
}


# 
# get_collectable_sf <- function(input){
#   
#   trait <- input$soilfertility_to_collect_field
#   if(!is.null(trait)){
#     #trait_values <- vector("list", length(trait))
#     trait_values <- as.list(rep("", length(trait)))
#     names(trait_values) <- 
#   } else {
#     
#   }
#   
#   
# }

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

# Product/Fertilizer Rates
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

#Product/Fertilizer calculation wrapper function for the DESIGN TAB
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

#Reshape fertilizer/product data
fertilizer_reshape<- function(df){
  
  out<- data.frame(t(as.numeric(df$mProdAmount)),stringsAsFactors = FALSE)
  names(out) <- paste(df$factorType,df$splitFertNum, 1:nrow(df),sep = "_")
  out
}

#Fertilizer calculation for the DESIGN TAB
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

# Fertilizer/Product Rates (output)
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

# Fertilizer/Product Rates (output) for Management Practices
#fertilizerRates_mgmt <- function(fertilizers, treatments) {
NutrientRates_mgmt <- function(fertilizers, treatments) {
  prod_names <- treatments$name
  out <- (fertilizers[,-c(1,2)]*treatments[,-c(1,2)])/100
  out$name <- prod_names
  nc <- ncol(out) 
  out <- out[, c(nc, 1:(nc-1))]
  out
}    

# Fertilizer/Product Rates (output) for Management Practices
#NutrientRates_mgmt <- function(fertilizers, treatments) {

FertilizerRates_mgmt <- function(fertilizers, treatments) {
  
  #out<- vector(mode="list", length = nrow(treatments)) 
  #for(i in 1:nrow(treatments)){
    out <- treatments[ rep(1, nrow(fertilizers)),]/(fertilizers[,-1]/100)
    print("--fertil mgmt--")
    #print(out)
    out <- replace(out, is.na(out), 0) #change nan values by 0
    is.na(out) <- do.call(cbind,lapply(out, is.infinite)) #change inf values by 0
  #}
  out
  #out <- data.table::rbindlist(out) %>% as.data.frame(stringsAsFactors=FALSE)
} 

# FertilizerRates_mgmt <- function(fertilizers, treatments) {
#   
#   out<- vector(mode="list", length = nrow(treatments)) 
#   for(i in 1:nrow(treatments)){
#     out[[i]] <- treatments[i,]  /(fertilizers[i,-1]/100)
#     out[[i]] <- replace(out[[i]], is.na(out[[i]]), 0) #change nan values by 0
#     is.na(out[[i]]) <- do.call(cbind,lapply(out[[i]], is.infinite)) #change inf values by 0
#   }
#   out <- data.table::rbindlist(out) %>% as.data.frame(stringsAsFactors=FALSE)
# } 

# Get nutrient management calculation and metadata
#
get_nutrient_mgmt <- function(allinputs, addId="mgp_nut_1"){
  
  #allinputs <- allinputs #AllInputs()
  indexSoilMagp<- getAddInputId(addId = addId, pattern= "mgp_nut_", replacement="")
  #out<<-get_nutrient_details_magm(allinputs, indexSoilMagp= nutIndexSoilMagp)
  
  dt <- allinputs %>% dplyr::filter(!str_detect(id, "add")) %>%
                      dplyr::filter(!str_detect(id, "button")) %>%
                      dplyr::filter(!str_detect(id, "_sel_factor_")) %>%
                      dplyr::filter(!str_detect(id, "-selectized"))  
  
  #UNIT
  nutUnit <- dt %>% dplyr::filter(str_detect(id,  paste0("^","sfNutUnit","$"))) %>% nth(2)
  
  #Product 
  nutProd <- dt %>% dplyr::filter(str_detect(id,  paste0("^","sfNutrientProduct_"))) 

  
    
  #NUTRIENT SPLIT ##########################################################################
  out_nut_mgmt <- list()
  for(i in seq.int(indexSoilMagp) ){
    
    out1 <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientSplit1_",indexSoilMagp[i])))  %>% nth(2)%>% as.numeric()
    out2 <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientSplit2_",indexSoilMagp[i])))   %>% nth(2)%>% as.numeric()
    out3 <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientSplit3_",indexSoilMagp[i])))   %>% nth(2)%>% as.numeric()
    out4 <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientSplit4_",indexSoilMagp[i])))   %>% nth(2)%>% as.numeric()
    out5 <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientSplit5_",indexSoilMagp[i])))   %>% nth(2)%>% as.numeric()
    out6 <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientSplit6_",indexSoilMagp[i])))   %>% nth(2)%>% as.numeric()
    out7 <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientSplit7_",indexSoilMagp[i])))   %>% nth(2)%>% as.numeric()
    out8 <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientSplit8_",indexSoilMagp[i])))   %>% nth(2)%>% as.numeric()
    out9 <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientSplit9_",indexSoilMagp[i])))   %>% nth(2)%>% as.numeric()
    out10 <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientSplit10_",indexSoilMagp[i]))) %>% nth(2)%>% as.numeric()
    out11 <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientSplit11_",indexSoilMagp[i]))) %>% nth(2)%>% as.numeric()
    out12 <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientSplit12_",indexSoilMagp[i]))) %>% nth(2)%>% as.numeric()
    out13 <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientSplit13_",indexSoilMagp[i]))) %>% nth(2)%>% as.numeric()
    out14 <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientSplit14_",indexSoilMagp[i]))) %>% nth(2)%>% as.numeric()
    
    out_nut_mgmt[[i]] <-data.frame(out1,out2,out3,out4,out5,out6,out7,out8,out9,out10,out11,out12,out13,out14, stringsAsFactors = FALSE)    
    names(out_nut_mgmt[[i]]) <-  c("N", "P", "K", "Ca", "Mg", "S" , "Mb" , "Zn", "B", "Cu", "Fe", "Mn", "Ni","Cl" )
  }  
  nut_mgmt<-data.table::rbindlist(out_nut_mgmt) %>% as.data.frame(stringsAsFactors=FALSE)
  #nut_mgmt$N <- c(25,36)
  treatments<-nut_mgmt
  ##########################################################################################
  
  #NUTRIENT CHOSE PRODUCT #################################################################
  out_prod_mgmt <- list()
  
  indexSoilProd <- lapply(1:length(nutProd$values), function(x) strsplit(nutProd$values, ", ")[[x]] %>% seq.int())
  indexCum <- lapply( 1:length(indexSoilMagp), function(x)paste0(indexSoilMagp[x], "_", indexSoilProd[[x]]) ) %>%  unlist()
  for(i in seq.int(indexCum) ){
    
    #for(j in 1:length(indexSoilProd[i]) ){
    splitId <- indexCum[i] %>% str_replace("_.","")
    prodname <- dt %>% dplyr::filter(str_detect(id,  paste0("nutrientProductSplit0_",indexCum[i]  ))) %>% nth(2) 
    out1 <- dt %>% dplyr::filter(str_detect(id,  paste0("nutrientProductSplit1_", indexCum[i] ))) %>% nth(2) %>% as.numeric()
    out2 <- dt %>% dplyr::filter(str_detect(id,  paste0("nutrientProductSplit2_",indexCum[i] ))) %>% nth(2)%>% as.numeric()
    out3 <- dt %>% dplyr::filter(str_detect(id,  paste0("nutrientProductSplit3_",indexCum[i] ))) %>% nth(2)%>% as.numeric()
    out4 <- dt %>% dplyr::filter(str_detect(id,  paste0("nutrientProductSplit4_",indexCum[i] ))) %>% nth(2)%>% as.numeric()
    out5 <- dt %>% dplyr::filter(str_detect(id,  paste0("nutrientProductSplit5_",indexCum[i] ))) %>% nth(2)%>% as.numeric()
    out6 <- dt %>% dplyr::filter(str_detect(id,  paste0("nutrientProductSplit6_",indexCum[i]  ))) %>% nth(2)%>% as.numeric()
    out7 <- dt %>% dplyr::filter(str_detect(id,  paste0("nutrientProductSplit7_",indexCum[i]  ))) %>% nth(2)%>% as.numeric()
    out8 <- dt %>% dplyr::filter(str_detect(id,  paste0("nutrientProductSplit8_",indexCum[i]  ))) %>% nth(2)%>% as.numeric()
    out9 <- dt %>% dplyr::filter(str_detect(id,  paste0("nutrientProductSplit9_", indexCum[i] ))) %>% nth(2)%>% as.numeric()
    out10 <- dt %>% dplyr::filter(str_detect(id,  paste0("nutrientProductSplit10_",indexCum[i]  ))) %>% nth(2)%>% as.numeric()
    out11 <- dt %>% dplyr::filter(str_detect(id,  paste0("nutrientProductSplit11_",indexCum[i] ))) %>% nth(2)%>% as.numeric()
    out12 <- dt %>% dplyr::filter(str_detect(id,  paste0("nutrientProductSplit12_",indexCum[i] ))) %>% nth(2)%>% as.numeric()
    out13 <- dt %>% dplyr::filter(str_detect(id,  paste0("nutrientProductSplit13_",indexCum[i] ))) %>% nth(2)%>% as.numeric()
    out14 <- dt %>% dplyr::filter(str_detect(id,  paste0("nutrientProductSplit14_",indexCum[i] ))) %>% nth(2)%>% as.numeric()
    
    out_prod_mgmt[[i]] <-data.frame(splitId, prodname, out1,out2,out3,out4,out5,out6,out7,out8,out9,out10,out11,out12,out13,out14, stringsAsFactors = FALSE)    
    #names(out_prod_mgmt[[j]]) <-  c("name", "N", "P", "K", "Ca", "Mg", "S" , "Mb" , "Zn", "B", "Cu", "Fe", "Mn", "Ni","Cl" )
    #}
  }  
  
  prod_mgmt <- data.table::rbindlist(out_prod_mgmt) %>% as.data.frame(stringsAsFactors=FALSE)
  #prod_mgmt$N <- c(90,89)
  fertilizers <- prod_mgmt
  names(fertilizers) <-  c("splitId","name", "N", "P", "K", "Ca", "Mg", "S" , "Mb" , "Zn", "B", "Cu", "Fe", "Mn", "Ni","Cl" )
  
  #outrate<- try({fertilizerRates_mgmt(fertilizers=fertilizers, treatments= treatments)})
  outrate<- list()
  for(i in seq.int(unique(prod_mgmt$splitId))  ){
    fertilizers2 <- fertilizers %>% dplyr::filter(splitId==i) %>% dplyr::select(-splitId)
    treatments2 <- treatments[i,]
    print("--fert y treat 2")
    print(fertilizers2)
    print(treatments2)
    
    outrate[[i]] <- try({FertilizerRates_mgmt(fertilizers=fertilizers2, treatments= treatments2)})
    print("--ourate---")
    print(outrate[[i]])
    
  }
  outrate<- data.table::rbindlist(outrate) %>% as.data.frame(stringsAsFactors=FALSE)
  #outrate<- try({FertilizerRates_mgmt(fertilizers=fertilizers, treatments= treatments)})
  
  out <- list(outrate= outrate, fertilizers= fertilizers,treatments= treatments )
  
} 

# Get product/fertilizer management
#
get_prodfert_mgmt <- function(allinputs, addId="mgp_pro_1", splitId){
  
  indexSoilMagp <- getAddInputId(addId = addId, pattern= "mgp_pro_", replacement="")
  splitId <- getAddInputId(splitId,"mgp_proidx_", "")
  #out<<-get_nutrient_details_magm(allinputs, indexSoilMagp= nutIndexSoilMagp)
  
  dt <- allinputs %>% dplyr::filter(!str_detect(id, "add")) %>%
                      dplyr::filter(!str_detect(id, "button")) %>%
                      dplyr::filter(!str_detect(id, "_sel_factor_")) %>%
                      dplyr::filter(!str_detect(id, "-selectized"))  
  
  #Product/Fertilizer data
  out_treatment_mgmt <- list()
  for(i in seq.int(splitId)){
    splitIdVal <- splitId[i]
    out0 <- dt %>% dplyr::filter(str_detect(id,  paste0("productProductSplit0_",splitId[i])) )  %>% dplyr::nth(2)
    prodfert <- dt %>% dplyr::filter(str_detect(id,  paste0("txtsfProductValue_",splitId[i])) )  %>% dplyr::nth(2)%>% as.numeric()
    
    out_treatment_mgmt[[i]] <-data.frame(splitIdVal, out0, prodfert, stringsAsFactors = FALSE)    
    names(out_treatment_mgmt[[i]]) <-  c("splitId", "name", "product")
  } 
  treatment_mgmt <- data.table::rbindlist(out_treatment_mgmt) %>% as.data.frame(stringsAsFactors=FALSE)
    
  #Product/Fertilizer split data
  out_prodfersplit_mgmt <- list()
  for(i in seq.int(splitId)){
    splitIdFerVal <- splitId[i]
    out0 <- dt %>% dplyr::filter(str_detect(id,  paste0("productProductSplit0_",splitId[i])) )  %>% nth(2)
    out1 <- dt %>% dplyr::filter(str_detect(id,  paste0("productProductSplit1_",splitId[i])) )  %>% nth(2)%>% as.numeric()
    out2 <- dt %>% dplyr::filter(str_detect(id,  paste0("productProductSplit2_",splitId[i])) )  %>% nth(2)%>% as.numeric()
    out3 <- dt %>% dplyr::filter(str_detect(id,  paste0("productProductSplit3_",splitId[i])) )  %>% nth(2)%>% as.numeric()
    out4 <- dt %>% dplyr::filter(str_detect(id,  paste0("productProductSplit4_",splitId[i])) )  %>% nth(2)%>% as.numeric()
    out5 <- dt %>% dplyr::filter(str_detect(id,  paste0("productProductSplit5_",splitId[i])) )  %>% nth(2)%>% as.numeric()
    out6 <- dt %>% dplyr::filter(str_detect(id,  paste0("productProductSplit6_",splitId[i])) )  %>% nth(2)%>% as.numeric()
    out7 <- dt %>% dplyr::filter(str_detect(id,  paste0("productProductSplit7_",splitId[i])) )  %>% nth(2)%>% as.numeric()
    out8 <- dt %>% dplyr::filter(str_detect(id,  paste0("productProductSplit8_",splitId[i])) )  %>% nth(2)%>% as.numeric()
    out9 <- dt %>% dplyr::filter(str_detect(id,  paste0("productProductSplit9_",splitId[i])) )  %>% nth(2)%>% as.numeric()
    out10 <- dt %>% dplyr::filter(str_detect(id, paste0("productProductSplit10_",splitId[i])) )  %>% nth(2)%>% as.numeric()
    out11 <- dt %>% dplyr::filter(str_detect(id, paste0("productProductSplit11_",splitId[i])) )  %>% nth(2)%>% as.numeric()
    out12 <- dt %>% dplyr::filter(str_detect(id, paste0("productProductSplit12_",splitId[i])) )  %>% nth(2)%>% as.numeric()
    out13 <- dt %>% dplyr::filter(str_detect(id, paste0("productProductSplit13_",splitId[i])) )  %>% nth(2)%>% as.numeric()
    out14 <- dt %>% dplyr::filter(str_detect(id, paste0("productProductSplit14_",splitId[i])) )  %>% nth(2)%>% as.numeric()
    
    out_prodfersplit_mgmt[[i]] <-data.frame(splitIdFerVal, out0, out1,out2,out3,out4,out5,out6,out7,out8,out9,out10,out11,out12,out13,out14, stringsAsFactors = FALSE)    
    names(out_prodfersplit_mgmt[[i]]) <-  c("splitId", "name","N", "P", "K", "Ca", "Mg", "S" , "Mb" , "Zn", "B", "Cu", "Fe", "Mn", "Ni","Cl" )
  }
  prodfert_mgmt <- data.table::rbindlist(out_prodfersplit_mgmt) %>% as.data.frame(stringsAsFactors=FALSE)
  
  out <- list(treatment_mgmt= treatment_mgmt, prodfert_mgmt= prodfert_mgmt)
  
}



