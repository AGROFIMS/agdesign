# Create experimental design in AGROFIMS
# 
# @description create experimental design in agrofims through user interface inputs
# @param design experimental design abbreviation.
# @param rep replication
# @param block blocks
# @param trt treatments
# @param ntrt number of treatments
# @param fnames list of factors names
# @param flevels list of levels according to previous factors
# @param mplot main plot
# @param splot sub plot
# @param ssplot sub-sub plot
# @param rowf row factor
# @param colf col factor
# @export

fbdesign_agrofims <- function(design, rep=2, block=2, trt=2, ntrt=NULL, 
                               fnames=NULL,flevels=NULL,
                               mplot=NULL, splot=NULL, ssplot=NULL,
                               rowf=NULL, colf=NULL){
            
            if (design == "crd") {
              fb<- try(st4gi::cr.crd(geno = trt,nrep = rep)$book)
              names(fb)<- c("PLOT", "ROW","COL","TREATMENT")
            }
            
            if (design == "rcbd") {
              fb<- try(st4gi::cr.rcbd(geno = trt,nb = block)$book)
              names(fb) <- c("PLOT","ROW","COL","BLOCK","TREATMENT")
            }
            
            if (design == "fcrd") { ##factorial crd
              fb <- try(st4gi::cr.f(fnames = fnames, flevels = flevels, design = "crd", nrep = rep)$book)
              names(fb)[1:4] <- c("PLOT","ROW","COL","TREATMENT") #rename first 4 cols
            }
            
            if (design == "frcbd") { ##factorial rcbd
              #print(fnames)
              #print(flevels)
              fb <- try(st4gi::cr.f(fnames = fnames, flevels = flevels, design = "rcbd", nrep = rep)$book)
              names(fb)[1:5] <- c("PLOT","BLOCK" ,"ROW","COL","TREATMENT")  #rename first 5 cols
            }
            
            if (design == "sprcbd") { #split plot rcbd
              fb <- try(st4gi::cr.spld(fnames = fnames ,flevels = flevels ,nb = block)$book )
              names(fb)[1:6] <- c("BLOCK" ,"PLOT","SUBPLOT","ROW","COL","TREATMENT")  #rename first 5 cols
              
            } #R.Eyzaguirre recommends just one Split Design
            
            if (design == "spsp") { #split-split plot
              fb <- try(st4gi::cr.spld(fnames = fnames ,flevels = flevels ,nb = block)$book )
              names(fb)[1:7]<- c("BLOCK" ,"PLOT","SUBPLOT","SUB-SUB-PLOT","ROW","COL","TREATMENT")
            }
            
            if (design == "strip") { #strip plot
              fb <- try(st4gi::cr.strd(A = flevels[[1]],B = flevels[[2]],nb = block)$book )
              names(fb)[1:7] <- c("PLOT", "BLOCK", "ROW","COL",fnames[1],fnames[2],"TREATMENT")
            }
            #print(fb)
            fb
          
}

## Get factors from design tab ############################################################
get_factors_design <- function(allinputs, index=NULL, design="fcrd",duplicate= TRUE){
  
  
  #Look up patterns for factors
  lookup <- paste0("^", design,"_sel_factor_")
  
  #Filter information to find out factors
  dt <- allinputs %>%  filter(!str_detect(id, "button")) %>%
                        filter(!str_detect(id, "-selectized")) %>%
                        filter(!str_detect(id, "other")) %>%
                        filter(str_detect(id, lookup))
  if(!is.null(index)){
    #Arrange by order
    dt <- arrange_by_pattern(dt, pattern = index)
  }
  
  
  #Other factor 
  out<- dt$values
  
  #Replace other factor by the ones that users type in
  for(i in 1:length(out)){
    if(out[i]=="Other"){
      of <- allinputs %>%  filter(str_detect(id, paste0(lookup,"other_",index[i])))
      out[i]<- of$values
    }
  }
  
  if(duplicate){
    out<- paste0(out,"_f", 1:length(out))
    out <- stringr::str_replace_all(out,pattern = "[[:space:]]",replacement = " ")
  }
  
  out
}


## Get levels from desigin tab
# allinputs: reactiveTable
# index : vector of indexes
# factors: vector of factors
# design: design
# data dictionary: data dictionary for FACTOR_V10
# format: list (default)
get_levels_design <- function(allinputs, index, indexEspLvl=NULL, factors, design="fcrd", 
                              data_dictionary=NULL, format=c("list","data.frame")){
  
  format<- match.arg(format)
  
  factors <- stringr::str_replace_all(string = factors,pattern = "_f[:digit:]",replacement = "")
  factors <- stringr::str_replace_all(string = factors,pattern = "_",replacement = " ")
  #print(factors)
  
  lookup<- paste0("^",design,"_lvl_")
  dt <- allinputs %>%   dplyr::filter(!str_detect(id, "add")) %>%
                              dplyr::filter(!str_detect(id, "button")) %>%
                              dplyr::filter(!str_detect(id, "unit")) %>% 
                              dplyr::filter(!str_detect(id, "_sel_factor_")) %>%
                              dplyr::filter(!str_detect(id, "-selectized")) %>%  
                              dplyr::filter(str_detect(id, lookup))
        
      #Arrange by order
      dt <- arrange_by_pattern(dt, pattern = index)
      
      out <- vector(mode="list",length = length(factors))
      a<-u<-NULL
        
        for(i in 1:length(factors)){
          
          # For timing factors
          if(stringr::str_detect(factors[i],pattern="timing")){
            print("Timing factor")
              # for(i in seq.int(factors)){
                out[[i]] <- get_timing_levels(allinputs, index= index[i], factors[i], design=design,
                                              data_dictionary=data_dictionary)
              #}
          }
          
          else if(factors[i]=="Crop residue amount" || factors[i]== "Irrigation amount"){
            
            print("crop residue amount")
            out[[i]] <- get_amountype_levels(allinputs, index= index[i], indexEspLvl = indexEspLvl,  factors[i], design=design,
                                             data_dictionary=data_dictionary)
            
          }
          
          else if(stringr::str_detect(factors[i],pattern="type and amount")){
            
            print("type and amount factor")
            
            out[[i]] <- get_amountype_levels(allinputs, index= index[i], indexEspLvl = indexEspLvl,  factors[i], design=design,
                                              data_dictionary=data_dictionary)
            
          }
          #General cases
          else {
                 
            out[[i]]<- dt %>% dplyr::filter(str_detect(id, paste0(lookup, index[i]))) %>% distinct()
            if(factors[i]==""){
              out[[i]] <- c("","","")
            }
            else {
              temp <- data_dictionary %>% filter(FACTOR==factors[i]) 
              
              ## Si es un factor estadarizado, entonces temp debe tener al menos una fila ----------------
              if(nrow(temp)>0){
                #When lenght(out) is equal to 1 ---> form is text input or combo.
                form<- data_dictionary %>% filter(FACTOR==factors[i]) %>% dplyr::select(FORM)
                form<- form$FORM   
                form <- stringi::stri_trim_both(str = form)
                
                if(form=="date"){
                  out[[i]] <- out[[i]] %>%  dplyr::filter(str_detect(id,  "date" ))
                  out[[i]] <-  out[[i]]$values
                } 
                else { 
                  #Avoid _1_1 ---> "$"
                  out[[i]]<- dt %>% dplyr::filter(str_detect(id, paste0(lookup, index[i],"$")))
                  
                  out[[i]] <- out[[i]] %>%  dplyr::filter(!str_detect(id,  "date" ))
                  out[[i]] <-  out[[i]]$values
                  out[[i]] <- strsplit(out[[i]],split= ",")[[1]]
                  out[[i]] <- stringi::stri_trim_both(str = out[[i]])
                  
                  #Detect Others
                  if( nrow(dt %>% dplyr::filter(str_detect(id, paste0(lookup,"other_", index[i] ))))>=1)  {
                    
                    a <- dt %>% dplyr::filter(str_detect(id, paste0(lookup,"other_",index[i])))
                    a<- a$values
                    a <- strsplit(a,split= ",")[[1]]
                    a <- stringi::stri_trim_both(str = a)
                    out[[i]]<- append(out[[i]], a)
                    out[[i]]<- setdiff(out[[i]],"Other") #remove other value from vector
                  }
                  
                  #Detect Units
                  if( nrow(allinputs %>% dplyr::filter(str_detect(id, paste0(lookup,"unit_", index[i],"$" ))))>=1 ){
                    
                    u<- allinputs %>% dplyr::filter(!str_detect(id, "-selectized")) %>%
                      dplyr::filter(str_detect(id,  paste0(lookup, "unit_",index[i],"$" ) ))
                    u<- u$values
                    out[[i]]<- paste0(out[[i]]," ",u) #quantity + whitespace + unit
                  }
                  
                  #We place underscore in `pattern` because the factor's names include underscore
                  if(stringr::str_detect(factors[i],pattern="_application_rate")){ #special case for product, nutrient and oxidzed
                    # 95, 96 y 97 from FACTOR_V10-DRAFT
                    print("application rate")
                    fert<- allinputs %>% dplyr::filter(!str_detect(id, "-selectized")) %>%
                    dplyr::filter(str_detect(id,  paste0(lookup, "fert_",index[i]) ))
                    fert<- fert$values
                    out[[i]]<- paste0(fert," ",out[[i]]) #quantity + whitespace + unit
                    
                  }
                  
                  
                  if(nrow(allinputs %>% dplyr::filter(str_detect(id,  paste0("^",design,"factor","_crop_input",index[i],"$"))) )>=1)
                  {
                    print("crop in design")
                    crop <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",design,"factor","_crop_input",index[i],"$"))) %>% 
                                          dplyr::nth(2)
                    #print(crop)
                    
                    if( length(crop) && crop!=""){
                      #crop <- crop$values
                      out[[i]] <- paste0(crop,"_",out[[i]])
                    }else{
                      out[[i]] <- out[[i]]
                    }
                    
                  }
                    
                }
                
              } 
              ## El factor no esta estandirizado cuando el factor no fue encontrado (no hay filas)
              else { #Si no tiene al menos una fila, este debe ser un OTHER FACTOR
                othFacType <- allinputs %>% dplyr::filter(!str_detect(id, "-selectized")) %>%  
                  dplyr::filter(str_detect(id, paste0("^",design,"_typeInput_",index[i])))  
                othFacType <- othFacType$values
                
                if(othFacType=="date"){
                  out[[i]] <- out[[i]] %>%  dplyr::filter(str_detect(id,  "date" ))
                  out[[i]] <-  out[[i]]$values
                }
                else{
                  out[[i]] <- out[[i]] %>%  dplyr::filter(!str_detect(id,  "date" )) 
                  out[[i]]<- strsplit(out[[i]]$values,split= ",")[[1]]
                  out[[i]] <- stringi::stri_trim_both(str = out[[i]])
                  
                  #Detect Units
                  if( nrow(allinputs %>% dplyr::filter(str_detect(id, paste0(lookup,"unit_", index[i] ))))>=1 ){
                    
                    u<- allinputs %>% dplyr::filter(!str_detect(id, "-selectized")) %>%
                      dplyr::filter(str_detect(id,  paste0(lookup, "unit_",index[i] ) ))
                    u<- u$values
                    out[[i]]<- paste0(out[[i]]," ",u) #quantity + whitespace + unit
                  }
                  
                }
                
              }
            }
            
              
           }
          
        }
        
      if(format=="data.frame"){
          print("tranform to data.frame")
      }
        
      out
  
}

# Get level for non-full factorial designs
get_nonfactorial_levels <- function(input,design){
  
  if(design=="crd"){
     n<- as.integer(input$crd_ntrt) 
     out <- NULL
     for(i in 1:n){
       out[i]<- input[[paste0("ui_NFF_summ_crd_", i)]]  
     }
  }else{
    n <- as.integer(input$rcbd_ntrt)
    out <- NULL
    for(i in 1:n){
      out[i]<- input[[paste0("ui_NFF_summ_rcbd_", i)]]  
    }
  }
  out
}


# Get levels for special factors : timing and amount/type factors
# ADVICE: NOT VECTORIZED, should be included in the main class get_levels_design
get_timing_levels <- function(allinputs, index="1", factors, design="fcrd", 
                               data_dictionary=NULL){
  
  #Remove underscore from factors
  factors <- stringr::str_replace_all(string = factors,pattern = "_f[:digit:]",replacement = "")
  factors <- stringr::str_replace_all(string = factors,pattern = "_",replacement = " ")

  #Look up for pattern
    lookup<- paste0("^",design,"_lvltiming") #Timing factor case : _lvltiming_

  dt <- allinputs %>%   dplyr::filter(!str_detect(id, "add")) %>%
                        dplyr::filter(!str_detect(id, "button")) %>%
                        dplyr::filter(!str_detect(id, "unit")) %>% 
                        dplyr::filter(!str_detect(id, "_sel_factor_")) %>%
                        dplyr::filter(!str_detect(id, "-selectized")) %>%  
                        dplyr::filter(str_detect(id, lookup))
  
  out <- vector(mode="character",length = length(factors))
  tproc<- vector(mode="character",length = length(factors))
  

  #Type of timing procedure
  tproc <- allinputs %>% dplyr::filter(!str_detect(id, "-selectized")) %>%
                          dplyr::filter(str_detect(id,  paste0(lookup,"_",index,"$") ))
  tproc <- tproc$values
  
  
  ## Case: Days after plating, Grow stage, Frequency: -------------------------------------------------------------
  if(tproc!="Date"){
          ##Pattern: lookup_timing_index[i]_"1"
          out <-  dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"Value","_",index,"_1$") ))
          out<- strsplit(out$values,split= ",")[[1]]
          out <- stringi::stri_trim_both(str = out)
  } 
  else {
          #Number of dates
          numdate <- allinputs %>%  dplyr::filter(str_detect(id,  paste0("^",design, "_numLevelsTimingESP_",index,"$")))
          numdate <- as.integer(numdate$value)
          
          ## For date that includes number of dates -------------------------------------
          out_date <- character(numdate)
          for(j in seq.int(numdate)){
            res <-  dt %>% dplyr::filter(str_detect(id,  paste0(lookup, "Value_",index,"_",j,"$")))
            out_date[j] <- res$values
          }
          out<- out_date
  }

  #print(tproc)
  #print(out)
  out
}


# Get levels for amount and type factors
# ADVICE: NOT VECTORIZED, should be included in the main class get_levels_design
# allinputs: All input values from AGROFIMS sessions
# index: design-factor index. Only one value is allowed.
# indexLvl: index's levels 
# factors: factor names
# design: design 
# data_dictionary: data dictionary from AGROFIMS
# 
get_amountype_levels <- function(allinputs, index, indexEspLvl=NULL, factors, design="fcrd", 
                                 data_dictionary=NULL){
  
  #Factor's name
  factors <- stringr::str_replace_all(string = factors,pattern = "_f[:digit:]",replacement = "")
  factors <- stringr::str_replace_all(string = factors,pattern = "_",replacement = " ")
  
  #Filter index from special factors and levels
  indexEspLvl <- filter_index_espLvl_design(index = index, indexEspLvl= indexEspLvl, design=design, designEspflvl="_lvl_espType_")
  indexEspLvl <- get_index_espLvl_design(indexEspLvl, paste0("^",design,"_lvl_espType_",index,"_"))  #"frcbd_lvl_espType_2_")
  
  #Lookup design pattern
  lookup <- paste0("^",design,"_")
  dt <- allinputs %>% dplyr::filter(!str_detect(id, "add")) %>%
                      dplyr::filter(!str_detect(id, "button")) %>%
                      #dplyr::filter(!str_detect(id, "unit")) %>%  ##Contemplate Unit case
                      dplyr::filter(!str_detect(id, "_sel_factor_")) %>%
                      dplyr::filter(!str_detect(id, "-selectized")) %>%  
                      dplyr::filter(str_detect(id, lookup))
  
  ## crop --------------------------------------------------------------------------------------------
  #structure: design_factor_crop_input_index
  crop <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",design,"factor","_crop_input",index,"$") )) 
  if(nrow(crop)!=0){
    crop <- crop$values  
  }else{
    crop<- ""
  }
  
  ## number of evaluation for each element -----------------------------------------------------------
  ## input structure: design_numLevelsESP_index
  #numEval<- allinputs %>% dplyr::filter(str_detect(id,  paste0("^",design, "_numLevelsESP_",index,"$"))) %>% nth(2)
  #numEval <- length(indexEspLvl)
  numEval <- as.integer(length(indexEspLvl))
  
  eleType <- unit <-NULL
  out <- list()
  lvl <- NULL
  for(j in seq.int(numEval)){
    
    ## level values. 
    ## input structure: design_lvl_espType_index_numEval------------------------------------------------------------------------------
    eleType <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"lvl_espType_",index,"_", indexEspLvl[j],"$"))) %>% nth(2) 
    print(eleType) 
    
    ## levels + unit -----------------------------------------------------------------------------------------------
    ## input structure:  #design_lvl_index_numEval
    lvl <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup, "lvl_espLvl_",index,"_", indexEspLvl[j],"$")))  %>% nth(2)
    lvl <-  strsplit(lvl,",")[[1]] %>% stringi::stri_trim_both()
    print(lvl)
    #unit  ----------------------------------------------------------------------------------------
    ##  input strcucture: design_lvl_unit_index_numEval 
    unit <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"lvl_espUnit_",index,"_", indexEspLvl[j],"$")))  %>% nth(2)
    print(unit)
    
    
    if(crop!=""){
      out[[j]] <- paste0(crop,"_",eleType,"_",lvl,unit)
      #print(out[j])
    } else {
      out[[j]] <- paste0(eleType,"_",lvl,unit)
    }
    #print(out[[j]])
    
  }
  out <- unlist(out)
}  


## Get experimental design label (or full name) based on abrreviations
experimental_design_label <- function(abbr_design = "frcbd"){
  
  abbr_design <- stringr::str_trim(abbr_design,side="both") %>% toupper()
  
  if(is.na(abbr_design))      {abbr_design <- ""; out <- ""}
  if(abbr_design == "UNDR")   {out <- "Unreplicated Design with No Randomization (UNDR)"  }
  if(abbr_design == "RCBD")   {out <- "Randomized Complete Block Design (RCBD)"}
  if(abbr_design == "CRD")    {out <- "Completely Randomized Design (CRD)" }
  if(abbr_design == "ABD")    {out <- "Augmented Block Design (ABD)"}
  if(abbr_design == "LSD")    {out <- "Latin Square Design (LSD)"}
  #if(abbr_design == "SPCRD") {out <- "Split Plot with Plots in CRD (SPCRD)"} #R.Eyzaguirre recommend to hide this line
  #if(abbr_design == "SPRCBD"){out <- "Split Plot with Plots in RCBD (SPRCBD)"}  #R.Eyzaguirre recommend to hide this line
  if(abbr_design == "SPRCBD") {out <- "Split Plot with Plots Design"} # #R.Eyzaguirre recommend to use just one split design under rcbd
  if(abbr_design == "SPSP")   {out <- "Split-Splot Plot Design"} # #R.Eyzaguirre recommend to use just one split design under rcbd
  if(abbr_design == "SPLSD")  {out <- "Split Plot with Plots in LSD (SPLSD)"}
  if(abbr_design == "STRIP")  {out <- "Strip Plot Design (STRIP)"}
  if(abbr_design == "FCRD")   {out <- "Factorial with CRD"}
  if(abbr_design == "FRCBD")  {out <- "Factorial with RCBD"}
  if(abbr_design == "AD")     {out <- "Alpha Design(0,1) (AD)"}
  if(abbr_design == "WD")     {out <- "Westcott Design (AD)"}
  
  out
  
}


## Get index from ID (provided by the statistical design prefix)
#id: character vector. Ids generated by agrofims during user's session
#design: character vector. Statistical design abbreviation provided by Shiny
#
get_index_design<- function(id, design){
  
 out<- stringr::str_replace_all(string = id,pattern =  paste0(design,"_"),replacement = "")
 
}

## Get index level from given ID (provided by the statistical design prefix)
#indexEspLvl: character vector (one or multiple values). Statistical design abbreviation + especial level prefix provided by Shiny. Ex "frcbd_lvl_espType_2_1"
#designEspflvl: statistical design abbreviation + especial level prefix: "frcbd_lvl_espType_"
#Ex.: get_index_espLvl_design("frcbd_lvl_espType_2_1", "frcbd_lvl_espType_")
get_index_espLvl_design<- function(indexEspLvl, designEspflvl=NULL){
  
  if(!is.null(designEspflvl)){
    out<- stringr::str_replace_all(string = indexEspLvl, pattern = designEspflvl, replacement = "")
  } else {
    out <- NULL
  }

}

#FILTER SPECIAL LEVEL INDEX BY DESIGN AND FACTOR INDEX
#index: one-value character vector. Index provided by the factor id. Ex. index="1"
#indexEspLvl: character vector (one or multiple values). Ex.: "frcbd_lvl_espType_2_1"
#design: character. Statistical design abbreviation. Ex.: design="crd".
#designEspflvl: especial level prefix. Ex. designEspflvl="frcbd_lvl_espType_"
#Example:
#lvlIds <- c("frcbd_lvl_espType_1_1", "frcbd_lvl_espType_2_1" ,"frcbd_lvl_espType_2_2")
#index<- "1"
#res<-filter_index_espLvl_design(index ="2",lvlIds= lvlIds, design="frcbd", designEspflvl="_lvl_espType_")

filter_index_espLvl_design <- function(index="1", indexEspLvl=NULL, design="frcbd", designEspflvl="_lvl_espType_"){
  
  if(!is.null(indexEspLvl)){
    out <- indexEspLvl[str_detect(indexEspLvl,paste0("^",design,"_lvl_espType_",index))]
  }
  else {
    out <- NULL
  }  
 
}

#################### DESIGN Get Details for Nutrient and Fertilizers ##################################################3

# Get index level from given ID (provided by the statistical design prefix)
# allinputs: All input values from AGROFIMS sessions
# indexEspLvl: index's levels 
# design: design
# index: design-factor index. Only one value is allowed.
# indexEspLvl: character vector (one or multiple values). Statistical design abbreviation + especial level prefix provided by Shiny. Ex "frcbd_lvl_espType_2_1"
# Ex.: res<- get_nutrient_details(allinputs=allinputs, design=design, index =2, indexEspLvl = indexEspLvl)

get_nutrient_details_design <- function(allinputs, design, index, indexEspLvl){
  
  #Filter by design and current factor index
  #indexEspLvl<- filter_index_espLvl_design(index= index, indexEspLvl=indexEspLvl, design=design, designEspflvl="_lvl_espType_")
  #Get index after filtering
  #return: 2_1, 2_2, 2_3
  indexEspLvl_subfix <- get_index_espLvl_design(indexEspLvl, paste0(design,"_lvl_espType_")) #"frcbd_lvl_espType_")
  #Number of elements
  #n <- length(get_amountype_levels(allinputs, index, indexEspLvl=indexEspLvl, factors="", design=design)) 
  
  
  #Lookup design pattern
  lookup <- paste0("^",design,"_")
  dt <- allinputs %>% dplyr::filter(!str_detect(id, "add")) %>%
    dplyr::filter(!str_detect(id, "button")) %>%
    #dplyr::filter(!str_detect(id, "unit")) %>%  ##Contemplate Unit case
    dplyr::filter(!str_detect(id, "_sel_factor_")) %>%
    dplyr::filter(!str_detect(id, "-selectized")) %>%  
    dplyr::filter(str_detect(id, lookup))
  
  dt <- arrange_nutfert(dt)
  
  nutAmount <- mNumTiming <- mNumTimingValue <- mTechnique <- mImplement <- mNutProduct <- NULL
  nutrient_list<- list()
  
  for(i in seq.int(indexEspLvl_subfix) ){
    
    #nutAmount <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputNutLvlDT_",indexEspLvl_subfix[i],"_"))) %>% dplyr::arrange(desc(values)) %>% nth(2)
    
    nutAmount <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputNutLvlDT_",indexEspLvl_subfix[i],"_")))  %>% nth(2)
    
    
    #mNumTiming <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"mNutTiming_",indexEspLvl_subfix[i],"_"))) %>% dplyr::arrange(desc(values)) %>% nth(2)
    mNumTiming <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"mNutTiming_",indexEspLvl_subfix[i],"_")))  %>% nth(2)
    if(length(mNumTiming)==0){ mNumTiming <- rep("", length(indexEspLvl_subfix))}
    
    #mNumTimingValue <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"NutTimingValue_",indexEspLvl_subfix[i],"_"))) %>% dplyr::arrange(desc(values)) %>% nth(2) 
    mNumTimingValue <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"NutTimingValue_",indexEspLvl_subfix[i],"_")))  %>% nth(2) 
    if(length(mNumTimingValue)==0){ mNumTimingValue <- rep("", length(indexEspLvl_subfix))}
    
    #mTechnique <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"mNutTechnique_",indexEspLvl_subfix[i],"_"))) %>% dplyr::arrange(desc(values)) %>% nth(2)
    mTechnique <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"mNutTechnique_",indexEspLvl_subfix[i],"_")))  %>% nth(2)
    if(length(mTechnique)==0){ mTechnique <- rep("", length(indexEspLvl_subfix))}
    
    #mImplement <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"mNutImplement_",indexEspLvl_subfix[i],"_"))) %>% dplyr::arrange(desc(values)) %>% nth(2)
    mImplement <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"mNutImplement_",indexEspLvl_subfix[i],"_"))) %>% nth(2)
    if(length(mImplement)==0){ mImplement <- rep("", length(indexEspLvl_subfix))}
    
    
    #TODO: GET the correct product amount according to number of rows
    #mNutProduct <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"mNutProduct_",indexEspLvl_subfix[i],"_"))) %>% dplyr::arrange(desc(values)) %>% nth(2) 
    mNutProduct <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"mNutProduct_",indexEspLvl_subfix[i],"_"))) %>% nth(2) 
    if(length(mNutProduct)==0){ mNutProduct <- rep("", length(indexEspLvl_subfix)) }
    
    mNutProduct<- rep(mNutProduct, each = length(mImplement)/length(mNutProduct))
    
    nutrient_list[[i]]<- data.table::data.table(nutAmount, mNumTiming, mNumTimingValue, mTechnique,mImplement,mNutProduct)
    
  }
  output <- data.table::rbindlist(nutrient_list)
  output
  
}


# Get index level from given ID (provided by the statistical design prefix)
# allinputs: All input values from AGROFIMS sessions
# indexEspLvl: index's levels 
# design: design
# index: design-factor index. Only one value is allowed.
# indexEspLvl: character vector (one or multiple values). Statistical design abbreviation + especial level prefix provided by Shiny. Ex "frcbd_lvl_espType_2_1"
# Ex.: res<- get_nutrient_details(allinputs=allinputs, design=design, index =2, indexEspLvl = indexEspLvl)

get_fertilizer_details_design <- function(allinputs, design, index, indexEspLvl){
  
  
  indexEspLvl_subfix <- get_index_espLvl_design(indexEspLvl, paste0(design,"_lvl_espType_")) #"frcbd_lvl_espType_")
  #Number of elements
  #n <- length(get_amountype_levels(allinputs, index, indexEspLvl=indexEspLvl, factors="", design=design)) 
  
  
  #Lookup design pattern
  lookup <- paste0("^",design,"_")
  dt <- allinputs %>% dplyr::filter(!str_detect(id, "add")) %>%
    dplyr::filter(!str_detect(id, "button")) %>%
    #dplyr::filter(!str_detect(id, "unit")) %>%  ##Contemplate Unit case
    dplyr::filter(!str_detect(id, "_sel_factor_")) %>%
    dplyr::filter(!str_detect(id, "-selectized")) %>%  
    dplyr::filter(str_detect(id, lookup))
  
  factorType<- mProdAmount<- mFerTiming<- mFerTimingValue<- mFerTechnique <- mFerImplement <- NULL
  fertilizer_list<- list()
 
  for(i in seq.int(indexEspLvl_subfix) ){
    
    factorType <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"factorType_",indexEspLvl_subfix[i],"_"))) %>% dplyr::arrange(desc(values)) %>% nth(2)
    
    mProdAmount <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"mFerProductAmount_",indexEspLvl_subfix[i],"_"))) %>% dplyr::arrange(desc(values)) %>% nth(2)
    
    mFerTiming <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"mFerTiming_",indexEspLvl_subfix[i],"_"))) %>% dplyr::arrange(desc(values)) %>% nth(2) 
    if(length(mFerTiming)==0){ mFerTiming <- rep("", length(indexEspLvl_subfix))}
    
    mFerTimingValue <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"FerTimingValue_",indexEspLvl_subfix[i],"_"))) %>% dplyr::arrange(desc(values)) %>% nth(2)
    if(length(mFerTimingValue)==0){mFerTimingValue <- rep("", length(factorType))}
    
    mFerTechnique <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"mFerTechnique_",indexEspLvl_subfix[i],"_"))) %>% dplyr::arrange(desc(values)) %>% nth(2)
    if(length(mFerTechnique)==0){ mFerTechnique <- rep("", length(factorType))}
    
    mFerImplement <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"mFerImplement_",indexEspLvl_subfix[i],"_"))) %>% dplyr::arrange(desc(values)) %>% nth(2)
    if(length(mFerImplement)==0){ mFerImplement <- rep("", length(factorType))}
    
    #mFerImplement
    #TODO: GET the correct product amount according to number of rows
    #mNutProduct <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"mNutProduct_",indexEspLvl_subfix[i],"_"))) %>% dplyr::arrange(desc(values)) %>% nth(2) 
    #mNutProduct<- rep(mNutProduct, each = length(mImplement)/length(mNutProduct))
    
    fertilizer_list[[i]]<- data.table::data.table(factorType, mProdAmount, mFerTiming, mFerTimingValue, mFerTechnique, mFerImplement)
    
  }
  #Juntamos las 3 listas.
  out <- data.table::rbindlist(fertilizer_list)
  
 
}



#################### MANAGEMENT PRACT Get Details for Nutrient and Fertilizers ############################################3


# Get index level from given ID (provided by the statistical design prefix)
# allinputs: All input values from AGROFIMS sessions
# indexEspLvl: index's levels 
# design: design
# index: design-factor index. Only one value is allowed.
# indexEspLvl: character vector (one or multiple values). Statistical design abbreviation + especial level prefix provided by Shiny. Ex "frcbd_lvl_espType_2_1"
# Ex.: res<- get_nutrient_details(allinputs=allinputs, design=design, index =2, indexEspLvl = indexEspLvl)

get_nutrient_details_magm <- function(allinputs, indexSoilMagp){
  
 #Lookup design pattern
  #lookup <- paste0("^",design,"_")
  dt <- allinputs %>% dplyr::filter(!str_detect(id, "add")) %>%
                      dplyr::filter(!str_detect(id, "button")) %>%
                      #dplyr::filter(!str_detect(id, "unit")) %>%  ##Contemplate Unit case
                      dplyr::filter(!str_detect(id, "_sel_factor_")) %>%
                      dplyr::filter(!str_detect(id, "-selectized")) #%>%  
                      #dplyr::filter(str_detect(id, lookup))
  
  splitApplicationTable <- mNumTiming <- mNumTimingValue <- mTechnique <- mImplement <- mNutProduct <- NULL
  nutrient_list<- list()
  
  for(i in seq.int(indexSoilMagp) ){
    
    #'TODO: Obtener las tablas "sfNutrientSplit_", con los valores de cada elemento
    #splitApplicationTable<- 
    #splitApplicationTable<- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"outputNutLvlDT_",indexEspLvl_subfix[i],"_")))  %>% nth(2)
    
    mNumTiming <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientTiming_",indexSoilMagp[i])))  %>% nth(2)
    if(length(mNumTiming)==0){ mNumTiming <- rep("", length(indexSoilMagp))}
    
    mNumTimingValue <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutTimingValue_",indexSoilMagp[i])))  %>% nth(2) 
    if(length(mNumTimingValue)==0){ mNumTimingValue <- rep("", length(indexSoilMagp))}
    
    mTechnique <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientTechnique_",indexSoilMagp[i])))  %>% nth(2)
    if(length(mTechnique)==0){ mTechnique <- rep("", length(indexSoilMagp))}
    
    mImplement <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientImplement_",indexSoilMagp[i]))) %>% nth(2)
    if(length(mImplement)==0){ mImplement <- rep("", length(indexSoilMagp))}
    
    
    mNutProduct <- dt %>% dplyr::filter(str_detect(id,  paste0("sfNutrientProduct_",indexSoilMagp[i]))) %>% nth(2) 
    if(length(mNutProduct)==0){ mNutProduct <- rep("", length(indexSoilMagp)) }
    mNutProduct<- rep(mNutProduct, each = length(mImplement)/length(mNutProduct))
    
    #Table of filtered values
    #TODO: GET the correct product amount according to number of rows
    #Filter table with "mNutProduct"
    
    nutrient_list[[i]]<- data.table::data.table(mNumTiming, mNumTimingValue, mTechnique,mImplement,mNutProduct)
    
  }
  output <- data.table::rbindlist(nutrient_list)
  output
  
}



# Get index level from given ID (provided by the statistical design prefix)
# allinputs: All input values from AGROFIMS sessions
# indexEspLvl: index's levels 
# design: design
# index: design-factor index. Only one value is allowed.
# indexEspLvl: character vector (one or multiple values). Statistical design abbreviation + especial level prefix provided by Shiny. Ex "frcbd_lvl_espType_2_1"
# Ex.: res<- get_nutrient_details(allinputs=allinputs, design=design, index =2, indexEspLvl = indexEspLvl)

get_fertilizer_details_magm <- function(allinputs, indexSoilMagp, indexProdSplit){
  
   dt <- allinputs %>% dplyr::filter(!str_detect(id, "add")) %>%
                      dplyr::filter(!str_detect(id, "button")) %>%
                      #dplyr::filter(!str_detect(id, "unit")) %>%  ##Contemplate Unit case
                      dplyr::filter(!str_detect(id, "_sel_factor_")) %>%
                      dplyr::filter(!str_detect(id, "-selectized")) #%>%  
  
  mProduct<- mFerTiming<- mFerTimingValue<- mFerTechnique <- mFerImplement <- NULL
  fertilizer_list<- list()
  
  for(i in seq.int(indexSoilMagp) ){
    
    mProduct <- dt %>% dplyr::filter(str_detect(id,  paste0("sfProductProduct_",indexSoilMagp[i]))) %>% dplyr::arrange(desc(values)) %>% nth(2)
    
    #outputDTsfProProduct_1
    #TODO: OBTENER LOS VALORES DE LA TABLA AL FILTAR POR "mProduct"
    #mProduct <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"sfProductProduct_",indexEspLvl_subfix[i],"_"))) %>% dplyr::arrange(desc(values)) %>% nth(2)
    
    mFerTiming <- dt %>% dplyr::filter(str_detect(id,  paste0("sfProductTiming_",indexSoilMagp[i]))) %>% dplyr::arrange(desc(values)) %>% nth(2) 
    if(length(mFerTiming)==0){ mFerTiming <- rep("", length(mProduct))}
    
    mFerTimingValue <- dt %>% dplyr::filter(str_detect(id,  paste0("sfProTimingValue_",indexSoilMagp[i]))) %>% dplyr::arrange(desc(values)) %>% nth(2)
    if(length(mFerTimingValue)==0){mFerTimingValue <- rep("", length(mProduct))}
    
    mFerTechnique <- dt %>% dplyr::filter(str_detect(id,  paste0("sfProductTechnique_",indexSoilMagp[i]))) %>% dplyr::arrange(desc(values)) %>% nth(2)
    if(length(mFerTechnique)==0){ mFerTechnique <- rep("", length(mProduct))}
    
    mFerImplement <- dt %>% dplyr::filter(str_detect(id,  paste0("sfProductImplement_",indexSoilMagp[i]))) %>% dplyr::arrange(desc(values)) %>% nth(2)
    if(length(mFerImplement)==0){ mFerImplement <- rep("", length(mProduct))}
    
    #'TODO Product Amount
    #txtsfProductValue_1_1 txtsfProductValue_1_2 txtsfProductValue_1_3
    #mProductAmount <- dt %>% dplyr::filter(str_detect(id,  paste0("txtsfProductValue_",indexProdSplit[i]))) %>% dplyr::arrange(desc(values)) %>% nth(2)
    
    
    #mFerImplement
    #TODO: GET the correct product amount according to number of rows
    #mNutProduct <- dt %>% dplyr::filter(str_detect(id,  paste0(lookup,"mNutProduct_",indexEspLvl_subfix[i],"_"))) %>% dplyr::arrange(desc(values)) %>% nth(2) 
    #mNutProduct<- rep(mNutProduct, each = length(mImplement)/length(mNutProduct))
    
    fertilizer_list[[i]]<- data.table::data.table(mProduct, mFerTiming, mFerTimingValue, mFerTechnique, mFerImplement)
    
  }
  #Juntamos las 3 listas.
  out <- data.table::rbindlist(fertilizer_list)
  
  
}


