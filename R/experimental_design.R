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
              fb <- try(st4gi::cr.strd(A = ,B = ,nb = )$book )
              names(fb)[1:4] <- c("PLOT", "BLOCK", "ROW","COL")
            }
            
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
get_levels_design <- function(allinputs, index, factors, design="fcrd", 
                              data_dictionary=NULL, format=c("list","data.frame")){
  
  format<- match.arg(format)
  
  factors <- stringr::str_replace_all(string = factors,pattern = "_f[:digit:]",replacement = "")
  factors <- stringr::str_replace_all(string = factors,pattern = "_",replacement = " ")
  print(factors)
  
  lookup<- paste0("^",design,"_lvl_")
  dt <- allinputs %>%   dplyr::filter(!str_detect(id, "add")) %>%
                        dplyr::filter(!str_detect(id, "button")) %>%
                        dplyr::filter(!str_detect(id, "unit")) %>% 
                        dplyr::filter(!str_detect(id, "_sel_factor_")) %>%
                        dplyr::filter(!str_detect(id, "-selectized")) %>%  
                        dplyr::filter(str_detect(id, lookup))
  
  #a2<<- dt
                      
  #Arrange by order
  dt <- arrange_by_pattern(dt, pattern = index)
  #a1<<- dt
  
  out <- vector(mode="list",length = length(factors))
  a<-u<-NULL
  
  for(i in 1:length(factors)){
    
    out[[i]]<- dt %>% dplyr::filter(str_detect(id, paste0(lookup, index[i] )))
    #out[[i]] <-  out[[i]]$values
    
    if(factors[i]==""){
      out[[i]] <- c("","","")
    }
    else {
      temp <- data_dictionary %>% filter(FACTOR==factors[i]) 
      print(temp)
      if(nrow(temp)>0){
        print("--1--")
        #When lenght(out) is equal to 1 ---> form is text input or combo.
        form<- data_dictionary %>% filter(FACTOR==factors[i]) %>% dplyr::select(FORM)
        form<- form$FORM   
        
        print("entro1")
        print(form)
        if(form=="date"){
          print("entro2")
          out[[i]] <- out[[i]] %>%  dplyr::filter(str_detect(id,  "date" ))
          out[[i]] <-  out[[i]]$values
          
        } 
        else { 
          print(out)
          pl <<- out
          
          #if(length(out[[i]])==1){
          out[[i]] <- out[[i]] %>%  dplyr::filter(!str_detect(id,  "date" ))
          out[[i]] <-  out[[i]]$values
          #print("1")
          out[[i]]<- strsplit(out[[i]],split= ", ")[[1]]
          
          #Detect Others
          if( nrow(dt %>% dplyr::filter(str_detect(id, paste0(lookup,"other_", index[i] ))))>=1)  {
            
            a <- dt %>% dplyr::filter(str_detect(id, paste0(lookup,"other_",index[i])))
            a<- a$values
            a <- strsplit(a,split= ", ")[[1]]
            out[[i]]<- append(out[[i]], a)
            out[[i]]<- setdiff(out[[i]],"Other") #remove other value from vector
          }
          
          #Detect Units
          if( nrow(allinputs %>% dplyr::filter(str_detect(id, paste0(lookup,"unit_", index[i] ))))>=1 ){
            
            u<- allinputs %>% dplyr::filter(!str_detect(id, "-selectized")) %>%
                                dplyr::filter(str_detect(id,  paste0(lookup, "unit_",index[i] ) ))
                              u<- u$values
            out[[i]]<- paste0(out[[i]]," ",u) #quantity + whitespace + unit
          }
          
          #We place underscore in `pattern` because factors include underscore
          if(stringr::str_detect(factors[i],pattern="_application_rate")){ #special case for product, nutrient and oxidzed
            # 95, 96 y 97 from FACTOR_V10-DRAFT
            
            print("application rate")
            fert<- allinputs %>% dplyr::filter(!str_detect(id, "-selectized")) %>%
              dplyr::filter(str_detect(id,  paste0(lookup, "fert_",index[i]) ))
            fert<- fert$values
            out[[i]]<- paste0(fert," ",out[[i]]) #quantity + whitespace + unit
            
          }
          
        }
        
      } 
      else { #Si no tiene al menos una fila, este debe ser un OTHER FACTOR
        print("--2--")
        kt<<-allinputs
        othFacType <- allinputs %>% dplyr::filter(!str_detect(id, "-selectized")) %>%  
                                    dplyr::filter(str_detect(id, paste0("^",design,"_typeInput_",index[i])))  
        print(i)
        print(othFacType)
        othFacType <- othFacType$values
        
        if(othFacType=="date"){
          out[[i]] <- out[[i]] %>%  dplyr::filter(str_detect(id,  "date" ))
          out[[i]] <-  out[[i]]$values
        }
        else{
          out[[i]] <- out[[i]] %>%  dplyr::filter(!str_detect(id,  "date" )) 
          out[[i]]<- strsplit(out[[i]]$values,split= ", ")[[1]]
          
          #Detect Units
          if( nrow(allinputs %>% dplyr::filter(str_detect(id, paste0(lookup,"unit_", index[i] ))))>=1 ){
            
            u<- allinputs %>% dplyr::filter(!str_detect(id, "-selectized")) %>%
              dplyr::filter(str_detect(id,  paste0(lookup, "unit_",index[i] ) ))
            u<- u$values
            out[[i]]<- paste0(out[[i]]," ",u) #quantity + whitespace + unit
          }
          
        }
        
        #out[[i]] <-  out[[i]]$values
        
        #out[[i]]<- dt %>% dplyr::filter(str_detect(id, paste0(lookup, index[i] )))
        #
        
        
        
      }
    }
    
    
    #Si es un factor estadarizado, entonces temp debe tener al menos una fila
    
  }
  
  if(format=="data.frame"){
    print("tranform to data.frame")
  }
  out
  
}


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


## Build experimental design table metadata
get_faclevdt <- function(design, allinputs){
  
  output <- try({  
  design <- tolower(design)
  dsg <- experimental_design_label(design)
  dsg_abbr <- design %>% toupper()

  #Get IDS from design inputs
  IdDesignInputs <- getFactorIds(design)
  #Get index from Design's IDs
  index <- get_index_design(IdDesignInputs, design)
  
  
  flbl<- get_factors_design(allinputs = allinputs, index, design = design,duplicate = FALSE)
  #Get list of labels
  flvl <- get_levels_design(allinputs = allinputs, data_dictionary= dtfactordesign,
                            index, factors = flbl, design=design, format="list")
  #out <- setDT(transpose(flvl))[]
  flvl <-  lapply(flvl, function(x)paste(x,collapse=", "))
  # Number of factors
  nf <- length(flvl)
  
  ## Labels
  flab<- paste("Factor", 1:length(flbl))
  levlab <- paste("Factor", 1:length(flbl), "- Levels")
  paramlab <- c(rbind(flab, levlab)) 
  #Ensemble as a data frame of factors and levels
  out<- data.frame()
  for( i in 1:length(flvl)){
    out <- rbind(out, rbind(flbl[i], flvl[[i]]) )
  }
  #Put as a table
  dsg_dt<- data.frame(Factor= c("Experimental design", "Experimental design abbreviation",
                                   "Number of factors"), 
                      Value = c(dsg,dsg_abbr, nf),stringsAsFactors = FALSE)
  out<- data.frame(Factor= paramlab, Value= out$V1)
  out<-rbind(dsg_dt, out) 

  out 
  })
  
  if(class(output)=="try-error"){
    out<- data.frame(Factor=NULL, Value= NULL)
  }else{
    out<- output
  }
  out
}


## Get index from ID (provided by the statistical design prefix)
get_index_design<- function(id, design){
  
 out<- str_replace_all(id, paste0(design,"_"),"")
 
}


