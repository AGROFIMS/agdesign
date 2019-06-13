
# The original version of this function is
# https://stackoverflow.com/questions/15956931/how-to-add-a-new-column-between-other-dataframe-columns
# We make some modification to use in the context of fbdesign/hidap
#'Append column using position.
#'@description Insert column between columns based on positions.
#'@param x data frame.
#'@param cols column to insert into data frame.
#'@param after the inserted column would be located after `(i)` position. `If \code{after=1}, the column will be located in the 2nd position.
#'@export
#'

append_col <- function(x, cols, after=length(x)) {
  x <- as.data.frame(x)
  if (is.character(after)) {
    ind <- which(colnames(x) == after)
    if (any(is.null(ind))) stop(after, "not found in colnames(x)\n")
  } else if (is.numeric(after)) {
    ind <- after
  }
  stopifnot(all(ind <= ncol(x)))
  cbind(x, cols)[, append(1:ncol(x), ncol(x) + 1:length(cols), after=ind)]
}



#' Get treatment and factor inputs from design of experiments
#'
#'@description Insert column between columns based on positions.
#'@param group group
#'@param subgroup subgroup
#'@param fct factor
#'@param dfr data frame with the inputs
#'@export
#'

getTrtInputs <- function(group, subgroup, fct, dfr){

  gp <-	  group #group
  sgp <-	subgroup #subgroup
  fct <-	fct #factor
  lblFct <- paste(gp, fct, sep = "_")

  if( !is.null(gp) ||  !is.null(sgp) || !is.null(fct) ){

    dfTrt <- dfr
    if(fct == "Start date" ){
      lvl <- dfTrt[[fct]]
    } else if( fct == "End date"){
      lvl <- dfTrt[[fct]]
    } else{
      lvl<- dfTrt[["text"]]
    }
  } else {
    lblFct <- ""
    lvl <- ""
  }
  out <- list(label = lblFct, level= lvl)

}

#' Get clean dates from shiny inputs
#'
#' @param input_date input date
#' @export
#'
getDateInput<- function(input_date){

  if(length(input_date)==0){
    input_date <- ""
  }
  paste(input_date)
}



#' Get input id derived from AddButton event 
#' @author Omar Benites
#' @param addId character vector input id(s)
#' @param pattern character pattern to replace
#' @param replacement character value to replace some pattern
#' 
#' @export
#' 
getAddInputId <- function(addId = "", pattern= "FA_", replacement=""){
  
  out <- str_replace_all(addId, pattern = pattern, replacement = replacement)
  
}



#' Get user's values from single input forms
#'
#' @param input \code{input} Character or shiny input variable (\code{input$id}), conjointly, with the id. Ex. \code{input$FundingAgencyName}
#' @param input_other \code{input} In case of having \code{Other} as entry value in the combo box. Ex. input$projLeadEnt == "Other"
#' @param type \code{character} Three type of inputs: \code{select} for select and selectize inputs, \code{date} for date inputs, and \code{text}, for text input
#' @param default \code{character} vector Value b
#' @param multiple \code{logical} \code{TRUE} for multiple input values, otherwise \code{FALSE}   
#' @param collapsed \code{logical} \code{TRUE} to separate
#' @param format \code{character} Export \code{vector} or \code{dataframe }
#' @param label \code{character} label name that appears in the user interface
#' @importFrom stringr str_trim
#' @importFrom tibble add_column
#' @export 

map_singleform_values <- function(input, input_other, type= c("select","combo box","date","text","text input", "numeric"), 
                                  default=NULL, multiple=FALSE,collapsed= FALSE, format = c("vector","data.frame"), 
                                  label = "Label"){
  
  format <- match.arg(format)
  type <- match.arg(type)
  
  #Type of input
  if(type=="select" || type=="combo box"){
    
      if(is.null(input) || length(input)==0 || all(is.na(input))){
        input<- ""  
        if(!is.null(default)){
          input <- default
        } 
      } else if(length(input)==1 && input=="Other"){ #for single input values
        input<- input_other
      } else {
        input<- input
      }
  } 
  else if(type =="date"){
    
        if(length(input)==0 || is.null(input) || is.na(input) ){
              input <- ""
        } else {
              input<- paste(input) #cast from date to character
        }
    
  } 
  else if(type =="text" || type =="text input") {
    
        if(length(input)==0 || is.null(input) || is.na(input) ){
          input <- ""
        } else {
          input<- str_trim(input, side = "both") #trim whitespaces from string chains
        }
  }
  else if(type =="numeric"){
    if(is.null(input) || length(input)==0 || is.na(input)){
      input <- ""
    } else {
      input <- as.character(input)
    }
    
  } 
  
  ##collapse values
  if(collapsed){
    res <- paste(input, collapse = ", ")
  } 
  else {
    res <- paste(input)
  }
  
  #output format
  if(format=="vector"){
    res<- res
  }
  if(format=="data.frame"){
    label <- rep(label, length(res))
    res <- data.frame(res) %>% tibble::add_column(label ,.before = 1)
  }
  res
}



#' Mimic the functionality of map functions and transform inputs by applying a function to each element and returning a vector the same length as the input.
#' 
#' @param input shinyInput input values from server side 
#' @param id_chr character character pattern id
#' @param id_rand character vector character random id
#' @param format character object format to be exported. "vector" for vector data structure or "data.frame" for data frame structure 
#' @param lbl characer Optional argument, in case format = "data.frame"
#' @importFrom plyr compact
#' @importFrom tibble rownames_to_column
#' @export
#' 
map_values <- function(input, id_chr="", id_rand, 
                       format = c("vector","data.frame"), lbl = NULL){
  #id_chr="designFieldbook_fundAgencyType_name_"
  format <- match.arg(format)
  
  funAgenVals <- vector(mode = "list", length = length(id_rand))
  for(i in id_rand){

    print(id_chr)
 
    if(is.null(input[[paste0(id_chr, i)]])){
      funAgenVals[[i]] <- ""        
    } 
  
    else if (input[[paste0(id_chr, i)]]=="Other"){
      
      #special cases 1 (for Project Lead) : projLeadEnt ==
      if(id_chr=="projLeadEnt_"){
        
        funAgenVals[[i]] <-  map_singleform_values(input = input[[paste0("lead_org_type_1_", i)]],
                                                   input_other = input[[paste0("lead_org_type_1_", i,"_","other")]],
                                                   type = "select", format="vector")
        
      } else if(id_chr=="tLeadCenter_" && input[[paste0("projLeadEnt_", i)]]=="Other" &&  input[[paste0("lead_org_type_1_", i,"_","other")]]=="Other"){
        # case 6: projLeadEnt=="Other", lead_org_type_1_=="Other"
        funAgenVals[[i]] <-  map_singleform_values(input = input[[paste0("leadNameOther_", i)]],
                                                   input_other = "",
                                                   type = "select", format="vector")
      } 
       
      else if(id_chr=="tLeadCenter_" && input[[paste0("projLeadEnt_", i)]]=="CGIAR center" ){
        # case 6: projLeadEnt=="Other", lead_org_type_1_=="Other"
        funAgenVals[[i]] <-  map_singleform_values(input = input[[paste0("tLeadCenter_", i)]],
                                                   input_other = "",
                                                   type = "select", format="vector")
      }
      
      else if(id_chr=="tLeadContCRP_" && input[[paste0("projLeadEnt_", i)]]=="CGIAR center" ){
        # case 6: projLeadEnt=="Other", lead_org_type_1_=="Other"
        funAgenVals[[i]] <-  map_singleform_values(input = input[[paste0("tLeadContCRP_", i)]],
                                                   input_other = "",
                                                   type = "select", format="vector")
      }
      #tLeadContCRP_1
      
      
      
      
      else if(id_chr== "int_cropCommonName_"){
         if(!is.null(input[[paste0("int_cropCommonName_", i)]])){
           if(input[[paste0("int_cropCommonName_", i)]]=="Other"){
             funAgenVals[[i]] <-input[[paste0("int_cropCommonName_", i,"_other")]]
           }
        }   
      } else if(id_chr== "rel_cropCommonName_"){
        if(!is.null(input[[paste0("rel_cropCommonName_", i)]])){
          if(input[[paste0("rel_cropCommonName_", i)]]=="Other"){
            funAgenVals[[i]] <-input[[paste0("rel_cropCommonName_", i,"_other")]]
          }
        }   
      } else { # Otherwise, user select : "Others"
        funAgenVals[[i]] <- input[[paste0(id_chr, i, "_other")]]  
          if(is.null(funAgenVals[[i]])){ #special cases 1 :  #in case we have another NULL value
            funAgenVals[[i]] <-  ""   
          }
      }
    #projLeadEnt_==NULL, lead_org_type_==NULL, lead_org_type_1_, lead_org_type_1_WEGQHZCJ_other
    } 
    else {
      funAgenVals[[i]] <- input[[paste0(id_chr, i)]]
        if(funAgenVals[[i]]==""){
          funAgenVals[[i]] <- ""
        }else {
          funAgenVals[[i]] <- setdiff(funAgenVals[[i]], "")
        }
    }
   
    #SPECIAL CASE: id_chr ="designFieldbook_fundAgencyType_name_" && input[["designFieldbook_fundAgencyType_"]]=="CGIAR"
    #When user select CGIAR center in Funding agency type select combo
    
    if(id_chr=="designFieldbook_fundAgencyType_name_") { 
      #special cases 3 (get Experiment, lead organization name): projLeadEnt == "Other" && tLeadCenter=="NULL
      if(!is.null(input[[paste0("designFieldbook_fundAgencyType_",i)]])) {
        
        if(input[[paste0("designFieldbook_fundAgencyType_",i)]]=="CGIAR center"){
          funAgenVals[[i]] <- map_singleform_values(input = input[[paste0("designFieldbook_fundAgencyType_cgiar_", i)]],
                                                    input_other = "None",
                                                    type = "select", format="vector",default = "None")
        }
      }
     }  #Specia
    
    #SPECIAL CASE:  (get Experiment, lead organization name): projLeadEnt == "Other" && tLeadCenter=="NULL
    if(!is.null(input[[paste0("projLeadEnt_",i)]] )) { 
      #special cases 3 (get Experiment, lead organization name): projLeadEnt == "Other" && tLeadCenter=="NULL
      if( input[[paste0("projLeadEnt_",i)]]=="Other" &&  id_chr=="tLeadCenter_"){
        funAgenVals[[i]] <- ""
      }
    }  
    
    #Deprecated case: SPECIAL CASE:  id_chr="tLeadCenter_" y projecLeadEnt=="Other" in EXPERIMENT LEAD BOX
    # if( id_chr=="tLeadCenter_") { 
    #   #special cases 3 (get Experiment, lead organization name): projLeadEnt == "Other" && tLeadCenter=="NULL
    #   if(input[[paste0("projLeadEnt_",i)]]=="Other"){
    #     funAgenVals[[i]] <-  map_singleform_values(input = input[[paste0("leadNameOther_", i)]],
    #                                                input_other = "",
    #                                                type = "select", format="vector",default = "None")
    #   }
    # }  
    
    
    #Special cases 4 (get Experiment, lead organization name): projLeadEnt == NULO &  id_chr=="tLeadCenter_" & tLeadCenter=="NULL
    if(length(input[[paste0("projLeadEnt_",i)]])==0 && id_chr=="tLeadCenter_") {
       #special cases 5: if id="tLeadCenter", projLeadEnt=0
       print("case 5")
       funAgenVals[[i]] <- "" #input[[paste0(id_chr, i)]]
    }
   
    
    
  }
  funAgenVals <- plyr::compact(funAgenVals) #remove NULL values frm List
  res <-unlist(funAgenVals) #unlist and get input values
  #print("fun agel vals")
  #print(res)
  if(format=="vector"){
    res<- res
  }
  if(format=="data.frame") {
    res <- as.data.frame(res) %>% as.tibble() #tibble::rownames_to_column()
    label<- paste(lbl, 1:nrow(res))
    res <- tibble::add_column(res , label,.before = 1) #add label column in the second position
  }
  res
}


#' Mimic the functionality of map functions and transform factor-group inputs into data frame
#' 
#' @param input shinyInput input values from server side 
#' @param id_chr character character pattern id
#' @param id_rand character vector character random id
#  @param format character object format to be exported. "vector" for vector data structure or "data.frame" for data frame structure 
#' @param lbl characer Optional argument, in case format = "data.frame"
#' @export
#' 

map_fgroup_values <- function(input, id_chr ="sel_factor_", id_rand,  lbl = "Factor " ){
  
  designVals <- data.frame(gr1= NA, gr2= NA, gr3= NA)
  for(i in 1:length(id_rand)){
    for(j in 1:3){
      if(is.null(input[[paste0(id_chr, id_rand[i], "_", j)]])){
        designVals[i,j] <- "-" #empty values
      } else {
        designVals[i,j] <- input[[paste0(id_chr, id_rand[i], "_", j)]]
      }
    }
  }
  lbl <- paste(lbl, 1:nrow(designVals)) 
  designVals <- tibble::add_column(designVals , lbl, .before = 1)
  names(designVals)<- c("NUM","GROUP","SUBGROUP","FACTOR")
  designVals
}  


#' Mimic the functionality of map functions and transform level inputs into data frame
#' 
#' @param input shinyInput input values from server side 
#' @param allinputs reactive shiny object that return a data frame with all the shiny inputs
#' @param isf character If \code{isf=="yes"} is full factorial, otherwise \code{isf=="no"} is for non-full factorial
#' @param id_type_dt data.frame Table with type of Factor and types of input form: \code{combo box}, \code{text input}, and \code{date}  
#  @param id_chr character character pattern id
#' @param id_rand character vector character random id
#  @param format character object format to be exported. "vector" for vector data structure or "data.frame" for data frame structure 
#' @param ntrt If \code{isf=="no"}, introduce the number of treatments in non-full factorials arragements.
#' @param lbl characer Optional argument, in case format = "data.frame"
#' @export
#'

map_level_values <- function(input, allinputs, isf=c("yes","no"), id_type_dt, #id_chr= c("levels_", "select_factor_treatment_"),
                             id_rand, ntrt=2, lbl= "f"){
  
  
  #levelVals <- vector(mode = "list")
  isf <- match.arg(isf)
  id_type <- id_type_dt[,"FORM"] #get column with type of input form
  
  #id_gf3 <- id_type_dt[,""]
  
  fct <- id_type_dt[,"FACTOR"] #get vector with selected factors
  
  #id_chr<- match.arg(id_chr)
  if(isf=="yes"){
    levelVals <- vector(mode = "list")  
    levelVals_s <- levelVals_e <- levelVals
    numlvl <- c()
    datelevelVals <- NULL
    
    for(i in 1:length(id_rand)){
    
      numlvl <- input[[paste0("numLevels_", id_rand[i])]]   #number of levels
      #for(j in 1:length(id_type)){
          
        if(is.na(id_type[i]) || is.null(id_type[i])){
          levelVals[[i]] <- "-"
        }
        else if(id_type_dt$GROUP[i]=="Soil fertility"){
          
            if(id_type_dt$FACTOR[i]=="Number of fertilizer applications"){
              levelVals[[i]] <- paste(id_type_dt$FACTOR[i], id_rand[i])
            } else if(id_type_dt$FACTOR[i]=="Nutrient element application rate") {
              levelVals[[i]] <- paste(id_type_dt$FACTOR[i], id_rand[i])
            } else if(id_type_dt$FACTOR[i]=="Fertilizer product application rate"){
              levelVals[[i]] <- paste(id_type_dt$FACTOR[i], id_rand[i])
            } else{
              levelVals[[i]] <- paste("Fertilizer product application rate", id_rand[i])
            }      
        }
        else if(id_type[i]=="combo box" || id_type[i]=="text input"){
            id_chr <- "levels_"
            if(is.null(input[[paste0(id_chr, id_rand[i])]] )){
              levelVals[[i]] <- "-"
            } else {
              levelVals[[i]] <- input[[paste0(id_chr, id_rand[i])]]
            }          
        } 
        else if(id_type[i]=="date" && fct[i]=="Start date" && !is.null(numlvl) ){
            id_chr_s <- "factor_start_date_"
            
                for(k in 1:numlvl){ 
                       if(is.null(input[[paste0(id_chr_s, id_rand[i],"_",k)]])){
                         print("no pasa")
                         print(input[[paste0(id_chr_s, id_rand[i],"_",k)]]) 
                         datelevelVals[k] <- "-"
                       } else {
                         print("pasa")
                         print(input[[paste0(id_chr_s, id_rand[i],"_",k)]])
                         datelevelVals[k] <- as.character(input[[paste0(id_chr_s, id_rand[i],"_",k)]])
                       }
                }
            levelVals[[i]] <- datelevelVals 
        } 
        else if(id_type[i]=="date" && fct[i]=="End date" && !is.null(numlvl)) {
              id_chr_e <- "factor_end_date_" 
                   for(k in 1:numlvl){ 
                           if(is.null(input[[paste0(id_chr_e, id_rand[i],"_",numlvl)]])){
                              datelevelVals[k] <- "-"
                           } else {
                              datelevelVals[k] <- as.character(input[[paste0(id_chr_e, id_rand[i],"_",numlvl)]])
                           }
                   }
              levelVals[[i]] <- datelevelVals 
      }
           
      
    }
    out<-levelVals
  }
  
  # id_chr<-"input_factor_treatment_"
  #input_factor_treatment_KDIDZIVW_1
  
  if(isf=="no"){
    #non full factorial
    dtnoflvl <- data.frame()
    for(i in 1:length(id_rand)){
      for(j in 1:ntrt){
        
        if(is.na(id_type[i]) || is.null(id_type[i])){ #in case of missing values or not mapped values
          #id_chr<- "select_factor_treatment_" #by default
          id_chr <- NULL
          
        } 
        else if(id_type[i]=="date"){
          #id_chr<- "date_factor_treatment_"
          id_chr<-"input_factor_treatment_"
          print(id_chr)
        } 
        else if(id_type[i]=="text input"){
          #id_chr<- "input_NFF_"
          id_chr<-"input_factor_treatment_"
          print(id_chr)
        } 
        else if(id_type[i]=="combo box"){
          id_chr<-"input_factor_treatment_"
          #id_chr<- "select_factor_treatment_"
          print(id_chr)
        }
        
        if(is.null(input[[paste0(id_chr, id_rand[i], "_", j)]]) || is.null(id_chr) ){
          dtnoflvl[i,j] <- "-"
        }  else if(id_type_dt$GROUP[i]=="Soil fertility"){
          
              if(id_type_dt$FACTOR[i]=="Number of fertilizer applications"){
                dtnoflvl[i,j] <- paste(id_type_dt$FACTOR[i], id_rand[i])
              } else if(id_type_dt$FACTOR[i]=="Nutrient element application rate") {
                dtnoflvl[i,j] <- paste(id_type_dt$FACTOR[i], id_rand[i])
              } else if(id_type_dt$FACTOR[i]=="Fertilizer product application rate"){
                dtnoflvl[i,j] <- paste(id_type_dt$FACTOR[i], id_rand[i])
              } else{
                dtnoflvl[i,j] <- paste("Fertilizer product application rate", id_rand[i])
              }      
          
        } else {
          dtnoflvl[i,j] <- map_singleform_values(input[[paste0(id_chr, id_rand[i], "_", j)]], type =  id_type[i])
        }

      }
    }
    out <- t(dtnoflvl) %>% as.tibble()  #transpose and make a list
    nms <- paste0(lbl, 1:ncol(out)) #header's name
    names(out)<- nms
    out<- out %>% as.list()
  }
  out
  


}




#' Get factor and levels from design interface and type of factorial
#' 
#' @param input shinyInput Variable defined in \code{server(input,output)}
#' @param designVars reactiveValues  Reactive expression that contains all the ID's related to add, delete and remove buttons. 
#' @param tf character type of factorial. \code{yes} equivalent to full-factorial arragement, otherwise, \code{no} is equivalent to non-full factorial
#' @export
#' 
get_fctlvl_values <- function(input, designVars, tf= c("yes","no")){
  
  tf <- match.arg(tf) #type of factorial (full or non-full)
  
  #Type of design
  if(tf=="yes"){ #full factorial arragement
    id_ff_rand <- getAddInputId(designVars$ids_FULL, "FF_", "") 
    nf <- length(id_ff_rand)
    #factors and levels
    fg <- map_fgroup_values(input= input, id_chr ="sel_factor_", id_rand = id_ff_rand, lbl = "Factor")
    flvl <- map_level_values(input= input, isf = tf, id_chr ="levels_", id_rand = id_ff_rand, lbl= "Level")
    #fb <- try(fb$book)
  } 
  if(tf=="no"){ #non full-factorial design
    id_nff_rand <- getAddInputId(designVars$ids_NFULL, "NFF_", "") 
    nf <- length(id_nff_rand)
    #factors and levels
    #fg <- map_fgroup_values(input= input, id_chr ="sel_factor_", id_rand = id_nff_rand, lbl = "Factor") 
    fg <- map_fgroup_values(input= input, id_chr ="sel_factor_", id_rand = id_nff_rand, lbl = "Factor") 
    flvl <- map_level_values(input= input, isf=tf, id_chr ="select_factor_treatment_", id_rand = id_nff_rand, 
                             ntrt = ntrt, lbl= "f")
  }
  #TODO: hacer una sola lista y cambiar los nombres de los items con fg$lbl
  out<- list(fg=fg, lvl=flvl, nf= nf)
  
}


#' Add columns to data frames with different sizes
#' 
#' @param dt data frame 
#' @param new.col vector New column to add into the data frame
#' @references https://stat.ethz.ch/pipermail/r-help/2004-October/059752.html

add.col<-function(dt, new.col) {
  n.row<-dim(dt)[1]
  length(new.col)<-n.row
  cbind(dt, new.col)
}


#' Add columns to data frames with different sizes
#' 
#' @param dt data frame 
#' @param dt_other vector New column to add into the data frame
#' @author Omar Benites

dt_inputs<- function(dt, dt_other){
  
  for(i in 1:nrow(dt)){
    
    if(dt[i,"values"]=="Other" || dt[i,"values"]=="other" ){
      dt[i,"values"] <-  dt_other[i,"values"]
    }
    else{
      dt[i,"values"]<- dt[i,"values"]
    }
  }  
  dt
}

# Arrange data.frame by pattern
arrange_by_pattern <- function(data, pattern){
  
  if(nrow(data)>0 && length(pattern)>0){
    dt <-  list() #data.frame()
    #pattern <- c("MORIBVKI","JVCBBMVG","POBNRFFA")
    for( i in 1:length(pattern)){
      #print(dt)
      #dt<- rbind(dt, data %>% filter(str_detect(id, pattern[i])))
      dt[[i]] <- data %>% filter(str_detect(id, pattern[i])) #rbind(dt, data %>% filter(str_detect(id, pattern[i])))
    }
    dt <- data.table::rbindlist(dt, fill=TRUE) %>% as.data.frame(stringsAsFactors=FALSE)
  } else {
    dt <- data.frame()
  }
  
  dt
} 


#Smart Bind data frames by columns

smart_colbind <- function(...){
  
 dt_list<- list(...)
 # #Remove/Clean all the list (or clean) that are equal to zero
 dt_list<- rlist::list.clean(dt_list, function(x) length(x) == 0)
 dt <- bind_cols(dt_list)
 
}

#Clean header for experiment conditions data frames
ec_clean_header <- function(dt){

  dt$`Fieldbook download`<-NULL
  dt$`Other - specify`<-NULL
  dt$Observation<-NULL
  dt$`Factor?` <- NULL
  dt$v1<- dt$v2 <- dt$v3<- NULL
  dt$`Fieldbook download`<- dt$`Other - specify`<- dt$Observation<- NULL
  dt$`Factor?`<-dt$`Reorganisation of all the variables (see GitHub 112 for the mock-up)`<- NULL
  #dt$Fieldbook_download<-NULL
  dt$Measurement_2<-NULL
  dt$temp<-NULL
  #dt <- dt %>% dplyr::select(-starts_with("NA."))
  dt
}

#Filter data for experiment conditions
ec_filter_data <- function(dt){
  
  
  dt <- dt %>% filter(TraitName!="")
  #dt <- dt %>% filter(Fieldbook_download!="")
               #filter(Fieldbook_download!="Residue_management_end_date") %>% 
               #filter(Fieldbook_download!="Residue_management_residue_incorporation_depth")
  
  # if(is.element("Fieldbook_download", names(dt))){
  #   dt <- dt %>% mutate(TraitName=Fieldbook_download)
  #   
  # } else{
  #   dt
  # }
  
  dt
}

#Get number of evaluations per season for experiment conditions
get_ns <- function(addId){
  
  n<- length(addId) 
  n
}

#Change units in TraitList sheet
changes_units <- function(ec, input, allinputs){
  
   
  dt <- as.data.frame(ec,stringsAsFactors=FALSE)
  col_name <- "TraitUnit"
  ############## Residue management ##############
  
  r_thick_unit <- map_singleform_values(input =input$rmgt_crop_residue_thick_unit, 
                                        type = "select", format = "vector", label = "Factor") #unit
  r_amount_unit <- map_singleform_values(input =input$rmgt_crop_residue_amount_sqm_unit, 
                                         type = "select", format = "vector", label = "Factor") #unit
  r_cov_unit <- map_singleform_values(input =input$rmgt_crop_residue_perc_cov_unit, 
                                      type = "select", format = "vector", label = "Factor")  #unit
  
  
  pos<- which(dt$TraitName=="Residue_management_crop_residue_thickness") 
  dt[pos,"TraitUnit"] <- r_thick_unit 
  pos<-which(dt$TraitName=="Residue_management_crop_residue_amount") 
  dt[pos,"TraitUnit"] <- r_amount_unit
  pos<-which(dt$TraitName=="Residue_management_crop_residue_thickness") 
  dt[pos,"TraitUnit"]<- r_cov_unit
  
  #lb2 <- "Residue incorporation depth Unit"
  rb_incordepthunit <- map_singleform_values(input =input$rmgt_residue_inc_depth_unit, 
                                             type = "select", format = "vector", label = "Factor") #unit
  pos<- which(dt$TraitName=="Residue_management_residue_incorporation_depth")
  dt[pos,"TraitUnit"]<- rb_incordepthunit
  ##############Pudling ##############
  lp_depth_unit <- map_singleform_values(input = input$puddling_depth_unit, 
                                         type = "select", format = "vector", label = "Factor")
  
  pos<- which(dt$TraitName=="Puddling_depth")
  dt[pos,"TraitUnit"]<- lp_depth_unit
  
  
  
  ############## Tillage##############
  lt_depth_unit  <- map_singleform_values(input = input$tillage_depth_unit,type = "select", format = "vector", label = "Factor" )
  pos<- which(dt$TraitName=="Tillage_depth")
  dt[pos,"TraitUnit"]<-lt_depth_unit 
  
  ############## Direct_seeding ##############################
  
  ptdi <- allinputs %>%  filter(!str_detect(id, "button")) %>%
    filter(!str_detect(id, "-selectized")) %>%
    filter(str_detect(id,"ptdi"))
  
  row_unit <- ptdi %>% filter(str_detect(id,   "ptdi_distance_rows_unit_[:digit:]+$" ))
  
  #seeding rate
  rate_unit <- ptdi %>% filter(str_detect(id, "ptdi_seeding_rate_unit_[:digit:]+$" ))
  
  #distance plants
  plan_unit<- ptdi %>% filter(str_detect(id,   "ptdi_distance_plants_unit_[:digit:]+$"))
  
  #plan density
  plden_unit <- ptdi %>% filter(str_detect(id,  "ptdi_seeding_plant_density_unit_[:digit:]+$"  ))  
  
  #bund
  bund_unit<- ptdi %>% filter(str_detect(id,  "ptdi_seeding_distance_bunds_unit_[:digit:]+$" ))
  
  pos<-which(dt$TraitName=="Direct_seeding_distance_between_rows")
  dt[pos,"TraitUnit"]<-row_unit$values
  pos<-which(dt$TraitName=="Direct_seeding_seed_rate")
  dt[pos,"TraitUnit"]<-  rate_unit$values
  pos<-which(dt$TraitName=="Direct_seeding_distance_between_plants")
  dt[pos,"TraitUnit"]<-plan_unit$values
  pos<-which(dt$TraitName=="Direct_seeding_plant_density")
  dt[pos,"TraitUnit"]<- plden_unit$values
  pos<- which(dt$TraitName=="Direct_seeding_distance_between_bunds") 
  dt[pos,"TraitUnit"]<- bund_unit$values
  
  
  ##############  Transplanting ############## 
  
  ptta <- allinputs %>%  filter(!str_detect(id, "button")) %>%
    filter(!str_detect(id, "-selectized")) %>%
    filter(str_detect(id,"ptta"))
  
  #distance between rows
  ta_drow_unit<- ptta %>% filter(str_detect(id,"ptta_trans_distance_rows_unit_[:digit:]+$"))
  
  #Seeding density
  ta_sden_unit <-ptta %>% filter(str_detect(id,"ptta_trans_seeding_density_unit_[:digit:]+$"))
  
  #distance plants
  ta_dplan_unit <- ptta %>% filter(str_detect(id,"ptta_trans_distance_plants_unit_[:digit:]+$"))
  
  #distance bunds
  ta_bunds_unit<- ptta %>% filter(str_detect(id,"ptta_trans_distance_bunds_unit_[:digit:]+$"))
  
  pos<-which(dt$TraitName=="Transplanting_distance_between_rows")
  dt[pos,"TraitUnit"]          <-ta_drow_unit$values
  pos<-which(dt$TraitName=="Transplanting_seedling_density") 
  dt[pos,"TraitUnit"]          <-ta_sden_unit$values
  pos<-which(dt$TraitName=="Transplanting_distance_between_plants") 
  dt[pos,"TraitUnit"]          <-ta_dplan_unit$values
  pos<-which(dt$TraitName=="Transplanting_distance_between_bunds")
  dt[pos,"TraitUnit"]       <-ta_bunds_unit$values
  
  #### MULCING ###########################
  
  mu <- allinputs %>% filter(!str_detect(id, "button")) %>%
    filter(!str_detect(id, "-selectized")) %>%
    filter(str_detect(id, "^mumd_"))
  
  mthick_unit <- mu %>% filter(str_detect(id, "^mumd_mulch_thickness_unit$"))
  
  #mulch amount
  mamount_unit <- mu %>% filter(str_detect(id, "^mumd_mulch_amountPerSq_unit$"))
  
  #percentage
  mper_unit <- mu %>% filter(str_detect(id, "^mumd_mulch_percCoverage_unit$"))
  
  pos<-which(dt$TraitName=="Mulch_thickness") 
  dt[pos,"TraitUnit"] <- mthick_unit$values
  pos<-which(dt$TraitName=="Mulch_amount") 
  dt[pos,"TraitUnit"] <-mamount_unit$values
  pos<-which(dt$TraitName=="Mulch_percentage_of_coverage") 
  dt[pos,"TraitUnit"] <-mper_unit$values
  
  ### IRRIIGATION
  
  dt
  
}

# Function to attach underscore and hashtag
#
# "NumberofMeasurementsPerSeason", "NumberofMeasurementsPerPlot"
add_season_numplot_prefix<- function(dt){
  
  if(!is.null(dt) && nrow(dt)!=0){
    out<-NULL
    dt$NumberofMeasurementsPerSeason <- as.numeric(dt$NumberofMeasurementsPerSeason)
    dt$NumberofMeasurementsPerPlot <- as.numeric(dt$NumberofMeasurementsPerPlot)
    season_idx <- which(dt$NumberofMeasurementsPerSeason<=0)
    nplot_idx <-  which(dt$NumberofMeasurementsPerPlot<=0)
    #out2<- list()
    
        if(length(season_idx)>0){
          dt$NumberofMeasurementsPerSeason[season_idx]<- 1
        }
        if(length(nplot_idx)>0){
          dt$NumberofMeasurementsPerPlot[nplot_idx]<- 1
        }
        out <- list()
        
        #Number of instaces per seasons
        for(i in 1:nrow(dt)) {
          out[[i]]<- paste(dt$TraitName[i],1:dt$NumberofMeasurementsPerSeason[i],sep="__") 
        }
    
        if(all(dt$NumberofMeasurementsPerPlot==1L)){
          
          out<- unlist(out)
          
        } else{
          
          out2<- list()
          for( i in 1:nrow(dt)){
            
            if(dt$NumberofMeasurementsPerPlot[i]==1L){
              out2[[i]] <- out[[i]]
            }else{
              out2[[i]]<- sort( as.vector(outer(out[[i]], 1:dt$NumberofMeasurementsPerPlot[i], paste, sep="#")))
            }
          }
          
          out<- unlist(out2)
          
        }

  } else {
    
    out<-NULL
    
  }
  out
  
} 

#TODO MEJORAR Y HACER 2 FOR LOOPS
# add_numplot_prefix <- function(dt, cs){
#   
#   if(!is.null(dt) && nrow(dt)!=0){
#   
#   dt$NumberofMeasurementsPerPlot <- as.numeric(dt$NumberofMeasurementsPerPlot)
#   nplot_idx <-  which(dt$NumberofMeasurementsPerPlot<=0)
#   if(length(nplot_idx)>0){
#     dt$NumberofMeasurementsPerPlot[nplot_idx]<- 1
#   }
#   out <- NULL
#   for(i in 1:nrow(dt)) {
#     #if(dt$NumberofMeasurementsPerPlot[i]!=1){
#       out<- append(out, paste(cs[i],1:dt$NumberofMeasurementsPerPlot[i],sep="#") )
#     #} else {
#      # out <- append(out, paste(cs[i]))
#     #}
#   }
#   
#   } else {
#     out<-NULL
#   }
#   out
#   
# }

