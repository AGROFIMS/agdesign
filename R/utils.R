#' List of the dbf files
#' @describeIn Read the table from M.List DB an write into the computer
#'
#'
fbdesign_mtl_files <- function(){

  #usando fbglobal
  path <- fbglobal::get_base_dir()
  dbf_file_list <- list.files(path, full.names = TRUE, pattern = ".rds")

  #sin fbglobal
  #dbf_file_list <- list.files(getwd(), full.names = TRUE, pattern = ".rds")

  lg_dbf <- length(dbf_file_list)

  if(lg_dbf == 0) { gmtfiles <- "" }
  if(lg_dbf>0)    {
    ignore_temps <- grepl(pattern = "~\\$",x =  dbf_file_list)
    dbf_file_list <-  dbf_file_list[!ignore_temps]
    short_name <- basename(dbf_file_list)
    gmtfiles <- data.frame(short_name, dbf_file_list, stringsAsFactors = FALSE)
    names(gmtfiles) <- c("short_name","full_name")

    out_list <- c("hot_fieldbook.rds","dspotatotrials_dpassport.rds", "dssweettrials_dpassport.rds", "potato_pedigree.rds",
                  "sweetpotato_pedigree.rds", "table_sites.rds", "potato_db_distribution.rds")

    gmtfiles <- dplyr::filter(.data = gmtfiles, !(short_name %in% out_list))

    gmtfiles
  }

  mtl_files <- gmtfiles$full_name
  mtl_files

}



#' Detection of parent list
#' @describeIn Logical. Say TRUE if your material belongs to parental list.
#' @param mlist_name SelectInput value (commonly),
#' @author Omar Benites


is_parentList <- function(mlist_name){
  mlist <- mlist_name
  cond <- stringr::str_detect(mlist,"_parent_")
  return(cond)
}


#' Get the type of list (clonal, family or parental list) according to the file name.
#' @describeIn Character. Say \code{parent} wheter it is a parental list. Otherwise, \code{standard} whether it's a clonal or family list.
#' @param mlist SelectInput value (commonly),
#' @author Omar Benites
#' @export


get_type_list_fname <- function(mlist){

  mlist <- mlist

  cond <- stringr::str_detect(mlist,"_parent_")
  if(cond==TRUE){
    type <- "parental"

  } else {
    #clonal or family list.
    type <- "clonal"
  }
  return(type)
}


#' Detection of parent list
#' @describeIn Character. Say \code{parent} wheter it is a parental list. Otherwise, \code{standard} whether it's a clonal or family list.
#' @param type_import shiny input value. SelectInput value for type of import
#' @param ml_file_name shiny input value. SelecInput value for file names
#' @author Omar Benites
#' @export


get_mlist_file_name <- function(type_import, ml_file_name){

  if(type_import == "Template") {

    mtlist_file_name <- ml_file_name
    if(is.null(mtlist_file_name)){mtlist_file_name <- NULL}

  }

  if(type_import == "Local List"){

    mtlist_file_name <- ml_file_name
    if(is.null(mtlist_file_name) || mtlist_file_name == ""){  mtlist_file_name <- NULL  }

  }

  mlist_file_name
}

#' Type of material list (clonal, family or parental list) according to the data structure.
#' @description Logical. Say TRUE if your material belongs to parental list.
#' @param mlist list. List of attributes based on breeding material tables.
#' @author Omar Benites


get_type_list_ds <- function(mlist){

   list_names <- names(mlist)
   #Parental tables are included in parental list files. For this reason, we search in the argument of the function.

   if(is.element("parental_table", list_names)){
      # parental list.
      type <- "parental"
   } else {
      #clonal or family list.
      type <- "clonal"
   }
  return(type)

}



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

#' Get agronomic operations inputs from experiment conditions
#'
#'@description Insert column between columns based on positions.
#'@param feature value of the feature
#'@param other other value
#'@export
#'
getAgrOper <- function(feature, other="") {

  if(is.null(feature) || is.na(feature)){
    out <- ""
  } else if(feature!= 'Other'){
    out <- feature   #ToDo: check value with R.Arias
  } else if(feature == 'Other'){
    out <- other  #ToDo: check value with R.Arias
  }
  out
}


#' Get agronomic operations inputs from experiment conditions
#
#' @description Insert column between columns based on positions.
#' @param feature value of the feature
#' @param n number of values
#' @param label an argument to get units from field operations
#' @param other character. Specify 'other' value inputs.
#' @export
#
get_loop_AgrOper <- function(feature= "", n, label = "none", other= "other"){

  out <- list()

  label <- label
  for(i in 1:n){
    if(label!= "unit"){
      fi <- paste(feature,i,sep="")
      res1 <- paste("input", fi, sep="$")
      res2 <- as.quoted(res1)
      out[[i]] <- res2[[1]]
      if(length(out[[i]])==0){ out[[i]] <- "" }
      } else {
      fi <- paste(feature,i,label,sep="")
      res1 <- paste("input", fi, sep="$")
      res2 <- as.quoted(res1)
      out[[i]] <- res2[[1]]
      if(length(out[[i]])==0){ out[[i]] <- "" }
     }
  }
  out
}
#irrigation_technique_1_other


#'Get techniques inputs
#'
#' @param technique vector. Vector with technique values.
#' @param module character. Name of the module.
#' @param submodule character. Name of the submodule with underscore symbol.
#' @export
#'
get_loop_irrigation_technique <- function(technique, module="irrigation", submodule="_type_"){

  n <- length(technique)
  out<- list()
  a<- NULL
  res <-NULL
  for (i in 1:n){

   if(module=="irrigation"){

      if(technique[i] == "Sprinkler irrigation"){
        out[[i]] <- paste0("input$","irrigation_using_sprinkler_systems_", i,sep="")
      }
      else if(technique[i] == "Surface"){
        out[[i]] <- paste0("input$","surface_irrigation_technique_", i)
        #if(out[i]=="Other"){ out[i] <- paste0("input$","surface_irrigation_technique_", i, "_other") } #other
      }
      else if(technique[i] == "Localized"){
        out[[i]] <- paste0("input$","localized_irrigation_technique", i)
        #if( out[i]=="Other"){ out[i] <- paste0("input$","localized_irrigation_technique", i, "_other") } #other
      }
      else if(technique[i] == "Other"){
        out[[i]] <- paste0("input$","irrigation_technique_", i, "_other") #other
      }
      else if( is.null(technique[i]) || is.na(technique[i]) || technique[i] == "NULL" ||  technique[i] =="character(0)") {

        out[[i]] <- "NA"
      }

   }

   if(module =="weeding"){
      if( technique[i]=="Other"){
        out[i] <- paste("input$",module, type, i, "_other")
      }
    }
    #print(out[[i]])
    a <- as.quoted(out[[i]])

    out[[i]] <- a[[1]]
  }
  out
}



#'Get inputs according to techniques and subtechniques inputs
#'
#' @param technique vector. Vector with technique values.
#' @param subtechnique vector.Vector with sub technique values.
#' @export
#'
#'
get_loop_irrigation_technique_other <-function(technique="Irrigation sprinker", subtechnique ="Other"){

  n <- length(technique)
  out<- list()
  a<- NULL

 for(i in 1:n){
  if(technique[i] == "Irrigation sprinker" && subtechnique[i] == "Other"){
    out[[i]] <- paste0("input$","irrigation_using_sprinkler_systems_", i,"_other")
    a <- as.quoted(out[[i]])
    #if(out[i]=="Other"){ out[i]<- paste0("input$", "irrigation_using_sprinkler_systems_", i, "_other", sep="") }#other
  }
  else if(technique[i] == "Surface" && subtechnique[i] == "Other"){
    out[[i]] <- paste0("input$","surface_irrigation_technique_", i,"_other")
    a <- as.quoted(out[[i]])
    #if(out[i]=="Other"){ out[i] <- paste0("input$","surface_irrigation_technique_", i, "_other") } #other
  }
  else if(technique[i] == "Localized" && subtechnique[i] == "Other"){
    #out[[i]] <- paste0("input$","localized_irrigation_technique_", i,"_other")
    out[[i]] <- paste0("input$","localized_irrigation_technique", i,"_other")

    a <- as.quoted(out[[i]])
    #if( out[i]=="Other"){ out[i] <- paste0("input$","localized_irrigation_technique", i, "_other") } #other
  }
  else if( is.null(technique[i]) || is.na(technique[i]) || technique[i]=="NA" ||
           subtechnique[i]=="NA"  || is.na(subtechnique[i]) || technique[i]=="NULL" || subtechnique[i]=="NULL"){
    out[[i]] <- "NA"
    a <- as.quoted(out[[i]])
  }
  else if(technique[i]==""){
    out[[i]] <- "NA"
    a <- as.quoted(out[[i]])
  }
  else {
    out[[i]] <- subtechnique[i]
    a<-out[[i]]
  }
  out[[i]] <- a[[1]]
 }
  out
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
#' @param input input Character or Shiny input variable (\code{input$id}), conjointly, with the id. Ex. \code{input$FundingAgencyName}
#' @param type character Three type of inputs: \code{select} for select and selectize inputs, \code{date} for date inputs, and \code{text}, for text input
#' @param default character vector Value b
#' @param multiple logical \code{TRUE} for multiple input values, otherwise \code{FALSE}   
#' @param collapsed logical \code{TRUE} to separate
#' @param format character Export \code{vector} or \code{dataframe }
#' @param label character label name that appears in the user interface
#' @importFrom stringr str_trim
#' @importFrom tibble add_column
#' @export 

map_singleform_values <- function(input, type= c("select","combo box","date","text","text input"), default=NULL, multiple=FALSE, 
                                  collapsed= FALSE, format = c("vector","data.frame"), label = "Label"){
  
  format <- match.arg(format)
  type <- match.arg(type)
  
  #Type of input
  if(type=="select" || type=="combo box"){
    
      if(is.null(input) || length(input)==0 || is.na(input)){
        
        input<- ""  
        if(!is.null(default)){
          input <- default
        } 
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
map_values <- function(input, id_chr="designFieldbook_fundAgencyType_", id_rand, 
                       format = c("vector","data.frame"), lbl = NULL){
  
  format <- match.arg(format)
  
  funAgenVals <- vector(mode = "list", length = length(id_rand))
  for(i in id_rand){
    #print("en la functions")
    #print(id)
    if(is.null(input[[paste0(id_chr, i)]])){
      funAgenVals[[i]] <- "-"        
    } else if ( input[[paste0(id_chr, i)]]=="Other"){
      funAgenVals[[i]] <- input[[paste0(id_chr, i, "_other")]] 
          #in case we have another NULL value
          if(is.null(funAgenVals[[i]])){
            funAgenVals[[i]] <-  "-"  
          }
      #funAgenVals[[i]] <- setdiff(funAgenVals[[i]], "")
    } else {
      funAgenVals[[i]] <- input[[paste0(id_chr, i)]]
        if(funAgenVals[[i]]==""){
          funAgenVals[[i]] <- "-"
        }else {
          funAgenVals[[i]] <- setdiff(funAgenVals[[i]], "")
        }
          
      #funAgenVals[[i]] <- ifelse(funAgenVals[[i]]=="", "", setdiff(funAgenVals[[i]]) ) #remove ("") element from funAgenVals 
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
    #print("table")
    #print(funAgenVals)
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
#' @param isf character If \code{isf=="yes"} is full factorial, otherwise \code{isf=="no"} is for non-full factorial
#' @param id_type_dt data.frame Table with type of Factor and types of input form: \code{combo box}, \code{text input}, and \code{date}  
#  @param id_chr character character pattern id
#' @param id_rand character vector character random id
#  @param format character object format to be exported. "vector" for vector data structure or "data.frame" for data frame structure 
#' @param ntrt If \code{isf=="no"}, introduce the number of treatments in non-full factorials arragements.
#' @param lbl characer Optional argument, in case format = "data.frame"
#' @export
#'

map_level_values <- function(input, isf=c("yes","no"), id_type_dt, #id_chr= c("levels_", "select_factor_treatment_"),
                             id_rand, ntrt=2, lbl= "f"){
  
  
  #levelVals <- vector(mode = "list")
  isf <- match.arg(isf)
  id_type <- id_type_dt[,"FORM"] #get column with type of input form
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
        
        if(id_type[i]=="combo box" || id_type[i]=="text input"){
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
  
  
  
  if(isf=="no"){
    #non full factorial
    dtnoflvl <- data.frame()
    for(i in 1:length(id_rand)){
      for(j in 1:ntrt){
        
        if(id_type[i]=="date"){
          id_chr<- "date_factor_treatment_"
        } else if(id_type[i]=="text input"){
          id_chr<- "input_NFF_"
        } else if(id_type[i]=="combo box"){
          id_chr<- "select_factor_treatment_"
        }
        
        if(is.null(input[[paste0(id_chr, id_rand[i], "_", j)]] ) ){
          dtnoflvl[i,j] <- "-"
        } else {
          dtnoflvl[i,j] <- map_singleform_values(input[[paste0(id_chr, id_rand[i], "_", j)]])
        }

      }
    }
    out <- t(dtnoflvl) %>% as.tibble()  #transpose and make a list
    nms <- paste0(lbl, 1:ncol(out)) #header's name
    names(out)<- nms
    out<- out %>% as.list()
  }
  out
  
  
  
  #levelVals <- vector(mode = "list")
  # isf<- match.arg(isf)
  # id_chr<- match.arg(id_chr)
  # 
  # if(isf=="yes"){
  # levelVals <- vector(mode = "list")  
  # for(i in 1:length(id_rand)){
  #     if(is.null( input[[paste0(id_chr, id_rand[i])]] )){
  #       levelVals[[i]] <- "-"
  #     } else {
  #       levelVals[[i]] <- input[[paste0(id_chr, id_rand[i])]]
  #     }
  # }
  #   out<-levelVals
  # }
  # 
  # 
  # 
  # if(isf=="no"){
  #   #non full factorial
  #   dtnoflvl <- data.frame()
  #   for(i in 1:length(id_rand)){
  #     for(j in 1:ntrt){
  #         if(is.null( input[[paste0(id_chr, id_rand[i], "_", j)]])){
  #           dtnoflvl[i,j] <- "-"
  #         } else {
  #           dtnoflvl[i,j] <- input[[paste0(id_chr, id_rand[i], "_", j)]]
  #         }
  #     }
  #   }
  #  out <- t(dtnoflvl) %>% as.tibble()  #transpose and make a list
  #  nms <- paste0(lbl, 1:ncol(out)) #header's name
  #  names(out)<- nms
  #  out<- out %>% as.list()
  # }
  # out

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


