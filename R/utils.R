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


