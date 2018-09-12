#' Get values from shinyTree object
#'
#' @param tree_input_value Input values selected from shinyTree objects
#' @param crop_selected the name of the crop
#' @description This function gets all the selected values from shinyTree checkboxes
#' @export
#'

#if(is.null(input$tree)) {print("omar")}
# get_selected(tree)
get_tree_value <- function(tree_input_value,crop_selected){

  trait_selected <- unlist(get_selected(tree_input_value))
  trait_selected <- stringr::str_replace_all(string = trait_selected,pattern = ":.*" ,replacement = "")

  #trial_headers <- fbmodule::list_modules(crop=crop_selected)

  if(crop_selected == "potato")     {tbl <- table_module_potato     } #using internal RDA
  if(crop_selected == "sweetpotato"){tbl <- table_module_sweetpotato} #using internal RDA

  #mdl <- tbl[tbl$crop_selected == crop_selected, c("module", "module_name")]
  mdl <- tbl[tbl$crop_selected == crop_selected, c("TRIAL_ABBR", "TRIAL")]
  mdl <- paste0(mdl[,2], " (", mdl[, 1],")")
  trial_headers <- sort(unique(mdl))

  trial_headers <- str_trim(gsub("\\(.*","", trial_headers ),side = "both")
  trait_selected <- trait_selected[!is.element(el = trait_selected, set = trial_headers)]
  #print(trait_selected)
}


#' Get values from shinyTree multiple objects
#'
#' @param tree_input_value Input values selected from shinyTree objects
#' @param crop_selected the name of the crop
#' @description This function gets all the selected values from shinyTree checkboxes in Big Modules or Special Modules
#' @export
#'

big_get_tree_value <- function(tree_input_value,crop_selected){

  p <- get_selected(tree_input_value)

  n <- length(get_selected(tree_input_value))
  var <- vector("character",length = n)
  form_sheet <- vector("character",length = n)

  for(i in 1:length(p)){
    if(length(attr(p[i][[1]], "ancestry", TRUE))>1){
      var[[i]] <- p[[i]][1]
      form_sheet[[i]] <-  attr(p[i][[1]], "ancestry", TRUE)[2]
    }
  }
  var <- var[var!=""]
  form_sheet <- form_sheet[form_sheet!=""]
  var <- stringr::str_replace_all(string = var,pattern = ":.*" ,replacement = "")

  table_sheet <- data.frame(form_sheet, var, stringsAsFactors = FALSE)
  list_table_sheet<- split(table_sheet , f = table_sheet$form_sheet)

  list_table_sheet
#   list_form_sheet<- list_table_sheet[[1]]$form
#   var_form_sheet <- list_table_sheet[[2]]$var

#   list_table_sheet[[1]]$form
#   list_table_sheet[[2]]$var
#   list_table_sheet[["sheet_form"]][["var"]]
}
