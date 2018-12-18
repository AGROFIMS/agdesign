#' UI List Sites for managing new trial site in for HIDAP-AGROFIMS

#' Returns user friendly ui for for HIDAP-AGROFIMS
#' @author Omar Benites/Ivan Perez/Raul Arias
#' @param type type of UI element, default is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#'

ui_listsites_agrofims <- function(type = "tab", title = "List Sites", name = "listSites"){
  
  shinydashboard::tabItem(tabName = name,
                          
                          h2(title),
                          
                          shinyjs::useShinyjs(),
                          shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
                          uiOutput("uiListMySites"),
tags$script('$(document).on("change", "select",  function(){
                               var a = this.id + "_other";
                               var b = "#" + a ;
                               var val = this.value
              
                               if(val == "Other") {
                               shinyjs.show(a)
                               $(b).focus();
                               }
                               else{shinyjs.hide(a)}
                               })
                '
                          )
                          
                          
                          
  ) #end tabItem
  
  
}

#' TextInputRow: Create a side-by-side input using shiny framework

