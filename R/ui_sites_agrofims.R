#' UI Side for managing new trial site in for HIDAP-AGROFIMS

#' Returns user friendly ui for for HIDAP-AGROFIMS
#' @author Omar Benites
#' @param type type of UI element, default is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#'

ui_sites_agrofims <- function(type = "tab", title = "", name = "createList"){
  
  shinydashboard::tabItem(tabName = name,
                          
                          h2(title),
                          shinyjs::useShinyjs(),
                          shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"), 
                          uiOutput('uiCreateSiteNew'),
                          shinyjs::hidden(textInput("xxxx", "xxxxx", value = "eerererere"))
                          
                          
                          
  ) #end tabItem
}

#' TextInputRow: Create a side-by-side input using shiny framework

