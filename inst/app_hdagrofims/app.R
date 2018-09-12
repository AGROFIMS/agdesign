library(shinyTree)
library(dplyr)
library(magrittr)
library(stringr)
library(shinyFiles)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyFiles)
library(rhandsontable)
library(shinyBS)
library(openxlsx)
library(data.table)
library(fbsites)
library(shinysky)

#new library 6/3/2016


#tabNameS <- "resource_fieldbook_design"
tabNameS <- "design_agrofims"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  agdesign::server_design_agrofims(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Design Fieldbook"),
                    dashboardSidebar(width = 200,
                                     menuItem("Resources",
                                              sidebarMenu(id = "menu",
                                                          menuSubItem("Design Fieldbook", icon = icon("star"),
                                                                      tabName = tabNameS)
                                              )
                                     )
                    ),
                    dashboardBody(

                      tabItems(
                        agdesign::ui_fieldbook_agrofims(name = tabNameS)#$,

                      )
                    )
)

shinyApp(ui = ui, server = server)


