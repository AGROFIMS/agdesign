#' shiny UI element for HIDAP-SESSION
#'
#' returns a re-usable user interface element HIDAP-SESSION
#'
#' @author Ivan Perez, Omar Benites
#' @param type type of ui element; default is a tab in a shiny dashboard
#' @param title display title
#' @param name a reference name
#' @export


ui_session_agrofims <- function(type="tab",title="Session manage",name="session_fieldbook_design"){
  #shinydashboard::tabItem(tabName = name, # begin data_processing tabItem
  #                         h2(title),   
  #                         
  #                         shinyjs::useShinyjs(),
  #                         shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
  #                         
  #                         fluidRow( #Buttons for database connection
  #                           box(
  #                             title = "ivan ", width = 12, status = "primary", height = "250px"#,
  #                             #p("Seleccione un cultivo y una base de datos"),
  #                             
  #                             
  #                           )#,
  #                           
  #                         )
  #                         
  # )#End data_processing tabItem
  
  ##### Tab item #####
  shinydashboard::tabItem(
    tabName = name,
    
    fluidRow(
      column(12, style = "margin-top: -16px; margin-bottom: 16px;", h1("Fieldbooks"))
    ),
    fluidRow(
      box(
        title = tagList(shiny::icon("list"), "List sessions"), status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
        column(
          12,
          fluidRow(
            column(12, DT::dataTableOutput("dtsession"))
          ),

          fluidRow(
            column(6, align = "left", style = "margin-top: 26px;",
                   actionButton('delete_file', 'Delete', icon("trash"), class = "btn-danger", style="color: #fff;", width = "100px"),
                   actionButton('duplicate_file', 'Duplicate', icon("copy"), class = "btn-info", style="color: #fff;", width = "100px")
                   #actionButton('download_file', 'Download', icon(""), class = "btn-warning", style="color: #fff;")
            ),
            column(6, align = "right", style = "margin-top: 26px;",
                   actionButton("refreshsession", "Refresh", icon("refresh"), width = "100px"),
                   actionButton('load_inputNew1', 'Load 1', icon("download"), class = "btn-primary", style="color: #fff;", onclick = "openTab('newFieldbookAgrofims')", width = "100px")#,
                   #actionButton('load_inputNew2', 'Load 2', icon("download"), class = "btn-primary", style="color: #fff;", onclick = "openTab('newFieldbookAgrofims')", width = "100px"),
                   #actionButton('load_inputNew3', 'Load 3', icon("download"), class = "btn-primary", style="color: #fff;", onclick = "openTab('newFieldbookAgrofims')", width = "100px")
                   #,actionButton("aa", "aaaa")
            )
          ),
          fluidRow(
            #column(12, verbatimTextOutput("text2"))
          )
        )
      )
    )
    
    
    # fluidRow(
    #   column(
    #     12,
    #     fluidRow(
    #       column(12, DT::dataTableOutput("dtsession"))
    #     ),
    # 
    #     fluidRow(
    #       column(12, align = "right", style = "margin-top: 26px;",
    #              actionButton("refreshsession", "Refresh", icon("sync")),
    #              actionButton('load_inputs', 'Load', icon("download"), class = "btn-primary", style="color: #fff;")
    #       )
    #     ),
    #     fluidRow(
    #       column(12, verbatimTextOutput("text2"))
    #     )
    #   )
    # )
  )
  
  
  
  
}
