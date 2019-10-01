#' shiny UI element for HIDAP-KDSMART
#'
#' returns a re-usable user interface element HIDAP-KDSMART
#'
#' @author Ivan Perez, Omar Benites
#' @param type type of ui element; default is a tab in a shiny dashboard
#' @param title display title
#' @param name a reference name
#' @export

ui_mobile_agrofims <- function(type="tab",title="KDSmart manage",name="kdsmart_fieldbook_design"){
  ##### Tab item #####
  shinydashboard::tabItem(
    tabName = name,
    
    fluidRow(
      column(12, style = "margin-top: -16px; margin-bottom: 16px;", h1("KDSmart"))
    ),
    fluidRow(
      box(
        title = tagList(shiny::icon("list"), "List sessions"), status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
        column(
          12,
          fluidRow(
            column(12, DT::dataTableOutput("dtkdsmart"))
          ),
          
          fluidRow(
            column(6, align = "left", style = "margin-top: 26px;"
                   #actionButton('delete_file2', 'Delete', icon("trash"), class = "btn-danger", style="color: #fff;", width = "100px"),
                   #actionButton('duplicate_file2', 'Duplicate', icon("copy"), class = "btn-info", style="color: #fff;", width = "100px")
                   #actionButton('download_file', 'Download', icon(""), class = "btn-warning", style="color: #fff;")
            ),
            column(6, align = "right", style = "margin-top: 26px;",
                   actionButton("refreshsession2", "Refresh", icon("refresh"), width = "100px"),
                   #actionButton('load_inputs2', 'Load', icon("download"), class = "btn-primary", style = "color: #fff;", onclick = "openTab('newFieldbookAgrofims')", width = "100px"),
                   downloadButton("downloadKDX", "Download KDX", icon("download"), class = "btn-primary", style = "color: #fff;")
            )
          ),
          fluidRow(
            #column(12, verbatimTextOutput("text2"))
          )
        )
      )
    )
  )
  
}
