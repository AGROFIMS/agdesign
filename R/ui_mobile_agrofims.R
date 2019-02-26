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
  shinydashboard::tabItem(tabName = name, # begin data_processing tabItem
                          h2(title),   
                          
                          shinyjs::useShinyjs(),
                          shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
                          
                          fluidRow( #Buttons for database connection
                            box(
                              title = " ", width = 12, status = "primary", height = "250px",
                              #p("Seleccione un cultivo y una base de datos"),
                              
                              fluidRow(
                                column(6, selectizeInput(inputId = "fbmlist_sel_crop_dist", label = "Select crop", width="100%",
                                                         #choices = c("potato","sweetpotato"),
                                                         choices = c("potato"),
                                                         options = list(
                                                           placeholder = 'Please select the crop',
                                                           onInitialize = I('function() { this.setValue(""); }')
                                                         )
                                                         
                                )#,
                                
                                
                                ),
                                
                                column(6, selectizeInput("fbmlist_sel_type_dist", "Select database", width="100%", selected = 2,
                                                         choices = c("Institutional")))
                              ),
                              
                              fluidRow(
                                #column(6, selectizeInput("fbmlist_sel_list", "Select data base", width="100%",
                                #                         choices = db_files_choices )),
                                #column(6, uiOutput("sel_list_dist_btn")),
                                column(6, actionButton("fbmlist_connect_dist", "Connect", icon("fa fa-database"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4", width = 150))
                              ),
                              
                            #  tags$style(type='text/css', "#fbmlist_sel_list_dist { width:100%; margin-top: 25px;}"),
                              tags$style(type='text/css', "#fbmlist_connect_dist  { width:100%; margin-top: 25px;}")
                              
                            )#,
                            
                          ), #End of #Buttons for database connection
                          
                          
                          # Conditional Panel for Selection of material and Search of material ---------------------------------
                          
                          
                          conditionalPanel( condition = "output.show_mtable_dist",  ##conditional Panel
                                            
                                            
                                            #fluidRow(
                                            
                                            #column(12,   
                                            shiny::fluidRow(    
                                            
                                              shinydashboard::box( height = "330px",
                                                                   #"Ingrese una lista de familias o clones", width = 4, status = "primary", height = "730px",
                                                                   title = "Search by clone number", width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                                                   br(),
                                                                   br(),
                                                                   
                                                                   tags$textarea(id="fbmlist_txtarea_dist", rows=6, cols=50, ""),
                                                                   
                                                                   shiny::wellPanel(
                                                                     shiny::HTML("<b>Observations </b>")
                                                                    
                                                                   ),
                                                                   
                                                                   br(),
                                                                   br()
                                                                   #actionButton("fbmlist_search", "Search", icon("fa fa-search"), style="color: #fff; background-color: #51a351; border-color: #51a351", width = 150)
                                                                   
                                              )  
                                              
                                            ),
                                            
                                            fluidRow(    
                                              box( 
                                                #"Resultados de busqueda", width = 8, status = "primary", height = "730px",
                                                title = "Search Results", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                                br(),
                                                
                                                #div(dataTableOutput("fbmlist_table_dist"), style = "font-size:85%"),
                                                #DT::dataTableOutput('fbmlist_table'),
                                                br(),
                                                #actionButton("fbmlist_select_dist", "Select marked", icon("fa fa-arrow-down"), style="color: #fff; background-color: #51a351; border-color: #51a351", width = 150),
                                                br(),
                                                br(),
                                                br()
                                              )#,
                                            ),
                                            br(),
                                            br(),
                                            br(),
                                            # ),
                                            
                                            br(),
                                            br()
                          ),##fin conditional Panel
                          
                 
                          br(),
                          br(),
                          br()
                          
  )#End data_processing tabItem
  
}
