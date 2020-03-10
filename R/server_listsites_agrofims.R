#' server_design for managing server functionalities of list sites on HIDAP-AGROFIMS
#'
#' Design a fieldbook for HIDAP-AGROFIMS
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
## @param dom target dom element name
#' @param values reactive values
#' @author Omar Benites/Ivan Perez/Raul Arias
#' @export
#'
#server_design <- function(input, output, session, dom="hot_fieldbook_design", values){

server_listsites_agrofims <- function(input, output, session, values){
  
  constUserDB <- "agrofims"
  constPassDB <- "cnReOdGjS851TTR140318"
  constDBName <- "agrofims"
  constDBHost <- "176.34.248.121"

  output$uiListMySites <- renderUI({
    uiTrialScreenList()
  })
  
  columnNames <- c("id", "Site ID", "Type", "Name", "Country","First level admin", "Second level admin", "Third level admin","Fourth level admin" , "Village", "Nearest populated place", "Elevation", "Latitude", "Longitude","Creation date")
  
  dt <- reactiveValues()
  
  uiTrialScreenList <- function(){
    aux <- updTrialSites()
    names(aux) <- columnNames
    dt$trialSites <- aux
    
    fluidRow(
      tags$script("$(document).on('click', 'button', function () {
                  Shiny.onInputChange('siteClickId',this.id);
                  Shiny.onInputChange('siteClick', Math.random())
  });"),
      box(
        #shinysky::showshinyalert(session, "alert_hagroSites", paste("New site has been successfully added"), styleclass = "success"),
        # title = " List site information",
        title = tagList(shiny::icon("list-ol"), "Manage sites"),
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
        
        br(),br(),
        dataTableOutput("Sites_table")
    ),
    box(
      title = "Map",
      # title = tagList(shiny::icon("list-ol"), "List Site information"),
      status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
      # actionButton("btShowMap", "View sites",  class = "btn-primary",style="color: #fff;"),br(), br(),
      column( width = 12,
              leafletOutput("mymap1a", "100%", "550px")
              # leaflet() %>%
              #   addTiles() %>%  # Add default OpenStreetMap map tiles
              #   # addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
              #   setView(lng = -4.04296, lat = 16.30796, zoom = 2) #%>%
      )
      
    ),
    box(solidHeader = FALSE, width = 12)
    )
  }
  
  output$Sites_table <- renderDataTable({
    
    DT=dt$trialSites
    
    if(nrow(dt$trialSites) > 0){
      DT[["Actions"]]<-
        paste0('
             <div  role="group" aria-label="Basic example">
             <button type="button" class="btn btn-secondary view" id=siteView_',1:nrow(dt$trialSites),'><i class="fa fa-eye"></i></button>
             <button type="button" class="btn btn-secondary delete" id=siteDelete_',1:nrow(dt$trialSites),'><i class="fa fa-trash"></i></button>
             </div>
             
             ')
    }
    datatable(DT,
              escape=F,
              selection = "none",
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                columnDefs = list(list(visible=FALSE, targets=c(1, 12, 13, 14, 15)))
              ))
    
    # })
  })
  
  observeEvent(input$siteClick,{
    
    
    # leaflet("mymap1a") %>%
    #   addMarkers(lng=-7.611575, lat=-72.552344, popup="The birthplace")
    
    
    if (input$siteClickId%like%"siteView"){
      
      row_to_view = as.numeric(gsub("siteView_","",input$siteClickId))
      # print(paste0("view:", row_to_view))
      showModal(modalViewSite(row_to_view))
    }
    else if (input$siteClickId%like%"siteEdit"){
      
      row_to_edit = as.numeric(gsub("siteEdit_","",input$siteClickId))
      output$trialScreen <- renderUI({
        uiTrialSiteNew(dt$trialSites[row_to_edit,])
      })
      # sitesValues$clickToEdit <- T
    }
    
    else if (input$siteClickId%like%"siteDelete"){
      
      row_to_delete = as.numeric(gsub("siteDelete_","",input$siteClickId))
      # dt$trialSites <- dt$trialSites[-row_to_delete, ]
      
      dt$trialSites <- deleteSite(row_to_delete)
      names(dt$trialSites) <- columnNames
      shinyalert("Success", "Site was successfully deleted", type = "success")
      updateTextInput(session, "xxxx", value= runif(1))
      # updateMarkers()
    }
  })
  

  modalViewSite <- function(pSiteID){
    nm <- names(dt$trialSites)
    len <- length(nm)
    vals  <- dt$trialSites[pSiteID,]
    ml <- c(vals[[2]])
    for (i in 3:len){
      ml <-c(ml, vals[[i]])
    }
    list <-data.frame(Variable = names(vals[,-1]),  Value = ml)
    
    modalDialog(
      fluidPage(
        h3(strong("Site information"),align="center"),
        hr(),
        HTML("<center>"),
        renderTable(list, align = "rl"),
        HTML("</center>")
        
      )
    )
  }
  
  
  deleteSite <- function(pSiteID){
    
    dbId <- dt$trialSites[pSiteID,1]
    mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
    strQry = paste0("DELETE FROM user_sites
                    WHERE user_id = ", session$userData$userId,
                    " AND id = " , dbId)
    
    qryMyfiles = dbSendQuery(mydb,strQry)
    dbDisconnect(mydb)
    updateSiteRDS()
    return(updTrialSites())
    # dt$trialSites <- dt$trialSites[-pSiteID,]
    
  }
  
  updTrialSites <- function() {
    mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
    strQry = paste0("SELECT
                  id,
                  var12,
                  var1,
                  var2,
                  var3,
                  var4,
                  var5,
                  var6,
                  var13,
                  var10,
                  var11,
                  var7,
                  var8,
                  var9,
                  
                  created
                  FROM user_sites
                  WHERE user_id = ", session$userData$userId,
                    " order by created DESC, id DESC")
    qryMyfiles = dbSendQuery(mydb,strQry)
    myFiles = fetch(qryMyfiles, n=-1)
    df_withMe <- data.frame(myFiles)
    dbDisconnect(mydb)
    return(df_withMe)
  }
  updateSiteRDS <- function(){
    mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
    
    strQry = paste0("SELECT
                    id,
                    user_id,
                    var12,
                    var1,
                    var2,
                    var3,
                    var4,
                    var5,
                    var6,
                    var7,
                    var8,
                    var9,
                    var10,
                    var11,
                    var13,
                    created
                    FROM user_sites
                    order by created DESC, id DESC")
    qryMySites = dbSendQuery(mydb,strQry)
    allSites = fetch(qryMySites, n=-1)
    df_allSites <- data.frame(allSites)
    headers_sites <- c("id","userId", "shortn", "Type" , "local", "cntry", "adm1", "adm2", "admin3", "elev", "latd" , "lond", "village", "nearpop" , "admin4", "date_creation")
    names(df_allSites) <- headers_sites
    path <- fbglobal::get_base_dir()
    path <- file.path(path, "table_sites_agrofims.rds")
    saveRDS(df_allSites, file = path)
    dbDisconnect(mydb)
  }  
  
  map1a = leaflet() %>%
    addTiles("https://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png") %>%
    setView(lng = -4.04296, lat = 16.30796, zoom = 3) #%>%
  
  output$mymap1a <- renderLeaflet(
    
    
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      # addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
      setView(lng = -4.04296, lat = 16.30796, zoom = 2) %>%
      addMarkers(data = mauxf())
  )
  
  mauxf <- function(){
    mdata <- as.data.frame(lapply(dt$trialSites[,13:14], as.numeric))
    # mdata[["popup"]] <- lapply(dt$trialSites[,1], as.character)
    mdata[["popup"]] <- dt$trialSites[,2]
    names(mdata) <- c("latitude", "longitude", "popup")
    # print(mdata)
    return(mdata)
  }
  
  observeEvent(input$xxxx, {
    if(session$userData$logged){
      dt$trialSites <- updTrialSites()
      names(dt$trialSites) <- columnNames
    }
    
  })
 
  
  
}
