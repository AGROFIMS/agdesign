#' server_design for managing server functionalities of sites on HIDAP-AGROFIMS
#'
#' Design a fieldbook for HIDAP-AGROFIMS
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
## @param dom target dom element name
#' @param values reactive values
#' @author Omar Benites
#' @export
#'
#server_design <- function(input, output, session, dom="hot_fieldbook_design", values){

server_sites_agrofims <- function(input, output, session, values){
  
  constUserDB <- "agrofims"
  constPassDB <- "cnReOdGjS851TTR140318"
  constDBName <- "agrofims"
  constDBHost <- "176.34.248.121"
  
  register_google(key = "AIzaSyAPWYHA8LkSrhnr1XxBFHuJ3aWeqi-N5lQ")

  output$uiCreateSiteNew <- renderUI({
    uiTrialSiteNew()
  })
  
  observeEvent(input$mymap_click, {
    lat  <- round(input$mymap_click$lat, 5)
    lon  <- round(input$mymap_click$lng, 5)
    
    updateTextInput(session, "inSiteLatitude", value = lat)
    updateTextInput(session, "inSiteLongitude", value = lon)
    
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      addMarkers(lon, lat)
  })
  
  uiTrialSiteNew <- function(pData = NULL){
    strCreate = "Create"
    strCreateId = "btCreateSite"
    boxTitle <- "Create Site information"
    boxIcon <- "plus-circle"
    veg <- NULL
    if(is.null(pData)){
      vData <- vector("list", 15)
    }
    else{
      vData <- pData
      strCreate <- "Save"
      strCreateId <- "btUpdateSite"
      boxTitle <- "Update Site information"
      boxIcon  <- "edit"
      # veg <-  strsplit(vData[[13]], ',')[[1]]
    }
    
    
    mchoices = c("Farmer field", "Experiment station field", "Greenhouse/screenhouse", "Government forest", "Private forest", "Pasture", "Water body")
    fluidRow(
      shinydashboard::box(
        title = tagList(shiny::icon(boxIcon), boxTitle),
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
        
        box(
          title = "Site name", solidHeader = TRUE, status = "warning", width=12,
          column(width = 4, 
                 disabled(textInput("inSiteID", label="Site ID", value = vData[[2]] )),
                 selectizeInput("inSiteType", label="Site type", choices = mchoices, multiple  = TRUE , options = list(maxItems = 1, placeholder ="Select one..."), selected= vData[[3]] ),
                 textInput("inSiteName", label = "Site name", value=vData[[4]])
          )
        ),
        
        
        box(
          title = "Site location", solidHeader = TRUE, status = "warning", width=12,
          column(4, 
                 selectizeInput("inSiteCountry", label="Country name", multiple = TRUE,
                                choices = unique(geodb$NAME_0),
                                selected= vData[[5]],
                                options = list(maxItems = 1, placeholder = 'Select country... ')),
                 
                 #textInput("inSiteAdmin1", label = "Site, first-level administrative division name", value=vData[[6]]),
                 
                 uiOutput("fbsites_ui_admin1"),
                 uiOutput("fbsites_ui_admin2"),
                 uiOutput("fbsites_ui_admin3"),
                 uiOutput("fbsites_ui_admin4"),
                 uiOutput("fbsites_ui_admin5"),
                 textInput("inSiteNearestPlace", label = "Nearest populated place", value=vData[[11]]), 
                 shiny::numericInput(inputId = "inSiteElevation" ,label = "Site elevation (meters)", value = vData[[12]] )
                 
          ), 
          column( width = 8,
                  br(),
                  HTML("<center>"),
                  radioButtons("mymap_radiobutton_type", "Map view type", c("Default", "Street map", "Geo map"), selected="Default",inline = T),
                  HTML("</center>"),
                  
                  leafletOutput("mymap"), 
                  fluidRow(
                    column(width = 6, 
                           shiny::numericInput(inputId = "inSiteLatitude" , label = "Site latitude (in decimal degrees)", value = vData[[13]] )
                    ),
                    column( width = 6,
                            shiny::numericInput(inputId = "inSiteLongitude" , label = "Site longitude (in decimal degrees)", value = vData[[14]] )
                    )
                  )
          )
        ),
        
        column(width = 12,
               useShinyalert(),
               actionButton(strCreateId, strCreate, class = "btn-primary",style="color: #fff;")
               #actionButton("goToMainSiteScreen", "Cancel")#,
        )
        
        
      )#end box
      ,
      box(solidHeader = FALSE, width = 12)
    )#end fluidrow
    
  }
  
  output$mymap <- renderLeaflet({
    ZOOM=2
    LAT=0
    LONG=0
    
    # Get latitude and longitude
    if( is.null(input$inSiteCountry) && is.null(input$input$inSiteAdmin1) && is.null(input$input$inSiteAdmin2) && is.null(input$input$inSiteAdmin3) ){
      ZOOM=2
      LAT=0
      LONG=0
    }
    else {
      minput <- if(is.null(input$inSiteCountry)) "" else input$inSiteCountry
      ZOOM=4
      
      if(!is.null(input$inSiteAdmin1) && input$inSiteAdmin1 != "NA"){
        
        minput <- paste0(minput," ", input$inSiteAdmin1)
        ZOOM <- ZOOM + 2
      }
      if(!is.null(input$inSiteAdmin2) && input$inSiteAdmin2 != "NA"){
        minput <- paste0(minput," ", input$inSiteAdmin2)
        ZOOM <- ZOOM + 2
      }
      if(!is.null(input$inSiteAdmin3) && input$inSiteAdmin3 != "NA"){
        minput <- paste0(minput," ", input$inSiteAdmin3)
        ZOOM <- ZOOM + 2
      }
      
      target_pos=geocode(minput)
      LAT= round(target_pos$lat, 5) 
      LONG=round(target_pos$lon, 5)  
      
     
      
    }
    updateTextInput(session, "inSiteLatitude", value = LAT)
    updateTextInput(session, "inSiteLongitude", value = LONG)
    
    map_type <- isolate(input$mymap_radiobutton_type)
    
    if(map_type == "Street map"){
      leaflet() %>% 
        setView(lng=LONG, lat=LAT, zoom=ZOOM ) %>%
        addTiles() %>%
        addMarkers(LONG, LAT)
    }
    else if(map_type == "Geo map"){
      leaflet() %>% 
        setView(lng=LONG, lat=LAT, zoom=ZOOM ) %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap)  %>%
        addMarkers(LONG, LAT)
    }else{
      leaflet() %>%
        setView(lng=LONG, lat=LAT, zoom=ZOOM ) %>%
        
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(LONG, LAT)
    }
  })
  
  observeEvent(input$mymap_radiobutton_type, {
    
    if(input$mymap_radiobutton_type == "Street map"){
      leafletProxy("mymap") %>%
        addTiles()
    }
    else if(input$mymap_radiobutton_type == "Geo map"){
      leafletProxy("mymap") %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap)
    }
    else{
      leafletProxy("mymap") %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE))
    }
    
  })
  
  output$fbsites_ui_admin1 <-renderUI({
    
    
    cntry <- input$inSiteCountry
    admin1 <- get_admin_agrofims(sites_data = geodb, country = cntry)
    selAdmin1 <- ""
    selectizeInput("inSiteAdmin1", label= "First-level administrative division",
                   choices = admin1,
                   multiple = TRUE,
                   options = list(maxItems = 1, placeholder = 'Select admin 1'),
                   selected= selAdmin1)
  })
  
  output$fbsites_ui_admin2 <-renderUI({
    
    cntry <- input$inSiteCountry
    admin1 <- input$inSiteAdmin1
    admin2 <- get_admin_agrofims(geodb, country = cntry, admin1 = admin1)
    
    selAdmin2<- ""
    
    if(is.na(admin2) && is.null(admin1)){
      textInput("inSiteAdmin2_text", label = "Second-level administrative division", value= selAdmin2) #)vData[[7]])
    } else {
      
      selectizeInput("inSiteAdmin2", label= "Second-level administrative division", multiple = TRUE,
                     choices = admin2,
                     selected= selAdmin2,
                     options = list(maxItems = 1, placeholder = 'Select admin 2'))
    }
    
  })
  
  output$fbsites_ui_admin3 <-renderUI({
    
    cntry <- input$inSiteCountry
    admin1 <- input$inSiteAdmin1
    admin2 <- input$inSiteAdmin2
    admin3 <- get_admin_agrofims(geodb, country = cntry,
                                 admin1 = admin1,
                                 admin2 = admin2)
    
    selAdmin3 <- ""
    
    if(is.na(admin3)){
      textInput("inSiteAdmin3_text", label = "Third-level administrative division", value= selAdmin3) #)vData[[7]])
    } else {
      
      selectizeInput("inSiteAdmin3", label= "Third-level administrative division", multiple = TRUE,
                     choices = admin3,
                     selected= selAdmin3,
                     options = list(maxItems = 1, placeholder = 'Select admin 3'))
    }
    
  })
  
  output$fbsites_ui_admin4 <-renderUI({
    
    cntry <- input$inSiteCountry
    admin1 <- input$inSiteAdmin1
    admin2 <- input$inSiteAdmin2
    admin3 <- input$inSiteAdmin3
    
    admin4 <- get_admin_agrofims(geodb, country = cntry,
                                 admin1 = admin1,
                                 admin2 = admin2,
                                 admin3 = admin3)
    
      
    selAdmin4 <- ""
   
    
    if(is.na(admin4)){
      
      textInput("inSiteAdmin4_text", label = "Fourth-level administrative division", value= selAdmin4) #)vData[[7]])
    } else {
      
      selectizeInput("inSiteAdmin4", label= "Fourth-level administrative division", multiple = TRUE,
                     choices = admin4,
                     selected= selAdmin4,
                     options = list(maxItems = 1, placeholder = 'Select admin 4'))
      
    }
  })
  
  output$fbsites_ui_admin5 <-renderUI({
    
    cntry <- input$inSiteCountry
    admin1 <- input$inSiteAdmin1
    admin2 <- input$inSiteAdmin2
    admin3 <- input$inSiteAdmin3
    admin4 <- input$inSiteAdmin4
    
    
    admin5 <- get_admin_agrofims(geodb, country = cntry,
                                 admin1 = admin1,
                                 admin2 = admin2,
                                 admin3 = admin3,
                                 admin4 = admin4
    )
    
   selAdmin5 <- ""
    
    if(is.na(admin5)){
      
      textInput("inSiteAdmin5_text", label = "Fifth-level administrative division", value= selAdmin5) #)vData[[7]])
      
    } else {
      
      selectizeInput("inSiteAdmin5", label= "Fifth-level administrative division", multiple = TRUE,
                     choices = admin5,
                     selected= selAdmin5,
                     options = list(maxItems = 1, placeholder = 'Select admin 5'))
    }
  })
  
  observe({
    
    #After all this conditions has been made, the submit button will appear to save the information
    toggleState("btCreateSite", !
                  # is.null(input$inSiteCountry) && str_trim(input$inSiteCountry, side = "both")!= "" &&
                  is.null(input$inSiteCountry) && str_trim(input$inSiteCountry, side = "both")!= ""
                
                # !is.null(input$inSiteName) && str_trim(input$inSiteName, side = "both")!= "" &&
                
                #   !is.null(input$inSiteAdmin1) && str_trim(input$inSiteAdmin1, side = "both")!= "" &&
                # 
                #  ((!is.null(input$inSiteAdmin2) && str_trim(input$inSiteAdmin2, side = "both")!="" ) ||
                #    (!is.null(input$inSiteAdmin2_text) && str_trim(input$inSiteAdmin2_text, side = "both")!="")) &&
                # 
                # ((!is.null(input$inSiteAdmin3) && str_trim(input$inSiteAdmin3, side = "both")!="") ||
                #     (!is.null(input$inSiteAdmin3_text) && str_trim(input$inSiteAdmin3_text, side = "both")!=""))
                
                # ((!is.null(input$inSiteAdmin4) && str_trim(input$inSiteAdmin4, side = "both")!="") ||
                #    (!is.null(input$inSiteAdmin4_text) && str_trim(input$inSiteAdmin4_text, side = "both")!="") ) &&
                
                # ((!is.null(input$inSiteAdmin5) && str_trim(input$inSiteAdmin5, side = "both")!="")  ||
                #    (!is.null(input$inSiteAdmin5_text) && str_trim(input$inSiteAdmin5_text, side = "both")!="") ) &&
                
                # !is.null(input$inSiteNearestPlace) && str_trim(input$inSiteNearestPlace, side = "both")!= "" &&
                # !is.null(input$inSiteLatitude) && str_trim(input$inSiteLatitude, side = "both")!= ""  &&
                # !is.null(input$inSiteLongitude) && str_trim(input$inSiteLongitude, side = "both")!= ""
                
    )
  })
  
  observeEvent(input$btCreateSite, {
    if(validateNewSiteInputs()){
      
      vSiteType <- input$inSiteType         #var1
      vSiteNama <- input$inSiteName         #var2
      vCountry  <- input$inSiteCountry      #var3
      vAdmin1   <- input$inSiteAdmin1       #var4
      
      
      flag_admin2 <- get_admin_agrofims(geodb, country = vCountry, admin1 = vAdmin1 )
      
      if(is.na(flag_admin2)){
        vAdmin2   <- input$inSiteAdmin2_text       #var5
      } else {
        vAdmin2   <- input$inSiteAdmin2
      }
      
      flag_admin3 <- get_admin_agrofims(geodb, country = vCountry, admin1 = vAdmin1, admin2 = vAdmin2)
      
      # print(flag_admin3)
      
      if(is.na(flag_admin3)){
        vAdmin3   <- input$inSiteAdmin3_text       #var5
      } else {
        vAdmin3   <- input$inSiteAdmin3
      }
      
      # print(vCountry)
      # print(vAdmin1)
      # print(vAdmin2)
      # print(vAdmin3)
      
      flag_admin4 <- get_admin_agrofims(geodb, country = vCountry, admin1 = vAdmin1, admin2 = vAdmin2, admin3 = vAdmin3)
      
      # print(flag_admin4)
      
      
      
      if(is.na(flag_admin4)){
        vAdmin4   <- input$inSiteAdmin4_text       #var5
      } else {
        vAdmin4   <- input$inSiteAdmin4
      }
      
      flag_admin5 <- get_admin_agrofims(geodb, country = vCountry,  admin1 = vAdmin1, admin2 = vAdmin2, admin3 = vAdmin3, admin4 = vAdmin4)
      
      if(is.na(flag_admin5)){
        vVillage   <- input$inSiteAdmin5_text       #var5
      } else {
        vVillage   <- input$inSiteAdmin5
      }
      
      vElevation <- input$inSiteElevation #var7
      vLatitud  <- round(input$inSiteLatitude, 5)    #var8
      vLongitude  <- round(input$inSiteLongitude, 5)  #var8
      vNearest <- input$inSiteNearestPlace #var 11
      # vDescNotes <- input$inSiteDescNotes #var10
      
      # validate(
      #   need(input$inSiteLatitude!= "", "Please insert latitude")
      # )
      #
      # validate(
      #   need(input$inSiteLongitude!= "", "Please insert longitude")
      # )
      #
      # validate(
      #   need(input$inSiteElevation!= "", "Please insert eleveation")
      # )
      
      
      #vSiteId <-  stri_rand_strings(1, 5,  '[A-Z]') #var12
      vSiteId <- geohash::gh_encode(vLatitud,vLongitude, 10)
      
      
      date  <- as.character(Sys.time(), "%Y%m%d%H%M%S")
      createDate <-as.character(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
      # insQry=  paste0("insert into user_sites ( " ,
      #                 " var1, var2, var3, var4, var5, var6, var7, var8, var9, var10 ,var11, var12, var13, created, user_id) values('")
      insQry=  paste0('insert into user_sites ( ' ,
                      ' var1, var2, var3, var4, var5, var6, var7, var8, var9, var10 ,var11, var12, var13, created, user_id) values("')
      insQry= paste0(insQry, vSiteType)
      insQry= paste(insQry, vSiteNama, vCountry, vAdmin1, vAdmin2, vAdmin3, vElevation, vLatitud, vLongitude, vVillage,  vNearest, vSiteId, vAdmin4, createDate, sep="\",\"")
      # insQry= paste0(insQry, "', " , USER$id, ")")
      insQry= paste0(insQry, "\", " , session$userData$userId, ")")
      qryUsers = dbSendQuery(mydb, insQry)
      dbDisconnect(mydb)
      updateSiteRDS()
      
      shinyalert("Success", "New site has been successfully added", type = "success")
      
      output$uiCreateSiteNew <- renderUI({
        uiTrialSiteNew()
      })
      updateTextInput(session, "xxxx", value= runif(1))
    }
    else{
      ## TO DO
      ## what to do if the input is not valid.
    }
  })
  
  ### to do: function to validate new site inputs
  validateNewSiteInputs <- function(){
    vLatitud  <- round(input$inSiteLatitude, 5)
    vLongitude  <- round(input$inSiteLongitude, 5)
    
    mgeoCode <- geohash::gh_encode(vLatitud,vLongitude, 10)
    
    mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
    
    strQry = paste0("SELECT count(*) as num FROM user_sites where var12 = '", mgeoCode, "'")
    qryGeoSites = dbSendQuery(mydb,strQry)
    numSites = fetch(qryGeoSites, n=-1)
    dbDisconnect(mydb)
    
    if(numSites$num != 0){
      shinyalert("Error while creating site", paste0("A site with the same coordinates already exists.\nSite ID: ", mgeoCode), type = "error")
      return(FALSE)
    }
    return(TRUE)
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
  
}
