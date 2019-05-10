#' Design field book for HIDAP-AGROFIMS
#'
#' proccess all information provided by users
#' 
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @export
#' @author Omar Benites / Ivan Perez

#' 
server_design_agrofims <- function(input, output, session, values){
  
  ########################################################################################################################################
  ########################################################### NEW CODIGO: IVAN ###########################################################
  ########################################################################################################################################
  
  ############################### Start: Tab Experiment (New) ###############################
  
  # Type of experiment
  
  output$typeExperiment <- renderUI({
    selectizeInput("designFieldbook_typeExperiment", "Type of experiment", multiple = TRUE,
                   options = list(maxItems = 8, placeholder = "Select one..."),
                   choices = c("on-farm",
                               "on-station",
                               "multi-season",
                               "one season",
                               "multi-location",
                               "one location",
                               "long-term (10+ years)",
                               "Other")
    )
  })
  
  observeEvent(input$othertE, {
    # vars <- unlist(strsplit(input$otherGENid, "_"))
    # design <- vars[1]
    # index <- vars[3]
    choises <-  input[[input$othertEid]]
    updateOthertE(choises)
  })
  
  updateOthertE <- function(choises) {
    if (any(choises == "Other") == T) {
      removeUI(selector = "#othertE", immediate = T)
      if (any(choises != "")) {
        # Other
        insertUI(
          selector = "#othertE_aux",
          where = "beforeBegin",
          ui = fluidRow(
            id = "othertE",
            column(
              12,
              textInput("designFieldbook_typeExperiment_other", "")
            )
          )
        )
      }
    } else {
      removeUI(selector = "#othertE", immediate = T)
    }
  }
  
  # Funcion que renderiza los air picker en el tab experiment
  output$exp_end_date <- renderUI({
    if (!is.null(input$fbDesign_project_start_date)) {
      airDatepickerInput("fbDesign_project_end_date",
                         "Experiment end date",
                         clearButton = T,
                         autoClose = T,
                         value = as.Date(input$fbDesign_project_start_date) + 30,
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         placeholder = "yyyy-mm-dd"
      )
    } else {
      airDatepickerInput("fbDesign_project_end_date",
                         "Experiment end date",
                         clearButton = T,
                         autoClose = T,
                         placeholder = "yyyy-mm-dd"
      )
    }
  })
  
  # funcion que imprime Experiment ID (new)
  expIdgenerator2 <- function() {
    x <- input$experimentName
    y <- input$experimentProjectName
    
    a <- substring(x, 1, 2)
    b <- substring(y, 1, 2)
    
    #t <- as.numeric(as.POSIXct("2019-02-12 09:31:06 -05"))
    t <- as.integer(as.POSIXct(Sys.time()))
    
    id <- paste(toupper(a), toupper(b), t, sep = "")
    id
  }
  
  # input "Experiment ID" Ej. EVLB1549379878 (autogenerado)
  output$experimentIdUI <- renderUI({
    disabled(textInput(inputId = "experimentId", label = "Experiment ID",
                       value = expIdgenerator2()))
  })
  
  # funcion que imprime ID principal
  idgenerator <- function() {
    id <- stri_rand_strings(1, 8,  '[A-Z0-9]')
    id
  }
  
  #input ID principal Ej. AKJGKJ56
  output$IdUI <- renderUI({
    disabled(textInput(inputId = "uniqueId", label = "",
                       value = idgenerator(), width = "100px"))
  })
  
  # Funcion que verifica input antes de dibujar el qr para fieldbook
  veriqr <- function() {
    if (input$experimentId != "") {
      a <- input$experimentId
      #print("ëntro")
    } else {
      a <- "NoId"
      #print("no")
    }
    a
  }
  
  # Funcion que renderiza en imagen png el codigo qr para fieldbook
  output$myqr <- renderImage({
    # delay(
    #   1000,
    validate(need(input$experimentId, ""))
    
    if (input$experimentId != "" || !is.null(input$experimentId)) {
      outfile <- tempfile(fileext = '.png')
      
      png(outfile, width = 100, height = 100)
      par(mar=c(0,0,0,0))
      image(qrencode_raster(veriqr()),
            asp=1, col=c("white", "black"), axes=FALSE,
            xlab="", ylab="")
      dev.off()
      
      list(src = outfile,
           contentType = 'image/png',
           width = "100px",
           height = "100px",
           alt = "This is alternate text")
    }
    #)
  }, deleteFile = TRUE)
  
  experimentVars <- reactiveValues()
  
  # Funding Agency
  experimentVars$num_FA <- 0
  experimentVars$DEFAULT_FA <- 1
  experimentVars$ids_FA <- c() # get actives fund. agency ids 
  
  
  # Project Management Entities  
  experimentVars$num_PE <- 0
  experimentVars$DEFAULT_PE <- 1
  experimentVars$ids_PE <- c() # get actives fund. agency ids 
  
  
  # Experiment Leads
  experimentVars$num_EL <- 0
  experimentVars$DEFAULT_EL <- 1
  experimentVars$ids_EL <- c() # get actives fund. agency ids 
  
  
  observeEvent(input$addFundingAgency, {
    defaultBoxes = experimentVars$DEFAULT_FA
    if (experimentVars$num_FA >= 1) {
      insertBoxFundingAgency(experimentVars$num_FA + 1)
    }
  })
  
  observeEvent(input$addManagEntity, {
    defaultBoxes = experimentVars$DEFAULT_PE
    if (experimentVars$num_PE  >= 1) {
      insertBoxManagEntity(experimentVars$num_PE + 1)
    }
  })
  
  observeEvent(input$addExperimentLeads, {
    defaultBoxes = experimentVars$DEFAULT_EL
    if (experimentVars$num_EL >= 1) {
      insertBoxExperimentLead(experimentVars$num_EL + 1)
    }
  })
  
  observe({
    if (experimentVars$num_FA == 0) {
      default <- experimentVars$DEFAULT_FA
      for (i in 1:default) {
        insertBoxFundingAgency(i)
      }
    }
    if (experimentVars$num_PE == 0) {
      default <- experimentVars$DEFAULT_PE
      for (i in 1:default) {
        insertBoxManagEntity(i)
      }
    }
    if (experimentVars$num_EL == 0) {
      default <- experimentVars$DEFAULT_EL
      for (i in 1:default) {
        insertBoxExperimentLead(i)
      }
    }
  })
  
  insertBoxFundingAgency <- function(index) {
    
    experimentVars$ids_FA <- c(experimentVars$ids_FA, paste0("FA_", index))
    
    insertUI(
      selector = "#fr_fundingAgency_boxes",
      where = "beforeBegin",
      ui = getUiFundingAgency(index)
    )
    experimentVars$num_FA <- experimentVars$num_FA + 1
  }
  
  insertBoxManagEntity <- function(index) {
    
    experimentVars$ids_PE <- c(experimentVars$ids_PE, paste0("PE_", index))
    
    
    insertUI(
      selector = "#fr_managementEntities_boxes",
      where = "beforeBegin",
      ui = getUiProjectEntity(index)
    )
    experimentVars$num_PE <- experimentVars$num_PE + 1
  }
  
  insertBoxExperimentLead <- function(index) {
    
    experimentVars$ids_EL <- c(experimentVars$ids_EL, paste0("EL_", index))
    
    
    insertUI(
      selector = "#fr_experimentLeads_boxes",
      where = "beforeBegin",
      ui = getUiExperimentLead(index)
    )
    experimentVars$num_EL <- experimentVars$num_EL + 1
  }
  
  getUiFundingAgency <- function(index) {
    
     
    fluidRow(
      id = paste0("fl_box_fundingAgency_", index), 
      box(
        title = "", solidHeader = TRUE, status = "warning", width=12,
        column(
          12, offset = 0, style='padding:0px; text-align:right;', actionButton(paste0("exp_closeBox_FA_", index), "", icon("close"))
        ),
        fluidRow(
          column(
            6,
            selectizeInput(
              paste0("designFieldbook_fundAgencyType_", index), "Funding agency type", multiple = TRUE,
              options = list(placeholder = "Select one...", maxItems = 1),
              choices = c("Academic institution",
                          "CGIAR center",
                          "Farmer organization",
                          "Finance entity",
                          "Insurance entity",
                          "Foundation",
                          "Public charity",
                          "Government",
                          "Government agency",
                          "International NGO",
                          "National NGO",
                          "Private sector entity",
                          "Other")
            ),
            hidden(textInput(paste0("designFieldbook_fundAgencyType_", index, "_other"), "", value = ""))
            
          ),
          conditionalPanel(
            paste0("input.designFieldbook_fundAgencyType_", index, " != 'CGIAR center'"),
            column(
              6,
              textInput(paste0("designFieldbook_fundAgencyType_name_", index), "Funding agency name")
            )
          ),
          conditionalPanel(
            paste0("input.designFieldbook_fundAgencyType_", index, " == 'CGIAR center'"),
            column(
              6,
              selectizeInput(
                paste0("designFieldbook_fundAgencyType_cgiar_", index), 
                "Choose CGIAR center", multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), 
                choices = c("Africa Rice Center",
                            "Bioversity International",
                            "CIAT - International Center for Tropical Agriclture",
                            "CIFOR - Center for International Forestry Research",
                            "CIMMYT - International Maize and Wheat Improvement Center",
                            "CIP - International Potato Center",
                            "ICARDA - International Center for Agricultural Research in the Dry Areas",
                            "ICRAF - World Agroforestry",
                            "ICRISAT - International Crops Research Institute for the Semi-Arid Tropics",
                            "IFPRI - International Food Policy Research Institute",
                            "IITA - International Institute of Tropical Agriculture",
                            "ILRI - International Livestock Research Institute",
                            "IRRI - International Rice Research Institute",
                            "IWMI -  International Water Management Institute",
                            "WorldFish")
              )
            )
          )
        )
      )
    )
  }
  
  getUiProjectEntity <- function(index) {
    fluidRow(
      id = paste0("fl_box_exp_ent_", index),
      box(
        title = "", solidHeader = TRUE, status = "warning", width=12,     
        column(
          12, offset = 0, style='padding:0px; text-align:right;', actionButton(paste0("exp_closeBox_PE_", index), "", icon("close"))
        ),
        fluidRow(
          column(
            width = 4,
            selectizeInput(
              paste0("projEntity_", index), 
              "Project management entity", multiple =T, options = list(maxItems =1, placeholder = "Select one.."), 
              choices = c("CGIAR center",
                          "Other")
            )
          ),
          conditionalPanel(
            paste0("input.projEntity_", index, " == 'CGIAR center'"),
            column(
              width = 4,
              selectizeInput(
                paste0("contCenter_", index), "Choose CGIAR center", multiple = TRUE, options = list(maxItems =1, placeholder = "Select one..."), 
                choices = c("Africa Rice Center",
                            "Bioversity International",
                            "CIAT - International Center for Tropical Agriclture",
                            "CIFOR - Center for International Forestry Research",
                            "CIMMYT - International Maize and Wheat Improvement Center",
                            "CIP - International Potato Center",
                            "ICARDA - International Center for Agricultural Research in the Dry Areas",
                            "ICRAF - World Agroforestry",
                            "ICRISAT - International Crops Research Institute for the Semi-Arid Tropics",
                            "IFPRI - International Food Policy Research Institute",
                            "IITA - International Institute of Tropical Agriculture",
                            "ILRI - International Livestock Research Institute",
                            "IRRI - International Rice Research Institute",
                            "IWMI -  International Water Management Institute",
                            "WorldFish")
              )
            ),
            column(
              width = 4,
              selectizeInput(
                paste0("contCRP_", index), "Contributor CRP", multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), 
                choices = sort(c("CGIAR Excellence in Breeding Platform",
                                 "CGIAR Genebank Platform",
                                 "CGIAR Platform for Big Data in Agriculture",
                                 "CGIAR Research Program on Agriculture for Nutrition and Health",
                                 "CGIAR Research Program on Climate Change, Agriculture and Food Security",
                                 "CGIAR Research Program on Fish",
                                 "CGIAR Research Program on Forests, Trees and Agroforestry",
                                 "CGIAR Research Program on Maize",
                                 "CGIAR Research Program on Grain Legumes and Dryland Cereals",
                                 "CGIAR Research Program on Livestock",
                                 "CGIAR Research Program on Policies, Institutions, and Markets",
                                 "CGIAR Research Program on Rice",
                                 "CGIAR Research Program on Roots, Tubers and Bananas",
                                 "CGIAR Research Program on Water, Land and Ecosystems",
                                 "CGIAR Research Program on Wheat"))
              )
            )
          ),
          column(
            width =4,style="padding-top: 5px;",
            hidden(textInput(paste0("projEntity_", index, "_other"), "", value = ""))
          )
        )
      )
    )
  }
  
  getUiExperimentLead <- function(index){
    fluidRow(
      id = paste0("fl_box_exp_lead_", index),
      box(
        title = "", solidHeader = TRUE, status = "warning", width = 12,
        column(
          12, offset = 0, style='padding:0px; text-align:right;', actionButton(paste0("exp_closeBox_EL_", index), "", icon("close"))
        ),
        fluidRow(
          column(
            width = 6,
            selectizeInput(
              paste0("projLeadEnt_", index), 
              "Experiment, lead organization type", selected="CGIAR center", multiple = T, options = list(maxItems = 1, placeholder = "Select one..."), 
              choices = c("CGIAR center",
                          "Other")
            ),
            conditionalPanel(
              paste0("input.projLeadEnt_", index, " == 'CGIAR center'"),
              selectizeInput(
                paste0("tLeadCenter_", index), "Choose CGIAR center", multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), 
                choices = c("Africa Rice Center",
                            "Bioversity International",
                            "Center for International Forestry Research (CIFOR)",
                            "International Center for Agricultural Research (ICARDA)",
                            "International Center for Tropical Agriculture (CIAT)",
                            "International Crops Research Institute for the Semi-Arid (ICRISAT)",
                            "International Food Policy Research Institute (IFPRI)",
                            "International Institute of Tropical Agriculture (IITA)",
                            "International Livestock Research Institure (ILRI)",
                            "International Maize and Wheat Improvement Center (CIMMYT)",
                            "International Potato Center (CIP)",
                            "International Rice Research Institute (IRRI)",
                            "International Water Management Institute (IWMI)",
                            "World Agroforestry Centre (ICRAF)",
                            "WorldFish",
                            "None")
              )
            ),
            conditionalPanel(
              paste0("input.projLeadEnt_", index, " == 'Other'"),
              selectizeInput(
                paste0("lead_org_type_", index), "",multiple = TRUE,  options = list(maxItems = 1, placeholder = "Select one..."),
                choices = c("Agricultural experimental extension",
                            "CGIAR center",
                            "Extension organization",
                            "Farm",
                            "Farmer association or cooperative",
                            "Government research institution, designated laboratory or center",
                            "Government research institution (NARS)",
                            "Non-governmental organization",
                            "Private company",
                            "University",
                            "University, main campus",
                            "Other")
              ),
              hidden(textInput(paste0("lead_org_type_", index, "_other"), "")),
              textInput(paste0("leadNameOther_", index), "Experiment, lead organization name", value = "")
            ),
            textInput(inputId = paste0("expLead_", index), label = "Experiment lead person / Primary Investigator", value = "")
          )
        )
      )
    )
  }
  
  observeEvent(input$closeBox_EXP, {
    vars <- unlist(strsplit(input$closeBox_EXPid,"_"))
    type <- vars[3]
    index <- vars[4]
    
    if (type == "FA") {
      removeUI(selector = paste0("#fl_box_fundingAgency_", index), immediate = T)
      experimentVars$ids_FA <- experimentVars$ids_FA[! experimentVars$ids_FA %in% paste0("FA_",index)]
    }
    
    if (type == "PE") {
      removeUI(selector = paste0("#fl_box_exp_ent_", index), immediate = T)
      experimentVars$ids_PE <- experimentVars$ids_PE[! experimentVars$ids_PE %in% paste0("PE_",index)]
      
    }
    
    if (type == "EL") {
      removeUI(selector = paste0("#fl_box_exp_lead_", index), immediate = T)
      experimentVars$ids_EL <- experimentVars$ids_EL[! experimentVars$ids_EL %in% paste0("EL_",index)]
    }
  })
  
  ############################### End: Tab Experiment (New) ###############################
  
  ############################### Start: Tab Personnel (New) ###############################
  
  personnelVars <- reactiveValues()
  
  personnelVars$num <- 0
  personnelVars$DEFAULT <- 1
  personnelVars$ids_PERS <- c()
  
  observeEvent(input$btLoadMyInfoPersonnel, {
    if (session$userData$logged) {
      updateTextInput(session, "person_firstName_1", value = session$userData$userFname)
      updateTextInput(session, "person_lastName_1", value = session$userData$userLname)
      updateTextInput(session, "person_email_1", value = session$userData$userMail)
    }
  })
  
  observeEvent(input$addPersonnel, {
    defaultBoxes = personnelVars$DEFAULT
    if (personnelVars$num >= 1) {
      insertBoxPersonnel(personnelVars$num + 1)
    }
  })
  
  observe({
    if (personnelVars$num == 0) {
      default <- personnelVars$DEFAULT
      for (i in 1:default) {
        insertBoxPersonnel(i)
      }
    }
  })
  
  insertBoxPersonnel <- function(index) {
    
    personnelVars$ids_PERS <- c(personnelVars$ids_PERS, paste0("PERS_", index))
    
    insertUI(
      selector = "#fr_personnel_boxes",
      where = "beforeBegin",
      ui = getUiPersonnel(index)
    )
    personnelVars$num <- personnelVars$num + 1
  }
  
  getUiPersonnel <- function(index){
    fluidRow(
      id = paste0("fr_personnel_box_", index),
      box(
        title = tagList(shiny::icon("user"), "Personnel"), solidHeader = TRUE, status = "warning", width=12,
        column(
          id= paste0("col_close_PERS", index), 
          12, offset = 0, 
          fluidRow(
            column(
              6, style='padding:0px; text-align:left;',
              h4(tagList(shiny::icon("user"), "Personnel"), style="font-weight: 800;color: #555;")
            ),
            column(
              6, style='padding:0px; text-align:right;',
              actionButton(paste0("per_closeBox_", index), "", icon("close"))
            )
          ),
          br()
        ),
        fluidRow(
          column(
            width=6,
            selectizeInput(
              paste0("personnel_type_", index), "Person type", multiple=TRUE,
              options = list(maxItems = 1, placeholder = "Select one..."),
              choices = c("Farmer",
                          "Researcher",
                          "Student",
                          "Research station worker",
                          "Extension agent",
                          "Faculty member",
                          "Other")
            ),
            hidden(textInput(paste0("personnel_type_", index, "_other"), "", value = "")),
            textInput(paste0("person_firstName_", index), "Person, first name", value = ""),
            textInput(paste0("person_lastName_", index), "Person, last name", value = "")
          ),
          column(
            width=6,
            textInput(paste0("person_email_", index), "Person email", value = ""),
            br(),
            selectizeInput(
              paste0("person_affiliation_", index), "Person, affiliation", multiple = T,
              options = list(maxItems = 1, placeholder = "Select one.."),
              choices = c("CGIAR Center",
                          "Other")
            ),
            conditionalPanel(
              paste0("input.person_affiliation_", index," == 'CGIAR Center'" ),
              selectizeInput(
                paste0("person_center_", index), "Organization name", multiple = TRUE,
                options = list(maxItems = 1, placeholder = "Select one..."),
                choices = c("Africa Rice Center",
                            "Bioversity International",
                            "Center for International Forestry Research (CIFOR)",
                            "International Center for Agricultural Research (ICARDA)",
                            "International Center for Tropical Agriculture (CIAT)",
                            "International Crops Research Institute for the Semi-Arid (ICRISAT)",
                            "International Food Policy Research Institute (IFPRI)",
                            "International Institute of Tropical Agriculture (IITA)",
                            "International Livestock Research Institure (ILRI)",
                            "International Maize and Wheat Improvement Center (CIMMYT)",
                            "International Potato Center (CIP)",
                            "International Rice Research Institute (IRRI)",
                            "International Water Management Institute (IWMI)",
                            "World Agroforestry Centre (ICRAF)",
                            "WorldFish")
              )
            ),
            hidden(textInput(paste0("person_affiliation_", index, "_other"), "", value = "")),
            textInput(
              inputId = paste0("person_orcid_", index),
              label = HTML("Person, ORCID id if available (if not, consider <a href='https://orcid.org/register' target='_blank'>registering</a>!)"),
              value = ""
            )
          )
        )
      )
    )
  }
  
  observeEvent(input$closeBox_PER, {
    vars <- unlist(strsplit(input$closeBox_PERid,"_"))
    index <- vars[3]
    
    removeUI(selector = paste0("#fr_personnel_box_", index), immediate = T)
    personnelVars$ids_PERS <- personnelVars$ids_PERS[! personnelVars$ids_PERS %in% paste0("PERS_",index)]
    
  })
  
  ############################### End: Tab Personnel (New) ###############################
  
  ############################### Start: Tab Site (New) ###############################
  
  
  
  ############################### End: Tab Site (New) ###############################
  
  ############################### Start: Tab Crop (New) ###############################
  
  ## Fieldbook details
  
  # funcion que imprime Experiment ID (old)
  expIdgenerator <- function() {
    # FMCassava200910_LaMolina
    
    if (input$croppingType == "Monocrop") {
      m <- as.character(input$croppingType)
      m <- substring(m, 1, 1)
      c <- input$cropCommonNameMono
      c <- gsub(" ", "", c)
      y <- input$fbDesign_project_start_date
      y <- substring(y[1], 1, 7)
      y <- gsub("-", "", y)
      l <- input$fbDesign_countryTrial
      #l <- "LocationName"
      
      id <- paste0("F", m, c, y, "_", l)
    } else if (input$croppingType == "Intercrop") {
      # m <- as.character(input$croppingType)
      # m <- substring(m, 1, 1)
      # c <- c()
      # c <- paste0(input$cropCommonName1,
      #             input$cropCommonName2,
      #             input$cropCommonName3,
      #             input$cropCommonName4,
      #             input$cropCommonName5,
      #             input$cropCommonName6,
      #             input$cropCommonName7)
      # 
      # y <- input$fbDesign_project_start_date
      # y <- substring(y[1], 1, 7)
      # y <- gsub("-", "", y)
      # l <- input$fbDesign_countryTrial
      # #l <- "LocationName"
      # 
      # id <- paste0("F", m, c, y, "_", l)
      
      # nueva version Intercrop
      y <- input$fbDesign_project_start_date
      y <- substring(y[1], 1, 7)
      y <- gsub("-", "", y)
      l <- input$fbDesign_countryTrial
      
      id <- paste0("FInt", y, "_", l)
    } else if (input$croppingType == "Relay crop") {
      # m <- as.character(input$croppingType)
      # m <- substring(m, 1, 1)
      # c <- c()
      # c <- paste0(input$cropCommonName1,
      #             input$cropCommonName2,
      #             input$cropCommonName3,
      #             input$cropCommonName4,
      #             input$cropCommonName5,
      #             input$cropCommonName6,
      #             input$cropCommonName7)
      # 
      # y <- input$fbDesign_project_start_date
      # y <- substring(y[1], 1, 7)
      # y <- gsub("-", "", y)
      # l <- input$fbDesign_countryTrial
      # #l <- "LocationName"
      # 
      # id <- paste0("F", m, c, y, "_", l)
      
      # nueva version Relay crop
      y <- input$fbDesign_project_start_date
      y <- substring(y[1], 1, 7)
      y <- gsub("-", "", y)
      l <- input$fbDesign_countryTrial
      
      id <- paste0("FRel", y, "_", l)
    } else if (input$croppingType == "Rotation") {
      # m <- as.character(input$croppingType)
      # m <- substring(m, 1, 1)
      # c <- c()
      # c <- paste0(input$cropCommonName1,
      #             input$cropCommonName2,
      #             input$cropCommonName3,
      #             input$cropCommonName4,
      #             input$cropCommonName5,
      #             input$cropCommonName6,
      #             input$cropCommonName7)
      # 
      # y <- input$fbDesign_project_start_date
      # y <- substring(y[1], 1, 7)
      # y <- gsub("-", "", y)
      # l <- input$fbDesign_countryTrial
      # #l <- "LocationName"
      # 
      # id <- paste0("F", m, c, y, "_", l)
      
      # nueva version Rotation
      y <- input$fbDesign_project_start_date
      y <- substring(y[1], 1, 7)
      y <- gsub("-", "", y)
      l <- input$fbDesign_countryTrial
      
      id <- paste0("FRot", y, "_", l)
    }
    
    id
  }
  
  # input "Fieldbook ID" Ej. FMCassava200910_LaMolina
  output$fieldbookIdUI <- renderUI({
    disabled(textInput(inputId = "fieldbookId", label = "Fieldbook ID",
                       value = expIdgenerator()))
  })
  
  # Funcion que verifica input antes de dibujar el qr para fieldbook
  veriqr2 <- function() {
    if (input$fieldbookId != "") {
      a <- input$fieldbookId
      #print("ëntro")
    } else {
      a <- "NoId"
      #print("no")
    }
    a
  }
  
  # Funcion que renderiza en imagen png el codigo qr para fieldbook
  output$myqr2 <- renderImage({
    # delay(
    #   1000,
    validate(need(input$fieldbookId, ""))
    
    if (input$fieldbookId != "" || !is.null(input$fieldbookId)) {
      outfile <- tempfile(fileext = '.png')
      
      png(outfile, width = 100, height = 100)
      par(mar=c(0,0,0,0))
      image(qrencode_raster(veriqr2()),
            asp=1, col=c("white", "black"), axes=FALSE,
            xlab="", ylab="")
      dev.off()
      
      list(src = outfile,
           contentType = 'image/png',
           width = "100px",
           height = "100px",
           alt = "This is alternate text")
    }
    #)
  }, deleteFile = TRUE)
  
  ## Cropping type
  
  ###################### START: INTERCROP ######################
  
  # Intercrop: Asigna variables reactivas
  NUM_BOX_INTERCROP_DEFAULT <- 2
  cropsVar <- reactiveValues()
  
  cropsVar$selectedIntercrop <- list()
  cropsVar$indexOtherIntercrop <- 0
  cropsVar$varAuxOtherIntercrop <- ""
  cropsVar$numIntercropShown <- NUM_BOX_INTERCROP_DEFAULT 
  cropsVar$CropsSelectedInterCrop <- list()
  
  intercropVars <- reactiveValues()
  intercropVars$num <- 0
  intercropVars$DEFAULT <- 2
  intercropVars$DEFAULTMAX <- 2
  intercropVars$ids <- c()
  
  
  # Intercrop: Inserta por defecto un row
  observe({
    if (intercropVars$num == 0) {
      default <- intercropVars$DEFAULT
      for (i in 1:default) {
        insertBoxcrop(i, typeCrop = "int")
      }
    }
  })
  
  # Intercrop: Agrega un row al hacer clic en el boton "Add crop"
  
  observeEvent(input$addIntercrop, {
    if (intercropVars$num >= 1) {
      
      insertBoxcrop(intercropVars$num + 1, typeCrop = "int")
      intercropVars$DEFAULTMAX <- intercropVars$DEFAULTMAX + 1
      
      if(intercropVars$DEFAULTMAX == 5){
        shinyjs::hide("addIntercrop")
      }
    }
  })
  
  
  
  ###################### END: INTERCROP ######################
  
  ###################### START: RELAYCROP ######################
  
  # Relaycrop: Asigna variables reactivas
  relaycropVars <- reactiveValues()
  relaycropVars$num <- 0
  relaycropVars$DEFAULT <- 2
  relaycropVars$DEFAULTMAX <- 2
  relaycropVars$ids <- c()
  
  # Relaycrop: Inserta por defecto un row
  observe({
    if (relaycropVars$num == 0) {
      default <- relaycropVars$DEFAULT
      for (i in 1:default) {
        insertBoxcrop(i, typeCrop = "rel")
      }
    }
  })
  
  # Relaycrop: Agrega un row al hacer clic en el boton "Add crop"
  observeEvent(input$addRelaycrop, {
    if (relaycropVars$num >= 1) {
      insertBoxcrop(relaycropVars$num + 1, typeCrop = "rel")
      relaycropVars$DEFAULTMAX <- relaycropVars$DEFAULTMAX + 1
      
      if(relaycropVars$DEFAULTMAX == 5){
        shinyjs::hide("addRelaycrop")
      }
    }
    
    # if(relaycropVars$num == 4)
    #   shinyjs::hide("addIntercrop")
  })
  
  
  observeEvent(input$cropBoxRelayVarOther,{
    vars <- unlist(strsplit(input$cropBoxRelayVarOtherId, "_"))
    cropType <- vars[1]
    crop_order <- vars[3]
    mtext <- input[[input$cropBoxRelayVarOtherId]]
    
    if(mtext=="")
      mtext=="Other"
    
    if (cropType == "rel") {
      #Intercrop
      if (input[[paste0("rel_cropCommonName_", crop_order)]] == "Other") {
        #Planting and Transplanting
        output[[paste0("title_panel_rel_pt_", crop_order)]] = renderText({
          mtext
        })
        #Harvest
        output[[paste0("title_panel_rel_harv_", crop_order)]] = renderText({
          mtext
        })
      }
      
    }
  })
  
  
  
  
  ###################### END: RELAYCROP ######################
  
  ###################### START: ROTATION ######################
  
  # Rotation: Asigna variables reactivas
  rotationcropVars <- reactiveValues()
  rotationcropVars$num <- 0
  rotationcropVars$DEFAULT <- 2
  rotationcropVars$ids <- c()
  
  # Rotation: Inserta por defecto un row
  observe({
    if (rotationcropVars$num == 0) {
      default <- rotationcropVars$DEFAULT
      for (i in 1:default) {
        insertBoxcrop(i, typeCrop = "rot")
      }
    }
  })
  
  # Rotation: Agrega un row al hacer clic en el boton "Add crop"
  observeEvent(input$addRotationcrop, {
    if (rotationcropVars$num >= 1) {
      insertBoxcrop(rotationcropVars$num + 1, typeCrop = "rot")
    }
  })
  
  ###################### END: ROTATION ######################
  
  ###################### START: FUNCIONES GENERALES INTERCROP/RELAYCROP/ROTATION ######################
  
  # Funcion GENERAL que inserta el UI dependiendo del tipo de cultivo
  insertBoxcrop <- function(index, typeCrop){
    if (typeCrop == "int") {
      intercropVars$ids <- c(intercropVars$ids, paste0("int_", index))
      intercropVars$num <- intercropVars$num + 1
      prev <- unlist(strsplit(intercropVars$ids[intercropVars$num - 1] ,"_"))
      output[[paste0("intercropName_row_crop_", index)]] <- renderText(paste0("Crop"))
      output[[paste0("intercropX_row_crop_", prev[2])]] <- renderText("X")
      
      insertUI(
        selector = "#fr_intercrop_boxes",
        where = "beforeBegin",
        ui = getUicropBox(index, typeCrop)
      )
      
      insertUI(
        selector = "#fr_intercrop_geometry_boxes",
        where = "beforeBegin",
        ui = getUiIntercropGeometryCol(index)
      )
    } else if (typeCrop == "rel") {
      relaycropVars$ids <- c(relaycropVars$ids, paste0("rel_", index))
      
      insertUI(
        selector = "#fr_relaycrop_boxes",
        where = "beforeBegin",
        ui = getUicropBox(index, typeCrop)
      )
      relaycropVars$num <- relaycropVars$num + 1
    } else if (typeCrop == "rot") {
      rotationcropVars$ids <- c(rotationcropVars$ids, paste0("rot_", index))
      
      insertUI(
        selector = "#fr_rotationcrop_boxes",
        where = "beforeBegin",
        ui = getUicropBox(index, typeCrop)
      )
      rotationcropVars$num <- rotationcropVars$num + 1
    }
  }
  
  # Funcion GENERAL que dibuja el titulo de INTERCROP/RELAYCROP/ROTATION dependiendo del tipo de cultivo
  titleCROP <- function(index, typeCrop) {
    if (typeCrop == "int") {
      title <- c("Select crop", "Crop variety name(s)")
    } else if (typeCrop == "rel") {
      if (index == 1) {
        title <- c("First crop common name", 
                   "First crop variety name")
      } else if (index == 2) {
        title <- c("Relay crop common name", 
                   "Relay crop variety name")
      } else {
        title <- c("Relay crop common name", 
                   "Relay crop variety name")
      }
    } else if (typeCrop == "rot") {
      title <- c("Select crop",
                 "Crop variety name(s)",
                 "Order in the rotation")
    }
  }
  
  # Funcion GENERAL que dibuja el UI dependiendo del tipo de cultivo
  getUicropBox <- function(index, typeCrop) {
    title <- titleCROP(index, typeCrop)
    
    if (typeCrop == "int") {
      fluidRow(
        id= paste0(typeCrop, "_fr_box_", index),
        box(
          title = paste0(""), solidHeader = TRUE, status = "warning", width = 12,
          column(
            12, offset = 0, 
            fluidRow(
              column(6, style = 'padding:0px; text-align:left;',
                     h4(tagList(shiny::icon(""), "Crop"), style = "font-weight: 800;color: #555;")
              ),
              column(6, style = 'padding:0px; text-align:right;',
                     actionButton(paste0(typeCrop, "_closeCrop_", index), "", icon("close"))
              )
            ),
            br()
          ),
          fluidRow(
            column(
              6,
              selectizeInput(
                paste0(typeCrop, "_cropCommonName_", index), label = title[1], selected = NULL, 
                multiple = T , options = list(maxItems = 1, placeholder = "Select crop"),
                choices = c("Cassava",
                            "Common bean",
                            "Maize",
                            "Potato",
                            "Rice",
                            "Sweetpotato",
                            "Wheat",
                            "Other")
              ),
              hidden(textInput(paste0(typeCrop, "_cropCommonName_", index, "_other"), "", value = ""))
            ),
            column(
              width = 6,
              selectizeInput(
                inputId = paste0(typeCrop, "_cropVarietyName_", index), label = title[2], 
                choices = c(), multiple = T, options = list('create' = TRUE)
              )
            )
          )
        )
      )
    } else if (typeCrop == "rel") {
      fluidRow(
        id= paste0(typeCrop, "_fr_box_", index),
        box(
          title = paste0(""), solidHeader = TRUE, status = "warning", width=12,
          column(
            12, offset = 0, 
            fluidRow(
              column(6, style = 'padding:0px; text-align:left;',
                     h4(tagList(shiny::icon(""), "Crop"), style="font-weight: 800;color: #555;")
              ),
              column(6, style = 'padding:0px; text-align:right;',
                     actionButton(paste0(typeCrop, "_closeCrop_", index), "", icon("close"))
              )
            ),
            br()
          ),
          fluidRow(
            column(
              6,
              selectizeInput(
                paste0(typeCrop, "_cropCommonName_", index), label = title[1], selected = NULL, 
                multiple = T , options = list(maxItems = 1, placeholder = "Select crop"),
                choices = c("Cassava",
                            "Common bean",
                            "Maize",
                            "Potato",
                            "Rice",
                            "Sweetpotato",
                            "Wheat",
                            "Other")
              ),
              hidden(textInput(paste0(typeCrop, "_cropCommonName_", index, "_other"), "", value = ""))
            ),
            column(
              6,
              selectizeInput(
                inputId = paste0(typeCrop, "_cropVarietyName_", index), label = title[2], 
                choices = c(), multiple = T, options = list('create' = TRUE)
              )
            )
          )
        )
      )
    } else if (typeCrop == "rot") {
      fluidRow(
        id= paste0(typeCrop, "_fr_box_", index),
        box(
          title = paste0(""), solidHeader = TRUE, status = "warning", width = 12,
          column(
            12, offset = 0, 
            fluidRow(
              column(6, style = 'padding:0px; text-align:left;',
                     h4(tagList(shiny::icon(""), "Crop"), style="font-weight: 800;color: #555;")
              ),
              column(6, style = 'padding:0px; text-align:right;',
                     actionButton(paste0(typeCrop, "_closeCrop_", index), "", icon("close"))
              )
            ),
            br()
          ),
          fluidRow(
            column(
              4,
              selectizeInput(
                paste0(typeCrop, "_cropCommonName_", index), label = title[1], selected = NULL, 
                multiple = T , options = list(maxItems = 1, placeholder = "Select crop"),
                choices = c("Cassava",
                            "Common bean",
                            "Maize",
                            "Potato",
                            "Rice",
                            "Sweetpotato",
                            "Wheat",
                            "Other")
              ),
              hidden(textInput(paste0(typeCrop, "_cropCommonName_", index, "_other"), "", value = ""))
            ),
            column(
              width = 4,
              selectizeInput(
                inputId = paste0(typeCrop, "_cropVarietyName_", index), label = title[2], 
                choices = c(), multiple = T, options = list('create' = TRUE)
              )
            ),
            column(
              width = 4,
              selectizeInput(
                inputId = paste0(typeCrop, "_orderRotation_", index), label = title[3], 
                choices = c(), multiple = T, options = list('create' = TRUE)
              )
            )
          )
        )
      )
    }
  }
  
  # Funcion GENERAL que dibuja el UI "row geometry" solo para INTERCROP
  getUiIntercropGeometryCol <- function(index){
    column(
      3, 
      id = paste0("intercrop_rows_crop_", index), style='padding:0px;',
      column(5, offset = 0, style = 'padding:25px 2px 0px 0px; text-align:center; word-wrap: break-word;', uiOutput(paste0("intercropName_row_crop_", index))),
      column(4, offset = 0, style = 'padding:0px; text-align:left;', textInput(paste0("intercropValue_row_crop_", index), "")),
      column(
        3, offset = 0, style='padding:25px 0px 0px 20px; text-align:center; word-wrap: break-word;',
        fluidRow(
          column(9, offset = 0, style = 'padding:0px; text-align:center;', "row(s)"),
          column(3, offset = 0, style = 'padding:0px; text-align:center;', uiOutput(paste0("intercropX_row_crop_", index)))
        )
      )
    )
  }
  
  # When intercrop is selected --> solo para intercrop
  observeEvent(input$cropBoxInterVar, {
    
    vars <- unlist(strsplit(input$cropBoxInterVarId, "_"))
    crop_order <- vars[3]
    value <- input[[input$cropBoxInterVarId]]
    xtitle <- "Crop"
    
    if (is.null(value)) {
      output[[paste0("intercropName_row_crop_", crop_order)]] <- renderText(paste0("Crop"))
      cropsVar$CropsSelectedInterCrop[[paste0('C', crop_order)]] <- NULL
    } else {
      if (value == "Other") {
        if (input[[paste0(input$cropBoxInterVarId, "_other")]] == '') {
          output[[paste0("intercropName_row_crop_", crop_order)]] <- renderText(value)
          xtitle <- "Other"
        } else {
          output[[paste0("intercropName_row_crop_", crop_order)]] <- renderText(input[[paste0(input$cropBoxInterVarId, "_other")]])
          xtitle <- input[[paste0(input$cropBoxInterVarId, "_other")]]
        }
      }
      else {
        output[[paste0("intercropName_row_crop_", crop_order)]] <- renderText(value)
        xtitle <- value
      }
      cropsVar$CropsSelectedInterCrop[[paste0('C', crop_order)]] <- value
    }
  })
  
  # When 'other crop' name is filled --> solo para intercrop
  observeEvent(input$cropBoxInterVarOther,{
    
    vars <- unlist(strsplit(input$cropBoxInterVarOtherId, "_"))
    cropType <- vars[1]
    crop_order <- vars[3]
    mtext <- input[[input$cropBoxInterVarOtherId]]
    
    if (mtext == "")
      mtext <- "Other"
    output[[paste0("intercropName_row_crop_", crop_order)]] <-
      renderText(mtext)
    
    if (cropType == "int") {
      #Intercrop
      if (input[[paste0("int_cropCommonName_", crop_order)]] == "Other") {
        #Planting and Transplanting
        output[[paste0("title_panel_int_pt_", crop_order)]] = renderText({
          mtext
        })
        #Harvest
        output[[paste0("title_panel_int_harv_", crop_order)]] = renderText({
          mtext
        })
      }
    }
  })
  
  # Funcion GENERAL que activa "Close"
  observeEvent(input$closeBox_CROP, {
    vars <- unlist(strsplit(input$closeBox_CROPid,"_"))
    typeCrop <- vars[1]
    typeMangPrac <- vars[2]
    index <- vars[3]
    
    if (typeCrop == "int") {
      if (length(intercropVars$ids) > 2) {
        removeUI(selector = paste0("#", typeCrop, "_fr_box_", index), immediate = T)
        removeUI(selector = paste0("#intercrop_rows_crop_", index), immediate = T)
        
        # Planting Transplanting
        removeTab(inputId = paste0("tabpanelPT", typeCrop), target = paste0("int_pt_",index)) #Removemos tab panel de PT
        expconPTinter$var <- expconPTinter$var[!expconPTinter$var %in% paste0("int_pt_",index)] #Actualizamos variable reactiva
        # Harvest
        removeTab(inputId = paste0("tabpanelHARV", typeCrop), target = paste0("int_harv_",index)) #Removemos tab panel de Harvest
        expconHARVinter$var <- expconHARVinter$var[!expconHARVinter$var %in% paste0("int_harv_",index)] #Actualizamos variable reactiva
        
        intercropVars$ids <- intercropVars$ids[!intercropVars$ids %in% paste0("int_", index)]
      }
    } else if (typeCrop == "rel") {
      if (length(relaycropVars$ids) > 2) {
        removeUI(selector = paste0("#", typeCrop, "_fr_box_", index), immediate = T)
        relaycropVars$ids <- relaycropVars$ids[!relaycropVars$ids %in% paste0("rel_", index)]
        
        # Planting Transplanting
        removeTab(inputId = paste0("tabpanelPT", typeCrop), target = paste0("rel_pt_",index)) #Removemos tab panel de PT
        expconPTinter$var <- expconPTinter$var[!expconPTinter$var %in% paste0("rel_pt_",index)] #Actualizamos variable reactiva
        # Harvest
        removeTab(inputId = paste0("tabpanelHARV", typeCrop), target = paste0("rel_harv_",index)) #Removemos tab panel de Harvest
        expconHARVinter$var <- expconHARVinter$var[!expconHARVinter$var %in% paste0("rel_harv_",index)] #Actualizamos variable reactiva
        
      }
    } else if (typeCrop == "rot") {
      if (length(rotationcropVars$ids) > 2) {
        removeUI(selector = paste0("#", typeCrop, "_fr_box_", index), immediate = T)
        rotationcropVars$ids <- rotationcropVars$ids[!rotationcropVars$ids %in% paste0("rot_", index)]
      }
    }
    
    
  })
  
  ###################### END: FUNCIONES GENERALES INTERCROP/RELAYCROP/ROTATION  ######################
  
  ############################### End: Tab Crop (New) ###############################
  
  ####################################################################################
  ############################### START SERVER: DESIGN ###############################
  
  # Database
  factores <- agdesign::dtfactordesign   #agdesign::dtfactordesign  #readxl::read_excel("FACTOR_V9.xlsx")
  dt <- factores %>% mutate(fchoices = FACTOR)
  
  ###################### START: CRD ######################
  
  # CRD: Asigna variables reactivas
  factorCRD <- reactiveValues()
  factorCRD$num <- 0
  factorCRD$DEFAULT <- 1
  factorCRD$ids <- c()
  
  # CRD: Treatments
  factorCRD$currNumReplications <- 2
  factorCRD$numRepAux <- 0
  
  # CRD: Variables de apoyo para la creacion de columnas, filas para la tabla inferior en CRD
  designVarsCRD <- reactiveValues()
  designVarsCRD$num_NFULL <- 0
  designVarsCRD$DEFAULT_TREAT_NFULL <- 2
  designVarsCRD$ids_NFULL <- c()
  
  # CRD: Inserta por defecto un row
  observe({
    if (factorCRD$num == 0) {
      defaultCRD <- factorCRD$DEFAULT
      
      for (i in 1:defaultCRD) {
        insertRow_GEN(i, design = "crd")
      }
    }
  })
  
  # CRD: Agrega un row al hacer clic en el boton "Add factor"
  observeEvent(input$crd_add, {
    if (factorCRD$num >= 1) {
      insertRow_GEN(factorCRD$num + 1, design = "crd")
      drawNewColumnNFF(paste0("crd_", factorCRD$num),"crd") #factorCRD$num es reactiva, no necesita +1 nuevamente
    }
  })
  
  ###################### END: CRD ######################
  
  ###################### START: RCBD ######################
  
  # RCBD: Asigna variables reactivas
  factorRCBD <- reactiveValues()
  factorRCBD$num <- 0
  factorRCBD$DEFAULT <- 1
  factorRCBD$ids <- c()
  
  # RCBD: Treatments
  factorRCBD$currNumReplications <- 2
  factorRCBD$numRepAux <- 0
  
  # RCBD: Variables de apoyo para la creacion de columnas, filas para la tabla RCBD
  designVarsRCBD <- reactiveValues()
  designVarsRCBD$num_NFULL <- 0
  designVarsRCBD$DEFAULT_TREAT_NFULL <- 2
  designVarsRCBD$ids_NFULL <- c()
  
  # RCBD: Inserta por defecto un row
  observe({
    if (factorRCBD$num == 0) {
      defaultRCBD <- factorRCBD$DEFAULT
      
      for (i in 1:defaultRCBD) {
        insertRow_GEN(i, design = "rcbd")
      }
    }
  })
  
  # RCBD: Agrega un row al hacer clic en el boton "Add factor"
  observeEvent(input$rcbd_add, {
    if (factorRCBD$num >= 1) {
      insertRow_GEN(factorRCBD$num + 1, design = "rcbd")
      drawNewColumnNFF(paste0("rcbd_", factorRCBD$num),"rcbd") #factorRCBD$num es reactiva, no necesita +1 nuevamente
    }
  })
  
  ###################### END: RCBD ######################
  
  ##################### START: FCRD #####################
  
  # FCRD: Asigna variables reactivas
  factorFCRD <- reactiveValues()
  factorFCRD$num <- 0
  factorFCRD$DEFAULT <- 2
  factorFCRD$ids <- c()
  
  # FCRD: Inserta por defecto un row
  observe({
    if (factorFCRD$num == 0) {
      defaultFCRD <- factorFCRD$DEFAULT
      
      for (i in 1:defaultFCRD) {
        insertRow_GEN(i, design = "fcrd")
      }
    }
  })
  
  # FCRD: Agrega un row al hacer clic en el boton "Add factor"
  observeEvent(input$fcrd_add, {
    if (factorFCRD$num >= 1) {
      insertRow_GEN(factorFCRD$num + 1, design = "fcrd")
    }
  })
  
  ###################### END: FCRD ######################
  
  ##################### START: FRCBD #####################
  
  # FRCBD: Asigna variables reactivas
  factorFRCBD <- reactiveValues()
  factorFRCBD$num <- 0
  factorFRCBD$DEFAULT <- 2
  factorFRCBD$ids <- c()
  
  # FRCBD: Inserta por defecto un row
  observe({
    if (factorFRCBD$num == 0) {
      defaultFRCBD <- factorFRCBD$DEFAULT
      
      for (i in 1:defaultFRCBD) {
        insertRow_GEN(i, design = "frcbd")
      }
    }
  })
  
  # FRCBD: Agrega un row al hacer clic en el boton "Add factor"
  observeEvent(input$frcbd_add, {
    if (factorFRCBD$num >= 1) {
      insertRow_GEN(factorFRCBD$num + 1, design = "frcbd")
    }
  })
  
  ##################### END: FRCBD #####################
  
  ###################### START: SPRCBD ######################
  
  # SPRCBD: Asigna variables reactivas
  factorSPRCBD <- reactiveValues()
  factorSPRCBD$num <- 0
  factorSPRCBD$DEFAULT <- 2
  factorSPRCBD$ids <- c()
  # SPRCBD: Inserta por defecto un row
  observe({
    if (factorSPRCBD$num == 0) {
      defaultSPRCBD <- factorSPRCBD$DEFAULT
      
      for (i in 1:defaultSPRCBD) {
        insertRow_GEN(i, design = "sprcbd")
      }
    }
  })
  
  # SPRCBD: Agrega un row al hacer clic en el boton "Add factor"
  observeEvent(input$sprcbd_add, {
    if (factorSPRCBD$num >= 1) {
      insertRow_GEN(factorSPRCBD$num + 1, design = "sprcbd")
    }
  })
  
  ###################### END: SPRCBD ######################
  
  ###################### START: SPSP ######################
  
  # SPSP: Asigna variables reactivas
  factorSPSP <- reactiveValues()
  factorSPSP$num <- 0
  factorSPSP$DEFAULT <- 3
  factorSPSP$ids <- c()
  
  # SPSP: Inserta por defecto un row
  observe({
    if (factorSPSP$num == 0) {
      defaultSPSP <- factorSPSP$DEFAULT
      
      for (i in 1:defaultSPSP) {
        insertRow_GEN(i, design = "spsp")
      }
    }
  })
  
  # SPSP: Agrega un row al hacer clic en el boton "Add factor"
  observeEvent(input$spsp_add, {
    if (factorSPSP$num >= 1) {
      insertRow_GEN(factorSPSP$num + 1, design = "spsp")
    }
  })
  
  ###################### END: SPSP ######################
  
  ###################### START: Funciones GENERALES CRD/RCBD/FCRD/FRCBD/SPRCBD/SPSP ######################
  
  # Variables de apoyo para obtener el diseno 
  # Son utiles para la creacion de tablas en CRD y RCBD
  designVarsFactor <- reactiveValues()
  designVarsFactor$design <- NULL
  
  # CRD/RCBD: Variables reactivas para capturar valores de los inputs
  fvalues <- reactiveValues()
  fvalues$flbl_crd <- NULL
  fvalues$flvl_crd <- NULL
  fvalues$flbl_rcbd <- NULL
  fvalues$flvl_rcbd <- NULL
  
  # Funcion GENERAL que responde al cambio de FACTORES 
  observeEvent(input$designFieldbook_agrofims, {
    designVarsFactor$design <- tolower(input$designFieldbook_agrofims) #tolower para usarlo en los Ids
    
    if (designVarsCRD$num_NFULL == 0 && designVarsFactor$design == "crd") {
      designVarsCRD$num_NFULL <- designVarsCRD$num_NFULL + 1
      crdId <- getFactorIds("crd") # obtiene ids de los factorboxs
      drawNewColumnNFF(crdId,designVarsFactor$design)
    } else if (designVarsRCBD$num_NFULL == 0 && designVarsFactor$design == "rcbd") {
      designVarsRCBD$num_NFULL <- designVarsRCBD$num_NFULL + 1
      rcbdId <- getFactorIds("rcbd") # obtiene ids de los factorboxs
      drawNewColumnNFF(rcbdId,designVarsFactor$design)
    }
  })
  
  #Funcion GENERAL que obtiene ids de los factor boxes
  getFactorIds <- function(design) {
    if (design == "crd") {
      factorCRD$ids
    } else if (design == "rcbd") {
      factorRCBD$ids
    } else if (design == "fcrd") {
      factorFCRD$ids
    } else if (design == "frcbd") {
      factorFRCBD$ids
    } else if (design == "sprcbd") {
      factorSPRCBD$ids
    } else if (design == "spsp") {
      factorSPSP$ids
    }
  }
  
  # Funcion GENERAL que inserta el UI dependiendo del diseno
  insertRow_GEN <- function(index, design) {
    # CRD
    if (design == "crd") {
      factorCRD$ids <- c(factorCRD$ids, paste0(design, "_", index))
      insertUI(
        selector = "#crd_boxes",
        where = "beforeBegin",
        ui = getDesignUI_GEN(index, design)
      )
      factorCRD$num <- factorCRD$num + 1
    }
    
    # RCBD
    if (design == "rcbd") {
      factorRCBD$ids <- c(factorRCBD$ids, paste0(design, "_", index))
      insertUI(
        selector = "#rcbd_boxes",
        where = "beforeBegin",
        ui = getDesignUI_GEN(index, design)
      )
      factorRCBD$num <- factorRCBD$num + 1
    }
    
    # FCRD
    if (design == "fcrd") {
      factorFCRD$ids <- c(factorFCRD$ids, paste0(design, "_", index))
      insertUI(
        selector = "#fcrd_boxes",
        where = "beforeBegin",
        ui = getDesignUI_GEN(index, design)
      )
      factorFCRD$num <- factorFCRD$num + 1
    }
    
    # FRCBD
    if (design == "frcbd") {
      factorFRCBD$ids <- c(factorFRCBD$ids, paste0(design, "_", index))
      insertUI(
        selector = "#frcbd_boxes",
        where = "beforeBegin",
        ui = getDesignUI_GEN(index, design)
      )
      factorFRCBD$num <- factorFRCBD$num + 1
    }
    
    # SPRCBD
    if (design == "sprcbd") {
      factorSPRCBD$ids <- c(factorSPRCBD$ids, paste0(design, "_", index))
      insertUI(
        selector = "#sprcbd_boxes",
        where = "beforeBegin",
        ui = getDesignUI_GEN(index, design)
      )
      factorSPRCBD$num <- factorSPRCBD$num + 1
    }
    
    # SPSP
    if (design == "spsp") {
      factorSPSP$ids <- c(factorSPSP$ids, paste0(design, "_", index))
      insertUI(
        selector = "#spsp_boxes",
        where = "beforeBegin",
        ui = getDesignUI_GEN(index, design)
      )
      factorSPSP$num <- factorSPSP$num + 1
    }
  }
  
  #Funcion que dibuja el titulo de SPRCBD/SPSP dependiendo del diseno
  titleGEN <- function(index, design) {
    if (design == "crd") {
      title <- "Factor"
    } else if (design == "rcbd") {
      title <- "Factor"
    } else if (design == "fcrd") {
      title <- "Factor"
    } else if (design == "frcbd") {
      title <- "Factor"
    } else if (design == "sprcbd") {
      if (index == 1) {
        title <- "Factor: main plot"
      } else if (index == 2) {
        title <- "Factor: sub plot"
      } else if (index >= 3) {
        title <- "Factor"
      }
    } else if (design == "spsp") {
      if (index == 1) {
        title <- "Factor: main plot"
      } else if (index == 2) {
        title <- "Factor: sub plot"
      } else if (index == 3) {
        title <- "Factor: sub-sub plot"
      } else if (index >= 4) {
        title <- "Factor"
      }
    }
  }
  
  # Funcion GENERAL que dibuja el UI dependiendo del diseno
  getDesignUI_GEN <- function(index, design, value = NULL) {
    title <- titleGEN(index, design)
    
    fluidRow(
      id = paste0(design, "_full_factor_box_", index),
      box(
        solidHeader = TRUE,
        status = "warning",
        width = 12,
        column(
          12,
          offset = 0,
          column(6, style = 'padding:0px; text-align:left;',
                 h4(title, style = "font-weight: 800;color: #555;")),
          column(
            6,
            style = 'padding:0px; text-align:right; ',
            conditionalPanel(
              "input.designFieldbook_agrofims == 'CRD' || input.designFieldbook_agrofims == 'RCBD' ||
              input.designFieldbook_agrofims == 'FCRD' || input.designFieldbook_agrofims == 'FRCBD'",
              actionButton(paste0(design, "_closeBox_", index), "", icon("close"))
            )
          )
          ),
        column(
          12,
          fluidRow(
            column(
              6,
              fluidRow(
                column(
                  10,
                  selectizeInput(
                    paste0(design, "_sel_factor_", index),
                    "",
                    multiple = TRUE,
                    options = list(placeholder = "Select...", maxItems =
                                     1),
                    choices = c(dt$fchoices),
                    selected = value
                  ),
                  fluidRow(id = paste0(design, "_sel_factorOth_aux_", index))
                )
              )
            ),
            column(
              6,
              fluidRow(
                column(
                  7,
                  fluidRow(
                    id = paste0(design, "_fl_title_factor_aux_", index)
                  )
                ),
                column(
                  5,
                  hidden(
                    selectInput(
                      paste0(design, "_numLevels_", index),
                      "Number of dates",
                      choices = 2:10,
                      selected = 2
                    )
                  )
                )
              ),           
              fluidRow(id = paste0(design, "_type_input_aux_", index)),
              fluidRow(id = paste0(design, "_levelSelection_aux_", index)),
              fluidRow(id = paste0(design, "_levelSelOther_aux_", index)),
              fluidRow(id = paste0(design, "_note_aux_", index))
            )
          )#,
          # column(
          #   12,
          #   style = "text-align:right",
          #   conditionalPanel(
          #     "input.designFieldbook_agrofims == 'CRD' || input.designFieldbook_agrofims == 'RCBD' ||
          #     input.designFieldbook_agrofims == 'FCRD' || input.designFieldbook_agrofims == 'FRCBD'",
          #     fluidRow(
          #       actionButton(paste0(design, "_btDuplicate_", index), "Duplicate")
          #     )
          #   )
          # )
        )
        )
      )
  }
  
  # Funcion GENERAL que responde a "FACTOR"
  observeEvent(input$selectGEN, {
    vars <- unlist(strsplit(input$selectGENid, "_"))
    design <- vars[1]
    index <- vars[4]
    
    value <-  input[[input$selectGENid]]
    value <- get_dfa_values(dt = dt, choice = value, attribute = "FACTOR")
    
    isolate(updateLevelSelectionGEN(index, value, design))
    
    # CRD: Asigna nomber a columnas
    if (design == "crd") {
      
      if (is.null(value) || value == ""){
        value <- "Factor"
      }
      num_treat <- input$crd_ntrt
      #Limpiamos el inputtext de la columna Treatment
      for (i in 1:num_treat) {
        fUpdateSelect(selID = paste0("input_factor_treatment_crd_",index,"_",i), #CRD
                      in_choices = c("") )
      }
      output[[paste0("title_col_NFF_", design, "_", index)]] <-
        renderText({value})
    }
    
    # RCBD: Asigna nombre a columnas
    if (design == "rcbd") {
      if (is.null(value) || value == "" ){
        value <- "Factor"
      }
      num_treat <- input$rcbd_ntrt
      #Limpiamos el inputtext de la columna Treatment
      for (i in 1:num_treat) {
        fUpdateSelect(selID = paste0("input_factor_treatment_rcbd_",index,"_",i), #RCBD
                      in_choices = c("") )
      }
      output[[paste0("title_col_NFF_", design, "_", index)]] <-
        renderText({value})
    }
    
    # Genera el "OTHER" del FACTOR
    choises <- input[[input$selectGENid]]
    updateSelectOtherGEN(index, choises, design)
    
    # Genera "OTHER/OTHER" de LEVELS
    flevel <- get_dfa_values(dt, choice = input[[paste0(design, "_sel_factor_", index)]], attribute = "LEVEL")
    choices <- strsplit(flevel, split = ";")
    names(choices) <- "Levels"
    removeUI(selector = paste0("#", design, "_type_input_", index), immediate = T)
    
    if (value == "Other") {
      removeUI(selector = paste0("#", design, "_levelSelection_", index))
      # Other level
      insertUI(
        selector = paste0("#", design, "_type_input_aux_", index),
        where = "beforeBegin",
        ui = fluidRow(
          id = paste0(design, "_type_input_", index),
          column(
            12,
            selectizeInput(
              paste0(design, "_typeInput_", index),
              "Type of input",
              multiple = TRUE,
              options = list(placeholder = "Select...", maxItems = 1),
              choices = c(choices)
            )
          )
        )
      )
    }
  })
  
  # Funcion GENERAL que activa "OTHER" de "FACTOR" dependiendo del diseno
  updateSelectOtherGEN <- function(index, choises, design, value = NULL) {
    if (any(choises == "Other") == T) {
      removeUI(
        selector = paste0("#", design, "_sel_factorOth_", index),
        immediate = T
      )
      
      if (any(choises != "")) {
        # Other
        insertUI(
          selector = paste0("#", design, "_sel_factorOth_aux_", index),
          where = "beforeBegin",
          ui = fluidRow(
            id = paste0(design, "_sel_factorOth_", index),
            column(12,
                   textInput(
                     paste0(design, "_sel_factor_other_", index), "", value=" "
                   ))
          )
        )
      }
    } else {
      removeUI(
        selector = paste0("#", design, "_sel_factorOth_", index),
        immediate = T
      )
    }    
  }
  
  # Funcion GENERAL que responde a "LEVELS"
  observeEvent(input$levelsGEN, {
    vars <- unlist(strsplit(input$levelsGENid, "_"))
    design <- vars[1]
    index <- vars[3]
    
    num_levels <-  input[[input$levelsGENid]]
    
    # ocultar en desarrollo la siguiente linea "factores"
    factores <- agdesign::dtfactordesign
    dt <- factores %>% mutate(fchoices = FACTOR)
    
    drawDateComboLevelGEN(
      order = index,
      dt = dt,
      design,
      input_choice = input[[paste0(design, "_sel_factor_", index)]],
      num_levels
    )
    shinyjs::show(id = paste0(design, "_numLevels_", index))
  })
  
  # Funcion GENERAL que activa "LEVELS" dependiendo del diseno
  updateLevelSelectionGEN <- function(index, value, design) {
    # ocultar en desarrollo la siguiente linea "factores"
    factores <- agdesign::dtfactordesign
    dt <- factores %>% mutate(fchoices = FACTOR)
    
    removeUI(selector = paste0("#", design, "_fl_title_factor_", index), immediate = T)
    removeUI(selector = paste0("#", design, "_note_", index), immediate = T)
    removeUI(selector = paste0("#", design, "_levelSelection_", index), immediate = T)
    removeUI(selector = paste0("#", design, "_levelSelOther_", index), immediate = T)
    shinyjs::hide(id = paste0(design, "_numLevels_", index))
    
    num_levels <- input[[paste0(design, "_numLevels_", index)]]
    
    if (value != "") {
      # Title
      insertUI(
        selector = paste0("#", design, "_fl_title_factor_aux_", index),
        where = "beforeBegin",
        ui = fluidRow(id = paste0(design, "_fl_title_factor_", index), column(12, h4(
          HTML(paste0("<b>", value, "</b>"))
        )))
      )
      
      # Note
      insertUI(
        selector = paste0("#", design, "_note_aux_", index),
        where = "beforeBegin",
        ui = fluidRow(
          column(
            12,
            id = paste0(design, "_note_", index),
            textAreaInput(paste0(design, "_note_factor_", index), "Note")
          )
        )
      )
    } else {
      removeUI(selector = paste0("#", design, "_type_input_", index), immediate = T)
    }
    
    if (!is.null(value)) {
      type_form <- get_dfa_values(dt, choice = input[[paste0(design, "_sel_factor_", index)]], attribute = "FORM")
      
      if (!is.null(type_form) && type_form == "combo box") {
        drawComboBoxLevelGEN(
          order = index,
          dt = dt,
          design,
          input_choice = input[[paste0(design, "_sel_factor_", index)]]
        )
        shinyjs::hide(id = paste0(design, "_numLevels_", index))
      } else if (!is.null(type_form) && type_form == "text input") {
        drawTextInputLevelGEN(
          order = index,
          dt = dt,
          design,
          input_choice = input[[paste0(design, "_sel_factor_", index)]],
          "numeric + units"
        )
        shinyjs::hide(id = paste0(design, "_numLevels_", index))
      } else if (!is.null(type_form) && type_form == "date") {
        drawDateComboLevelGEN(
          order = index,
          dt = dt,
          design,
          input_choice = input[[paste0(design, "_sel_factor_", index)]],
          num_levels
        )
        shinyjs::show(id = paste0(design, "_numLevels_", index))
      } else if (!is.null(type_form) && type_form == "other input") {
        if (!is.null(input[[paste0(design, "_typeInput_", index)]]) &&
            input[[paste0(design, "_typeInput_", index)]] == "text") {
          drawTextInputLevelGEN(
            order = index,
            dt = dt,
            design,
            input_choice = input[[paste0(design, "_sel_factor_", index)]],
            "text"
          )
          shinyjs::hide(id = paste0(design, "_numLevels_", index))
        } else if (!is.null(input[[paste0(design, "_typeInput_", index)]]) &&
                   input[[paste0(design, "_typeInput_", index)]] == "numeric") {
          drawTextInputLevelGEN(
            order = index,
            dt = dt,
            design,
            input_choice = input[[paste0(design, "_sel_factor_", index)]],
            "numeric"
          )
          shinyjs::hide(id = paste0(design, "_numLevels_", index))
        } else if (!is.null(input[[paste0(design, "_typeInput_", index)]]) &&
                   input[[paste0(design, "_typeInput_", index)]] == "numeric + units") {
          drawTextInputLevelGEN(
            order = index,
            dt = dt,
            design,
            input_choice = input[[paste0(design, "_sel_factor_", index)]],
            "numeric + units"
          )
          shinyjs::hide(id = paste0(design, "_numLevels_", index))
        } else if (!is.null(input[[paste0(design, "_typeInput_", index)]]) &&
                   input[[paste0(design, "_typeInput_", index)]] == "date") {
          drawDateComboLevelGEN(
            order = index,
            dt = dt,
            design,
            input_choice = input[[paste0(design, "_sel_factor_", index)]],
            num_levels
          )
          shinyjs::show(id = paste0(design, "_numLevels_", index))
        }
      }
    }
  }
  
  # Funcion GENERAL que responde a "OTHER" de "LEVELS"
  observeEvent(input$otherGEN, {
    vars <- unlist(strsplit(input$otherGENid, "_"))
    design <- vars[1]
    index <- vars[3]
    choises <-  input[[input$otherGENid]]
    
    updateLevelSelOtherGEN(index, choises, design)
  })
  
  # Funcion GENERAL que activa "OTHER" de "LEVELS" dependiendo del diseno
  updateLevelSelOtherGEN <- function(index, choises, design) {
    if (any(choises == "Other") == T) {
      removeUI(
        selector = paste0("#", design, "_levelSelOther_", index),
        immediate = T
      )
      
      if (any(choises != "")) {
        # Other
        insertUI(
          selector = paste0("#", design, "_levelSelOther_aux_", index),
          where = "beforeBegin",
          ui = fluidRow(
            id = paste0(design, "_levelSelOther_", index),
            column(
              12,
              selectizeInput(
                paste0(design, "_lvl_other_", index),
                label = "Insert other levels",
                multiple = T,
                choices = c(),
                options = list(
                  maxItems = 20,
                  placeholder = "Write..." ,
                  'create' = TRUE,
                  'persist' = FALSE
                )
              )
            )
          )
        )
      }
    } else {
      removeUI(
        selector = paste0("#", design, "_levelSelOther_", index),
        immediate = T
      )
    }
  }
  
  # Funcion GENERAL que responde a "OTHER/OTHER" de LEVELS
  observeEvent(input$otherOthGEN, {
    vars <- unlist(strsplit(input$otherOthGENid, "_"))
    design <- vars[1]
    index <- vars[3]
    
    value <-  input[[input$selectGENid]]
    value <-
      get_dfa_values(dt = dt,
                     choice = value,
                     attribute = "FACTOR")
    
    updateLevelSelectionGEN(index, value, design)
  })
  
  # Funcion GENERAL que responde a "DUPLICATE"
  observeEvent(input$duplicateGEN, {
    vars <- unlist(strsplit(input$duplicateGENid, "_"))
    design <- vars[1]
    index <- vars[3]
    str_id <- stri_rand_strings(1, 8,  '[A-Z]')
    insertBoxDuplicateGEN(index, str_id, design)
    
    factorIdsAux <- getFactorIds(design)
    id <- match(paste0(design, "_", index), factorIdsAux) #encuentra la posicion del factor de la lista de los IDs
    
    len <- length(factorIdsAux)
    left_side <- factorIdsAux[1:id]
    right_side <- NULL
    
    if (len > 1 && id < len) {
      rg <- id + 1
      right_side <- factorIdsAux[rg:len]
    }
    
    factorIdsAux <- c(left_side, paste0(design, "_", str_id), right_side) #junta todos los casos
    
    updateFactorIdsAfterDuplicate(design, factorIdsAux) #actualiza los FactorIds Reactivos luego de duplicar
  })
  
  #Funcion GENERAL que actualiza FactorIds luego de duplicar
  updateFactorIdsAfterDuplicate <- function(design, factorIdsAux) {
    if (design == "crd") {
      factorCRD$ids <- factorIdsAux
    } else if (design == "rcbd") {
      factorRCBD$ids <- factorIdsAux
    } else if (design == "fcrd") {
      factorFCRD$ids <- factorIdsAux
    } else if (design == "frcbd") {
      factorFRCBD$ids <- factorIdsAux
    } else if (design == "sprcbd") {
      factorSPRCBD$ids <- factorIdsAux
    } else if (design == "spsp") {
      factorSPSP$ids <- factorIdsAux
    }
  }
  
  # Funcion GENERAL que activa "DUPLICATE"
  insertBoxDuplicateGEN <- function(index, str_id, design) {
    val <- input[[paste0(design, "_sel_factor_", index)]]
    value <- get_dfa_values(dt = dt, choice = val, attribute = "FACTOR")
    
    insertUI(
      selector = paste0("#", design, "_full_factor_box_", index),
      where = "afterEnd",
      ui = getDesignUI_GEN(str_id, design, val),
      immediate = T
    )
    
    if (design == "crd" || design == "rcbd") {
      delay(200, isolate(drawNewColumnNFFDuplicate(design,index, paste0(design,"_", str_id))))
    }
    
    oth <- input[[paste0(design, "_sel_factor_other_", index)]]
    
    delay(100, isolate(updateLevelSelectionGEN(str_id, value, design)))
    delay(100, isolate(updateSelectOtherGEN(str_id, val, design, oth)))
    
    if (val == "Other" && !is.null(val)) {
      # Other level
      flevel <- get_dfa_values(dt, choice = input[[paste0(design, "_sel_factor_", index)]], attribute = "LEVEL")
      choices <- strsplit(flevel, split = ";")
      names(choices) <- "Levels"
      
      delay(
        200,
        insertUI(
          selector = paste0("#", design, "_type_input_aux_", str_id),
          where = "beforeBegin",
          ui = fluidRow(
            id = paste0(design, "_type_input_", str_id),
            column(
              12,
              selectizeInput(
                paste0(design, "_typeInput_", str_id),
                "Type of input",
                multiple = TRUE,
                options = list(placeholder = "Select...", maxItems = 1),
                choices = c(choices)
              )
            )
          )
        )
      )
    }
  }
  
  # Funcion GENERAL que activa "Close"
  observeEvent(input$closeBox_button_GEN, {
    vars <- unlist(strsplit(input$closeBox_button_GENid, "_"))
    design <- vars[1]
    index <- vars[3]
    
    # CRD
    if (design == "crd") {
      if (length(factorCRD$ids) > 1) {
        removeUI(
          selector = paste0("#", design, "_full_factor_box_", index),
          immediate = T
        )
        factorCRD$ids <- factorCRD$ids[!factorCRD$ids %in% paste0(design, "_", index)]
        #Removemos la columna de la tabla de CRD
        removeUI(
          selector = paste0("#", "col_NFF_" , design, "_" , index),
          immediate = T
        )
        
        num_treat <- input$crd_ntrt
        #Limpiamos el inputtext de la columna Treatment
        for (i in 1:num_treat) {
          updateSummary("crd",i)
        }
      }
    }
    
    # RCBD
    if (design == "rcbd") {
      if (length(factorRCBD$ids) > 1) {
        removeUI(
          selector = paste0("#", design, "_full_factor_box_", index),
          immediate = T
        )
        factorRCBD$ids <- factorRCBD$ids[!factorRCBD$ids %in% paste0(design, "_", index)]
        #Removemos la columna de la tabla de RCBD
        removeUI(selector = paste0("#", "col_NFF_" , design, "_" , index), immediate = T)
        #CAMBIO----------------------
        num_treat <- input$rcbd_ntrt  
        ##---------------------------
        #Limpiamos el inputtext de la columna Treatment
        for (i in 1:num_treat) {
          updateSummary("rcbd",i)
        }
      }
    }
    
    # FCRD
    if (design == "fcrd") {
      if (length(factorFCRD$ids) > 2) {
        removeUI(
          selector = paste0("#", design, "_full_factor_box_", index),
          immediate = T
        )
        factorFCRD$ids <- factorFCRD$ids[!factorFCRD$ids %in% paste0(design, "_", index)]
      }
    }
    
    # FRCBD
    if (design == "frcbd") {
      if (length(factorFRCBD$ids) > 2) {
        removeUI(
          selector = paste0("#", design, "_full_factor_box_", index),
          immediate = T
        )
        factorFRCBD$ids <- factorFRCBD$ids[!factorFRCBD$ids %in% paste0(design, "_", index)]
      }
    }
    
    # SPRCBD
    if (design == "sprcbd") {
      if (length(factorSPRCBD$ids) > 1) {
        removeUI(
          selector = paste0("#", design, "_full_factor_box_", index),
          immediate = T
        )
        factorSPRCBD$ids <- factorSPRCBD$ids[!factorSPRCBD$ids %in% paste0(design, "_", index)]
      }
    }
    
    # SPSP
    if (design == "spsp") {
      if (length(factorSPSP$ids) > 1) {
        removeUI(
          selector = paste0("#", design, "_full_factor_box_", index),
          immediate = T
        )
        factorSPSP$ids <- factorSPSP$ids[!factorSPSP$ids %in% paste0(design, "_", index)]
      }
    }
  })
  
  #Get design-factor values from design_factor table
  get_dfa_values <- function(dt, choice = "Abiotic stress Abiotic stress End date", attribute = "LEVEL") {
    if (!is.null(choice)) {
      out <- dt %>% filter(fchoices == choice)
      out <- out[, attribute][[1]]
    } else{
      out <-  ""
    }
    out
  }
  
  # Funcion GENERAL que dibuja ComboBox dependiendo del diseno
  drawComboBoxLevelGEN <- function(order, dt, design, input_choice) {
    flevel <- get_dfa_values(dt, choice = input_choice, attribute = "LEVEL")
    choices <- strsplit(flevel, split = ";")
    names(choices) <- "Levels"
    lbl <-
      get_dfa_values(dt, choice = input_choice, attribute = "FACTOR")
    unit <-
      get_dfa_values(dt, choice = input_choice, attribute = "UNIT")
    
    if (is.na(unit)) {
      removeUI(
        selector = paste0("#", design, "_levelSelection_", order),
        immediate = T
      )
      
      insertUI(
        selector = paste0("#", design, "_levelSelection_aux_", order),
        where = "afterEnd",
        ui = fluidRow(
          id = paste0(design, "_levelSelection_", order),
          column(
            12,
            selectizeInput(
              inputId = paste0(design, "_lvl_", order),
              label = "Enter levels",
              multiple = TRUE,
              options = list(placeholder = "Select..."),
              choices = choices
            )
          )
        )
      )
    } else {
      choices_unit <- strsplit(unit, ",")[[1]]
      removeUI(
        selector = paste0("#", design, "_levelSelection_", order),
        immediate = T
      )
      
      insertUI(
        selector = paste0("#", design, "_levelSelection_aux_", order),
        where = "afterEnd",
        ui = fluidRow(
          id = paste0(design, "_levelSelection_", order),
          column(
            6,
            selectizeInput(
              inputId = paste0(design, "_lvl_", order),
              label = "Enter levels",
              multiple = TRUE,
              options = list(placeholder = "Select..."),
              choices = choices
            )
          ),
          column(
            6,
            selectInput(
              inputId = paste0(design, "_lvl_unit_", order),
              label = "Unit",
              choices = c(choices_unit),
              selected = 2
            )
          )
        )
      )
    }
  }
  
  # Funcion GENERAL que dibuja TextInput dependiendo del diseno
  drawTextInputLevelGEN <- function(order, dt,  design, input_choice, type) {
    lbl <- get_dfa_values(dt, choice = input_choice, attribute = "FACTOR")
    unit <- get_dfa_values(dt, choice = input_choice, attribute = "UNIT")
    
    if (input_choice == "Fertilizer product application rate") {
      #List of fertilizers
      flevel <- get_dfa_values(dt, choice = input_choice, attribute = "LEVEL")
      choices <- strsplit(flevel, split = ";")
      names(choices) <- "Levels"
      removeUI(
        selector = paste0("#", design, "_levelSelection_", order),
        immediate = T
      )
      removeUI(
        selector = paste0("#", design, "_fluid_levels_", order),
        immediate = T
      )
      insertUI(
        selector = paste0("#", design, "_levelSelection_aux_", order),
        where = "afterEnd",
        ui = fluidRow(
          id = paste0(design, "_levelSelection_", order),
          column(
            12,
            selectizeInput(
              inputId = paste0(design, "_lvl_fert_", order),
              label = "Enter fertilizer",
              multiple = FALSE,
              options = list(placeholder = "Select..."),
              choices = choices
            ),
            
            selectizeInput(
              paste0(design, "_lvl_", order),
              label = "Enter levels",
              multiple = T,
              choices = c(),
              options = list(
                maxItems = 20,
                placeholder = "Write..." ,
                'create' = TRUE,
                'persist' = FALSE
              )
            )
          )
        )
      )
    } else if (input_choice == "Nutrient element application rate") {
      #List of fertilizers
      flevel <- get_dfa_values(dt, choice = input_choice, attribute = "LEVEL")
      choices <- strsplit(flevel, split = ";")
      names(choices) <- "Levels"
      removeUI(
        selector = paste0("#", design, "_levelSelection_", order),
        immediate = T
      )
      removeUI(
        selector = paste0("#", design, "_fluid_levels_", order),
        immediate = T
      )
      insertUI(
        selector = paste0("#", design, "_levelSelection_aux_", order),
        where = "afterEnd",
        ui = fluidRow(
          id = paste0(design, "_levelSelection_", order),
          column(
            12,
            
            selectizeInput(
              inputId = paste0(design, "_lvl_fert_", order),
              label = "Enter nutrient element",
              multiple = FALSE,
              options = list(placeholder = "Select..."),
              choices = choices
            ),
            
            selectizeInput(
              paste0(design, "_lvl_", order),
              label = "Enter levels",
              multiple = T,
              choices = c(),
              options = list(
                maxItems = 20,
                placeholder = "Write..." ,
                'create' = TRUE,
                'persist' = FALSE
              )
            )
          )
        )
      )
    } else if (input_choice == "Oxidized nutrient application rate") {
      flevel <- get_dfa_values(dt, choice = input_choice, attribute = "LEVEL")
      choices <- strsplit(flevel, split = ";")
      names(choices) <- "Levels"
      removeUI(
        selector = paste0("#", design, "_levelSelection_", order),
        immediate = T
      )
      removeUI(
        selector = paste0("#", design, "_fluid_levels_", order),
        immediate = T
      )
      insertUI(
        selector = paste0("#", design, "_levelSelection_aux_", order),
        where = "afterEnd",
        ui = fluidRow(
          id = paste0(design, "_levelSelection_", order),
          column(
            12,
            textInput(
              inputId = paste0(design, "_lvl_fert_", order),
              label = "Oxidized nutrient",
              value = "",
              placeholder = "enter oxidized nutrient"
            ),
            selectizeInput(
              paste0(design, "_lvl_", order),
              label = "Enter levels",
              multiple = T,
              choices = c(),
              options = list(
                maxItems = 20,
                placeholder = "Write..." ,
                'create' = TRUE,
                'persist' = FALSE
              )
            )
          )
        )
      )
    } else if (type == "numeric + units") {
      if (is.na(unit)) {
        removeUI(
          selector = paste0("#", design, "_levelSelection_", order),
          immediate = T
        )
        insertUI(
          selector = paste0("#", design, "_levelSelection_aux_", order),
          where = "afterEnd",
          ui = fluidRow(
            id = paste0(design, "_levelSelection_", order),
            column(
              12,
              selectizeInput(
                paste0(design, "_lvl_", order),
                label = "Enter levels",
                multiple = T,
                choices = c(),
                options = list(
                  maxItems = 20,
                  placeholder = "Write..." ,
                  'create' = TRUE,
                  'persist' = FALSE
                )
              )
            )
          )
        )
      } else {
        choices_unit <- strsplit(unit, ",")[[1]]
        removeUI(
          selector = paste0("#", design, "_levelSelection_", order),
          immediate = T
        )
        insertUI(
          selector = paste0("#", design, "_levelSelection_aux_", order),
          where = "afterEnd",
          ui = fluidRow(
            id = paste0(design, "_levelSelection_", order),
            column(
              6,
              selectizeInput(
                paste0(design, "_lvl_", order),
                label = "Enter levels",
                multiple = T,
                choices = c(),
                options = list(
                  maxItems = 20,
                  placeholder = "Write..." ,
                  'create' = TRUE,
                  'persist' = FALSE
                )
              )
            ),
            column(
              6,
              selectInput(
                inputId = paste0(design, "_lvl_unit_", order),
                label = "Unit",
                choices = c(choices_unit),
                selected = 2
              )
            )
          )
        )
      }
    } else if (type == "text" || type == "numeric") {
      removeUI(
        selector = paste0("#", design, "_levelSelection_", order),
        immediate = T
      )
      insertUI(
        selector = paste0("#", design, "_levelSelection_aux_", order),
        where = "afterEnd",
        ui = fluidRow(
          id = paste0(design, "_levelSelection_", order),
          column(
            12,
            selectizeInput(
              paste0(design, "_lvl_", order),
              label = "Enter levels",
              multiple = T,
              choices = c(),
              options = list(
                maxItems = 20,
                placeholder = "Write..." ,
                'create' = TRUE,
                'persist' = FALSE
              )
            )
          )
        )
      )
    }
  }
  
  # Funcion GENERAL que dibuja Date dependiendo del diseno
  drawDateComboLevelGEN <- function(order, dt, design, input_choice, num_levels) {
    lbl <- get_dfa_values(dt, choice = input_choice, attribute = "FACTOR")
    removeUI(
      selector = paste0("#", design, "_levelSelection_", order),
      immediate = T
    )
    insertUI(
      selector = paste0("#", design, "_levelSelection_aux_", order),
      where = "afterEnd",
      ui = fluidRow(
        id = paste0(design, "_levelSelection_", order),
        column(
          12,
          fluidRow(
            id = paste0(design, "_factor_dates_", order, "_1"),
            column(
              12,
              airDatepickerInput(
                inputId = paste0(design, "_lvl_", order, "_1", "_dateinput"),
                label = paste0("#1 ", lbl),
                dateFormat = "yyyy-mm-dd",
                value = Sys.Date(),
                placeholder = "yyyy-mm-dd",
                clearButton = TRUE
              )
            )
          )
        )
      )
    )
    
    num_levels <- as.integer(num_levels)
    
    if (num_levels > 1) {
      for (i in 2:num_levels) {
        insertUI(
          selector = paste0("#", design, "_factor_dates_", order, "_", i - 1),
          where = "afterEnd",
          ui = fluidRow(
            id = paste0(design, "_factor_dates_", order, "_", i),
            column(
              12,
              airDatepickerInput(
                inputId = paste0(design, "_lvl_", order, "_", i, "_dateinput"),
                label = paste0("#", i, " ", lbl),
                dateFormat = "yyyy-mm-dd",
                value = Sys.Date(),
                placeholder = "yyyy-mm-dd",
                clearButton = TRUE
              )
            )
          )
        )
      }
    }
  }
  
  ###################### END: Funciones GENERALES CRD/RCBD/FCRD/FRCBD/SPRCBD/SPSP ######################
  
  ###################### START: Funciones Compartidas CRD/RCBD ######################
  
  # Funcion que responde a LVLS de la tabla CRD-RCBD 
  observeEvent(input$levelInput, {
    designFactor <- tolower(designVarsFactor$design)
    if (designFactor == "crd") {
      design <- tolower(input$designFieldbook_agrofims)
      IdDesignInputs <- getFactorIds(design)
      index <<- get_index_design(IdDesignInputs, design)
      allinputs<<-AllInputs()
      flbl<- get_factors_design(allinputs = AllInputs(), index, design = design)
      flvl<- get_levels_design(allinputs = AllInputs(),index=index, data_dictionary=dt,
                               factors = flbl, design=design, format="list")
      fvalues$flbl_crd <- flbl #get_factors_design(allinputs = AllInputs(),  design = design)
      fvalues$flvl_crd <- flvl #get_levels_design(allinputs = AllInputs(), factors = fvalues$flbl, design=design, format="list")
      #fvalues$flbl_crd <- get_factors_design(allinputs = AllInputs(),  design = designFactor)
      #fvalues$flvl_crd <-get_levels_design(allinputs = AllInputs(), factors = fvalues$flbl_crd, design = designFactor, format = "list")
      
      #Eliminar
      print ("######################### START:VALORES VALORES  ##########################")
      
      print(fvalues$flvl_crd)
      
      
      print ("######################### END:VALORES VALORES  ############################")
      
      fill_CRD_RCBD_ValuesInput(designFactor)
      
    } else if (designFactor == "rcbd") {
      design <- tolower(input$designFieldbook_agrofims)
      IdDesignInputs <- getFactorIds(design)
      index <- get_index_design(IdDesignInputs, design)
      allinputs<<-AllInputs()
      flbl<- get_factors_design(allinputs = AllInputs(), index, design = design)
      flvl<- get_levels_design(allinputs = AllInputs(),index=index, data_dictionary=dt,
                               factors = flbl, design=design, format="list")
      fvalues$flbl_rcbd <- flbl #get_factors_design(allinputs = AllInputs(),  design = design)
      fvalues$flvl_rcbd <- flvl #get_levels_design(allinputs = AllInputs(), factors = fvalues$flbl, design=design, format="list")
      #fvalues$flbl_rcbd <- get_factors_design(allinputs = AllInputs(),  design = designFactor)
      #fvalues$flvl_rcbd <-get_levels_design(allinputs = AllInputs(), factors = fvalues$flbl_rcbd, design = designFactor, format = "list")
      
      fill_CRD_RCBD_ValuesInput(designFactor)
    }
  })
  
  # Funcion que responde a los selects de la tabla para CRD-RCBD 
  observeEvent(input$input_factor_treatment, {
    designFactor <- tolower(designVarsFactor$design)
    inputId <- input$levelInputid
    
    #Verificamos que el input no sea nulo
    if(is.null(inputId))
      return()
    
    vars <- unlist(strsplit(inputId, ","))
    inputValue <- input[[input$levelInputid]]
    
    if (designFactor == "crd" || designFactor == "rcbd") {
      num_treat <- input[[paste0(designFactor,"_ntrt")]]
      if (is.null(num_treat)) {
        if (designFactor == "crd") {
          num_treat <- designVarsCRD$DEFAULT_TREAT_NFULL
        } else if(designFactor == "rcbd") {
          num_treat <- designVarsRCBD$DEFAULT_TREAT_NFULL
        }
      } else {
        num_treat <- as.integer(num_treat)
      }
      if (num_treat < 1) {
        return()
      }
      for (i in 1:num_treat) {
        updateSummary(designFactor,i)
      }
    }
  })
  
  # Funcion general que responde a input Other para CRD-RCBD 
  observeEvent(input$crd_rcbd_SelFactorOther, {
    designFactor <- tolower(designVarsFactor$design)
    
    value <- input[[input$crd_rcbd_SelFactorOtherid]]
    id <- input$crd_rcbd_SelFactorOtherid
    
    vals <- unlist(strsplit(id, "_"))
    index <- vals[5]
    
    selFactorValue <- input[[paste0(designFactor,"_sel_factor_",index)]]
    
    if (designFactor == "crd") {
      if (selFactorValue == "Other"){
        output[[paste0("title_col_NFF_", designFactor, "_", index)]] <- renderText({value})
      }
      if(str_trim(value) == "")
      {
        output[[paste0("title_col_NFF_", designFactor, "_", index)]] <- renderText({"Other"})
      }
    } else if (designFactor == "rcbd") {
      if (selFactorValue == "Other") {
        output[[paste0("title_col_NFF_", designFactor, "_", index)]] <- renderText({value})
      }
      if(str_trim(value) == "")
      {
        output[[paste0("title_col_NFF_", designFactor, "_", index)]] <- renderText({"Other"})
      }
    }
  })
  
  # Funcion que responde al numero de tratamientos para CRD
  observeEvent(input$crd_ntrt, {
    designVarsCRD$ids_NFULL <- getFactorIds("crd")
    designFactor <- designVarsFactor$design
    
    ids <- designVarsCRD$ids_NFULL
    rep <- as.numeric(input$crd_ntrt) #CRD
    
    if (factorCRD$currNumReplications > rep  && !is.na(rep)) {
      start <- rep + 1
      for (i in factorCRD$currNumReplications:start) {
        deleteSummaryEntry(designFactor,i)
      }
    } else if (factorCRD$currNumReplications < rep && !is.na(rep)) {
      start  <- factorCRD$currNumReplications + 1
      for (i in start:rep) {
        insertSummaryEntry(i,designFactor)
        updateSummary(designFactor,i)
      }
    }
    for (id in ids) {
      vars <- id
      if (factorCRD$currNumReplications > rep  && !is.na(rep)) {
        start <- rep + 1
        for (i in factorCRD$currNumReplications:start) {
          removeUI(selector = paste0("#aux_col_NFF_", vars, "_", i),
                   immediate = T)
        }
      } else if (factorCRD$currNumReplications < rep && !is.na(rep)) {
        start  <- factorCRD$currNumReplications + 1
        for (i in start:rep) {
          insertUI(
            selector = paste0("#fr_col_NFF_", vars),
            where = "beforeBegin",
            ui = column(
              id = paste0("aux_col_NFF_", vars, "_", i),
              width = 12,
              uiOutput(paste0("ui_col_NFF_", vars, "_", i))
            )
          )
          drawInputNFF(designFactor,vars, i)
        }
      }
    }
    
    factorCRD$currNumReplications <- rep
    updateSummaryAll(designFactor,rep)
  })
  
  # Funcion que responde al numero de tratamientos para RCBD 
  observeEvent(input$rcbd_ntrt, {
    designVarsRCBD$ids_NFULL <- getFactorIds("rcbd")
    designFactor <- designVarsFactor$design
    
    ids <- designVarsRCBD$ids_NFULL
    rep <- as.numeric(input$rcbd_ntrt) #RCBD
    
    if (factorRCBD$currNumReplications > rep  && !is.na(rep)) {
      start <- rep + 1
      for (i in factorRCBD$currNumReplications:start) {
        deleteSummaryEntry(designFactor,i)
      }
    } else if (factorRCBD$currNumReplications < rep && !is.na(rep)) {
      start  <- factorRCBD$currNumReplications + 1
      for (i in start:rep) {
        insertSummaryEntry(i,designFactor)
        updateSummary(designFactor,i)
      }
    }
    
    for (id in ids) {
      vars <- id
      if (factorRCBD$currNumReplications > rep  && !is.na(rep)) {
        start <- rep + 1
        for (i in factorRCBD$currNumReplications:start) {
          removeUI(selector = paste0("#aux_col_NFF_", vars, "_", i),
                   immediate = T)
        }
      } else if (factorRCBD$currNumReplications < rep && !is.na(rep)) {
        start  <- factorRCBD$currNumReplications + 1
        for (i in start:rep) {
          insertUI(
            selector = paste0("#fr_col_NFF_", vars),
            where = "beforeBegin",
            ui = column(
              id = paste0("aux_col_NFF_", vars, "_", i),
              width = 12,
              uiOutput(paste0("ui_col_NFF_", vars, "_", i))
            )
          )
          
          drawInputNFF(designFactor,vars, i)
        }
      }
    }
    
    factorRCBD$currNumReplications <- rep
    updateSummaryAll(designFactor,rep)
  })
  
  # Funcion que dibujar y carga valores en los inputs de la tabla CRD-RCBD
  drawInputNFF <- function(designFactor,index, order) {
    ids <- getFactorIds(designFactor) #getFactorIds
    if(designFactor == "crd") {
      output[[paste0("ui_col_NFF_", index, "_", order)]] <-
        renderUI(
          selectizeInput(
            paste0("input_factor_treatment_", index, "_", order),
            label = "",
            multiple = TRUE,
            options =  list(maxItems = 1, placeholder = "Select one..."),
            choices = fvalues$flvl_crd[[which(ids == index, arr.ind = TRUE)]] #CRD
          )
        )
    } else if (designFactor == "rcbd") {
      output[[paste0("ui_col_NFF_", index, "_", order)]] <-
        renderUI(
          selectizeInput(
            paste0("input_factor_treatment_", index, "_", order),
            label = "",
            multiple = TRUE,
            options =  list(maxItems = 1, placeholder = "Select one..."),
            choices = fvalues$flvl_rcbd[[which(ids == index, arr.ind = TRUE)]] #RCBD
          )
        )
    }
  }
  
  
  # Funcion que dibujar los inputs de la tabla cuando se hace click en Add Factor CRD-RCBD
  drawInputAddFactorNFF <- function(designFactor,index, order) {
    ids <- getFactorIds(designFactor) #getFactorIds
    if(designFactor == "crd") {
      output[[paste0("ui_col_NFF_", index, "_", order)]] <-
        renderUI(
          selectizeInput(
            paste0("input_factor_treatment_", index, "_", order),
            label = "",
            multiple = TRUE,
            options =  list(maxItems = 1, placeholder = "Select one..."),
            choices = c() #CRD
          )
        )
      return()
    } else if (designFactor == "rcbd") {
      output[[paste0("ui_col_NFF_", index, "_", order)]] <-
        renderUI(
          selectizeInput(
            paste0("input_factor_treatment_", index, "_", order),
            label = "",
            multiple = TRUE,
            options =  list(maxItems = 1, placeholder = "Select one..."),
            choices = c() #RCBD
          )
        )
    }
  }
  
  
  
  # Funcion que elimina inputs treatment summary para CRD-RCBD
  deleteSummaryEntry <- function(designFactor,treat_num) {
    removeUI(selector = paste0("#row_NFF_summ_",designFactor,"_", treat_num),
             immediate = T)
  }
  
  # Funcion que dibuja inputs treatment summary para CRD-RCBD
  insertSummaryEntry <- function(treat_num,designFactor) {
    if(designFactor == "crd") {
      cn <- length(designVarsCRD$ids_NFULL)
    } else if(designFactor=="rcbd") {
      cn <- length(designVarsRCBD$ids_NFULL)
    }
    
    if (!is.null(cn) && cn > 0) {
      repl <- rep("-", cn)
      xvalue <- paste(repl, collapse = " / ")
    } else {
      xvalue <- ''
    }
    
    insertUI(
      selector = paste0("#fr_col_NFF_cons_",designFactor),
      where = "beforeBegin",
      ui = column(
        12,
        id = paste0("row_NFF_summ_",designFactor,"_", treat_num),
        HTML('<center>'),
        disabled(textInput(
          paste0("ui_NFF_summ_",designFactor,"_", treat_num), "" , value = xvalue
        )),
        HTML('</center>')
      )
    )
  }
  
  # Funcion que dibuja columnas en la tabla para CRD-RCBD
  drawNewColumnNFF <- function(index,designFactor) {
    if (designFactor == "crd") {
      if (factorCRD$numRepAux == 0) {
        for (i in 1:factorCRD$currNumReplications) {
          insertSummaryEntry(i,designFactor)
        }
        factorCRD$numRepAux <- 1
      }
    } else if (designFactor == "rcbd") {
      if (factorRCBD$numRepAux == 0) {
        for (i in 1:factorRCBD$currNumReplications) {
          insertSummaryEntry(i,designFactor)
        }
        factorRCBD$numRepAux <- 1
      }
    }
    insertUI(
      selector = paste0("#not_full_factor_table_",designFactor),
      where = "beforeBegin",
      ui = column(
        id = paste0("col_NFF_", index),
        width = 2,
        HTML("<center>"),
        uiOutput(paste0("title_col_NFF_", index)),
        HTML("</center>"),
        fluidRow(id = paste0("fr_col_NFF_", index))
      )
    )
    
    vals <- unlist(strsplit(index, "_"))
    value <- input[[paste0(designFactor,"_sel_factor_", vals[2])]]
    
    if (is.null(value))
      value <- "Factor"
    output[[paste0("title_col_NFF_", index)]] <- renderText({
      value
    })
    
    num_treat <- input[[paste0(designFactor,"_ntrt")]]
    
    if (is.null(num_treat)) {
      if (designFactor == "crd") {
        num_treat <- designVarsCRD$DEFAULT_TREAT_NFULL #CRD
      } else if (designFactor=="rcbd") {
        num_treat <- designVarsRCBD$DEFAULT_TREAT_NFULL #RCBD
      }
    } else {
      num_treat <- as.integer(num_treat)
    }
    if (num_treat < 1)
      return()
    
    for (i in 1:num_treat) {
      insertUI(
        selector = paste0("#fr_col_NFF_", index),
        where = "beforeBegin",
        ui = column(
          id = paste0("aux_col_NFF_", index, "_", i),
          width = 12,
          uiOutput(paste0("ui_col_NFF_", index, "_", i))
        )
      )
      
      drawInputAddFactorNFF(designFactor,index, i)
      updateSummary(designFactor,i)
      
    }
  }
  
  # Funcion duplicate que dibuja columnas en la tabla para CRD-RCBD
  drawNewColumnNFFDuplicate <- function(designFactor,prev, index) {
    insertUI(
      selector = paste0("#col_NFF_",designFactor,"_", prev),
      where = "afterEnd",
      ui = column(
        id = paste0("col_NFF_", index),
        width = 2,
        HTML("<center>"),
        uiOutput(paste0("title_col_NFF_", index)),
        HTML("</center>"),
        fluidRow(id = paste0("fr_col_NFF_", index))
      )
    )
    
    value <- NULL
    
    if (is.null(value))
      value <- "Factor"
    
    output[[paste0("title_col_NFF_", index)]] <- renderText({
      value
    })
    
    if (designFactor == "crd") {
      num_treat <- input$crd_ntrt
    } else if(designFactor == "rcbd") {
      num_treat <- input$rcbd_ntrt
    }
    
    if (is.null(num_treat)) {
      if (designFactor == "crd") {
        num_treat <- designVarsCRD$DEFAULT_TREAT_NFULL #CRD
      } else if (designFactor == "rcbd") {
        num_treat <- designVarsRCBD$DEFAULT_TREAT_NFULL #RCBD
      }
    }
    else {
      num_treat <- as.integer(num_treat)
    }
    if (num_treat < 1)
      return()
    
    for (i in 1:num_treat) {
      insertUI(
        selector = paste0("#fr_col_NFF_", index),
        where = "beforeBegin",
        ui = column(
          id = paste0("aux_col_NFF_", index, "_", i),
          width = 12,
          uiOutput(paste0("ui_col_NFF_", index, "_", i))
        )
      )
      
      isolate(drawInputNFF(designFactor,index, i))
      updateSummary(designFactor,i)
    }
  }
  
  # Funcion que actualiza el input summary de la tabla para CRD-RCBD
  updateSummary <- function(designFactor,treat_index) {
    ids <- getFactorIds(designFactor)
    l <- c()
    
    for (id in ids) {
      vals <- unlist(strsplit(id, "_"))
      val <- input[[paste0("input_factor_treatment_",vals[1],"_",vals[2],"_",treat_index)]]
      
      if (typeof(val) == 'double') {
        val = as.character(val)
      }
      
      if (is.null(val) || val == '')
        val <- '-'
      l <- c(l, val)
    }
    
    updateTextInput(
      session,
      inputId = paste0("ui_NFF_summ_",designFactor,"_", treat_index),
      value = paste(l, collapse = " / ")
    )
  }
  
  # Funcion que actualiza los input summary de la tabla para CRD-RCBD
  updateSummaryAll <- function(designFactor,num = NULL) {
    if (is.null(num)) {
      num_treat <- input[[paste0(designFactor,"_ntrt")]]
      if (is.null(num_treat))
        if (designFactor == "crd" ) {
          num_treat <- designVarsCRD$DEFAULT_TREAT_NFULL #CRD
          
        } else if (designFactor == "rcbd"){
          num_treat <- designVarsRCBD$DEFAULT_TREAT_NFULL #RCBD
        }
    } else {
      num_treat <-  num
    }
    
    for (i in 1:num_treat) {
      updateSummary(designFactor,i)
    }
  }
  
  # Funcion que actualiza el valor de los selectbox de la tabla para CRD-RCBD
  fUpdateSelect <- function(selID,in_choices = NULL,value = NULL) {
    updateSelectInput(session, selID, choices = in_choices, selected = value)
  }
  
  # Funcion que recorre los select box y los actualiza de la tabla para CRD-RCBD
  fill_CRD_RCBD_ValuesInput <- function(designFactor) {
    ids <- getFactorIds(designFactor)
    rep <- input[[paste0(designFactor,"_ntrt")]]
    
    for (id in ids) {
      vars <- id
      for (i in 1:rep) {
        idInput <- paste0("input_factor_treatment_", vars, "_", i)
        inputValue <- input[[paste0("input_factor_treatment_", vars, "_", i)]]
        
        if(designFactor=="crd") {
          fUpdateSelect(selID = idInput,
                        in_choices = fvalues$flvl_crd[[which(ids == id, arr.ind = TRUE)]], inputValue)
          
          
        } else if (designFactor == "rcbd") {
          fUpdateSelect(selID = idInput,
                        in_choices = fvalues$flvl_rcbd[[which(ids == id, arr.ind = TRUE)]], inputValue)
          
        }
      }
    }
  }
  
  ###################### END: Funciones Compartidas CRD/RCBD ######################
  
  ############################### END SERVER: DESIGN ###############################
  ##################################################################################
  
  ##################################################################################################
  ############################### START SERVER: MANAGEMENT PRACTICES ###############################
  
  ###################### START: GENERAL ######################
  
  nutTabs = list("Residue management" = "tabResidue",
                 "Seedbed preparation" = "tabSeedbed",
                 "Soil fertility" = "tabSoil",
                 "Planting and transplanting" = "tabPlanting",
                 "Mulch management" ="tabMulching",
                 "Irrigation" = "tabIrrigation",
                 "Weeding" = "tabWeeding",
                 "Harvest" = "tabHarvest")
  
  observe({
    hideTab("nutrienTabPanels", "tabResidue")
    hideTab("nutrienTabPanels", "tabSeedbed")
    hideTab("nutrienTabPanels", "tabSoil")
    hideTab("nutrienTabPanels", "tabPlanting")
    hideTab("nutrienTabPanels", "tabMulching")
    hideTab("nutrienTabPanels", "tabIrrigation")
    hideTab("nutrienTabPanels", "tabWeeding")
    hideTab("nutrienTabPanels", "tabHarvest")
    
    if(!is.null(input$selectAgroFeature)){
      l <- input$selectAgroFeature
      
      for (i in l) {
        showTab("nutrienTabPanels", nutTabs[[i]])
      }
    }
  })
  
  ###################### END: GENERAL ######################
  
  ###################### START: RESIDUE MANAGEMENT ######################
  
  ## Residue management
  # residue_start_date 
  output$res_start_date <- renderUI({
    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
      airDatepickerInput("rmgt_residue_start_date",
                         "Start date",
                         clearButton = T,
                         autoClose = T,
                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                         placeholder = "yyyy-mm-dd",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("rmgt_residue_start_date",
                         "Start date",
                         clearButton = T,
                         autoClose = T,
                         placeholder = "yyyy-mm-dd"
      )
    }
  })
  
  ###################### END: RESIDUE MANAGEMENT ######################
  
  ###################### START: LAND LEVELLING ######################
  
  ## land preparation
  # landLeveling_start_date 
  output$landLev_start_date <- renderUI({
    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
      airDatepickerInput("landLeveling_start_date",
                         "Start date",
                         clearButton = T,
                         autoClose = T,
                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                         placeholder = "yyyy-mm-dd",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("landLeveling_start_date",
                         "Start date",
                         clearButton = T,
                         autoClose = T,
                         placeholder = "yyyy-mm-dd"
      )
    }
  })
  
  # puddling_start_date 
  output$pud_start_date <- renderUI({
    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
      airDatepickerInput("puddling_start_date",
                         "Start date",
                         clearButton = T,
                         autoClose = T,
                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                         placeholder = "yyyy-mm-dd",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("puddling_start_date",
                         "Start date",
                         clearButton = T,
                         autoClose = T,
                         placeholder = "yyyy-mm-dd"
      )
    }
  })
  
  # tillage_start_date 
  output$till_start_date <- renderUI({
    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
      airDatepickerInput("tillage_start_date",
                         "Start date",
                         clearButton = T,
                         autoClose = T,
                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                         placeholder = "yyyy-mm-dd",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("tillage_start_date",
                         "Start date",
                         clearButton = T,
                         autoClose = T,
                         placeholder = "yyyy-mm-dd"
      )
    }
  })
  
  ###################### END: LAND LEVELLING ######################
  
  ###################### START: PLANTING & TRANSPLANTING ######################
  
  # Oculta por defecto los tabs de inicio de Int, Rel & Rot
  observe({
    shiny::hideTab(inputId = "tabpanelPTint", target = "pt_int_default")
    shiny::hideTab(inputId = "tabpanelPTrel", target = "pt_rel_default")
    shiny::hideTab(inputId = "tabpanelPTrot", target = "pt_rot_default")
  })
  
  # Inserta los tabs en Exp Cond dependiendo del tipo de cultivo
  observeEvent(input$PTBoxInterVar, {
    
    id <- input$PTBoxInterVarid
    vars <- unlist(strsplit(id, "_"))
    cropType <- vars[1]
    index <- vars[3]
    
    vals <- getValuesCrop(cropType)
    
    insertTabsPT(vals, cropType)
    
    #Actualiza el nombre de los tabs según crop
    output[[paste0("title_panel_",cropType,"_pt_",index)]] = renderText({
      input[[id]]
    })
  })
  
  # Funcion que extrae lista de nombres que ira en los tabs de exp cond
  getValuesCrop <- function(cropType) {
    if (cropType == "int") { ids <- intercropVars$ids }
    else if (cropType == "rel") { ids <- relaycropVars$ids }
    else if (cropType == "rot") { ids <- rotationcropVars$ids }
    
    
    newids <- inputid <- c()
    
    for (i in 1:length(ids)) {
      vars <- unlist(strsplit(ids[i], "_"))
      newids[i] <- vars[2]
    }
    
    for (i in 1:length(newids)) {
      inputid[i] <- paste0(cropType, "_cropCommonName_", newids[i])
    }
    
    df <- data.frame(id = c(inputid), values = "", stringsAsFactors = F)
    
    
    val <- AllInputs() %>% dplyr::filter(id %in% df$id)
    val <- c(val$values)
    
    i<-1
    for (id in newids){
      if(val[i] != ""){
        val[i] <- paste0(cropType, "_pt_", id)
        
      }
      i<-i+1
    }
    
    val <- val[val != ""]
    val
    
  }
  
  expconPTinter <- reactiveValues()
  expconPTinter$var <- c()
  
  # Funcion que inserta los tabs dependiendo del tipo de cultivo
  insertTabsPT <- function(vals, cropType) {
    
    if (length(vals) != 0) {
      xx <- expconPTinter$var[!expconPTinter$var%in%vals]
      vals <- vals[!vals %in% unique(expconPTinter$var)]
      expconPTinter$var <- c(expconPTinter$var, vals)
      
      if (!is.null(xx)) {
        for (i in 1:length(xx)) {
          removeTab(inputId = paste0("tabpanelPT", cropType), target = xx[i])
          expconPTinter$var <- expconPTinter$var[!expconPTinter$var %in% xx]
        }
      }
      
      if (length(vals) >= 1) {
        
        for (i in 1:length(vals)) {
          
          insertTab(
            inputId = paste0("tabpanelPT", cropType),
            tabPanel(
              title = uiOutput(paste0("title_panel_",vals[i])),
              value = vals[i],
              br(),
              fluidRow(id = paste0(vals[i], "_fr_plantingTransplating")),
              # actionButton(paste0(vals[i], "_pt_add"), "Add Planting & Transplanting"),
              insertRow_PT(crop = vals[i], 1)
            ),
            target = paste0("pt_", cropType, "_default"),
            position = "before",
            select = T
          )
        }
      }
      
      
      
    }
  }
  
  ## Planting & Transplanting: Asigna variables reactivas
  # monocrop
  expconPTmonocrop <- reactiveValues()
  expconPTmonocrop$num <- 0
  expconPTmonocrop$DEFAULT <- 1
  # inter PT crop 1
  expconIntPTcrop1 <- reactiveValues()
  expconIntPTcrop1$num <- 0
  expconIntPTcrop1$DEFAULT <- 1
  # inter PT crop 2
  expconIntPTcrop2 <- reactiveValues()
  expconIntPTcrop2$num <- 0
  expconIntPTcrop2$DEFAULT <- 1
  # inter PT crop 3
  expconIntPTcrop3 <- reactiveValues()
  expconIntPTcrop3$num <- 0
  expconIntPTcrop3$DEFAULT <- 1
  # inter PT crop 4
  expconIntPTcrop4 <- reactiveValues()
  expconIntPTcrop4$num <- 0
  expconIntPTcrop4$DEFAULT <- 1
  # inter PT crop 5
  expconIntPTcrop5 <- reactiveValues()
  expconIntPTcrop5$num <- 0
  expconIntPTcrop5$DEFAULT <- 1
  # relay PT crop 1
  expconRelPTcrop1 <- reactiveValues()
  expconRelPTcrop1$num <- 0
  expconRelPTcrop1$DEFAULT <- 1
  # relay PT crop 2
  expconRelPTcrop2 <- reactiveValues()
  expconRelPTcrop2$num <- 0
  expconRelPTcrop2$DEFAULT <- 1
  # relay PT crop 3
  expconRelPTcrop3 <- reactiveValues()
  expconRelPTcrop3$num <- 0
  expconRelPTcrop3$DEFAULT <- 1
  # relay PT crop 4
  expconRelPTcrop4 <- reactiveValues()
  expconRelPTcrop4$num <- 0
  expconRelPTcrop4$DEFAULT <- 1
  # relay PT crop 5
  expconRelPTcrop5 <- reactiveValues()
  expconRelPTcrop5$num <- 0
  expconRelPTcrop5$DEFAULT <- 1
  
  # Planting & Transplanting: Inserta por defecto un row en monocrop
  observe({
    if (!is.null(input$selectAgroFeature)) {
      # monocrop
      if (expconPTmonocrop$num == 0) {
        defaultPTmonocrop <- expconPTmonocrop$DEFAULT
        
        for (i in 1:defaultPTmonocrop) {
          insertRow_PT(crop = "monocrop", i)
        }
      }
    }
  })
  
  # Planting & Transplanting: Agrega un row al hacer clic en el boton "Add Planting & Transplanting"
  observeEvent(input$PTBoxVar, {
    vars <- unlist(strsplit(input$PTBoxVarid, "_"))
    crop <- paste0(vars[1], "_", vars[2], "_", vars[3])
    
    if (expconPTmonocrop$num >= 1 && crop != "monocrop") { insertRow_PT(crop = "monocrop", expconPTmonocrop$num + 1) }
    if (expconIntPTcrop1$num >= 1 && crop == "int_pt_1") { insertRow_PT(crop = "int_pt_1", expconIntPTcrop1$num + 1) }
    if (expconIntPTcrop2$num >= 1 && crop == "int_pt_2") { insertRow_PT(crop = "int_pt_2", expconIntPTcrop2$num + 1) }
    if (expconIntPTcrop3$num >= 1 && crop == "int_pt_3") { insertRow_PT(crop = "int_pt_3", expconIntPTcrop3$num + 1) }
    if (expconIntPTcrop4$num >= 1 && crop == "int_pt_4") { insertRow_PT(crop = "int_pt_4", expconIntPTcrop4$num + 1) }
    if (expconIntPTcrop5$num >= 1 && crop == "int_pt_5") { insertRow_PT(crop = "int_pt_5", expconIntPTcrop5$num + 1) }
    if (expconRelPTcrop1$num >= 1 && crop == "rel_pt_1") { insertRow_PT(crop = "rel_pt_1", expconRelPTcrop1$num + 1) }
    if (expconRelPTcrop2$num >= 1 && crop == "rel_pt_2") { insertRow_PT(crop = "rel_pt_2", expconRelPTcrop2$num + 1) }
    if (expconRelPTcrop3$num >= 1 && crop == "rel_pt_3") { insertRow_PT(crop = "rel_pt_3", expconRelPTcrop3$num + 1) }
    if (expconRelPTcrop4$num >= 1 && crop == "rel_pt_4") { insertRow_PT(crop = "rel_pt_4", expconRelPTcrop4$num + 1) }
    if (expconRelPTcrop5$num >= 1 && crop == "rel_pt_5") { insertRow_PT(crop = "rel_pt_5", expconRelPTcrop5$num + 1) }
  })
  
  insertRow_PT <- function(crop, index) {
    # monocrop
    if (crop == "monocrop") {
      insertUI(
        selector = "#monocrop_fr_plantingTransplating",
        where = "beforeBegin",
        ui = getPTUI_GEN(crop, index)
      )
      expconPTmonocrop$num <- expconPTmonocrop$num + 1
    }
    # inter PT crop 1
    if (crop == "int_pt_1") {
      insertUI(
        selector = "#int_pt_1_fr_plantingTransplating",
        where = "beforeBegin",
        ui = getPTUI_GEN(crop, index)
      )
      expconIntPTcrop1$num <- expconIntPTcrop1$num + 1
    }
    # inter PT crop 2
    if (crop == "int_pt_2") {
      insertUI(
        selector = "#int_pt_2_fr_plantingTransplating",
        where = "beforeBegin",
        ui = getPTUI_GEN(crop, index)
      )
      expconIntPTcrop2$num <- expconIntPTcrop2$num + 1
    }
    # inter PT crop 3
    if (crop == "int_pt_3") {
      insertUI(
        selector = "#int_pt_3_fr_plantingTransplating",
        where = "beforeBegin",
        ui = getPTUI_GEN(crop, index)
      )
      expconIntPTcrop3$num <- expconIntPTcrop3$num + 1
    }
    # inter PT crop 4
    if (crop == "int_pt_4") {
      insertUI(
        selector = "#int_pt_4_fr_plantingTransplating",
        where = "beforeBegin",
        ui = getPTUI_GEN(crop, index)
      )
      expconIntPTcrop4$num <- expconIntPTcrop4$num + 1
    }
    # inter PT crop 5
    if (crop == "int_pt_5") {
      insertUI(
        selector = "#int_pt_5_fr_plantingTransplating",
        where = "beforeBegin",
        ui = getPTUI_GEN(crop, index)
      )
      expconIntPTcrop5$num <- expconIntPTcrop5$num + 1
    }
    # relay PT crop 1
    if (crop == "rel_pt_1") {
      insertUI(
        selector = "#rel_pt_1_fr_plantingTransplating",
        where = "beforeBegin",
        ui = getPTUI_GEN(crop, index)
      )
      expconRelPTcrop1$num <- expconRelPTcrop1$num + 1
    }
    # relay PT crop 2
    if (crop == "rel_pt_2") {
      insertUI(
        selector = "#rel_pt_2_fr_plantingTransplating",
        where = "beforeBegin",
        ui = getPTUI_GEN(crop, index)
      )
      expconRelPTcrop2$num <- expconRelPTcrop2$num + 1
    }
    # relay PT crop 3
    if (crop == "rel_pt_3") {
      insertUI(
        selector = "#rel_pt_3_fr_plantingTransplating",
        where = "beforeBegin",
        ui = getPTUI_GEN(crop, index)
      )
      expconRelPTcrop3$num <- expconRelPTcrop3$num + 1
    }
    # relay PT crop 4
    if (crop == "rel_pt_4") {
      insertUI(
        selector = "#rel_pt_4_fr_plantingTransplating",
        where = "beforeBegin",
        ui = getPTUI_GEN(crop, index)
      )
      expconRelPTcrop4$num <- expconRelPTcrop4$num + 1
    }
    # relay PT crop 5
    if (crop == "rel_pt_5") {
      insertUI(
        selector = "#rel_pt_5_fr_plantingTransplating",
        where = "beforeBegin",
        ui = getPTUI_GEN(crop, index)
      )
      expconRelPTcrop5$num <- expconRelPTcrop5$num + 1
    }
  }
  
  getPTUI_GEN <- function(crop, index) {
    fluidRow(
      id = paste0(crop, "_fr_plantingTrasplanting_", index),
      box(
        id = paste0(crop, "_plantingTransplanting_boxid_", index),
        title = actionLink(paste0(crop, "_plantingTransplanting_titleId_", index), "Planting & Transplanting details:"),
        status = "primary", solidHeader = TRUE,
        width = 12, collapsible = TRUE,  collapsed = FALSE,
        fluidRow(
          box(
            id = paste0(crop, "_direct_seeding_boxid_", index),
            title = checkboxInput(paste0(crop, "_directSeeding_checkbox_", index), actionLink(paste0(crop, "_direct_seeding_titleId_", index), "Direct seeding"), F),
            status = "primary",
            solidHeader = TRUE,
            width = 12, collapsible = TRUE,  collapsed = TRUE,
            fluidRow(
              column(
                6,
                fluidRow(
                  column(
                    6,
                    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                      airDatepickerInput(paste0(crop, "_ptdi_planting_start_date_", index),
                                         "Start date",
                                         clearButton = T,
                                         autoClose = T,
                                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                                         placeholder = "yyyy-mm-dd",
                                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                      )
                    } else {
                      airDatepickerInput(paste0(crop, "_ptdi_planting_start_date_", index),
                                         "Start date",
                                         clearButton = T,
                                         autoClose = T,
                                         placeholder = "yyyy-mm-dd"
                      )
                    }
                  )
                )
              )
            ),
            fluidRow(
              column(
                6,
                fluidRow(
                  box(
                    title = "Planting, transplanting method", solidHeader = TRUE, status = "warning", width=12,
                    fluidRow(
                      column(12, h4("Planting, transplanting method", style="font-weight: 800;color: #555;"))
                    ),
                    selectizeInput(
                      paste0(crop, "_ptdi_seeding_environment_", index), label = "Seeding environment", 
                      multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                      choices = c("Flat seed bed",
                                  "Hill",
                                  "Ridge", 
                                  "Other")
                    ),
                    hidden(textInput(paste0(crop, "_ptdi_seeding_environment_", index, "_other"), "", value="")),
                    selectizeInput(
                      paste0(crop, "_ptdi_seeding_technique_", index), label = "Seeding technique", 
                      multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                      choices = c("Broadcasting",
                                  "Line sowing",
                                  "Dibbling")
                    ),
                    hidden(textInput(paste0(crop, "_ptdi_seeding_technique_", index,"_other"), "", value="")),
                    textInput(paste0(crop, "_ptdi_seed_treatment_", index), value="", label = "Seed treatment")
                  )
                ),
                fluidRow(
                  box(
                    title = "Implement", solidHeader = TRUE, status = "warning", width = 12,
                    fluidRow(
                      column(12, h4("Implement", style="font-weight: 800;color: #555;"))
                    ),
                    selectizeInput(
                      paste0(crop, "_ptdi_seeding_implement_type_", index), label = "Type", 
                      multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                      choices = c("Bucket broadcaster",
                                  "Dibbling stick",
                                  "Drum seeder",
                                  "Jab planter",
                                  "Seed drill",
                                  "Other")
                    ),
                    hidden(textInput(paste0(crop, "_ptdi_seeding_implement_type_", index, "_other"), "", value="")),
                    selectizeInput(
                      paste0(crop, "_ptdi_seeding_traction_", index), label = "Traction", 
                      multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                      choices = c("Animal",
                                  "Manual",
                                  "2 wheel tractor",
                                  "4 wheel tractor",
                                  "Other")
                    ),
                    hidden(textInput(paste0(crop, "_ptdi_seeding_traction_", index, "_other"), "", value=""))
                  )
                )
              ),
              column(
                6,
                fluidRow(
                  box(
                    title = "Seeding density", solidHeader = TRUE, status = "warning", width = 12,
                    fluidRow(
                      column(12, h4("Seeding density", style="font-weight: 800;color: #555;"))
                    ),
                    fluidRow(
                      column(6, numericInput(paste0(crop, "_ptdi_distance_rows_", index),  label = "Distance between rows", min = 0, max = 100, step = 0.1,value = NULL)),
                      column(
                        6,
                        selectizeInput(
                          paste0(crop, "_ptdi_distance_rows_unit_", index), label = "Unit", 
                          multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                          choices = c("cm",
                                      "ft",
                                      "in",
                                      "m"),
                          selected = "cm"
                        )
                      )
                    ),
                    fluidRow(
                      column(6, numericInput(paste0(crop, "_ptdi_seeding_rate_", index),  label = "Seeding rate", min = 0, max = 100, step = 1, value = NULL)),
                      column(
                        6,
                        selectizeInput(
                          paste0(crop, "_ptdi_seeding_rate_unit_", index), label = "Unit", 
                          multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                          choices = c("kg/ha",
                                      "lb/ac",
                                      "plants/pot"),
                          selected = "kg/ha"
                        )
                      )
                    ),
                    fluidRow(
                      column(6, numericInput(paste0(crop, "_ptdi_distance_plants_", index),  label = "Distance between plants", min = 0, max = 100, step = 0.1, value = NULL)),
                      column(
                        6,
                        selectizeInput(
                          paste0(crop, "_ptdi_distance_plants_unit_",index), label = "Unit", 
                          multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                          choices = c("cm",
                                      "ft",
                                      "in",
                                      "m"),
                          selected = "cm"
                        )
                      )
                    ),
                    numericInput(paste0(crop, "_ptdi_seeding_density_number_rows_", index),  label = "Number of rows", min = 0, max = 100, step = 1, value = NULL),
                    fluidRow(
                      column(6, numericInput(paste0(crop, "_ptdi_seeding_plant_density_", index),  label = "Plant density", min = 0, max = 100, step = 0.1, value = NULL)),
                      column(
                        6,
                        selectizeInput(
                          paste0(crop, "_ptdi_seeding_plant_density_unit_", index), label = "Unit", 
                          multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                          choices = c("plants/hill",
                                      "plants/m2",
                                      "plants/pot",
                                      "plants/row"),
                          selected = "plants/m2"
                        )
                      )
                    ),
                    fluidRow(
                      column(6, numericInput(paste0(crop, "_ptdi_seeding_distance_bunds_", index),  label = "Distance between bunds", min = 0, max = 100, step = 0.1, value = NULL)),
                      column(
                        6,
                        selectizeInput(
                          paste0(crop, "_ptdi_seeding_distance_bunds_unit_", index), label = "Unit", 
                          multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                          choices = c("cm",
                                      "m",
                                      "in",
                                      "ft"),
                          selected = "cm"
                        )
                      )
                    ),
                    textAreaInput(paste0(crop, "_ptdi_direct_seeding_notes_", index), label="Notes", value="")
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            id = paste0(crop, "_transplanting_boxid_", index),
            title = checkboxInput(paste0(crop, "_transplanting_checkbox_", index), actionLink(paste0(crop, "_transplanting_titleId_", index), "Transplanting"), F),
            status = "primary", solidHeader = TRUE,
            width = 12, collapsible = TRUE, collapsed = TRUE,
            fluidRow(
              column(
                6,
                fluidRow(
                  column(
                    6,
                    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                      airDatepickerInput(
                        paste0(crop, "_ptta_transplanting_start_date_", index),
                        "Start date",
                        clearButton = T,
                        autoClose = T,
                        #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                        placeholder = "yyyy-mm-dd",
                        minDate = as.Date(input$fbDesign_project_start_date) + 1,
                        maxDate = as.Date(input$fbDesign_project_end_date) + 1
                      )
                    } else {
                      airDatepickerInput(
                        paste0(crop, "_ptta_transplanting_start_date_", index),
                        "Start date",
                        clearButton = T,
                        autoClose = T,
                        placeholder = "yyyy-mm-dd"
                      )
                    }
                  ),
                  column(
                    6,
                    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                      airDatepickerInput(
                        paste0(crop, "_ptta_transplanting_end_date_", index),
                        "End date",
                        clearButton = T,
                        autoClose = T,
                        #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                        placeholder = "yyyy-mm-dd",
                        minDate = as.Date(input$fbDesign_project_start_date) + 1,
                        maxDate = as.Date(input$fbDesign_project_end_date) + 1
                      )
                    } else {
                      airDatepickerInput(
                        paste0(crop, "_ptta_transplanting_end_date_", index),
                        "End date",
                        clearButton = T,
                        autoClose = T,
                        placeholder = "yyyy-mm-dd"
                      )
                    }
                  )
                ),
                numericInput(paste0(crop, "_ptta_age_seedling_", index), value="", label = "Age of seedling (days)", min=0, max=100, step=1),
                selectizeInput(
                  paste0(crop, "_ptta_transplanting_environment_", index), label = "Seedling environment", 
                  multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                  choices =c("Flat seed bed",
                             "Hill",
                             "Ridge",
                             "Other")
                ),
                hidden(textInput(paste0(crop, "_ptta_transplanting_environment_", index, "_other"), "", value="")),
                selectizeInput(
                  paste0(crop, "_ptta_transplanting_technique_", index), label = "Technique", 
                  multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                  choices = c("Manual",
                              "Mechanical",
                              "Other")
                ),
                hidden(textInput(paste0(crop, "_ptta_transplanting_technique_", index, "_other"), "", value="")),
                textInput(paste0(crop, "_ptta_transplanting_treatment_", index), value="", label = "Seed treatment"),
                selectizeInput(
                  paste0(crop, "_ptta_trans_traction_", index), label = "Traction", 
                  multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                  choices = c("Animal",
                              "Traction",
                              "2 wheel tractor",
                              "4 wheel tractor",
                              "Other")
                ),
                hidden(textInput(paste0(crop, "_ptta_trans_traction_", index,"_other"), "", value=""))
              ),
              column(
                6,
                fluidRow(
                  box(
                    title = "Transplanting density", solidHeader = TRUE, status = "warning", width = 12,
                    fluidRow(
                      column(12, h4("Transplanting density", style="font-weight: 800;color: #555;"))
                    ),
                    fluidRow(
                      column(6, numericInput(paste0(crop, "_ptta_trans_distance_rows_", index),  label = "Distance between rows", value="", min = 0, max = 100, step = 0.1)),
                      column(
                        6,
                        selectizeInput(
                          paste0(crop, "_ptta_trans_distance_rows_unit_", index), label = "Unit", 
                          multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                          choices = c("cm",
                                      "ft",
                                      "in",
                                      "m"),
                          selected = "cm"
                        )
                      )
                    ),
                    fluidRow(
                      column(6, numericInput(paste0(crop, "_ptta_trans_seeding_density_", index),  label = "Seedling density", value = "", min = 0, max = 100, step = 1)),
                      column(
                        6,
                        selectizeInput(
                          paste0(crop, "_ptta_trans_seeding_density_unit_", index), label = "Unit", 
                          multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                          choices = c("plants/hill",
                                      "plants/m2",
                                      "plants/pot",
                                      "plants/row"),
                          selected = "plants/m2"
                        )
                      )
                    ),
                    numericInput(paste0(crop, "_ptta_trans_num_rows_", index), "Number of rows", value ="", min = 0, max = 100, step = 1),
                    fluidRow(
                      column(6, numericInput(paste0(crop, "_ptta_trans_distance_plants_", index),  label = "Distance between plants", value = "", min = 0, max = 100, step = 0.1)),
                      column(
                        6,
                        selectizeInput(
                          paste0(crop, "_ptta_trans_distance_plants_unit_", index), label = "Unit", 
                          multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), selected="m", 
                          choices = c("m")
                        )
                      )
                    ),
                    fluidRow(
                      column(6, numericInput(paste0(crop, "_ptta_trans_distance_bunds_", index),  label = "Distance between bunds", min = 0, max = 100, step = 0.1, value = NULL)), 
                      column(
                        6,
                        selectizeInput(
                          paste0(crop, "_ptta_trans_distance_bunds_unit_", index), label = "Unit", 
                          multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                          choices = c("cm",
                                      "m",
                                      "in",
                                      "ft"),
                          selected = "cm"
                        )
                      )
                    ),
                    textAreaInput(paste0(crop, "_ptta_transplanting_density_notes_", index), label="Notes", value="")
                  )
                )
              )
            )
          )
        )
      )
    )
  }
  
  ###################### END: PLANTING & TRANSPLANTING ######################
  
  ###################### START: MULCH MANAGEMENT ######################
  
  ## Mulching and Residue
  # mulch_start_date 
  output$mul_start_date <- renderUI({
    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
      airDatepickerInput("mumd_mulch_start_date",
                         "Start date",
                         clearButton = T,
                         autoClose = T,
                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                         placeholder = "yyyy-mm-dd",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("mumd_mulch_start_date",
                         "Mulching start date",
                         clearButton = T,
                         autoClose = T,
                         placeholder = "yyyy-mm-dd"
      )
    }
  })
  
  # mulch_end_date
  output$mul_end_date <- renderUI({
    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
      airDatepickerInput("mulch_end_date",
                         "Mulching end date",
                         clearButton = T,
                         autoClose = T,
                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                         placeholder = "yyyy-mm-dd",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("mulch_end_date",
                         "Mulching end date",
                         clearButton = T,
                         autoClose = T,
                         placeholder = "yyyy-mm-dd"
      )
    }
  })
  
  # mulch_remove_start_date 
  output$mulre_start_date <- renderUI({
    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
      airDatepickerInput("mumd_mulch_remove_start_date",
                         "Mulch removal start date",
                         clearButton = T,
                         autoClose = T,
                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                         placeholder = "yyyy-mm-dd",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("mumd_mulch_remove_start_date",
                         "Mulch removal start date",
                         clearButton = T,
                         autoClose = T,
                         placeholder = "yyyy-mm-dd"
      )
    }
  })
  
  # mulch_remove_end_date
  output$mulre_end_date <- renderUI({
    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
      airDatepickerInput("mumd_mulch_remove_end_date",
                         "Mulch removal end date",
                         clearButton = T,
                         autoClose = T,
                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                         placeholder = "yyyy-mm-dd",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("mumd_mulch_remove_end_date",
                         "Mulch removal end date",
                         clearButton = T,
                         autoClose = T,
                         placeholder = "yyyy-mm-dd"
      )
    }
  })
  
  ###################### END: MULCH MANAGEMENT ######################
  
  ###################### START: IRRIGATION ######################
  
  ## Irrigation: Asigna variables reactivas
  # monocrop
  expconIRRImonocrop <- reactiveValues()
  expconIRRImonocrop$num <- 0
  expconIRRImonocrop$DEFAULT <- 1
  expconIRRImonocrop$ids <- c()
  
  # Irrigation: Inserta por defecto un row en monocrop
  observe({
    if (!is.null(input$selectAgroFeature)) {
      # monocrop
      if (expconIRRImonocrop$num == 0) {
        defaultIRRImonocrop <- expconIRRImonocrop$DEFAULT
        
        for (i in 1:defaultIRRImonocrop) {
          insertRow_IRRI(crop = "monocrop", i)
        }
      }
    }
  })
  
  # Irrigation: Agrega un row al hacer clic en el boton "Add irrigation"
  observeEvent(input$IRRIBoxVar, {
    vars <- unlist(strsplit(input$IRRIBoxVarid, "_"))
    crop <- vars[1]
    
    if (expconIRRImonocrop$num >= 1 && crop == "monocrop") { insertRow_IRRI(crop = "monocrop", expconIRRImonocrop$num + 1) }
  })
  
  insertRow_IRRI <- function(crop, index) {
    # monocrop
    if (crop == "monocrop") {
      expconIRRImonocrop$ids <- c(expconIRRImonocrop$ids, paste0("mono_irri_", index))
      
      insertUI(
        selector = "#monocrop_fr_irrigation",
        where = "beforeBegin",
        ui = getIRRIUI_GEN(crop, index)
      )
      expconIRRImonocrop$num <- expconIRRImonocrop$num + 1
    }
  }
  
  getIRRIUI_GEN <- function(crop, index) {
    #expCondsVars$ids_irri <- c(expCondsVars$ids_irri, paste0("ECIR_", str_id))
    
    fluidRow(
      id = paste0(crop, "_fr_irrigation_box_", index),
      box(
        column(
          12, offset = 0, 
          column(
            6, style='padding:0px; text-align:left;',
            h4("Irrigation details", style="font-weight: 800;color: #555;")
          ),
          column(6, style='padding:0px; text-align:right; ',  actionButton(paste0(crop, "_closeBox_ECIRRI_", index), "", icon("close")))
          
        ),
        br(),
        width = 12, solidHeader = TRUE, status = "warning",
        column(
          6,
          fluidRow(
            column(
              6,
              if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                airDatepickerInput(
                  paste0(crop, "_irid_irrigationevent_start_date_", index),
                  "Start date",
                  clearButton = T,
                  autoClose = T,
                  #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                  placeholder = "yyyy-mm-dd",
                  minDate = as.Date(input$fbDesign_project_start_date) + 1,
                  maxDate = as.Date(input$fbDesign_project_end_date) + 1
                )
              } else {
                airDatepickerInput(
                  paste0(crop, "_irid_irrigationevent_start_date_", index),
                  "Start date",
                  clearButton = T,
                  autoClose = T,
                  placeholder = "yyyy-mm-dd"                           
                )
              }
            ),
            column(
              6,
              if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                airDatepickerInput(
                  paste0(crop, "_irid_irrigationevent_end_date_", index),
                  "End date",
                  clearButton = T,
                  autoClose = T,
                  #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                  placeholder = "yyyy-mm-dd",
                  minDate = as.Date(input$fbDesign_project_start_date) + 1,
                  maxDate = as.Date(input$fbDesign_project_end_date) + 1                 
                )
              } else {
                airDatepickerInput(
                  paste0(crop, "_irid_irrigationevent_end_date_", index),
                  "End date",
                  clearButton = T,
                  autoClose = T,
                  placeholder = "yyyy-mm-dd"
                )
              }
            )
          ),
          selectizeInput(
            paste0(crop, "_irid_irrigation_technique_", index), label = "Irrigation technique", 
            multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
            choices = c("Sprinkler irrigation",
                        "Localized",
                        "Surface",
                        #"Sub-irrigation",
                        "Other")
          ),
          hidden(textInput(paste0(crop, "_irid_irrigation_technique_", index, "_other"), "")),
          conditionalPanel(
            paste0("input.", crop, "_irid_irrigation_technique_", index, "== 'Surface'"),
            selectizeInput(
              paste0(crop, "_irid_surface_irrigation_technique_", index), label = "Surface irrigation technique", 
              multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
              choices = c("Basin irrigation",
                          "Border irrigation",
                          "Continuous flood",
                          "Furrow irrigation",
                          "Uncontrolled flooding",
                          "Other")
            ),
            hidden(textInput(paste0(crop, "_irid_surface_irrigation_technique_", index, "_other"), ""))
          ),
          conditionalPanel(
            paste0("input.", crop, "_irid_irrigation_technique_", index, "== 'Localized'"),
            selectizeInput(
              paste0(crop, "_irid_localized_irrigation_technique", index), label = "Localized irrigation technique", 
              multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
              choices = c("Bubbler irrigation",
                          "Drip irrigation",
                          "Mist irrigation",
                          "Pitcher irrigation",
                          "Subsurface drip irrigation",
                          "Subsurface textile irrigation",
                          "Other")
            ),
            hidden(textInput(paste0(crop, "_irid_localized_irrigation_technique", index, "_other"), ""))
          ),
          conditionalPanel(
            paste0("input.", crop, "_irid_irrigation_technique_", index, "== 'Sprinkler irrigation'"),
            selectizeInput(
              paste0(crop, "_irid_irrigation_using_sprinkler_systems_", index), label = "Sprinkler irrigation system", 
              multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
              choices = c("Center pivot irrigation",
                          "Irrigation by lateral move",
                          "Irrigation by side move",
                          "Other")
            ),
            hidden(textInput(paste0(crop, "_irid_irrigation_using_sprinkler_systems_", index, "_other"), ""))
          ),
          selectizeInput(
            paste0(crop, "_irid_irrigation_source_", index), label = "Irrigation source", 
            multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
            choices = c("Drainage",
                        "Groundwater",
                        "Lake",
                        "Reservoir",
                        "River",
                        "Spring",
                        "Other")
          ),
          hidden(textInput(paste0(crop, "_irrigation_source_", index,  "_other"), ""))
        ),
        column(
          6,
          fluidRow(
            column(6, numericInput(paste0(crop, "_irid_irrigation_source_distance_", index), label = "Irrigation source distance", value = "", min = 0, step = 0.1)),
            column(
              6,
              selectizeInput(
                paste0(crop, "_irid_irrigation_source_distance_", index, "unit"), "Unit", 
                multiple = T, options = list(maxItems = 1, placeholder="Select one..."),
                choices = c("ft", "km", "m", "mi"),
                selected = "m"
              )
            )
          ),
          fluidRow(
            column(6, numericInput(paste0(crop, "_irid_irrigation_amount_", index), label = "Irrigation amount", value = "", min = 0, step = 0.1)),
            column(
              6,
              selectizeInput(
                paste0(crop, "_irid_irrigation_amount_", index, "unit"), "Unit", 
                multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                choices = c("in", "mm"),#, "cm", "m", "in", "ft", "ml", "L", "gal", "cu m", "cu in", "cu ft")
                selected = "mm"
              )
            )
          ),
          textAreaInput(paste0(crop, "_irid_irrigation_notes_", index), label = "Notes", value = "")
        )
      )
    )
  }
  
  # Irrigation: Funcion GENERAL que activa "Close"
  observeEvent(input$closeBox_ECIRRI_GEN, {
    vars <- unlist(strsplit(input$closeBox_ECIRRI_GENid, "_"))
    crop <- vars[1]
    index <- vars[4]
    
    if (length(expconIRRImonocrop$ids) > 1) {
      removeUI(
        selector = paste0("#", crop, "_fr_irrigation_box_", index),
        immediate = T
      )
      expconIRRImonocrop$ids <- expconIRRImonocrop$ids[!expconIRRImonocrop$ids %in% paste0("mono_irri_", index)]
    }
  })
  
  ###################### END: IRRIGATION ######################
  
  ###################### START: WEEDING ######################
  
  ## Weeding: Asigna variables reactivas
  # monocrop
  expconWEEmonocrop <- reactiveValues()
  expconWEEmonocrop$num <- 0
  expconWEEmonocrop$DEFAULT <- 1
  expconWEEmonocrop$ids <- c()
  
  # Weeding: Inserta por defecto un row en monocrop
  observe({
    if (!is.null(input$selectAgroFeature)) {
      # monocrop
      if (expconWEEmonocrop$num == 0) {
        defaultWEEmonocrop <- expconWEEmonocrop$DEFAULT
        
        for (i in 1:defaultWEEmonocrop) {
          insertRow_WEE(crop = "monocrop", i)
        }
      }
    }
  })
  
  # Weeding: Agrega un row al hacer clic en el boton "Add irrigation"
  observeEvent(input$WEEBoxVar, {
    vars <- unlist(strsplit(input$WEEBoxVarid, "_"))
    crop <- vars[1]
    
    if (expconWEEmonocrop$num >= 1 && crop == "monocrop") { insertRow_WEE(crop = "monocrop", expconWEEmonocrop$num + 1) }
  })
  
  insertRow_WEE <- function(crop, index) {
    # monocrop
    if (crop == "monocrop") {
      expconWEEmonocrop$ids <- c(expconWEEmonocrop$ids, paste0("mono_wee_", index))
      
      insertUI(
        selector = "#monocrop_fr_weeding",
        where = "beforeBegin",
        ui = getWEEUI_GEN(crop, index)
      )
      expconWEEmonocrop$num <- expconWEEmonocrop$num + 1
    }
  }
  
  getWEEUI_GEN <- function(crop, index) {
    #expCondsVars$ids_irri <- c(expCondsVars$ids_irri, paste0("ECIR_", str_id))
    
    fluidRow(
      id = paste0(crop, "_fr_weeding_box_", index),
      box(
        column(
          12, offset = 0, 
          column(
            6, style='padding:0px; text-align:left;',
            h4("Weeding details", style="font-weight: 800;color: #555;")
          ),
          column(
            6, 
            style='padding:0px; text-align:right;', actionButton(paste0(crop, "_closeBox_ECWEE_", index), "", icon("close"))
          )
        ),
        width = 12, solidHeader = TRUE, status = "warning",
        column(
          6,
          fluidRow(
            column(
              6, 
              if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                airDatepickerInput(
                  paste0(crop, "_wewd_weeding_start_date_", index),
                  "Start date",
                  clearButton = T,
                  autoClose = T,
                  #value = startDate,
                  placeholder = "yyyy-mm-dd",
                  minDate = as.Date(input$fbDesign_project_start_date) + 1,
                  maxDate = as.Date(input$fbDesign_project_end_date) + 1
                )
              } else {
                airDatepickerInput(
                  paste0(crop, "_wewd_weeding_start_date_", index),
                  "Start date",
                  clearButton = T,
                  #value = startDate,
                  autoClose = T,
                  placeholder = "yyyy-mm-dd"                           
                )
              }
            )
          ),
          selectizeInput(
            paste0(crop, "_wewd_weeding_technique_", index), "Technique", multiple = TRUE, 
            options = list(maxItems =1, placeholder ="Select one..."),
            choices = c("Chemical",
                        "Manual",
                        "Mechanized")
          ),
          textAreaInput(paste0(crop, "_wewd_weeding_notes_", index), "Notes")
        ),
        column(
          6,
          fluidRow(
            column(12, h4("Implement", style="font-weight: 800;color: #555;"))
          ),
          selectizeInput(
            paste0(crop, "_wewd_weeding_type_",index ), "Type", multiple = TRUE,
            options = list(maxItems =1, placeholder ="Select one..."),
            choices = c("Cultivator",
                        "Manual",
                        "Sprayer",
                        "Weed cutter/puller",
                        "Other")
          ),
          hidden(textInput(paste0(crop, "_wewd_weeding_type_",index, "_other" ), "")),
          selectizeInput(
            paste0(crop, "_wewd_weeding_traction_", index), "Traction",multiple = TRUE,
            options = list(maxItems =1, placeholder ="Select one..."),
            choices = c("Animal",
                        "Manual",
                        "2 wheel tractor",
                        "4 wheel tractor",
                        "Other")
          ),
          hidden(textInput(paste0(crop, "_wewd_weeding_traction_",index, "_other" ), ""))
        )
      )
    )
  }
  
  # Weeding: Funcion GENERAL que activa "Close"
  observeEvent(input$closeBox_ECWEE_GEN, {
    vars <- unlist(strsplit(input$closeBox_ECWEE_GENid, "_"))
    crop <- vars[1]
    index <- vars[4]
    
    if (length(expconWEEmonocrop$ids) > 1) {
      removeUI(
        selector = paste0("#", crop, "_fr_weeding_box_", index),
        immediate = T
      )
      expconWEEmonocrop$ids <- expconWEEmonocrop$ids[!expconWEEmonocrop$ids %in% paste0("mono_wee_", index)]
    }
  })
  
  ###################### END: WEEDING ######################
  
  ###################### START: HARVEST ######################
  
  # Oculta por defecto los tabs de inicio de Int, Rel & Rot
  observe({
    shiny::hideTab(inputId = "tabpanelHARVint", target = "harv_int_default")
    shiny::hideTab(inputId = "tabpanelHARVrel", target = "harv_rel_default")
    shiny::hideTab(inputId = "tabpanelHARVrot", target = "harv_rot_default")
  })
  
  # Inserta los tabs en Exp Cond dependiendo del tipo de cultivo
  observeEvent(input$HARVBoxInterVar, {
    id <- input$HARVBoxInterVarid
    vars <- unlist(strsplit(id, "_"))
    cropType <- vars[1]
    index <- vars[3]
    
    vals <- getValuesCropHARV(cropType)
    insertTabsHARV(vals, cropType)
    
    output[[paste0("title_panel_",cropType,"_harv_",index)]] = renderText({
      input[[id]]
    })
    
  })
  
  # Funcion que extrae lista de nombres que ira en los tabs de exp cond
  getValuesCropHARV <- function(cropType) {
    if (cropType == "int") { ids <- intercropVars$ids }
    else if (cropType == "rel") { ids <- relaycropVars$ids }
    else if (cropType == "rot") { ids <- rotationcropVars$ids }
    
    newids <- inputid <- c()
    
    for (i in 1:length(ids)) {
      vars <- unlist(strsplit(ids[i], "_"))
      newids[i] <- vars[2]
    }
    
    for (i in 1:length(newids)) {
      inputid[i] <- paste0(cropType, "_cropCommonName_", newids[i])
    }
    
    df <- data.frame(id = c(inputid), values = "", stringsAsFactors = F)
    val <- AllInputs() %>% dplyr::filter(id %in% df$id)
    val <- c(val$values)
    
    
    i<-1
    for (id in newids){
      if(val[i] != ""){
        val[i] <- paste0(cropType, "_harv_", id)
        
      }
      i<-i+1
    }
    
    val <- val[val != ""]
    val
  }
  
  expconHARVinter <- reactiveValues()
  expconHARVinter$var <- c()
  
  # Funcion que inserta los tabs dependiendo del tipo de cultivo
  insertTabsHARV <- function(vals, cropType) {
    if (length(vals) != 0) {
      xx <- expconHARVinter$var[!expconHARVinter$var%in%vals]
      vals <- vals[!vals %in% unique(expconHARVinter$var)]
      expconHARVinter$var <- c(expconHARVinter$var, vals)
      
      if (!is.null(xx)) {
        for (i in 1:length(xx)) {
          removeTab(inputId = paste0("tabpanelHARV", cropType), target = xx[i])
          expconHARVinter$var <- expconHARVinter$var[!expconHARVinter$var %in% xx]
        }
      }
      
      if (length(vals) >= 1) {
        for (i in 1:length(vals)) {
          insertTab(
            inputId = paste0("tabpanelHARV", cropType),
            tabPanel(
              title = uiOutput(paste0("title_panel_",vals[i])),
              value = vals[i],
              br(),
              fluidRow(id = paste0(vals[i], "_fr_harvest")),
              actionButton(paste0(vals[i], "_harv_add"), "Add harvest"),
              # insertRow_HARV_TEST(cropType,crop = vals[i], 1)
              
              insertRow_HARV(crop = vals[i], 1)
            ),
            target = paste0("harv_", cropType, "_default"),
            position = "before",
            select = T
          )
        }
      }
    }
  }
  
  ## Harvest: Asigna variables reactivas
  # monocrop
  expconHARVmonocrop <- reactiveValues()
  expconHARVmonocrop$num <- 0
  expconHARVmonocrop$DEFAULT <- 1
  expconHARVmonocrop$ids <- c()
  # inter HARV crop 1
  expconIntHARVcrop1 <- reactiveValues()
  expconIntHARVcrop1$num <- 0
  expconIntHARVcrop1$DEFAULT <- 1
  expconIntHARVcrop1$ids <- c()
  # inter HARV crop 2
  expconIntHARVcrop2 <- reactiveValues()
  expconIntHARVcrop2$num <- 0
  expconIntHARVcrop2$DEFAULT <- 1
  expconIntHARVcrop2$ids <- c()
  # inter HARV crop 3
  expconIntHARVcrop3 <- reactiveValues()
  expconIntHARVcrop3$num <- 0
  expconIntHARVcrop3$DEFAULT <- 1
  expconIntHARVcrop3$ids <- c()
  # inter HARV crop 4
  expconIntHARVcrop4 <- reactiveValues()
  expconIntHARVcrop4$num <- 0
  expconIntHARVcrop4$DEFAULT <- 1
  expconIntHARVcrop4$ids <- c()
  # inter HARV crop 5
  expconIntHARVcrop5 <- reactiveValues()
  expconIntHARVcrop5$num <- 0
  expconIntHARVcrop5$DEFAULT <- 1
  expconIntHARVcrop5$ids <- c()
  # relay HARV crop 1
  expconRelHARVcrop1 <- reactiveValues()
  expconRelHARVcrop1$num <- 0
  expconRelHARVcrop1$DEFAULT <- 1
  expconRelHARVcrop1$ids <- c()
  # relay HARV crop 2
  expconRelHARVcrop2 <- reactiveValues()
  expconRelHARVcrop2$num <- 0
  expconRelHARVcrop2$DEFAULT <- 1
  expconRelHARVcrop2$ids <- c()
  # relay HARV crop 3
  expconRelHARVcrop3 <- reactiveValues()
  expconRelHARVcrop3$num <- 0
  expconRelHARVcrop3$DEFAULT <- 1
  expconRelHARVcrop3$ids <- c()
  # relay HARV crop 4
  expconRelHARVcrop4 <- reactiveValues()
  expconRelHARVcrop4$num <- 0
  expconRelHARVcrop4$DEFAULT <- 1
  expconRelHARVcrop4$ids <- c()
  # relay HARV crop 5
  expconRelHARVcrop5 <- reactiveValues()
  expconRelHARVcrop5$num <- 0
  expconRelHARVcrop5$DEFAULT <- 1
  expconRelHARVcrop5$ids <- c()
  
  # Harvest: Inserta por defecto un row en monocrop
  observe({
    if (!is.null(input$selectAgroFeature)) {
      # monocrop
      if (expconHARVmonocrop$num == 0) {
        defaultHARVmonocrop <- expconHARVmonocrop$DEFAULT
        
        for (i in 1:defaultHARVmonocrop) {
          insertRow_HARV(crop = "monocrop", i)
        }
      }
    }
  })
  
  
  # Harvest: Agrega un row al hacer clic en el boton "Add harvest"
  observeEvent(input$HARVBoxVar, {
    vars <- unlist(strsplit(input$HARVBoxVarid, "_"))
    crop <- paste0(vars[1], "_", vars[2], "_", vars[3])
    
    if (expconHARVmonocrop$num >= 1 && crop != "monocrop") { insertRow_HARV(crop = "monocrop", expconHARVmonocrop$num + 1) }
    if (expconIntHARVcrop1$num >= 1 && crop == "int_harv_1") { insertRow_HARV(crop = "int_harv_1", expconIntHARVcrop1$num + 1) }
    if (expconIntHARVcrop2$num >= 1 && crop == "int_harv_2") { insertRow_HARV(crop = "int_harv_2", expconIntHARVcrop2$num + 1) }
    if (expconIntHARVcrop3$num >= 1 && crop == "int_harv_3") { insertRow_HARV(crop = "int_harv_3", expconIntHARVcrop3$num + 1) }
    if (expconIntHARVcrop4$num >= 1 && crop == "int_harv_4") { insertRow_HARV(crop = "int_harv_4", expconIntHARVcrop4$num + 1) }
    if (expconIntHARVcrop5$num >= 1 && crop == "int_harv_5") { insertRow_HARV(crop = "int_harv_5", expconIntHARVcrop5$num + 1) }
    if (expconRelHARVcrop1$num >= 1 && crop == "rel_harv_1") { insertRow_HARV(crop = "rel_harv_1", expconRelHARVcrop1$num + 1) }
    if (expconRelHARVcrop2$num >= 1 && crop == "rel_harv_2") { insertRow_HARV(crop = "rel_harv_2", expconRelHARVcrop2$num + 1) }
    if (expconRelHARVcrop3$num >= 1 && crop == "rel_harv_3") { insertRow_HARV(crop = "rel_harv_3", expconRelHARVcrop3$num + 1) }
    if (expconRelHARVcrop4$num >= 1 && crop == "rel_harv_4") { insertRow_HARV(crop = "rel_harv_4", expconRelHARVcrop4$num + 1) }
    if (expconRelHARVcrop5$num >= 1 && crop == "rel_harv_5") { insertRow_HARV(crop = "rel_harv_5", expconRelHARVcrop5$num + 1) }
  })
  
  insertRow_HARV <- function(crop, index) {
    # monocrop
    if (crop == "monocrop") {
      expconHARVmonocrop$ids <- c(expconHARVmonocrop$ids, paste0("mono_harv_", index))
      
      insertUI(
        selector = "#monocrop_fr_harvest",
        where = "beforeBegin",
        ui = getHARVUI_GEN(crop, index)
      )
      expconHARVmonocrop$num <- expconHARVmonocrop$num + 1
    }
    # inter HARV crop 1
    if (crop == "int_harv_1") {
      expconIntHARVcrop1$ids <- c(expconIntHARVcrop1$ids, paste0("int_harv_1_", index))
      
      insertUI(
        selector = "#int_harv_1_fr_harvest",
        where = "beforeBegin",
        ui = getHARVUI_GEN(crop, index)
      )
      expconIntHARVcrop1$num <- expconIntHARVcrop1$num + 1
    }
    # inter HARV crop 2
    if (crop == "int_harv_2") {
      expconIntHARVcrop2$ids <- c(expconIntHARVcrop2$ids, paste0("int_harv_2_", index))
      
      insertUI(
        selector = "#int_harv_2_fr_harvest",
        where = "beforeBegin",
        ui = getHARVUI_GEN(crop, index)
      )
      expconIntHARVcrop2$num <- expconIntHARVcrop2$num + 1
    }
    # inter HARV crop 3
    if (crop == "int_harv_3") {
      expconIntHARVcrop3$ids <- c(expconIntHARVcrop3$ids, paste0("int_harv_3_", index))
      
      insertUI(
        selector = "#int_harv_3_fr_harvest",
        where = "beforeBegin",
        ui = getHARVUI_GEN(crop, index)
      )
      expconIntHARVcrop3$num <- expconIntHARVcrop3$num + 1
    }
    # inter HARV crop 4
    if (crop == "int_harv_4") {
      expconIntHARVcrop4$ids <- c(expconIntHARVcrop4$ids, paste0("int_harv_4_", index))
      
      insertUI(
        selector = "#int_harv_4_fr_harvest",
        where = "beforeBegin",
        ui = getHARVUI_GEN(crop, index)
      )
      expconIntHARVcrop4$num <- expconIntHARVcrop4$num + 1
    }
    # inter HARV crop 5
    if (crop == "int_harv_5") {
      expconIntHARVcrop5$ids <- c(expconIntHARVcrop5$ids, paste0("int_harv_5_", index))
      
      insertUI(
        selector = "#int_harv_5_fr_harvest",
        where = "beforeBegin",
        ui = getHARVUI_GEN(crop, index)
      )
      expconIntHARVcrop5$num <- expconIntHARVcrop5$num + 1
    }
    # relay HARV crop 1
    if (crop == "rel_harv_1") {
      expconRelHARVcrop1$ids <- c(expconRelHARVcrop1$ids, paste0("rel_harv_1_", index))
      
      insertUI(
        selector = "#rel_harv_1_fr_harvest",
        where = "beforeBegin",
        ui = getHARVUI_GEN(crop, index)
      )
      expconRelHARVcrop1$num <- expconRelHARVcrop1$num + 1
    }
    # relay HARV crop 2
    if (crop == "rel_harv_2") {
      expconRelHARVcrop2$ids <- c(expconRelHARVcrop2$ids, paste0("rel_harv_2_", index))
      
      insertUI(
        selector = "#rel_harv_2_fr_harvest",
        where = "beforeBegin",
        ui = getHARVUI_GEN(crop, index)
      )
      expconRelHARVcrop2$num <- expconRelHARVcrop2$num + 1
    }
    # relay HARV crop 3
    if (crop == "rel_harv_3") {
      expconRelHARVcrop3$ids <- c(expconRelHARVcrop3$ids, paste0("rel_harv_3_", index))
      
      insertUI(
        selector = "#rel_harv_3_fr_harvest",
        where = "beforeBegin",
        ui = getHARVUI_GEN(crop, index)
      )
      expconRelHARVcrop3$num <- expconRelHARVcrop3$num + 1
    }
    # relay HARV crop 4
    if (crop == "rel_harv_4") {
      expconRelHARVcrop4$ids <- c(expconRelHARVcrop4$ids, paste0("rel_harv_4_", index))
      
      insertUI(
        selector = "#rel_harv_4_fr_harvest",
        where = "beforeBegin",
        ui = getHARVUI_GEN(crop, index)
      )
      expconRelHARVcrop4$num <- expconRelHARVcrop4$num + 1
    }
    # relay HARV crop 5
    if (crop == "rel_harv_5") {
      expconRelHARVcrop5$ids <- c(expconRelHARVcrop5$ids, paste0("rel_harv_5_", index))
      
      insertUI(
        selector = "#rel_harv_5_fr_harvest",
        where = "beforeBegin",
        ui = getHARVUI_GEN(crop, index)
      )
      expconRelHARVcrop5$num <- expconRelHARVcrop5$num + 1
    }
  }
  
  getHARVUI_GEN <- function(crop, index) {
    fluidRow(
      id = paste0(crop, "_fr_harvest_", index),
      box(
        column(
          12, offset = 0, 
          column(
            6, style='padding:0px; text-align:left;',
            h4("Harvest details", style="font-weight: 800;color: #555;")
          ),
          column(
            6,
            style='padding:0px; text-align:right;', actionButton(paste0(crop, "_closeBox_ECHARV_", index), "", icon("close"))
          )
        ),
        width = 12, status = "warning", solidHeader = TRUE,
        column(
          6,
          fluidRow(
            column(
              6,
              if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                airDatepickerInput(
                  paste0(crop, "_hahd_harvest_start_date_", index),
                  "Start date",
                  clearButton = T,
                  autoClose = T,
                  #value = as.Date(input$fbDesign_project_start_date) + 1,
                  placeholder = "yyyy-mm-dd",
                  minDate = as.Date(input$fbDesign_project_start_date) + 1,
                  maxDate = as.Date(input$fbDesign_project_end_date) + 1                           
                )
              } else {
                airDatepickerInput(
                  paste0(crop, "_hahd_harvest_start_date_", index),
                  "Start date",
                  clearButton = T,
                  autoClose = T,
                  placeholder = "yyyy-mm-dd"
                )
              }
            ),
            column(
              6,
              if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                airDatepickerInput(
                  paste0(crop, "_hahd_harvest_end_date_", index),
                  "End date",
                  clearButton = T,
                  autoClose = T,
                  #value = as.Date(input$fbDesign_project_start_date) + 1,
                  placeholder = "yyyy-mm-dd",
                  minDate = as.Date(input$fbDesign_project_start_date) + 1,
                  maxDate = as.Date(input$fbDesign_project_end_date) + 1
                )
              } else {
                airDatepickerInput(
                  paste0(crop, "_hahd_harvest_end_date_", index),
                  "End date",
                  clearButton = T,
                  autoClose = T,
                  placeholder = "yyyy-mm-dd"                           
                )
              }
            )
          ),
          fluidRow(
            column(
              6,
              selectizeInput(
                paste0(crop, "_hahd_harvest_method_", index), label = "Harvest method", 
                multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                choices = c("Baling", "Cutting", "Mowing", "Haymaking", "Picking", "Threshing", "Trussing", "Windrowing","Winnowing","Other")
              )
            )
          ),
          hidden(textInput(paste0(crop, "_hahd_harvest_method_", index,"_other"), "")),
          selectizeInput(
            paste0(crop, "_hahd_crop_component_harvested_", index), label = "Crop component harvested", 
            multiple = TRUE, options = list(maxItems =11, placeholder ="Select one..."), 
            choices = c("Canopy", "Aboveground biomass","Leaves","Stems","Seed","Pod", "Grain", "Tuber","Roots (excluding storage roots)", "Storage roots", "Other")
          ),
          hidden(textInput(paste0(crop, "_hahd_crop_component_harvested_",index,"_other"), "")),
          selectizeInput(
            paste0(crop, "_hahd_crop_harvestable_area_", index), label = "Harvestable area", 
            multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
            choices = c("m2 units", "Individual plants","Rows","Entire plot","Other")
          ),
          conditionalPanel(
            paste0("input.", crop, "_hahd_crop_harvestable_area_",index, " == 'm2 units'"),
            textInput(paste0("hahd_crop_component_harvested_m2_",index), "Number of m2 units harvested")
          ),
          conditionalPanel(
            paste0("input.", crop, "_hahd_crop_harvestable_area_",index, " == 'Individual plants'"),
            textInput(paste0("hahd_crop_component_harvested_ip_",index), "Number of plants harvested")
          ),
          conditionalPanel(
            paste0("input.", crop, "_hahd_crop_harvestable_area_",index, " == 'Rows'"),
            fluidRow(
              column(6, textInput(paste0(crop, "_hahd_crop_component_harvested_num_",index), "Number of rows harvested"))
            ),
            fluidRow(
              column(6, textInput(paste0(crop, "_hahd_crop_component_harvested_len_",index), "Length of rows harvested")),
              column(
                6,
                selectizeInput(
                  paste0(crop, "_hahd_crop_component_harvested_lenunit_",index),  label ="Unit", multiple = TRUE, 
                  options = list(maxItems =11, placeholder ="Select one..."), 
                  choices = c("cm", "m", "in","ft"), selected = "cm"
                )
              )
            ),
            fluidRow(
              column(6, textInput(paste0(crop, "_hahd_crop_component_harvested_width_",index), "Width within rows harvested")),
              column(
                6,
                selectizeInput(
                  paste0(crop, "_hahd_crop_component_harvested_widthunit_",index),  label ="Unit", multiple = TRUE, 
                  options = list(maxItems =11, placeholder ="Select one..."), 
                  choices = c("cm", "m", "in","ft"), selected = "cm"
                )
              )
            ),
            fluidRow(
              column(6, numericInput(paste0(crop, "_hahd_space_rows_harvested_", index), "Space between rows harvested", value = "", min = 0, step = 0.1)),
              column(
                6,
                selectizeInput(
                  paste0(crop, "_hahd_crop_component_harvested_spaceunit_",index),  label ="Unit", multiple = TRUE, 
                  options = list(maxItems =11, placeholder ="Select one..."), 
                  choices = c("cm", "m", "in","ft"), selected = "cm"
                )
              )
            )
          ),
          conditionalPanel(
            paste0("input.", crop, "_hahd_crop_harvestable_area_",index, " == 'Entire plot'"),
            fluidRow(
              column(6, textInput(paste0(crop, "_hahd_crop_component_harvested_entire_",index), "Plot area harvested")),
              column(
                6, 
                selectizeInput(
                  paste0(crop, "_hahd_crop_component_harvested_entireunit_",index),  label ="Unit", multiple = TRUE, 
                  options = list(maxItems =11, placeholder ="Select one..."), 
                  choices = c("m2", "ha", "ft2","ac"), selected = "ha"
                )
              )
            )
          ),
          hidden(textInput(paste0(crop, "_hahd_crop_harvestable_area_", index,"_other"), "")),
          fluidRow(
            column(6, numericInput(paste0(crop, "_hahd_amount_harvested_", index), "Amount harvested", value = "", min = 0, step = 0.1)),
            column(
              6,
              selectizeInput(
                paste0(crop, "_hahd_amount_harvested_unit_", index), label="Unit", multiple = TRUE, 
                options = list(maxItems =1, placeholder ="Select one..."), 
                choices=c("g", "kg", "lb", "t"), selected = "g"
              )
            )
          ),
          fluidRow(
            column(6, numericInput(paste0(crop, "_hahd_harvest_cut_height_", index), "Harvest cut height", value = "", min = 0, step = 0.1)),
            column(
              6,
              selectizeInput(
                paste0(crop, "_hahd_harvest_cut_height_unit_", index), label="Unit", multiple = TRUE, 
                options = list(maxItems =1, placeholder ="Select one..."), 
                choices=c("cm", "ft", "in", "m"), selected = "cm"
              )
            )
          ),
          textAreaInput(inputId = paste0(crop, "_hahd_harvest_notes_", index), label = "Notes", value = "")
        ),
        column(
          6,
          fluidRow(
            column(12, h4("Implement", style="font-weight: 800;color: #555;"))
          ),     
          selectizeInput(
            paste0(crop, "_hahd_harvest_implement_", index), label = "Type", multiple = TRUE, 
            options = list(maxItems =1, placeholder ="Select one..."), 
            choices = c("Baler",
                        "Chopper",
                        "Combine",
                        "Digger",
                        "Mower",
                        "Reaper",
                        "Roller",
                        "Sickle",
                        "Other")
          ),
          hidden(textInput(paste0(crop, "_hahd_harvest_implement_", index, "_other"), "")),
          selectizeInput(
            paste0(crop, "_hahd_harvest_traction_" , index), label = "Traction", multiple = TRUE, 
            options = list(maxItems =1, placeholder ="Select one..."), 
            choices = c("Animal",
                        "Manual",
                        "2 wheel tractor",
                        "4 wheel tractor",
                        "Other")
          ),
          hidden(textInput(paste0(crop, "_hahd_harvest_traction_",index,"_other"), ""))     
        )
      )
    )
  }
  
  # Harvest: Funcion GENERAL que activa "Close"
  observeEvent(input$closeBox_ECHARV_GEN, {
    vars <- unlist(strsplit(input$closeBox_ECHARV_GENid, "_"))
    typeCrop <- vars[1]
    
    if (typeCrop == "monocrop") {
      crop <- vars[1]
      index <- vars[4]
    } else {
      crop <- paste0(vars[1], "_", vars[2], "_", vars[3])
      index <- vars[6]
    }
    
    if (crop == "monocrop") {
      if (length(expconHARVmonocrop$ids) > 1) {
        removeUI(selector = paste0("#", crop, "_fr_harvest_", index), immediate = T)
        expconHARVmonocrop$ids <- expconHARVmonocrop$ids[!expconHARVmonocrop$ids %in% paste0("mono_harv_", index)]
      }
    }
    if (crop == "int_harv_1") {
      if (length(expconIntHARVcrop1$ids) > 1) {
        removeUI(selector = paste0("#", crop, "_fr_harvest_", index), immediate = T)
        expconIntHARVcrop1$ids <- expconIntHARVcrop1$ids[!expconIntHARVcrop1$ids %in% paste0("int_harv_1_", index)]
      }
    }
    if (crop == "int_harv_2") {
      if (length(expconIntHARVcrop2$ids) > 1) {
        removeUI(selector = paste0("#", crop, "_fr_harvest_", index), immediate = T)
        expconIntHARVcrop2$ids <- expconIntHARVcrop2$ids[!expconIntHARVcrop2$ids %in% paste0("int_harv_2_", index)]
      }
    }
    if (crop == "int_harv_3") {
      if (length(expconIntHARVcrop3$ids) > 1) {
        removeUI(selector = paste0("#", crop, "_fr_harvest_", index), immediate = T)
        expconIntHARVcrop3$ids <- expconIntHARVcrop3$ids[!expconIntHARVcrop3$ids %in% paste0("int_harv_3_", index)]
      }
    }
    if (crop == "int_harv_4") {
      if (length(expconIntHARVcrop4$ids) > 1) {
        removeUI(selector = paste0("#", crop, "_fr_harvest_", index), immediate = T)
        expconIntHARVcrop4$ids <- expconIntHARVcrop4$ids[!expconIntHARVcrop4$ids %in% paste0("int_harv_4_", index)]
      }
    }
    if (crop == "int_harv_5") {
      if (length(expconIntHARVcrop5$ids) > 1) {
        removeUI(selector = paste0("#", crop, "_fr_harvest_", index), immediate = T)
        expconIntHARVcrop5$ids <- expconIntHARVcrop5$ids[!expconIntHARVcrop5$ids %in% paste0("int_harv_5_", index)]
      }
    }
    if (crop == "rel_harv_1") {
      if (length(expconRelHARVcrop1$ids) > 1) {
        removeUI(selector = paste0("#", crop, "_fr_harvest_", index), immediate = T)
        expconRelHARVcrop1$ids <- expconRelHARVcrop1$ids[!expconRelHARVcrop1$ids %in% paste0("rel_harv_1_", index)]
      }
    }
    if (crop == "rel_harv_2") {
      if (length(expconRelHARVcrop2$ids) > 1) {
        removeUI(selector = paste0("#", crop, "_fr_harvest_", index), immediate = T)
        expconRelHARVcrop2$ids <- expconRelHARVcrop2$ids[!expconRelHARVcrop2$ids %in% paste0("rel_harv_2_", index)]
      }
    }
    if (crop == "rel_harv_3") {
      if (length(expconRelHARVcrop3$ids) > 1) {
        removeUI(selector = paste0("#", crop, "_fr_harvest_", index), immediate = T)
        expconRelHARVcrop3$ids <- expconRelHARVcrop3$ids[!expconRelHARVcrop3$ids %in% paste0("rel_harv_3_", index)]
      }
    }
    if (crop == "rel_harv_4") {
      if (length(expconRelHARVcrop4$ids) > 1) {
        removeUI(selector = paste0("#", crop, "_fr_harvest_", index), immediate = T)
        expconRelHARVcrop4$ids <- expconRelHARVcrop4$ids[!expconRelHARVcrop4$ids %in% paste0("rel_harv_4_", index)]
      }
    }
    if (crop == "rel_harv_5") {
      if (length(expconRelHARVcrop5$ids) > 1) {
        removeUI(selector = paste0("#", crop, "_fr_harvest_", index), immediate = T)
        expconRelHARVcrop5$ids <- expconRelHARVcrop5$ids[!expconRelHARVcrop5$ids %in% paste0("rel_harv_5_", index)]
      }
    }
  })
  
  ###################### END: HARVEST ######################
  
  ############################### END SERVER: MANAGEMENT PRACTICES ###############################
  ################################################################################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # observeEvent(input$load_inputs, {
  #   n <- 5
  #   for (i in 1:n) {
  #     Sys.sleep(0.1)
  #     shinyjs::click("addFundingAgency")
  #   }
  # })
  
  observeEvent(input$savetest4, {
    id_rand_fa <- getAddInputId(experimentVars$ids_FA, "FA_", "")
    print(id_rand_fa)
    
    # fa <- AllInputs() %>%
    #   filter(!str_detect(id, "button")) %>%
    #   filter(!str_detect(id, "-selectized"))
    
    # a <- fa %>% filter(str_detect(id,"designFieldbook_fundAgencyType_[:uppercase:]+$"))
    # print(a)
    a <- a <- c()
    for (i in 1:length(id_rand_fa)) {
      a[i] <- paste0("designFieldbook_fundAgencyType_", id_rand_fa[i])
      b[i] <- paste0("designFieldbook_fundAgencyType_", id_rand_fa[i], "_other")
    }
    #df2 <- data.frame(inputId = a1, type = a2, create = a3, stringsAsFactors = F)
    z <- data.frame(id = c(a, b), values = "", stringsAsFactors = F)
    print(z)
    
    bb <- AllInputs() %>% dplyr::filter(id %in% z$id)
    print(bb)
    
    # c <- rbind(a, bb)
    # print(c)
    
    resall <- arrange_by_pattern(bb, id_rand_fa)
    print(resall)
    
    # b <- fa %>% filter(str_detect(id,"designFieldbook_fundAgencyType_name_[:uppercase:]+$"))
    # c <- fa %>% filter(str_detect(id,"designFieldbook_fundAgencyType_cgiar_[:uppercase:]+$"))
    # 
    # faall <- rbind(a, b, c)
    # print(faall)
    # resall <- arrange_by_pattern(faall, id_rand_fa)
    # print(resall)
    # 
    # write.csv(resall, paste0(sessionpath, input$uniqueId, ".csv"), row.names = FALSE)
  })
  
  observeEvent(input$savetest3, {
    id_rand_fa <- getAddInputId(experimentVars$ids_FA, "FA_", "")
    print(id_rand_fa)
    aa <- a <- b <- c <- d <- c()
    for (i in 1:length(id_rand_fa)) {
      a[i] <- paste0("designFieldbook_fundAgencyType_", id_rand_fa[i])
      b[i] <- paste0("designFieldbook_fundAgencyType_", id_rand_fa[i], "_other")
      c[i] <- paste0("designFieldbook_fundAgencyType_name_", id_rand_fa[i])
      d[i] <- paste0("designFieldbook_fundAgencyType_cgiar_", id_rand_fa[i])
    }
    aa <- c(a, b, c, d)
    print(aa)
    
    fa <- AllInputs() %>%
      filter(!str_detect(id, "button")) %>%
      filter(!str_detect(id, "-selectized")) 
    
    #c <- a %>% dplyr::filter(id %in% b$inputId)
    faall <- fa %>% dplyr::filter(id %in% aa)
    print(faall)
    resall <- arrange_by_pattern(faall, id_rand_fa)
    print(resall)
    sd <- resall[order(resall[,1]),]
    print(sd)
    write.csv(AllInputs(), paste0(sessionpath, input$uniqueId, ".csv"), row.names = FALSE)
    #aa <- data.frame(id = a, values = , stringsAsFactors = F)
  })
  
  observeEvent(input$savetest2, {
    #write.csv(AllInputs(), paste0(sessionpath, input$uniqueId, ".csv"), row.names = FALSE)
    
    #funcion de omar
    id_rand_fa <- getAddInputId(experimentVars$ids_FA, "FA_", "")
    print(id_rand_fa)
    
    fa <- AllInputs() %>%
      filter(!str_detect(id, "button")) %>%
      filter(!str_detect(id, "-selectized")) 
    
    fatype <- fa %>% filter(str_detect(id,"designFieldbook_fundAgencyType_[:uppercase:]+$"))
    
    
    #designFieldbook_fundAgencyType_JLBSTFLS_other
    
    a <- c()
    for (i in 1:length(id_rand_fa)) {
      a[i] <- paste0("designFieldbook_fundAgencyType_", id_rand_fa[i], "_other")
    }
    print(a)
    
    #aa <- data.frame(id = a, values = , stringsAsFactors = F)
    
    fatypeother <- fa %>% filter(str_detect(id,a))
    print(fatypeother)
    # for (i in 1:length(input$designFieldbook_fundAgencyType)) {
    #   a1[i] <- paste0("fundName_", i)
    #   a2[i] <- "textInput"
    #   a3[i] <- "n"
    # }
    # df2 <- data.frame(inputId = a1, type = a2, create = a3, stringsAsFactors = F)
    
    faname <- fa %>% filter(str_detect(id,"designFieldbook_fundAgencyType_name_[:uppercase:]+$"))
    
    faall <- rbind(fatype, fatypeother, faname)
    resall <- arrange_by_pattern(faall, id_rand_fa)
    
    write.csv(resall, paste0(sessionpath, input$uniqueId, ".csv"), row.names = FALSE)
  })
  
  
  
  
  

  #################### START: PATHS GENERALES ####################
  # path global para lectura de RDS's
  globalpath <- "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/www/internal_files/"
  
  # path del template para new fieldbook
  templatepath <- "/home/obenites/AGROFIMS/template/"
  #################### END: PATHS GENERALES ####################
  
  ##################################
  ##################################
  
  #################### START: GUARDAR KDSmart FIELDBOOK ####################
  
  # path para guardar los fieldbooks en formato KDSmart
  kdsmartpath <- "/home/obenites/AGROFIMS/kdsmart/"
  
  
  # Simular el fielbook
  
  fbtest <- iris
  
  savefb <- function() {
    write.csv(fbtest, file = paste0(kdsmartpath, input$uniqueId, ".csv"), row.names = F)
  }
  
  checkDS <- function() {
    
  }
  
  savefbDB <- function() {
    statusfb <- "subido"
    
    mydb = dbConnect(MySQL(), user='agrofims', password='cnReOdGjS851TTR140318', dbname='agrofims', host='176.34.248.121')
    
    query <- paste0("INSERT INTO `kdsmart`(`uniqueId`, `experimentId`, `fieldbookId`, `user`, `registered`, `modified`, `status`) VALUES ('",
                    input$uniqueId,"','",
                    input$experimentId,"','",
                    input$fieldbookId,"','",
                    session$userData$userMail,"','",
                    Sys.Date(),"','",
                    Sys.Date(),"','",
                    statusfb,"')")
    
    #print(query)
    dbSendQuery(mydb, query)
  }
  
  observeEvent(input$sendKDSmart, {
    
    savefb()
    checkDS()
    savefbDB()
    
  })
  
  # output$sendKDSmart <- downloadHandler(
  #   
  #   #savefb(),
  #   write.csv(fbtest, file = paste0(kdsmartpath, input$uniqueId, ".csv"), row.names = F),
  #   filename = function() {
  #     paste("fileNameBook.csv", sep="")
  #   },
  #   content = function(file) {
  #     write.csv(fbtest, file)
  #     # mydb = dbConnect(MySQL(), user='agrofims', password='cnReOdGjS851TTR140318', dbname='agrofims', host='176.34.248.121')
  #     # query <- sprintf("INSERT INTO kdsmart (id,user,registered,modified,status) VALUES(\'XADFE\',\'ciro\',\'%s\',\'\',\'Uploaded\') ON DUPLICATE KEY UPDATE modified=\'%s\'",Sys.Date(),Sys.Date())
  #     # dbSendQuery(mydb, query)
  #     # print(dbListTables(mydb))
  #     
  #   }
  # )
  
  #################### END: GUARDAR KDSmart FIELDBOOK ####################
  
  ##################################
  ##################################
  
  #################### START: GUARDAR SESION DEL FIELDBOOK ####################
 
  # GLOBAL PATH donde se aloja las sessiones y backups
  sessionpath <- "/home/obenites/AGROFIMS/savesession/"
  sessionpathbk <- "/home/obenites/AGROFIMS/savesession_bk/"
   
    #Funcion que crea lista de inputs a guardar: Experiment
    inputsExperiment <- function() {
      a1 <- a2 <- a3 <- a4 <- a5 <- a6 <- a7 <- a8 <- a9 <- a10 <- a11 <- a12 <- c()
      b1 <- b2 <- b3 <- b4 <- b5 <- b6 <- b7 <- b8 <- b9 <- b10 <- b11 <- b12 <- c()
      c1 <- c2 <- c3 <- c4 <- c5 <- c6 <- c7 <- c8 <- c9 <- c10 <- c11 <- c12 <- c13 <- c14 <- c15 <- c16 <- c17 <- c18 <- c()

      inputRds <- readRDS(paste0(globalpath, "inputId1_v4.rds"))
      inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Experiment")
      df1 <- inputRds[c(4, 5, 6)]
      
      # inputs para: Funding Agency
      id_rand_fa <- getAddInputId(experimentVars$ids_FA, "FA_", "")
      
      for (i in 1:length(id_rand_fa)) {
        a1[i] <- paste0("designFieldbook_fundAgencyType_", id_rand_fa[i])
        a2[i] <- paste0("designFieldbook_fundAgencyType_", id_rand_fa[i], "_other")
        a3[i] <- paste0("designFieldbook_fundAgencyType_name_", id_rand_fa[i])
        a4[i] <- paste0("designFieldbook_fundAgencyType_cgiar_", id_rand_fa[i])
        
        a5[i] <- "selectizeInput"
        a6[i] <- "textInput"
        a7[i] <- "textInput"
        a8[i] <- "selectizeInput"
        
        a9[i] <- "n"
        a10[i] <- "n"
        a11[i] <- "n"
        a12[i] <- "n"
      }
      
      df2 <- data.frame(inputId = c(a1, a2, a3, a4),
                        type = c(a5, a6, a7, a8),
                        create = c(a9, a10, a11, a12),
                        stringsAsFactors = F)
      
      # inputs para: Project Management Entities
      id_rand_pe <- getAddInputId(experimentVars$ids_PE, "PE_", "")
      
      for (i in 1:length(id_rand_pe)) {
        b1[i] <- paste0("projEntity_", id_rand_pe[i])
        b2[i] <- paste0("projEntity_", id_rand_pe[i], "_other")
        b3[i] <- paste0("contCenter_", id_rand_pe[i])
        b4[i] <- paste0("contCRP_", id_rand_pe[i])
        
        b5[i] <- "selectizeInput"
        b6[i] <- "textInput"
        b7[i] <- "selectizeInput"
        b8[i] <- "selectizeInput"
        
        b9[i] <- "n"
        b10[i] <- "n"
        b11[i] <- "n"
        b12[i] <- "n"
      }
      
      df3 <- data.frame(inputId = c(b1, b2, b3, b4),
                        type = c(b5, b6, b7, b8),
                        create = c(b9, b10, b11, b12),
                        stringsAsFactors = F)
      
      # inputs para: Experiment Leads
      id_rand_el <- getAddInputId(experimentVars$ids_EL, "EL_", "")
      
      for (i in 1:length(id_rand_el)) {
        c1[i] <- paste0("projLeadEnt_", id_rand_el[i])
        c2[i] <- paste0("tLeadCenter_", id_rand_el[i])
        c3[i] <- paste0("lead_org_type_1_", id_rand_el[i])
        c4[i] <- paste0("lead_org_type_1_", id_rand_el[i], "_other")
        c5[i] <- paste0("leadNameOther_", id_rand_el[i])
        c6[i] <- paste0("expLead_", id_rand_el[i])
        
        c7[i] <- "selectizeInput"
        c8[i] <- "selectizeInput"
        c9[i] <- "selectizeInput"
        c10[i] <- "textInput"
        c11[i] <- "textInput"
        c12[i] <- "textInput"
        
        c13[i] <- "n"
        c14[i] <- "n"
        c15[i] <- "n"
        c16[i] <- "n"
        c17[i] <- "n"
        c18[i] <- "n"
      }
      
      df4 <- data.frame(inputId = c(c1, c2, c3, c4, c5, c6),
                        type = c(c7, c8, c9, c10, c11, c12),
                        create = c(c13, c14, c15, c16, c17, c18),
                        stringsAsFactors = F)
      
      # Union de todos los resultados
      res <- rbind(df1, df2, df3, df4)
      res
    }
    
    # Funcion que crea lista de inputs a guardar: Personnel
    inputsPersonnel <- function() {
      a1 <- a2 <- a3 <- a4 <- a5 <- a6 <- a7 <- a8 <- a9 <- a10 <- a11 <- a12 <- a13 <- a14 <- a15 <- a16 <- a17 <- a18 <- a19 <- a20 <- a21 <- a22 <- a23 <- a24 <- a25 <- a26 <- a27 <- c()
      
      # inputs para: Number of personnel
      id_rand_pers <-  getAddInputId(personnelVars$ids, "PERS_", "")
      
      for (i in 1:id_rand_pers) {
        a1[i] <- paste0("personnel_type_", id_rand_pers[i])
        a2[i] <- paste0("personnel_type_", id_rand_pers[i], "_other")
        a3[i] <- paste0("person_firstName_", id_rand_pers[i])
        a4[i] <- paste0("person_lastName_", id_rand_pers[i])
        a5[i] <- paste0("person_email_", id_rand_pers[i])
        a6[i] <- paste0("person_affiliation_", id_rand_pers[i])
        a7[i] <- paste0("person_center_", id_rand_pers[i])
        a8[i] <- paste0("person_affiliation_", id_rand_pers[i], "_other")
        a9[i] <- paste0("person_orcid_", id_rand_pers[i])
        
        a10[i] <- "selectizeInput"
        a11[i] <- "textInput"
        a12[i] <- "textInput"
        a13[i] <- "textInput"
        a14[i] <- "textInput"
        a15[i] <- "selectizeInput"
        a16[i] <- "selectizeInput"
        a17[i] <- "textInput"
        a18[i] <- "textInput"
        
        a19[i] <- "n"
        a20[i] <- "n"
        a21[i] <- "n"
        a22[i] <- "n"
        a23[i] <- "n"
        a24[i] <- "n"
        a25[i] <- "n"
        a26[i] <- "n"
        a27[i] <- "n"
      }
      df1 <- data.frame(inputId = c(a1, a2, a3, a4, a5, a6, a7, a8, a9),
                        type = c(a10, a11, a12, a13, a14, a15, a16, a17, a18),
                        create = c(a19, a20, a21, a22, a23, a24, a25, a26, a27),
                        stringsAsFactors = F)
      
      res <- df1
      res
    }
    
    # Funcion que crea lista de inputs a guardar: Site
    inputsSite <- function() {
      inputRds <- readRDS(paste0(globalpath, "inputId1_v3.rds"))
      inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Site")
      df1 <- inputRds[c(4, 5, 6)]
      
      res <- df1
      res
    }
    
    # Funcion que crea lista de inputs a guardar: Crop
    inputsCrop <- function() {
      df2 <- df3 <- data.frame()
      a1 <- a2 <- a3 <- a4 <- a5 <- a6 <- a7 <- a8 <- a9 <- c()
      
      inputRds <- readRDS(paste0(globalpath, "inputId1_v3.rds"))
      inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Crop")
      df1 <- dplyr::filter(inputRds, is.na(category))
      df1 <- df1[c(4, 5, 6)]
      
      # inputs para: Monocrop
      if (!is.null(input$croppingType) && !is.na(input$croppingType) && input$croppingType == "Monocrop") {
        df2 <- dplyr::filter(inputRds, category == "Monocrop")
        df2 <- df2[c(4, 5, 6)]
      }
      
      # inputs para: Intercrop
      id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
      
      if (!is.null(input$croppingType) && !is.na(input$croppingType) && input$croppingType == "Intercrop") {
        for (i in 1:length(id_ic_rand)) {
          a1[i] <- paste0("int_cropCommonName_", id_ic_rand[i])
          a2[i] <- paste0("int_cropCommonName_", id_ic_rand[i], "_other", i)
          a3[i] <- paste0("cropVarietyName_", id_ic_rand[i])
          
          a4[i] <- "selectizeInput"
          a5[i] <- "textInput"
          a6[i] <- "selectizeInput"
          
          a7[i] <- "n"
          a8[i] <- "n"
          a9[i] <- "y"
        }
        df3 <- data.frame(inputId = c(a1, a2, a3), type = c(a4, a5, a6), create = c(a7, a8, a9), stringsAsFactors = F)
      }
      
      # inputs para: Relay crop
      id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
      
      if (!is.null(input$croppingType) && !is.na(input$croppingType) && input$croppingType == "Intercrop") {
        for (i in 1:length(id_re_rand)) {
          b1[i] <- paste0("rel_cropCommonName_", id_re_rand[i])
          b2[i] <- paste0("rel_cropCommonName_", id_re_rand[i], "_other", i)
          b3[i] <- paste0("cropVarietyName_", id_re_rand[i])
          
          b4[i] <- "selectizeInput"
          b5[i] <- "textInput"
          b6[i] <- "selectizeInput"
          
          b7[i] <- "n"
          b8[i] <- "n"
          b9[i] <- "y"
        }
        df4 <- data.frame(inputId = c(b1, b2, b3), type = c(b4, b5, b6), create = c(b7, b8, b9), stringsAsFactors = F)
      }
      
      res <- rbind(df1, df2, df3, df4)
      res
    }
    
    # Funcion que crea lista de inputs a guardar: Design
    inputsDesign <- function() {
      df2 <- df3 <- df4 <- data.frame()
      
      inputRds <- readRDS(paste0(globalpath, "inputId1_v3.rds"))
      inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Design")
      df1 <- dplyr::filter(inputRds, is.na(category))
      df1 <- df1[c(4, 5, 6)]
      
      # inputs para: Information on experimental unit -> plot
      if (!is.null(input$info_experiment_unit) && !is.na(input$info_experiment_unit) && input$info_experiment_unit == "plot") {
        df2 <- dplyr::filter(inputRds, category == "plot")
        df2 <- df2[c(4, 5, 6)]
      }
      
      # inputs para: Information on experimental unit -> field
      if (!is.null(input$info_experiment_unit) && !is.na(input$info_experiment_unit) && input$info_experiment_unit == "field") {
        df3 <- dplyr::filter(inputRds, category == "field")
        df3 <- df3[c(4, 5, 6)]
      }
      
      # inputs para: Information on experimental unit -> pot
      if (!is.null(input$info_experiment_unit) && !is.na(input$info_experiment_unit) && input$info_experiment_unit == "pot") {
        df4 <- dplyr::filter(inputRds, category == "pot")
        df4 <- df4[c(4, 5, 6)]
      }
      
      res <- rbind(df1, df2, df3, df4)
      res
    }
    
    # Funcion que crea lista de inputs a guardar: Experiment conditions
    inputsExpCon <- function() {
      df2 <- df3 <- df4 <- df5 <- df6 <- df7 <- df8 <- data.frame()
      
      inputRds <- readRDS(paste0(globalpath, "inputId2_v1.rds"))
      inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Experiment conditions" & is.na(subTabPanel))
      df1 <- inputRds[c(6, 7, 8)]
      
      inp <- c()
      z <- a <- b <- data.frame()
      
      if (is.null(input$selectAgroFeature) || is.na(input$selectAgroFeature)) {
        inp <- ""
      } else {
        inp <- input$selectAgroFeature
      }
      
      for (i in 1:length(inp)) {
        if (inp[i] == "Harvest") {
          df2 <- inputsExpConHarvest()
        }
        
        if (inp[i] == "Irrigation") {
          df3 <- inputsExpConIrrigation()
        }
        
        if (inp[i] == "Land preparation") {
          df4 <- inputsExpConLanPreparation()
        }
        
        if (inp[i] == "Mulching and residue") {
          df5 <- inputsExpConMulchResi()
        }
        
        if (inp[i] == "Planting and transplanting") {
          df6 <- inputsExpPlantTrans()
        }
        
        if (inp[i] == "Soil fertility") {
          df7 <- inputsExpConSoilFer()
        }
        
        if (inp[i] == "Weeding") {
          df8 <- inputsExpConWeeding()
        }
      }
      
      res <- rbind(df1, df2, df3, df4, df5, df6, df7, df8)
      res
    }
    
    # Funcion que crea lista de inputs a guardar: Experiment conditions -> Harvest
    inputsExpConHarvest <- function() {
      inputRds <- readRDS(paste0(globalpath, "inputId2_v1.rds"))
      inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Experiment conditions" & subTabPanel == "Harvest")
      df1 <- inputRds[c(6, 7, 8)]
      
      res <- df1
    }
    
    # Funcion que crea lista de inputs a guardar: Experiment conditions -> Irrigation
    inputsExpConIrrigation <- function() {
      a1 <- a2 <- a3 <- a4 <- a5 <- a6 <- a7 <- a8 <- a9 <- a10 <- a11 <- a12 <- a13 <- a14 <- a15 <- a16 <- a17 <- a18 <- a19 <- a20 <- a21 <- a22 <- a23 <- a24 <- a25 <- a26 <- a27 <- a28 <- a29 <- a30 <- a31 <- a32 <- a33 <- a34 <- a35 <- a36 <- a37 <- a38 <- a39 <- a40 <- a41 <- a42 <- a43 <- a44 <- a45 <- a46 <- a47 <- a48 <- c()
      df2 <- data.frame()
      
      inputRds <- readRDS(paste0(globalpath, "inputId2_v1.rds"))
      inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Experiment conditions" & subTabPanel == "Irrigation")
      df1 <- inputRds[c(6, 7, 8)]
      
      # inputs para: Number of irrigations
      if (!is.null(input$numApplicationsIrrigation) && !is.na(input$numApplicationsIrrigation) && input$numApplicationsIrrigation >= 3) {
        for (i in 1:input$numApplicationsIrrigation) {
          a1[i] <- paste0("irid_irrigationevent_start_date_", i)
          a2[i] <- paste0("irid_irrigationevent_end_date_", i)
          a3[i] <- paste0("irid_irrigation_technique_", i)
          a4[i] <- paste0("irid_irrigation_technique_", i, "_other")
          a5[i] <- paste0("irid_irrigation_using_sprinkler_systems_", i)
          a6[i] <- paste0("irid_irrigation_using_sprinkler_systems_", i, "_other")
          a7[i] <- paste0("irid_localized_irrigation_technique", i)
          a8[i] <- paste0("irid_localized_irrigation_technique", i, "_other")
          a9[i] <- paste0("irid_surface_irrigation_technique_", i)
          a10[i] <- paste0("irid_surface_irrigation_technique_", i, "_other")
          a11[i] <- paste0("irid_irrigation_source_", i)
          a12[i] <- paste0("irid_irrigation_source_distance_", i)
          a13[i] <- paste0("irid_irrigation_source_distance_", i, "unit")
          a14[i] <- paste0("irid_irrigation_amount_", i)
          a15[i] <- paste0("irid_irrigation_amount_", i, "unit")
          a16[i] <- paste0("irid_irrigation_notes_", i)
          
          a17[i] <- paste0("dateInput")
          a18[i] <- paste0("dateInput")
          a19[i] <- paste0("selectizeInput")
          a20[i] <- paste0("textInput")
          a21[i] <- paste0("selectizeInput")
          a22[i] <- paste0("textInput")
          a23[i] <- paste0("selectizeInput")
          a24[i] <- paste0("textInput")
          a25[i] <- paste0("selectizeInput")
          a26[i] <- paste0("textInput")
          a27[i] <- paste0("selectizeInput")
          a28[i] <- paste0("numericInput")
          a29[i] <- paste0("selectizeInput")
          a30[i] <- paste0("numericInput")
          a31[i] <- paste0("selectizeInput")
          a32[i] <- paste0("textAreaInput")
          
          a33[i] <- paste0("n")
          a34[i] <- paste0("n")
          a35[i] <- paste0("n")
          a36[i] <- paste0("n")
          a37[i] <- paste0("n")
          a38[i] <- paste0("n")
          a39[i] <- paste0("n")
          a40[i] <- paste0("n")
          a41[i] <- paste0("n")
          a42[i] <- paste0("n")
          a43[i] <- paste0("n")
          a44[i] <- paste0("n")
          a45[i] <- paste0("n")
          a46[i] <- paste0("n")
          a47[i] <- paste0("n")
          a48[i] <- paste0("n")
        }
        df2 <- data.frame(inputId = c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16),
                          type = c(a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32),
                          create = c(a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48), stringsAsFactors = F)
      } else {
        df2 <- NULL
      }
      
      res <- rbind(df1, df2)
      res
    }
    
    # Funcion que crea lista de inputs a guardar: Experiment conditions -> Land preparation
    inputsExpConLanPreparation <- function() {
      inputRds <- readRDS(paste0(globalpath, "inputId2_v1.rds"))
      inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Experiment conditions" & subTabPanel == "Land preparation")
      df1 <- inputRds[c(6, 7, 8)]
      
      res <- df1
      res
    }
    
    # Funcion que crea lista de inputs a guardar: Experiment conditions -> Mulching and residue
    inputsExpConMulchResi <- function() {
      inputRds <- readRDS(paste0(globalpath, "inputId2_v1.rds"))
      inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Experiment conditions" & subTabPanel == "Mulching and residue")
      df1 <- inputRds[c(6, 7, 8)]
      
      res <- df1
      res
    }
    
    # Funcion que crea lista de inputs a guardar: Experiment conditions -> Planting and transplanting
    inputsExpPlantTrans <- function() {
      inputRds <- readRDS(paste0(globalpath, "inputId2_v1.rds"))
      inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Experiment conditions" & subTabPanel == "Planting and transplanting")
      df1 <- inputRds[c(6, 7, 8)]
      
      res <- df1
      res
    }
    
    # Funcion que crea lista de inputs a guardar: Experiment conditions -> Soil fertility
    inputsExpConSoilFer <- function() {
      a1 <- a2 <- a3 <- a4 <- a5 <- a6 <- a7 <- a8 <- a9 <- a10 <- a11 <- a12 <- a13 <- a14 <- a15 <- a16 <- a17 <- a18 <- a19 <- a20 <- a21 <- a22 <- a23 <- a24 <- a25 <- a26 <- a27 <- a28 <- a29 <- a30 <- a31 <- a32 <- a33 <- a34 <- a35 <- a36 <- c()
      df2 <- data.frame()
      
      inputRds <- readRDS(paste0(globalpath, "inputId2_v1.rds"))
      inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Experiment conditions" & subTabPanel == "Soil fertility")
      df1 <- inputRds[c(6, 7, 8)]
      
      # inputs para: Number of applications
      if (!is.null(input$soil_fertilizer_num_apps) && !is.na(input$soil_fertilizer_num_apps) && input$soil_fertilizer_num_apps >= 1) {
        for (i in 1:input$soil_fertilizer_num_apps) {
          a1[i] <- paste0("select_fertilizerType_soil_table_row_", i)
          a2[i] <- paste0("select_product_soil_table_row_", i)
          a3[i] <- paste0("select_product_soil_table_row_", i, "_other")
          a4[i] <- paste0("input_productRate_soil_table_row", i)
          a5[i] <- paste0("select_element_soil_table_row_", i)
          a6[i] <- paste0("select_element_soil_table_row_", i, "_other")
          a7[i] <- paste0("input_elementRate_soil_table_row_", i)
          a8[i] <- paste0("input_startdate_soil_table_row_", i)
          a9[i] <- paste0("input_enddate_soil_table_row_", i )
          a10[i] <- paste0("select_techinque_soil_table_row_", i)
          a11[i] <- paste0("select_techinque_soil_table_row_", i, "_other")
          a12[i] <- paste0("textArea_soil_table_row_", i)
          
          a13[i] <- paste0("selectizeInput")
          a14[i] <- paste0("selectizeInput")
          a15[i] <- paste0("textInput")
          a16[i] <- paste0("textInput")
          a17[i] <- paste0("selectizeInput")
          a18[i] <- paste0("textInput")
          a19[i] <- paste0("textInput")
          a20[i] <- paste0("dateInput")
          a21[i] <- paste0("dateInput")
          a22[i] <- paste0("selectizeInput")
          a23[i] <- paste0("textInput")
          a24[i] <- paste0("textAreaInput")
          
          a25[i] <- paste0("n")
          a26[i] <- paste0("n")
          a27[i] <- paste0("n")
          a28[i] <- paste0("n")
          a29[i] <- paste0("n")
          a30[i] <- paste0("n")
          a31[i] <- paste0("n")
          a32[i] <- paste0("n")
          a33[i] <- paste0("n")
          a34[i] <- paste0("n")
          a35[i] <- paste0("n")
          a36[i] <- paste0("n")
        }
        df2 <- data.frame(inputId = c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12),
                          type = c(a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24),
                          create = c(a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36), stringsAsFactors = F)
      } else {
        df2 <- NULL
      }
      
      res <- rbind(df1, df2)
      res
    }
    
    # Funcion que crea lista de inputs a guardar: Experiment conditions -> Weeding
    inputsExpConWeeding <- function() {
      a1 <- a2 <- a3 <- a4 <- a5 <- a6 <- a7 <- a8 <- a9 <- a10 <- a11 <- a12 <- a13 <- a14 <- a15 <- a16 <- a17 <- a18 <- a19 <- a20 <- a21 <- c()
      df2 <- data.frame()
      
      inputRds <- readRDS(paste0(globalpath, "inputId2_v1.rds"))
      inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Experiment conditions" & subTabPanel == "Weeding")
      df1 <- inputRds[c(6, 7, 8)]
      
      # inputs para: Number of weedings
      if (!is.null(input$numWeeding) && !is.na(input$numWeeding) && input$numWeeding >= 1) {
        for (i in 1:input$numWeeding) {
          a1[i] <- paste0("wewd_weeding_start_date_", i)
          a2[i] <- paste0("wewd_weeding_end_date_", i)
          a3[i] <- paste0("wewd_weeding_technique_", i)
          a4[i] <- paste0("wewd_weeding_type_", i)
          a5[i] <- paste0("wewd_weeding_type_", i, "_other")
          a6[i] <- paste0("wewd_weeding_traction_", i)
          a7[i] <- paste0("wewd_weeding_traction_", i, "_other")
          
          a8[i] <- paste0("dateInput")
          a9[i] <- paste0("dateInput")
          a10[i] <- paste0("selectizeInput")
          a11[i] <- paste0("selectizeInput")
          a12[i] <- paste0("textInput")
          a13[i] <- paste0("selectizeInput")
          a14[i] <- paste0("textInput")
          
          a15[i] <- paste0("n")
          a16[i] <- paste0("n")
          a17[i] <- paste0("n")
          a18[i] <- paste0("n")
          a19[i] <- paste0("n")
          a20[i] <- paste0("n")
          a21[i] <- paste0("n")
        }
        df2 <- data.frame(inputId = c(a1, a2, a3, a4, a5, a6, a7),
                          type = c(a8, a9, a10, a11, a12, a13, a14),
                          create = c(a15, a16, a17, a18, a19, a20, a21), stringsAsFactors = F)
      } else {
        df2 <- NULL
      }
      
      res <- rbind(df1, df2)
      res
    }
    
    # Boton para hacer test
    # observeEvent(input$xtest, {
    #   print(inputsExpConHarvest())
    # })
    
    # Funcion que guarda la session del usuario [old]
    # savesession <- function() {
    #   if(session$userData$logged){
    #     expid <- input$uniqueId
    #     
    #     if (file.exists(isolate(paste0(sessionpath, expid, ".csv")))) {
    #       x <- read.csv(paste0(sessionpath, expid, ".csv"))
    #       datecreate <- as.character(x[2, 4])
    #       datemodified <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
    #     } else {
    #       datecreate <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
    #       datemodified <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
    #     }
    #     
    #     inputs1 <- inputs2 <- inputs3 <- NULL
    #     
    #     inputs_to_save <- rbind(inputsExperiment())#,
    #                             #inputsPersonnel(),
    #                             #inputsSite(),
    #                             #inputsCrop(),
    #                             #inputsDesign(),
    #                             #inputsExpCon())
    #     
    #     case1p <- dplyr::filter(inputs_to_save, type == "textInput" |
    #                               type == "numericInput" |
    #                               type == "textAreaInput" |
    #                               type == "checkboxInput" |
    #                               type == "dateInput")
    #     case1 <- case1p[[1]]
    #     case1_type <- case1p[[2]]
    #     case1_create <- case1p[[3]]
    #     
    #     case2p <- dplyr::filter(inputs_to_save, type == "dateRangeInput")
    #     case2 <- case2p[[1]]
    #     case2_type <- case2p[[2]]
    #     case2_create <- case2p[[3]]
    #     
    #     case3p <- dplyr::filter(inputs_to_save, type == "selectizeInput" | type == "selectInput")
    #     case3 <- case3p[[1]]
    #     case3_type <- case3p[[2]]
    #     case3_create <- case3p[[3]]
    #     
    #     for (i in 1:length(case1)) {
    #       # textInput && numericInput && textAreaInput && checkboxInput && dateinput
    #       if (is.null(input[[paste0(case1[i])]]) || is.na(input[[paste0(case1[i])]])) {
    #         inputs1[i] <- ""
    #       } else {
    #         inputs1[i] <- as.character(input[[paste0(case1[i])]])
    #       }
    #     }
    #     inputs_data_frame1 <- data.frame(inputId = case1, type = case1_type, create = case1_create, value = inputs1)
    #     
    #     # for (i in 1:length(case2)) {
    #     #   # dateRangeInput
    #     #   if (is.null(input[[paste0(case2[i])]]) || is.na( input[[paste0(case2[i])]])) {
    #     #     inputs2[i] <- ""
    #     #   } else {
    #     #     inputs2[i] <- paste(input[[paste0(case2[i])]], collapse = "&")
    #     #   }
    #     # }
    #     # inputs_data_frame2 <- data.frame(inputId = case2, type = case2_type, create = case2_create, value = inputs2)
    #     
    #     for (i in 1:length(case3)) {
    #       # selectizeInput && selectInput
    #       if (is.null(input[[paste0(case3[i])]]) || is.na( input[[paste0(case3[i])]])) {
    #         inputs3[i] <- ""
    #       } else {
    #         inputs3[i] <- paste(input[[paste0(case3[i])]], collapse = "&")
    #       }
    #     }
    #     inputs_data_frame3 <- data.frame(inputId = case3, type = case3_type, create = case3_create, value = inputs3)
    #     
    #     #inputs_data_frame <- rbind(inputs_data_frame1, inputs_data_frame2, inputs_data_frame3)
    #     inputs_data_frame <- rbind(inputs_data_frame1, inputs_data_frame3)
    #     nr <- data.frame(inputId = "user", type = "", create = "", value = session$userData$userMail)
    #     nr2 <- data.frame(inputId = "datec", type = "", create = "", value = datecreate)
    #     nr3 <- data.frame(inputId = "datem", type = "", create = "", value = datemodified)
    #     final_inputs_df <- rbind(nr, nr2, nr3, inputs_data_frame)
    #     
    #     write.csv(final_inputs_df, paste0(sessionpath, input$uniqueId, ".csv"), row.names = FALSE)
    #     write.csv(final_inputs_df, paste0(sessionpathbk, input$uniqueId, ".csv"), row.names = FALSE)
    #     
    #     updateTextInput(session,
    #                     inputId = "uniqueId",
    #                     value = "")
    #     updateTextInput(session,
    #                     inputId = "uniqueId",
    #                     value = expid)
    #     
    #     shinyalert("Saved successfully", type = "success", timer = 1500, showConfirmButton = F)
    #   } else {
    #     shinyalert("Sorry", "You must login to save avance", type = "info", timer = 1500, showConfirmButton = F)
    #   }
    # }
    
    # dinamicInputs <- function() {
    #   a <- b <- c <- d <- c()
    #   
    #   id_rand_fa <- getAddInputId(experimentVars$ids_FA, "FA_", "")
    #   print(id_rand_fa)
    #   
    #   for (i in 1:length(id_rand_fa)) {
    #     a[i] <- paste0("designFieldbook_fundAgencyType_", id_rand_fa[i])
    #     b[i] <- paste0("designFieldbook_fundAgencyType_", id_rand_fa[i], "_other")
    #     # c[i] <- paste0("designFieldbook_fundAgencyType_name_", id_rand_fa[i])
    #     # d[i] <- paste0("designFieldbook_fundAgencyType_cgiar_", id_rand_fa[i])
    #   }
    #   
    #   #z <- data.frame(id = c(a, b, c, d), values = "", stringsAsFactors = F)
    #   z <- data.frame(id = c(a, b), values = "", stringsAsFactors = F)
    #   #print(z)
    #   
    #   zz <- AllInputs() %>% dplyr::filter(id %in% z$id)
    #   #print(zz)
    #   
    #   resall <- arrange_by_pattern(zz, id_rand_fa)
    #   print(resall)
    # }
    
    # Funcion que guarda la session del usuario
    savesession <- function() {
      if(session$userData$logged){
        expid <- input$uniqueId

        if (file.exists(isolate(paste0(sessionpath, expid, ".csv")))) {
          x <- read.csv(paste0(sessionpath, expid, ".csv"))
          datecreate <- as.character(x[2, 4])
          datemodified <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
        } else {
          datecreate <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
          datemodified <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
        }

        inputs1 <- inputs2 <- inputs3 <- NULL

        inputs_to_save <- rbind(inputsExperiment())#,
                                #inputsPersonnel(),
                                #inputsSite(),
                                #inputsCrop(),
                                #inputsDesign(),
                                #inputsExpCon())

        case1p <- dplyr::filter(inputs_to_save, type == "textInput" |
                                  type == "numericInput" |
                                  type == "textAreaInput" |
                                  type == "checkboxInput" |
                                  type == "dateInput")
        case1 <- case1p[[1]]
        case1_type <- case1p[[2]]
        case1_create <- case1p[[3]]

        case2p <- dplyr::filter(inputs_to_save, type == "dateRangeInput")
        case2 <- case2p[[1]]
        case2_type <- case2p[[2]]
        case2_create <- case2p[[3]]

        case3p <- dplyr::filter(inputs_to_save, type == "selectizeInput" | type == "selectInput")
        case3 <- case3p[[1]]
        case3_type <- case3p[[2]]
        case3_create <- case3p[[3]]

        for (i in 1:length(case1)) {
          # textInput && numericInput && textAreaInput && checkboxInput && dateinput
          if (is.null(input[[paste0(case1[i])]]) || is.na(input[[paste0(case1[i])]])) {
            inputs1[i] <- ""
          } else {
            inputs1[i] <- as.character(input[[paste0(case1[i])]])
          }
        }
        inputs_data_frame1 <- data.frame(inputId = case1, type = case1_type, create = case1_create, value = inputs1)

        # for (i in 1:length(case2)) {
        #   # dateRangeInput
        #   if (is.null(input[[paste0(case2[i])]]) || is.na( input[[paste0(case2[i])]])) {
        #     inputs2[i] <- ""
        #   } else {
        #     inputs2[i] <- paste(input[[paste0(case2[i])]], collapse = "&")
        #   }
        # }
        # inputs_data_frame2 <- data.frame(inputId = case2, type = case2_type, create = case2_create, value = inputs2)

        for (i in 1:length(case3)) {
          # selectizeInput && selectInput
          if (is.null(input[[paste0(case3[i])]]) || is.na( input[[paste0(case3[i])]])) {
            inputs3[i] <- ""
          } else {
            inputs3[i] <- paste(input[[paste0(case3[i])]], collapse = "&")
          }
        }
        inputs_data_frame3 <- data.frame(inputId = case3, type = case3_type, create = case3_create, value = inputs3)

        #inputs_data_frame <- rbind(inputs_data_frame1, inputs_data_frame2, inputs_data_frame3)
        inputs_data_frame <- rbind(inputs_data_frame1, inputs_data_frame3)
        nr <- data.frame(inputId = "user", type = "", create = "", value = session$userData$userMail)
        nr2 <- data.frame(inputId = "datec", type = "", create = "", value = datecreate)
        nr3 <- data.frame(inputId = "datem", type = "", create = "", value = datemodified)
        final_inputs_df <- rbind(nr, nr2, nr3, inputs_data_frame)

        write.csv(final_inputs_df, paste0(sessionpath, input$uniqueId, ".csv"), row.names = FALSE)
        write.csv(final_inputs_df, paste0(sessionpathbk, input$uniqueId, ".csv"), row.names = FALSE)

        updateTextInput(session,
                        inputId = "uniqueId",
                        value = "")
        updateTextInput(session,
                        inputId = "uniqueId",
                        value = expid)

        shinyalert("Saved successfully", type = "success", timer = 1500, showConfirmButton = F)
      } else {
        shinyalert("Sorry", "You must login to save avance", type = "info", timer = 1500, showConfirmButton = F)
      }
    }
    
    # Save session
    observeEvent(input$savefieldbook, {
      savesession()
    })
    
    # Funcion reactiva que muestra las fechas de modificacion del fieldbook
    timeExp <- reactive({
      expid <- input$uniqueId
      
      if (file.exists(isolate(paste0(sessionpath, expid, ".csv")))) {
        x <- read.csv(paste0(sessionpath, expid, ".csv"))
        datemodified <- as.character(x[3, 4])
        datemodified <- paste0("<font color='#00a65a'>", datemodified, "</font>")
      } else {
        datemodified <- paste0("<font color='red'>never</font>")
      }
      
      datemodified
    })
    
    # Funcion reactiva que muestra el id del fieldbook para generar qr
    # idExp <- reactive({
    #   expid <- input$experimentId
    #   expid
    # })
    
  # Renderiza el mensaje de guardado de sesion
  output$lastsaved <- renderText({
    paste0("Last modified: ", timeExp())
  })
  #################### END: GUARDAR SESION DEL FIELDBOOK ####################
  
  ##################################
  ##################################
  
  #################### START: LOAD FIELDBOOK ####################
  
  # Lista los archivos para crear el DT de sesiones
  my_files2 <- function() {
    lf <- list.files(sessionpath)
    lf
  }
  
  # Crea el DF de sesiones
  getFbDF <- function() {
    df <- data.frame()
    a <- b <- c <- d <- e <- f <- g <- c()
    
    if (length(my_files2()) >= 1) {
      for (i in 1:length(my_files2())) {
        # Unique ID
        mf <- my_files2()[i]
        mf <- unlist(strsplit(mf, "[.]"))
        a[i] <- mf[1]
        
        fl <- read.csv(paste0(sessionpath, my_files2()[i]))
        
        # Date modified
        f[i] <- as.character(fl[3, 4])
        
        # User
        g[i] <- as.character(fl[1, 4])
      }
      
      userM <- session$userData$userMail
      
      df <- data.frame(a, f, g, stringsAsFactors = F)
      df <- dplyr::filter(as.data.frame(df), g == userM)
      df <- df %>% dplyr::arrange(desc(f))
      df
    } else {
      df <- data.frame()
    }
  }
  
  # Extrae el ID del libro para devolver el ID 
  getFbId <- function() {
    rowDT <- input$dtsession_rows_selected
    
    id <- getFbDF()
    id <- id[rowDT, 1]
  }
  
  # Verifica previo a remover los dinamicos
  beforeRemoveDinCheck <- function() {
    
    print("Before remove dinamics")
    
    ## Experiment details
    
    # Funding Agency
    id_rand_fa <- getAddInputId(experimentVars$ids_FA, "FA_", "")
    nfa <- length(id_rand_fa)
    print(paste0("before: ", nfa, " - Funding Agency"))
    
    # Project Management Entities
    id_rand_pe <- getAddInputId(experimentVars$ids_PE, "PE_", "")
    npe <- length(id_rand_pe)
    print(paste0("before: ", npe, " - Project Management Entities"))
    
    # Experiment Leads
    id_rand_el <- getAddInputId(experimentVars$ids_EL, "EL_", "")
    nel <- length(id_rand_el)
    print(paste0("before: ", nel, " - Funding Agency"))
  }
  
  # Remueve los dinamicos para dejarlos por defecto en 1
  removeDin <- function() {
    beforeRemoveDinCheck()
    
    for (i in 1:3) {
      if (i == 1) {
        # Remueve dinamicos: Funding Agency
        id_rand_fa <- getAddInputId(experimentVars$ids_FA, "FA_", "")
        nfa <- length(id_rand_fa)
        
        if (nfa > 1) {
          for (i in 1:nfa-1) {
            Sys.sleep(0.1)
            shinyjs::click(paste0("closeBox_FA_", id_rand_fa[i]))
          }
        }
      }
      
      if (i == 2) {
        # Remueve dinamicos: Project Management Entities
        id_rand_pe <- getAddInputId(experimentVars$ids_PE, "PE_", "")
        npe <- length(id_rand_pe)
        
        if (npe > 1) {
          for (i in 1:npe-1) {
            Sys.sleep(0.1)
            shinyjs::click(paste0("closeBox_PE_", id_rand_pe[i]))
          }
        }
      }
      
      if (i == 3) {
        # Remueve dinamicos: Experiment Leads
        id_rand_el <- getAddInputId(experimentVars$ids_EL, "EL_", "")
        nel <- length(id_rand_el)
        
        if (nel > 1) {
          for (i in 1:nel-1) {
            Sys.sleep(0.1)
            shinyjs::click(paste0("closeBox_EL_", id_rand_el[i]))
          }
        }
      }
    }
    
  }
  
  # Verifica los removidos si estan en 1
  afterRemoveDinCheck <- function() {
    
    print("After remove dinamics")
    
    ## Experiment details
    
    # Funding Agency
    id_rand_fa <- getAddInputId(experimentVars$ids_FA, "FA_", "")
    nfa <- length(id_rand_fa)
    print(paste0("after: ", nfa, " - Funding Agency"))
    
    # Project Management Entities
    id_rand_pe <- getAddInputId(experimentVars$ids_PE, "PE_", "")
    npe <- length(id_rand_pe)
    print(paste0("after: ", npe, " - Project Management Entities"))
    
    # Experiment Leads
    id_rand_el <- getAddInputId(experimentVars$ids_EL, "EL_", "")
    nel <- length(id_rand_el)
    print(paste0("after: ", nel, " - Funding Agency"))
    
    if (nfa == 1 && npe == 1 && nel == 1) {
      print("Successful removal")
    } else {
      print("Remove failed")
    }
  }
  
  # Genera los dinamicos de acuerdo al fieldbook
  generateDin <- function() {
    afterRemoveDinCheck()
    
    df <- read.csv(paste0(sessionpath, getFbId(), ".csv"))
    
    for (i in 1:3) {
      if (i == 1) {
        # Genera dinamicos: Funding Agency
        df_fa <- df %>% dplyr::filter(str_detect(inputId, "designFieldbook_fundAgencyType_"))
        nfa <- nrow(df_fa)
        nfatest <- nfa/4
        print(nfatest)
        nfa <- (nfa/4)-1
        
        if (nfa >= 1) {
          for (i in 1:nfa) {
            Sys.sleep(0.1)
            shinyjs::click("addFundingAgency")
          }
        }
      }
      
      if (i == 2) {
        # Genera dinamicos: Project Management Entities
        df_pe <- df %>% dplyr::filter(str_detect(inputId, "projEntity_|contCenter_|contCRP_"))
        npe <- nrow(df_pe)
        npetest <- npe/4
        print(npetest)
        npe <- (npe/4)-1
        
        if (npe >= 1) {
          for (i in 1:npe) {
            Sys.sleep(0.1)
            shinyjs::click("addManagEntity")
          }
        }
      }
      
      if (i == 3) {
        # Genera dinamicos: Experiment Leads
        df_el <- df %>% dplyr::filter(str_detect(inputId, "projLeadEnt_|tLeadCenter_|lead_org_type_1_|leadNameOther_|expLead_"))
        nel <- nrow(df_el)
        neltest <- nel/6
        print(neltest)
        nel <- (nel/6)-1
        
        if (nel >= 1) {
          for (i in 1:nel) {
            Sys.sleep(0.1)
            shinyjs::click("addExperimentLeads")
          }
        }
      }
    }
  }
  
  # Valida los generados
  afterGenerateDinCheck <- function() {
    
    print("After generate dinamics")
    
    df <- read.csv(paste0(sessionpath, getFbId(), ".csv"))

    ## Experiment details

    # Funding Agency
    df_fa <- df %>% dplyr::filter(str_detect(inputId, "designFieldbook_fundAgencyType_"))
    nfa <- nrow(df_fa)
    nfaold <- nfa/4
    
    id_rand_fa <- getAddInputId(experimentVars$ids_FA, "FA_", "")
    nfanew <- length(id_rand_fa)
    print(paste0("after gen: ", nfanew, " - Funding Agency"))

    # Project Management Entities
    df_pe <- df %>% dplyr::filter(str_detect(inputId, "projEntity_|contCenter_|contCRP_"))
    npe <- nrow(df_pe)
    npeold <- npe/4
    
    id_rand_pe <- getAddInputId(experimentVars$ids_PE, "PE_", "")
    npenew <- length(id_rand_pe)
    print(paste0("after gen: ", npenew, " - Project Management Entities"))

    # Experiment Leads
    df_el <- df %>% dplyr::filter(str_detect(inputId, "projLeadEnt_|tLeadCenter_|lead_org_type_1_|leadNameOther_|expLead_"))
    nel <- nrow(df_el)
    nelold <- nel/6
    
    id_rand_el <- getAddInputId(experimentVars$ids_EL, "EL_", "")
    nelnew <- length(id_rand_el)
    print(paste0("after gen: ", nelnew, " - Funding Agency"))

    if (nfanew == nfaold && npenew == npeold && nelnew == nelold) {
      print("Successful gererate")
    } else {
      print("Generate failed")
    }
  }
  
  dftest <- function() {
    # Sys.sleep(10)
    # tutu()
    df_old <- read.csv(paste0(sessionpath, getFbId(), ".csv"))
    print(df_old)
    
    
    
    ######
    inputs1 <- inputs2 <- inputs3 <- NULL
    inputs_to_save <- rbind(inputsExperiment())#,
    #inputsPersonnel(),
    #inputsSite(),
    #inputsCrop(),
    #inputsDesign(),
    #inputsExpCon())
    
    case1p <- dplyr::filter(inputs_to_save, type == "textInput" |
                              type == "numericInput" |
                              type == "textAreaInput" |
                              type == "checkboxInput" |
                              type == "dateInput")
    case1 <- case1p[[1]]
    case1_type <- case1p[[2]]
    case1_create <- case1p[[3]]
    
    case2p <- dplyr::filter(inputs_to_save, type == "dateRangeInput")
    case2 <- case2p[[1]]
    case2_type <- case2p[[2]]
    case2_create <- case2p[[3]]
    
    case3p <- dplyr::filter(inputs_to_save, type == "selectizeInput" | type == "selectInput")
    case3 <- case3p[[1]]
    case3_type <- case3p[[2]]
    case3_create <- case3p[[3]]
    
    for (i in 1:length(case1)) {
      # textInput && numericInput && textAreaInput && checkboxInput && dateinput
      if (is.null(input[[paste0(case1[i])]]) || is.na(input[[paste0(case1[i])]])) {
        inputs1[i] <- ""
      } else {
        inputs1[i] <- as.character(input[[paste0(case1[i])]])
      }
    }
    inputs_data_frame1 <- data.frame(inputId = case1, type = case1_type, create = case1_create, value = inputs1)
    
    # for (i in 1:length(case2)) {
    #   # dateRangeInput
    #   if (is.null(input[[paste0(case2[i])]]) || is.na( input[[paste0(case2[i])]])) {
    #     inputs2[i] <- ""
    #   } else {
    #     inputs2[i] <- paste(input[[paste0(case2[i])]], collapse = "&")
    #   }
    # }
    # inputs_data_frame2 <- data.frame(inputId = case2, type = case2_type, create = case2_create, value = inputs2)
    
    for (i in 1:length(case3)) {
      # selectizeInput && selectInput
      if (is.null(input[[paste0(case3[i])]]) || is.na( input[[paste0(case3[i])]])) {
        inputs3[i] <- ""
      } else {
        inputs3[i] <- paste(input[[paste0(case3[i])]], collapse = "&")
      }
    }
    inputs_data_frame3 <- data.frame(inputId = case3, type = case3_type, create = case3_create, value = inputs3)
    
    #inputs_data_frame <- rbind(inputs_data_frame1, inputs_data_frame2, inputs_data_frame3)
    inputs_data_frame <- rbind(inputs_data_frame1, inputs_data_frame3)
    ######
    
    nr <- data.frame(inputId = "user", type = "", create = "", value = "")
    nr2 <- data.frame(inputId = "datec", type = "", create = "", value = "")
    nr3 <- data.frame(inputId = "datem", type = "", create = "", value = "")
    final_inputs_df <- rbind(nr, nr2, nr3, inputs_data_frame)
    print(final_inputs_df)
    
    if (nrow(df_old) == nrow(final_inputs_df)) {
      final_inputs_df$value <- df_old$value
      print(final_inputs_df)
    }
    
    
  }
  
  # Funcion load session
  loadsession <- function() {
    if (length(getFbId() != 0)) {
      if (file.exists(isolate(paste0(sessionpath, getFbId(), ".csv")))){
        
        afterGenerateDinCheck()
        
        df_old <- read.csv(paste0(sessionpath, getFbId(), ".csv"))
        
        #uploaded_inputs <- read.csv(paste0(sessionpath, getFbId(), ".csv"))
        uploaded_inputs <- dftest()
        print(paste0("old ", nrow(df_old)))
        print(paste0("new ", nrow(uploaded_inputs)))
        
        if (length(uploaded_inputs) >= 1) {
          if (nrow(df_old) == nrow(uploaded_inputs)) {
            
            #print(uploaded_inputs)
            for(i in 1:nrow(uploaded_inputs)) {
              type <- as.character(uploaded_inputs[i, 2])
              create <- as.character(uploaded_inputs[i, 3])
              
              if (type == "textInput") {
                updateTextInput(session,
                                inputId = uploaded_inputs$inputId[i],
                                value = uploaded_inputs$value[i])
              }
              
              if (type == "dateRangeInput") {
                if (uploaded_inputs[i, 4] != "") {
                  v <- getInputs(uploaded_inputs[i, 4], "")
                  x <- as.Date(v[1]) + 1
                  y <- as.Date(v[2]) + 1
                  updateAirDateInput(session,
                                     inputId = uploaded_inputs$inputId[i],
                                     value = c(x, y),
                                     clear = T)
                } else {
                  updateAirDateInput(session,
                                     inputId = uploaded_inputs$inputId[i],
                                     clear = T)
                }
              }
              
              if (type == "selectizeInput" && create == "n") {
                updateSelectizeInput(session,
                                     inputId = uploaded_inputs$inputId[i],
                                     selected = getInputs(uploaded_inputs[i, 4], ""))
              }
              
              if (type == "selectizeInput" && create == "y") {
                updateSelectizeInput(session,
                                     inputId = uploaded_inputs$inputId[i],
                                     selected = getInputs(uploaded_inputs[i, 4], ""),
                                     choices = getInputs(uploaded_inputs[i, 4], ""),
                                     options = list('create' = TRUE))
              }
              
              if (type == "textAreaInput") {
                updateTextAreaInput(session,
                                    inputId = uploaded_inputs$inputId[i],
                                    value = uploaded_inputs$value[i])
              }
              
              if (type == "numericInput") {
                updateNumericInput(session,
                                   inputId = uploaded_inputs$inputId[i],
                                   value = uploaded_inputs$value[i])
              }
              
              if (type == "checkboxInput") {
                if (uploaded_inputs$value[i] == "FALSE") {
                  x <- FALSE
                } else {
                  x <- TRUE
                }
                
                updateCheckboxInput(session,
                                    inputId = uploaded_inputs$inputId[i],
                                    value = x)
              }
              
              if (type == "dateInput") {
                if (uploaded_inputs[i, 4] != "") {
                  v <- getInputs(uploaded_inputs[i, 4], "")
                  v <- as.Date(v) + 1
                  updateAirDateInput(session,
                                     inputId = uploaded_inputs$inputId[i],
                                     value = v,
                                     clear = T)
                } else {
                  updateAirDateInput(session,
                                     inputId = uploaded_inputs$inputId[i],
                                     clear = T)
                }
              }
            }
            
            delay(
              1500,
              for(i in 1:nrow(uploaded_inputs)) {
                type <- as.character(uploaded_inputs[i, 2])
                create <- as.character(uploaded_inputs[i, 3])
                
                if (type == "textInput") {
                  updateTextInput(session,
                                  inputId = uploaded_inputs$inputId[i],
                                  value = uploaded_inputs$value[i])
                }
                
                if (type == "dateRangeInput") {
                  if (uploaded_inputs[i, 4] != "") {
                    v <- getInputs(uploaded_inputs[i, 4], "")
                    x <- as.Date(v[1]) + 1
                    y <- as.Date(v[2]) + 1
                    updateAirDateInput(session,
                                       inputId = uploaded_inputs$inputId[i],
                                       value = c(x, y),
                                       clear = T)
                  } else {
                    updateAirDateInput(session,
                                       inputId = uploaded_inputs$inputId[i],
                                       clear = T)
                  }
                }
                
                if (type == "selectizeInput" && create == "n") {
                  updateSelectizeInput(session,
                                       inputId = uploaded_inputs$inputId[i],
                                       selected = getInputs(uploaded_inputs[i, 4], ""))
                }
                
                if (type == "selectizeInput" && create == "y") {
                  updateSelectizeInput(session,
                                       inputId = uploaded_inputs$inputId[i],
                                       selected = getInputs(uploaded_inputs[i, 4], ""),
                                       choices = getInputs(uploaded_inputs[i, 4], ""),
                                       options = list('create' = TRUE))
                }
                
                if (type == "textAreaInput") {
                  updateTextAreaInput(session,
                                      inputId = uploaded_inputs$inputId[i],
                                      value = uploaded_inputs$value[i])
                }
                
                if (type == "numericInput") {
                  updateNumericInput(session,
                                     inputId = uploaded_inputs$inputId[i],
                                     value = uploaded_inputs$value[i])
                }
                
                if (type == "checkboxInput") {
                  if (uploaded_inputs$value[i] == "FALSE") {
                    x <- FALSE
                  } else {
                    x <- TRUE
                  }
                  
                  updateCheckboxInput(session,
                                      inputId = uploaded_inputs$inputId[i],
                                      value = x)
                }
                
                if (type == "dateInput") {
                  if (uploaded_inputs[i, 4] != "") {
                    v <- getInputs(uploaded_inputs[i, 4], "")
                    v <- as.Date(v) + 1
                    updateAirDateInput(session,
                                       inputId = uploaded_inputs$inputId[i],
                                       value = v,
                                       clear = T)
                  } else {
                    updateAirDateInput(session,
                                       inputId = uploaded_inputs$inputId[i],
                                       clear = T)
                  }
                }
              }
            )
            
            #output$text2 <- renderText({"Loaded successfully"})
            shinyalert("Loaded successfully", type = "success", timer = 1500, showConfirmButton = F)
          } else {
            shinyalert("Oops!", "The session load has failed", type = "error", timer = 1500, showConfirmButton = F)
          }
        } else {
          shinyalert("Oops!", "The session load has failed", type = "error", timer = 1500, showConfirmButton = F)
        }
        
      }
      else{
        #output$text <- renderText({"The session file does not exist"})
        shinyalert("Oops!", "The session file does not exist", type = "error", timer = 1500, showConfirmButton = F)
      }
    }
  }
  
  # Funcion que devuelve valores de un array para la funcion Load session
  getInputs<- function(valor, q){
    valor <- sapply(valor, as.character)
    valor[is.na(valor)] <- " "
    valor
    
    if (stringr::str_detect(valor, "&")) {
      if (q == "start") {
        valor <- unlist(strsplit(valor, "&"))
        valor <- valor[[1]]
      }
      
      if (q == "end") {
        valor <- unlist(strsplit(valor, "&"))
        valor <- valor[[2]]
      }
    }
    
    if(stringr::str_detect(valor,"&")){
      valor<-unlist(strsplit(valor, "&"))
    } else {
      valor<-valor
    }
    
    valor
  }
  
  tutu <- function() {
    Sys.sleep(0.1)
    id_rand_fa <- getAddInputId(experimentVars$ids_FA, "FA_", "")
    a <- length(id_rand_fa)
    id_rand_pe <- getAddInputId(experimentVars$ids_PE, "PE_", "")
    b <- length(id_rand_pe)
    id_rand_el <- getAddInputId(experimentVars$ids_EL, "EL_", "")
    c <- length(id_rand_el)
    
    print(id_rand_fa)
    print(id_rand_pe)
    print(id_rand_el)
  }
  
  #Boton load session
  
  observeEvent(input$load_inputNew1, {
    if (session$userData$logged) {
      
      removeDin()
      
      showModal(modalDialog(
        title =
          fluidRow(
            column(
              6,
              style = ("margin-top: -20px;margin-bottom: -10px;"),
              h3("Load fieldbook")
            ),
            column(
              6,
              align = "right",
              style = "margin-top: 0px;"#,
              #actionLink("btncancel2", "X")
            )
          ),
        fluidRow(
          column(
            1,
            icon("exclamation-triangle", "fa-3x")
          ),
          column(
            11,
            "Save changes to fieldbook before closing?",
            br(),
            "Your changes will be lost if you don’t save them."
          )
        ),
        br(),
        fluidRow(
          column(
            12,
            align = "center",
            actionButton('load_inputNew2', 'Next', icon("download"), class = "btn-primary", style="color: #fff;", onclick = "openTab('newFieldbookAgrofims')", width = "100px")#,
            # actionButton("btnsave", "Save", class = "btn-success", style="color: #fff;", width = "95px"),
            # actionButton("btndontsave", "Don't save", width = "95px"),
            # actionButton("btncancel", "Cancel", width = "95px")
          )
        ),
        footer = NULL
      ))
    } else {
      shinyalert("Sorry", "You must login to create new fieldbook", type = "info", timer = 1500, showConfirmButton = F)
    }
  })
  
  observeEvent(input$load_inputNew2, {
    if (session$userData$logged) {
      
      generateDin()
      
      showModal(modalDialog(
        title =
          fluidRow(
            column(
              6,
              style = ("margin-top: -20px;margin-bottom: -10px;"),
              h3("Load fieldbook")
            ),
            column(
              6,
              align = "right",
              style = "margin-top: 0px;"#,
              #actionLink("btncancel2", "X")
            )
          ),
        fluidRow(
          column(
            1,
            icon("exclamation-triangle", "fa-3x")
          ),
          column(
            11,
            "Save changes to fieldbook before closing?",
            br(),
            "Your changes will be lost if you don’t save them."
          )
        ),
        br(),
        fluidRow(
          column(
            12,
            align = "center",
            actionButton('load_inputNew3', 'Finish', icon("download"), class = "btn-primary", style="color: #fff;", onclick = "openTab('newFieldbookAgrofims')", width = "100px")#,
            # actionButton("btnsave", "Save", class = "btn-success", style="color: #fff;", width = "95px"),
            # actionButton("btndontsave", "Don't save", width = "95px"),
            # actionButton("btncancel", "Cancel", width = "95px")
          )
        ),
        footer = NULL
      ))
    } else {
      shinyalert("Sorry", "You must login to create new fieldbook", type = "info", timer = 1500, showConfirmButton = F)
    }
  })
  
  observeEvent(input$load_inputNew3, {
    if (session$userData$logged) {
      
      loadsession()
      removeModal()
      # showModal(modalDialog(
      #   title =
      #     fluidRow(
      #       column(
      #         6,
      #         style = ("margin-top: -20px;margin-bottom: -10px;"),
      #         h3("Load fieldbook")
      #       ),
      #       column(
      #         6,
      #         align = "right",
      #         style = "margin-top: 0px;"#,
      #         #actionLink("btncancel2", "X")
      #       )
      #     ),
      #   fluidRow(
      #     column(
      #       1,
      #       icon("exclamation-triangle", "fa-3x")
      #     ),
      #     column(
      #       11,
      #       "Save changes to fieldbook before closing?",
      #       br(),
      #       "Your changes will be lost if you don’t save them."
      #     )
      #   ),
      #   br(),
      #   fluidRow(
      #     column(
      #       12,
      #       align = "center",
      #       actionButton('ssss', 'close', icon("download"), class = "btn-primary", style="color: #fff;", onclick = "openTab('newFieldbookAgrofims')", width = "100px")#,
      #       # actionButton("btnsave", "Save", class = "btn-success", style="color: #fff;", width = "95px"),
      #       # actionButton("btndontsave", "Don't save", width = "95px"),
      #       # actionButton("btncancel", "Cancel", width = "95px")
      #     )
      #   ),
      #   footer = NULL
      # ))
    } else {
      shinyalert("Sorry", "You must login to create new fieldbook", type = "info", timer = 1500, showConfirmButton = F)
    }
  })
  
  
  
  
  observeEvent(input$load_inputNew11, {
    
    
    # withProgress(message = 'Before remove dinamics', value = 0, {
    #   beforeRemoveDinCheck()
    #   n <- 10
    #   for (i in 1:n) {
    #     incProgress(1/n, detail = paste("Doing part", i))
    #     Sys.sleep(0.1)
    #   }
    # })
    # 
    withProgress(message = 'Remove dinamics', value = 0, {
      removeDin()
      #removeDin()
      n <- 10
      for (i in 1:n) {
        incProgress(1/n, detail = paste("Doing part", i))
        Sys.sleep(0.1)
      }
    })
    # 
    # withProgress(message = 'After remove dinamics', value = 0, {
    #   delay(2000, afterRemoveDinCheck())
    #   n <- 10
    #   for (i in 1:n) {
    #     incProgress(1/n, detail = paste("Doing part", i))
    #     Sys.sleep(0.1)
    #   }
    # })
    
    # withProgress(message = 'Add new dinamics', value = 0, {
    #   delay(500, generateDin())
    #   n <- 10
    #   for (i in 1:n) {
    #     incProgress(1/n, detail = paste("Doing part", i))
    #     Sys.sleep(1)
    #   }
    # })
    
    # withProgress(message = 'Update inputs', value = 0, {
    #   delay(1500, loadsession())
    #   n <- 10
    #   for (i in 1:n) {
    #     incProgress(1/n, detail = paste("Doing part", i))
    #     Sys.sleep(0.2)
    #   }
    # })
    
    
    
    #### old:
    
    #withProgress(message = 'Loading session...', value = 0, {
     # Sys.sleep(2)
      #removeDin()
      
      #delay(500, generateDin())
      
      #delay(1000, dftest())
      #delay(1000, loadsession())
      
    # withProgress(message = 'Calculation in progress', value = 0, {
    #   for (i in 1:3) {
    #     Sys.sleep(0.1)
    #     if (i == 1) {
    #       removeDin()
    #       print("fin 1")
    #     }
    # 
    #     if (i == 2) {
    #       #Sys.sleep(3)
    #       #generateDin()
    #       delay(5000, generateDin())
    #       #tutu()
    #       print("fin 2")
    #     }
    # 
    #     if (i == 3) {
    #       #loadsession()
    #       #dftest()
    #       delay(10000, dftest())
    #       print("fin 3")
    #     }
    #   }
    # })
    
    # list(
    #   removeDin(),
    #   generateDin(),
    #   
    #   dftest()
    # )
      
      
    
    #})
    
      ############ COD: IVAN CANSADO ###############
    
    
  })
  
  observeEvent(input$load_inputNew22, {
    withProgress(message = 'After remove dinamics', value = 0, {
      generateDin()
      
      n <- 10
      for (i in 1:n) {
        incProgress(1/n, detail = paste("Doing part", i))
        Sys.sleep(0.1)
      }
    })
  })
  
  observeEvent(input$load_inputNew33, {
    withProgress(message = 'After generate dinamics', value = 0, {
      loadsession()
      
      n <- 10
      for (i in 1:n) {
        incProgress(1/n, detail = paste("Doing part", i))
        Sys.sleep(0.1)
      }
    })
  })
  
  #################### END: LOAD FIELDBOOK ####################
  
  #################### START: GENERA NUEVO FIELDBOOK ####################
  # New fieldbook button
  observeEvent(input$newfieldbook, {
    if (session$userData$logged) {
      showModal(modalDialog(
        title =
          fluidRow(
            column(
              6,
              style = ("margin-top: -20px;margin-bottom: -10px;"),
              h3("Save fieldbook?")
            ),
            column(
              6,
              align = "right",
              style = "margin-top: 0px;",
              actionLink("btncancel2", "X")
            )
          ),
        fluidRow(
          column(
            1,
            icon("exclamation-triangle", "fa-3x")
          ),
          column(
            11,
            "Save changes to fieldbook before closing?",
            br(),
            "Your changes will be lost if you don’t save them."
          )
        ),
        br(),
        fluidRow(
          column(
            12,
            align = "center",
            actionButton("btnsave", "Save", class = "btn-success", style="color: #fff;", width = "95px"),
            actionButton("btndontsave", "Don't save", width = "95px"),
            actionButton("btncancel", "Cancel", width = "95px")
          )
        ),
        footer = NULL
      ))
    } else {
      shinyalert("Sorry", "You must login to create new fieldbook", type = "info", timer = 1500, showConfirmButton = F)
    }
  })
  
  # Boton que guarda el avance de la sesion antes que crear un nuevo fieldbook
  observeEvent(input$btnsave, {
    savesession()
    resetExperiment()
    removeModal()
  })
  
  # Boton que crea un nuevo fieldbook sin guardar
  observeEvent(input$btndontsave, {
    resetExperiment()
    removeModal()
  })
  
  # Boton que cierra el modal
  observeEvent(input$btncancel, {
    removeModal()
  })
  
  # Boton que cierra el modal desde el X
  observeEvent(input$btncancel2, {
    removeModal()
  })
  
  # Funcion que separa los valores del array para la funcion reset experiment
  getInputs<- function(valor, q) {
    valor <- sapply(valor, as.character)
    valor[is.na(valor)] <- " "
    valor
    
    if (stringr::str_detect(valor, "&")) {
      if (q == "start") {
        valor <- unlist(strsplit(valor, "&"))
        valor <- valor[[1]]
      }
      
      if (q == "end") {
        valor <- unlist(strsplit(valor, "&"))
        valor <- valor[[2]]
      }
    }
    
    if(stringr::str_detect(valor,"&")){
      valor<-unlist(strsplit(valor, "&"))
    } else {
      valor<-valor
    }
    
    valor
  }
  
  # Funcion que restaura por defecto los inputs: Experiment
  resetExperiment <- function() {
    if (session$userData$logged) {
      if (file.exists(isolate(paste0(templatepath, "template.csv")))){
        uploaded_inputs <- read.csv(paste0(templatepath, "template.csv"))
        
        for(i in 1:nrow(uploaded_inputs)) {
          inputId <- as.character(uploaded_inputs[i, 1])
          type <- as.character(uploaded_inputs[i, 2])
          create <- as.character(uploaded_inputs[i, 3])
          
          if (type == "textInput") {
            if (inputId == "uniqueId") {
              updateTextInput(session,
                              inputId = uploaded_inputs$inputId[i],
                              value = idgenerator())
            } else {
              updateTextInput(session,
                              inputId = uploaded_inputs$inputId[i],
                              value = uploaded_inputs$value[i])
            }
          }
          
          # if (type == "dateRangeInput") {
          #   updateDateRangeInput(session, #"fbDesign_project_time_line", 
          #                        inputId = uploaded_inputs$inputId[i], 
          #                        start = Sys.Date() - 2, 
          #                        end = Sys.Date() + 20)
          # }
          
          if (type == "dateRangeInput") {
            if (uploaded_inputs[i, 4] != "") {
              v <- getInputs(uploaded_inputs[i, 4], "")
              x <- as.Date(v[1]) + 1
              y <- as.Date(v[2]) + 1
              updateAirDateInput(session,
                                 inputId = uploaded_inputs$inputId[i],
                                 value = c(x, y),
                                 clear = T)
            } else {
              updateAirDateInput(session,
                                 inputId = uploaded_inputs$inputId[i],
                                 clear = T)
            }
          }
          
          if (type == "selectizeInput" && create == "n") {
            updateSelectizeInput(session,
                                 inputId = uploaded_inputs$inputId[i],
                                 selected = getInputs(uploaded_inputs[i, 4], ""))
          }
          
          if (type == "selectizeInput" && create == "y") {
            updateSelectizeInput(session,
                                 inputId = uploaded_inputs$inputId[i],
                                 selected = getInputs(uploaded_inputs[i, 4], ""),
                                 choices = getInputs(uploaded_inputs[i, 4], ""),
                                 options = list('create' = TRUE))
          }
          
          if (type == "textAreaInput") {
            updateTextAreaInput(session,
                                inputId = uploaded_inputs$inputId[i],
                                value = uploaded_inputs$value[i])
          }
          
          if (type == "numericInput") {
            updateNumericInput(session,
                               inputId = uploaded_inputs$inputId[i],
                               value = uploaded_inputs$value[i])
          }
        }
      }
    } else {
      shinyalert("Sorry", "You must login to create new fieldbook", type = "info", timer = 1500, showConfirmButton = F)
    }
  }
  
  
  #################### END: GENERA NUEVO FIELDBOOK ####################

  
  
  
  
  
  
  
  
  

  # observeEvent(input$openfieldbook, {
  #   onclick = "openTab('opensession')"
  # })

  # Muestra el experiment ID como titulo grande
  # output$idsession <- renderText({
  #   input$experimentId
  # })

  # Muestra el boton save
  # output$saveUI <- renderUI({
  #   if(session$userData$logged){
  #     actionButton('save_inputs', 'Save', icon("save"), class = "btn-success", style="color: #fff;")
  #   }
  # })
  
  
  
  ##########
  ##########



  #########
  #########

  # Carga data de ejemplo MONOCROP:
  observeEvent(input$load_exampleM, {

    ## Experiment
    # Experiment details
    updateTextInput(session, "experimentName", value = "Experiment name")
    updateTextInput(session, "experimentProjectName", value = "Big Data project")
    updateDateRangeInput(session, "fbDesign_project_time_line", start = "2018-08-31", end = "2018-12-31")
    updateSelectizeInput(session, "designFieldbook_typeExperiment", selected = "Controlled treatment trial")
    updateTextAreaInput(session, "experimentObj", value = "Genomic tools for sweetppotato improvements")
    # Institutions/Organizations/Agencies associated with experiment
    updateSelectizeInput(session, "designFieldbook_fundAgencyType", selected = c("Academic institution", "Other"))
    updateNumericInput(session, "numProjEntity", value = 2)
    # Experiment leads
    updateNumericInput(session, "numLeads", value = 2)

    ## Personnel
    updateSelectInput(session, "npersons", selected = 3)
    # Personnel associated with the experiment
    updateSelectizeInput(session, "personnel1Type", selected = "Farmer")
    updateTextInput(session, "person1FirstName", value = "Medha")
    updateTextInput(session, "person1LastName", value = "Devare")
    updateTextInput(session, "person1Email", value = "m.devare@cgiar.org")
    updateSelectizeInput(session, "person1Affiliation", selected = "CGIAR Center")
    updateSelectizeInput(session, "person1Center", selected = "Africa Rice Center")
    updateTextInput(session, "person1ORCID", value = "654676")

    ## Site
    # Site surrounding description
    updateSelectizeInput(session, "fbDesign_inHighLevel", selected = "Basin")
    updateSelectizeInput(session, "fbDesign_inSiteVegetation", selected = c("Crops", "Forest"))
    updateTextAreaInput(session, "inSiteDescNotes", value = "Description notes")

    ## Crop
    # Description of crops sown
    updateSelectInput(session, "croppingType", selected = "Monocrop")
    # Crop information
    updateSelectizeInput(session, "cropCommonNameMono", selected = "Cassava")
    updateSelectizeInput(session, "cultivarNameMono", selected = c("Variety name 1", "Variety name 2"), choices = c("Variety name 1", "Variety name 2"), options = list('create' = TRUE))
    #Previous crop or fallow
    updateSelectizeInput(session, "prevCropName", selected = "Maize")

    delay(500, c(## Experiment
                 updateTextInput(session, "fundName_1", value = "Institution name"),
                 updateTextInput(session, "fundName_2", value = "International Potato Center"),
                 updateSelectizeInput(session, "projEntity_1", selected = "CGIAR center"),
                 updateSelectizeInput(session, "contCenter_1", selected = "Africa Rice Center"),
                 updateSelectizeInput(session, "contCRP_1", selected = "CGIAR Research Program on Agriculture for Nutrition and Health"),
                 updateSelectizeInput(session, "projEntity_2", selected = "Other"),
                 updateSelectizeInput(session, "projEntity_2_other", selected = "Project name"),
                 updateSelectizeInput(session, "projLeadEnt_1", selected = "CGIAR center"),
                 updateSelectizeInput(session, "tLeadCenter_1", selected = "Africa Rice Center"),
                 updateTextInput(session, "expLead_1", value = "Lead person name"),
                 updateSelectizeInput(session, "projLeadEnt_2", selected = "Other"),
                 updateSelectizeInput(session, "lead_org_type_1_2", selected = "Agricultural experimental extension"),
                 updateTextInput(session, "leadNameOther_2", value = "Lead organization name"),
                 updateTextInput(session, "expLead_2", value = "Primary investigator"))
    )
  })

  # Carga data de ejemplo INTERCROP:
  observeEvent(input$load_exampleI, {

    ## Experiment
    # Experiment details
    updateTextInput(session, "experimentName", value = "Experiment name")
    updateTextInput(session, "experimentProjectName", value = "Big Data project")
    updateDateRangeInput(session, "fbDesign_project_time_line", start = "2018-08-31", end = "2018-12-31")
    updateSelectizeInput(session, "designFieldbook_typeExperiment", selected = "Controlled treatment trial")
    updateTextAreaInput(session, "experimentObj", value = "Genomic tools for sweetppotato improvements")
    # Institutions/Organizations/Agencies associated with experiment
    updateSelectizeInput(session, "designFieldbook_fundAgencyType", selected = c("Academic institution", "Other"))
    updateNumericInput(session, "numProjEntity", value = 2)
    # Experiment leads
    updateNumericInput(session, "numLeads", value = 2)

    ## Personnel
    updateSelectInput(session, "npersons", selected = 3)
    # Personnel associated with the experiment
    updateSelectizeInput(session, "personnel1Type", selected = "Farmer")
    updateTextInput(session, "person1FirstName", value = "Medha")
    updateTextInput(session, "person1LastName", value = "Devare")
    updateTextInput(session, "person1Email", value = "m.devare@cgiar.org")
    updateSelectizeInput(session, "person1Affiliation", selected = "CGIAR Center")
    updateSelectizeInput(session, "person1Center", selected = "Africa Rice Center")
    updateTextInput(session, "person1ORCID", value = "654676")

    ## Site
    # Site surrounding description
    updateSelectizeInput(session, "fbDesign_inHighLevel", selected = "Basin")
    updateSelectizeInput(session, "fbDesign_inSiteVegetation", selected = c("Crops", "Forest"))
    updateTextAreaInput(session, "inSiteDescNotes", value = "Description notes")

    ## Crop
    # Description of crops sown
    updateSelectInput(session, "croppingType", selected = "Intercrop")
    updateSelectizeInput(session, "cropsSelected", selected = c("Cassava", "Maize"))
    # Crop information
    updateSelectizeInput(session, "cropVarietyName1", selected = c("Cassava variety name 1", "Cassava variety name 2"), choices = c("Cassava variety name 1", "Cassava variety name 2"), options = list('create' = TRUE))
    updateSelectizeInput(session, "cropVarietyName2", selected = c("Maize variety name 1", "Maize variety name 2"), choices = c("Maize variety name 1", "Maize variety name 2"), options = list('create' = TRUE))
    # updateTextInput(session, "intercropValue_row_crop_1", value = 12)
    # updateTextInput(session, "intercropValue_row_crop_2", value = 40)
    #Previous crop or fallow
    updateSelectizeInput(session, "prevCropName", selected = "Rice")


    delay(500, c(## Experiment
      updateTextInput(session, "fundName_1", value = "Institution name"),
      updateTextInput(session, "fundName_2", value = "International Potato Center"),
      updateSelectizeInput(session, "projEntity_1", selected = "CGIAR center"),
      updateSelectizeInput(session, "contCenter_1", selected = "Africa Rice Center"),
      updateSelectizeInput(session, "contCRP_1", selected = "CGIAR Research Program on Agriculture for Nutrition and Health"),
      updateSelectizeInput(session, "projEntity_2", selected = "Other"),
      updateSelectizeInput(session, "projEntity_2_other", selected = "Project name"),
      updateSelectizeInput(session, "projLeadEnt_1", selected = "CGIAR center"),
      updateSelectizeInput(session, "tLeadCenter_1", selected = "Africa Rice Center"),
      updateTextInput(session, "expLead_1", value = "Lead person name"),
      updateSelectizeInput(session, "projLeadEnt_2", selected = "Other"),
      updateSelectizeInput(session, "lead_org_type_1_2", selected = "Agricultural experimental extension"),
      updateTextInput(session, "leadNameOther_2", value = "Lead organization name"),
      updateTextInput(session, "expLead_2", value = "Primary investigator"),
      ## Crop
      updateTextInput(session, "intercropValue_row_crop_1", value = 12),
      updateTextInput(session, "intercropValue_row_crop_2", value = 40))
    )
  })


  #### Others

  ########## abrir boxes ###############################################################

  observeEvent(input$boxTitleClicked, {
    boxId <- gsub('_titleId', '_boxid', input$boxTitleClickedId)
    js$collapse(boxId)
  })

  ############ fin abrir boxes ##########################################################

  ############ botones 'NEXT' ##########################################################
  observeEvent(input$btnNextPersonnelInfo, {
    updateTabsetPanel(session, "fbDesignNav", selected = "tabPersonnel")
    shinyjs::runjs("window.scrollTo(0, 50)")
  })
  observeEvent(input$btnNextSite, {
    updateTabsetPanel(session, "fbDesignNav", selected = "tabSite")
    shinyjs::runjs("window.scrollTo(0, 50)")
  })
  observeEvent(input$btnNextCropInfo, {
    updateTabsetPanel(session, "fbDesignNav", selected = "tabCrop")
    shinyjs::runjs("window.scrollTo(0, 50)")
  })
  observeEvent(input$btnDesign, {
    updateTabsetPanel(session, "fbDesignNav", selected = "tabDesign")
    shinyjs::runjs("window.scrollTo(0, 50)")
  })
  observeEvent(input$btnNextPlotInfo, {
    updateTabsetPanel(session, "fbDesignNav", selected = "tabPlotInfo")
    shinyjs::runjs("window.scrollTo(0, 50)")
  })
  observeEvent(input$btnNextAgro, {
    updateTabsetPanel(session, "fbDesignNav", selected = "tabAgroFeat")
    shinyjs::runjs("window.scrollTo(0, 50)")
  })
  observeEvent(input$btnNextCropPheno, {
    updateTabsetPanel(session, "fbDesignNav", selected = "tabCropPheno")
    shinyjs::runjs("window.scrollTo(0, 50)")
  })

  observeEvent(input$btnNextTraits, {
    updateTabsetPanel(session, "fbDesignNav", selected = "tabTraits")
    shinyjs::runjs("window.scrollTo(0, 50)")
  })
  observeEvent(input$btnNextEnv, {
    updateTabsetPanel(session, "fbDesignNav", selected = "tabEnvironment")
    shinyjs::runjs("window.scrollTo(0, 50)")
  })

  ############ fin botones 'NEXT' ##########################################################

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



  

  ######## Start Crop Measurement Ultima Version #########

  #### Start Tabs Crop Measurement: ####
  # observe({
  #   if (input$croppingType == "Monocrop") {
  #     shiny::hideTab(inputId = "fbDesignNav", target = "crop_measurement_inter")
  #     shiny::showTab(inputId = "fbDesignNav", target = "crop_measurement_mono")
  #   }
  #
  #   if (input$croppingType == "Intercrop") {
  #     shiny::showTab(inputId = "fbDesignNav", target = "crop_measurement_inter")
  #     shiny::hideTab(inputId = "fbDesignNav", target = "crop_measurement_mono")
  #   }
  # })

  observeEvent(input$croppingType, {
    if (input$croppingType == "Monocrop") {
      shiny::hideTab(inputId = "fbDesignNav", target = "crop_measurement_inter")
      shiny::hideTab(inputId = "fbDesignNav", target = "crop_measurement_relay")
      shiny::showTab(inputId = "fbDesignNav", target = "crop_measurement_mono")
    }

    if (input$croppingType == "Intercrop") {
      shiny::showTab(inputId = "fbDesignNav", target = "crop_measurement_inter")
      shiny::hideTab(inputId = "fbDesignNav", target = "crop_measurement_mono")
      shiny::hideTab(inputId = "fbDesignNav", target = "crop_measurement_relay")
    }
    
    if (input$croppingType == "Relay crop") {
      shiny::showTab(inputId = "fbDesignNav", target = "crop_measurement_relay")
      shiny::hideTab(inputId = "fbDesignNav", target = "crop_measurement_mono")
      shiny::hideTab(inputId = "fbDesignNav", target = "crop_measurement_inter")
    }
  })

  chu <- c("crop_measurement_Cassava", "crop_measurement_Commonbean", "crop_measurement_Maize", "crop_measurement_Potato", "crop_measurement_Rice",
           "crop_measurement_Sweetpotato", "crop_measurement_Wheat", "crop_measurement_Other_1", "crop_measurement_Other_2", "crop_measurement_Other_3",
           "crop_measurement_Other_4", "crop_measurement_Other_5", "crop_measurement_Other_6", "crop_measurement_Other_7", "crop_measurement_Other_8",
           "crop_measurement_Other_9", "crop_measurement_Other_10")
  
  observe({
    for (i in 1:length(chu)) {
      shiny::hideTab(inputId = "tabpanelinter", target = chu[i])
    }
  })
  
  chuRelay <- c("crop_measurement_relay_Cassava", "crop_measurement_relay_Commonbean", "crop_measurement_relay_Maize", "crop_measurement_relay_Potato", "crop_measurement_relay_Rice",
           "crop_measurement_relay_Sweetpotato", "crop_measurement_relay_Wheat", "crop_measurement_relay_Other_1", "crop_measurement_relay_Other_2", "crop_measurement_relay_Other_3",
           "crop_measurement_relay_Other_4", "crop_measurement_relay_Other_5", "crop_measurement_relay_Other_6", "crop_measurement_relay_Other_7", "crop_measurement_relay_Other_8",
           "crop_measurement_relay_Other_9", "crop_measurement_relay_Other_10")
  
  observe({
    for (i in 1:length(chuRelay)) {
      shiny::hideTab(inputId = "tabpanelrelay", target = chuRelay[i])
    }
  })
  
  # observe({
  #   
  #   ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector", default = "Monocrop")
  #   #print(ct)
  #   if (ct == "Intercrop") {
  #     id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
  #     #print(id_ic_rand)
  #     circm <- map_values(input, id_chr="int_cropCommonName_", id_ic_rand, format = "vector", lbl= "Select crop")
  #     #print(circm)
  #     cropivan <- paste0("crop_measurement_", circm)
  #   } else{
  #     #if(ct=="Monocrop"){
  #     crp <- map_singleform_values(input$cropCommonNameMono, input_other = input$cropCommonNameMono_other, type= "combo box", format = "vector", label = "Crop",default = "Maize")
  #     #print(crp)
  #     cropivan <- paste0("crop_measurement_",crp)
  #     #var<- map_singleform_values(input$cultivarNameMono, type= "combo box", format = "data.frame",label = "Crop variety(s)",collapsed = TRUE)
  #     #out <- rbind(ctd, crp, var)
  #     #}
  #   }
  #   
  #   for (i in 1:length(chu)) {
  #     shiny::hideTab(inputId = "tabpanelinter", target = chu[i])
  #   }
  #   
  #   for (i in 1:length(cropivan)) {
  #     #print(gsub(" ","",cropivan[i]))
  #     shiny::showTab(inputId = "tabpanelinter", target = gsub(" ","",cropivan[i]), select = T)
  #   }
  #   
  # })


  observeEvent(input$fbDesignNav, {

    # ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector", default = "Monocrop")
    # #print(ct)
    # if (ct == "Intercrop") {
    #   id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
    #   #print(id_ic_rand)
    #   circm <- map_values(input, id_chr="int_cropCommonName_", id_ic_rand, format = "vector", lbl= "Select crop")
    #   #print(circm)
    #   cropivan <- paste0("crop_measurement_", circm)
    # } else{
    #   #if(ct=="Monocrop"){
    #   crp <- map_singleform_values(input$cropCommonNameMono, input_other = input$cropCommonNameMono_other, type= "combo box", format = "vector", label = "Crop",default = "Maize")
    #   #print(crp)
    #   cropivan <- paste0("crop_measurement_",crp)
    #   #var<- map_singleform_values(input$cultivarNameMono, type= "combo box", format = "data.frame",label = "Crop variety(s)",collapsed = TRUE)
    #   #out <- rbind(ctd, crp, var)
    #   #}
    # }

    if (input$croppingType == "Intercrop") {
      rt <- c()
      
      id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
      #print(id_ic_rand)
      circm <- map_values(input, id_chr="int_cropCommonName_", id_ic_rand, format = "vector", lbl= "Select crop")
      #print(circm)
      
      
      for (i in 1:length(id_ic_rand)) {
        rt[i] <- as.character(input[[paste0("int_cropCommonName_", id_ic_rand[i])]])
      }
      
      #print(rt)
      
      #print(length(circm))
      a <- b <- c()
      # for (i in 1:length(circm)) {
      #   if (circm[i] == "") {
      #     a[i] <- paste0("crop_measurement_Other_", i)
      #   }
      # }
      print(length(rt))
      
      if (length(rt) >= 1) {
        for (i in 1:length(rt)) {
          if (rt[i] == "Other") {
            b[i] <- paste0("crop_measurement_Other_", i)
          }
        }
        
        #a <- a[!is.null(a)]
        #print(a)
        b <- b[!is.null(b)]
        
        cropivan <- paste0("crop_measurement_", rt)
        cropivan <- cropivan[!cropivan %in% "crop_measurement_Other"]
        cropivan <- c(cropivan, b)
        
        #print(cropivan)
        
        for (i in 1:length(chu)) {
          shiny::hideTab(inputId = "tabpanelinter", target = chu[i])
        }
        
        for (i in 1:length(cropivan)) {
          #print(gsub(" ","",cropivan[i]))
          shiny::showTab(inputId = "tabpanelinter", target = gsub(" ","",cropivan[i]), select = T)
        }
      }
      
    }
    
    if (input$croppingType == "Relay crop") {
      rtrel <- c()
      
      id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
      
      for (i in 1:length(id_re_rand)) {
        rtrel[i] <- as.character(input[[paste0("rel_cropCommonName_", id_re_rand[i])]])
      }
      
      ar <- br <- c()
      
      if (length(rtrel) >= 1) {
        for (i in 1:length(rtrel)) {
          if (rtrel[i] == "Other") {
            br[i] <- paste0("crop_measurement_relay_Other_", i)
          }
        }
        
        br <- br[!is.null(br)]
        
        cropivanrel <- paste0("crop_measurement_relay_", rtrel)
        cropivanrel <- cropivanrel[!cropivanrel %in% "crop_measurement_relay_Other"]
        cropivanrel <- c(cropivanrel, br)
        
        for (i in 1:length(chuRelay)) {
          shiny::hideTab(inputId = "tabpanelrelay", target = chuRelay[i])
        }
        
        for (i in 1:length(cropivanrel)) {
          shiny::showTab(inputId = "tabpanelrelay", target = gsub(" ","",cropivanrel[i]), select = T)
        }
      }
      
    }

  })

  #### End Tabs Crop Measurement: ####

  # Base de datos general para Crop Measurement:
  dfmea <- readRDS(paste0(globalpath, "crop_measurements_v6.4.rds"))
  dfmea <- as.data.frame(dfmea, stringsAsFactors=FALSE)
  colnames(dfmea) <- c("Crop",
                       "Group",
                       "Subgroup",
                       "Measurement",
                       "TraitUnit",
                       "NumberOfMeasurementsPerSeason",
                       "NumberOfMeasurementsPerPlot",
                       "TraitAlias",
                       "TraitDataType",
                       "TraitValidation",
                       "VariableId")

  #### Start Crop Measurement Monocrop ####
  output$uiCropMeaMono <- renderUI({
    DTOutput("tblMono")
  })

  fmono <- function(){
    crop_in <- input$cropCommonNameMono
    oth <- input$cropCommonNameMono_other

    if (!is.null(crop_in) && crop_in != "Other") {
      aux <- dplyr::filter(dfmea, Crop == crop_in)
    } else if(!is.null(crop_in) && crop_in == "Other") {
      aux <- dplyr::filter(dfmea, Crop == "Other")

      if (oth != "") {
        aux$Crop <- oth
        aux
      } else {
        aux
      }
    } else {
      aux <- dfmea[0,]
    }
  }
  dtMonocrop<- data.frame()

  output$tblMono = renderDT(
    datatable(
      dtMonocrop <<- fmono(),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )

  proxyMono = dataTableProxy('tblMono')

  observeEvent(input$tblMono_cell_edit, {
    info = input$tblMono_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtMonocrop[i, j] <<- DT::coerceValue(v, dtMonocrop[i, j])
    replaceData(proxyMono, dtMonocrop, resetPaging = FALSE, clearSelection = "none")
  })
  #### End Crop Measurement Monocrop ####

  #### Start Crop Measurement Intercrop #########################################
  finter <- function(crop_in) {
    #crop_in <- input$cropCommonNameMono
    oth <- input$cropCommonNameMono_other

    if (!is.null(crop_in) && crop_in != "Other") {
      aux <- dplyr::filter(dfmea, Crop == crop_in)
    } else if(!is.null(crop_in) && crop_in == "Other") {
      aux <- dplyr::filter(dfmea, Crop == "Other")

      if (oth != "") {
        aux$Crop <- oth
        aux
      } else {
        aux
      }
    } else {
      aux <- dfmea[0,]
    }
  }

  # Cassava
  dtInterCassava <- data.frame()
  output$tblInterCassava = renderDT(
    datatable(
      dtInterCassava <<- finter("Cassava"),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )

  proxyMonoCassava = dataTableProxy('tblInterCassava')

  observeEvent(input$tblInterCassava_cell_edit, {
    info = input$tblInterCassava_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtInterCassava[i, j] <<- DT::coerceValue(v, dtInterCassava[i, j])
    replaceData(proxyMonoCassava, dtInterCassava, resetPaging = FALSE, clearSelection = "none")
  })

  # Common bean
  dtInterCommon <- data.frame()
  output$tblInterCommon = renderDT(
    datatable(
      dtInterCommon <<- finter("Common bean"),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )

  proxyMonoCommonbean = dataTableProxy('tblInterCommon')

  observeEvent(input$tblInterCommon_cell_edit, {
    info = input$tblInterCommon_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtInterCommon[i, j] <<- DT::coerceValue(v, dtInterCommon[i, j])
    replaceData(proxyMonoCommonbean, dtInterCommon, resetPaging = FALSE, clearSelection = "none")
  })

  # Maize
  dtInterMaize <- data.frame()
  output$tblInterMaize = renderDT(
    datatable(
      dtInterMaize <<- finter("Maize"),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )

  proxyMonoMaize = dataTableProxy('tblInterMaize')

  observeEvent(input$tblInterMaize_cell_edit, {
    info = input$tblInterMaize_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtInterMaize[i, j] <<- DT::coerceValue(v, dtInterMaize[i, j])
    replaceData(proxyMonoMaize, dtInterMaize, resetPaging = FALSE, clearSelection = "none")
  })

  # Potato
  dtInterPotato <- data.frame()
  output$tblInterPotato = renderDT(
    datatable(
      dtInterPotato <<- finter("Potato"),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )

  proxyMonoPotato = dataTableProxy('tblInterPotato')

  observeEvent(input$tblInterPotato_cell_edit, {
    info = input$tblInterPotato_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtInterPotato[i, j] <<- DT::coerceValue(v, dtInterPotato[i, j])
    replaceData(proxyMonoPotato, dtInterPotato, resetPaging = FALSE, clearSelection = "none")
  })

  # Rice
  dtInterRice <- data.frame()
  output$tblInterRice = renderDT(
    datatable(
      dtInterRice <<- finter("Rice"),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )

  proxyMonoRice = dataTableProxy('tblInterRice')

  observeEvent(input$tblInterRice_cell_edit, {
    info = input$tblInterRice_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtInterRice[i, j] <<- DT::coerceValue(v, dtInterRice[i, j])
    replaceData(proxyMonoRice, dtInterRice, resetPaging = FALSE, clearSelection = "none")
  })

  # Sweetpotato
  dtInterSweetpotato<<- data.frame()
  output$tblInterSweetpotato = renderDT(
    datatable(
      dtInterSweetpotato <<- finter("Sweetpotato"),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )

  proxyMonoSweetpotato = dataTableProxy('tblInterSweetpotato')

  observeEvent(input$tblInterSweetpotato_cell_edit, {
    info = input$tblInterSweetpotato_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtInterSweetpotato[i, j] <<- DT::coerceValue(v, dtInterSweetpotato[i, j])
    replaceData(proxyMonoSweetpotato, dtInterSweetpotato, resetPaging = FALSE, clearSelection = "none")
  })

  # Wheat
  dtInterWheat <<- data.frame()
  output$tblInterWheat = renderDT(
    datatable(
      dtInterWheat <<- finter("Wheat"),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )

  proxyMonoWheat = dataTableProxy('tblInterWheat')

  observeEvent(input$tblInterWheat_cell_edit, {
    info = input$tblInterWheat_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtInterWheat[i, j] <<- DT::coerceValue(v, dtInterWheat[i, j])
    replaceData(proxyMonoWheat, dtInterWheat, resetPaging = FALSE, clearSelection = "none")
  })
  
  ## Funcione para Others de Intercrop: Crop Measurement
  
  # Funcion que hace clic al boton cada vez que cambiamos de tap panel, asi ejecuta el update del DT de others
  observeEvent(input$fbDesignNav, {
    shinyjs::click("do")
  })
  
  # Funcion que oculta el boton por defecto a vista del usuario
  observe({
    shinyjs::hide("do")
  })
  
  previousSelection <- NULL
  
  # Other 1: ===================================================
  finterMOt1 <- eventReactive(input$do, {
    id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
    
    crop_id <- as.character(input[[paste0("int_cropCommonName_", id_ic_rand[1])]])
    oth <- as.character(input[[paste0("int_cropCommonName_", id_ic_rand[1], "_other")]])
    
    if (nrow(dtInterOther1) >= 1) {
      
      if (crop_id == "Other" && dtInterOther1[1, 1] != oth) {
        f1()
      } else {
        
        previousSelection <<- input$tblInterOther1_rows_selected
        dtInterOther1
      }
      
    } else {
      f1()
    }
    
  }, ignoreNULL = FALSE)
  
  f1 <- function() {
    id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
    #print("entraivan")
    # Cambiar el parametro numerico [?] para cada other
    o <- paste0("int_cropCommonName_", id_ic_rand[1], "_other")
    oth <- as.character(input[[o]])
    
    aux <- dplyr::filter(dfmea, Crop == "Other")
    
    if (oth != "") {
      aux$Crop <- oth
      aux
    } else {
      aux
    }
  }
  
  f <- function(oid) {
    id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
    
    # Cambiar el parametro numerico [?] para cada other
    o <- paste0("int_cropCommonName_", id_ic_rand[1], "_other")
    oth <- as.character(input[[o]])
    print(length(oth))
    print(oth)
    aux <- dplyr::filter(dfmea, Crop == "Other")
    
    if (length(oth >= 1)) {
      print("a")
      if (oth != "") {
        print("b")
        aux$Crop <- oth
        aux
      } else {
        aux
      }
    } else {
      aux
      print("c")
    }
  }
  
  dtInterOther1 <- data.frame()
  output$tblInterOther1 = renderDT(
    datatable(
      dtInterOther1 <<- finterMOt1(),
      #selection = 'multiple',
      selection = list(mode = "multiple", target = "row", selected = previousSelection),
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
  )

  proxyMonoOther1 = dataTableProxy('tblInterOther1')

  observeEvent(input$tblInterOther1_cell_edit, {
    info = input$tblInterOther1_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtInterOther1[i, j] <<- DT::coerceValue(v, dtInterOther1[i, j])
    replaceData(proxyMonoOther1, dtInterOther1, resetPaging = FALSE, clearSelection = "none")
  })
  
  # Other 2: ===================================================
  finterMOt2 <- eventReactive(input$do, {
    id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
    
    crop_id <- as.character(input[[paste0("int_cropCommonName_", id_ic_rand[2])]])
    oth <- as.character(input[[paste0("int_cropCommonName_", id_ic_rand[2], "_other")]])
    
    if (nrow(dtInterOther2) >= 1) {
      
      if (crop_id == "Other" && dtInterOther2[1, 1] != oth) {
        f2()
      } else {
        previousSelection <<- input$tblInterOther2_rows_selected
        dtInterOther2
      }
      
    } else {
      f2()
    }
    
  }, ignoreNULL = FALSE)
  
  f2 <- function() {
    id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
    
    # Cambiar el parametro numerico [?] para cada other
    o <- paste0("int_cropCommonName_", id_ic_rand[2], "_other")
    oth <- as.character(input[[o]])
    
    aux <- dplyr::filter(dfmea, Crop == "Other")
    
    if (oth != "") {
      aux$Crop <- oth
      aux
    } else {
      aux
    }
  }

  dtInterOther2 <- data.frame()
  output$tblInterOther2 = renderDT(
    datatable(
      dtInterOther2 <<- finterMOt2(),
      #selection = 'multiple',
      selection = list(mode = "multiple", target = "row", selected = previousSelection),
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
  )

  proxyMonoOther2 = dataTableProxy('tblInterOther2')

  observeEvent(input$tblInterOther2_cell_edit, {
    info = input$tblInterOther2_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtInterOther2[i, j] <<- DT::coerceValue(v, dtInterOther2[i, j])
    replaceData(proxyMonoOther2, dtInterOther2, resetPaging = FALSE, clearSelection = "none")
  })

  # Other 3: ===================================================
  finterMOt3 <- eventReactive(input$do, {
    id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
    
    crop_id <- as.character(input[[paste0("int_cropCommonName_", id_ic_rand[3])]])
    oth <- as.character(input[[paste0("int_cropCommonName_", id_ic_rand[3], "_other")]])
    
    if (nrow(dtInterOther3) >= 1) {
      
      if (crop_id == "Other" && dtInterOther3[1, 1] != oth) {
        f3()
      } else {
        previousSelection <<- input$tblInterOther3_rows_selected
        dtInterOther3
      }
      
    } else {
      f3()
    }
    
  }, ignoreNULL = FALSE)
  
  f3 <- function() {
    id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
    
    # Cambiar el parametro numerico [?] para cada other
    o <- paste0("int_cropCommonName_", id_ic_rand[3], "_other")
    oth <- as.character(input[[o]])
    
    aux <- dplyr::filter(dfmea, Crop == "Other")
    
    if (oth != "") {
      aux$Crop <- oth
      aux
    } else {
      aux
    }
  }

  dtInterOther3 <- data.frame()
  output$tblInterOther3 = renderDT(
    datatable(
      dtInterOther3 <<- finterMOt3(),
      #selection = 'multiple',
      selection = list(mode = "multiple", target = "row", selected = previousSelection),
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
  )

  proxyMonoOther3 = dataTableProxy('tblInterOther3')

  observeEvent(input$tblInterOther3_cell_edit, {
    info = input$tblInterOther3_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtInterOther3[i, j] <<- DT::coerceValue(v, dtInterOther3[i, j])
    replaceData(proxyMonoOther3, dtInterOther3, resetPaging = FALSE, clearSelection = "none")
  })

  # Other 4: ===================================================
  finterMOt4 <- eventReactive(input$do, {
    id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
    
    crop_id <- as.character(input[[paste0("int_cropCommonName_", id_ic_rand[4])]])
    oth <- as.character(input[[paste0("int_cropCommonName_", id_ic_rand[4], "_other")]])
    
    if (nrow(dtInterOther4) >= 1) {
      
      if (crop_id == "Other" && dtInterOther4[1, 1] != oth) {
        f4()
      } else {
        previousSelection <<- input$tblInterOther4_rows_selected
        dtInterOther4
      }
      
    } else {
      f4()
    }
    
  }, ignoreNULL = FALSE)
  
  f4 <- function() {
    id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
    
    # Cambiar el parametro numerico [?] para cada other
    o <- paste0("int_cropCommonName_", id_ic_rand[4], "_other")
    oth <- as.character(input[[o]])
    
    aux <- dplyr::filter(dfmea, Crop == "Other")
    
    if (oth != "") {
      aux$Crop <- oth
      aux
    } else {
      aux
    }
  }

  dtInterOther4 <- data.frame()
  output$tblInterOther4 = renderDT(
    datatable(
      dtInterOther4 <<- finterMOt4(),
      #selection = 'multiple',
      selection = list(mode = "multiple", target = "row", selected = previousSelection),
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
  )

  proxyMonoOther4 = dataTableProxy('tblInterOther4')

  observeEvent(input$tblInterOther4_cell_edit, {
    info = input$tblInterOther4_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtInterOther4[i, j] <<- DT::coerceValue(v, dtInterOther4[i, j])
    replaceData(proxyMonoOther4, dtInterOther4, resetPaging = FALSE, clearSelection = "none")
  })

  # Other 5: ===================================================
  finterMOt5 <- eventReactive(input$do, {
    id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
    
    crop_id <- as.character(input[[paste0("int_cropCommonName_", id_ic_rand[5])]])
    oth <- as.character(input[[paste0("int_cropCommonName_", id_ic_rand[5], "_other")]])
    
    if (nrow(dtInterOther5) >= 1) {
      
      if (crop_id == "Other" && dtInterOther5[1, 1] != oth) {
        f5()
      } else {
        previousSelection <<- input$tblInterOther5_rows_selected
        dtInterOther5
      }
      
    } else {
      f5()
    }
    
  }, ignoreNULL = FALSE)
  
  f5 <- function() {
    id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
    
    # Cambiar el parametro numerico [?] para cada other
    o <- paste0("int_cropCommonName_", id_ic_rand[5], "_other")
    oth <- as.character(input[[o]])
    
    aux <- dplyr::filter(dfmea, Crop == "Other")
    
    if (oth != "") {
      aux$Crop <- oth
      aux
    } else {
      aux
    }
  }

  dtInterOther5 <- data.frame()
  output$tblInterOther5 = renderDT(
    datatable(
      dtInterOther5 <<- finterMOt5(),
      #selection = 'multiple',
      selection = list(mode = "multiple", target = "row", selected = previousSelection),
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
  )

  proxyMonoOther5 = dataTableProxy('tblInterOther5')

  observeEvent(input$tblInterOther5_cell_edit, {
    info = input$tblInterOther5_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtInterOther5[i, j] <<- DT::coerceValue(v, dtInterOther5[i, j])
    replaceData(proxyMonoOther5, dtInterOther5, resetPaging = FALSE, clearSelection = "none")
  })
  #### End Crop Measurement Intercrop ############################################################
  
  #### Start Crop Measurement Relay crop ####
  frelay <- function(crop_in) {
    #crop_in <- input$cropCommonNameMono
    oth <- input$cropCommonNameMono_other
    
    if (!is.null(crop_in) && crop_in != "Other") {
      aux <- dplyr::filter(dfmea, Crop == crop_in)
    } else if(!is.null(crop_in) && crop_in == "Other") {
      aux <- dplyr::filter(dfmea, Crop == "Other")
      
      if (oth != "") {
        aux$Crop <- oth
        aux
      } else {
        aux
      }
    } else {
      aux <- dfmea[0,]
    }
  }
  
  # Cassava
  dtRelayCassava <- data.frame()
  output$tblRelayCassava = renderDT(
    datatable(
      dtRelayCassava <<- frelay("Cassava"),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )
  
  proxyRelayCassava = dataTableProxy('tblRelayCassava')
  
  observeEvent(input$tblRelayCassava_cell_edit, {
    info = input$tblRelayCassava_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtRelayCassava[i, j] <<- DT::coerceValue(v, dtRelayCassava[i, j])
    replaceData(proxyRelayCassava, dtRelayCassava, resetPaging = FALSE, clearSelection = "none")
  })
  
  # Common bean
  dtRelayCommon <- data.frame()
  output$tblRelayCommon = renderDT(
    datatable(
      dtRelayCommon <<- frelay("Common bean"),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )
  
  proxyRelayCommonbean = dataTableProxy('tblRelayCommon')
  
  observeEvent(input$tblRelayCommon_cell_edit, {
    info = input$tblRelayCommon_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtRelayCommon[i, j] <<- DT::coerceValue(v, dtRelayCommon[i, j])
    replaceData(proxyRelayCommonbean, dtRelayCommon, resetPaging = FALSE, clearSelection = "none")
  })
  
  # Maize
  dtRelayMaize <- data.frame()
  output$tblRelayMaize = renderDT(
    datatable(
      dtRelayMaize <<- frelay("Maize"),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )
  
  proxyRelayMaize = dataTableProxy('tblRelayMaize')
  
  observeEvent(input$tblRelayMaize_cell_edit, {
    info = input$tblRelayMaize_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtRelayMaize[i, j] <<- DT::coerceValue(v, dtRelayMaize[i, j])
    replaceData(proxyRelayMaize, dtRelayMaize, resetPaging = FALSE, clearSelection = "none")
  })
  
  # Potato
  dtRelayPotato <- data.frame()
  output$tblRelayPotato = renderDT(
    datatable(
      dtRelayPotato <<- frelay("Potato"),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )
  
  proxyRelayPotato = dataTableProxy('tblRelayPotato')
  
  observeEvent(input$tblRelayPotato_cell_edit, {
    info = input$tblRelayPotato_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtRelayPotato[i, j] <<- DT::coerceValue(v, dtRelayPotato[i, j])
    replaceData(proxyRelayPotato, dtRelayPotato, resetPaging = FALSE, clearSelection = "none")
  })
  
  # Rice
  dtRelayRice <- data.frame()
  output$tblRelayRice = renderDT(
    datatable(
      dtRelayRice <<- frelay("Rice"),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )
  
  proxyRelayRice = dataTableProxy('tblRelayRice')
  
  observeEvent(input$tblRelayRice_cell_edit, {
    info = input$tblRelayRice_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtRelayRice[i, j] <<- DT::coerceValue(v, dtRelayRice[i, j])
    replaceData(proxyRelayRice, dtRelayRice, resetPaging = FALSE, clearSelection = "none")
  })
  
  # Sweetpotato
  dtRelaySweetpotato<- data.frame()
  output$tblRelaySweetpotato = renderDT(
    datatable(
      dtRelaySweetpotato <<- frelay("Sweetpotato"),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )
  
  proxyRelaySweetpotato = dataTableProxy('tblRelaySweetpotato')
  
  observeEvent(input$tblRelaySweetpotato_cell_edit, {
    info = input$tblRelaySweetpotato_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtRelaySweetpotato[i, j] <<- DT::coerceValue(v, dtRelaySweetpotato[i, j])
    replaceData(proxyRelaySweetpotato, dtRelaySweetpotato, resetPaging = FALSE, clearSelection = "none")
  })
  
  # Wheat
  dtRelayWheat <- data.frame()
  output$tblRelayWheat = renderDT(
    datatable(
      dtRelayWheat <<- frelay("Wheat"),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )
  
  proxyRelayWheat = dataTableProxy('tblRelayWheat')
  
  observeEvent(input$tblRelayWheat_cell_edit, {
    info = input$tblRelayWheat_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtRelayWheat[i, j] <<- DT::coerceValue(v, dtRelayWheat[i, j])
    replaceData(proxyRelayWheat, dtRelayWheat, resetPaging = FALSE, clearSelection = "none")
  })
  
  ## Funcione para Others de Relay crop: Crop Measurement
  
  # Funcion que hace clic al boton cada vez que cambiamos de tap panel, asi ejecuta el update del DT de others
  observeEvent(input$fbDesignNav, {
    shinyjs::click("doRelay")
  })
  
  # Funcion que oculta el boton por defecto a vista del usuario
  observe({
    shinyjs::hide("doRelay")
  })
  
  previousSelection <- NULL
  
  # Other 1: ===================================================
  frelayMOt1 <- eventReactive(input$doRelay, {
    id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
    
    crop_id <- as.character(input[[paste0("rel_cropCommonName_", id_re_rand[1])]])
    oth <- as.character(input[[paste0("rel_cropCommonName_", id_re_rand[1], "_other")]])
    
    if (nrow(dtRelayOther1) >= 1) {
      
      if (crop_id == "Other" && dtRelayOther1[1, 1] != oth) {
        frel1()
      } else {
        
        previousSelection <<- input$tblRelayOther1_rows_selected
        dtRelayOther1
      }
      
    } else {
      frel1()
    }
    
  }, ignoreNULL = FALSE)
  
  frel1 <- function() {
    id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
    #print("entraivan")
    # Cambiar el parametro numerico [?] para cada other
    o <- paste0("rel_cropCommonName_", id_re_rand[1], "_other")
    oth <- as.character(input[[o]])
    
    aux <- dplyr::filter(dfmea, Crop == "Other")
    
    if (oth != "") {
      aux$Crop <- oth
      aux
    } else {
      aux
    }
  }
  
  dtRelayOther1 <<- data.frame()
  output$tblRelayOther1 = renderDT(
    datatable(
      dtRelayOther1 <<- frelayMOt1(),
      #selection = 'multiple',
      selection = list(mode = "multiple", target = "row", selected = previousSelection),
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
  )
  
  proxyRelayOther1 = dataTableProxy('tblRelayOther1')
  
  observeEvent(input$tblRelayOther1_cell_edit, {
    info = input$tblRelayOther1_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtRelayOther1[i, j] <<- DT::coerceValue(v, dtRelayOther1[i, j])
    replaceData(proxyRelayOther1, dtRelayOther1, resetPaging = FALSE, clearSelection = "none")
  })
  
  # Other 2: ===================================================
  frelayMOt2 <- eventReactive(input$doRelay, {
    id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
    
    crop_id <- as.character(input[[paste0("rel_cropCommonName_", id_re_rand[2])]])
    oth <- as.character(input[[paste0("rel_cropCommonName_", id_re_rand[2], "_other")]])
    
    if (nrow(dtRelayOther2) >= 1) {
      
      if (crop_id == "Other" && dtRelayOther2[1, 1] != oth) {
        frel2()
      } else {
        
        previousSelection <<- input$tblRelayOther2_rows_selected
        dtRelayOther2
      }
      
    } else {
      frel2()
    }
    
  }, ignoreNULL = FALSE)
  
  frel2 <- function() {
    id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
    #print("entraivan")
    # Cambiar el parametro numerico [?] para cada other
    o <- paste0("rel_cropCommonName_", id_re_rand[2], "_other")
    oth <- as.character(input[[o]])
    
    aux <- dplyr::filter(dfmea, Crop == "Other")
    
    if (oth != "") {
      aux$Crop <- oth
      aux
    } else {
      aux
    }
  }
  
  dtRelayOther2 <<- data.frame()
  output$tblRelayOther2 = renderDT(
    datatable(
      dtRelayOther2 <<- frelayMOt2(),
      #selection = 'multiple',
      selection = list(mode = "multiple", target = "row", selected = previousSelection),
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
  )
  
  proxyRelayOther2 = dataTableProxy('tblRelayOther2')
  
  observeEvent(input$tblRelayOther2_cell_edit, {
    info = input$tblRelayOther2_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtRelayOther2[i, j] <<- DT::coerceValue(v, dtRelayOther2[i, j])
    replaceData(proxyRelayOther2, dtRelayOther2, resetPaging = FALSE, clearSelection = "none")
  })
  
  # Other 3: ===================================================
  frelayMOt3 <- eventReactive(input$doRelay, {
    id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
    
    crop_id <- as.character(input[[paste0("rel_cropCommonName_", id_re_rand[3])]])
    oth <- as.character(input[[paste0("rel_cropCommonName_", id_re_rand[3], "_other")]])
    
    if (nrow(dtRelayOther3) >= 1) {
      
      if (crop_id == "Other" && dtRelayOther3[1, 1] != oth) {
        frel3()
      } else {
        
        previousSelection <<- input$tblRelayOther3_rows_selected
        dtRelayOther3
      }
      
    } else {
      frel3()
    }
    
  }, ignoreNULL = FALSE)
  
  frel3 <- function() {
    id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
    #print("entraivan")
    # Cambiar el parametro numerico [?] para cada other
    o <- paste0("rel_cropCommonName_", id_re_rand[3], "_other")
    oth <- as.character(input[[o]])
    
    aux <- dplyr::filter(dfmea, Crop == "Other")
    
    if (oth != "") {
      aux$Crop <- oth
      aux
    } else {
      aux
    }
  }
  
  dtRelayOther3 <<- data.frame()
  output$tblRelayOther3 = renderDT(
    datatable(
      dtRelayOther3 <<- frelayMOt3(),
      #selection = 'multiple',
      selection = list(mode = "multiple", target = "row", selected = previousSelection),
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
  )
  
  proxyRelayOther3 = dataTableProxy('tblRelayOther3')
  
  observeEvent(input$tblRelayOther3_cell_edit, {
    info = input$tblRelayOther3_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtRelayOther3[i, j] <<- DT::coerceValue(v, dtRelayOther3[i, j])
    replaceData(proxyRelayOther3, dtRelayOther3, resetPaging = FALSE, clearSelection = "none")
  })
  
  # Other 4: ===================================================
  frelayMOt4 <- eventReactive(input$doRelay, {
    id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
    
    crop_id <- as.character(input[[paste0("rel_cropCommonName_", id_re_rand[4])]])
    oth <- as.character(input[[paste0("rel_cropCommonName_", id_re_rand[4], "_other")]])
    
    if (nrow(dtRelayOther4) >= 1) {
      
      if (crop_id == "Other" && dtRelayOther4[1, 1] != oth) {
        frel4()
      } else {
        
        previousSelection <<- input$tblRelayOther4_rows_selected
        dtRelayOther4
      }
      
    } else {
      frel4()
    }
    
  }, ignoreNULL = FALSE)
  
  frel4 <- function() {
    id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
    #print("entraivan")
    # Cambiar el parametro numerico [?] para cada other
    o <- paste0("rel_cropCommonName_", id_re_rand[4], "_other")
    oth <- as.character(input[[o]])
    
    aux <- dplyr::filter(dfmea, Crop == "Other")
    
    if (oth != "") {
      aux$Crop <- oth
      aux
    } else {
      aux
    }
  }
  
  dtRelayOther4 <<- data.frame()
  output$tblRelayOther4 = renderDT(
    datatable(
      dtRelayOther4 <<- frelayMOt4(),
      #selection = 'multiple',
      selection = list(mode = "multiple", target = "row", selected = previousSelection),
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
  )
  
  proxyRelayOther4 = dataTableProxy('tblRelayOther4')
  
  observeEvent(input$tblRelayOther4_cell_edit, {
    info = input$tblRelayOther4_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtRelayOther4[i, j] <<- DT::coerceValue(v, dtRelayOther4[i, j])
    replaceData(proxyRelayOther4, dtRelayOther4, resetPaging = FALSE, clearSelection = "none")
  })
  
  # Other 5: ===================================================
  frelayMOt5 <- eventReactive(input$doRelay, {
    id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
    
    crop_id <- as.character(input[[paste0("rel_cropCommonName_", id_re_rand[5])]])
    oth <- as.character(input[[paste0("rel_cropCommonName_", id_re_rand[5], "_other")]])
    
    if (nrow(dtRelayOther5) >= 1) {
      
      if (crop_id == "Other" && dtRelayOther5[1, 1] != oth) {
        frel5()
      } else {
        
        previousSelection <<- input$tblRelayOther5_rows_selected
        dtRelayOther5
      }
      
    } else {
      frel5()
    }
    
  }, ignoreNULL = FALSE)
  
  frel5 <- function() {
    id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
    #print("entraivan")
    # Cambiar el parametro numerico [?] para cada other
    o <- paste0("rel_cropCommonName_", id_re_rand[5], "_other")
    oth <- as.character(input[[o]])
    
    aux <- dplyr::filter(dfmea, Crop == "Other")
    
    if (oth != "") {
      aux$Crop <- oth
      aux
    } else {
      aux
    }
  }
  
  dtRelayOther5 <<- data.frame()
  output$tblRelayOther5 = renderDT(
    datatable(
      dtRelayOther5 <<- frelayMOt5(),
      #selection = 'multiple',
      selection = list(mode = "multiple", target = "row", selected = previousSelection),
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11)))
      ))
  )
  
  proxyRelayOther5 = dataTableProxy('tblRelayOther5')
  
  observeEvent(input$tblRelayOther5_cell_edit, {
    info = input$tblRelayOther5_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtRelayOther5[i, j] <<- DT::coerceValue(v, dtRelayOther5[i, j])
    replaceData(proxyRelayOther5, dtRelayOther5, resetPaging = FALSE, clearSelection = "none")
  })
  #### End Crop Measurement Relay crop ####

  ######## End Crop Measurement Ultima Version #########

  ######################################################
  ######################################################

  ######## Start Crop Phenology Ultima Version #########

  #### Start Tabs Crop Phenology: ####
  observeEvent(input$croppingType, {
    if (input$croppingType == "Monocrop") {
      shiny::hideTab(inputId = "fbDesignNav", target = "crop_phenology_inter")
      shiny::hideTab(inputId = "fbDesignNav", target = "crop_phenology_relay")
      shiny::showTab(inputId = "fbDesignNav", target = "crop_phenology_mono")
    }

    if (input$croppingType == "Intercrop") {
      shiny::showTab(inputId = "fbDesignNav", target = "crop_phenology_inter")
      shiny::hideTab(inputId = "fbDesignNav", target = "crop_phenology_mono")
      shiny::hideTab(inputId = "fbDesignNav", target = "crop_phenology_relay")
    }
    
    if (input$croppingType == "Relay crop") {
      shiny::showTab(inputId = "fbDesignNav", target = "crop_phenology_relay")
      shiny::hideTab(inputId = "fbDesignNav", target = "crop_phenology_mono")
      shiny::hideTab(inputId = "fbDesignNav", target = "crop_phenology_inter")
    }
  })

  chuphe <- c("crop_phenology_Cassava", "crop_phenology_Commonbean", "crop_phenology_Maize", "crop_phenology_Potato", "crop_phenology_Rice",
           "crop_phenology_Sweetpotato", "crop_phenology_Wheat", "crop_phenology_Other_1",
           "crop_phenology_Other_2",
           "crop_phenology_Other_3",
           "crop_phenology_Other_4",
           "crop_phenology_Other_5",
           "crop_phenology_Other_6",
           "crop_phenology_Other_7",
           "crop_phenology_Other_8",
           "crop_phenology_Other_9",
           "crop_phenology_Other_10")
  
  observe({
    for (i in 1:length(chuphe)) {
      shiny::hideTab(inputId = "tabpanelinterphe", target = chuphe[i])
    }
  })
  
  chupheRelay <- c("crop_phenology_relay_Cassava", "crop_phenology_relay_Commonbean", "crop_phenology_relay_Maize", "crop_phenology_relay_Potato", "crop_phenology_relay_Rice",
                   "crop_phenology_relay_Sweetpotato", "crop_phenology_relay_Wheat", "crop_phenology_relay_Other_1",
                   "crop_phenology_relay_Other_2",
                   "crop_phenology_relay_Other_3",
                   "crop_phenology_relay_Other_4",
                   "crop_phenology_relay_Other_5",
                   "crop_phenology_relay_Other_6",
                   "crop_phenology_relay_Other_7",
                   "crop_phenology_relay_Other_8",
                   "crop_phenology_relay_Other_9",
                   "crop_phenology_relay_Other_10")
  
  observe({
    for (i in 1:length(chupheRelay)) {
      shiny::hideTab(inputId = "tabpanelrelayphe", target = chupheRelay[i])
    }
  })
  
  observeEvent(input$fbDesignNav, {

    # if (input$croppingType == "Intercrop") {
    #   id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
    #   circm <- map_values(input, id_chr="int_cropCommonName_", id_ic_rand, format = "vector", lbl= "Select crop")
    #   #print(circm)
    #   cropivanphe <- paste0("crop_phenology_", circm)
    # 
    #   for (i in 1:length(chuphe)) {
    #     shiny::hideTab(inputId = "tabpanelinterphe", target = chuphe[i])
    #   }
    # 
    #   for (i in 1:length(cropivanphe)) {
    #     #print(gsub(" ","",cropivanphe[i]))
    #     shiny::showTab(inputId = "tabpanelinterphe", target = gsub(" ","",cropivanphe[i]), select = T)
    #   }
    # }
    
    if (input$croppingType == "Intercrop") {
      rtphe <- c()
      
      id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
      #circm <- map_values(input, id_chr="int_cropCommonName_", id_ic_rand, format = "vector", lbl= "Select crop")
      
      for (i in 1:length(id_ic_rand)) {
        rtphe[i] <- as.character(input[[paste0("int_cropCommonName_", id_ic_rand[i])]])
      }
      
      aa <- bb <- c()
      
      if (length(rtphe) >= 1) {
        for (i in 1:length(rtphe)) {
          if (rtphe[i] == "Other") {
            bb[i] <- paste0("crop_phenology_Other_", i)
          }
        }
        
        bb <- bb[!is.null(bb)]
        
        cropivanphe <- paste0("crop_phenology_", rtphe)
        cropivanphe <- cropivanphe[!cropivanphe %in% "crop_phenology_Other"]
        cropivanphe <- c(cropivanphe, bb)
        
        for (i in 1:length(chuphe)) {
          shiny::hideTab(inputId = "tabpanelinterphe", target = chuphe[i])
        }
        
        for (i in 1:length(cropivanphe)) {
          shiny::showTab(inputId = "tabpanelinterphe", target = gsub(" ","",cropivanphe[i]), select = T)
        }
      }
      
      
    }
    
    if (input$croppingType == "Relay crop") {
      rtRelPhe <- c()
      
      id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
      
      for (i in 1:length(id_re_rand)) {
        rtRelPhe[i] <- as.character(input[[paste0("rel_cropCommonName_", id_re_rand[i])]])
      }
      
      aarel <- bbrel <- c()
      
      if (length(rtRelPhe) >= 1) {
        for (i in 1:length(rtRelPhe)) {
          if (rtRelPhe[i] == "Other") {
            bbrel[i] <- paste0("crop_phenology_relay_Other_", i)
          }
        }
        
        bbrel <- bbrel[!is.null(bbrel)]
        
        cropivanpherel <- paste0("crop_phenology_relay_", rtRelPhe)
        cropivanpherel <- cropivanpherel[!cropivanpherel %in% "crop_phenology_relay_Other"]
        cropivanpherel <- c(cropivanpherel, bbrel)
        
        for (i in 1:length(chupheRelay)) {
          shiny::hideTab(inputId = "tabpanelrelayphe", target = chupheRelay[i])
        }
        
        for (i in 1:length(cropivanpherel)) {
          shiny::showTab(inputId = "tabpanelrelayphe", target = gsub(" ","",cropivanpherel[i]), select = T)
        }
      }
      
      
    }

  })
  #### End Tabs Crop Phenology: ####

  # Base de datos general para Crop Phenology:
  #dfphe <- readRDS(paste0(globalpath, "crop_measurements_v6.3.rds"))
  dfphe <- pheno_vars #as.data.frame(dfphe, stringsAsFactors=FALSE)
  dfphe <- ec_clean_header(dfphe)
  colnames(dfphe) <- c("Crop",
                       "Group",
                       "Subgroup",
                       "Measurement",
                       "TraitName",
                       "TraitUnit",
                       "NumberOfMeasurementsPerSeason",
                       "NumberOfMeasurementsPerPlot",
                       "TraitAlias",
                       "TraitDataType",
                       "TraitValidation",
                       "VariableId",
                       "Fieldbook_download")

  #### Start Crop Phenology Monocrop ####
  output$uiCropPheMono <- renderUI({
    DTOutput("tblMonoPhe")
  })

  fmonophe <- function(){
    crop_in <- input$cropCommonNameMono
    oth <- input$cropCommonNameMono_other

    if (!is.null(crop_in) && crop_in != "Other") {
      #aux <- dplyr::filter(dfmea, Crop == crop_in)
      aux <- dfphe
    } else if(!is.null(crop_in) && crop_in == "Other") {
      #aux <- dplyr::filter(dfmea, Crop == "Other")
      aux <- dfphe

      if (oth != "") {
        # aux$Crop <- oth
        # aux
        aux <- dfphe
      } else {
        aux
      }
    } else {
      aux <- dfphe[0,]
    }
  }

  dtMonocropphe <- pheno_vars

  #dtMonocropphe <- fmonophe()
  output$tblMonoPhe = renderDT(
    datatable(
      dtMonocropphe <<- fmonophe(),
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      )
    )# %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )

  # proxyMonoPhe = dataTableProxy('tblMonoPhe')
  #
  # observeEvent(input$tblMonoPhe_cell_edit, {
  #   info = input$tblMonoPhe_cell_edit
  #   #str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   dtMonocropphe[i, j] <<- DT::coerceValue(v, dtMonocropphe[i, j])
  #   replaceData(proxyMono, dtMonocropphe, resetPaging = FALSE, clearSelection = "none")
  # })
  #### End Crop Phenology Monocrop ####

  #### Start Crop Phenology Intercrop ####
  finterphe <- function(crop_in) {
    #crop_in <- input$cropCommonNameMono
    oth <- input$cropCommonNameMono_other

    if (!is.null(crop_in) && crop_in != "Other") {
      #aux <- dplyr::filter(dfmea, Crop == crop_in)
      aux <- dfphe
    } else if(!is.null(crop_in) && crop_in == "Other") {
      #aux <- dplyr::filter(dfmea, Crop == "Other")
      aux <- dfphe

      if (oth != "") {
        # aux$Crop <- oth
        # aux
        aux <- dfphe
      } else {
        aux
      }
    } else {
      aux <- dfphe[0,]
    }
  }

  # Cassava
  dtInterPheCassava <- data.frame()
  output$tblInterPheCassava = renderDT(
    datatable(
      dtInterPheCassava <<- finterphe("Cassava"),
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )

  # proxyInterCassava = dataTableProxy('tblInterPheCassava')
  #
  # observeEvent(input$tblInterPheCassava_cell_edit, {
  #   info = input$tblInterPheCassava_cell_edit
  #   #str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   dtInterPheCassava[i, j] <<- DT::coerceValue(v, dtInterPheCassava[i, j])
  #   replaceData(proxyInterCassava, dtInterPheCassava, resetPaging = FALSE, clearSelection = "none")
  # })

  # Common bean
  dtInterPheCommon <- data.frame()
  output$tblInterPheCommon = renderDT(
    datatable(
      dtInterPheCommon <<- finterphe("Common bean"),
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )

  # proxyInterCommonbean = dataTableProxy('tblInterPheCommon')
  #
  # observeEvent(input$tblInterPheCommon_cell_edit, {
  #   info = input$tblInterPheCommon_cell_edit
  #   #str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   dtInterPheCommon[i, j] <<- DT::coerceValue(v, dtInterPheCommon[i, j])
  #   replaceData(proxyInterCommonbean, dtInterPheCommon, resetPaging = FALSE, clearSelection = "none")
  # })

  # Maize
  dtInterPheMaize <- data.frame()
  output$tblInterPheMaize = renderDT(
    datatable(
      dtInterPheMaize <<- finterphe("Maize"),
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )

  # proxyInterMaize = dataTableProxy('tblInterPheMaize')
  #
  # observeEvent(input$tblInterPheMaize_cell_edit, {
  #   info = input$tblInterPheMaize_cell_edit
  #   #str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   dtInterPheMaize[i, j] <<- DT::coerceValue(v, dtInterPheMaize[i, j])
  #   replaceData(proxyInterMaize, dtInterPheMaize, resetPaging = FALSE, clearSelection = "none")
  # })

  # Potato
  dtInterPhePotato <- data.frame()
  output$tblInterPhePotato = renderDT(
    datatable(
      dtInterPhePotato <<- finterphe("Potato"),
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )

  # proxyInterPotato = dataTableProxy('tblInterPhePotato')
  #
  # observeEvent(input$tblInterPhePotato_cell_edit, {
  #   info = input$tblInterPhePotato_cell_edit
  #   #str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   dtInterPhePotato[i, j] <<- DT::coerceValue(v, dtInterPhePotato[i, j])
  #   replaceData(proxyInterPotato, dtInterPhePotato, resetPaging = FALSE, clearSelection = "none")
  # })

  # Rice
  dtInterPheRice <- data.frame()
  output$tblInterPheRice = renderDT(
    datatable(
      dtInterPheRice <<- finterphe("Rice"),
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )

  # proxyInterRice = dataTableProxy('tblInterPheRice')
  #
  # observeEvent(input$tblInterPheRice_cell_edit, {
  #   info = input$tblInterPheRice_cell_edit
  #   #str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   dtInterPheRice[i, j] <<- DT::coerceValue(v, dtInterPheRice[i, j])
  #   replaceData(proxyInterRice, dtInterPheRice, resetPaging = FALSE, clearSelection = "none")
  # })

  # Sweetpotato
  dtInterPheSweetpotato <- data.frame()
  output$tblInterPheSweetpotato = renderDT(
    datatable(
      dtInterPheSweetpotato <<- finterphe("Sweetpotato"),
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )

  # proxyInterSweetpotato = dataTableProxy('tblInterPheSweetpotato')
  #
  # observeEvent(input$tblInterPheSweetpotato_cell_edit, {
  #   info = input$tblInterPheSweetpotato_cell_edit
  #   #str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   dtInterPheSweetpotato[i, j] <<- DT::coerceValue(v, dtInterPheSweetpotato[i, j])
  #   replaceData(proxyInterSweetpotato, dtInterPheSweetpotato, resetPaging = FALSE, clearSelection = "none")
  # })

  # Wheat
  dtInterPheWheat <- data.frame()
  output$tblInterPheWheat = renderDT(
    datatable(
      dtInterPheWheat <<- finterphe("Wheat"),
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )

  # proxyInterWheat = dataTableProxy('tblInterPheWheat')
  #
  # observeEvent(input$tblInterPheWheat_cell_edit, {
  #   info = input$tblInterPheWheat_cell_edit
  #   #str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   dtInterPheWheat[i, j] <<- DT::coerceValue(v, dtInterPheWheat[i, j])
  #   replaceData(proxyInterWheat, dtInterPheWheat, resetPaging = FALSE, clearSelection = "none")
  # })

  ## Funcione para Others de Intercrop: Crop Phenology
  
  # Funcion que hace clic al boton cada vez que cambiamos de tap panel, asi ejecuta el update del DT de others
  # observeEvent(input$fbDesignNav, {
  #   shinyjs::click("dop")
  # })
  
  # Funcion que oculta el boton por defecto a vista del usuario
  # observe({
  #   shinyjs::hide("dop")
  # })
  
  # Other 1 Phe: ===================================================
  # finterPOt1 <- eventReactive(input$dop, {
  #   id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
  #   
  #   crop_id <- as.character(input[[paste0("int_cropCommonName_", id_ic_rand[1])]])
  #   oth <- as.character(input[[paste0("int_cropCommonName_", id_ic_rand[1], "_other")]])
  #   
  #   if (nrow(dtInterPheOther1) >= 1) {
  #     
  #     if (crop_id == "Other" && dtInterPheOther1[1, 1] != oth) {
  #       fp1()
  #     } else {
  #       dtInterPheOther1
  #     }
  #     
  #   } else {
  #     fp1()
  #   }
  #   
  # }, ignoreNULL = FALSE)
  # 
  # fp1 <- function() {
  #   id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
  #   
  #   # Cambiar el parametro numerico [?] para cada other
  #   o <- paste0("int_cropCommonName_", id_ic_rand[1], "_other")
  #   oth <- as.character(input[[o]])
  #   
  #   #aux <- dplyr::filter(dfphe, Crop == "Other")
  #   aux <- dfphe
  #   
  #   if (oth != "") {
  #     # aux$Crop <- oth
  #     # aux
  #     aux <- dfphe
  #   } else {
  #     aux
  #   }
  # }
  
  dtInterPheOther1 <- data.frame()
  output$tblInterPheOther1 = renderDT(
    datatable(
      dtInterPheOther1 <<- dfphe,
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
  )
  
  # Other 2 Phe: ===================================================
  dtInterPheOther2 <- data.frame()
  output$tblInterPheOther2 = renderDT(
    datatable(
      dtInterPheOther2 <<- dfphe,
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
  )
  
  # Other 3 Phe: ===================================================
  dtInterPheOther3 <- data.frame()
  output$tblInterPheOther3 = renderDT(
    datatable(
      dtInterPheOther3 <<- dfphe,
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
  )
  
  # Other 4 Phe: ===================================================
  dtInterPheOther4 <- data.frame()
  output$tblInterPheOther4 = renderDT(
    datatable(
      dtInterPheOther4 <<- dfphe,
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
  )
  
  # Other 5 Phe: ===================================================
  dtInterPheOther5 <- data.frame()
  output$tblInterPheOther5 = renderDT(
    datatable(
      dtInterPheOther5 <<- dfphe,
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
  )
  #### End Crop Phenology Intercrop ####
  
  #### Start Crop Phenology Relay crop ####
  frelayphe <- function(crop_in) {
    #crop_in <- input$cropCommonNameMono
    oth <- input$cropCommonNameMono_other
    
    if (!is.null(crop_in) && crop_in != "Other") {
      #aux <- dplyr::filter(dfmea, Crop == crop_in)
      aux <- dfphe
    } else if(!is.null(crop_in) && crop_in == "Other") {
      #aux <- dplyr::filter(dfmea, Crop == "Other")
      aux <- dfphe
      
      if (oth != "") {
        # aux$Crop <- oth
        # aux
        aux <- dfphe
      } else {
        aux
      }
    } else {
      aux <- dfphe[0,]
    }
  }
  
  # Cassava
  dtRelayPheCassava <- data.frame()
  output$tblRelayPheCassava = renderDT(
    datatable(
      dtRelayPheCassava <<- frelayphe("Cassava"),
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )
  
  # proxyInterCassava = dataTableProxy('tblInterPheCassava')
  #
  # observeEvent(input$tblInterPheCassava_cell_edit, {
  #   info = input$tblInterPheCassava_cell_edit
  #   #str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   dtInterPheCassava[i, j] <<- DT::coerceValue(v, dtInterPheCassava[i, j])
  #   replaceData(proxyInterCassava, dtInterPheCassava, resetPaging = FALSE, clearSelection = "none")
  # })
  
  # Common bean
  dtRelayPheCommon <- data.frame()
  output$tblRelayPheCommon = renderDT(
    datatable(
      dtRelayPheCommon <<- frelayphe("Common bean"),
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )
  
  # proxyInterCommonbean = dataTableProxy('tblInterPheCommon')
  #
  # observeEvent(input$tblInterPheCommon_cell_edit, {
  #   info = input$tblInterPheCommon_cell_edit
  #   #str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   dtInterPheCommon[i, j] <<- DT::coerceValue(v, dtInterPheCommon[i, j])
  #   replaceData(proxyInterCommonbean, dtInterPheCommon, resetPaging = FALSE, clearSelection = "none")
  # })
  
  # Maize
  dtRelayPheMaize <- data.frame()
  output$tblRelayPheMaize = renderDT(
    datatable(
      dtRelayPheMaize <<- frelayphe("Maize"),
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )
  
  # proxyInterMaize = dataTableProxy('tblInterPheMaize')
  #
  # observeEvent(input$tblInterPheMaize_cell_edit, {
  #   info = input$tblInterPheMaize_cell_edit
  #   #str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   dtInterPheMaize[i, j] <<- DT::coerceValue(v, dtInterPheMaize[i, j])
  #   replaceData(proxyInterMaize, dtInterPheMaize, resetPaging = FALSE, clearSelection = "none")
  # })
  
  # Potato
  dtRelayPhePotato <- data.frame()
  output$tblRelayPhePotato = renderDT(
    datatable(
      dtRelayPhePotato <<- frelayphe("Potato"),
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )
  
  # proxyInterPotato = dataTableProxy('tblInterPhePotato')
  #
  # observeEvent(input$tblInterPhePotato_cell_edit, {
  #   info = input$tblInterPhePotato_cell_edit
  #   #str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   dtInterPhePotato[i, j] <<- DT::coerceValue(v, dtInterPhePotato[i, j])
  #   replaceData(proxyInterPotato, dtInterPhePotato, resetPaging = FALSE, clearSelection = "none")
  # })
  
  # Rice
  dtRelayPheRice <- data.frame()
  output$tblRelayPheRice = renderDT(
    datatable(
      dtRelayPheRice <<- frelayphe("Rice"),
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )
  
  # proxyInterRice = dataTableProxy('tblInterPheRice')
  #
  # observeEvent(input$tblInterPheRice_cell_edit, {
  #   info = input$tblInterPheRice_cell_edit
  #   #str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   dtInterPheRice[i, j] <<- DT::coerceValue(v, dtInterPheRice[i, j])
  #   replaceData(proxyInterRice, dtInterPheRice, resetPaging = FALSE, clearSelection = "none")
  # })
  
  # Sweetpotato
  dtRelayPheSweetpotato <- data.frame()
  output$tblRelayPheSweetpotato = renderDT(
    datatable(
      dtRelayPheSweetpotato <<- frelayphe("Sweetpotato"),
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )
  
  # proxyInterSweetpotato = dataTableProxy('tblInterPheSweetpotato')
  #
  # observeEvent(input$tblInterPheSweetpotato_cell_edit, {
  #   info = input$tblInterPheSweetpotato_cell_edit
  #   #str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   dtInterPheSweetpotato[i, j] <<- DT::coerceValue(v, dtInterPheSweetpotato[i, j])
  #   replaceData(proxyInterSweetpotato, dtInterPheSweetpotato, resetPaging = FALSE, clearSelection = "none")
  # })
  
  # Wheat
  dtRelayPheWheat <- data.frame()
  output$tblRelayPheWheat = renderDT(
    datatable(
      dtRelayPheWheat <<- frelayphe("Wheat"),
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )
  
  # proxyInterWheat = dataTableProxy('tblInterPheWheat')
  #
  # observeEvent(input$tblInterPheWheat_cell_edit, {
  #   info = input$tblInterPheWheat_cell_edit
  #   #str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   dtInterPheWheat[i, j] <<- DT::coerceValue(v, dtInterPheWheat[i, j])
  #   replaceData(proxyInterWheat, dtInterPheWheat, resetPaging = FALSE, clearSelection = "none")
  # })
  
  ## Funcione para Others de Intercrop: Crop Phenology
  
  # Funcion que hace clic al boton cada vez que cambiamos de tap panel, asi ejecuta el update del DT de others
  # observeEvent(input$fbDesignNav, {
  #   shinyjs::click("dop")
  # })
  
  # Funcion que oculta el boton por defecto a vista del usuario
  # observe({
  #   shinyjs::hide("dop")
  # })
  
  # Other 1 Phe: ===================================================
  # finterPOt1 <- eventReactive(input$dop, {
  #   id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
  #   
  #   crop_id <- as.character(input[[paste0("int_cropCommonName_", id_ic_rand[1])]])
  #   oth <- as.character(input[[paste0("int_cropCommonName_", id_ic_rand[1], "_other")]])
  #   
  #   if (nrow(dtInterPheOther1) >= 1) {
  #     
  #     if (crop_id == "Other" && dtInterPheOther1[1, 1] != oth) {
  #       fp1()
  #     } else {
  #       dtInterPheOther1
  #     }
  #     
  #   } else {
  #     fp1()
  #   }
  #   
  # }, ignoreNULL = FALSE)
  # 
  # fp1 <- function() {
  #   id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
  #   
  #   # Cambiar el parametro numerico [?] para cada other
  #   o <- paste0("int_cropCommonName_", id_ic_rand[1], "_other")
  #   oth <- as.character(input[[o]])
  #   
  #   #aux <- dplyr::filter(dfphe, Crop == "Other")
  #   aux <- dfphe
  #   
  #   if (oth != "") {
  #     # aux$Crop <- oth
  #     # aux
  #     aux <- dfphe
  #   } else {
  #     aux
  #   }
  # }
  
  dtRelayPheOther1 <<- data.frame()
  output$tblRelayPheOther1 = renderDT(
    datatable(
      dtRelayPheOther1 <<- dfphe,
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
  )
  
  # Other 2 Phe: ===================================================
  dtRelayPheOther2 <<- data.frame()
  output$tblRelayPheOther2 = renderDT(
    datatable(
      dtRelayPheOther2 <<- dfphe,
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
  )
  
  # Other 3 Phe: ===================================================
  dtRelayPheOther3 <<- data.frame()
  output$tblRelayPheOther3 = renderDT(
    datatable(
      dtRelayPheOther3 <<- dfphe,
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
  )
  
  # Other 4 Phe: ===================================================
  dtRelayPheOther4 <<- data.frame()
  output$tblRelayPheOther4 = renderDT(
    datatable(
      dtRelayPheOther4 <<- dfphe,
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
  )
  
  # Other 5 Phe: ===================================================
  dtRelayPheOther5 <<- data.frame()
  output$tblRelayPheOther5 = renderDT(
    datatable(
      dtRelayPheOther5 <<- dfphe,
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,7,8,9,10,11,12,13)))
      ))
  )
  #### End Crop Phenology Relay crop ####

  ######## End Crop Phenology Ultima Version #########

  # oculto fin
  
  
  
  
  #### Start Weather Ultima version ####
  output$uiWeatherTab2 <- renderUI({
    DTOutput("tblWeather")
  })
  
  fweather <- function(){
    # crop_in <- input$cropCommonNameMono
    # oth <- input$cropCommonNameMono_other
    # 
    # if (!is.null(crop_in) && crop_in != "Other") {
    #   #aux <- dplyr::filter(dfmea, Crop == crop_in)
    #   aux <- dfphe
    # } else if(!is.null(crop_in) && crop_in == "Other") {
    #   #aux <- dplyr::filter(dfmea, Crop == "Other")
    #   aux <- dfphe
    #   
    #   if (oth != "") {
    #     # aux$Crop <- oth
    #     # aux
    #     aux <- dfphe
    #   } else {
    #     aux
    #   }
    # } else {
    #   aux <- dfphe[0,]
    # }
    dt<- as.data.frame(weather_station_vars,stringsAsFactors=FALSE)
    colnames(dt) <- c("Crop", 
                      "Group",
                      "Subgroup",
                      "Measurement",
                      "TraitUnit",
                      "NumberOfMeasurmentsPerSeason",
                      "NumberOfMeasurmentsPerPlot",
                      "TraitName",
                      "TraitAlias",
                      "TraitDataType",
                      "TraitValidation",
                      "VariableId")
    dt
  }
  
  #dtWeather<- data.frame()
  
  dtWeather<- data.frame()
  
  output$tblWeather = renderDT(
    datatable(
      dtWeather <<- fweather(),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,8,9,10,11,12)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )
  
  proxyWeather = dataTableProxy('tblWeather')

  observeEvent(input$tblWeather_cell_edit, {
    info = input$tblWeather_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtWeather[i, j] <<- DT::coerceValue(v, dtWeather[i, j])
    replaceData(proxyWeather, dtWeather, resetPaging = FALSE, clearSelection = "none")
  })
  #### End Weather Ultima version ####
  
  #### Start Soil Ultima version ####
  output$uiSoilTab2 <- renderUI({
    DTOutput("tblSoil")
  })
  
  fsoil <- function(){
    # crop_in <- input$cropCommonNameMono
    # oth <- input$cropCommonNameMono_other
    # 
    # if (!is.null(crop_in) && crop_in != "Other") {
    #   #aux <- dplyr::filter(dfmea, Crop == crop_in)
    #   aux <- dfphe
    # } else if(!is.null(crop_in) && crop_in == "Other") {
    #   #aux <- dplyr::filter(dfmea, Crop == "Other")
    #   aux <- dfphe
    #   
    #   if (oth != "") {
    #     # aux$Crop <- oth
    #     # aux
    #     aux <- dfphe
    #   } else {
    #     aux
    #   }
    # } else {
    #   aux <- dfphe[0,]
    # }
    dt<- as.data.frame(soil_data,stringsAsFactors=FALSE)
    colnames(dt) <- c("Crop", 
                      "Group",
                      "Subgroup",
                      "Measurement",
                      "TraitUnit",
                      "NumberOfMeasurmentsPerSeason",
                      "NumberOfMeasurmentsPerPlot",
                      "TraitName",
                      "TraitAlias",
                      "TraitDataType",
                      "TraitValidation",
                      "VariableId")
    dt
  }
  dtSoil <- data.frame()
  output$tblSoil = renderDT(
    datatable(
      dtSoil <<- fsoil(),
      selection = 'multiple',
      editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,8,9,10,11,12)))
      ))
    # ) %>% formatStyle(
    #   c("Crop measurement per season", "Crop measurement per plot"),
    #   backgroundColor = ("lightblue")
    # )
  )
  
  proxySoil = dataTableProxy('tblSoil')
  
  observeEvent(input$tblSoil_cell_edit, {
    info = input$tblSoil_cell_edit
    #str(info)
    i = info$row
    j = info$col
    v = info$value
    dtSoil[i, j] <<- DT::coerceValue(v, dtSoil[i, j])
    replaceData(proxySoil, dtSoil, resetPaging = FALSE, clearSelection = "none")
  })
  #### End Soil Ultima version ####
 
  
  
  ### continuar borrando aqui....
  
  
  #######
  
  ### start crop measurement 1 ###

  traitsVals <- reactiveValues()
  traitsVals$aux <- data.frame()
  traitsVals$selectedRows <- list()
  traitsVals$Data <- data.table()

  #dict <- readRDS("/home/obenites/HIDAP_SB_1.0.0/hidap/inst/hidap_agrofims/www/internal_files/crop_measurements_v4.rds")
  #dict <- readRDS(paste0(globalpath, "crop_measurements_v5.rds"))
  #dict <- readRDS(paste0(globalpath, "crop_measurements_v6.rds"))
  dict <- readRDS(paste0(globalpath, "crop_measurements_v6.2.rds"))
  dict <- as.data.frame(dict, stringsAsFactors=FALSE)
  # colnames(dict) <- c("Status", "Crop", "Group", "Subgroup", "Measurement", "a", 
  #                     "b", "TraitUnit", "c", "d", "e", "g", "Crop measurement per season", 
  #                     "h", "Crop measurement per plot")

  # observe({
  #   aux <- NULL
  #   rs <- "Other"
  #   newVal <- "Other"
  # 
  #    if(input$croppingType == "Monocrop"){
  #     if(!is.null(input$cropCommonNameMono)){
  #       valSel <- input$cropCommonNameMono[1]
  #       aux <- dplyr::filter(as.data.frame(dict),Crop==valSel)
  # 
  #       if(valSel == "Other") {
  #         newVal <- trim(input$cropCommonNameMono_other)
  #         if(newVal == "") newVal <- "Other"
  #         aux$Crop<- rep(newVal, length(aux$Crop))
  #       }
  #       traitsVals$otherMonocropPrev <-  newVal
  # 
  #     }
  #     else{
  #       aux <- dplyr::filter(as.data.frame(dict),Crop=="AnyValueToCreateEmptyFrame")
  #     }
  #   }
  #   else if(input$croppingType == "Intercrop"){
  #     crop_selected <- input$cropsSelected
  #     aux <- dplyr::filter(as.data.frame(dict), Crop %in% crop_selected)
  #     
  #     if("Other" %in% crop_selected){
  #       newVal <- trim(cropsVar$varAuxOtherIntercrop)
  #       if( newVal == "" ) newVal <- "Other"
  #       aux$Crop<- gsub("Other", newVal, aux$Crop)
  #     }
  #   }
  #   traitsVals$Data <- data.table(aux)
  # })
  
  
  # expCondsVars$num_harvest <- 0
  # expCondsVars$DEFAULT_harvest <- 1
  # expCondsVars$ids_harvest <- c()
  # 
  # observeEvent(input$croppingType,{
  # 
  #   if(input$croppingType == "Monocrop"){
  #     # removeTab(inputId = "fbDesignNav",target = "Crop_Measurement_intercrop")
  #     # removeTab(inputId = "fbDesignNav",target = "Crop_Phenology_intercrop")
  #     # 
  #     # insertTab(inputId = "fbDesignNav",
  #     #           tabPanel("Crop Measurement",  value = "Crop_Measurement_monocrop", icon = shiny::icon("leaf"),
  #     #                    column(width = 12,
  #     #                           h2("Crop measurement"),
  #     #                           p(class = "text-muted", style="text-align:justify",
  #     #                             paste("Please, select measurement by click.")
  #     #                           ),
  #     #                           column(12, align = "center", checkboxInput("dt_sel", "Select all")),
  #     #                           br(),br()
  #     #                    ),
  #     #                    uiOutput("uiTraitsList3"),
  #     #                    sidebarPanel(id="sidebar", width = 12,
  #     #                                 actionButton("btnNextEnv", "Next", class = "btn-primary",style="color: #fff;", href="#top")
  #     #                    )
  #     #           ),
  #     #           position =  "after",
  #     #           target = "tabAgroFeat")
  #     # insertTab(inputId = "fbDesignNav",
  #     #           tabPanel("Crop Phenology", value = "Crop_Phenology_monocrop",  icon = shiny::icon("envira"),
  #     #                    column(width = 12,
  #     #                           h2("Crop Phenology"),
  #     #                           p(class = "text-muted", style="text-align:justify",
  #     #                             paste("Please, select phenology by click.")
  #     #                           ),
  #     #                           #column(12, align = "center", checkboxInput("dt_sel", "Select all"))
  #     #                           DTOutput('phenoDT')
  #     # 
  #     #                    )#,
  #     #                    #DTOutput('phenoDT')
  #     #           ),
  #     #           position =  "after",
  #     #           target = "Crop_Measurement_monocrop")
  # 
  #     isolate(removeAgroBoxesIntercrop())
  #     isolate(drawAgroBoxes(1))
  #     shinyjs::show(id="addHarvest")
  # 
  #   }
  #   else if(input$croppingType == "Intercrop"){
  # 
  #     # removeTab(inputId = "fbDesignNav",target = "Crop_Measurement_monocrop")
  #     # removeTab(inputId = "fbDesignNav",target = "Crop_Phenology_monocrop")
  #     # 
  #     # 
  #     isolate(ids <- intercropVars$ids)
  #     tt <- unlist(strsplit(ids[1],"_"))
  #     # 
  #     # insertTab(inputId = "fbDesignNav",
  #     #        tabPanel("Crop Measurement",  value = "Crop_Measurement_intercrop", icon = shiny::icon("leaf"),
  #     #                 column(12, h2("Crop Measurement"),
  #     #                 tabsetPanel( id= "intercropMeasuTabs",
  #     #                              tabPanel(title = textOutput(paste0("intercrop_tab_measu_title_",tt[2])), value = paste0("intercrop_tab_measu_",tt[2]),
  #     #                                       br(),
  #     #                                       column(12,DTOutput(paste0("crop_measurement_table_", tt[2])))
  #     #                              )
  #     #                 ))
  #     # 
  #     #        ),
  #     #       position =  "after",
  #     #       target = "tabAgroFeat")
  #     # 
  #     # insertTab(inputId = "fbDesignNav",
  #     #         tabPanel("Crop Phenology",  value = "Crop_Phenology_intercrop", icon = shiny::icon("envira"),
  #     #                  column(12, h2("Crop Phenology"),
  #     #                   tabsetPanel( id= "intercropPhenoTabs",
  #     #                                tabPanel(title = textOutput(paste0("intercrop_tab_pheno_title_",tt[2])), value = paste0("intercrop_tab_pheno_",tt[2]),
  #     #                                         br(),
  #     #                                         column(12,renderDataTable(pheno_vars , options = list(lengthChange = FALSE)))
  #     #                                )
  #     #                   ))
  #     #          ),
  #     #         position =  "after",
  #     #         target = "Crop_Measurement_intercrop")
  # 
  #    #isolate(drawTabsIntercrop(ids))
  #    isolate(removeAgroBoxesMonocrop())
  #    expCondsVars$ids_harvest <- c()
  #    isolate(drawAgroBoxesIntercrop())
  #    shinyjs::hide(id="addHarvest")
  #   }
  # })
  # 
  # drawTabsIntercrop <- function(ids){
  #   tt <- unlist(strsplit(ids[1],"_"))
  #   
  #   mtarget <- paste0("intercrop_tab_measu_",tt[2])
  #   ptarget <- paste0("intercrop_tab_pheno_",tt[2])
  #   intercropVars$pheno[[tt[2]]] <- pheno_vars
  #   
  #   xtitle <- input[[paste0("int_cropCommonName_", tt[2])]]
  #   mcrop <- xtitle
  #   if(is.null(mcrop)) mcrop <- "Crop" 
  #   
  #   updateCropMeasurementTable(tt[2],mcrop)
  #   
  #   if(!is.null(xtitle)){
  #     if(xtitle == "Other"  && input[[paste0("int_cropCommonName_", tt[2]), "_other"]] != "")  
  #       xtitle <- input[[paste0("int_cropCommonName_", tt[2]), "_other"]]
  #     else xtitle <- input[[paste0("int_cropCommonName_", tt[2])]]
  #   }
  #   else {
  #     xtitle= "Crop"
  #   }
  #   
  #   
  #   mtitle <- paste0(xtitle, " Measurement")
  #   ptitle <- paste0(xtitle, " Phenology")
  #   
  # 
  #   isolate(renameTab(paste0("intercrop_tab_measu_title_",tt[2]), mtitle))
  #   isolate(renameTab(paste0("intercrop_tab_pheno_title_",tt[2]), ptitle))
  #   
  #   len <- length(ids)
  #   if(len < 2) return()
  #   
  #   for(ind  in 2:len){
  #     vars <- unlist(strsplit(ids[ind],"_")) 
  #     isolate(insertTabInterCrop(vars[2], mtarget, ptarget))
  #     ptarget <- paste0("intercrop_tab_pheno_",vars[2])
  #     mtarget <- paste0("intercrop_tab_measu_",vars[2])
  #   }
  #   
  #   for(ind  in 2:len){
  #     vars <- unlist(strsplit(ids[ind],"_")) 
  #     isolate(insertTabRelayCrop(vars[2], mtarget, ptarget))
  #     ptarget <- paste0("relaycrop_tab_pheno_",vars[2])
  #     mtarget <- paste0("relaycrop_tab_measu_",vars[2])
  #   }
  #   
  #   for(ind  in 2:len){
  #     vars <- unlist(strsplit(ids[ind],"_")) 
  #     isolate(insertTabRotationCrop(vars[2], mtarget, ptarget))
  #     ptarget <- paste0("rotationcrop_tab_pheno_",vars[2])
  #     mtarget <- paste0("rotationcrop_tab_measu_",vars[2])
  #   }
  # }
  # 
  # insertTabInterCrop <- function(index, mtarget, ptarget){
  #   
  #   xtitle <- input[[paste0("int_cropCommonName_", index)]]
  #   
  #   mcrop <- xtitle
  #   if(is.null(mcrop)) mcrop <- "Crop"
  #   
  #   if(!is.null(xtitle)){
  #     if(xtitle == "Other"  && input[[paste0("int_cropCommonName_", index, "_other")]] != "")  
  #       xtitle <- input[[paste0("int_cropCommonName_", index, "_other")]]
  #     else xtitle <- input[[paste0("int_cropCommonName_", index)]]
  #   }
  #   else {
  #     xtitle= "Crop"
  #   }
  #   
  #   
  #   mtitle <- paste0(xtitle, " Measurement")
  #   ptitle <- paste0(xtitle, " Phenology")
  #   
  #   intercropVars$pheno[[index]] <- pheno_vars
  #   insertTab(inputId = "intercropPhenoTabs",
  #             tabPanel(title = textOutput(paste0("intercrop_tab_pheno_title_",index)) , value = paste0("intercrop_tab_pheno_",index), 
  #                      br(),
  #                      column(12,renderDataTable(intercropVars$pheno[[index]] , options = list(lengthChange = FALSE)))
  #             ), 
  #             position="after",
  #             target = ptarget
  #   )
  #   
  #   insertTab(inputId = "intercropMeasuTabs",
  #             tabPanel(title = textOutput(paste0("intercrop_tab_measu_title_",index)) , value = paste0("intercrop_tab_measu_",index),
  #                      br(),
  #                      column(12,DTOutput(paste0("crop_measurement_table_", index)))
  #             ), 
  #             position="after",
  #             target = mtarget
  #   )
  #   
  #   
  #   #isolate(renameTab(paste0("intercrop_tab_measu_title_",index), mtitle))
  #   #isolate(renameTab(paste0("intercrop_tab_pheno_title_",index), ptitle))
  #   #updateCropMeasurementTable(index,mcrop)
  # }
  # 
  # ##Insert Relay Crop
  # 
  # insertTabRelayCrop <- function(index, mtarget, ptarget){
  #   
  #   xtitle <- input[[paste0("rel_cropCommonName_", index)]]
  #   
  #   mcrop <- xtitle
  #   if(is.null(mcrop)) mcrop <- "Crop"
  #   
  #   if(!is.null(xtitle)){
  #     if(xtitle == "Other"  && input[[paste0("rel_cropCommonName_", index, "_other")]] != "")  
  #       xtitle <- input[[paste0("rel_cropCommonName_", index, "_other")]]
  #     else xtitle <- input[[paste0("rel_cropCommonName_", index)]]
  #   }
  #   else {
  #     xtitle= "Crop"
  #   }
  #   
  #   
  #   mtitle <- paste0(xtitle, " Measurement")
  #   ptitle <- paste0(xtitle, " Phenology")
  #   
  #   relaycropVars$pheno[[index]] <- pheno_vars
  #   insertTab(inputId = "relaycropPhenoTabs",
  #             tabPanel(title = textOutput(paste0("relaycrop_tab_pheno_title_",index)) , value = paste0("relaycrop_tab_pheno_",index), 
  #                      br(),
  #                      column(12,renderDataTable(relaycropVars$pheno[[index]] , options = list(lengthChange = FALSE)))
  #             ), 
  #             position="after",
  #             target = ptarget
  #   )
  #   
  #   insertTab(inputId = "relaycropMeasuTabs",
  #             tabPanel(title = textOutput(paste0("relaycrop_tab_measu_title_",index)) , value = paste0("relaycrop_tab_measu_",index),
  #                      br(),
  #                      column(12,DTOutput(paste0("crop_measurement_table_", index)))
  #             ), 
  #             position="after",
  #             target = mtarget
  #   )
  #   
  #   
  #   isolate(renameTab(paste0("relaycrop_tab_measu_title_",index), mtitle))
  #   isolate(renameTab(paste0("relaycrop_tab_pheno_title_",index), ptitle))
  #   updateCropMeasurementTable(index,mcrop)
  # }
  # 
  # 
  # ##Insert Rotation Crop
  # 
  # insertTabRotationCrop <- function(index, mtarget, ptarget){
  #   
  #   xtitle <- input[[paste0("cropCommonNameRotation_", index)]]
  #   
  #   mcrop <- xtitle
  #   if(is.null(mcrop)) mcrop <- "Crop"
  #   
  #   if(!is.null(xtitle)){
  #     if(xtitle == "Other"  && input[[paste0("cropCommonNameRotation_", index, "_other")]] != "")  
  #       xtitle <- input[[paste0("cropCommonNameRotation_", index, "_other")]]
  #     else xtitle <- input[[paste0("cropCommonNameRotation_", index)]]
  #   }
  #   else {
  #     xtitle= "Crop"
  #   }
  #   
  #   
  #   mtitle <- paste0(xtitle, " Measurement")
  #   ptitle <- paste0(xtitle, " Phenology")
  #   
  #   rotationcropVars$pheno[[index]] <- pheno_vars
  #   insertTab(inputId = "rotationcropPhenoTabs",
  #             tabPanel(title = textOutput(paste0("rotationcrop_tab_pheno_title_",index)) , value = paste0("rotationcrop_tab_pheno_",index), 
  #                      br(),
  #                      column(12,renderDataTable(rotationcropVars$pheno[[index]] , options = list(lengthChange = FALSE)))
  #             ), 
  #             position="after",
  #             target = ptarget
  #   )
  #   
  #   insertTab(inputId = "rotationcropMeasuTabs",
  #             tabPanel(title = textOutput(paste0("rotationcrop_tab_measu_title_",index)) , value = paste0("rotationcrop_tab_measu_",index),
  #                      br(),
  #                      column(12,DTOutput(paste0("crop_measurement_table_", index)))
  #             ), 
  #             position="after",
  #             target = mtarget
  #   )
  #   
  #   
  #   isolate(renameTab(paste0("rotationcrop_tab_measu_title_",index), mtitle))
  #   isolate(renameTab(paste0("rotationcrop_tab_pheno_title_",index), ptitle))
  #   updateCropMeasurementTable(index,mcrop)
  # }
  # 
  # #Function to Update Crop Measurement Table for Intercrop Trials
  # 
  # updateCropMeasurementTable <- function(index, crop_in){
  #   aux <- dplyr::filter(as.data.frame(dict),Crop==crop_in)
  # 
  #   if(crop_in == "Other") {
  #     newVal <- trim(input[[paste0("int_cropCommonName_", index, "_other")]])
  #     if(newVal == "") newVal <- "Other"
  #     aux$Crop<- rep(newVal, length(aux$Crop))
  #   }
  #   
  #   ## Create DT crop measurement tables for Intercrop trial
  #   output[[paste0("crop_measurement_table_", index)]] <- renderDataTable(
  #     data.table(aux),
  #     escape = FALSE,
  #     options = list(
  #       scrollX = TRUE,
  #       pageLength = 25,
  #       #columnDefs = list(list(visible=FALSE, targets=c(1,6)))
  #       columnDefs = list(list(visible=FALSE, targets=c(1,6,7,9,10,11,12,13,14,15)))#,
  #     )
  #   )
  # }
  # 
  # 
  # renameTab <- function(id, name){
  #   output[[id]] <- renderText({name})
  # }
  
  output$uiWeatherTab <- renderUI({
    column(12, 
           br(),
           # fluidRow(
           #   box(status = "primary", width = 12,collapsible = TRUE, collapsed = TRUE,solidHeader = TRUE,
           #       id="manual_measurement_boxid",
           #       title = checkboxInput("manualMeasurement_checkbox", actionLink("manual_measurement_titleId", "Manual measurement"), F),
           #       DTOutput('weatherManualDT')
           #   )
           #   
           # ),
           # br(),
           # fluidRow(
           #   box(status = "primary",width = 12, collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
           #       id="station_measurement_boxid",
           #       title = checkboxInput("stationMeasurement_checkbox", actionLink("station_measurement_titleId", "Weather station measurement"), F),
           #       DTOutput('weatherStationDT')
           #   )
           # )
           fluidRow(
             box(status = "primary",width = 12, collapsible = TRUE, collapsed = F, solidHeader = TRUE,
                 id="station_measurement_boxid",
                 #title = checkboxInput("stationMeasurement_checkbox", actionLink("station_measurement_titleId", "Weather measurement"), F),
                 title = "Weather measurement",
                 DTOutput('weatherStationDT')
             )
           )
           
           
           )
  })
  
  output$uiSoilTab <- renderUI({
      DTOutput('soilDT')
  })
  
  observeEvent(input$addHarvest,{
    str_id <- stri_rand_strings(1, 8,  '[A-Z]')
    expCondsVars$ids_harvest <- c(expCondsVars$ids_harvest, paste0(str_id))
    expCondsVars$num_harvest <- expCondsVars$num_harvest + 1
    drawBoxHarvest(str_id)
    output[[paste0("harvest_title_", str_id)]] <- renderText({"Harvest details"})
  })
  
  
  removeAgroBoxesIntercrop <- function(){
    isolate(ids <- intercropVars$ids)
    for( id in ids){
      vars <- unlist(strsplit(id,"_")) 
      removeAgroBoxes(vars[2])
    }
  }
  
  removeAgroBoxes <- function(index){
    removeHarvestBox(index)
    
    removeUI(
      selector = paste0("#fr_plantingTrasplanting_", index),
      immediate = T
    )
  }
  
  removeAgroBoxesMonocrop <- function(){
    ids <- expCondsVars$ids_harvest
    for(id in ids){
      removeHarvestBox(id)
    }
    
    removeUI(
      selector = paste0("#fr_plantingTrasplanting_", 1),
      immediate = T
    )
    
  }
  
  removeHarvestBox <- function(index){
    removeUI(
      selector = paste0("#fr_harvestbox_", index),
      immediate = T
    )
  }
  
  ######### harvest and planting #########
  
 

  # drawAgroBoxes <- function(index){
  #   ## adding harvest box
  #   
  #   index_harv <- index
  #   crop <- NULL
  #   xtitle <- "Crop"
  #   
  #   isolate(
  #     if(input$croppingType == 'Monocrop'){
  #       expCondsVars$ids_harvest <- c()
  #       str_id <- stri_rand_strings(1, 8,  '[A-Z]')
  #       expCondsVars$ids_harvest <- c(expCondsVars$ids_harvest, paste0(str_id))
  #       index_harv <- str_id
  #       expCondsVars$num_harvest <- 1
  #     }
  #     else{
  #       crop <- input[[paste0("int_cropCommonName_", index)]]
  #       if(!is.null(crop)){
  #         if(crop == "Other"){
  #           if(input[[paste0("int_cropCommonName_", index, "_other")]] == ''){
  #             xtitle <- "Other"
  #           }
  #           else {
  #             xtitle <- input[[paste0("int_cropCommonName_", index, "_other")]]
  #           }
  #         }
  #         else{
  #           xtitle <- crop
  #         }
  #       }
  #     }
  #   )
  #   
  #   drawBoxHarvest(index_harv)
  #   
  #   ## adding planting transplanting box
  #   insertUI(
  #     selector = "#fr_plantingTransplating_reference_point",
  #     where = "beforeBegin",
  #     ui <- uiPlantingTransplantingBox(index)
  #   )
  #   if(is.null(crop)){
  #     renameAgroBoxes(index_harv, index)  
  #   }
  #   else{
  #     renameAgroBoxes(index_harv, index, xtitle)
  #   }
  #   
  #   
  #   
  # }
  # 
  # drawAgroBoxesIntercrop <- function(){
  #   isolate(ids <- intercropVars$ids)
  #   for( id in ids){
  #     vars <- unlist(strsplit(id,"_")) 
  #     drawAgroBoxes(vars[2])
  #   }
  # }
  # renameAgroBoxes <- function(index_harvest, index_planting, crop = ""){
  #   titleHarvest <- "Harvest details"
  #   titlePlanting <- "Planting & Transplanting details"
  #   isolate(
  #     if(input$croppingType == 'Intercrop'){
  #         if(crop == ""){
  #           titleHarvest <- "Harvest details: Crop"
  #           titlePlanting <- "Planting & Transplanting details: Crop"
  #         }
  #         else{
  #           titleHarvest <- paste0("Harvest details: ", crop)
  #           titlePlanting <- paste0("Planting & Transplanting details: ", crop)
  #         }
  #         
  #       }
  #    )
  #     
  #     output[[paste0("planting_title_", index_planting)]] <- renderText({titlePlanting})
  #     output[[paste0("harvest_title_", index_harvest)]] <- renderText({titleHarvest})
  #   
  # }
  # 
  # drawBoxHarvest <- function(index){
  #   insertUI(
  #     selector = "#fr_harvest_reference_point",
  #     where = "beforeBegin",
  #     ui = uiHarvestBox(index)
  #   )
  # }
  # 
  # 
  # 
  # uiHarvestBox <- function(index){
  #   fluidRow( id = paste0("fr_harvestbox_", index),
  #             box(id=paste0("desc_harvest_boxid_",index ),
  #                 title = actionLink(paste0("desc_harvest_titleId_", index), uiOutput(paste0("harvest_title_", index))),
  #                 # title = div(id=paste0("desc_harvest_titleId_", index), "Harvest detailssss"),
  #                 status = "primary",
  #                 solidHeader = TRUE,
  #                 width = 12, collapsible = TRUE, collapsed = FALSE,
  #                 fluidRow(
  #                   column(width = 6,
  #                          fluidRow(
  #                            column(width = 6,
  #                                   #dateInput(paste0("harvest_start_date_", index), label ="Start date", format = "yyyy-mm-dd")
  #                                   #airDatepickerInput("harvest_start_date", "Start date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
  #                                   #uiOutput("h_start_date")
  #                                   
  #                                   if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
  #                                     airDatepickerInput(paste0("hahd_harvest_start_date_", index),
  #                                                        "Start date",
  #                                                        clearButton = T,
  #                                                        autoClose = T,
  #                                                        #value = as.Date(input$fbDesign_project_start_date) + 1,
  #                                                        placeholder = "yyyy-mm-dd",
  #                                                        minDate = as.Date(input$fbDesign_project_start_date) + 1,
  #                                                        maxDate = as.Date(input$fbDesign_project_end_date) + 1
  #                                                        
  #                                     )
  #                                   } else {
  #                                     airDatepickerInput(paste0("hahd_harvest_start_date_", index),
  #                                                        "Start date",
  #                                                        clearButton = T,
  #                                                        autoClose = T,
  #                                                        placeholder = "yyyy-mm-dd"
  #                                     )
  #                                   }
  #                            ),
  #                            column(width = 6,
  #                                   #dateInput(paste0("harvest_end_date_", index), label ="End date", format = "yyyy-mm-dd")
  #                                   #airDatepickerInput("harvest_end_date", "End date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
  #                                   #uiOutput("h_end_date")
  #                                   
  #                                   if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
  #                                     airDatepickerInput(paste0("hahd_harvest_end_date_", index),
  #                                                        "End date",
  #                                                        clearButton = T,
  #                                                        autoClose = T,
  #                                                        #value = as.Date(input$fbDesign_project_start_date) + 1,
  #                                                        placeholder = "yyyy-mm-dd",
  #                                                        minDate = as.Date(input$fbDesign_project_start_date) + 1,
  #                                                        maxDate = as.Date(input$fbDesign_project_end_date) + 1
  #                                                        
  #                                     )
  #                                   } else {
  #                                     airDatepickerInput(paste0("hahd_harvest_end_date_", index),
  #                                                        "End date",
  #                                                        clearButton = T,
  #                                                        autoClose = T,
  #                                                        placeholder = "yyyy-mm-dd"
  #                                     )
  #                                   }
  #                            )
  #                          ),
  #                          
  #                          fluidRow(
  #                            column(6,
  #                                   selectizeInput(paste0("hahd_harvest_method_", index), label = "Harvest method", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                                    c("Baling", "Cutting", "Mowing", "Haymaking", "Picking", "Threshing", "Trussing", "Windrowing","Winnowing","Other")
  #                                   ))
  #                            
  #                            
  #                          ),
  #                          
  #                          hidden(textInput(paste0("hahd_harvest_method_", index,"_other"), "")),
  #                          selectizeInput(paste0("hahd_crop_component_harvested_", index), label = "Crop component harvested", multiple = TRUE, options = list(maxItems =11, placeholder ="Select one..."), choices =
  #                                           c("Canopy", "Aboveground biomass","Leaves","Stems","Seed","Pod", "Grain", "Tuber","Roots (excluding storage roots)", "Storage roots",
  #                                             "Other")
  #                          ),
  #                          
  #                          hidden(textInput(paste0("hahd_crop_component_harvested_",index,"_other"), "")),
  #                          
  #                          selectizeInput(paste0("hahd_crop_harvestable_area_", index), label = "Harvestable area", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                           c("m2 units", "Individual plants","Rows","Entire plot","Other")
  #                          ),
  #                          
  #                          
  #                          conditionalPanel(paste0("input.hahd_crop_harvestable_area_",index, " == 'm2 units'"),
  #                                           textInput(paste0("hahd_crop_component_harvested_m2_",index), "Number of m2 units harvested")
  #                          ),
  #                          conditionalPanel(paste0("input.hahd_crop_harvestable_area_",index, " == 'Individual plants'"),
  #                                           textInput(paste0("hahd_crop_component_harvested_ip_",index), "Number of plants harvested")
  #                          ),
  #                          conditionalPanel(paste0("input.hahd_crop_harvestable_area_",index, " == 'Rows'"),
  #                                           fluidRow(
  #                                             column(6,
  #                                                    textInput(paste0("hahd_crop_component_harvested_num_",index), "Number of rows harvested")
  #                                             )
  #                                           ),
  #                                           
  #                                           
  #                                           fluidRow(
  #                                             column(6,
  #                                                    textInput(paste0("hahd_crop_component_harvested_len_",index), "Length of rows harvested")
  #                                                    
  #                                             ),
  #                                             column(6,
  #                                                    selectizeInput(paste0("hahd_crop_component_harvested_lenunit_",index),  label ="Unit", multiple = TRUE, options = list(maxItems =11, placeholder ="Select one..."), choices =
  #                                                                     c("cm", "m", "in","ft"), selected = "cm")
  #                                             )
  #                                           ),
  #                                           fluidRow(
  #                                             column(6,
  #                                                    textInput(paste0("hahd_crop_component_harvested_width_",index), "Width within rows harvested")
  #                                             ),
  #                                             column(6,
  #                                                    selectizeInput(paste0("hahd_crop_component_harvested_widthunit_",index),  label ="Unit", multiple = TRUE, options = list(maxItems =11, placeholder ="Select one..."), choices =
  #                                                                     c("cm", "m", "in","ft"), selected = "cm")
  #                                             )
  #                                           ),
  #                                           fluidRow(
  #                                             column(6,
  #                                                    numericInput(paste0("hahd_space_rows_harvested_", index), "Space between rows harvested", value = "", min = 0, step = 0.1)
  #                                             ),
  #                                             column(6,
  #                                                    selectizeInput(paste0("hahd_crop_component_harvested_spaceunit_",index),  label ="Unit", multiple = TRUE, options = list(maxItems =11, placeholder ="Select one..."), choices =
  #                                                                     c("cm", "m", "in","ft"), selected = "cm")
  #                                             )
  #                                           )
  #                          ),
  #                          conditionalPanel(paste0("input.hahd_crop_harvestable_area_",index, " == 'Entire plot'"),
  #                                           fluidRow(
  #                                             column(6,
  #                                                    textInput(paste0("hahd_crop_component_harvested_entire_",index), "Plot area harvested")
  #                                             ),
  #                                             column(6,
  #                                                    selectizeInput(paste0("hahd_crop_component_harvested_entireunit_",index),  label ="Unit", multiple = TRUE, options = list(maxItems =11, placeholder ="Select one..."), choices =
  #                                                                     c("m2", "ha", "ft2","ac"), selected = "ha")
  #                                             )
  #                                           )
  #                          ),
  #                          hidden(textInput(paste0("hahd_crop_harvestable_area_", index,"_other"), "")),
  #                          fluidRow(
  #                            column(width = 6,
  #                                   numericInput(paste0("hahd_amount_harvested_", index), "Amount harvested", value = "", min = 0, step = 0.1)
  #                            ),
  #                            column(width = 6,#IMPLEMENTAR EN EXCEL
  #                                   selectizeInput(paste0("hahd_amount_harvested_unit_", index), label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("g", "kg", "lb", "t"), selected = "g")
  #                            )
  #                          ),
  #                          
  #                          
  #                          
  #                          #textInput("num_rows_harvested", "Number of rows harvested"),
  #                          
  #                          # textInput(paste0("num_plants_area_harvested_", index), "Number of plants in area harvested"),
  #                          
  #                          fluidRow(
  #                            column(width = 6,
  #                                   numericInput(paste0("hahd_harvest_cut_height_", index), "Harvest cut height", value = "", min = 0, step = 0.1)
  #                            ),
  #                            column(width = 6,
  #                                   selectizeInput(paste0("hahd_harvest_cut_height_unit_", index), label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("cm", "ft", "in", "m"), selected = "cm")
  #                            )
  #                          ),
  #                          textAreaInput(inputId = paste0("hahd_harvest_notes_", index), label = "Notes", value = "")
  #                          
  #                   ),
  #                   column(width = 6,
  #                          br(),
  #                          isolate(
  #                            if(input$croppingType == 'Monocrop'){
  #                              column(12, 
  #                                     style='padding:0px; text-align:right; ',  actionButton(paste0("closeBox_HARV_",index ), "", icon("close")),
  #                                     br(),br()
  #                              )                               
  #                            }
  #                          ),
  #                          
  #                         
  #                          
  #                          fluidRow(
  #                            box(
  #                              title = "Implement", solidHeader = TRUE, status = "warning", width=12,
  #                              # selectizeInput(paste0("harvest_technique_", index), label = "Technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                              #                  c("Manual",
  #                              #                    "Mechanized")
  #                              # ),
  #                              fluidRow(
  #                                column(12,
  #                                       h4("Implement", style="font-weight: 800;color: #555;")
  #                                )
  #                              ),
  #                              selectizeInput(paste0("hahd_harvest_implement_", index), label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                               c("Baler",
  #                                                 "Chopper",
  #                                                 "Combine",
  #                                                 "Digger",
  #                                                 "Mower",
  #                                                 "Reaper",
  #                                                 "Roller",
  #                                                 "Sickle",
  #                                                 "Other")
  #                              ),
  #                              hidden(textInput(paste0("hahd_harvest_implement_", index, "_other"), "")),
  #                              # textInput("harvest_make", value="", label = "Implement make"),
  #                              # textInput("harvest_model", value="", label = "Implement model"),
  #                              selectizeInput(paste0("hahd_harvest_traction_" , index), label = "Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                               c("Animal",
  #                                                 "Manual",
  #                                                 "2 wheel tractor",
  #                                                 "4 wheel tractor",
  #                                                 "Other"
  #                                               )
  #                              ),
  #                              hidden(textInput(paste0("hahd_harvest_traction_",index,"_other"), ""))
  #                              
  #                            ))
  #                   ))
  #             ))#end box description harvest
  # }
  # 
  # uiPlantingTransplantingBox <- function(index){
  #   fluidRow(id= paste0("fr_plantingTrasplanting_", index),
  #            box( id = paste0("plantingTransplanting_boxid_", index),
  #                 title = actionLink(paste0("plantingTransplanting_titleId_", index), uiOutput(paste0("planting_title_", index)) ),
  #                 status = "primary",
  #                 solidHeader = TRUE,
  #                 width = 12, collapsible = TRUE,  collapsed = FALSE,
  #                 fluidRow(
  #                   
  #                   box(id=paste0("direct_seeding_boxid_", index),
  #                       title = checkboxInput(paste0("directSeeding_checkbox_", index), actionLink(paste0("direct_seeding_titleId_", index), "Direct seeding"), F),
  #                       status = "primary",
  #                       solidHeader = TRUE,
  #                       width = 12, collapsible = TRUE,  collapsed = TRUE,
  #                       
  #                       fluidRow(
  #                         column(width = 6,
  #                                fluidRow(
  #                                  column(width = 6,
  #                                         #dateInput(paste0("planting_start_date_", index), label ="Start date", format = "yyyy-mm-dd")
  #                                         #airDatepickerInput("planting_start_date", "Start date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
  #                                         # uiOutput("pl_start_date")
  #                                         
  #                                         if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
  #                                           airDatepickerInput(paste0("ptdi_planting_start_date_", index),
  #                                                              "Start date",
  #                                                              clearButton = T,
  #                                                              autoClose = T,
  #                                                              #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
  #                                                              placeholder = "yyyy-mm-dd",
  #                                                              minDate = as.Date(input$fbDesign_project_start_date) + 1,
  #                                                              maxDate = as.Date(input$fbDesign_project_end_date) + 1
  #                                                              
  #                                           )
  #                                         } else {
  #                                           airDatepickerInput(paste0("ptdi_planting_start_date_", index),
  #                                                              "Start date",
  #                                                              clearButton = T,
  #                                                              autoClose = T,
  #                                                              placeholder = "yyyy-mm-dd"
  #                                           )
  #                                         }
  #                                         
  #                                  )
  #                                  
  #                                )
  #                         )
  #                       ),
  #                       fluidRow(
  #                         
  #                         column(width = 6,
  #                                fluidRow(
  #                                  box(
  #                                    title = "Planting, transplanting method", solidHeader = TRUE, status = "warning", width=12,
  #                                    fluidRow(
  #                                      column(12,
  #                                             h4("Planting, transplanting method", style="font-weight: 800;color: #555;")
  #                                      )
  #                                    ),
  #                                    # textInput("planting_directSeeding", value="", label = "Direct seeding"),
  #                                    selectizeInput(paste0("ptdi_seeding_environment_", index), label = "Seeding environment", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                                     c("Flat seed bed",
  #                                                       "Hill",
  #                                                       "Ridge", 
  #                                                       "Other")
  #                                    ),
  #                                    hidden(textInput(paste0("ptdi_seeding_environment_", index, "_other"), "", value="")),
  #                                    selectizeInput(paste0("ptdi_seeding_technique_", index), label = "Seeding technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                                     c("Broadcasting",
  #                                                       "Line sowing",
  #                                                       "Dibbling"
  #                                                     )
  #                                    ),
  #                                    hidden(textInput(paste0("ptdi_seeding_technique_", index,"_other"), "", value="")),
  #                                    textInput(paste0("ptdi_seed_treatment_", index), value="", label = "Seed treatment")
  #                                    
  #                                  )),
  #                                
  #                                # column(width = 6,
  #                                fluidRow(
  #                                  box(
  #                                    title = "Implement", solidHeader = TRUE, status = "warning", width=12,
  #                                    fluidRow(
  #                                      column(12,
  #                                             h4("Implement", style="font-weight: 800;color: #555;")
  #                                      )
  #                                    ),
  #                                    selectizeInput(paste0("ptdi_seeding_implement_type_", index), label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                                     c("Bucket broadcaster",
  #                                                       "Dibbling stick",
  #                                                       "Drum seeder",
  #                                                       "Jab planter",
  #                                                       "Seed drill",
  #                                                       "Other"
  #                                                     )
  #                                    ),
  #                                    hidden(textInput(paste0("ptdi_seeding_implement_type_", index, "_other"), "", value="")),
  #                                    selectizeInput(paste0("ptdi_seeding_traction_", index), label = "Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                                     c("Animal",
  #                                                       "Manual",
  #                                                       "2 wheel tractor",
  #                                                       "4 wheel tractor",
  #                                                       "Other"
  #                                                     )
  #                                    ),
  #                                    hidden(textInput(paste0("ptdi_seeding_traction_", index, "_other"), "", value=""))
  #                                  )
  #                                )
  #                                # )
  #                         ),
  #                         column(width = 6,
  #                                fluidRow(
  #                                  box(
  #                                    title = "Seeding density", solidHeader = TRUE, status = "warning", width=12,
  #                                    fluidRow(
  #                                      column(12,
  #                                             h4("Seeding density", style="font-weight: 800;color: #555;")
  #                                      )
  #                                    ),
  #                                    fluidRow(
  #                                      column(width = 6,
  #                                             numericInput(paste0("ptdi_distance_rows_", index),  label = "Distance between rows", min=0, max=100, step=0.1,value=NULL)
  #                                      ),
  #                                      column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
  #                                             selectizeInput(paste0("ptdi_distance_rows_unit_", index), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                                              c("cm",
  #                                                                "ft",
  #                                                                "in",
  #                                                                "m"),
  #                                                            selected = "cm"
  #                                             )
  #                                      )
  #                                    ),
  #                                    fluidRow(
  #                                      column(width = 6,
  #                                             numericInput(paste0("ptdi_seeding_rate_", index),  label = "Seeding rate", min=0, max=100, step=1,value=NULL)
  #                                      ),
  #                                      column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
  #                                             selectizeInput(paste0("ptdi_seeding_rate_unit_", index), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                                              c("kg/ha",
  #                                                                "lb/ac",
  #                                                                "plants/pot"),
  #                                                            selected = "kg/ha"
  #                                             )
  #                                      )
  #                                    ),
  #                                    # textInput("seeds_per_hil", "Seeds/seedlings per hill", value =""),
  #                                    fluidRow(
  #                                      column(width = 6,
  #                                             numericInput(paste0("ptdi_distance_plants_", index),  label = "Distance between plants", min=0, max=100, step=0.1,value=NULL)
  #                                      ),
  #                                      column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
  #                                             selectizeInput(paste0("ptdi_distance_plants_unit_",index), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                                              c("cm",
  #                                                                "ft",
  #                                                                "in",
  #                                                                "m"),
  #                                                            selected = "cm"
  #                                             )
  #                                      )
  #                                    ),
  #                                    numericInput(paste0("ptdi_seeding_density_number_rows_", index),  label = "Number of rows", min=0, max=100, step=1, value=NULL),
  #                                    fluidRow(
  #                                      column(width = 6,
  #                                             numericInput(paste0("ptdi_seeding_plant_density_", index),  label = "Plant density", min=0, max=100, step=0.1, value=NULL)
  #                                      ),
  #                                      column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
  #                                             selectizeInput(paste0("ptdi_seeding_plant_density_unit_", index), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                                              c("plants/hill",
  #                                                                "plants/m2",
  #                                                                "plants/pot",
  #                                                                "plants/row"),
  #                                                            selected = "plants/m2"
  #                                             )
  #                                      )
  #                                    ),
  #                                    fluidRow(
  #                                      column(width = 6,
  #                                             numericInput(paste0("ptdi_seeding_distance_bunds_", index),  label = "Distance between bunds", min=0, max=100, step=0.1, value=NULL)
  #                                      ),
  #                                      column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
  #                                             selectizeInput(paste0("ptdi_seeding_distance_bunds_unit_", index), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                                              c("cm",
  #                                                                "m",
  #                                                                "in",
  #                                                                "ft"),
  #                                                            selected = "cm"
  #                                             )
  #                                      )
  #                                    ),
  #                                    textAreaInput(paste0("ptdi_direct_seeding_notes_", index), label="Notes", value="")
  #                                    
  #                                  )
  #                                )
  #                         )
  #                       )
  #                       
  #                       
  #                   )
  #                 ),
  #                 fluidRow(
  #                   box(id=paste0("transplanting_boxid_", index),
  #                       title = checkboxInput(paste0("transplanting_checkbox_", index), actionLink(paste0("transplanting_titleId_", index), "Transplanting"), F),
  #                       status = "primary",
  #                       solidHeader = TRUE,
  #                       width = 12, collapsible = TRUE, collapsed = TRUE,
  #                       fluidRow(
  #                         
  #                         column(width = 6,
  #                                fluidRow(
  #                                  column(width = 6,
  #                                         #dateInput(paste0("transplanting_start_date_", index), label ="Start date", format = "yyyy-mm-dd")
  #                                         #airDatepickerInput("transplanting_start_date", "Start date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
  #                                         # uiOutput("trans_start_date")
  #                                         
  #                                         if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
  #                                           airDatepickerInput(paste0("ptta_transplanting_start_date_", index),
  #                                                              "Start date",
  #                                                              clearButton = T,
  #                                                              autoClose = T,
  #                                                              #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
  #                                                              placeholder = "yyyy-mm-dd",
  #                                                              minDate = as.Date(input$fbDesign_project_start_date) + 1,
  #                                                              maxDate = as.Date(input$fbDesign_project_end_date) + 1
  #                                                              
  #                                           )
  #                                         } else {
  #                                           airDatepickerInput(paste0("ptta_transplanting_start_date_", index),
  #                                                              "Start date",
  #                                                              clearButton = T,
  #                                                              autoClose = T,
  #                                                              placeholder = "yyyy-mm-dd"
  #                                           )
  #                                         }
  #                                         
  #                                  ),
  #                                  column(width = 6,
  #                                         #dateInput(paste0("transplanting_end_date_", index), label ="End date", format = "yyyy-mm-dd")
  #                                         #airDatepickerInput("transplanting_end_date", "End date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
  #                                         # uiOutput("trans_end_date")
  #                                         
  #                                         if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
  #                                           airDatepickerInput(paste0("ptta_transplanting_end_date_", index),
  #                                                              "End date",
  #                                                              clearButton = T,
  #                                                              autoClose = T,
  #                                                              #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
  #                                                              placeholder = "yyyy-mm-dd",
  #                                                              minDate = as.Date(input$fbDesign_project_start_date) + 1,
  #                                                              maxDate = as.Date(input$fbDesign_project_end_date) + 1
  #                                                              
  #                                           )
  #                                         } else {
  #                                           airDatepickerInput(paste0("ptta_transplanting_end_date_", index),
  #                                                              "End date",
  #                                                              clearButton = T,
  #                                                              autoClose = T,
  #                                                              placeholder = "yyyy-mm-dd"
  #                                           )
  #                                         }
  #                                  )),
  #                                numericInput(paste0("ptta_age_seedling_", index), value="", label = "Age of seedling (days)", min=0, max=100, step=1),
  #                                selectizeInput(paste0("ptta_transplanting_environment_", index), label = "Seedling environment", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                                 c("Flat seed bed",
  #                                                   "Hill",
  #                                                   "Ridge",
  #                                                   "Other")
  #                                ),
  #                                hidden(textInput(paste0("ptta_transplanting_environment_", index, "_other"), "", value="")),
  #                                selectizeInput(paste0("ptta_transplanting_technique_", index), label = "Technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                                 c("Manual",
  #                                                   "Mechanical",
  #                                                   "Other")
  #                                ),
  #                                hidden(textInput(paste0("ptta_transplanting_technique_", index, "_other"), "", value="")),
  #                                textInput(paste0("ptta_transplanting_treatment_", index), value="", label = "Seed treatment"),
  #                                selectizeInput(paste0("ptta_trans_traction_", index), label = "Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                                 c("Animal",
  #                                                   "Traction",
  #                                                   "2 wheel tractor",
  #                                                   "4 wheel tractor",
  #                                                   "Other"
  #                                                 )
  #                                ),
  #                                hidden(textInput(paste0("ptta_trans_traction_", index,"_other"), "", value=""))
  #                         ),
  #                         
  #                         
  #                         column(width = 6,
  #                                fluidRow(
  #                                  box(
  #                                    title = "Transplanting density", solidHeader = TRUE, status = "warning", width=12,
  #                                    fluidRow(
  #                                      column(12,
  #                                             h4("Transplanting density", style="font-weight: 800;color: #555;")
  #                                      )
  #                                    ),
  #                                    #br(),
  #                                    fluidRow(
  #                                      column(width = 6,
  #                                             numericInput(paste0("ptta_trans_distance_rows_", index),  label = "Distance between rows", value="", min=0, max=100, step=0.1)
  #                                      ),
  #                                      column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
  #                                             selectizeInput(paste0("ptta_trans_distance_rows_unit_", index), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                                              c("cm",
  #                                                                "ft",
  #                                                                "in",
  #                                                                "m"),
  #                                                            selected = "cm"
  #                                             )
  #                                      )
  #                                    ),
  #                                    fluidRow(
  #                                      column(width = 6,
  #                                             numericInput(paste0("ptta_trans_seeding_density_", index),  label = "Seedling density", value="", min=0, max=100, step=1)
  #                                      ),
  #                                      column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
  #                                             selectizeInput(paste0("ptta_trans_seeding_density_unit_", index), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                                              c("plants/hill",
  #                                                                "plants/m2",
  #                                                                "plants/pot",
  #                                                                "plants/row"),
  #                                                            selected = "plants/m2"
  #                                             )
  #                                      )
  #                                    ),
  #                                    numericInput(paste0("ptta_trans_num_rows_", index), "Number of rows", value ="", min=0, max=100, step=1),
  #                                    fluidRow(
  #                                      column(width = 6,
  #                                             numericInput(paste0("ptta_trans_distance_plants_", index),  label = "Distance between plants", value="", min=0, max=100, step=0.1)
  #                                      ),
  #                                      column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
  #                                             selectizeInput(paste0("ptta_trans_distance_plants_unit_", index), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), selected="m", choices =
  #                                                              c("m")
  #                                             )
  #                                      )
  #                                    ),
  #                                    fluidRow(
  #                                      column(width = 6,
  #                                             numericInput(paste0("ptta_trans_distance_bunds_", index),  label = "Distance between bunds", min=0, max=100, step=0.1, value=NULL)
  #                                      ),
  #                                      column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
  #                                             selectizeInput(paste0("ptta_trans_distance_bunds_unit_", index), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
  #                                                              c("cm",
  #                                                                "m",
  #                                                                "in",
  #                                                                "ft"),
  #                                                            selected = "cm"
  #                                             )
  #                                      )
  #                                    ),
  #                                    textAreaInput(paste0("ptta_transplanting_density_notes_", index), label="Notes", value="")
  #                                    
  #                                  )
  #                                )
  #                                
  #                                
  #                         )
  #                       ) #end fluidrow,
  #                       
  #                       
  #                   ) #end box sowing
  #                 )
  #            )
  #   )
  # }
  # 
  
  #####################################
  
  
  observe({
    aux <- NULL
    rs <- "Other"
    newVal <- "Other"
    
    
    if(input$croppingType == "Monocrop"){
      if(!is.null(input$cropCommonNameMono)){
        valSel <- input$cropCommonNameMono[1]
        aux <- dplyr::filter(as.data.frame(dict),Crop==valSel)
        
        if(valSel == "Other") {
          newVal <- trim(input$cropCommonNameMono_other)
          if(newVal == "") newVal <- "Other"
          aux$Crop<- rep(newVal, length(aux$Crop))
        }
        traitsVals$otherMonocropPrev <-  newVal
        
      }
      else{
        aux <- dplyr::filter(as.data.frame(dict),Crop=="AnyValueToCreateEmptyFrame")
      }
    }
    # else if(input$croppingType == "Intercrop"){
    #   crop_selected <- cropsVar$CropsSelectedInterCrop
    #   aux <- dplyr::filter(as.data.frame(dict), Crop %in% crop_selected)
    # }
    traitsVals$Data <- data.table(aux)
  })
  

  # observeEvent(input$btGetCheckedValues, {
  #   print( data.frame(order = seq_len(nrow(traitsVals$Data)), value = shinyValue('cropMeasure_check_', nrow(traitsVals$Data))))
  # })


  ## listening depending on number of crops available in intercrop list
  observe({
    if(cropsVar$indexOtherIntercrop == 1){
      cropsVar$varAuxOtherIntercrop <- input$cropCommonName1
    }
    if(cropsVar$indexOtherIntercrop == 2){
      cropsVar$varAuxOtherIntercrop <- input$cropCommonName2
    }
    if(cropsVar$indexOtherIntercrop == 3){
      cropsVar$varAuxOtherIntercrop <- input$cropCommonName3
    }
    if(cropsVar$indexOtherIntercrop == 4){
      cropsVar$varAuxOtherIntercrop <- input$cropCommonName4
    }
    if(cropsVar$indexOtherIntercrop == 5){
      cropsVar$varAuxOtherIntercrop <- input$cropCommonName5
    }
    if(cropsVar$indexOtherIntercrop == 6){
      cropsVar$varAuxOtherIntercrop <- input$cropCommonName6
    }
    if(cropsVar$indexOtherIntercrop == 7){
      cropsVar$varAuxOtherIntercrop <- input$cropCommonName7
    }
  })

  output$uiTraitsList <- renderUI({

    column(12,
           #column(12,  style = "padding-top:0px; padding-bottom:10px; text-align:center; color:red;", id = "ms_traitsTable_message", h3("Must select a crop to show traits list")),
           br(),
           dataTableOutput("Main_table"),

           tags$script("$(document).on('change', '.selectRow', function () {
                       Shiny.onInputChange('selectRowClickId',this.id);
                       Shiny.onInputChange('selectRowClickChecked',this.checked);
                       Shiny.onInputChange('selectRowClick', Math.random())
                      });"
                  ),
           tags$script("$(document).on('change', '.select_scale', function () {
                       Shiny.onInputChange('selectScaleClickId',this.id);
                       Shiny.onInputChange('selectScaleClickValue',this.value);
                       Shiny.onInputChange('selectScaleClick', Math.random())
                      });"
                  )
           ,
           tags$script('$(document).on("change", "input[id^=\'cropCommonName\']",  function(){
                                if($("#" + this.id).is(":enabled")){
                                      Shiny.onInputChange("jsCropCommonNameOtherVal", this.value);
                                      //Shiny.onInputChange("jsCropCommonNameOtherFlag",Math.random());
                                }
                        })'
           )


    )
})

  # auxData <- reactiveValues()
  # auxData$mm <- NULL

  observe({
    if(nrow(traitsVals$Data) >0){
      # DT[["Select variable"]] <- drawButtonSelect()
      hide("ms_traitsTable_message")

    }
    else{
      shinyjs::show(id="ms_traitsTable_message")
      }
  })

  # output$Main_table <-renderDataTable({
  #   # DsT= traitsVals$Data
  #   #DT[["Change scale"]] <- drawComboInTable()
  #
  #
  #
  #
  #   #DT[["Variable ID"]] <- traitsVals$Data[,6]
  #   DT<-traitsVals$Data
  #
  #   datatable(DT,
  #             # preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
  #             # drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
  #             escape = FALSE,
  #             # selection = list(mode = 'multiple', selected = traitsVals$selectedRows),
  #             selection = list(mode = 'none'),
  #             options = list(
  #               scrollX = TRUE,
  #               pageLength = 25,
  #               columnDefs = list(list(visible=FALSE, targets=c(1,6)),list(width = '30%', targets = c(1)), list(className = 'dt-center', targets = c(7,8)))
  #             )
  #   )
  # })


  # shinyInput = function(FUN, len, id, ...) {
  #   inputs = character(len)
  #   # l <- c()
  #   for (i in seq_len(len)) {
  #     inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
  #     # a = as.character(FUN(paste0(id, i), label = NULL, ...))
  #     # l <- c(l, a)
  #   }
  #   inputs
  #
  # }
  #
  # shinyValue = function(id, len) {
  #   unlist(lapply(seq_len(len), function(i) {
  #     value = input[[paste0(id, i)]]
  #     if (is.null(value)) NA else value
  #   }))
  # }


  cropMeasureTable <- reactive({
    dd <- traitsVals$Data
    if(nrow(dd) > 0 ) dd[["Select scale"]] <- drawButtonSelect()
    # if(nrow(dd) > 0 ) dd[["Select scale"]] <- shinyInput(checkboxInput, nrow(dd), 'cropMeasure_check_', value = F)
    dd
  })


  output$Main_table <-renderDataTable(
    cropMeasureTable(),
    # server = F,
    escape = FALSE,
    selection = 'none',
    options = list(
      scrollX = TRUE,
      pageLength = 25,
      columnDefs = list(list(visible=FALSE, targets=c(1,6)), 
                        list(width = '30%', targets = c(1)), list(className = 'dt-center', targets = c(7,8)))
      # preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      # drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
    )

  )

  # cropMeasureSelected  <- reactive ({
  #   a <- shinyValue('cropMeasure_check_', nrow(traitsVals$Data))
  # })



  observeEvent(input$selectRowClick, {
    selectedRow  <- as.numeric(gsub("selectRow_","",input$selectRowClickId))
    row <- traitsVals$Data[selectedRow,]
    if(input$selectRowClickChecked){
      # traitsVals$Data[[1]][selectedRow] <- "<font color='red'><b>Selected</b></font>"
      traitsVals$Data[[1]][selectedRow] <- "Selected"
    }
    else{
      # traitsVals$Data[[1]][selectedRow] <- "<font color='black'>Not selected</font>"
      traitsVals$Data[[1]][selectedRow] <- "Not selected"

    }

  })

  observeEvent(input$selectScaleClick,{
    vv  <- strsplit(input$selectScaleClickValue, "-")[[1]]
    var <- list()
    if(length(vv) == 1){
      var[[1]] = vv
      var[[2]] = ""
    }
    else{
      var <- vv
    }

    traitsVals$Data[[6]][as.numeric(gsub("select_scale_","",input$selectScaleClickId))]<- var[[1]]
    traitsVals$Data[[7]][as.numeric(gsub("select_scale_","",input$selectScaleClickId))]<- var[[2]]
  })

  drawButtonSelect <- function(){
    n<- nrow(traitsVals$Data)
    l <- c()
    for(index in 1:n){

      old_row <- traitsVals$Data[index,]

      ckecked <-  ""
      if(old_row[[1]] %like% "Selected"){
        ckecked <- "checked"
      }

      str <-  paste0('<div class="btn-group" role="group" aria-label="Basic example">
        <input style="width:100px; background-color:green; color:white;" type="checkbox" class="selectRow"  id=selectRow_',index ,' ',ckecked,  '></input>
        </div>')
      l<- c(l,str)
    }
    return(l)
  }

  drawComboInTable <- function(){
    n<- nrow(traitsVals$Data)
    l <- c()
    for(index in 1:n){
      old_row = traitsVals$Data[index,]
      options = old_row[[5]]
      str  <- paste0('<select id="select_scale_' , index, '" class="select_scale" style="width:150px;">')
      arrOpt <- strsplit(options, ",")[[1]]

      if(length(arrOpt) == 1){
        str <- ""
      }
      else{
        for(val in arrOpt){
          mval  <- strsplit(val, "-")[[1]]
          # if(mval[[2]] == old_row[[6]]) sel <- "selected" else sel <-""
          #
          # str <- paste0(str, '<option value="', mval[[1]], "-" , mval[[2]], '" ', sel,'> ', mval[[2]], '</option>')
          if(mval[[1]] == old_row[[7]]) sel <- "selected" else sel <-""

          str <- paste0(str, '<option value="', mval[[2]], "-" , mval[[1]], '" ', sel,'> ', mval[[1]], '</option>')
        }
        str <- paste0(str, "</select>")
      }
      l <- c(l, str)
    }
    return(l)
  }

  ### end crop measurement 1 ###

  ### start crop measurement 2 ###

  traitsVals2 <- reactiveValues()
  traitsVals2$aux <- data.frame()
  traitsVals2$selectedRows <- list()
  traitsVals2$Data <- data.table()

  res = reactive({
    #dict2 <- readRDS("/home/obenites/HIDAP_SB_1.0.0/hidap/inst/hidap_agrofims/www/internal_files/crop_measurements_v1_cbox.rds")
    dict2 <- readRDS(paste0(globalpath, "crop_measurements_v1_cbox.rds"))
    dict2 <- as.data.frame(dict2, stringsAsFactors=FALSE)
  })

  # observe({
  #   aux <- NULL
  #   rs <- "Other"
  #   newVal <- "Other"
  #
  #   if(input$croppingType == "Monocrop"){
  #     if(!is.null(input$cropCommonNameMono)){
  #       valSel <- input$cropCommonNameMono[1]
  #       aux <- dplyr::filter(as.data.frame(dict),Crop==valSel)
  #
  #       if(valSel == "Other") {
  #         newVal <- trim(input$cropCommonNameMono_other)
  #         if(newVal == "") newVal <- "Other"
  #         aux$Crop<- rep(newVal, length(aux$Crop))
  #       }
  #       traitsVals$otherMonocropPrev <-  newVal
  #
  #     }
  #     else{
  #       aux <- dplyr::filter(as.data.frame(dict),Crop=="AnyValueToCreateEmptyFrame")
  #     }
  #   }
  #   else if(input$croppingType == "Intercrop"){
  #     crop_selected <- input$cropsSelected
  #     aux <- dplyr::filter(as.data.frame(dict), Crop %in% crop_selected)
  #     if("Other" %in% crop_selected && !is.null(input$jsCropCommonNameOtherVal)){
  #       newVal <- trim(input$jsCropCommonNameOtherVal)
  #       if( newVal == "" ) newVal <- "Other"
  #       aux$Crop<- gsub("Other", newVal, aux$Crop)
  #     }
  #   }
  #   traitsVals$Data <- data.table(aux)
  #
  # })

  output$uiTraitsList2 <- renderUI({

    column(12,
           #column(12,  style = "padding-top:0px; padding-bottom:10px; text-align:center; color:red;", id = "ms_traitsTable_message", h3("Must select a crop to show traits list")),
           br(),
           dataTableOutput("Main_table2")#,

#            tags$script("$(document).on('change', '.selectRow', function () {
#                        Shiny.onInputChange('selectRowClickId',this.id);
#                        Shiny.onInputChange('selectRowClickChecked',this.checked);
#                        Shiny.onInputChange('selectRowClick', Math.random())
#   });"
#                   ),
#            tags$script("$(document).on('change', '.select_scale', function () {
#                        Shiny.onInputChange('selectScaleClickId',this.id);
#                        Shiny.onInputChange('selectScaleClickValue',this.value);
#                        Shiny.onInputChange('selectScaleClick', Math.random())
# });"
#                   ),
#            tags$script('$(document).on("change", "input[id^=\'cropCommon\']",  function(){
#                        if($("#" + this.id).is(":enabled")){
#                        Shiny.onInputChange("jsCropCommonNameOtherVal", this.value);
#                        //Shiny.onInputChange("jsCropCommonNameOtherFlag",Math.random());
#                        }
#
# })
#                        '
#   )


    )
})


  output$Main_table2 <-renderDataTable(
    #DT= traitsVals$Data
    #DT=dict2

    # res(), server = FALSE, escape = FALSE, selection = 'none', options = list(
    #   pageLength = 25,
    #   preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
    #   drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
    # )

    traitsVals$Data,
    # server = F,
    escape = FALSE,
    # selection = 'none',
    options = list(
      scrollX = TRUE,
      pageLength = 25,
      columnDefs = list(list(visible=FALSE, targets=c(1,6)))
      # preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      # drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
    )


    #DT[["Change scale"]] <- drawComboInTable()

    # if(nrow(traitsVals$Data) >0){
    #   DT[["Select variable"]] <- drawButtonSelect()
    #   hide("ms_traitsTable_message")
    # }
    # else{shinyjs::show(id="ms_traitsTable_message")}
    # #DT[["Variable ID"]] <- traitsVals$Data[,6]
    # datatable(DT,
    #           escape=F,
    #           # selection = list(mode = 'multiple', selected = traitsVals$selectedRows),
    #           selection = list(mode = 'none'),
    #           options = list(
    #             scrollX = TRUE,
    #             pageLength = 25,
    #             columnDefs = list(list(visible=FALSE, targets=c(1,6)),list(width = '30%', targets = c(1)), list(className = 'dt-center', targets = c(7,8)))
    #           )
    # )
    )

  output$uiTraitsList3 <- renderUI({

    column(12,
           # column(12,  style = "padding-top:0px; padding-bottom:10px; text-align:center; color:red;", id = "ms_traitsTable_message", h3("Must select a crop to show traits list")),
           # br(),
           dataTableOutput("dt")
    )
  })

  
  
  ############ Omar Benites xxx #################################################################################################################################
  
  
  
  dat <- reactive({
    dat<- traitsVals$Data
    colnames(dat) <- c("Status", "Crop", "Group", "Subgroup", "Measurement", "a",
                        "b", "TraitUnit", "c", "d", "e", "g", "Crop measurement per season",
                        "h", "Crop measurement per plot")
    dat
    #colnames(traitsVals$Data) <- c("Status", "Crop", "Group", "Subgroup", "Measurement", "", "", "Unit", "", "", "", "", "Season", "", "Plot")
  })
 
  ### Crop measurement Table Interface ----------------------------------------------------
  output$dt <- DT::renderDT(
    dat(),
    server = TRUE,
    escape = FALSE,
    options = list(
      scrollX = TRUE,
      pageLength = 25,
      #autoWidth = TRUE,
      columnDefs = list(list(visible=FALSE, targets=c(1,6,7,9,10,11,12,13,14,15)))#,
      #columnDefs = list(list(width = '200px', targets = c(13,15)))
    )
  )
  dt_proxy <- DT::dataTableProxy("dt")
  observeEvent(input$dt_sel, {
    if (isTRUE(input$dt_sel)) {
      DT::selectRows(dt_proxy, input$dt_rows_all)
    } else {
      DT::selectRows(dt_proxy, NULL)
    }
  })
  output$selected_rows <- renderPrint(print(input$dt_rows_selected))

  # ID or name of the field book ########################################################
  fbdesign_id <- shiny::reactive({

    if (!is.null(input$designFieldbook_crop)) {
      #tbl = fbcrops::get_crop_table()

      tbl <- table_crops
      nExp_id <- input$fbDesign_nExp
      #print(nExp_id)
      crop_id    <- tbl[tbl$crop_name == input$designFieldbook_crop, "crop_id"]
      program_id <- input$designFieldbook_program
      phase_id   <-  input$designFieldbook_phase
      module_id  <- input$designFieldbook_module


      date_book <- input$fbDesign_project_time_line[1]
      date_book <- unlist(str_split(date_book,"-"))
      date_book <- paste(date_book[2],date_book[1],sep="")

      #sites = input$designFieldbook_sites
      sites <- stringr::str_trim(input$designFieldbook_sites, side="both")

      if(nExp_id=="-"){
        out <- paste0(crop_id, program_id, phase_id, module_id, date_book,"_", sites)

      } else {
        out <-  paste0(crop_id, program_id, phase_id, module_id, date_book,"_", sites, "_", nExp_id)
      }

      paste(out, collapse = ", ")
    }
  })

  # RenderText for displaying the book's name ###########################################
  output$fbDesign_id <- shiny::renderText({
    fbdesign_id()
  })


  #Phenomic DataTable ###################################################################
  output$phenoDT = renderDT(
    #pheno_vars %>% dplyr::select(Level, Sublevel)  , 
    pheno_vars %>% dplyr::select(Measurement, TraitUnit),
    options = list(
      #lengthChange = FALSE,
      scrollX = TRUE,
      pageLength = 25
    )
  )
  
  #Weather Manual DataTable #############################################################
  output$weatherManualDT = renderDT(
    
    #dplyr::filter(weather_vars, Group == "Manual measurement") %>% dplyr::select(Variable, Unit) ,  options = list(lengthChange = FALSE) 
    weather_manual_vars %>% dplyr::select(Measurement, TraitUnit) ,  
    options = list(
      #lengthChange = FALSE,
      scrollX = TRUE,
      pageLength = 25
    ) 
  )
  #Weather Station DataTable #############################################################
  output$weatherStationDT = renderDT(
    #dplyr::filter(weather_vars, Group == "Weather station") %>% dplyr::select(Variable, Unit) , options = list(lengthChange = FALSE)
    weather_station_vars %>% dplyr::select(Measurement, TraitUnit) ,  
    options = list(
      #lengthChange = FALSE
      scrollX = TRUE,
      pageLength = 25#,
      #selection = list(mode = 'multiple', selected = c(1, 3, 8, 12))
    ) 
  )
  
  #Soil## Station DataTable #############################################################
  output$soilDT = renderDT(
    
    #TODO: NO CAMBIAR LAS CABEZERAS Y USAR DIRECTAMENTE DE GOOGLE DRIVE
    soil_data %>% dplyr::select(Measurement, TraitUnit) , 
    #soil_data %>% dplyr::select(Variable, Unit), 
    options = list(
      #lengthChange = FALSE,
      scrollX = TRUE,
      pageLength = 25
    )
  )
  
  # output$dt <- DT::renderDT(
  #   dat(),
  #   server = TRUE,
  #   escape = FALSE,
  #   options = list(
  #     scrollX = TRUE,
  #     pageLength = 25,
  #     columnDefs = list(list(visible=FALSE, targets=c(1,6)))
  #   )
  # )

  #Select Input for split plot designs ####################################################
  output$fbdesign_split_cb <- shiny::renderUI({
    choices <- c(input$factor_name, "INSTN")
    shiny::selectInput("designFieldbook_split_cb",label = "Factor to Plots", choices =  choices, selected= "INSTN")


  })

  
  # Observed value for geographical information ###########################################

  shiny::observe({
    path <- fbglobal::get_base_dir()
    geodb_file <- "table_sites_agrofims.rds"
    path <- file.path(path, geodb_file)
    x_sites_data <- readRDS(file = path)
    # values$sites_data <-  dplyr::filter(x_sites_data, userId==0)
    values$sites_data <-  x_sites_data
  })

  observeEvent(input$xxxx, {
    frefreshListSites()
  })
  
  frefreshListSites <- function(){
    path <- fbglobal::get_base_dir()
    geodb_file <- "table_sites_agrofims.rds"
    path <- file.path(path, geodb_file)
    x_sites_data <- readRDS(file = path)
    values$sites_data <-  x_sites_data
  }

  observeEvent(input$refreshSiteList,{
    frefreshListSites()
    # path <- fbglobal::get_base_dir()
    # geodb_file <- "table_sites_agrofims.rds"
    # path <- file.path(path, geodb_file)
    # x_sites_data <- readRDS(file = path)
    # 
    # if(session$userData$logged){
    #   values$sites_data <- dplyr::filter(x_sites_data, userId==session$userData$userId)
    # }
    # else{
    #   values$sites_data <-  dplyr::filter(x_sites_data, userId==0)
    # }
  })



  
  # Country ###################################################################################
  output$fbDesign_country <- shiny::renderUI({
    #sites_data <- fbsites::get_site_table() #before
    # sites_data <- site_table #data from package fbdesign as an internal data BEFORE

    sites_data <- values$sites_data # read trial sites using reactive values from xdata folder (NEW CODE)

    cntry <- fbsites::get_country_list(sites_data = sites_data) #new code: use file fbsites


    shiny::selectizeInput("fbDesign_countryTrial", label = "Country name",
                          choices = cntry , selected = 1,  multiple = FALSE)

  })


  
  # Sites ##################################################################################################
  fbdesign_sites <- reactive({

    #sites_data <- site_table #using data from package #Former code before useing rective values
    sites_data <- values$sites_data
    fbsites::get_filter_locality_agrofims(sites_data = sites_data, country_input= input$fbDesign_countryTrial)
  })

  
  # Country_site_select #####################################################################################
  output$fbDesign_countrySite <- shiny::renderUI({

    req(input$fbDesign_countryTrial)

    #locs <- site_table #using data from package fbsite (OLD CODE)
    locs <- values$sites_data # read trial sites using reactive values from xdata folder (NEW CODE)

    if(nrow(locs) == 0){
      fbdesign_sites_selected = c()

    }else{
      fbdesign_sites_selected <- fbdesign_sites()
    }


    #print(locs)
    if (nrow(locs) > 0 ){
      #chc = locs$shortn
      shiny::selectizeInput("designFieldbook_sites", label = "Village name",
                            choices = fbdesign_sites_selected, selected = 1,  multiple = FALSE)
    }
  })


  ### Message for Alpha Design #########################################################
  # output$alphaMessage <- shiny::renderText({
  #
  #   germoplasm <-material_table()$Accession_Number
  #   if(!is.null(germoplasm)){
  #
  #     print(germoplasm)
  #     n <- length(germoplasm)
  #     r <- as.numeric(input$designFieldbook_r)
  #     k <- as.numeric(input$designFieldbook_k)
  #
  #     dach <- design.alpha.check(trt= germoplasm,k=k,r=r)
  #     if(!dach$alfares){
  #       paste(dach$res,". The combination of ",r," and", k, " is wrong using ",n ," genotypes.")
  #     } else {
  #       paste("The combination of replication (r) and block size (k) is perfect!")
  #     }
  #   }
  #   else{
  #     paste("Enter your genotypes in the Germoplams List.")
  #   }
  # })

  #reactive value to show BookPreview/draft fieldbook table
  output$show_agrotable <- reactive({
    p <- input$fbDesign_draft_agrofims[1]
    if(p==0){
      k <- FALSE
    }else{
      k<-TRUE
    }
    return(k)
   # return(!is.null( (fb_agrofims()) ))
  })
  #
  #
  # #set options for show_mtable
   outputOptions(output, 'show_agrotable', suspendWhenHidden=FALSE)


  # Fieldbook design ################################################################

  # Fieldbook with traits ###########################################################
   
  # fb_agrofims_traits <- reactive({
   fb_agrofims_traits <- function(){

     fb <- fb_agrofims()
     #print(fb)

     trait <- traits_dt()
     #print(trait)
     cr<- trait$Crop
     cm <- trait$`Crop measurement`
     sb<- trait$Subgroup
     sc <- trait$Scale
     sc[is.na(sc)] <- "unitless"
     #co <- trait$VariableId
     cs <- paste(cr,sb, cm, sc, sep="-")

     #trait_selected <- trait_agrofims() %>% as.data.frame(stringsAsFactors =FALSE) #unlist(shinyTree::get_selected(input$designFieldbook_traits_agrofims))
     trait_selected <- cs
     #print("Trait selected")
     #print(trait_selected)

     if(!is.null(trait_selected) || length(trait_selected)==0 ){
       mm  <-  matrix(nrow = nrow(fb), ncol = length(trait_selected) )
       nm  <-  c(names(fb), trait_selected)
       fb  <-  cbind(fb, mm)
       names(fb)  <-  nm
     }

     fb
     }
   #})


  ### reactive table for installation info ##########################################
  dt_installation_agrofims <- reactive({


    crop <- input$cropCommonNameMono #monocrop
    #crop <- input$cropsSelected


    if(crop == "Wheat" || crop == "Maize" || crop == "Soybean"){
     agromfims_installation_sheet <- installation1_template_list
    }

    if(crop == "Potato" || crop == "Sweetpotato" || crop == "Cassava") {
      agromfims_installation_sheet <-installation2_template_list
    }

    add_installation_agrofims(agronomic_crop_template= agromfims_installation_sheet, col_name = "Value",


                crop = input$cropCommonNameMono,
                designFieldbook_agrofims	=	input$designFieldbook_agrofims,#all crops
                designFieldbook_agrofims_r	=	input$designFieldbook_agrofims_r, #all crops


                numPlantsPerPlot	=	input$numPlantsPerPlot,#potato cassava sweetpotato
                numRowsPerPlot	=	input$numRowsPerPlot,#potato cassava sweetpotato
                numPlantsPerRow	=	input$numPlantsPerRow,#potato cassava sweetpotato
                plotSize	=	input$plotSize,#potato cassava sweetpotato
                distancebwPlants = input$distancebwPlants,
                distanceBwRows = input$distanceBwRows,
                spaceBwPlants	=	input$spaceBwPlants,#potato cassava sweetpotato
                spaceBwRows	=	input$spaceBwRows,#potato cassava sweetpotato
                planDensity	=	input$planDensity,#potato cassava sweetpotato

                plotSpacing	=	input$plotSpacing,#wehat maize soybean
                rowSpacing = input$rowSpacing,#wehat maize soybean
                rowOrientation	=	input$rowOrientation,#wehat maize soybean
                spaceBwPlantsRow = input$spaceBwPlantsRow,#wehat maize soybean
                hillSpacing	=	input$hillSpacing,#wehat maize soybean
                numsMsPlantPerPlot = input$numsMsPlantPerPlot,#wehat maize soybean
                fieldArea = input$fieldArea,#wehat maize soybean
                expFieldMaxWidth = input$expFieldMaxWidth,#wehat maize soybean
                expFieldMaxLength = input$expFieldMaxLength,#wehat maize soybean


                factor_hdafims1	=	input$factor_hdafims1,
                lvl_hdafims1	=	input$lvl_hdafims1,

                factor_hdafims2	=	input$factor_hdafims2,
                lvl_hdafims2	=	input$lvl_hdafims2,

                factor_hdafims3	=	input$factor_hdafims3,
                lvl_hdafims3	=	input$lvl_hdafims3,

                factor_hdafims4	=	input$factor_hdafims4,
                lvl_hdafims4	=	input$lvl_hdafims4,

                factor_hdafims5	=	input$factor_hdafims5,
                lvl_hdafims5	=	input$lvl_hdafims5
                )

  })

  ############################### BEGIN EXPERIMENT CONDITIONS #######################################
  
  AllInputs <- reactive({
     x <- reactiveValuesToList(input)
     for(i in 1:length(names(x))){
       if(is.null(x[[i]])){
          x[[i]]<- ""
       } else { # else if(length(x[[i]])>1) {
       x[[i]]<-paste(x[[i]], collapse=", ")
       }
     }
     data.frame(
       id = names(x),
       values = unlist(x, use.names = FALSE),stringsAsFactors = FALSE
     )
   }) 
   
  #Residue management ###############################################################
  dt_residual<- reactive({
     
     #ai<- readRDS("/home/obenites/AGROFIMS/agdesign/inst/table_ids.rds")
     #input<-readRDS("/home/obenites/AGROFIMS/agdesign/inst/inputs.rds")
     
     if(isTRUE(input$residueDesc_checkbox)){
       dt1 <- get_ec_resdesc(input=input)$dt         
     }else {
       dt1 <- data.frame()
     }
     
     if(isTRUE(input$residueManag_checkbox)){
       dt2 <- get_ec_resmgt(input=input)$dt 
     } else{
       dt2 <- data.frame()
     }
     dt <- smart_colbind(dt1,dt2) #column bind of two sub tabs (description and management)
     if(nrow(fbdesign())==0 &&  length(dt)>0){
       dt <- dt
     } else if(nrow(fbdesign())>0 &&  length(dt)>0 ) {
       dt <- cbind(fbdesign(), dt)
     } else{
       dt<- data.frame()
     }
     dt
   })
  dt_prot_residual<-reactive({
    
    ecname <- "AgroFIMS_Agronomy_DataDictionary_05-3-2019.xlsx"
    globalpath <- "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/www/internal_files/"
    kds_resmgt <- readxl::read_excel(paste0(globalpath, ecname),sheet = "Residue management")
    
    if(isTRUE(input$residueDesc_checkbox)){
      dt1 <- get_protocol_resdesc(input=input)
      print(dt1)
    } 
    else {
      dt1 <- data.frame()
    }
    
    if(isTRUE(input$residueManag_checkbox)){
      dt2 <- get_protocol_resmgt(input=input)
      print(dt2)
    } 
    else{
      dt2 <- data.frame()
    }
    
    dt <- rbind(dt1,dt2) #column bind of two sub tabs (description and management)
    print("dt")
    dt<- dplyr::left_join(kds_resmgt, dt) %>% filter(Value!="")
    print(dt)
    #dt<- ec_clean_header(dt)
    dt
    
  }) 
  lbl_residual <- reactive({
    
    if(isTRUE(input$residueDesc_checkbox)){
      lbl1 <- get_ec_resdesc(input=input)$lbl         
    }else {
      lbl1 <- NULL
    }
    if(isTRUE(input$residueManag_checkbox)){
      lbl2 <- get_ec_resmgt(input=input)$lbl
    } else{
      lbl2 <- NULL
    }
    lbl<- c(lbl1,lbl2)
    if(length(lbl)==0){lbl <- "no-label"}
    lbl
  })
  
  #seedbed preparation  #############################################################
  dt_seedbed <- reactive({
     
     if(isTRUE(input$landLevelling_checkbox)){
       land <- get_ec_sblalv(input=input)$dt
     } else{
       land <- data.frame()  
     }
     
     if(isTRUE(input$puddling_checkbox)){
       pud<- get_ec_sbpud(input= input)$dt
     } else{
       pud<- data.frame()
     }
     
     if(isTRUE(input$tillage_checkbox)){
       till<- get_ec_sbtill(input=input)$dt
     } else {
       till<- data.frame()
     }
     
     dt<- smart_colbind(land,pud,till)
     
     if(nrow(fbdesign())==0 && length(dt)>0){
       dt <- dt
     } else if( nrow(fbdesign())>0 && length(dt)>0 ) {
       dt <- cbind(fbdesign(), dt)
     } else {
       dt <- data.frame()
     }
     dt
   })
  lbl_seedbed <- reactive({
    
    if(isTRUE(input$landLevelling_checkbox)){
      s1<- get_ec_sblalv(input=input)$lbl
    } else{
      s1 <- NULL  
    }
    
    if(isTRUE(input$puddling_checkbox)){
      s2 <- get_ec_sbpud(input= input)$lbl
    } else{
      s2 <- NULL 
    }
    
    if(isTRUE(input$tillage_checkbox)){
      s3 <- get_ec_sbtill(input=input)$lbl
    } else {
      s3 <- NULL 
    }
    
    lbl<- c(s1,s2,s3)
    if(length(lbl)==0){lbl <- "no-label"}
    lbl
    
    
  }) 

  ## Soil Fertility     #############################################################
  dt_soilFertility <- reactive({
    
    if(is.null(input$soil_fertilizer_num_apps)){
      napp <- 1
    } else{
      napp <- as.numeric(input$soil_fertilizer_num_apps)
    }
    dt<- get_ec_sf(allinputs= AllInputs(), napp=napp )$dt
    
    if(nrow(fbdesign())==0){
      dt <- dt
    }else {
      dt <-cbind(fbdesign() ,dt)
    }
    
    dt
  })
  lbl_soilFertility <- reactive({
    
    if(is.null(input$soil_fertilizer_num_apps)){
      napp <- 1
    } else{
      napp <- as.numeric(input$soil_fertilizer_num_apps)
    }
    lbl<- get_ec_sf(allinputs= AllInputs(), napp=napp )$lbl
    if(length(lbl)==0){lbl <- "no-label"}
    lbl
    
  })
  
  
  ### Planting & Transplanting   #####################################################################
  dt_plantrans <- reactive({
    
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector",default = "Monocrop") 
    
    if(ct=="Monocrop"){
      dt<- get_ec_plantrans(allinputs=AllInputs(), input, ctype="monocrop", cropId= "1", addId="1")$dt
      if(nrow(fbdesign())==0 && length(dt)>0){
        dt <- dt
      } else if( nrow(fbdesign())>0 && length(dt)>0 ) {
        dt <- cbind(fbdesign(), dt)
      } else {
        dt <- data.frame()
      }
      
    } 
    else if(ct=="Intercrop"){
      
      id_rand_inter <- getAddInputId(intercropVars$ids, "int_", "") 
      circm <- map_values(input, id_chr="int_cropCommonName_",id_rand_inter, format = "vector", lbl= "Select crop")
      
      ptdt_list<-NULL
      for(i in 1:length(id_rand_inter)){
         ptdt_list[[i]] <- get_ec_plantrans(allinputs=AllInputs(), input=input, ctype="intercrop", cropId=id_rand_inter[i], addId="1")$dt
         print("entro rara")
         print(ptdt_list[[i]])
       }
      print("total de pdt list")
      print(ptdt_list)
       
      #Join fbdesign with harvest header of each crop for intercrop trials
       dt<-NULL  
       for(i in 1:length(ptdt_list)){
            if(nrow(fbdesign())==0){
              dt[[i]] <- ptdt_list[[i]]
            } else if( nrow(ptdt_list[[i]])==0){ # when one platn-trans data.frame is empty
              dt[[i]] <- smart_colbind(fbdesign(), ptdt_list[[i]] )
            } else if(nrow(ptdt_list[[i]])!=0){ #when one plan-trans data.frame is full
              dt[[i]] <- cbind(fbdesign(), ptdt_list[[i]] )
            }
        }
       names(dt)<- circm
    } 
    else if(ct=="Relay crop"){
      
      id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "") 
      crecm <- map_values(input, id_chr="rel_cropCommonName_",id_re_rand, format = "vector", lbl= "Select crop")
      
      ptdt_list<-NULL
      for(i in 1:length(id_re_rand)){
        ptdt_list[[i]] <- get_ec_plantrans(allinputs=AllInputs(), input=input, ctype="relay crop", cropId=id_re_rand[i], addId="1")$dt
      }
      
      #Join fbdesign with harvest header of each crop for intercrop trials
      dt<-NULL  
      for(i in 1:length(ptdt_list)){
        if(nrow(fbdesign())==0){
          dt[[i]] <- ptdt_list[[i]]
        } else {
          dt[[i]] <-smart_colbind(fbdesign(), ptdt_list[[i]] )
        }
      }
      names(dt)<- crecm
    } 
    
   
    dt  
  })
  lbl_plantrans <- reactive({
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector",default = "Monocrop") 
    if(ct=="Monocrop"){
      lbl <- get_ec_plantrans(allinputs = AllInputs(), input = input, ctype="monocrop", cropId="1",addId="1" )$lbl
      if(length(lbl)==0){lbl <- "no-label"}
      #lbl
    } 
    else if(ct=="Intercrop") {
      id_rand_inter <- getAddInputId(intercropVars$ids, "int_", "") 
      circm <- map_values(input, id_chr="int_cropCommonName_",id_rand_inter, format = "vector", lbl= "Select crop")
      ptlbl_list<- NULL
      for(i in 1:length(id_rand_inter)){
        ptlbl_list[[i]] <- get_ec_plantrans(allinputs=AllInputs(), input=input, ctype="intercrop", cropId= id_rand_inter[i], addId="1")$lbl
        print("entro label")
        print(ptlbl_list[[i]])
      }
      lbl<- NULL
      for(i in 1:length(ptlbl_list)){
        if(length(ptlbl_list[[i]])!=0){
          lbl[[i]] <- ptlbl_list[[i]] #str_replace_all(string = names(ptdt_list[[i]]), pattern = "__[:digit:]+$",replacement = "")
         }
      }
      if(!is.null(lbl)){
        names(lbl)<- circm
        lbl <- purrr::compact(lbl)  
      }
      
    } 
    else if (ct=="Relay crop"){
      
      id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "") 
      crecm <- map_values(input, id_chr="rel_cropCommonName_",id_re_rand, format = "vector", lbl= "Select crop")
      ptlbl_list<- NULL
      for(i in 1:length(id_re_rand)){
        ptlbl_list[[i]] <- get_ec_plantrans(allinputs=AllInputs(), input=input, ctype="relay crop", cropId= id_re_rand[i], addId="1")$lbl
      }
      lbl<- NULL
      for(i in 1:length(ptlbl_list)){
        if(length(ptlbl_list[[i]])!=0){
          lbl[[i]] <- ptlbl_list[[i]] #str_replace_all(string = names(ptdt_list[[i]]), pattern = "__[:digit:]+$",replacement = "")
        }
      }
      names(lbl)<- crecm
      lbl <- purrr::compact(lbl)
      
    }
    lbl
  }) 
  
  
  #'TODO Mulching and residue ############################################################
  dt_mulching <- reactive({
    
    dt <- get_ec_mulching(allinputs= AllInputs())$dt
    if(nrow(fbdesign())==0){
      dt <- dt
    }else {
      dt <-cbind(fbdesign() ,dt)
    }
    dt
   })
  lbl_mulching <- reactive({
    
    lbl <- get_ec_mulching(allinputs= AllInputs())$lbl
    if(length(lbl)==0){lbl <- "no-label"}
    lbl
  })
  
  #'TODO Irrigation  #####################################################################
  dt_irrigation <- reactive({
    
    addId <- getAddInputId(addId = expconIRRImonocrop$ids, "mono_irri_", "")
    dt<- get_ec_irri(allinputs=AllInputs(), addId=addId)$dt
    if(nrow(fbdesign())==0){
      dt <- dt
    }else {
      dt <-cbind(fbdesign() ,dt)
    }
    dt
    
  })
  lbl_irrigation <- reactive({
    
    addId <- getAddInputId(addId = expconIRRImonocrop$ids, "mono_irri_", "")
    lbl<- get_ec_irri(allinputs=AllInputs(), addId=addId)$lbl
    if(length(lbl)==0){lbl <- "no-label"}
    lbl
    
    
  })
  ns_irrigation <- reactive({
    addId <- getAddInputId(addId = expconIRRImonocrop$ids, "mono_irri_", "")
    ns <- get_ns(addId)
    ns
  })
  
  ## Weeding #########################################################################
  dt_weeding <- reactive({
   
    addId <- getAddInputId(addId = expconWEEmonocrop$ids, "mono_wee_", "")
    dt<- get_ec_weed(allinputs=AllInputs(), addId=addId)$dt
    if(nrow(fbdesign())==0){
      dt <- dt
    }else {
      dt <-cbind(fbdesign() ,dt)
    }
    dt 
    
   })
  lbl_weeding<- reactive({
    addId <- getAddInputId(addId =  expconWEEmonocrop$ids, "mono_wee_", "")
    lbl <- get_ec_weed(allinputs=AllInputs(), addId=addId)$lbl
    if(length(lbl)==0){lbl <- "no-label"}
    lbl
  })
  ns_weeding<- reactive({
    addId <- getAddInputId(addId =  expconWEEmonocrop$ids, "mono_wee_", "")
    ns <- get_ns(addId)
    ns
  })
  
  
  ### Harvest  ######################################################################
  dt_harvest <- reactive({
    
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector",default = "Monocrop") 
    if(ct=="Monocrop" || ct== "Relay crop"){
       addId <- getAddInputId(addId = expCondsVars$ids_harvest, "HARV_", "")
       print(addId)
       dt <- get_ec_harv(allinputs=AllInputs(), addId=addId)$dt
       if(nrow(fbdesign())==0){
         dt <- dt
       }else {
         dt <-cbind(fbdesign() ,dt)
       }
       
    }else{
      #INTERCROP
      
      if(ct=="Intercrop"){
        id_rand <- getAddInputId(intercropVars$ids, "int_", "")
        circm <- map_values(input = input, id_chr="int_cropCommonName_",id_rand, format = "vector", lbl= "Select crop")
      }
        
      dt<- get_ec_harv_inter(allinputs=AllInputs(), addId= id_rand, circm)
      
      #Join fbdesign with harvest header of each crop for intercrop trials
      for(j in 1:length(dt)){
          if(nrow(fbdesign())==0){
            dt[[ circm[j] ]] <- dt[[ circm[j] ]]
          }else {
            dt[[ circm[j] ]] <-cbind(fbdesign() ,dt[[ circm[j] ]] )
          }
      }
      
    }
     dt
     
  })
  lbl_harvest <- reactive({
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector",default = "Monocrop") 
    
    if(ct=="Monocrop" || ct== "Relay crop"){
      addId <- getAddInputId(addId = expCondsVars$ids_harvest, "HARV_", "")
      lbl <- get_ec_harv(allinputs=AllInputs(), addId=addId)$lbl
      if(length(lbl)==0){lbl <- "no-label"}
      
    }else{
      #id_rand_inter <- getAddInputId(intercropVars$ids, "int_", "") 
      #circm <- map_values(input, id_chr="int_cropCommonName_",id_rand_inter, format = "vector", lbl= "Select crop")
      
      #if(ct=="Intercrop"){
        id_rand <- getAddInputId(intercropVars$ids, "int_", "")
        circm <- map_values(input = input, id_chr="int_cropCommonName_",id_rand, format = "vector", lbl= "Select crop")
      #}
      

      
      dt<- get_ec_harv_inter(allinputs=AllInputs(), addId= id_rand, circm)
      lbl<- NULL
      for(i in 1:length(dt)){
        lbl[[i]] <- str_replace_all(string = names(dt[[i]]), pattern = "__[:digit:]+$",replacement = "") 
        names(lbl[[i]])<-circm[i]
      }
    }
    lbl
  })
  ns_harvest <- reactive({
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector",default = "Monocrop") 
    if(ct=="Monocrop" || ct== "Relay crop"){
      addId <- getAddInputId(addId = expCondsVars$ids_harvest, "HARV_", "")
       ns<- get_ns(addId)
     }else{
       ns <- 1
     }
    ns
  }) 
  
  ################################END EXPERIMENT CONDITIONS ########################################

  
  ##################### Phenolgy,  Weather and Soil tables #######################################
  
  #Reactive phenology data #######################################################
  pheno_dt <- reactive({
    ct <- map_singleform_values(input$croppingType,  type = "combo box", format = "vector",default = "Monocrop") 
    ## BEGIN MONORCROP 
    if(ct=="Monocrop"){
      row_select <- input$tblMonoPhe_rows_selected
      dt <- dtMonocropphe[row_select, ]
      lbl <- dt$TraitName
      
      if(length(lbl)==0 && nrow(dt)==0){
        dt <- data.frame()
      } else if(nrow(fbdesign())==0 && length(lbl)>=1){
        dt<- t(rep("", length(lbl)))%>% as.data.frame(stringAsFactors=FALSE)
        names(dt) <- lbl
      } else if(nrow(fbdesign())>0 && length(lbl)>=1) {
        dt<- t(rep("", length(lbl)))%>% as.data.frame(stringAsFactors=FALSE)
        names(dt) <- lbl
        dt <-cbind(fbdesign() ,dt)
      }
      ## END MONORCROP 
      
      # BEGIN INTERCORP
    } else {
      
      ##TODO :CHECK REMOVE THESE FRIST TWO(2) IFS
        if(ct=="Intercrop"){
            id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
            circm <- map_values(input = input, id_chr="int_cropCommonName_",id_ic_rand, format = "vector", lbl= "Select crop")
        }

        if(ct=="Relay crop"){
            id_rc_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
            circm <- map_values(input = input, id_chr="rel_cropCommonName_",id_rc_rand, format = "vector", lbl= "Select crop")
        }
      
      if(ct=="Intercrop"){
          id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
          circm <- map_values(input = input, id_chr="int_cropCommonName_",id_ic_rand, format = "vector", lbl= "Select crop")
          crop_oficial <- c("Cassava","Common bean","Maize",  "Potato",  "Rice",  "Sweetpotato",  "Wheat")
          
          dt<-list()
          #iterate per crop
          for(i in 1:length(circm)){
            
            #TODO: check when crop_row_selected is zero length
            if(circm[i]=="Cassava"){
              phe_row_selected<-input$tblInterPheCassava_rows_selected
              dtPhenoInter <-dtInterPheCassava
            }
            if(circm[i]=="Common bean"){
              phe_row_selected<-input$tblInterPheCommon_rows_selected
              dtPhenoInter <-dtInterPheCommon
            }
            if(circm[i]=="Maize"){
              phe_row_selected<-input$tblInterPheMaize_rows_selected
              dtPhenoInter <-dtInterPheMaize
            }
            if(circm[i]=="Potato"){
              phe_row_selected<-input$tblInterPhePotato_rows_selected
              dtPhenoInter <-dtInterPhePotato
            }
            if(circm[i]=="Rice"){
              phe_row_selected<-input$tblInterPheRice_rows_selected
              dtPhenoInter <-dtInterPheRice
            }
            if(circm[i]=="Sweetpotato"){
              phe_row_selected<-input$tblInterPheSweetpotato_rows_selected
              dtPhenoInter <-dtInterPheSweetpotato
            }
            if(circm[i]=="Wheat"){
              phe_row_selected<-input$tblInterPheWheat_rows_selected
              dtPhenoInter <-dtInterPheWheat
            }
            
            if(!is.element(circm[i],crop_oficial)){
              pos<- i
              
              if(i==1){
                tbl <- dtInterPheOther1
                phe_row_selected<- input[[paste0("tblInterPheOther",i,"_rows_selected")]]
              }else if(i==2){
                tbl <- dtInterPheOther2
                phe_row_selected<- input[[paste0("tblInterPheOther",i,"_rows_selected")]]
              }else if(i==3){
                tbl <- dtInterPheOther3
                phe_row_selected<- input[[paste0("tblInterPheOther",i,"_rows_selected")]]
              }else if(i==4){
                tbl<- dtInterPheOther4
                phe_row_selected<- input[[paste0("tblInterPheOther",i,"_rows_selected")]]
              }else if(i==5){
                tbl <- dtInterPheOther5
                phe_row_selected<- input[[paste0("tblInterPheOther",i,"_rows_selected")]]
              }
              dtPhenoInter <- tbl
              
            }
          
            if(!is.null(phe_row_selected)){
              dt[[i]] <- intercrop_phetables(dtPhenoInter, fbdesign(), phe_row_selected)
            } else {
              dt[[i]] <-  data.frame()
            }
          }
          names(dt) <- circm
      #a<-dt
      }
      
      if(ct=="Relay crop"){

        id_rc_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
        circm <- map_values(input = input, id_chr="rel_cropCommonName_",id_rc_rand, format = "vector", lbl= "Select crop")     
        #crop_oficial <- c("Cassava","Common bean","Maize",  "Potato",  "Rice",  "Sweetpotato",  "Wheat")
        crop_oficial <- c("Cassava","Common bean","Maize",  "Potato",  "Rice",  "Sweetpotato",  "Wheat")
        
       dt<-list()
       
      #iterate per crop
       for(i in 1:length(circm)){

              #TODO: check when crop_row_selected is zero length
              if(circm[i]=="Cassava"){
                phe_row_selected<-input$tblRelayPheCassava_rows_selected
                print(phe_row_selected)
                dtPhenoRelay <-dtRelayPheCassava
                print(dtPhenoRelay)
              }
              if(circm[i]=="Common bean"){
                #print("omar")
                phe_row_selected<-input$tblRelayPheCommon_rows_selected
                print(phe_row_selected)
                dtPhenoRelay <-dtRelayPheCommon
                print(dtPhenoRelay)
              }
              if(circm[i]=="Maize"){
                phe_row_selected<-input$tblRelayPheMaize_rows_selected
                print(phe_row_selected)
                dtPhenoRelay <-dtRelayPheMaize
                print(dtPhenoRelay)
              }
              if(circm[i]=="Potato"){
                phe_row_selected<-input$tblRelayPhePotato_rows_selected
                dtPhenoRelay <-dtRelayPhePotato
              }
              if(circm[i]=="Rice"){
                phe_row_selected<-input$tblRelayPheRice_rows_selected
                dtPhenoRelay <-dtRelayPheRice
              }
              if(circm[i]=="Sweetpotato"){
                phe_row_selected<-input$tblRelayPheSweetpotato_rows_selected
                dtPhenoRelay <-dtRelayPheSweetpotato
              }
              if(circm[i]=="Wheat"){
                phe_row_selected<-input$tblRelayPheWheat_rows_selected
                dtPhenoRelay <-dtRelayPheWheat
              }
              if(!is.element(circm[i],crop_oficial)){
                pos<- i

                if(i==1){
                  tbl <- dtRelayPheOther1
                  phe_row_selected<- input[[paste0("tblRelayPheOther",i,"_rows_selected")]]
                }else if(i==2){
                  tbl <- dtRelayPheOther2
                  phe_row_selected<- input[[paste0("tblRelayPheOther",i,"_rows_selected")]]
                }else if(i==3){
                  tbl <- dtRelayPheOther3
                  phe_row_selected<- input[[paste0("tblRelayPheOther",i,"_rows_selected")]]
                }else if(i==4){
                  tbl<- dtRelayPheOther4
                  phe_row_selected<- input[[paste0("tblRelayPheOther",i,"_rows_selected")]]
                }else if(i==5){
                  tbl <- dtRelayPheOther5
                  phe_row_selected<- input[[paste0("tblRelayPheOther",i,"_rows_selected")]]
                }
                dtPhenoRelay <- tbl

              }

              
              if(!is.null(phe_row_selected)){
                dt[[i]] <- intercrop_phetables(dtPhenoRelay, fbdesign(), phe_row_selected)
              } else {
                dt[[i]] <-  data.frame()
              }
            }
            names(dt) <- circm
        #a<-dt
      }
      
    } 
    ##END INTERCROP 
    dt
  })
  
  ##Reactive weather   ###########################################################
  weather_dt <- reactive({
    
    #wstation<- weather_station_vars #%>% dplyr::select(Measurement, Unit)
    #ww<- dtWeather
    row_select <- sort(input$tblWeather_rows_selected)
    ww<- dtWeather[row_select, ]
   
    
    
    if(nrow(ww)>0){
    
      #NEW CODE : add prefix per season and per plot
      colnames(ww) <- c("Crop","Group","Subgroup","Measurement",
                        "TraitUnit","CropMeasurementPerSeason",
                        "CropMeasurementPerPlot","TraitName", "TraitAlias",
                        "TraitDataType","TraitValidation","VariableId")
      
      cs<- add_season_numplot_prefix(dt=ww)
      #cs<- add_numplot_prefix(dt=ww,cs)
      ww<- cs
      #END NEW CODE
      
      #OLD CODE
      #ww<- ww$TraitName
      
      m <- data.frame(matrix("", ncol = length(ww), nrow = 1),stringsAsFactors = FALSE)
      names(m)<-ww
      ww<-m
    } else {
      # a2 <- data.frame(Measurement = "", TraitUnit = "", TraitAlias = "",
      #                  TraitDataType = "", TraitValidation ="", VariableId= "")
      ww <-  data.frame() #data.frame()
    }
    #print("entro 21")
    dt <- ww
    
  })
  
  ##reactive soil  ###############################################################
  soil_dt<- reactive({
    
    row_select <- sort(input$tblSoil_rows_selected)
    dt <- dtSoil[row_select,  ] #IF row_select ==NULL -> OUTPUT: empty data frame(0 rows/0 cols) WITH headers
    
    #NEW CODE: add prefix per season and per plot
    if(nrow(dt)>0){
      
      colnames(dt) <- c("Crop","Group","Subgroup","Measurement",
                        "TraitUnit","CropMeasurementPerSeason",
                        "CropMeasurementPerPlot","TraitName", "TraitAlias",
                        "TraitDataType","TraitValidation","VariableId")
      cs<- add_season_numplot_prefix(dt=dt)
      #cs<- add_numplot_prefix(dt=dt,cs)
      lbl<- cs
      
        
    } else {
      lbl <- NULL
    }
      
    if(length(lbl)==0){
     dt <- data.frame()
    } else if(nrow(fbdesign())==0 && length(lbl)>=1){
      
      dt<- t(rep("", length(lbl)))%>% as.data.frame(stringAsFactors=FALSE)
      names(dt) <- lbl
    } else if(nrow(fbdesign())>0 && length(lbl)>=1) {
      print("case 3")
      dt<- t(rep("", length(lbl)))%>% as.data.frame(stringAsFactors=FALSE)
      names(dt) <- lbl
      dt <-cbind(fbdesign() ,dt)
    }
    dt 
  })
  
  #############  Metadata ########################################################
  #experiment
  exp_dt<- reactive({
    id<- map_singleform_values(input = input$experimentId, type = "text input",format = "data.frame", label="Experiment ID")
    exname<- map_singleform_values(input = input$experimentName, type = "text input",format = "data.frame", label="Experiment name")
    prname<- map_singleform_values(input = input$experimentProjectName, type = "text input",format = "data.frame", label="Experiment project name")
    sdate<- map_singleform_values(input = input$fbDesign_project_start_date,type = "date",format = "data.frame", label="Experiment start date")
    edate<- map_singleform_values(input = input$fbDesign_project_end_date,type = "date",format = "data.frame", label="Experiment end date")
    type<- map_singleform_values(input = input$designFieldbook_typeExperiment,type = "combo box",format = "data.frame", label="Type of experiment")
    obj<- map_singleform_values(input = input$experimentObj,type = "text input",format = "data.frame", label="Experiment objective")
    out <- rbind(id,exname, prname, sdate, edate, type, obj)
    names(out) <- c("Factor", "Value")
    out
  }) 
  #funding agency
  fa_dt<- reactive({
    #Funding agency type
    id_rand_fa <- getAddInputId(experimentVars$ids_FA, "FA_", "")
    fat <- map_values(input, id_chr="designFieldbook_fundAgencyType_", id_rand_fa, format = "data.frame", lbl= "Funding agency type")
    
    fatn <- map_values(input, id_chr="designFieldbook_fundAgencyType_name_", id_rand_fa,format = "data.frame", lbl= "Funding agency name")
    #fatn_cgiar <- map_values(input, id_chr="designFieldbook_fundAgencyType_cgiar_", id_rand_fa,format = "data.frame", lbl= "Funding agency name")
    
    gn <- map_singleform_values(input = input$experiment_grantNumber,type = "text input",format = "data.frame", label="Grant number")
    gt <- map_singleform_values(input = input$experiment_grantTitle, type="text", format = "data.frame",label = "Grant title")
    out<- rbind(fat, fatn,gn, gt)
    names(out) <- c("Factor", "Value")
    out
  })
  #project management entity
  pe<- reactive({
    # Project entity
    id_rand_pe <- getAddInputId(experimentVars$ids_PE, "PE_", "")
    pe <- map_values(input, id_chr="projEntity_", id_rand_pe, format = "data.frame", lbl= "Project management entity")
    pec <- map_values(input, id_chr="contCenter_", id_rand_pe, format = "data.frame", lbl= "Project management center")
    pecrp <- map_values(input, id_chr="contCRP_", id_rand_pe, format = "data.frame", lbl= "Project management contributor")
    out<- rbind(pe, pec, pecrp)
    names(out) <- c("Factor", "Value")
    out
  })
  #experiment lead organization
  epl<- reactive({
    #Experiment Leads
    id_rand_el <- getAddInputId(experimentVars$ids_EL, "EL_", "")
    pl <- map_values(input, id_chr="projLeadEnt_", id_rand_el,format = "data.frame",lbl= "Experiment, lead organization type")
    plc <- map_values(input, id_chr="tLeadCenter_", id_rand_el, format = "data.frame", lbl= "Experiment, lead organization name")
    pel <- map_values(input, id_chr="expLead_", id_rand_el,format = "data.frame", lbl= "Experiment lead person / Primary Investigator")
   
    out <-rbind(pl,plc,pel)
    names(out) <- c("Factor", "Value")
    out
  })
  #personnel tab
  pers_dt<- reactive({
    #Personnel  
    id_rand_pers <-  getAddInputId(personnelVars$ids_PERS, "PERS_", "") 
    pst <- map_values(input, id_chr="personnel_type_", id_rand_pers,format = "data.frame", lbl= "Person type")
    prfn <- map_values(input, id_chr="person_firstName_", id_rand_pers,format = "data.frame", lbl= "Person, first name")
    prsn <- map_values(input, id_chr="person_lastName_", id_rand_pers,format = "data.frame", lbl= "Person, last name")
    prmail <- map_values(input, id_chr="person_email_", id_rand_pers,format = "data.frame", lbl= "Person email")
    praf <- map_values(input, id_chr="person_affiliation_", id_rand_pers, format = "data.frame", lbl= "Person, affiliation")
    pecen<- map_values(input, id_chr="person_center_", id_rand_pers , format = "data.frame", lbl= "Organization name")
    prorcid <- map_values(input, id_chr="person_orcid_", id_rand_pers,format = "data.frame", lbl= "Person, ORCID")
    out<- rbind(pst, prfn, prsn, prmail, praf, pecen, prorcid)
    names(out) <- c("Factor", "Value")
    out
  })
  #crop
  crop_dt <- reactive({

    #Crop Type
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector",default = "Monocrop") 
    #Table
    ctd <- map_singleform_values(input$croppingType,  type = "combo box", format = "data.frame", "Cropping type") 
    
    #Crop
    #TODO: saber como hacer match entre la tabla circm y cirvar.
    if(ct=="Monocrop"){
      crp <- map_singleform_values(input$cropCommonNameMono, input_other = input$cropCommonNameMono_other, type= "combo box", format = "data.frame",label = "Crop")
      var<- map_singleform_values(input$cultivarNameMono, type= "combo box", format = "data.frame",label = "Crop variety(s)",collapsed = TRUE)
      out <- rbind(ctd, crp, var)
    }
    if(ct=="Intercrop"){
      id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "") 
      circm <- map_values(input, id_chr="int_cropCommonName_", id_ic_rand, format = "data.frame", lbl= "Crop")
      cirvar <- map_values(input, id_chr="int_cropVarietyName_", id_ic_rand,format = "data.frame", lbl= "Crop variety")
      
      #Generate labels according to each crop ####################################
      #Related to issue: 
      #https://github.com/AGROFIMS/hagrofims/issues/166#issuecomment-474452350
      circm_lbl <- map_values(input, id_chr="int_cropCommonName_", id_ic_rand, format = "vector", lbl= "Crop")
      cil<- reactiveValuesToList(input)
      id_chr <- "int_cropVarietyName_"
      crim_val<- crim_lbl <- NULL
      for(i in 1:length(id_ic_rand)){
        if( is.null( cil[[paste0(id_chr, id_ic_rand[i])]] )){
          crim_val[[i]]<-"-"
          crim_lbl[[i]]<- paste(circm_lbl[i], "variety", 1:length(crim_val[[i]]))
        } else{
          crim_val[[i]]<- cil[[paste0(id_chr, id_ic_rand[i])]]
          crim_lbl[[i]] <- paste(circm_lbl[i], "variety", 1:length(crim_val[[i]]))
        }
      }
      crim_lbl<- unlist(crim_lbl)
      #Changing labels in crop table for intercrop
      cirvar[,1]<-crim_lbl
      ################################################################################
      
      ciarre<- map_singleform_values(input = input$fr_intercrop_arrangement, 
                                     type="combo box",format = "data.frame", label= "Intercrop arragement")
      
      row <- map_values(input, id_chr="intercropValue_row_crop_", id_ic_rand, format = "data.frame", lbl= "Row geometry")
      out <- rbind(ctd, circm, cirvar, ciarre, row) 
    } 
    if(ct=="Relay crop"){
      
      id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
      circm <- map_values(input, id_chr="rel_cropCommonName_", id_re_rand, format = "data.frame", lbl= "Select crop")
      cirvar <- map_values(input, id_chr="rel_cropVarietyName_", id_re_rand,format = "data.frame", lbl= "Crop variety(s)")
      #ciarre<- map_singleform_values(input = input$fr_intercrop_arrangement, 
      #                               type="combo box",format = "data.frame", label= "Intercrop arragement")
      #row <- map_values(input, id_chr="intercropValue_row_crop_", id_ic_rand, format = "data.frame", lbl= "Row geometry")
      out <- rbind(ctd, circm, cirvar) 
      
    }
    
    pvc<- map_singleform_values(input$prevCropName,input_other = input$prevCropName_other, type= "combo box", format = "data.frame",  label= "Previous crop")
    out <- rbind(out, pvc)
    names(out)<- c("Factor", "Value")
    print("out")
    print(out)
    out
  })
  
  
  #### Design tab ###############################################################
  #Unit in design
  infounit<- reactive({
   
    if(input$designFieldbook_agrofims=="CRD" ||input$designFieldbook_agrofims=="RCBD"||  input$designFieldbook_agrofims=="FCRD"||
       input$designFieldbook_agrofims=="FRCBD"){
     
      ifunit<- agdesign::map_singleform_values(input$info_experiment_unit, type="select")
      if(ifunit==""){
        out <- data.frame(Factor = c("Information on experimental unit","Width", "Length"), 
                          Value = c("","","") ,stringsAsFactors = FALSE )
      }
      if(ifunit == "plot"){
        
        wi <- map_singleform_values(input = input$expt_plot_width, type = "text",format = "vector", label = "Factor") 
        wunit <- map_singleform_values(input = input$expt_plot_width_unit, type = "combo box",format = "vector", label = "Factor")
        len <- map_singleform_values(input = input$expt_plot_length   , type = "text",format = "vector", label = "Factor") 
        lunit <-  map_singleform_values(input = input$expt_plot_length_unit, type = "combo box",format = "vector", label = "Factor")
        wif<- paste(wi, wunit)
        lenf<-  paste(len, lunit)
        iou<- data.frame(Factor = "Information on experimental unit", Value = ifunit )
        ow<- data.frame(Factor = "Experimental plot width", Value = wif )
        ol<- data.frame(Factor = "Experimental plot length",Value = lenf )
        out<- rbind(iou, ow, ol)
        
      }
      if(ifunit == "field"){
        
        wi <- map_singleform_values(input = input$expt_field_width , type = "text",format = "vector", label = "Factor") 
        wunit <- map_singleform_values(input = input$expt_field_width_unit, type = "combo box",format = "vector", label = "Factor")
        len <- map_singleform_values(input = input$expt_field_length   , type = "text",format = "vector", label = "Factor") 
        lunit <-  map_singleform_values(input = input$expt_field_length_unit, type = "combo box",format = "vector", label = "Factor")
        wif<- paste(wi, wunit)
        lenf<-  paste(len, lunit)
        iou<- data.frame(Factor = "Information on experimental unit", Value = ifunit )
        ow<- data.frame(Factor = "Experimental field width", Value = wif )
        ol<- data.frame(Factor = "Experimental field length", Value = lenf )
        out<- rbind(iou,ow, ol)
      } 
      if(ifunit == "pot"){
        
        di <- map_singleform_values(input = input$pot_diameter , type = "text",format = "vector", label = "Factor") 
        dunit <- map_singleform_values(input = input$pot_diameter_unit, type = "combo box",format = "vector", label = "Factor")
        de <- map_singleform_values(input = input$pot_depth   , type = "text",format = "vector", label = "Factor") 
        deunit <-  map_singleform_values(input = input$pot_depth_unit, type = "combo box",format = "vector", label = "Factor")
        dif<- paste(di, dunit)
        def<-  paste(de, deunit)
        iou<- data.frame(Factor = "Information on experimental unit", Value = ifunit )
        ow<- data.frame(Factor = "Experimental pot width", Value = dif )
        ol<- data.frame(Factor = "Experimental plot length", Value = def )
        out<- rbind(iou, ow, ol)
        
      }
      
    } 
    else if(input$designFieldbook_agrofims=="SPRCBD"){
    
      ## Main ##
      wi_main <- map_singleform_values(input = input$sprcbd_main_expt_plot_width, type = "text",format = "vector", label = "Factor") 
      wunit_main <- map_singleform_values(input = input$sprcbd_main_expt_plot_width_unit, type = "combo box",format = "vector", label = "Factor")
      len_main <- map_singleform_values(input = input$sprcbd_main_expt_plot_length   , type = "text",format = "vector", label = "Factor") 
      lunit_main <-  map_singleform_values(input = input$sprcbd_main_expt_plot_length_unit, type = "combo box",format = "vector", label = "Factor")
      wif_main <- paste(wi_main, wunit_main)
      lenf_main <-  paste(len_main, lunit_main)
      
      iou_main<- data.frame(Factor = "Information on experimental unit", Value = "Main plot" )
      ow_main<- data.frame(Factor = "Experimental plot width", Value = wif_main )
      ol_main<- data.frame(Factor = "Experimental plot length",Value = lenf_main )
      
      ## Sub plot ##
      
      wi_sub <- map_singleform_values(input = input$sprcbd_sub_expt_plot_width, type = "text",format = "vector", label = "Factor") 
      wunit_sub <- map_singleform_values(input = input$sprcbd_sub_expt_plot_width_unit, type = "combo box",format = "vector", label = "Factor")
      len_sub <- map_singleform_values(input = input$sprcbd_sub_expt_plot_length   , type = "text",format = "vector", label = "Factor") 
      lunit_sub <-  map_singleform_values(input = input$sprcbd_sub_expt_plot_length_unit, type = "combo box",format = "vector", label = "Factor")
      wif_sub <- paste(wi_sub, wunit_sub)
      lenf_sub <-  paste(len_sub, lunit_sub)
      
      iou_sub<- data.frame(Factor = "Information on experimental unit", Value = "Sub plot" )
      ow_sub<- data.frame(Factor = "Experimental plot width", Value = wif_sub )
      ol_sub<- data.frame(Factor = "Experimental plot length",Value = lenf_sub )
      
      ## Consolidation of all main, sub plot
      out_main <- rbind(iou_main, ow_main, ol_main)
      out_sub<- rbind(iou_sub, ow_sub, ol_sub)
      out<- rbind(out_main, out_sub)
      
        
    } 
    else if(input$designFieldbook_agrofims=="SPSP"){
     
      wi_main <- map_singleform_values(input = input$spsp_main_expt_plot_width, type = "text",format = "vector", label = "Factor") 
      wunit_main <- map_singleform_values(input = input$spsp_main_expt_plot_width_unit, type = "combo box",format = "vector", label = "Factor")
      len_main <- map_singleform_values(input = input$spsp_main_expt_plot_length   , type = "text",format = "vector", label = "Factor") 
      lunit_main <-  map_singleform_values(input = input$spsp_main_expt_plot_length_unit, type = "combo box",format = "vector", label = "Factor")
      wif_main <- paste(wi_main, wunit_main)
      lenf_main <-  paste(len_main, lunit_main)
      
      iou_main<- data.frame(Factor = "Information on experimental unit", Value = "Main plot" )
      ow_main<- data.frame(Factor = "Experimental plot width", Value = wif_main )
      ol_main<- data.frame(Factor = "Experimental plot length",Value = lenf_main )
      
      ## Sub plot ##
      
      wi_sub <- map_singleform_values(input = input$spsp_sub_expt_plot_width, type = "text",format = "vector", label = "Factor") 
      wunit_sub <- map_singleform_values(input = input$spsp_sub_expt_plot_width_unit, type = "combo box",format = "vector", label = "Factor")
      len_sub <- map_singleform_values(input = input$spsp_sub_expt_plot_length   , type = "text",format = "vector", label = "Factor") 
      lunit_sub <-  map_singleform_values(input = input$spsp_sub_expt_plot_length_unit, type = "combo box",format = "vector", label = "Factor")
      wif_sub <- paste(wi_sub, wunit_sub)
      lenf_sub <-  paste(len_sub, lunit_sub)
      
      iou_sub<- data.frame(Factor = "Information on experimental unit", Value = "Sub plot" )
      ow_sub<- data.frame(Factor = "Experimental sub plot width", Value = wif_sub )
      ol_sub<- data.frame(Factor = "Experimental sub plot length",Value = lenf_sub )
      
      ## Sub-Sub Plot ##
      
      wi_subsub <- map_singleform_values(input = input$spsp_subsub_expt_plot_width, type = "text",format = "vector", label = "Factor") 
      wunit_subsub <- map_singleform_values(input = input$spsp_subsub_expt_plot_width_unit, type = "combo box",format = "vector", label = "Factor")
      len_subsub <- map_singleform_values(input = input$spsp_subsub_expt_plot_length   , type = "text",format = "vector", label = "Factor") 
      lunit_subsub <-  map_singleform_values(input = input$spsp_subsub_expt_plot_length_unit, type = "combo box",format = "vector", label = "Factor")
      wif_subsub <- paste(wi_subsub, wunit_subsub)
      lenf_subsub <-  paste(len_subsub, lunit_subsub)
      
      iou_subsub<- data.frame(Factor = "Information on experimental unit", Value = "Sub-sub plot" )
      ow_subsub<- data.frame(Factor = "Experimental sub-sub plot width", Value = wif_subsub )
      ol_subsub<- data.frame(Factor = "Experimental sub-sub plot length",Value = lenf_subsub )
      
      ## Consolidation of all main, sub and sub-sub plot
      out_main <- rbind(iou_main, ow_main, ol_main)
      out_sub <- rbind(iou_sub, ow_sub, ol_sub)
      out_subsub <- rbind(iou_subsub, ow_subsub, ol_subsub)
      out <- rbind(out_main, out_sub,out_subsub)
      
    }
    
    out
  })
  
  #### Integration of all the Metadata ##########################################
  globalMetadata<- function(){
     fl_dt <- get_faclevdt(design=input$designFieldbook_agrofims, allinputs=AllInputs() )
     vers_dt <- data.frame(Factor = "Version", Value= "test version 23")
     gtable <- rbind( exp_dt(), fa_dt(), pe(), epl(), pers_dt(),crop_dt(), infounit(),
                      #TODO:: MEJORAR
                      fl_dt,
                      site_dt(),
                      vers_dt
                      )

     names(gtable)[1]<- "Parameter"
     gtable
   }
  
  ### Fieldbook design (statistical design) ########################################
  fbdesign <- function(){
  
    #Get statistical design abbreviation
    design <- tolower(input$designFieldbook_agrofims) #lowercase
    #Get IDS from design inputs
    IdDesignInputs <- getFactorIds(design)
    #Get index from Design's IDs
    index <- get_index_design(IdDesignInputs, design)
    
    try({
    
    if(design=="crd"){
      #ntrt <- as.integer(input$crd_ntrt)
      rep<- as.integer(input$crd_rep)
      
      #flbl<- get_factors_design(allinputs = AllInputs(), index, design = "crd")
      flvl<- get_nonfactorial_levels(input,"crd")
      fb<- fbdesign_agrofims(design=design, rep=rep,trt=flvl) 
                              
    } else if(design =="rcbd"){
      block<- as.integer(input$rcbd_rep)
     
      #flbl<- get_factors_design(allinputs = AllInputs(), index, design = "fcrd")
      flvl<- get_nonfactorial_levels(input,"rcbd")
      fb<- fbdesign_agrofims(design=design, block=block, trt=flvl)
                             
    } else if(design=="fcrd"){
      rep <- as.integer(input$fcrd_rep)
      flbl<- get_factors_design(allinputs = AllInputs(), index, design = "fcrd")
      flvl<- get_levels_design(allinputs = AllInputs(), data_dictionary=dtfactordesign,
                               index, factors = flbl, design="fcrd", format="list")
      fb<- fbdesign_agrofims(design=design, rep=rep,  fnames= flbl, flevels= flvl) 
    } 
    else if(design=="frcbd"){
      block<- as.integer(input$frcbd_block)
      flbl<- get_factors_design(allinputs = AllInputs(),index,  design = "frcbd")
      print(flbl)
      flvl <- get_levels_design(allinputs = AllInputs(),data_dictionary=dtfactordesign, 
                                index, factors = flbl, design="frcbd", format="list")
      print(flvl)
      fb<- fbdesign_agrofims(design=design, rep=block,  fnames= flbl, flevels= flvl) 
    } else if(design =="sprcbd"){
    
      block <- as.integer(input$sp1_block)
      flbl<- get_factors_design(allinputs = AllInputs(), index, design = "sprcbd")
      flvl<- get_levels_design(allinputs = AllInputs(),data_dictionary=dtfactordesign,
                               index, factors = flbl, design="sprcbd", format="list")
      fb<- fbdesign_agrofims(design=design, block=block,  fnames= flbl, flevels= flvl) 
      
    } else if(design =="spsp"){
      block<- as.integer(input$spsp2_block)
      flbl<- get_factors_design(allinputs = AllInputs(), index, design = "spsp")
      flvl<- get_levels_design(allinputs = AllInputs(), data_dictionary=dtfactordesign,
                               index, factors = flbl, design="spsp", format="list")
      fb<- fbdesign_agrofims(design=design, block=block,  fnames= flbl, flevels= flvl)
    }
    
    fb
    }) #end try
  }
  
  ### Site data #####################################################################
  site_dt <- reactive({

    vsitetype <- ""
    vsitename <- ""
    vsiteId <- ""
    vsiteCountry <- ""
    vsiteadmin1 <- ""
    vsiteadmin2 <- ""
    vsiteVillage <- ""
    vsitenear <- ""
    vsiteElev <- ""
    vsiteLat <- ""
    visteLon <- ""

    if(!is.null(input$fbDesign_countryTrial) && !is.null(input$designFieldbook_sites)){
      vsiteCountry <- input$fbDesign_countryTrial

      xpath <- fbglobal::get_base_dir()
      xfp <- file.path(path, "table_sites_agrofims.rds")

      xaux <- input$designFieldbook_sites
      vsiteId <- xaux

      x_sites_data <- readRDS(file = xfp)
      data <- dplyr::filter(x_sites_data, shortn==vsiteId)
      if(nrow(data) != 0){
        xsite <- data[1,]
        vsitetype <- xsite$Type
        vsitename <- xsite$local
        vsiteadmin1 <- xsite$adm1
        vsiteadmin2 <- xsite$adm2
        vsiteVillage <- xsite$village
        vsitenear <- xsite$nearpop
        vsiteElev <- xsite$elev
        vsiteLat <- xsite$latd
        visteLon <- xsite$lond
      }


    }

    c26 <- c('Site type',vsitetype)
    c27 <- c('Site name',vsitename)
    c28 <- c('Site ID', vsiteId)
    c29 <- c('Country name', vsiteCountry)
    c30 <- c('Site, first-level administrative division name',vsiteadmin1 )
    c31 <- c('Site, second-level administrative division name',vsiteadmin2 )
    c32 <- c('Village name', vsiteVillage)
    c33 <- c('Nearest populated place', vsitenear)
    c34 <- c('Site elevation',vsiteElev )
    c35 <- c('Site latitude (in decimal degrees)', vsiteLat)
    c36 <- c('Site longitude (in decimal degrees)',visteLon )

    vHighLevel <- ""
    if(!is.null(input$fbDesign_inHighLevel)) vHighLevel <- input$fbDesign_inHighLevel

    c37 <- c('Higher-level landform',vHighLevel)

    vSiteVegetation <- ""
    if(!is.null(input$fbDesign_inSiteVegetation)) vSiteVegetation <- paste(input$fbDesign_inSiteVegetation, collapse = ",")

    c38 <- c('Vegetation surrounding the experimental site', vSiteVegetation)
    c39 <- c('Site description notes', input$inSiteDescNotes)


    out <- data.frame(c26,	c27,	c28,	c29,	c30,	c31,	c32,	c33,	c34,	c35,	c36,	c37,	c38,	c39)
    out<- as.data.frame(t(out), stringsAsFactors=FALSE)
    names(out)<- c("Factor", "Value")
    out

  })
  

  ### Trait variables data #####################################################################
  traits_dt <- function(){
    
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector",default = "Monocrop") 
    
    if(ct=="Monocrop"){
      a<- dtMonocrop #fg()
      if(nrow(a) >0){
        
        colnames(a) <- c("Crop","Group","Subgroup","Measurement",
                         "TraitUnit","CropMeasurementPerSeason",
                         "CropMeasurementPerPlot","TraitAlias",
                         "TraitDataType","TraitValidation","VariableId")

        row_select <- input$tblMono_rows_selected
        row_select <- sort(row_select)
        aux_dt<- a[row_select,]
        #Remove Status column
        aux_dt$Status <- NULL
        
        #Place TraitName in traits_dt()
        cr<- aux_dt$Crop
        sb<- aux_dt$Subgroup
        cm <- aux_dt$Measurement
        sc <- aux_dt$TraitUnit
        sc[is.na(sc)] <- "unitless"
        cs <- paste(cr,sb, cm, sc, sep="_")
        cs<- stringr::str_replace_all(cs,pattern = "[[:space:]]","_")
        
        aux_dt$TraitName <- cs
        # Asign final trait_dt to a  
        a<- aux_dt
        
      } 
    } 
    else {
      #For intercrop trial
      if(ct=="Intercrop"){
        id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "") 
        circm <- map_values(input = input, id_chr="int_cropCommonName_",id_ic_rand, format = "vector", lbl= "Select crop")
        dt<-list()
        crop_oficial <- c("Cassava","Common bean","Maize",  "Potato",  "Rice",  "Sweetpotato",  "Wheat")
        
        for(i in 1:length(circm)){
          
          #TODO: check when crop_row_selected is zero length
          if(circm[i]=="Cassava"){
            inter_row_selected<-input$tblInterCassava_rows_selected
            dtInterCrop<- dtInterCassava    
          }
          if(circm[i]=="Common bean"){
            inter_row_selected<-input$tblInterCommon_rows_selected
            dtInterCrop<-dtInterCommon
          }
          if(circm[i]=="Maize"){
            inter_row_selected<-input$tblInterMaize_rows_selected
            dtInterCrop<-dtInterMaize 
          }
          if(circm[i]=="Potato"){
            inter_row_selected<-input$tblInterPotato_rows_selected
            dtInterCrop<-dtInterPotato
          }
          if(circm[i]=="Rice"){
            inter_row_selected<-input$tblInterRice_rows_selected
            dtInterCrop<-dtInterRice
          }
          if(circm[i]=="Sweetpotato"){
            inter_row_selected<-input$tblInterSweetpotato_rows_selected
            dtInterCrop<-dtInterSweetpotato
          }
          if(circm[i]=="Wheat"){
            inter_row_selected<-input$tblInterWheat_rows_selected
            dtInterCrop<-dtInterWheat
          }  
          if(!is.element(circm[i],crop_oficial)){
            # inter_row_selected<- input$tblInterOther_rows_selected
            # dtInterCrop<-dtInterOther
            pos<- i
            
            if(i==1){
              tbl <- dtInterOther1
              inter_row_selected<- input[[paste0("tblInterOther",i,"_rows_selected")]]
            }else if(i==2){
              tbl <- dtInterOther2
              inter_row_selected<- input[[paste0("tblInterOther",i,"_rows_selected")]]
            }else if(i==3){
              tbl <- dtInterOther3
              inter_row_selected<- input[[paste0("tblInterOther",i,"_rows_selected")]]
            }else if(i==4){
              tbl<- dtInterOther4
              inter_row_selected<- input[[paste0("tblInterOther",i,"_rows_selected")]]
            }else if(i==5){
              tbl <- dtInterOther5
              inter_row_selected<- input[[paste0("tblInterOther",i,"_rows_selected")]]
            }
            dtInterCrop<- tbl
            
          }      
          
          
          
          if(!is.null(inter_row_selected)){  
            dt[[i]] <- intercrop_cmtables(dtInterCrop ,inter_row_selected) 
          } else {
            dt[[i]] <- data.frame(Status="",Crop="", Group="", Subgroup="", Measurement="",
                                  Measurement_2="",Measurement_3="",
                                  TraitUnit="", TraitAlias="", TraitDataType="",
                                  TraitValidation="", VariableId="",
                                  v1= "", v2="", v3="")
          }
          
        }
        names(dt) <- circm
        a<-dt
      }
      
      else if(ct=="Relay crop"){
        id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
        circm <- map_values(input = input, id_chr="rel_cropCommonName_",id_re_rand, format = "vector", lbl= "Select crop")
        
        dt<-list()
        crop_oficial <- c("Cassava","Common bean","Maize",  "Potato",  "Rice",  "Sweetpotato",  "Wheat")
        
        for(i in 1:length(circm)){
          
          #TODO: check when crop_row_selected is zero length
          if(circm[i]=="Cassava"){
            relay_row_selected<-input$tblRelayCassava_rows_selected
            dtRelayCrop<- dtRelayCassava    
          }
          if(circm[i]=="Common bean"){
            relay_row_selected<-input$tblRelayCommon_rows_selected
            dtRelayCrop<-dtRelayCommon
          }
          if(circm[i]=="Maize"){
            relay_row_selected<-input$tblRelayMaize_rows_selected
            dtRelayCrop<-dtRelayMaize 
          }
          if(circm[i]=="Potato"){
            relay_row_selected<-input$tblRelayPotato_rows_selected
            dtRelayCrop<-dtRelayPotato
          }
          if(circm[i]=="Rice"){
            relay_row_selected<-input$tblRelayRice_rows_selected
            dtRelayCrop<-dtRelayRice
          }
          if(circm[i]=="Sweetpotato"){
            relay_row_selected<-input$tblRelaySweetpotato_rows_selected
            dtRelayCrop<-dtRelaySweetpotato
          }
          if(circm[i]=="Wheat"){
            relay_row_selected<-input$tblRelayWheat_rows_selected
            dtRelayCrop<-dtRelayWheat
          }  
          if(!is.element(circm[i],crop_oficial)){
            # inter_row_selected<- input$tblInterOther_rows_selected
            # dtInterCrop<-dtInterOther
            pos<- i
            
            if(i==1){
              tbl <- dtRelayOther1
              relay_row_selected<- input[[paste0("tblRelayOther",i,"_rows_selected")]]
            }else if(i==2){
              tbl <- dtRelayOther2
              relay_row_selected<- input[[paste0("tblRelayOther",i,"_rows_selected")]]
            }else if(i==3){
              tbl <- dtRelayOther3
              relay_row_selected<- input[[paste0("tblRelayOther",i,"_rows_selected")]]
            }else if(i==4){
              tbl<- dtRelayOther4
              relay_row_selected<- input[[paste0("tblRelayOther",i,"_rows_selected")]]
            }else if(i==5){
              tbl <- dtRelayOther5
              relay_row_selected<- input[[paste0("tblIRelayOther",i,"_rows_selected")]]
            }
            dtRelayCrop<- tbl
            
          }      
          
          
          
          if(!is.null(relay_row_selected)){  
            dt[[i]] <- intercrop_cmtables(dtRelayCrop ,relay_row_selected) 
          } else {
            dt[[i]] <- data.frame(Status="",Crop="", Group="", Subgroup="", Measurement="",
                                  Measurement_2="",Measurement_3="",
                                  TraitUnit="", TraitAlias="", TraitDataType="",
                                  TraitValidation="", VariableId="",
                                  v1= "", v2="", v3="")
          }
          
        }
        names(dt) <- circm                     
        a<-dt
      }
      
    }
    return(a)
  }
  
  ### Fieldbook + Traits for Monocrop ##########################################################
  fbdesign_traits <- reactive({
    
    fb <- fbdesign()
    trait_dt <- traits_dt()
    print(trait_dt)

    ##NEW CODE
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector",default = "Monocrop") 
    
    if(ct=="Monocrop"){
      
      cr<- trait_dt$Crop
      sb<- trait_dt$Subgroup
      cm <- trait_dt$Measurement
      cm <- trait_dt$Crop.measurement
      sc <- trait_dt$Scale
      sc <- trait_dt$TraitUnit
      
      sc[is.na(sc)] <- "unitless"
      #co <- trait$VariableId
      cs <- paste(cr,sb, cm, sc, sep="_")
      cs<- stringr::str_replace_all(cs,pattern = "[[:space:]]","_")
      
      cs<- add_season_numplot_prefix(dt=trait_dt) #trait is a table
      #cs<- add_numplot_prefix(dt=trait_dt,cs)
    } else{
      cs<-NULL
    }
    
    ## NEW CODE
    
    #trait_selected <- trait_agrofims() %>% as.data.frame(stringsAsFactors =FALSE) #unlist(shinyTree::get_selected(input$designFieldbook_traits_agrofims))
    trait_selected <- cs
     
    if(!is.null(trait_selected) || length(trait_selected)==0 ){
      mm  <-  matrix(nrow = nrow(fb), ncol = length(trait_selected) )
      nm  <-  c(names(fb), trait_selected)
      fb  <-  cbind(fb, mm)
      names(fb)  <-  nm
    }
    
    fb
    
  })
  
  ### Fieldbook + Traits for multicrop trials ##########################################################
  fbdesign_mult_traits <- reactive({
    
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector",default = "Intercrop") 
    
    if(ct=="Intercrop"){
      id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
      crop_name <- map_values(input = input, id_chr="int_cropCommonName_",id_ic_rand, format = "vector", lbl= "Select crop")
    }
    if(ct=="Relay crop"){
      id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
      crop_name <- map_values(input = input, id_chr="rel_cropCommonName_",id_re_rand, format = "vector", lbl= "Select crop")
      
    }
    if(ct!="Monocrop"){
      
      fb <- fbdesign()
      trait <- traits_dt()
      fb_inter <- list()
      for(i in 1:length(trait)){
        
        cr<- trait[[i]]$Crop
        sb<- trait[[i]]$Subgroup
        cm <- trait[[i]]$Measurement
        sc <- trait[[i]]$TraitUnit
        sc[is.na(sc)] <- "unitless"
        cs <- paste(cr,sb, cm, sc, sep="-")
        cs <- stringr::str_replace_all(cs,"[[:space:]]","_")
        #trait_selected <- trait_agrofims() %>% as.data.frame(stringsAsFactors =FALSE) #unlist(shinyTree::get_selected(input$designFieldbook_traits_agrofims))
        ## CHECK IF Trait table for each crop has information of the crop, in case not skip
        if(trait[[i]]$Crop[1]!=""){
        cs<- add_season_numplot_prefix(dt=trait[[i]])
        #cs<- add_numplot_prefix(dt=trait[[i]],cs)
        }
        trait_selected <- cs
        
        
        if(!is.null(trait_selected) || length(trait_selected)==0 ){
          mm  <-  matrix(nrow = nrow(fb), ncol = length(trait_selected) )
          nm  <-  c(names(fb), trait_selected)
          fb_inter[[i]]  <-  cbind(fb, mm)
          names(fb_inter[[i]])  <-  nm
          if(is.element("---",names(fb_inter[[i]]))){ fb_inter[[i]][,"---"]<-NULL } 
        }
      }
      names(fb_inter) <- crop_name
      fb <-fb_inter
    }
    fb 
  })
  
  #Phenologic var for Multicrop trials 
  pheno_inter_vars<- reactive({
    
    
    ct <- map_singleform_values(input$croppingType,  type = "combo box", format = "vector",default = "Monocrop") 
    dt<-list()
    ## BEGIN MONORCROP 
    if(ct!="Monocrop"){
    
      if(ct=="Intercrop"){
        id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "") 
        circm <- map_values(input = input, id_chr="int_cropCommonName_",id_ic_rand, format = "vector", lbl= "Select crop")
      }
      
      if(ct=="Relay crop"){
        id_rc_rand <- getAddInputId(relaycropVars$ids, "rel_", "") 
        circm <- map_values(input = input, id_chr="rel_cropCommonName_",id_rc_rand, format = "vector", lbl= "Select crop")
      }
      
      print(circm)
      
      if(ct=="Intercrop"){
        dt<-list()
        crop_oficial <- c("Cassava","Common bean","Maize",  "Potato",  "Rice",  "Sweetpotato",  "Wheat")
        #phe_row_selected<-NULL
        
        #iterate per crop
        for(i in 1:length(circm)){
          
          #TODO: check when crop_row_selected is zero length
          if(circm[i]=="Cassava"){
            phe_row_selected<-input$tblInterPheCassava_rows_selected
            dtPhenoInter <-dtInterPheCassava
          }
          if(circm[i]=="Common bean"){
            print("omar")
            phe_row_selected<-input$tblInterPheCommon_rows_selected
            dtPhenoInter <-dtInterPheCommon
          }
          if(circm[i]=="Maize"){
            phe_row_selected<-input$tblInterPheMaize_rows_selected
            dtPhenoInter <-dtInterPheMaize
          }
          if(circm[i]=="Potato"){
            phe_row_selected<-input$tblInterPhePotato_rows_selected
            dtPhenoInter <-dtInterPhePotato
          }
          if(circm[i]=="Rice"){
            phe_row_selected<-input$tblInterPheRice_rows_selected
            dtPhenoInter <-dtInterPheRice
          }
          if(circm[i]=="Sweetpotato"){
            phe_row_selected<-input$tblInterPheSweetpotato_rows_selected
            dtPhenoInter <-dtInterPheSweetpotato
          }
          if(circm[i]=="Wheat"){
            phe_row_selected<-input$tblInterPheWheat_rows_selected
            dtPhenoInter <-dtInterPheWheat
          }  
          if(!is.element(circm[i],crop_oficial)){
            pos<- i
            
            if(i==1){
              tbl <- dtInterPheOther1
              phe_row_selected<- input[[paste0("tblInterPheOther",i,"_rows_selected")]]
            }else if(i==2){
              tbl <- dtInterPheOther2
              phe_row_selected<- input[[paste0("tblInterPheOther",i,"_rows_selected")]]
            }else if(i==3){
              tbl <- dtInterPheOther3
              phe_row_selected<- input[[paste0("tblInterPheOther",i,"_rows_selected")]]
            }else if(i==4){
              tbl<- dtInterPheOther4
              phe_row_selected<- input[[paste0("tblInterPheOther",i,"_rows_selected")]]
            }else if(i==5){
              tbl <- dtInterPheOther5
              phe_row_selected<- input[[paste0("tblInterPheOther",i,"_rows_selected")]]
            }
            dtPhenoInter <- tbl
            
          }
          
          if(!is.null(phe_row_selected)){  
            dt[[i]] <- intercrop_phe_vars(dtPhenoInter, phe_row_selected) 
            dt[[i]]$Crop <- circm[i]
            
            colnames(dt[[i]]) <- c("Crop","Group","Subgroup","Measurement",
                                   "TraitUnit","CropMeasurementPerSeason",
                                   "CropMeasurementPerPlot","TraitName", "TraitAlias",
                                   "TraitDataType","TraitValidation","VariableId")
            
            
            dt[[i]]$CropMeasurementPerSeason<-1
            dt[[i]]$CropMeasurementPerPlot<-1
          } else {
            dt[[i]] <- data.frame(Status="",Crop="", Group="", Subgroup="", Measurement="",
                                  Measurement_2="",Measurement_3="",
                                  TraitUnit="", TraitAlias="", TraitDataType="",
                                  TraitValidation="", VariableId="",
                                  v1= "", v2="", v3="")
          }
        }
        names(dt) <- circm
      }
      if(ct=="Relay crop"){
        dt<-list()
        crop_oficial <- c("Cassava","Common bean","Maize",  "Potato",  "Rice",  "Sweetpotato",  "Wheat")
        #phe_row_selected<-NULL
        
        #iterate per crop
        for(i in 1:length(circm)){
          
          #TODO: check when crop_row_selected is zero length
          if(circm[i]=="Cassava"){
            phe_row_selected<-input$tblRelayPheCassava_rows_selected
            dtPhenoRelay <-dtRelayPheCassava
          }
          if(circm[i]=="Common bean"){
            print("omar")
            phe_row_selected<-input$tblRelayPheCommon_rows_selected
            dtPhenoRelay <-dtRelayPheCommon
          }
          if(circm[i]=="Maize"){
            phe_row_selected<-input$tblRelayPheMaize_rows_selected
            dtPhenoRelay <-dtRelayPheMaize
          }
          if(circm[i]=="Potato"){
            phe_row_selected<-input$tblRelayPhePotato_rows_selected
            dtPhenoRelay <-dtRelayPhePotato
          }
          if(circm[i]=="Rice"){
            phe_row_selected<-input$tblRelayPheRice_rows_selected
            dtPhenoRelay <-dtRelayPheRice
          }
          if(circm[i]=="Sweetpotato"){
            phe_row_selected<-input$tblRelayPheSweetpotato_rows_selected
            dtPhenoRelay <-dtRelayPheSweetpotato
          }
          if(circm[i]=="Wheat"){
            phe_row_selected<-input$tblRelayPheWheat_rows_selected
            dtPhenoRelay <-dtRelayPheWheat
          }  
          if(!is.element(circm[i],crop_oficial)){
            pos<- i
            
            if(i==1){
              tbl <- dtRelayPheOther1
              phe_row_selected<- input[[paste0("tblRelayPheOther",i,"_rows_selected")]]
            }else if(i==2){
              tbl <- dtRelayPheOther2
              phe_row_selected<- input[[paste0("tblRelayPheOther",i,"_rows_selected")]]
            }else if(i==3){
              tbl <- dtRelayPheOther3
              phe_row_selected<- input[[paste0("tblRelayPheOther",i,"_rows_selected")]]
            }else if(i==4){
              tbl<- dtRelayPheOther4
              phe_row_selected<- input[[paste0("tblRelayPheOther",i,"_rows_selected")]]
            }else if(i==5){
              tbl <- dtRelayPheOther5
              phe_row_selected<- input[[paste0("tblRelayPheOther",i,"_rows_selected")]]
            }
            dtPhenoRelay <- tbl
            
          }
          
          if(!is.null(phe_row_selected)){  
            dt[[i]] <- intercrop_phe_vars(dtPhenoRelay, phe_row_selected) 
            dt[[i]]$Crop <- circm[i]
            
            colnames(dt[[i]]) <- c("Crop","Group","Subgroup","Measurement",
                                   "TraitUnit","CropMeasurementPerSeason",
                                   "CropMeasurementPerPlot","TraitName", "TraitAlias",
                                   "TraitDataType","TraitValidation","VariableId")
            
            
            dt[[i]]$CropMeasurementPerSeason<-1
            dt[[i]]$CropMeasurementPerPlot<-1
          } else {
            dt[[i]] <- data.frame(Status="",Crop="", Group="", Subgroup="", Measurement="",
                                  Measurement_2="",Measurement_3="",
                                  TraitUnit="", TraitAlias="", TraitDataType="",
                                  TraitValidation="", VariableId="",
                                  v1= "", v2="", v3="")
          }
        }
        names(dt) <- circm
        
      }
      
     
    } 
  dt
    
})
  
  
  ### Book preview ##################################################################
  shiny::observeEvent(input$fbDesign_draft_agrofims, {

     withProgress(message = 'Fieldbook Preview', value = 0, {

      incProgress(1/10,message = "...")
       
       # #Flag variable to know if everything is ok
        flag <- TRUE
       
       #Get crop
       #Monocrop
       ct<- input$croppingType  
       crp <- map_singleform_values(input$cropCommonNameMono, input_other = input$cropCommonNameMono_other, type= "combo box", 
                                    format = "vector",label = "Crop")
        
        
       if(class(fbdesign())=="try-error"){
         shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Select factors and levels properly"), styleclass = "danger")
         flag<-FALSE
       }else if(crp=="" && ct=="Monocrop"){
         #Monocrop
         shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Select the crop for your experiment before preview"), styleclass = "danger")
         flag<-FALSE
       }

       if(flag){

       fb  <- fbdesign_traits()# fb_agrofims_traits()
       print(fb)
       if(is.element("PLOT",names(fb))){fb$PLOT <- as.factor(fb$PLOT)}
       if(is.element("SUBPLOT",names(fb))){fb$SUBPLOT <- as.factor(fb$SUBPLOT)}
       if(is.element("SUB-SUB-PLOT",names(fb))){fb$`SUB-SUB-PLOT` <- as.factor(fb$`SUB-SUB-PLOT`)}
       
       output$fbDesign_table_agrofims <- rhandsontable::renderRHandsontable({
         rhandsontable::rhandsontable(fb , readOnly = T)})
       }

       incProgress(9/10,message = "...")
       incProgress(10/10,message = "...")

     })

   })

 
  ############# donwload fieldbook ##################################################
  output$downloadData <- downloadHandler(
     filename = "fileNameBook.xlsx",
     content = function(file) {

       withProgress(message = 'Downloading fieldbook', value = 0, {
        
          # ai <- AllInputs()
          # saveRDS(ai, "/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/table_ids.rds")
          # x <- reactiveValuesToList(input)
          # saveRDS(x, "/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/inputs.rds")
          # 
          design <- tolower(input$designFieldbook_agrofims) #lowercase
          #Get IDS from design inputs
          IdDesignInputs <- getFactorIds(design)
          #Get index from Design's IDs
          #index22 <- get_index_design(IdDesignInputs, design)
          #trait2<<-traits_dt()
          #id_rand_inter <<- getAddInputId(intercropVars$ids, "int_", "")
          
          #p1<<- dt_plantrans()
          #p2<<- lbl_plantrans()
         
        if(class(fbdesign())=="try-error"){ 
           shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: There is a missing factors or level"), styleclass = "danger")
           fname <- paste(file,"xlsx",sep=".")
           wb <- createWorkbook()
           openxlsx::addWorksheet(wb, "NoData", gridLines = TRUE)
           openxlsx::writeDataTable(wb, "NoData", x = data.frame(Message= "Error selecting factor and labels"),
                                    colNames = TRUE, withFilter = FALSE)
           saveWorkbook(wb, file = fname , overwrite = TRUE)
           
         } 
        else {
         
          ############
          gmetadata <- globalMetadata()
          #gmetadata <- rbind(globalMetadata(), dtot)
          
          
         fname <- paste(file,"xlsx",sep=".")
         fb  <- fbdesign_traits()
         
         print("inicio")
         wb <- createWorkbook()
         print("inicio2")
         incProgress(2/20,message = "Downloading data...")
         print("inicio3")
         incProgress(6/20,message = "Metadata metadata sheet...")
         openxlsx::addWorksheet(wb, "Metadata", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Metadata", x = gmetadata,
                                 colNames = TRUE, withFilter = FALSE)
         print("inicio 3 -1 ")
         ###### Protocol data  ########
         protocol <- dt_prot_residual()
         openxlsx::addWorksheet(wb, "Protocol", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Protocol", x = protocol,colNames = TRUE, withFilter = FALSE)
         #############
         
         ##### Notes_deviations ########################################################################3
         openxlsx::addWorksheet(wb, "Notes_Deviations", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Notes_Deviations", x = fbdesign(),colNames = TRUE, withFilter = FALSE)
         #######
         
         
         
         
         print("inicio4")
       
         #Cropping type
         ct <- map_singleform_values(input$croppingType,  type = "combo box", format = "vector",default = "Monocrop") 
         
         print("PRINT TYPE OF CROPPING")
         print(ct)
         
         #FIELDBOOK design sheet  ------------------------------------------
         if(ct=="Monocrop"){
         incProgress(7/20,message = "Adding Crop measurements data...")
         openxlsx::addWorksheet(wb, "Crop_measurements", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Crop_measurements", x = fb,
                                  colNames = TRUE, withFilter = FALSE)
         
         } else {
         if(ct=="Intercrop"){
             id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "") 
             circm <- map_values(input = input, id_chr="int_cropCommonName_",id_ic_rand, format = "vector", lbl= "Select crop")
             for(i in 1:length(id_ic_rand)){
               incProgress(7/20,message = "Adding Crop measurements data...")
               openxlsx::addWorksheet(wb, paste0("Crop_measurements-",circm[i]), gridLines = TRUE)
               openxlsx::writeDataTable(wb, paste0("Crop_measurements-",circm[i]), 
                                        x = fbdesign_mult_traits()[[ circm[i] ]],
                                        colNames = TRUE, withFilter = FALSE)
               
             }
           }
         if(ct=="Relay crop"){
             
             id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "") 
             crecm <- map_values(input = input, id_chr="rel_cropCommonName_", id_re_rand, format = "vector", lbl= "Select crop")
             for(i in 1:length(id_re_rand)){
               incProgress(7/20,message = "Adding Crop measurements data...")
               openxlsx::addWorksheet(wb, paste0("Crop_measurements-",crecm[i]), gridLines = TRUE)
               openxlsx::writeDataTable(wb, paste0("Crop_measurements-",crecm[i]), 
                                        x = fbdesign_mult_traits()[[ i ]],
                                        colNames = TRUE, withFilter = FALSE)
               
               
               
             }
           }
           
         }
         
         #Experimental conditions
         if(is.element("Residue management",input$selectAgroFeature)){
         print("Adding residue management")
           
           #print(dt_residual())
           
           if(nrow(dt_residual())!=0){
             incProgress(7/20,message = "Adding residue management")
             openxlsx::addWorksheet(wb, "Residue management", gridLines = TRUE)
             openxlsx::writeDataTable(wb, "Residue management", x = dt_residual(),
                                      colNames = TRUE, withFilter = FALSE)
           }   
         }
         
         if(is.element(el = "Seedbed preparation", set = input$selectAgroFeature)){
         
           if(nrow(dt_seedbed())!=0){
             print("Adding seedbed sheet")
             incProgress(7/20,message = "Adding Seedbed preparation sheet")
             openxlsx::addWorksheet(wb, "Seedbed preparation", gridLines = TRUE)
             openxlsx::writeDataTable(wb, "Seedbed preparation", x = dt_seedbed(),
                                      colNames = TRUE, withFilter = FALSE)
           }  
          
         }
         
         # if(is.element("Soil fertility",input$selectAgroFeature)){
         #   
         #   print("soil fertility")
         #   incProgress(7/20,message = "Adding soil and fertility")
         #   openxlsx::addWorksheet(wb, "Soil fertility", gridLines = TRUE)
         #   openxlsx::writeDataTable(wb, "Soil fertility", x = dt_soilFertility(),
         #                            colNames = TRUE, withFilter = FALSE)
         #   
         # }
         # 
         
         resu<<- dt_plantrans()
         if(is.element("Planting and transplanting",input$selectAgroFeature)){
           print("Adding planting")
           if(ct=="Monocrop"){
             if(nrow(dt_plantrans())!=0){
             incProgress(7/20,message = "Adding planting and transplating")
             openxlsx::addWorksheet(wb, "Planting_transplating", gridLines = TRUE)
             openxlsx::writeDataTable(wb, "Planting_transplating", x = dt_plantrans(),
                                      colNames = TRUE, withFilter = FALSE)
             }
           }
           else if(ct=="Intercrop") {
             #TODO: #-Show error when one crop is missing
             id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
             circm <- map_values(input = input, id_chr="int_cropCommonName_",id_ic_rand, format = "vector", lbl= "Select crop")

                 for(i in 1:length(id_ic_rand)){
                   incProgress(7/20,message = "Adding planting and transplating" )##paste("Adding", circm[i] , "harvest sheet",sep=""))

                   #
                   if(nrow(dt_plantrans()[[i]])!=0  && ncol(fbdesign())!= ncol(dt_plantrans()[[i]])){
                   dt_pltr <- dt_plantrans()[[i]]
                     

                   #TODO: Avoid LONG names in sheetNames (error) max 32 characters
                   openxlsx::addWorksheet(wb,  paste("Planting-",circm[i]), gridLines = TRUE)
                   openxlsx::writeDataTable(wb, paste("Planting-",circm[i]), x = dt_pltr,
                                            colNames = TRUE, withFilter = FALSE)
                   }

                 }
          }
           else if(ct=="Relay crop"){
             #TODO: #-Show error when one crop is missing
             id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
             crecm <- map_values(input = input, id_chr="rel_cropCommonName_",id_re_rand, format = "vector", lbl= "Select crop")

             for(i in 1:length(id_re_rand)){
               incProgress(7/20,message = "Adding planting and transplating")

                 if(nrow(dt_plantrans()[[i]])!=0  && ncol(fbdesign())!= ncol(dt_plantrans()[[i]])){
                 dt_pltr <- dt_plantrans()

                 #TODO Avoid LONG names in sheetNames (error) max 32 characters
                 openxlsx::addWorksheet(wb,  paste("Planting-",crecm[i]), gridLines = TRUE)
                 openxlsx::writeDataTable(wb, paste("Planting-",crecm[i]), x = dt_pltr[[i]],
                                          colNames = TRUE, withFilter = FALSE)
               }

             }
           }
        }

         if(is.element("Mulch management",input$selectAgroFeature)){
         print("Adding Mulching") 
         incProgress(7/20,message = "Adding mulching sheet")
         openxlsx::addWorksheet(wb, "Mulch_management", gridLines = TRUE)
       
         openxlsx::writeDataTable(wb, "Mulch_management", x = dt_mulching(),
                                  colNames = TRUE, withFilter = FALSE)
        
         }
         
         if(is.element("Irrigation",input$selectAgroFeature)){
         incProgress(7/20,message = "Adding irrigation sheet")
         openxlsx::addWorksheet(wb, "Irrigation", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Irrigation", x = dt_irrigation(),
                                  colNames = TRUE, withFilter = FALSE)

         }
         # 
         if(is.element("Weeding",input$selectAgroFeature)){
         print("weeding")
         incProgress(7/20,message = "Adding weeding sheet")
         openxlsx::addWorksheet(wb, "Weeding", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Weeding", x = dt_weeding(),
                                  colNames = TRUE, withFilter = FALSE)
         }
         
         # if(is.element("Harvest",input$selectAgroFeature)){
         # print("harvest")
         # if(ct=="Monocrop" || ct=="Relay crop"){
         #   incProgress(7/20,message = "Adding harvest sheet")
         #   openxlsx::addWorksheet(wb, "Harvest", gridLines = TRUE)
         #   openxlsx::writeDataTable(wb, "Harvest", x = dt_harvest(),
         #                            colNames = TRUE, withFilter = FALSE)
         # }else{
         #  
         # 
         #   if(ct=="Intercrop"){
         #     id_rand <- getAddInputId(intercropVars$ids, "int_", "")
         #     print(id_rand)
         #     circm <- map_values(input = input, id_chr="int_cropCommonName_",id_rand, format = "vector", lbl= "Select crop")
         #   }
         #   # 
         #   # if(ct=="Relay crop"){
         #   #   id_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
         #   #   circm <- map_values(input = input, id_chr="rel_cropCommonName_",id_rand, format = "vector", lbl= "Select crop")
         #   # }
         #   
         #   #hrv<<- dt_harvest()
         #              
         #   for(i in 1:length(circm)){
         #     incProgress(7/20,message = "Adding harvest" )##paste("Adding", circm[i] , "harvest sheet",sep=""))
         #     dt_harv <- dt_harvest()
         #     print("paso")
         #     openxlsx::addWorksheet(wb,  paste0("Harvest-",circm[i]), gridLines = TRUE)
         #     openxlsx::writeDataTable(wb, paste0("Harvest-",circm[i]), x = dt_harv[[circm[i]]],
         #                              colNames = TRUE, withFilter = FALSE)
         #   }
         #   
         # }   
         # }
         
         print("inicio6")
         
         incProgress(9/20,message = "Adding crop measurement sheet...")
         
         ### HIDE----------------------------------------------------------------------
         
         #PHENOLOGY SHEET ------------------------------------------------------------
         if(ct=="Monocrop"){
         print("inicio8 1")
         if(nrow(pheno_dt())!=0){   
           openxlsx::addWorksheet(wb, "Phenology", gridLines = TRUE)
           openxlsx::writeDataTable(wb, "Phenology", x = pheno_dt(),
                                    colNames = TRUE, withFilter = FALSE)
          }
         } else {
           #FOR INTERCROP PHENOLOGY
           print("inicio8 2")
           
           
           if(ct=="Intercrop"){
             id_ir_rand <- getAddInputId(intercropVars$ids, "int_", "") 
             circm <- map_values(input = input, id_chr="int_cropCommonName_",id_ir_rand, format = "vector", lbl= "Select crop")
          
             print("ENTRO A INTERROP")
             # print(ct)
             # ivan<<- pheno_dt()
             # 
             
             for(i in 1:length(id_ir_rand)){
               
               print(i)
              
               
               if(nrow(pheno_dt()[[ circm[i] ]])!=0 &&  !is.element("Measurement_3", names(pheno_dt()[[ circm[i] ]]) )){
                 incProgress(7/20,message = "Adding Phenology data...")
                 openxlsx::addWorksheet(wb, paste0("Phenology-",circm[i]), gridLines = TRUE)
                 openxlsx::writeDataTable(wb, paste0("Phenology-",circm[i]), 
                                          x = pheno_dt()[[ i ]],
                                          colNames = TRUE, withFilter = FALSE)
               }
               
             }
             
             }
           
           if(ct=="Relay crop"){
             
             print("ENTRO A RELAY CROPl")
             
             id_rc_rand <- getAddInputId(relaycropVars$ids, "rel_", "") 
             print(id_rc_rand)
             crccm <- map_values(input = input, id_chr="rel_cropCommonName_",id_rc_rand, format = "vector", lbl= "Select crop")
             print(crccm)
             for(i in 1:length(id_rc_rand)){
               
               
               #print(pheno_dt()[[ circm[i] ]])
               
               if(nrow(pheno_dt()[[ crccm[i] ]])!=0 &&  !is.element("Measurement_3", names(pheno_dt()[[crccm[i] ]]) )){
                 incProgress(7/20,message = "Adding Phenology data...")
                 openxlsx::addWorksheet(wb, paste0("Phenology-",crccm[i]), gridLines = TRUE)
                 openxlsx::writeDataTable(wb, paste0("Phenology-",crccm[i]), 
                                          x = pheno_dt()[[ i ]],
                                          colNames = TRUE, withFilter = FALSE)
               }
               
             }
             
             }
           
           # id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "") 
           # circm <- map_values(input = input, id_chr="int_cropCommonName_",id_ic_rand, format = "vector", lbl= "Select crop")
           # 
          
         }
         print("inicio9")
         
         # WEATHER SHEET ------------------------------------------------------------ 
         if(nrow(weather_dt())!=0){
         openxlsx::addWorksheet(wb, "Weather", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Weather", x = weather_dt(),
                                  colNames = TRUE, withFilter = FALSE)
         }
         # SOIL SHEET ------------------------------------------------------------ 
         print("inicio10")
         if(nrow(soil_dt())!=0){
           openxlsx::addWorksheet(wb, "Soil", gridLines = TRUE)
           openxlsx::writeDataTable(wb, "Soil", x = soil_dt(),
                                    colNames = TRUE, withFilter = FALSE)
         }
         # 
         #dtl: data for trai list sheet/table
         print("inicio11")
         
         # CROP MEASUREMENT TRAIT LIST ---------------------------------------
         #print(traits_dt())
         if(ct=="Monocrop"){
           row_sel<- input$tblMono_rows_selected
            if(length(row_sel)>0){
              #print(dtMonocrop[row_sel,])
              row_sel<- sort(row_sel)
              cm_tl <- dtMonocrop[row_sel,]
              
              
              colnames(cm_tl) <- c("Crop","Group","Subgroup","Measurement",
                               "TraitUnit","CropMeasurementPerSeason",
                               "CropMeasurementPerPlot","TraitAlias",
                               "TraitDataType","TraitValidation","VariableId")
              
              cr<- cm_tl$Crop
              sb<- cm_tl$Subgroup
              cm <- cm_tl$Measurement
              sc <- cm_tl$TraitUnit
              sc[is.na(sc)] <- "unitless"
              cs <- paste(cr,sb, cm, sc, sep="_")
              cs<- stringr::str_replace_all(cs,"[[:space:]]","_")
              cm_tl$TraitName <- cs
              cm_tl <- cm_tl
            } else{
              cm_tl<- data.frame()
            }
         } 
         else { #intecrop
        
           cm_tl <- rbindlist(traits_dt(),fill = TRUE)
           cm_tl <- ec_clean_header(cm_tl)
         }
     
         # 
         # SOIL MEASUREMENT FOR TRAIT LIST  -----------------------------------------------
         if(nrow(soil_dt())!=0){
           row_select <- input$tblSoil_rows_selected
           row_select<- sort(row_select)
           dt <- dtSoil[row_select, ]
           soil_tl <- data.table(dt)
           colnames(soil_tl) <- c("Crop","Group","Subgroup","Measurement",
                                "TraitUnit",
                                "CropMeasurementPerSeason","CropMeasurementPerPlot",
                                "TraitName", "TraitAlias",
                                "TraitDataType","TraitValidation","VariableId")
           #soil_tl <- data.table(soil_dt())
           soil_tl$Group <- "Soil"
           #soil_tl$CropMeasurementPerSeason <-	soil_tl$CropMeasurementPerPlot <- 1
         } else{
           soil_tl <- data.frame()
         }

         # WEATHER TRAIT LIST ---------------------------------------------------------
         if(nrow(weather_dt())!=0){
           print("entro wheater")
           #w_tl<- dtWeather
           row_select <- input$tblWeather_rows_selected
           row_select<- sort(row_select)
           w_tl<-  dtWeather[row_select, ]
           
           colnames(w_tl) <- c("Crop","Group","Subgroup","Measurement",
                                  "TraitUnit","CropMeasurementPerSeason",
                                  "CropMeasurementPerPlot","TraitName", "TraitAlias",
                                  "TraitDataType","TraitValidation","VariableId")
           #w_tl<- data.table(weather_dt())
           w_tl$Group <- "Weather"
         } else{
           w_tl<- data.frame()
         }
         
         # 
         # PHENOLOGY TRAIT LIST ---------------------------------------------------------
         print("pheno trait list")
         
         if(ct=="Monocrop"){
         
             if(nrow(pheno_dt())!=0){
               row_select <- input$tblMonoPhe_rows_selected
               row_select<- sort(row_select)
               dt <- dtMonocropphe[row_select, ]
               dt <- ec_clean_header(dt)
               ph_tl <- dt
               #ph_tl<- data.table(pheno_dt())
             } else  {
               ph_tl <- data.frame()
             }
           
         } 
         else {
            
         
               ph_tl <- rbindlist(pheno_inter_vars(),fill = TRUE)
               ph_tl <- ec_clean_header(ph_tl)
         }  
           
         # Experiment conditon Trait List ----- 
         
         ##Consolidation of crop measurement, soil, weather amd phenology data
         print("inicio14")
         l_lt <- list(cm_tl, soil_tl, w_tl, ph_tl)
         dt_kds<- rbindlist(l_lt, fill = TRUE)
        
         print("inicio 14.1")
         print(head(dt_kds))
         
         # #Remove foo columns
         # print("inicio15")
         print("inicio15")
         dt_kds<- ec_clean_header(dt_kds)
         # 
         ecname <- "AgroFIMS_Agronomy_DataDictionary_05-3-2019.xlsx"
         
         if(is.element("Residue management",input$selectAgroFeature)){
           globalpath <- "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/www/internal_files/"
          
           kds_resmgt <- readxl::read_excel(paste0(globalpath, ecname),sheet = "Residue management")
           kds_resmgt <- ec_filter_data(kds_resmgt) 
           kds_resmgt <-  kds_resmgt %>% dplyr::filter(TraitName %in% lbl_residual())
           #kds_resmgt <- ec_filter_data(kds_resmgt) 
           kds_resmgt <- data.table(kds_resmgt)
           dt_kds<-rbindlist(list(dt_kds,kds_resmgt),fill = TRUE)
           dt_kds<- ec_clean_header(dt_kds)
         }
         if(is.element("Seedbed preparation",input$selectAgroFeature)){
           globalpath <- "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/www/internal_files/"
           kds_sedbed <- readxl::read_excel(paste0(globalpath,ecname),sheet = "Seedbed preparation")
           kds_sedbed <- ec_filter_data(kds_sedbed)
           kds_sedbed <- kds_sedbed %>% dplyr::filter(TraitName %in% lbl_seedbed())
           
           kds_sedbed <- data.table(kds_sedbed)
           dt_kds<-rbindlist(list(dt_kds,kds_sedbed),fill = TRUE)
           dt_kds <- ec_clean_header(dt_kds)
         }
         print("seedbed pt")
         # if(is.element("Soil fertility",input$selectAgroFeature)){
         #   globalpath <- "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/www/internal_files/"
         #   kds_soilf<- readxl::read_excel(paste0(globalpath, ecname),sheet = "Soil fertility")
         #   kds_soilf <- ec_filter_data(kds_soilf)
         #   kds_soilf <- kds_soilf %>% dplyr::filter(TraitName %in% lbl_soilFertility())
         #   
         #   kds_soilf <- data.table(kds_soilf)
         #   dt_kds<-rbindlist(list(dt_kds,kds_soilf),fill = TRUE)
         #   dt_kds<-ec_clean_header(dt_kds)
         # }
         # if(is.element("Planting and transplanting",input$selectAgroFeature)){
         #   globalpath <- "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/www/internal_files/"
         #   kds_platra <- readxl::read_excel(paste0(globalpath, ecname),sheet = "Planting, Transplanting")
         #   kds_platra <- ec_filter_data(kds_platra)
         #  
         #   #TODO :generalizar para intercrop
         #   if(ct=="Monocrop"){
         #   kds_platra <- kds_platra %>% dplyr::filter(TraitName %in% lbl_plantrans())
         #   } 
         #   else if(ct=="Intercrop") {
         #     
         #     temp_platra <- list()
         #     for(i in 1:length(lbl_plantrans()) ) {
         #      
         #       if( length(lbl_plantrans())!=0) {
         #           temp_platra[[i]] <- kds_platra %>% dplyr::filter(TraitName %in% lbl_plantrans()[[i]])
         #           temp_platra[[i]]$Crop <- circm[i]
         #       }
         #     }
         #     kds_platra <- rbindlist(temp_platra,fill = TRUE)
         #   }
         #   else if(ct=="Relay crop"){
         #     temp_platra <- list()
         #     for(i in 1:length(lbl_plantrans()) ) {
         #       
         #       if( length(lbl_plantrans())!=0) {
         #         temp_platra[[i]] <- kds_platra %>% dplyr::filter(TraitName %in% lbl_plantrans()[[i]])
         #         temp_platra[[i]]$Crop <- crecm[i]
         #       }
         #     }
         #     kds_platra <- rbindlist(temp_platra,fill = TRUE)
         #   }
         #   
         #   kds_platra <- data.table(kds_platra)
         #   dt_kds<-rbindlist(list(dt_kds,kds_platra),fill = TRUE)
         #   dt_kds<- ec_clean_header(dt_kds)
         # }
         # print("paso pt")
         if(is.element("Mulch management",input$selectAgroFeature)){
           globalpath <- "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/www/internal_files/"
           kds_mulch <- readxl::read_excel(paste0(globalpath,ecname),sheet = "Mulch management")
           kds_mulch <- ec_filter_data(kds_mulch)
           kds_mulch <- kds_mulch %>% dplyr::filter(TraitName %in% lbl_mulching())
           
           kds_mulch <- data.table(kds_mulch)
           dt_kds<-rbindlist(list(dt_kds,kds_mulch),fill = TRUE)
           dt_kds<- ec_clean_header(dt_kds)
         }
         print("paso mul")
         if(is.element("Irrigation",input$selectAgroFeature)){
           globalpath <- "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/www/internal_files/"
           kds_irri <- readxl::read_excel(paste0(globalpath,ecname),sheet = "Irrigation")
           kds_irri <- ec_filter_data(kds_irri)
           kds_irri <-  kds_irri %>% dplyr::filter(TraitName %in% lbl_irrigation())
           
           kds_irri$CropMeasurementPerSeason <- ns_irrigation()
           kds_irri <- data.table(kds_irri)
           dt_kds<-rbindlist(list(dt_kds,kds_irri),fill = TRUE)
           dt_kds<- ec_clean_header(dt_kds)
         }
         print("paso irri")
         if(is.element("Weeding",input$selectAgroFeature)){
           globalpath <- "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/www/internal_files/"
           kds_weed <- readxl::read_excel(paste0(globalpath,ecname),sheet = "Weeding")
           kds_weed <- ec_filter_data(kds_weed)
           kds_weed <-  kds_weed %>% dplyr::filter(TraitName %in% lbl_weeding())
           
           kds_weed$CropMeasurementPerSeason <- ns_weeding()
           kds_weed <- data.table(kds_weed)
           dt_kds<-rbindlist(list(dt_kds,kds_weed),fill = TRUE)
           dt_kds<- ec_clean_header(dt_kds)
         }
         print("paso wwed")
         # if(is.element("Harvest",input$selectAgroFeature)){
         #   globalpath <- "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/www/internal_files/"
         #   kds_harv <- readxl::read_excel(paste0(globalpath,ecname),sheet = "Harvest")
         #   kds_harv <- ec_filter_data(kds_harv) 
         #   #TODO :generalizar para intercrop
         #   if(ct=="Monocrop" || ct== "Relay crop"){
         #     kds_harv <-  kds_harv %>% dplyr::filter(TraitName %in% lbl_harvest())
         #   }else{
         #     temp_harv <- list()
         #     for(i in 1:length(lbl_harvest())){
         #       temp_harv[[i]] <- kds_harv %>% dplyr::filter(TraitName %in% lbl_harvest()[[i]])
         #       temp_harv[[i]]$Crop <- circm[i]
         #       #circm[i]
         #     }
         #     kds_harv <- rbindlist(temp_harv,fill = TRUE)
         #   }
         #   
         #   kds_harv <- data.table(kds_harv)
         #   kds_harv$CropMeasurementPerSeason <- ns_harvest()  
         #   dt_kds<-rbindlist(list(dt_kds,kds_harv),fill = TRUE)
         #   dt_kds<- ec_clean_header(dt_kds)
         # }
         # 
         # ADDING  EXTRA VARIABLES------------------------------------------------------------
         dt_extra_vars <- ec_clean_header(extra_variables)
         dt_kds<-rbindlist(list(dt_kds,dt_extra_vars),fill = TRUE)
         # END ADDING EXTRA VARIABLES --------------------------------------------------------
         
         lbl_traitlist_dt <- c("Crop","Group","Subgroup","Measurement","TraitName",
                             "TraitUnit",
                             "CropMeasurementPerSeason","CropMeasurementPerPlot",
                             "TraitAlias",
                             "TraitDataType","TraitValidation","VariableId")
         
         
         dt_kds <- dt_kds[,lbl_traitlist_dt]
         #dt_kds<- changes_units(ec=dt_kds, input, allinputs= AllInputs())
         
         #TODO: Update current internal data
         #Changes names in per season and per plot
         names(dt_kds)<- c("Crop","Group","Subgroup","Measurement","TraitName",
                          "TraitUnit",
                          "NumberOfMeasurementsPerSeason","NumberOfMeasurementsPerPlot",
                          "TraitAlias",
                          "TraitDataType","TraitValidation","VariableId")
         
         dt_kds <- dt_kds %>% dplyr::filter(TraitName!="")
         
         print("inicio17")
         #dt_kds<- ec_clean_header(dt_kds)
         openxlsx::addWorksheet(wb, "TraitList", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "TraitList", x = dt_kds,
                                  colNames = TRUE, withFilter = FALSE)

        
         ### END HIDE ----------------------------------------------------------
         print("inicio18")  
         incProgress(19/20,message = "Downloading file...")
         saveWorkbook(wb, file = fname , overwrite = TRUE)
         file.rename(fname, file)
        }

       })

     },
     contentType="application/xlsx"
   )
  
  ##Invalidate donwloadData
  observe({
    #After all this conditions has been made, 0the submit button will appear to save the information
    toggleState("downloadData", !is.null(input$croppingType)
    )
  })


}