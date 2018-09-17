#'
#' Design field book for HIDAP-AGROFIMS
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
## @param dom target dom element name
#' @param values reactive values
#' @author Omar Benites / Ivan Perez
#' @export
#'

server_design_agrofims <- function(input, output, session, values){

  #################### START: PATHS GENERALES ####################
  # path global para lectura de RDS's
  globalpath <- "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/www/internal_files/"
  
  # path del template para new fieldbook
  templatepath <- "/home/obenites/AGROFIMS/template/"
  #################### END: PATHS GENERALES ####################
  
  #################### START: GUARDAR SESION DEL FIELDBOOK ####################
  # path global donde se aloja las sessiones y backups
  sessionpath <- "/home/obenites/AGROFIMS/savesession/"
  sessionpathbk <- "/home/obenites/AGROFIMS/savesession_bk/"

  # Funcion que crea lista de inputs a guardar: Experiment
  inputsExperiment <- function() {
    a1 <- a2 <- a3 <- c()
    b1 <- b2 <- b3 <- b4 <- b5 <- b6 <- b7 <- b8 <- b9 <- b10 <- b11 <- b12 <- c()
    c1 <- c2 <- c3 <- c4 <- c5 <- c6 <- c7 <- c8 <- c9 <- c10 <- c11 <- c12 <- c13 <- c14 <- c15 <- c16 <- c17 <- c18 <- c()

    inputRds <- readRDS(paste0(globalpath, "inputId1_v1.rds"))
    inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Experiment")
    df1 <- inputRds[c(4, 5, 6)]

    # inputs para: Funding agency type
    if (!is.null(input$designFieldbook_fundAgencyType) && !is.na(input$designFieldbook_fundAgencyType) && length(input$designFieldbook_fundAgencyType) >= 1) {
      for (i in 1:length(input$designFieldbook_fundAgencyType)) {
        a1[i] <- paste0("fundName_", i)
        a2[i] <- "textInput"
        a3[i] <- "n"
      }
      df2 <- data.frame(inputId = a1, type = a2, create = a3, stringsAsFactors = F)
    } else {
      df2 <- NULL
    }

    # inputs para: Number of project management entities
    if (!is.null(input$numProjEntity) && !is.na(input$numProjEntity) && input$numProjEntity >= 1) {
      for (i in 1:input$numProjEntity) {
        b1[i] <- paste0("projEntity_", i)
        b2[i] <- paste0("projEntity_", i, "_other")
        b3[i] <- paste0("contCenter_", i)
        b4[i] <- paste0("contCRP_", i)

        b5[i] <- "selectizeInput"
        b6[i] <- "textInput"
        b7[i] <- "selectizeInput"
        b8[i] <- "selectizeInput"

        b9[i] <- "n"
        b10[i] <- "n"
        b11[i] <- "n"
        b12[i] <- "n"
      }
      df3 <- data.frame(inputId = c(b1, b2, b3, b4), type = c(b5, b6, b7, b8), create = c(b9, b10, b11, b12), stringsAsFactors = F)
    } else {
      df3 <- NULL
    }

    # inputs para: Number of experiment leads
    if (!is.null(input$numLeads) && !is.na(input$numLeads) && input$numLeads >= 1) {
      for (i in 1:input$numLeads) {
        c1[i] <- paste0("projLeadEnt_", i)
        c2[i] <- paste0("tLeadCenter_", i)
        c3[i] <- paste0("lead_org_type_1_", i)
        c4[i] <- paste0("lead_org_type_1_", i, "_other")
        c5[i] <- paste0("leadNameOther_", i)
        c6[i] <- paste0("expLead_", i)

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
      df4 <- data.frame(inputId = c(c1, c2, c3, c4, c5, c6), type = c(c7, c8, c9, c10, c11, c12), create = c(c13, c14, c15, c16, c17, c18), stringsAsFactors = F)
    } else {
      df4 <- NULL
    }

    res <- rbind(df1, df2, df3, df4)
    res
  }

  # Funcion que crea lista de inputs a guardar: Personnel
  inputsPersonnel <- function() {
    a1 <- a2 <- a3 <- a4 <- a5 <- a6 <- a7 <- a8 <- a9 <- a10 <- a11 <- a12 <- a13 <- a14 <- a15 <- a16 <- a17 <- a18 <- a19 <- a20 <- a21 <- a22 <- a23 <- a24 <- a25 <- a26 <- a27 <- c()

    inputRds <- readRDS(paste0(globalpath, "inputId1_v1.rds"))
    inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Personnel")
    df1 <- inputRds[c(4, 5, 6)]

    # inputs para: Number of personnel
    if (!is.null(input$npersons) && !is.na(input$npersons) && input$npersons >= 1) {
      for (i in 1:input$npersons) {
        a1[i] <- paste0("personnel", i, "Type")
        a2[i] <- paste0("personnel", i, "Type_other")
        a3[i] <- paste0("person", i, "FirstName")
        a4[i] <- paste0("person", i, "LastName")
        a5[i] <- paste0("person", i, "Email")
        a6[i] <- paste0("person", i, "Affiliation")
        a7[i] <- paste0("person", i, "Center")
        a8[i] <- paste0("person", i, "Affiliation_other")
        a9[i] <- paste0("person", i, "ORCID")

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
      df2 <- data.frame(inputId = c(a1, a2, a3, a4, a5, a6, a7, a8, a9),
                        type = c(a10, a11, a12, a13, a14, a15, a16, a17, a18),
                        create = c(a19, a20, a21, a22, a23, a24, a25, a26, a27),
                        stringsAsFactors = F)
    } else {
      df2 <- NULL
    }

    res <- rbind(df1, df2)
    res
  }

  # Funcion que crea lista de inputs a guardar: Site
  inputsSite <- function() {
    inputRds <- readRDS(paste0(globalpath, "inputId1_v1.rds"))
    inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Site")
    df1 <- inputRds[c(4, 5, 6)]

    res <- df1
    res
  }

  # Funcion que crea lista de inputs a guardar: Crop
  inputsCrop <- function() {
    df2 <- df3 <- data.frame()
    a1 <- a2 <- a3 <- a4 <- a5 <- a6 <- a7 <- a8 <- a9 <- c()

    inputRds <- readRDS(paste0(globalpath, "inputId1_v1.rds"))
    inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Crop")
    df1 <- dplyr::filter(inputRds, is.na(category))
    df1 <- df1[c(4, 5, 6)]

    # inputs para: Monocrop
    if (!is.null(input$croppingType) && !is.na(input$croppingType) && input$croppingType == "Monocrop") {
      df2 <- dplyr::filter(inputRds, category == "Monocrop")
      df2 <- df2[c(4, 5, 6)]
    }

    # inputs para: Intercrop
    if (!is.null(input$croppingType) && !is.na(input$croppingType) && input$croppingType == "Intercrop") {
      # inputs para: Select crops
      if (!is.null(input$cropsSelected) && length(input$cropsSelected) >= 1) {
        for (i in 1:length(input$cropsSelected)) {
          a1[i] <- paste0("cropCommonName", i)
          a2[i] <- paste0("cropVarietyName", i)
          a3[i] <- paste0("intercropValue_row_crop_", i)

          a4[i] <- "textInput"
          a5[i] <- "selectizeInput"
          a6[i] <- "textInput"

          a7[i] <- "n"
          a8[i] <- "y"
          a9[i] <- "n"
        }
        df3 <- data.frame(inputId = c(a1, a2, a3), type = c(a4, a5, a6), create = c(a7, a8, a9), stringsAsFactors = F)
      } else {
        df3 <- NULL
      }
    }

    res <- rbind(df1, df2, df3)
    res
  }
  
  # Funcion que crea lista de inputs a guardar: Design
  inputsDesign <- function() {
    df2 <- df3 <- df4 <- data.frame()
    
    inputRds <- readRDS(paste0(globalpath, "inputId1_v1.rds"))
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
        a1[i] <- paste0("irrigationevent_start_date_", i)
        a2[i] <- paste0("irrigationevent_end_date_", i)
        a3[i] <- paste0("irrigation_technique_", i)
        a4[i] <- paste0("irrigation_technique_", i, "_other")
        a5[i] <- paste0("irrigation_using_sprinkler_systems_", i)
        a6[i] <- paste0("irrigation_using_sprinkler_systems_", i, "_other")
        a7[i] <- paste0("localized_irrigation_technique", i)
        a8[i] <- paste0("localized_irrigation_technique", i, "_other")
        a9[i] <- paste0("surface_irrigation_technique_", i)
        a10[i] <- paste0("surface_irrigation_technique_", i, "_other")
        a11[i] <- paste0("irrigation_source_", i)
        a12[i] <- paste0("irrigation_source_distance_", i)
        a13[i] <- paste0("irrigation_source_distance_", i, "unit")
        a14[i] <- paste0("irrigation_amount_", i)
        a15[i] <- paste0("irrigation_amount_", i, "unit")
        a16[i] <- paste0("irrigation_notes_", i)
        
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
        a1[i] <- paste0("weeding_start_date_", i)
        a2[i] <- paste0("weeding_end_date_", i)
        a3[i] <- paste0("weeding_technique_", i)
        a4[i] <- paste0("weeding_type_", i)
        a5[i] <- paste0("weeding_type_", i, "_other")
        a6[i] <- paste0("weeding_traction_", i)
        a7[i] <- paste0("weeding_traction_", i, "_other")
        
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
  observeEvent(input$xtest, {
    print(inputsExpCon())
  })
  
  # Funcion que guarda la session del usuario
  savesession <- function() {
    if(session$userData$logged){
      expid <- input$experimentId
      
      if (file.exists(isolate(paste0(sessionpath, expid, ".csv")))) {
        x <- read.csv(paste0(sessionpath, expid, ".csv"))
        datecreate <- as.character(x[2, 4])
        datemodified <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
      } else {
        datecreate <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
        datemodified <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
      }
      
      inputs1 <- inputs2 <- inputs3 <- NULL
      inputs_to_save <- rbind(inputsExperiment(), 
                              inputsPersonnel(), 
                              inputsSite(), 
                              inputsCrop(), 
                              inputsDesign(), 
                              inputsExpCon())
      
      case1p <- dplyr::filter(inputs_to_save, type == "textInput" | 
                                type == "numericInput" | 
                                type == "textAreaInput" |
                                type == "checkboxInput")
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
        # textInput && numericInput && textAreaInput
        if (is.null(input[[paste0(case1[i])]]) || is.na(input[[paste0(case1[i])]])) {
          inputs1[i] <- ""
        } else {
          inputs1[i] <- input[[paste0(case1[i])]]
        }
      }
      inputs_data_frame1 <- data.frame(inputId = case1, type = case1_type, create = case1_create, value = inputs1)
      
      for (i in 1:length(case2)) {
        # dateRangeInput
        if (is.null(input[[paste0(case2[i])]]) || is.na( input[[paste0(case2[i])]])) {
          inputs2[i] <- ""
        } else {
          inputs2[i] <- paste(input[[paste0(case2[i])]], collapse = "&")
        }
      }
      inputs_data_frame2 <- data.frame(inputId = case2, type = case2_type, create = case2_create, value = inputs2)
      
      for (i in 1:length(case3)) {
        # selectizeInput && selectInput
        if (is.null(input[[paste0(case3[i])]]) || is.na( input[[paste0(case3[i])]])) {
          inputs3[i] <- ""
        } else {
          inputs3[i] <- paste(input[[paste0(case3[i])]], collapse = "&")
        }
      }
      inputs_data_frame3 <- data.frame(inputId = case3, type = case3_type, create = case3_create, value = inputs3)
      
      inputs_data_frame <- rbind(inputs_data_frame1, inputs_data_frame2, inputs_data_frame3)
      nr <- data.frame(inputId = "user", type = "", create = "", value = session$userData$userMail)
      nr2 <- data.frame(inputId = "datec", type = "", create = "", value = datecreate)
      nr3 <- data.frame(inputId = "datem", type = "", create = "", value = datemodified)
      final_inputs_df <- rbind(nr, nr2, nr3, inputs_data_frame)
      
      write.csv(final_inputs_df, paste0(sessionpath, input$experimentId, ".csv"), row.names = FALSE)
      write.csv(final_inputs_df, paste0(sessionpathbk, input$experimentId, ".csv"), row.names = FALSE)
      
      updateTextInput(session,
                      inputId = "experimentId",
                      value = "")
      updateTextInput(session,
                      inputId = "experimentId",
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
    expid <- input$experimentId
    
    if (file.exists(isolate(paste0(sessionpath, expid, ".csv")))) {
      x <- read.csv(paste0(sessionpath, expid, ".csv"))
      datemodified <- as.character(x[3, 4])
      datemodified <- paste0("<font color='#00a65a'>", datemodified, "</font>")
    } else {
      datemodified <- paste0("<font color='red'>never</font>")
    }

    datemodified
  })
  
  # Renderiza el mensaje de guardado de sesion
  output$lastsaved <- renderText({
    paste0("Last modified: ", timeExp())
  })
  
  # Funcion reactiva que muestra el id del fieldbook para generar qr
  # idExp <- reactive({
  #   expid <- input$experimentId
  #   expid
  # })
  #################### END: GUARDAR SESION DEL FIELDBOOK ####################
  
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
            if (inputId == "experimentId") {
              updateTextInput(session,
                              inputId = uploaded_inputs$inputId[i],
                              value = idgenerator())
            } else {
              updateTextInput(session,
                              inputId = uploaded_inputs$inputId[i],
                              value = uploaded_inputs$value[i])
            }
          }
          
          if (type == "dateRangeInput") {
            updateDateRangeInput(session, "fbDesign_project_time_line", 
                                 inputId = uploaded_inputs$inputId[i], 
                                 start = Sys.Date() - 2, 
                                 end = Sys.Date() + 20)
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

  # funcion que imprime ID
  idgenerator <- function() {
    id <- stri_rand_strings(1, 8,  '[A-Z0-9]')
    id
  }

  # input Experiment ID
  output$experimentIdUI <- renderUI({
    disabled(textInput(inputId = "experimentId", label = "Experiment ID",
                       value = idgenerator()))
  })
  
  

  





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

  observeEvent(input$land_levelling_titleId, {
    js$collapse("land_levelling_boxid")
  })

  observeEvent(input$puddling_titleId, {
    js$collapse("puddling_boxid")
  })

  observeEvent(input$tillage_titleId, {
    js$collapse("tillage_boxid")
  })

  observeEvent(input$liming_titleId, {
    js$collapse("liming_boxid")
  })

  observeEvent(input$mulch_management_titleId, {
    js$collapse("mulch_management_boxid")
  })

  observeEvent(input$residue_management_titleId, {
    js$collapse("residue_management_boxid")
  })

  observeEvent(input$direct_seeding_titleId, {
    js$collapse("direct_seeding_boxid")
  })

  observeEvent(input$transplanting_titleId, {
    js$collapse("transplanting_boxid")
  })

  observeEvent(input$desc_biofertilizer_titleId, {
    js$collapse("desc_biofertilizer_boxid")
  })

  observeEvent(input$irrigation_desc_titleId, {
    js$collapse("irrigation_desc_boxid")
  })

  observeEvent(input$desc_harvest_titleId, {
    js$collapse("desc_harvest_boxid")
  })

  observeEvent(input$pest_control_titleId, {
    js$collapse("pest_control_boxid")
  })

  observeEvent(input$weeding_titleId, {
    js$collapse("weeding_boxid")
  })

  observeEvent(input$fertilizer_application_titleId, {
    js$collapse("fertilizer_application_details_boxid")
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



  ################ Experiment ###########################################################

  observe({
    removeUI(
      selector = "#fl_agencies_assoc_exp_aux", immediate = T
    )
    if(!is.null(input$designFieldbook_fundAgencyType)){
      l <- input$designFieldbook_fundAgencyType
      insertUI(
        selector = "#fl_agencies_assoc_exp",
        where ="afterBegin",
        ui = column(id = "fl_agencies_assoc_exp_aux", width = 12)
      )
      count <- 1
      for (ag in l){
        insertUI(
          selector = "#fl_agencies_assoc_exp_aux",
          where = "beforeEnd",
          ui = textInput(paste0("fundName_", count), paste0(ag, "  name"))
        )
        count <- count+1
      }
    }
  })

  projectEntities <- reactiveValues()
  projectEntities$num <- 0
  projectEntities$numLeads <- 0

  observe({
    if(is.numeric(input$numProjEntity)){
      n <- input$numProjEntity
      if(projectEntities$num == 0){

        insertUI(
          selector = "#fl_entities_exp",
          where ="afterBegin",
          ui = column(id = "fl_entites_exp_aux", width = 12)
        )
      }

      if(projectEntities$num < n){
        start <-  projectEntities$num + 1
        count <- start
        for (num in start:n) {
          insertUI(
            selector = "#fl_entites_exp_aux",
            where = "beforeEnd",
            ui =fluidRow(id = paste0("fl_box_exp_ent_", count),
                         box(title = paste0("Project management entity #", count), solidHeader = TRUE, status = "warning", width=12,
                             fluidRow(
                               column(width = 4,
                                      selectizeInput(paste0("projEntity_", count), "Project management entity", multiple =T, options = list(maxItems =1, placeholder="Select one.."), choices=
                                                       c("CGIAR center",
                                                         "Other"
                                                       )
                                      )
                               ),

                               conditionalPanel(paste0("input.projEntity_", count, " == 'CGIAR center'"),
                                                column(width = 4,
                                                       selectizeInput(paste0("contCenter_", count), "Choose CGIAR center", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c(
                                                         "Africa Rice Center",
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
                                                column(width = 4,
                                                       selectizeInput(paste0("contCRP_", count), "Contributor CRP", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = sort(c(
                                                         "CGIAR Research Program on Fish",
                                                         "CGIAR Research Program on Forests, Trees and Agroforestry",
                                                         "CGIAR Research Program on Grain Legumes and Dryland Cereals",
                                                         "CGIAR Research Program on Wheat",
                                                         "CGIAR Research Program on Livestock",
                                                         "CGIAR Research Program on Maize",
                                                         "CGIAR Research Program on Rice",
                                                         "CGIAR Research Program on Roots, Tubers and Bananas",
                                                         "CGIAR Research Program on Agriculture for Nutrition and Health",
                                                         "CGIAR Research Program on Climate Change, Agriculture and Food Security",
                                                         "CGIAR Research Program on Policies, Institutions, and Markets",
                                                         "CGIAR Research Program on Water, Land and Ecosystems",
                                                         "None"))
                                                       )
                                                )

                               ),
                               column(width =4,style="padding-top: 5px;",
                                      hidden(textInput(paste0("projEntity_", count, "_other"), "", value = ""))
                               )
                             )





                         ) #end box
            )

          )
          count <- count + 1
        }
      }
      else if(projectEntities$num > n){
        start <- n+1
        end <- projectEntities$num
        count <- start
        for(num in start:end){
          removeUI(
            selector = paste0("#fl_box_exp_ent_", count),
            immediate = T
          )
          count <- count +1
        }

      }
      projectEntities$num <- n
    }
    else{
      removeUI(selector = "#fl_entites_exp_aux", immediate = T)
      projectEntities$num <- 0
    }
  })


  observe({
    if(is.numeric(input$numLeads)){
      n <- input$numLeads
      if(projectEntities$numLeads == 0){

        insertUI(
          selector = "#fl_exp_leads",
          where ="afterBegin",
          ui = column(id = "fl_exp_leads_aux", width = 12)
        )
      }

      if(projectEntities$numLeads < n){
        start <-  projectEntities$numLeads + 1
        count <- start
        for (num in start:n) {
          insertUI(
            selector = "#fl_exp_leads_aux",
            where = "beforeEnd",
            ui =fluidRow(id = paste0("fl_box_exp_lead_", count),
                         box(title = paste0("#", count, ". Experiment lead organization, if different from project management entity"), solidHeader = TRUE, status = "warning", width=12,
                             fluidRow(
                               column(width = 6,

                                      selectizeInput(paste0("projLeadEnt_", count), "Experiment, lead organization type", multiple =T, options = list(maxItems =1, placeholder="Select one..."), choices=
                                                       c("CGIAR center",
                                                         "Other"
                                                       )
                                      ),

                                      conditionalPanel(paste0("input.projLeadEnt_", count, " == 'CGIAR center'"),

                                                       selectizeInput(paste0("tLeadCenter_", count), "Choose CGIAR center", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c(
                                                         "Africa Rice Center",
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
                                      conditionalPanel(paste0("input.projLeadEnt_", count, " == 'Other'"),
                                                       selectizeInput(paste0("lead_org_type_1_", count), "",multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."),
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
                                                                                  "Other")),
                                                       hidden(textInput(paste0("lead_org_type_1_", count, "_other"), "")),
                                                       textInput(paste0("leadNameOther_", count), "Experiment, lead organization name", value = "")
                                      ),
                                      textInput(inputId = paste0("expLead_", count), label = "Experiment lead person / Primary Investigator", value = "")
                               )
                             )
                         ) #end box
            )
          )
          count <- count + 1
        }
      }
      else if(projectEntities$numLeads > n){
        start <- n+1
        end <- projectEntities$numLeads
        count <- start
        for(num in start:end){
          removeUI(
            selector = paste0("#fl_box_exp_lead_", count),
            immediate = T
          )
          count <- count +1
        }

      }
      projectEntities$numLeads <- n
    }
    else{
      removeUI(selector = "#fl_exp_leads_aux", immediate = T)
      projectEntities$numLeads <- 0
    }
  })

  ################# fin experiment ######################################################



  ################# personnel ######################################################

   observeEvent(input$btLoadMyInfoPersonnel, {
     if(session$userData$logged){
       updateTextInput(session, "person1FirstName", value=session$userData$userFname)
       updateTextInput(session, "person1LastName", value=session$userData$userLname)
       updateTextInput(session, "person1Email", value=session$userData$userMail)
     }
   })

  ################# fin personnel ######################################################



  ################# site ######################################################

  ### to add when making it dynamic

  ################# fin site ######################################################



  ################# Crop ######################################################

  ### observe for selectize of crops for intercropping

  cropsVar <- reactiveValues()
  cropsVar$selectedIntercrop <- list()
  cropsVar$indexOtherIntercrop <- 0
  cropsVar$varAuxOtherIntercrop <- ""

  observe({
    if(!is.null(input$cropsSelected)){
      otherIndex <- 0

      l <- input$cropsSelected
      n <- length(input$cropsSelected)

      currLen <- length(cropsVar$selectedIntercrop)
      if(currLen == n) return() ## observe has been triggered not by select change


      if(currLen > n){
        start <- n +1
        for(i in start:currLen){
          removeUI(selector = paste0("#intercrop_rows_crop_", i), immediate = T)
          cropsVar$selectedIntercrop[[paste0("c", i)]] <- NULL
        }
      }
      else if(currLen < n){
        start <- currLen +1
        for(i in start:n){
          mselector = paste0("#intercrop_rows_crop_", i-1 )
          if(i == 1){  mselector = "#fr_intercrop_rows"}
          insertUI(
            selector = mselector,
            where = "afterEnd",
            ui =
              column(3, id= paste0("intercrop_rows_crop_", i), style='padding:0px;',
                     column(5, offset = 0, style='padding:25px 2px 0px 0px; text-align:center; word-wrap: break-word;', uiOutput(paste0("intercropName_row_crop_", i))),
                     column(4, offset = 0, style='padding:0px; text-align:left; ', textInput(paste0("intercropValue_row_crop_", i), "")),
                     column(3, offset = 0, style='padding:25px 0px 0px 20px; text-align:center; word-wrap: break-word;',
                            fluidRow(
                              column(9, offset = 0, style='padding:0px; text-align:center;', "row(s)"),
                              column(3, offset = 0, style='padding:0px; text-align:center;',uiOutput(paste0("intercropX_row_crop_", i)))
                            )
                      )
              )

          )
          cropsVar$selectedIntercrop[[paste0("c", i)]] <- "newcrop" ### this will be updated lines below
        }
      }

      for (i in  1:n) {

        if(l[[i]] == "Other"){
          enable(paste0("cropCommonName", i))
          cropsVar$indexOtherIntercrop <- i

          updateTextInput(session,  paste0("cropCommonName", i),  value ="")
        }
        else {
          disable(paste0("cropCommonName", i))
          updateTextInput(session,  paste0("cropCommonName", i),  value = l[[i]])
        }
        cropsVar$selectedIntercrop[[paste0("c", i)]] <- l[[i]]
        output[[paste0("intercropX_row_crop_", i)]] <- renderText("X")
      }

      if(n>=1) output[[paste0("intercropName_row_crop_", 1)]] <- renderText(paste0(cropsVar$selectedIntercrop[[paste0("c", 1)]], ":"))
      if(n>=2) output[[paste0("intercropName_row_crop_", 2)]] <- renderText(paste0(cropsVar$selectedIntercrop[[paste0("c", 2)]], ":"))
      if(n>=3) output[[paste0("intercropName_row_crop_", 3)]] <- renderText(paste0(cropsVar$selectedIntercrop[[paste0("c", 3)]], ":"))
      if(n>=4) output[[paste0("intercropName_row_crop_", 4)]] <- renderText(paste0(cropsVar$selectedIntercrop[[paste0("c", 4)]], ":"))
      if(n>=5) output[[paste0("intercropName_row_crop_", 5)]] <- renderText(paste0(cropsVar$selectedIntercrop[[paste0("c", 5)]], ":"))
      if(n>=6) output[[paste0("intercropName_row_crop_", 6)]] <- renderText(paste0(cropsVar$selectedIntercrop[[paste0("c", 6)]], ":"))
      if(n>=7) output[[paste0("intercropName_row_crop_", 7)]] <- renderText(paste0(cropsVar$selectedIntercrop[[paste0("c", 7)]], ":"))

      output[[paste0("intercropX_row_crop_", n)]] <- renderText("")




      # traitsVals$Data$Crop<-rep(ivan(), nrow(traitsVals$Data))
      # traitsVals$Data$Crop<-rep(input$cropCommonNameMono_other, nrow(traitsVals$Data))
    }
    else{
        removeUI(selector = paste0("#intercrop_rows_crop_", 1), immediate = T)
        cropsVar$selectedIntercrop[[paste0("c", 1)]] <- NULL
    }

  })



  ################# fin crop ######################################################



  ################# Design ######################################################

  path <- fbglobal::get_base_dir()
  # field operations as list of factors
  fp <- file.path(path, "listFactors_v6.rds")

  # para guardar lista de comboboxes para la tabla en treatment description
  lvl <- reactiveValues()
  factors <- as.data.frame(readRDS(fp))
  lvl$lv_1_1 <- unique(factors$GROUP)
  lvl$lv_1_2 <- NULL
  lvl$lv_1_3 <- NULL

  lvl$lv_2_1 <- unique(factors$GROUP)
  lvl$lv_2_2 <- NULL
  lvl$lv_2_3 <- NULL

  lvl$lv_3_1 <- unique(factors$GROUP)
  lvl$lv_3_2 <- NULL
  lvl$lv_3_3 <- NULL

  lvl$lv_4_1 <- unique(factors$GROUP)
  lvl$lv_4_2 <- NULL
  lvl$lv_4_3 <- NULL

  lvl$lv_5_1 <- unique(factors$GROUP)
  lvl$lv_5_2 <- NULL
  lvl$lv_5_3 <- NULL


  ### para la tabla del treatment description cuando
  ### no es full factorial
  treatmentValues <- reactiveValues()

  ## cambia de titulo de bloques a repliacaciones y viceversa cuando se escoge CRD o RCBD
  observeEvent(input$designFieldbook_agrofims, {
    if(input$designFieldbook_agrofims =="CRD"){
      updateSelectInput(session,"designFieldbook_agrofims_r_y", label ="Replications")
      updateSelectInput(session,"designFieldbook_agrofims_r_n", label ="Replications")
    }
    else if(input$designFieldbook_agrofims =="RCBD"){
      updateSelectInput(session,"designFieldbook_agrofims_r_y", label ="Blocks")
      updateSelectInput(session,"designFieldbook_agrofims_r_n", label ="Blocks")
    }

  })

  ### reactivo cuando se selecciona si es full factorial o no
  observeEvent(input$fullFactorialRB, {


    ## titulo de blocks segun el diseno estadistico
    rep_title <- ""
    if(input$designFieldbook_agrofims =="CRD"){
      rep_title <- "Replications"
    }
    else if(input$designFieldbook_agrofims =="RCBD"){
      rep_title <- "Blocks"
    }

    ## verificando si es o no full factorial
    if(input$fullFactorialRB == "Yes"){

      end <- numFactors$numNotFull

      for(num in 1:end){
        removeUI(
          selector = paste0("#not_full_factor_box_", num),
          immediate = T
        )
      }
      numFactors$numNotFull <- 0

      removeUI(
        selector="#not_fluid_full_factor",
        immediate = T
      )

      insertUI(
        selector = "#fluid_treatment_description",
        where = "afterBegin",
        ui = fluidRow( id= "fluid_full_factor",
                       column(width = 12,
                              column(width = 6,
                                     selectInput(inputId = "nfactors_hdafims_y", label = "Number of factors", choices = 1:5, 1)
                              ),
                              column(width = 6,
                                     shiny::selectInput("designFieldbook_agrofims_r_y", rep_title , 2:100, 2 )
                              ),

                              fluidRow(id="full_factor_input")
                       )
        )

      )

      treatmentValues$data <- data.table(c("", ""), # treatment
                                         c("", ""), # factor 1
                                         c("", ""), # factor 2
                                         c("", ""), # factor 3
                                         c("", ""), # factor 4
                                         c("", ""), # factor 5
                                         c("", ""), # factor 1 seleted/wirtten value
                                         c("", ""), # factor 2 seleted/wirtten value
                                         c("", ""), # factor 3 seleted/wirtten value
                                         c("", ""), # factor 4 seleted/wirtten value
                                         c("", "")  # factor 5 seleted/wirtten value
      )

      treatmentValues$baseRow <- c("-", "", "", "", "", "", "", "", "", "", "") ## base row when adding one to treatment table

      colnames(treatmentValues$data) <-  c('TREATMENT', 'FACTOR 1', 'FACTOR 2', 'FACTOR 3', 'FACTOR 4','FACTOR 5', "val1", "val2", "val3", "val4","val5")

    }
    else if(input$fullFactorialRB == "No"){

      aux <- numFactors$numFull +1

      updateSelectInput(session, "nfactors_hdafims_y", selected = aux)


      end <- numFactors$numFull

      end <- end+1

      for(num in 1:end){
        removeUI(
          selector = paste0("#full_factor_box_", num),
          immediate = T
        )
      }

      numFactors$numFull <- 0


      removeUI(
        selector="#fluid_full_factor",
        immediate = T
      )


      insertUI(
        selector = "#fluid_treatment_description",
        where = "afterBegin",
        ui = fluidRow( id= "not_fluid_full_factor",
                       column(width = 12,
                              column(width = 4,
                                     selectizeInput(inputId = "nfactors_hdafims_n", label = "Number of factors",  choices = 1:5, 1)
                              ),
                              column(width = 4,
                                     shiny::selectInput("designFieldbook_agrofims_t_n", "Number of treatments", 2:100, 2 )
                              ),
                              column(width = 4,
                                     shiny::selectInput("designFieldbook_agrofims_r_n", rep_title, 2:100, 2 )
                                     # selectInput(inputId = "nfactors_hdafims_n", label = "Number of factors", choices = 1:5)
                              ),
                              fluidRow(id="not_full_factor_input"),
                              br(),

                              column(12,h2("Level Selection"),
                                     dataTableOutput("Table_treatments"),
                                     tags$head(
                                       tags$script("$(document).on('change', '.select_treatment', function () {
                                          Shiny.onInputChange('treatmentValueClickId',this.id);
                                          Shiny.onInputChange('treatmentValueSelected',this.value);
                                          Shiny.onInputChange('treatmentValueClick', Math.random())
                                  });"
                                       ),

                                  tags$script("$(document).on('change', '.input_treatment', function() {
                                        Shiny.onInputChange('treatmentValueButttonClickId',this.id);
                                        Shiny.onInputChange('treatmentValueButttonEntered',this.value);
                                        Shiny.onInputChange('treatmentValueButttonClick', Math.random())
                                        this.blur();
                                  });"
                                       )
                                     )
                              )


                       )
        )
      )

    }

    deleteAllTabsSoilFertility() ## cleaning all soil fertility tabs

  })


  ### variables para manejo de el numero de factores seleccionados
  numFactors <- reactiveValues()
  numFactors$numFull <- 0
  numFactors$numNotFull <- 0

  ## variables para numero de treatments seleccionados en NOT FULL FACTORIAL
  num <- reactiveValues()
  num$currNumReplications <- 2 ## valor por defecto


  ### cuando se cambia el numero de factores a YES FULL FACTORIAL
  observeEvent(input$nfactors_hdafims_y, {

    iter <- as.numeric(input$nfactors_hdafims_y)

    if(numFactors$numFull < iter){
      start <- numFactors$numFull + 1
      for(i in start:iter){
        drawFullFactorialFactor(i)
      }
    }
    else if(numFactors$numFull > iter){
      start <- iter+1
      end <- numFactors$numFull
      for(num in start:end){
        removeUI(
          selector = paste0("#full_factor_box_", num),
          immediate = T
        )
        removeTabSoilFertility(num) ## deleting soil fertility tab if exists
      }

    }
    numFactors$numFull <- iter
  })

  ### function para dibujar  box con los select cuando es YES FULL FACTORIAL
  drawFullFactorialFactor <- function(order){
    insertUI(
      selector = "#full_factor_input",
      where = "beforeBegin",
      ui =fluidRow(id = paste0("full_factor_box_", order),
                   column(width = 12,
                          box(title = paste0("#", order, " Factor"),
                              width = 12,
                              solidHeader = TRUE, status = "warning",
                              column(width = 12,
                                     fluidRow(
                                       column( width = 6,
                                               fluidRow(
                                                 fluidRow(
                                                   column(width = 4,
                                                          selectizeInput(paste0("sel", order, "_1"), "", choices = lvl[[paste0("lv_", order, "_1")]], multiple =T, options = list(maxItems =1, placeholder ="Select..."))
                                                   ),
                                                   column(width = 4,
                                                          selectizeInput(paste0("sel", order, "_2"), "", choices = NULL, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
                                                   ),
                                                   column(width = 4,
                                                          selectizeInput(paste0("sel", order, "_3"), "", choices =  NULL, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
                                                   )
                                                 )
                                               )

                                       ),
                                       column(width = 6,
                                              fluidRow(
                                                column(width = 6,
                                                       fluidRow(id=paste0("fl_title_factor_aux_", order))

                                                ),
                                                column(width = 6,
                                                       numericInput(paste0("numLevels_", order), HTML("Number of levels"), max = 5, min = 2, value = 2)
                                                )
                                              ),
                                              fluidRow(id= paste0("levelSelection_", order))
                                       )
                                     )
                              )
                          )
                   ))
    )

  }


  ### cuando se cambia el numero de factores a NO FULL FACTORIAL
  observeEvent(input$nfactors_hdafims_n, {
    iter <- as.integer(input$nfactors_hdafims_n)

    if(is.na(iter)  || iter < 1 ) return()

    if(numFactors$numNotFull < iter ){
      start <- numFactors$numNotFull + 1
      for(i in start:iter){
        drawNotFullFactorialFactor(i)
      }
    }
    else if(numFactors$numNotFull > iter){
      start <- iter+1
      end <- numFactors$numNotFull
      for(i in start:end){
        removeUI(
          selector = paste0("#not_full_factor_box_", i),
          immediate = T
        )

        convertListToHTMLSelect(i)
        removeTabSoilFertility(i) ## deleting soil fertility tab if exists
      }
    }
    generateTreatmentStringColumn()
    numFactors$numNotFull <- iter
  })


  ### function para dibujar  box con los select cuando es NO FULL FACTORIAL
  drawNotFullFactorialFactor <- function(order){
    insertUI(
      selector = "#not_full_factor_input",
      where = "beforeBegin",
      ui =
        fluidRow(id = paste0("not_full_factor_box_", order),
                 column(width = 12,

                        box(
                          title = paste0("#", order, " Factor"),
                          width = 12,
                          solidHeader = TRUE, status = "warning",

                          column(width = 12,

                                 fluidRow(
                                   fluidRow(
                                     column(width = 4,
                                            selectizeInput(paste0("sel", order, "_1"), "", choices = lvl[[paste0("lv_", order, "_1")]], multiple =T, options = list(maxItems =1, placeholder ="Select..."))
                                     ),
                                     column(width = 4,
                                            selectizeInput(paste0("sel", order, "_2"), "", choices = NULL, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
                                     ),
                                     column(width = 4,
                                            selectizeInput(paste0("sel", order, "_3"), "", choices =  NULL, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
                                     )
                                   )
                                 )
                          )
                        )
                 )
        )
    )

  }

  ### dibujando tabla de treatments cuando es NO FULL FACTORIAL
  output$Table_treatments <-renderDataTable({
    DT=treatmentValues$data
    datatable(DT,
              escape=F,
              selection = list(mode = 'none'),
              options = list(
                searching = F,
                ordering=F,
                scrollX = TRUE,
                pageLength = 10,
                columnDefs = list(list(className = 'dt-center', width = '15%', targets = 1:6),list(visible=FALSE, targets=7:11) )
              )
    )}
  )


  ### event when a option is selected in list inside treatment table in NO FULL FACTORIAL
  observeEvent(input$treatmentValueClick, {

    var <- input$treatmentValueSelected

    coords <- gsub("select_factor_treatment_","",input$treatmentValueClickId)
    coords <- strsplit(coords, "_")[[1]]

    ## deselecting whichever was selected first
    sel <- gsub(' selected', "", treatmentValues$data[[as.numeric(coords[1])+1]][as.numeric(coords[2])])
    ## the update with the selected value
    sel <- gsub(paste0('<option value="', var,'"'), paste0('<option value="', var,'" selected'), sel)
    treatmentValues$data[[as.numeric(coords[1])+6]][as.numeric(coords[2])] <- var
    treatmentValues$data[[as.numeric(coords[1])+1]][as.numeric(coords[2])] <- sel
    treatmentValues$data[[1]][as.numeric(coords[2])] <-  generateTreatmentString(coords[2])
  })

  ### event when a textbox is written and enter is pressed inside treatment table in NO FULL FACTORIAL
  observeEvent(input$treatmentValueButttonClick, {
    var <- input$treatmentValueButttonEntered
    coords <- gsub("input_factor_treatment_","",input$treatmentValueButttonClickId)
    coords <- strsplit(coords, "_")[[1]]

    var2 <-paste0('<input id="input_factor_treatment_', coords[1], '_', coords[2], '" class ="input_treatment"  value = "', var, '" style="width:150px;"/>')
    treatmentValues$data[[as.numeric(coords[1])+1]][as.numeric(coords[2])] <- var2
    treatmentValues$data[[as.numeric(coords[1])+6]][as.numeric(coords[2])] <- var
    treatmentValues$data[[1]][as.numeric(coords[2])] <-  generateTreatmentString(coords[2])
  })


  ### funcion que concantena los seleccionados de los factores en la tabla treatment en NO FULL FACTORIAL
  generateTreatmentString <- function(row_index){
    nfactors <- as.numeric(input$nfactors_hdafims_n)
    index <- as.numeric(row_index)
    str <- c()

    for(i in 1:nfactors){
      if(treatmentValues$data[[i+6]][index] == ""){
        str <- c(str, "-")
      }
      else{
        str <- c(str,  treatmentValues$data[[i+6]][index])
      }
    }

    return(paste(str, collapse = "/"))
  }


  ### funcion que genera la columna 'treatment' en la tabla de treatments en NO FULL FACTORIAL
  generateTreatmentStringColumn <- function(){
    numTreatments <- as.numeric(input$designFieldbook_agrofims_t_n)

    vals <- c()
    for( i in 1:numTreatments){
      vals <- c(vals, generateTreatmentString(i))
    }
    treatmentValues$data[1] <- vals

  }

  ### evento cuando se cambia el numero de tratamientos en NO FULL FACTORIAL
  observeEvent(input$designFieldbook_agrofims_t_n, {
    rep <- as.numeric(input$designFieldbook_agrofims_t_n)
    if(num$currNumReplications > rep  && !is.na(rep)){
      start<- rep +1
      for(i in num$currNumReplications:start){
        treatmentValues$data <- treatmentValues$data[-i,]
      }
      num$currNumReplications <- rep
    }
    else if(num$currNumReplications < rep && !is.na(rep)){
      start  <- num$currNumReplications +1
      for(i in start:rep){
        treatmentValues$data <-  rbind(treatmentValues$data, as.list(lapply(treatmentValues$baseRow, function(x) gsub("_NUM", paste0("_", i), x))))
      }
      num$currNumReplications <- rep
    }

  })


  ### genera lista desplegable que se usara en el treatment table en NO FULL FACTORIAL
  convertListToHTMLSelect <- function(index, myList="", form="", colname = ""){

    if(is.null(input[["fullFactorialRB"]]) || input[["fullFactorialRB"]] == "Yes" ) return()

    numTreatments <- isolate(input$designFieldbook_agrofims_t_n)
    numTreatments <-  as.integer(numTreatments)

    factor_sel_1 <- input[[paste0("sel", index, "_1")]]

    if(is.null(factor_sel_1) || !is.integer(numTreatments) || numTreatments  < 1 ) return()
    if(!is.null(input[[paste0("sel", index, "_3")]])) colname <- input[[paste0("sel", index, "_3")]]

    ans <- c() ## list for the factor column in table
    ans2 <- c() ## for selected values - hidden column
    opt <- NULL

    str <- ""
    base <- ""
    base_2 <-""

    for(i in 1:numTreatments){
      ans2 <- c(ans2, "")
    }

    if(factor_sel_1 == "Soil fertility"){
      ans2 <- c()
      nLevels <- input[[paste0("numLevels_tabSoil_", index)]]
      base <- paste0('<select id="select_factor_treatment_', index, '_NUM" class ="select_treatment" style="width:150px;">')

      if(is.null(nLevels)) nLevels <- 1
      options_str <- ""

      for(i in 1:nLevels){
        options_str <- paste0(options_str, '<option value="Level ', i,'">Level ', i, '</option>')
      }

      for(i in 1:numTreatments){
        str <- paste0('<select id="select_factor_treatment_', index, '_', i,  '" class ="select_treatment" style="width:150px;">')
        str <- paste0(str, options_str,"</select>" )
        ans <- c(ans, str)
        ans2 <- c(ans2, "Level 1")
      }

    }
    else{
      if(form == "combo box"){ ## is a list separated by semicolons
        opts <- strsplit(myList, ";")[[1]]
        ans2 <- c()
        base <- paste0('<select id="select_factor_treatment_', index, '_NUM" class ="select_treatment" style="width:150px;">')
        base_2 <- opts[1]

        options_str <- ""
        for(opt in opts){
          options_str <- paste0(options_str, '<option value="', opt,'">', opt, '</option>')
        }

        base <- paste0(base, options_str,"</select>" )

        for(i in 1:numTreatments){
          str <- paste0('<select id="select_factor_treatment_', index, '_', i ,  '" class ="select_treatment" style="width:150px;">')
          str <- paste0(str, options_str,"</select>" )
          ans <- c(ans, str)
          ans2 <- c(ans2, opts[1])
        }
      }
      else if( form=="text input"){
        base <- paste0('<input id="input_factor_treatment_', index, '_NUM" class ="input_treatment"  value = "" style="width:150px;"/>')
        for(i in 1:numTreatments){
          str <- paste0('<input id="input_factor_treatment_', index, '_', i, '" class ="input_treatment"  value = "" style="width:150px;"/>')
          ans <- c(ans, str)
        }
      }
      else{ ## is a single value
        str <- myList
        for(i in 1:numTreatments){
          ans <- c(ans, str)
        }
        ans2 <- ans
        base <- str
        base_2 <- str
      }
    }

    if(colname == "") colname <- paste0("FACTOR ", index)

    treatmentValues$data[index+1] <- ans
    treatmentValues$data[index+6] <- ans2  ##  reseting hidden values selected
    colnames(treatmentValues$data)[index+1] <- colname

    treatmentValues$baseRow[index+1] <- base
    treatmentValues$baseRow[index+6] <- base_2

    ## changing base
    numFactors <- as.numeric(input$nfactors_hdafims_n)
    end <- numFactors + 6
    aux <- treatmentValues$baseRow[7:end]
    treatmentValues$baseRow[1] <- paste(replace(aux, aux == "", "-"), collapse = "/")

    generateTreatmentStringColumn()
  }



  ### variable to keep track of soils tabs
  numSoilPanels <- reactiveValues()
  numSoilPanels$current <- c()
  numSoilPanels$levels <- c() ## to control how many levels each tab has
  numSoilPanels$appList <-list() ## to control the list inside comboboxes for applications in soil fertility tabs

  ### function to add tabs for soil fertility
  addTabSoilFertility <- function(index){

    ind <- match(index, numSoilPanels$current)
    if(!is.na(ind)) return()

    len <- length(numSoilPanels$current)
    mtarget <- "tabTreatmentFactors" ## default if list is empty or the tab goes first


    numSoilPanels$current <- c(numSoilPanels$current, index)
    numSoilPanels$levels <- c(numSoilPanels$levels, 0)
    numSoilPanels$appList[[paste0("f", index)]] <- list("void", c()) ## user has not chosen factor yet

    aux_sort <- sort(numSoilPanels$current)

    ind <- match(index, aux_sort)


    if(is.numeric(ind) && ind != 1){
      aux <- aux_sort[ind-1]
      mtarget <- paste0("panelTreatment_soilFertility_",  aux)
    }

    insertTab(inputId = "treatmentSetPanel",
              tabPanel(paste0("Soil fertility details - factor ", index),  value = paste0("panelTreatment_soilFertility_", index),
                       column(12, br(),
                              fluidRow(
                                column(6,
                                  uiOutput(paste0("uiFactorName_tabSoil_", index))
                                ),
                                column(6,
                                       column(4,
                                          numericInput(paste0("numLevels_tabSoil_", index), "Levels", min =1, max=100, value=1)
                                       )
                                )
                              ),
                              fluidRow(id=paste0("fluidRow_levelsTabSoil_", index))
                        )
              ),
              position = "after",
              target = mtarget
    )

  }

  ## observe when changing levels at tab soil fertility for factor 1
  observeEvent(input$numLevels_tabSoil_1, {
    if(!is.null(input$numLevels_tabSoil_1)){
      print(input$numLevels_tabSoil_1)
      isolate(
        drawLevelsSoilTab(1,input$numLevels_tabSoil_1))
      convertListToHTMLSelect(1)
    }
  })

  ## observe when changing levels at tab soil fertility for factor 2
  observeEvent(input$numLevels_tabSoil_2, {
    if(!is.null(input$numLevels_tabSoil_2)){
      isolate(
        drawLevelsSoilTab(2,input$numLevels_tabSoil_2))
      convertListToHTMLSelect(2)
    }

  })

  ## observe when changing levels at tab soil fertility for factor 3
  observeEvent(input$numLevels_tabSoil_3, {
    if(!is.null(input$numLevels_tabSoil_3)){
      isolate(
        drawLevelsSoilTab(3,input$numLevels_tabSoil_3))
      convertListToHTMLSelect(3)
    }
  })

  ## observe when changing levels at tab soil fertility for factor 4
  observeEvent(input$numLevels_tabSoil_4, {
    if(!is.null(input$numLevels_tabSoil_4)){
      isolate(
        drawLevelsSoilTab(4,input$numLevels_tabSoil_4))
        convertListToHTMLSelect(4)
    }
  })

  ## observe when changing levels at tab soil fertility for factor 5
  observeEvent(input$numLevels_tabSoil_5, {
    if(!is.null(input$numLevels_tabSoil_5)){
      isolate(
        drawLevelsSoilTab(5,input$numLevels_tabSoil_5))
      convertListToHTMLSelect(5)
    }
  })

  ## function to draw and remove box levels in soil fertility tabs
  drawLevelsSoilTab <- function(index, levels){
    if(!is.numeric(levels) || levels < 1) return()

    ind <- match(index, numSoilPanels$current)
    mlevels <- numSoilPanels$levels[ind]
    if(!is.numeric(mlevels)) return()

    if(mlevels < levels){
      start <- mlevels + 1
      for(i in start:levels){
          drawBoxLevelTabSoil(index, i)
      }
    }
    else if(mlevels > levels){
      removeBoxeLevelTabSoil(index, levels + 1, mlevels)
    }
    numSoilPanels$levels[ind] <- levels
  }

  ## ui of box levels for soil fertility tabs
  drawBoxLevelTabSoil <- function(index, level){
    box_id <- paste0("box_level_soilTab_", index, "_", level)
    insertUI(selector =paste0("#fluidRow_levelsTabSoil_", index),
             where = "beforeBegin",
             ui =
                fluidRow(id= box_id,
                         box( title = paste0("Level ", level),
                              width = 12,
                              solidHeader = TRUE, status = "warning",
                              fluidRow(
                                column(2),
                                column(10,
                                  column(4),
                                  column(4),
                                  column(4,
                                         selectInput(paste0("numApps_tabSoil_factor_", index, "_box_", level), "# of applications", choices = 1:6, selected = 3)
                                  )
                                )
                              ),
                              fluidRow(
                                column(2, HTML("<center>"), h4(" "), HTML("</center>")),
                                column(10,
                                    column(3, HTML("<center>"),
                                           fluidRow(
                                           column(12,h4("Fertilizer product"), style = "padding: 0px; text-align:center;")
                                           # column(6, style="padding-left:5px; text-align:left;" ,checkboxInput(paste0("checkb_product_", index, "_level_", level), ""))
                                           ),
                                           HTML("</center>")
                                    ),
                                    column(3, HTML("<center>"), h4("Fertilizer product rate (kg/ha)"), HTML("</center>")),
                                    column(3, HTML("<center>"),
                                           fluidRow(
                                             column(12,h4("Nutrient element"), style = "padding: 0px; text-align:center;")
                                             # column(6, style="padding-left:5px; text-align:left;" ,checkboxInput(paste0("checkb_element_", index, "_level_", level), ""))
                                           ),
                                           HTML("</center>")
                                    ),
                                    column(3, HTML("<center>"), h4("Nutrient element rate (kg/ha)"), HTML("</center>"))
                                )
                              ),
                              conditionalPanel(paste0("parseInt(input.numApps_tabSoil_factor_", index, "_box_", level, ")  >=1 ") ,
                                                      drawApplicationRowSoilTab(1, index, level)
                              ),
                              conditionalPanel(paste0("parseInt(input.numApps_tabSoil_factor_", index, "_box_", level, ") >= 2 ") ,
                                                      drawApplicationRowSoilTab(2, index, level)
                              ),
                              conditionalPanel(paste0("parseInt(input.numApps_tabSoil_factor_", index, "_box_", level, ") >= 3 "),
                                                      drawApplicationRowSoilTab(3, index, level)
                              ),
                              conditionalPanel(paste0("parseInt(input.numApps_tabSoil_factor_", index, "_box_", level, ") >= 4 ") ,
                                                      drawApplicationRowSoilTab(4, index, level)
                              ),
                              conditionalPanel(paste0("parseInt(input.numApps_tabSoil_factor_", index, "_box_", level, ") >= 5"),
                                                      drawApplicationRowSoilTab(5, index, level)
                              ),
                              conditionalPanel(paste0("parseInt(input.numApps_tabSoil_factor_", index, "_box_", level, ") >= 6"),
                                               drawApplicationRowSoilTab(6, index, level)
                              ),


                              fluidRow(id= paste0("fluidRow_soilTab_factor_", index, "_level_", level)),
                              fluidRow(
                                column(2),
                                column(10,

                                  column(3,br(),HTML("<div style='text-align:center;'>"), h5("Total calculated application:"), HTML("</div>")),
                                  column(3, disabled(textInput(paste0("input_product_RateTotal_factor_", index, "_level_", level), ""))),
                                  # column(3,
                                  #        fluidRow(
                                  #          column(9,
                                  #                 textInput(paste0("input_product_RateTotal_factor_", index, "_level_", level), "")
                                  #          ),
                                  #          column(3,style=" text-align:left; padding-left:2px;",br(),
                                  #                 actionButton(paste0("buttonSoilTab_product_RateTotal_factor_", index, "_level_", level), "", icon =icon("calculator"))
                                  #                 )
                                  #        )
                                  # ),
                                  column(3),
                                  column(3,disabled(textInput(paste0("input_element_RateTotal_factor_", index, "_level_", level), "")))

                                  # column(3,
                                  #        fluidRow(
                                  #          column(9,
                                  #                 textInput(paste0("input_element_RateTotal_factor_", index, "_level_", level), "")
                                  #          ),
                                  #          column(3,style=" text-align:left; padding-left:2px;",br(),
                                  #                 actionButton(paste0("buttonSoilTab_element_RateTotal_factor_", index, "_level_", level), "", icon =icon("calculator"))
                                  #          )
                                  #        )
                                  # )
                                )
                              )
                          )
                )
    )
  }

  drawApplicationRowSoilTab <- function(napp,index, level){
    fluidRow(
      column(2, br(),HTML("<center>"), h5(paste0("Application ", napp)), HTML("</center>")),
      column(10,
             column(3,
                    fluidRow(id = paste0("fr_selectProductRef_factor_", index, "_level_", level, "_app_", napp)),
                    fluidRow( id = paste0("fr_selectProduct_factor_", index, "_level_", level, "_app_", napp), column(12,
                              selectizeInput(paste0("select_product_factor_", index, "_level_", level, "_app_", napp), "",
                                             getList(numSoilPanels$appList[[paste0("f", index)]][[2]]),  multiple = T, options = list(placeholder ="Select..."))
                    ))
             ),
             column(3,
                    textInput(paste0("input_tabSoil_rate_product_", index, "_level_", level, "_app_", napp), "")
             ),
             column(3,
                    selectizeInput(paste0("input_element_factor_", index, "_level_", level, "_app_", napp), "",multiple = T, options = list(placeholder ="Select..."),
                                   choices = c("Nitrogen",
                                               "Phosphorus",
                                               "Potassium",
                                               "Boron",
                                               "Calcium",
                                               "Copper",
                                               "Iron",
                                               "Manganese",
                                               "Molybdenum",
                                               "Sulfur",
                                               "Zinc",
                                               "Other"
                                               )
                                   )
             ),
             column(3,
                    textInput(paste0("input_tabSoil_rate_element_", index, "_level_", level, "_app_", napp), "")
             )
      )
    )
  }

  # design
  observeEvent(input$calculateTabSoil,{
    aux_vals <- strsplit(input$calculateTabSoilButtonId ,"_")[[1]]
    index  <- aux_vals[5]
    level <- aux_vals[7]
    type <- aux_vals[4]

    napps = as.numeric(input[[paste0("numApps_tabSoil_factor_" , index,"_box_" , level)]])
    values = list()
    lens = list()
    results = list()
    max_len = 0
    sum <- ""

    #print(aux_vals)
    #print(index)

    if(napps >0){
      for(i  in 1:napps){
        in_id = paste0("input_tabSoil_rate_", type ,"_" , index , "_level_" , level , "_app_" , i)
        #print(in_id)
        inp <- input[[in_id]]
        if(inp != ""){
          values[[paste0("v", i)]] <- strsplit(inp, ":")
          lens[[paste0("v", i)]] <- length(values[[paste0("v", i)]][[1]] )
        }
        else{
          values[[paste0("v", i)]] = ""
          lens[[paste0("v", i)]] = 0
        }
        if(max_len < lens[[paste0("v", i)]]) max_len = lens[[paste0("v", i)]]
      }

      if(max_len != 0 ){

        for(i in 1:max_len){

          results[[paste0("v", i)]] <- 0
          for(j in 1:napps){
            if(lens[[j]] >= i){
              num <- as.integer(values[[paste0("v",j)]][[1]][i])
              if(!is.na(num))  results[[paste0("v", i)]] <- num + results[[paste0("v", i)]]
            }
          }
        }
        sum <- paste(results, collapse = ":")
      }

      updateTextInput(session, paste0("input_", type, "_RateTotal_factor_", index, "_level_", level), value= sum)
    }


  })

  # exp conditions 1
  observeEvent(input$calculateTabSoil2,{
    aux_vals <- strsplit(input$calculateTabSoil2ButtonId ,"_")[[1]]
    index  <- aux_vals[5]
    level <- aux_vals[7]
    type <- aux_vals[4]

    #napps = as.numeric(input[[paste0("numApps_tabSoil_factor_" , index,"_box_" , level)]])
    napps = as.numeric(input[[paste0("soil_fertilizer_num_apps")]])
    values = list()
    lens = list()
    results = list()
    max_len = 0
    sum <- ""

    #print(aux_vals)
    #print(index)

    if(napps >0){
      for(i  in 1:napps){
        #in_id = paste0("input_tabSoil_rate_", type ,"_" , index , "_level_" , level , "_app_" , i)
        in_id = paste0("input_productRate_soil_table_row", i)
        inp <- input[[in_id]]

        # add
        # inp <- as.character(inp)
        # if (is.na(inp)) {
        #   inp <- ""
        # }

        #print(inp)
        if(inp != ""){
          values[[paste0("v", i)]] <- strsplit(inp, ":")
          lens[[paste0("v", i)]] <- length(values[[paste0("v", i)]][[1]] )
        }
        else{
          values[[paste0("v", i)]] = ""
          lens[[paste0("v", i)]] = 0
        }
        if(max_len < lens[[paste0("v", i)]]) max_len = lens[[paste0("v", i)]]
      }

      if(max_len != 0 ){

        for(i in 1:max_len){

          results[[paste0("v", i)]] <- 0
          for(j in 1:napps){
            if(lens[[j]] >= i){
              num <- as.integer(values[[paste0("v",j)]][[1]][i])
              if(!is.na(num))  results[[paste0("v", i)]] <- num + results[[paste0("v", i)]]
            }
          }
        }
        sum <- paste(results, collapse = ":")
      }

      #updateTextInput(session, paste0("input_", type, "_RateTotal_factor_", index, "_level_", level), value= sum)
      updateTextInput(session, "soil_fertilizer_totalAppRate1", value= sum)

    }


  })

  # exp conditions 2
  observeEvent(input$calculateTabSoil3,{
    aux_vals <- strsplit(input$calculateTabSoil3ButtonId ,"_")[[1]]
    index  <- aux_vals[5]
    level <- aux_vals[7]
    type <- aux_vals[4]

    #napps = as.numeric(input[[paste0("numApps_tabSoil_factor_" , index,"_box_" , level)]])
    napps = as.numeric(input[[paste0("soil_fertilizer_num_apps")]])
    values = list()
    lens = list()
    results = list()
    max_len = 0
    sum <- ""

    #print(aux_vals)
    #print(index)

    if(napps >0){
      for(i  in 1:napps){
        #in_id = paste0("input_tabSoil_rate_", type ,"_" , index , "_level_" , level , "_app_" , i)
        in_id = paste0("input_elementRate_soil_table_row_", i)
        inp <- input[[in_id]]

        # add
        # inp <- as.character(inp)
        # if (is.na(inp)) {
        #   inp <- ""
        # }

        #print(inp)
        if(inp != ""){
          values[[paste0("v", i)]] <- strsplit(inp, ":")
          lens[[paste0("v", i)]] <- length(values[[paste0("v", i)]][[1]] )
        }
        else{
          values[[paste0("v", i)]] = ""
          lens[[paste0("v", i)]] = 0
        }
        if(max_len < lens[[paste0("v", i)]]) max_len = lens[[paste0("v", i)]]
      }

      if(max_len != 0 ){

        for(i in 1:max_len){

          results[[paste0("v", i)]] <- 0
          for(j in 1:napps){
            if(lens[[j]] >= i){
              num <- as.integer(values[[paste0("v",j)]][[1]][i])
              if(!is.na(num))  results[[paste0("v", i)]] <- num + results[[paste0("v", i)]]
            }
          }
        }
        sum <- paste(results, collapse = ":")
      }

      #updateTextInput(session, paste0("input_", type, "_RateTotal_factor_", index, "_level_", level), value= sum)
      updateTextInput(session, "soil_fertilizer_totalAppRate2", value= sum)

    }


  })



  ### function to remove box levels for soil fertility tab
  removeBoxeLevelTabSoil <- function(index, start, end){
    for(i in start:end){
      box_id <- paste0("#box_level_soilTab_", index, "_", i)
      removeUI(
        selector = box_id,
        immediate = T,
        session = getDefaultReactiveDomain()
      )
    }
  }

  ### function to remove tabs for soil fertility
  removeTabSoilFertility <- function(index){

    ind <- match(index, numSoilPanels$current)

    if(is.na(ind) || ind < 0){ return() }

    removeTab(inputId = "treatmentSetPanel",
              target= paste0("panelTreatment_soilFertility_", index)
    )

    numSoilPanels$current <- numSoilPanels$current[-ind]
    numSoilPanels$levels <- numSoilPanels$levels[-ind]
    numSoilPanels$appList[[paste0("f", index)]] <- NULL
  }

  ### function to delete all soil fertility detail tabs
  deleteAllTabsSoilFertility <- function(){
    mlist <- numSoilPanels$current
    for(val in mlist){
      removeTabSoilFertility(val)
    }

  }

  generateListLevelsSoilTab <- function(index, form ="void", values = NULL, factorName = ""){

    numSoilPanels$appList[[paste0("f", index)]] <- list(form, values)
    output[[paste0("uiFactorName_tabSoil_", index)]] <- renderUI(h3(paste0("Factor: ", factorName)))

    numLevels <- input[[paste0("numLevels_tabSoil_",index)]]

    if(is.numeric(numLevels) && numLevels > 0){
      for(i in 1:numLevels){
        ## number of applications static for now
        numApps <- 6
        for(j in 1:numApps){
          select_id <-  paste0("fr_selectProduct_factor_", index, "_level_", i, "_app_", j)

          ## removing and inserting ui bc  updateSelectizeInput when updating choices to null is not working
          ## must look for better options
          updateSelectizeInput(session,paste0("select_product_factor_", index, "_level_", i, "_app_", j), choices = getList(values))
            # removeUI(
            #   selector =  paste0("#", select_id),
            #   immediate = T,
            #   session = getDefaultReactiveDomain()
            # )
            #
            # insertUI(
            #   selector = paste0("#fr_selectProductRef_factor_", index, "_level_", i , "_app_", j),
            #   where = "afterEnd",
            #   ui = fluidRow( id = select_id,
            #                  selectizeInput(paste0("select_product_factor_", index, "_level_", i, "_app_", j), "",
            #                                 getList(values), multiple = T, options = list(placeholder ="Select..."))
            #   )
            # )

        }

      }
    }

  }
  getList <- function(str){
    if(is.character(str)) return(unlist(strsplit( str, ";")))
    else{ return(c(""))}
  }


  ################# fin design ######################################################


  ###########################################################

  # featNames <- names(Agronomic_features$`Agronomic features`)




  # }) End agronomic trait shinyTree  ####################################


  #### factors ####################################################################################


  ## observe when comboboxes of factors are changed
  observe({
    updateSelectInput(session, "sel1_3", choices = lvl$lv_1_3)
  })
  observe({
    updateSelectInput(session, "sel1_2", choices = lvl$lv_1_2)
  })
  observe({
    updateSelectInput(session, "sel2_3", choices = lvl$lv_2_3)
  })
  observe({
    updateSelectInput(session, "sel2_2", choices = lvl$lv_2_2)
  })
  observe({
    updateSelectInput(session, "sel3_3", choices = lvl$lv_3_3)
  })
  observe({
    updateSelectInput(session, "sel3_2", choices = lvl$lv_3_2)
  })
  observe({
    updateSelectInput(session, "sel4_3", choices = lvl$lv_4_3)
  })
  observe({
    updateSelectInput(session, "sel4_2", choices = lvl$lv_4_2)
  })
  observe({
    updateSelectInput(session, "sel5_3", choices = lvl$lv_5_3)
  })
  observe({
    updateSelectInput(session, "sel5_2", choices = lvl$lv_5_2)
  })

  ## when number of levels are changed for a factor
  observe({
    if(is.numeric(input$numLevels_1) && input$numLevels_1 > 0){
      isolate(updateLevelSelection(1))
    }
  })
  observe({
    if(is.numeric(input$numLevels_2) && input$numLevels_2 > 0){
      isolate(updateLevelSelection(2))
    }
  })
  observe({
    if(is.numeric(input$numLevels_3) && input$numLevels_3 > 0){
      isolate(updateLevelSelection(3))
    }
  })
  observe({
    if(is.numeric(input$numLevels_4) && input$numLevels_4 > 0){
      isolate(updateLevelSelection(4))
    }
  })
  observe({
    if(is.numeric(input$numLevels_5) && input$numLevels_5 > 0){
      isolate(updateLevelSelection(5))
    }
  })


  ## function to draw level selection when is full factorial
  updateLevelSelection <- function(index){

    sel_1 <- input[[paste0("sel", index, "_1")]]
    sel_2 <- input[[paste0("sel", index, "_2")]]
    sel_3 <- input[[paste0("sel", index, "_3")]]

    if(is.null(sel_1) || is.null(sel_2) || is.null(sel_3)) return()

    aux <- dplyr::filter(factors,GROUP==sel_1 & SUBGROUP==sel_2 & FACTOR==sel_3)

    removeUI(selector = paste0("#fl_title_factor_", index), immediate = T)

    if(nrow(aux) > 0){


      if(isolate(input$fullFactorialRB == "No" )){
        if(aux$FORM == "combo box"){
          convertListToHTMLSelect(index, aux$LEVEL, aux$FORM, sel_3)
        }
        else{
          convertListToHTMLSelect(index, sel_3, aux$FORM, sel_3)
        }

      }

      isolate(
        if(sel_1 == "Soil fertility"){
          generateListLevelsSoilTab(index, aux$FORM, aux$LEVEL, sel_3)

          insertUI(
            selector = paste0("#fl_title_factor_aux_", index),
            where = "beforeBegin",
            ui = fluidRow(id=paste0("fl_title_factor_", index),
                          column(width = 12, br(),
                                 h4(HTML(paste0("<b>", sel_3, "</b>")))
                          ),
                          column(width = 12,
                                 h4(HTML(paste0("<font color='red'><b>", "Add design details using tab at top" , "</b></font>")))
                          )
                )
          )

        }
        else{
          insertUI(
            selector = paste0("#fl_title_factor_aux_", index),
            where = "beforeBegin",
            ui = fluidRow(id=paste0("fl_title_factor_", index), column(width = 12, br(), h4(HTML(paste0("<b>", sel_3, "</b>")))))
          )

          if(isolate(is.numeric(input[[paste0("numLevels_", index)]]))){
            if(aux$FORM == "combo box"){
              drawComboboxLevel(index, input[[paste0("numLevels_", index)]], aux$LEVEL)
            }
            else if(aux$FORM == "text input"){
              drawTextInputLevel(index, input[[paste0("numLevels_", index)]], aux$UNIT)
            }
            else if(aux$FORM == "numeric input"){
              drawNumericInputLevel(index, input[[paste0("numLevels_", index)]])
            }

            else if(aux$FORM == "date"){
              drawDateLevel(index, input[[paste0("numLevels_", index)]])
            }

          }


        }
      )


    }
    else{
      removeUI(selector = paste0("#fluid_levels_", index), immediate = T)
      isolate(if(sel_1 == "Soil fertility") {generateListLevelsSoilTab(index)})
    }

  }

  auxfunction <- function(index){

  }

  ## cuando se cambia el primer select del primer factor
  observe({
    if(!is.null(input$sel1_1)){
      aux <- dplyr::filter(factors,GROUP==input$sel1_1)
      lvl$lv_1_2 <- unique(aux$SUBGROUP)
      isolate(
        if(input$sel1_1 == "Soil fertility"){
          addTabSoilFertility(1)
          shinyjs::hide(id="numLevels_1")
          }
        else{
            removeTabSoilFertility(1)
            shinyjs::show(id="numLevels_1")
        }
      )
    }
    else{
      removeTabSoilFertility(1)
      lvl$lv_1_2 <- NULL
      shinyjs::show(id="numLevels_1")
    }

    lvl$lv_1_3 <- NULL
    removeUI(selector = "#fluid_levels_1", immediate = T)
    isolate(convertListToHTMLSelect(1))
    removeUI( selector ="#fl_title_factor_1", immediate = T )

  })
  ## cuando se cambia el segundo select del primer factor
  observe( {
    if(!is.null(input$sel1_2)){
      aux <- dplyr::filter(factors,GROUP==input$sel1_1 & SUBGROUP==input$sel1_2)
      lvl$lv_1_3 <- unique(aux$FACTOR)
    }
    else{
      lvl$lv1_3 <- NULL
    }

    isolate(convertListToHTMLSelect(1))
    isolate(if(!is.null(input$sel1_1) && input$sel1_1 == "Soil fertility") generateListLevelsSoilTab(1))
    removeUI(selector = "#fluid_levels_1", immediate = T)
    removeUI( selector ="#fl_title_factor_1", immediate = T )
  })
  ## cuando se cambia el tercer select del primer factor
  observeEvent(input$sel1_3, {
    removeUI( selector ="#fl_title_factor_1", immediate = T )
    if(!is.null(input$sel1_3)){
      updateLevelSelection(1)
    }
    else{
      isolate(convertListToHTMLSelect(1))
      removeUI(selector = "#fluid_levels_1", immediate = T)
      isolate(if(input$sel1_1 == "Soil fertility") {generateListLevelsSoilTab(1)})
    }
  })


  ## cuando se cambia el primer select del segundo factor
  observe({
    if(!is.null(input$sel2_1)){
      aux <- dplyr::filter(factors,GROUP==input$sel2_1)
      lvl$lv_2_2 <- unique(aux$SUBGROUP)

      isolate(
        if(input$sel2_1 == "Soil fertility"){
          shinyjs::hide(id="numLevels_2")
          addTabSoilFertility(2)
        }
        else{
          shinyjs::show(id="numLevels_2")
          removeTabSoilFertility(2)
        }
      )
    }
    else{
      lvl$lv_2_2 <- NULL
      shinyjs::show(id="numLevels_2")
      removeTabSoilFertility(2)
      updateSelectInput(session, "sel2_2", choices = NULL)
    }
    removeUI(selector = "#fluid_levels_2", immediate = T)
    lvl$lv_2_3 <- NULL
    isolate(convertListToHTMLSelect(2))
    updateSelectInput(session, "sel2_3", choices = NULL)
    removeUI( selector ="#fl_title_factor_2", immediate = T )

  })

  ## cuando se cambia el segudo select del segundo factor
  observe( {
    if(!is.null(input$sel2_2)){
      aux <- dplyr::filter(factors,GROUP==input$sel2_1 & SUBGROUP==input$sel2_2)
      lvl$lv_2_3 <- unique(aux$FACTOR)
    }
    else{
      lvl$lv_2_3 <- NULL
    }
    isolate(convertListToHTMLSelect(2))
    isolate(if(!is.null(input$sel2_1) && input$sel2_1 == "Soil fertility") {generateListLevelsSoilTab(2)})
    removeUI(selector = "#fluid_levels_2", immediate = T)
    removeUI( selector ="#fl_title_factor_2", immediate = T )
  })

  ## cuando se cambia el tercer select del segundo factor
  observeEvent(input$sel2_3,{
    removeUI( selector ="#fl_title_factor_2", immediate = T )
    if(!is.null(input$sel2_3)){
      updateLevelSelection(2)
    }
    else{
      isolate(convertListToHTMLSelect(2))
      isolate(if(input$sel2_1 == "Soil fertility"){generateListLevelsSoilTab(2)})
      removeUI(selector = "#fluid_levels_2", immediate = T)
    }
  })

  ## cuando se cambia el primer select del tercer factor
  observe({
    if(!is.null(input$sel3_1)){
      aux <- dplyr::filter(factors,GROUP==input$sel3_1)
      lvl$lv_3_2 <- unique(aux$SUBGROUP)
      isolate(
        if(input$sel3_1 == "Soil fertility"){
          shinyjs::hide(id="numLevels_3")
          addTabSoilFertility(3)
        }
        else{
          shinyjs::show(id="numLevels_3")
          removeTabSoilFertility(3)
        }
      )
    }
    else{
      lvl$lv_3_2 <- NULL
      shinyjs::show(id="numLevels_3")
      removeTabSoilFertility(3)
      updateSelectInput(session, "sel3_2", choices = NULL)
    }
    removeUI(selector = "#fluid_levels_3", immediate = T)
    lvl$lv_3_3 <- NULL
    isolate(convertListToHTMLSelect(3))
    removeUI( selector ="#fl_title_factor_3", immediate = T )

  })

  ## cuando se cambia el segundo select del tercer factor
  observe( {
    lvl$lv_3_3 <- NULL
    if(!is.null(input$sel3_2)){
      aux <- dplyr::filter(factors,GROUP==input$sel3_1 & SUBGROUP==input$sel3_2)
      lvl$lv_3_3 <- unique(aux$FACTOR)
    }
    # else{
    #
    #   updateSelectInput(session, "sel3_3", choices = NULL)
    # }
    isolate(convertListToHTMLSelect(3))
    isolate(if(!is.null(input$sel3_1) && input$sel3_1 == "Soil fertility") {generateListLevelsSoilTab(3)})
    removeUI(selector = "#fluid_levels_3", immediate = T)
    removeUI( selector ="#fl_title_factor_3", immediate = T )
  })

  ## cuando se cambia el tercer select del tercer factor
  observeEvent(input$sel3_3,{
    removeUI( selector ="#fl_title_factor_3", immediate = T )
    if(!is.null(input$sel3_3)){
      updateLevelSelection(3)
    }
    else{
      isolate(convertListToHTMLSelect(3))
      isolate(if(input$sel3_1 == "Soil fertility"){generateListLevelsSoilTab(3)})
      removeUI(selector = "#fluid_levels_3", immediate = T)
    }
  })

  ## cuando se cambia el primer select del cuarto factor
  observe({
    if(!is.null(input$sel4_1)){
      aux <- dplyr::filter(factors,GROUP==input$sel4_1)
      lvl$lv_4_2 <- unique(aux$SUBGROUP)

      isolate(
        if(input$sel4_1 == "Soil fertility"){
          shinyjs::hide(id="numLevels_4")
          addTabSoilFertility(4)
        }
        else{
          shinyjs::show(id="numLevels_4")
          removeTabSoilFertility(4)
        }
      )
    }
    else{
      lvl$lv_4_2 <- NULL
      shinyjs::show(id="numLevels_4")
      removeTabSoilFertility(4)
    }
    isolate(convertListToHTMLSelect(4))
    removeUI(selector = "#fluid_levels_4", immediate = T)
    lvl$lv_4_3 <- NULL
    removeUI( selector ="#fl_title_factor_4", immediate = T )

  })

  ## cuando se cambia el segundo select del cuarto factor
  observe({
    lvl$lv_4_3 <- NULL
    if(!is.null(input$sel4_2)){
      aux <- dplyr::filter(factors,GROUP==input$sel4_1 & SUBGROUP==input$sel4_2)
      lvl$lv_4_3 <- unique(aux$FACTOR)
    }
    # else{
    #
    #   updateSelectInput(session, "sel4_3", choices = NULL)
    # }
    isolate(convertListToHTMLSelect(4))
    isolate(if(!is.null(input$sel4_1) && input$sel4_1 == "Soil fertility") {generateListLevelsSoilTab(4)})
    removeUI(selector = "#fluid_levels_4", immediate = T)
    removeUI( selector ="#fl_title_factor_4", immediate = T )
  })

  ## cuando se cambia el tercer select del cuarto factor
  observeEvent(input$sel4_3,{
    removeUI( selector ="#fl_title_factor_4", immediate = T )
    if(!is.null(input$sel4_3)){
      updateLevelSelection(4)
    }
    else{
      isolate(convertListToHTMLSelect(4))
      isolate(if(input$sel4_1 == "Soil fertility"){generateListLevelsSoilTab(4)})
      removeUI(selector = "#fluid_levels_4", immediate = T)
    }
  })

  ## cuando se cambia el primer select del quinto factor
  observe({
    if(!is.null(input$sel5_1)){
      aux <- dplyr::filter(factors,GROUP==input$sel5_1)
      lvl$lv_5_2 <- unique(aux$SUBGROUP)
      isolate(
        if(input$sel5_1 == "Soil fertility"){
          shinyjs::show(id="numLevels_5")
          addTabSoilFertility(5)
        }
        else{
          shinyjs::show(id="numLevels_5")
          removeTabSoilFertility(5)
        }
      )
    }
    else{
      lvl$lv_5_2 <- NULL
      shinyjs::show(id="numLevels_5")
      removeTabSoilFertility(5)
    }
    removeUI(selector = "#fluid_levels_5", immediate = T)
    lvl$lv_5_3 <- NULL
    isolate(convertListToHTMLSelect(5))
    removeUI( selector ="#fl_title_factor_5", immediate = T )
  })
  ## cuando se cambia el segudo select del quinto factor
  observe({
    lvl$lv_5_3 <- NULL
    if(!is.null(input$sel5_2)){
      aux <- dplyr::filter(factors, GROUP==input$sel5_1 & SUBGROUP==input$sel5_2)
      lvl$lv_5_3 <- unique(aux$FACTOR)
    }
    # else{
    #
    #   updateSelectInput(session, "sel5_3", choices = NULL)
    # }
    isolate(convertListToHTMLSelect(5))
    isolate(if(!is.null(input$sel5_1) && input$sel5_1 == "Soil fertility") {generateListLevelsSoilTab(5)})
    removeUI(selector = "#fluid_levels_5", immediate = T)
    removeUI( selector ="#fl_title_factor_5", immediate = T )
  })
  ## cuando se cambia el terccer select del quinto factor
  observeEvent(input$sel5_3,{
    removeUI( selector ="#fl_title_factor_5", immediate = T )
    if(!is.null(input$sel5_3)){
      updateLevelSelection(5)
    }
    else{
      isolate(convertListToHTMLSelect(5))
      isolate(if(input$sel5_1 == "Soil fertility"){generateListLevelsSoilTab(5)})
      removeUI(selector = "#fluid_levels_5", immediate = T)
    }
  })

  ## dibuja selectizeInput en los factores cuando tercer select es del tipo lista
  drawComboboxLevel <- function(order, num, lev){
    opt <- strsplit(lev, ";")
    removeUI(selector = paste0("#fluid_levels_", order), immediate = T)
    insertUI(selector = paste0("#levelSelection_", order),
             where = "afterEnd",
             ui = fluidRow( id= paste0("fluid_levels_", order),
                            column(width = 12,
                            selectizeInput(paste0("levels_", order), HTML("Select levels"),
                                           multiple =T,
                                           options = list(maxItems = num, placeholder = "Select ..." ),
                                           choices = opt[[1]]
                            )
                            )
             )
    )
  }

  ## dibuja selectizeInput para escribir en los factores cuando tercer select es del tipo text input
  drawTextInputLevel <- function(order, num, units){
    removeUI(selector = paste0("#fluid_levels_", order), immediate = T)
    if(is.na(units)){
      insertUI(selector = paste0("#levelSelection_", order),
               where = "afterEnd",
               ui = fluidRow( id= paste0("fluid_levels_", order),
                              column(width = 12,
                                selectizeInput(paste0("levels_", order), HTML("Enter levels"),
                                               multiple =T, choices = c(),
                                               options = list(maxItems = num, placeholder = "Write..." ,
                                                              'create' = TRUE,
                                                              'persist' = FALSE)
                                )
                              )
               )
      )
    }
    else{
      vunits <- strsplit(units, ",")
      insertUI(selector = paste0("#levelSelection_", order),
               where = "afterEnd",
               ui = fluidRow( id= paste0("fluid_levels_", order),
                              column(width = 6,
                                     selectizeInput(paste0("levels_",order), HTML("Enter levels"),
                                                    multiple =T, choices = c(),
                                                    options = list(maxItems = num, placeholder = "Write..." ,
                                                                   'create' = TRUE,
                                                                   'persist' = FALSE)
                                     )
                              ),
                              column(width = 6,
                                     selectizeInput(paste0("funits_", order), HTML("Unit"),
                                                    multiple =T, choices = vunits[[1]] ,
                                                    options = list(maxItems = 1, placeholder = "Select unit...")
                                     )
                              )
               )
      )
    }
  }

  ## dibuja numericInput en los factores cuando tercer select es del tipo numeric input
  drawNumericInputLevel <- function(order, num){
    removeUI(selector = paste0("#fluid_levels_", order), immediate = T)
    insertUI(selector = paste0("#levelSelection_", order),
             where = "afterEnd",
             ui = fluidRow( id= paste0("fluid_levels_", order),
                            column(width = 12,
                              numericInput(paste0("levels_", order), HTML("Levels"), min=1, max = num, value = 1)
                            )
             )
    )
  }

  ## dibuja dateInput en los factores cuando tercer select es del tipo date
  drawDateLevel <- function(order, num){
    removeUI(selector = paste0("#fluid_levels_", order), immediate = T)
    insertUI(selector = paste0("#levelSelection_", order),
             where = "afterEnd",
             ui = fluidRow( id= paste0("fluid_levels_", order),
                            column(width = 12,
                                fluidRow( id = paste0("factor_dates_", order , "_1"),
                                  column(width = 6,
                                         dateInput(paste0("factor_start_date_", order, "_1"), HTML("#1 Start date"),format = "yyyy/mm/dd")
                                         ),
                                  column(width = 6,
                                         dateInput(paste0("factor_end_date_", order, "_1"), HTML("#1 End date"),format = "yyyy/mm/dd")
                                  )
                                )
                            )
             )
    )
    if(num > 1){
      for (i in 2:num) {
        insertUI(selector = paste0("#factor_dates_", order,"_", i-1),
                 where = "afterEnd",
                 ui = fluidRow(id = paste0("factor_dates_", order , "_", i) ,
                   column(width = 6,
                          dateInput(paste0("factor_start_date_", order, "_", i), HTML(paste0("#",i, " Start date")),format = "yyyy/mm/dd")
                   ),
                   column(width = 6,
                          dateInput(paste0("factor_end_date_", order, "_", i), HTML(paste0("#", i, " End date")),format = "yyyy/mm/dd")
                   )
                 )
                 # ui =  dateRangeInput(paste0("dates_",order ,"_", i), paste0("#" ,i, " Select dates"), startview = "year",format = "yyyy/mm/dd")
        )
      }}
  }


  #### end factors ####################################################################################

  ################# tabs en field operations ######################################################
  nutTabs = list (#"Crop" = "tabCrop",
                  "Harvest" = "tabHarvest",
                  "Irrigation" = "tabIrrigation",
                  "Land preparation" = "tabLandPr",
                  "Mulching and residue" ="tabMulching",
                  "Planting and transplanting" ="tabPlanting",
                  "Soil fertility" = "tabNutrient",
                  "Weeding" = "tabWeeding"
                  #"Biofertilizer" = "tabBiofertilizer",
                  #"Pest observation and control" = "tabPestNDisease" ,
                  )
  observe({
    # hideTab("nutrienTabPanels", "tabCrop")
    hideTab("nutrienTabPanels", "tabHarvest")
    hideTab("nutrienTabPanels", "tabIrrigation")
    hideTab("nutrienTabPanels", "tabLandPr")
    hideTab("nutrienTabPanels", "tabMulching")
    hideTab("nutrienTabPanels", "tabPlanting")
    hideTab("nutrienTabPanels", "tabNutrient")
    hideTab("nutrienTabPanels", "tabWeeding")
    #hideTab("nutrienTabPanels", "tabBiofertilizer")
    #hideTab("nutrienTabPanels", "tabPestNDisease")


    if(!is.null(input$selectAgroFeature)){
      l <- input$selectAgroFeature
      n <- length(input$selectAgroFeature)

      for (mtab in l) {
        showTab("nutrienTabPanels", nutTabs[[mtab]])

      }

    }
  })

  ################# fin de tabs en field operations ######################################################




  ###########  biofertilizer ##########################################

  ## valor pra guardar cuantos boxes hay actualmente dibujados
  bioferVar <- reactiveValues()
  bioferVar$nApps <-1


  observeEvent(input$numApplicationsBiofert, {
    num <- input$numApplicationsBiofert
      if(is.numeric(num) &&  num>0){
          if(bioferVar$nApps == 1 && num  == 1 ){

            insertUI(selector ="#bio_description",
                     where = "afterEnd",
                     ui = drawBoxBiofertilizer(1))
          }
          else if(bioferVar$nApps == 0 && num  == 1 ){

            insertUI(selector ="#bio_description",
                     where = "afterEnd",
                     ui = drawBoxBiofertilizer(1))
          }
          else if(bioferVar$nApps > num){
            removeBoxesBiofert(num+1, bioferVar$nApps)
            bioferVar$nApps <- num
          }
          else if(bioferVar$nApps < num){
            start <- bioferVar$nApps + 1
            for (i in start:num ) {
                insertUI(selector = paste0("#box_bio_", i-1),
                         where = "afterEnd",
                         ui = drawBoxBiofertilizer(i)
                )
            }
            bioferVar$nApps <- num
          }

    }
    else{
      removeBoxesBiofert(1, bioferVar$nApps)
      bioferVar$nApps <- 0

    }

  })


  removeBoxesBiofert <- function(begin, end){
    for(i in begin:end){
      removeUI(
        selector = paste0("#box_bio_", i),
        immediate = T
      )
    }

  }



  drawBoxBiofertilizer <- function(order){
  fluidRow(id= paste0("box_bio_", order),
    box( title = paste0("Application #", order),
        width = 12,
        solidHeader = TRUE, status = "warning",
        column(width = 6,


               fluidRow(
                 column(width = 6,
                        dateInput(paste0("biofertilizer_landLeveling_start_date_", order), label ="Start date", format = "yyyy/mm/dd")
                 ),
                 column(width = 6,
                        dateInput(paste0("biofertilizer_landLeveling_end_date",  order), label ="End date", format = "yyyy/mm/dd")
                 )
               ),
               selectizeInput(paste0("biofertilizer_rhizobium_inoculum_strain_", order), label = "Rhizobium inoculum strain", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                c("Rhizobium japonicum",
                                  "Rhizobium leguminosarum",
                                  "Rhizobium loti",
                                  "Rhizobium meliloti",
                                  "Rhizobium spp.",
                                  "Rhizobium trifolii",
                                  "Other")
               ),

               conditionalPanel(paste0("input.biofertilizer_rhizobium_inoculum_strain_", order,  " == 'Other'"),
                                textInput(paste0("rhizobium_name_", order),"",value="")),
               fluidRow(
                 column(width = 6,
                        textInput(paste0("biofertilizer_quantity_applied_", order), value = "", label="Biofertilizer quantity applied")
                 ),
                 column(width = 6, #IMPLEMENTAR EN EXCEl
                        selectizeInput(paste0("biofertilizer_quantity_applied_unit_", order), label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("g/m2", "kg/ha", "lb/ac"))
                 )

               )


      ),
      column(width = 6,

        selectizeInput(paste0("biofertilizer_inoculation_method_", order), label = "Inoculation method", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                         c("Seed coating (Seed application of inoculum)",
                           "Directly to the soil",
                           "Other")
        ),
        conditionalPanel(paste0("input.biofertilizer_inoculation_method_", order, " == 'Other'"),
                         textInput(paste0("inoculation_method_name_", order),"",value="")),

        selectizeInput(paste0("biofertilizer_product_formulation_", order), label = "Product formulation", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                         c("Soil application with granules/pellets",
                           "Soil application with slurry of liquid culture")
        ),
        textInput(paste0("biofertilizer_days_sowing_after_rhizobium_inocculation_", order), value="", label = "Days to sowing after Rhizobium inoculation")
      )
    ))


  }

  ############ end biofertilizer ########################################################





  ###########  Pest and Disease ##########################################
  ## valor pra guardar cuantos boxes hay actualmente dibujados
  pestVar <- reactiveValues()
  pestVar$nApps <-1


  observeEvent(input$numApplicationsPestDisease, {
    num <- input$numApplicationsPestDisease
    if(is.numeric(num) &&  num>0){
      if(pestVar$nApps == 1 && num  == 1 ){

        insertUI(selector ="#pestNDisease_fluid",
                 where = "afterEnd",
                 ui = drawBoxPest(1))
      }
      else if(pestVar$nApps == 0 && num  == 1 ){

        insertUI(selector ="#pestNDisease_fluid",
                 where = "afterEnd",
                 ui = drawBoxPest(1))
      }
      else if(pestVar$nApps > num){
        removeBoxesPest(num+1, pestVar$nApps)
        pestVar$nApps <- num
      }
      else if(pestVar$nApps < num){
        start <- pestVar$nApps + 1
        for (i in start:num ) {
          insertUI(selector = paste0("#box_pest_", i-1),
                   where = "afterEnd",
                   ui = drawBoxPest(i)
          )
        }
        pestVar$nApps <- num
      }

    }
    else{
      removeBoxesPest(1, pestVar$nApps)
      pestVar$nApps <- 0

    }

  })


  removeBoxesPest <- function(begin, end){
    for(i in begin:end){
      removeUI(
        selector = paste0("#box_pest_", i),
        immediate = T
      )
    }

  }



  drawBoxPest <- function(order){
    fluidRow(id= paste0("box_pest_", order),
             box( title = paste0("Application #", order),
                  width = 12,
                  solidHeader = TRUE, status = "warning",
                  column(width = 6,

                                  fluidRow(
                                    column(width = 6,
                                           dateInput(paste0("pestcontrol_start_date_",order), label ="Start date", format = "yyyy/mm/dd")
                                    ),
                                    column(width = 6,
                                           dateInput(paste0("pestcontrol_end_date_",order), label ="End date", format = "yyyy/mm/dd")
                                    )
                                  ),
                                  selectizeInput(paste0("pest_control_technique_",order), label = "Pest control technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                   c("Biological pest control",
                                                     "Chemical pest control",
                                                     "Mechanical pest control")
                                  ),
                                  textInput(paste0("pest_name_form_",order), "Pest name/formulation")

                                  # fileInput("myFile", "Pesticide box or bottle picture", accept = c('image/png', 'image/jpeg')),
                                  # textInput("pest_control_applications_totnumber", value="", label = "Pest control applications total number"),
                                  # textInput("pest_control_details", value="", label = "Pest control details (e.g. name of parasitoid etc), treatment evaluation"),
                                  # selectizeInput("chemical_pest_control_equipment", label = "Chemical pest control equipment", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                  #               c("Aerial applicator",
                                  #                 "Airblast sprayer",
                                  #                 "Backpack sprayer",
                                  #                 "Boom sprayer",
                                  #                 "Duster",
                                  #                 "Electrostatic sprayer",
                                  #                 "Fogger",
                                  #                 "Hand sprayer",
                                  #                 "Injection sprayer",
                                  #                 "Mist blower",
                                  #                 "Recirculating sprayer",
                                  #                 "Seed treater",
                                  #                 "Tree injector",
                                  #                 "Wiper")
                                  # )
                           # column(width = 6,
                           #        br(),
                           #        fluidRow(
                           #        box(
                           #          title = "Pesticide Implement", solidHeader = TRUE, status = "warning", width=12,
                           #          textInput("pesticide_implement_make", value="", label = "Implement make"),
                           #          textInput("pesticide_implement_model", value="", label = "Implement model"),
                           #          selectizeInput("pesticide_animal_traction", label = "Animal Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                           #                        c("Buffalo",
                           #                          "Camel",
                           #                          "Donkey",
                           #                          "Elephant",
                           #                          "Horse",
                           #                          "Mule",
                           #                          "Ox / Bullock / Steer",
                           #                          "Other"
                           #                        )
                           #          ),
                           #          textInput("pesticide_humanPowered", value="", label = "Human powered"),
                           #          selectizeInput("pesticide_motorized_traction", label = "Motorized Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                           #                        c("2 wheel tractor",
                           #                          "4 wheel tractor",
                           #                          "Other"
                           #                        )
                           #          )
                           #        ))
                           # )


             ),
             column(width = 6,
                    textInput(paste0("pesticide_application_depth_",order), value="", label = "Pesticide application depth, if applied to soil"),
                    fluidRow(
                      column(width = 6,
                             textInput(paste0("pesticide_amount_",order), value = "", label="Pesticide amount")
                      ),
                      column(width = 6,#IMPLEMENTAR EN EXCEL
                             selectizeInput(paste0("pesticide_amount_unit_",order), label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("kg/m2", "kg/ha", "t/ha"))
                      )
                    ),
                    selectizeInput(paste0("pesticide_control_equip",order), "Pest control equipment", multiple =T, options=list(maxItems=1, placeholder ="Select one..."),
                                   choices = c("Aerial applicator",
                                               "Airblast sprayer",
                                               "Backpack sprayer",
                                               "Boom sprayer",
                                               "Duster",
                                               "Electrostatic sprayer",
                                               "Fogger",
                                               "Hand sprayer",
                                               "Injection sprayer",
                                               "Injection sprayer",
                                               "Mist blower",
                                               "Recirculating sprayer" )
                              )
                  )
             ))


  }

  ############# end Pest and Disease #######################################################


  ########### weeding ###############################################
  ## valor pra guardar cuantos boxes hay actualmente dibujados
  weedingVar <- reactiveValues()
  weedingVar$nApps <- 0


  observeEvent(input$numWeeding, {

    num <- input$numWeeding

    if(!is.numeric(num) || num < 1) return()


    if(weedingVar$nApps < num){
      start <- weedingVar$nApps +1
      for (i in start:num){
        insertUI(selector ="#weeding_description",
                 where = "beforeBegin",
                 ui = drawBoxWeeding(i))
      }

    }

    else if(weedingVar$nApps > num){
      removeBoxesWeeding(num + 1, weedingVar$nApps)
    }

    weedingVar$nApps <- num

  })

  drawBoxWeeding <- function(index){
    fluidRow(id= paste0("box_weeding_", index),
       box( title = paste0("Weeding number ", index),
            width = 12,
            solidHeader = TRUE, status = "warning",
            column(width = 6,
                   h4(HTML(" ")),
                   fluidRow(
                     column(6, dateInput(paste0("weeding_start_date_", index), "Start date", format = "yyyy/mm/dd")),
                     column(6, dateInput(paste0("weeding_end_date_", index), "End date", format = "yyyy/mm/dd"))
                   ),
                   #selectInput(paste0("weeding_technique_", index), "Technique", c("Chemical", "Manual", "Mechanized"))
                   selectizeInput(paste0("weeding_technique_", index), "Technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."),
                                  choices =  c(
                                    "Chemical",
                                    "Manual",
                                    "Mechanized")
                   )
            ),
            column(6,
                   h4("Implement"),
                   selectizeInput(paste0("weeding_type_",index ), "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."),
                               choices =  c(
                                  "Cultivator",
                                  "Manual",
                                  "Sprayer",
                                  "Weed cutter/puller",
                                  "Other")
                              ),
                   hidden(textInput(paste0("weeding_type_",index, "_other" ), "")

                  ),
                  selectizeInput(paste0("weeding_traction_", index), "Traction",multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."),
                              choices= c(
                                "Animal",
                                "Manual",
                                "2 wheel tractor",
                                "4 wheel tractor",
                                "Other")
                  ),
                  hidden(textInput(paste0("weeding_traction_",index, "_other" ), ""))


              )
        )

    )

  }


  removeBoxesWeeding <- function(begin, end){
    for(i in begin:end){
      removeUI(
        selector = paste0("#box_weeding_", i),
        immediate = T
      )
    }

  }

  #################### end weeding #############################################

  ###########  irrigation ##########################################
  ## valor pra guardar cuantos boxes hay actualmente dibujados
  irrigVar <- reactiveValues()
  irrigVar$nApps <-3

  observeEvent(input$numApplicationsIrrigation, {
    num <- input$numApplicationsIrrigation

    if(is.numeric(num) &&  num > 2){
      if(irrigVar$nApps == 3 && num  == 3){

        insertUI(selector ="#irrig_description",
                 where = "afterEnd",
                 ui = drawBoxIrrigation(1))

        insertUI(selector = paste0("#box_irrig_", 1),
                 where = "afterEnd",
                 ui = drawBoxIrrigation(2)
        )

        insertUI(selector = paste0("#box_irrig_", 2),
                 where = "afterEnd",
                 ui = drawBoxIrrigation(3)
        )

      }
      else if(irrigVar$nApps == 0 ){

        insertUI(selector ="#irrig_description",
                 where = "afterEnd",
                 ui = drawBoxIrrigation(1))
        start = 2;
        for (i in start:num ) {
          insertUI(selector = paste0("#box_irrig_", i-1),
                   where = "afterEnd",
                   ui = drawBoxIrrigation(i)
          )
        }
        irrigVar$nApps <- num
      }
      else if(irrigVar$nApps > num){
        removeBoxesIrrigation(num+1, irrigVar$nApps)
        irrigVar$nApps <- num
      }
      else if(irrigVar$nApps < num){
        start <- irrigVar$nApps + 1
        for (i in start:num ) {
          insertUI(selector = paste0("#box_irrig_", i-1),
                   where = "afterEnd",
                   ui = drawBoxIrrigation(i)
          )
        }
        irrigVar$nApps <- num
      }

    }
    else{
      removeBoxesIrrigation(1, irrigVar$nApps)
      irrigVar$nApps <- 0
    }

  })

  removeBoxesIrrigation <- function(begin, end){
    for(i in begin:end){
      removeUI(
        selector = paste0("#box_irrig_", i),
        immediate = T
      )
    }

  }

  drawBoxIrrigation <- function(order){
    fluidRow(id= paste0("box_irrig_", order),
             box( title = paste0("Application #", order),
                  width = 12,
                  solidHeader = TRUE, status = "warning",
                  column(width = 6,


                         fluidRow(
                           column(width = 6,
                                  dateInput(paste0("irrigationevent_start_date_", order), label ="Start date", value = NA, format = "yyyy/mm/dd")
                           ),
                           column(width = 6,
                                  dateInput(paste0("irrigationevent_end_date_", order), label = "End date", value = NA, format = "yyyy/mm/dd")
                           )
                         ),
                         selectizeInput(paste0("irrigation_technique_", order), label = "Irrigation technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                          c("Sprinkler irrigation",
                                            "Localized",
                                            "Surface",
                                            #"Sub-irrigation",
                                            "Other")
                         ),
                         hidden(textInput(paste0("irrigation_technique_", order, "_other"), "")),
                         conditionalPanel(paste0("input.irrigation_technique_", order, "== 'Surface'"),
                                          selectizeInput(paste0("surface_irrigation_technique_", order), label = "Surface irrigation technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                           c("Basin irrigation",
                                                             "Border irrigation",
                                                             "Continuous flood",
                                                             "Furrow irrigation",
                                                             "Uncontrolled flooding",
                                                             "Other")
                                          ),
                                          hidden(textInput(paste0("surface_irrigation_technique_", order, "_other"), ""))
                         ),
                         conditionalPanel(paste0("input.irrigation_technique_", order, "== 'Localized'"),

                                          selectizeInput(paste0("localized_irrigation_technique", order), label = "Localized irrigation technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                           c("Bubbler irrigation",
                                                             "Drip irrigation",
                                                             "Mist irrigation",
                                                             "Pitcher irrigation",
                                                             "Subsurface drip irrigation",
                                                             "Subsurface textile irrigation",
                                                             "Other")
                                          ),
                                          hidden(textInput(paste0("localized_irrigation_technique", order, "_other"), ""))
                         ),
                         conditionalPanel(paste0("input.irrigation_technique_", order, "== 'Sprinkler irrigation'"),

                                          selectizeInput(paste0("irrigation_using_sprinkler_systems_", order), label = "Sprinkler irrigation system", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                           c("Center pivot irrigation",
                                                             "Irrigation by lateral move",
                                                             "Irrigation by side move",
                                                             "Other")
                                          ),
                                          hidden(textInput(paste0("irrigation_using_sprinkler_systems_", order, "_other"), ""))
                         ),


                         #Sacar myFile upload
                         # fileInput(paste0("myFile", "Irrigation system picture_", order), accept = c('image/png', 'image/jpeg')),
                         # textInput(paste0("irrigation_water_source_", order), value="", label = "Water source"),
                         selectizeInput(paste0("irrigation_source_", order), label = "Irrigation source", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                          c("Drainage",
                                            "Groundwater",
                                            "Lake",
                                            "Reservoir",
                                            "River",
                                            "Spring",
                                            "Other")
                         ),
                         hidden(textInput(paste0("irrigation_source_", order,  "_other"), ""))#,
                         # fluidRow(
                         #    column(width = 6,
                         #      textInput(paste0("irrigation_water_source_distance_", order), value="", label = "Water source distance")
                         #    ),
                         #    column(width = 6,
                         #           selectizeInput(paste0("irrigation_water_source_distance_", order, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                         #                          choices = c("m", "km")
                         #           )
                         #    )
                         # )#,
                         # fluidRow(
                         #   column(width = 6,
                         #          textInput(paste0("irrigation_bund_height_", order), value="", label = "Bund height")
                         #   ),
                         #   column(width = 6,
                         #          selectizeInput(paste0("irrigation_bund_height_", order, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                         #                         choices = c("cm", "in", "m")
                         #          )
                         #   )
                         # )

                  ),
                  column(width = 6,

                         # fluidRow(
                         #     column(width = 6,
                         #        textInput(paste0("irrigation_percolation_rate_", order), value="", label = "Percolation rate")
                         #     ),
                         #    column(width = 6,
                         #           selectizeInput(paste0("irrigation_percolation_rate_", order, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                         #                          choices = c("mm per day")
                         #           )
                         #    )
                         #  ),
                         # fluidRow(
                         #   column(width = 6,
                         #        textInput(paste0("irrigation_equipment_depth_", order), value="", label = "Irrigation equipment depth")
                         #   ),
                         #   column(width = 6,
                         #          selectizeInput(paste0("irrigation_equipment_depth_", order, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                         #                         choices = c("cm", "in", "m")
                         #          )
                         #   )
                         # ),
                         # fluidRow(
                         #   column(width = 6,
                         #      textInput(paste0("irrigation_well_depth_", order), value="", label = "Well depth")
                         #   ),
                         #   column(width = 6,
                         #          selectizeInput(paste0("irrigation_well_depth_", order, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                         #                         choices = c("m")
                         #          )
                         #   )
                         # ),
                         fluidRow(
                           column(width = 6,
                                  #textInput(paste0("irrigation_source_distance_", order), value="", label = "Irrigation source distance")
                                  numericInput(paste0("irrigation_source_distance_", order), label = "Irrigation source distance", value = "", min = 0, step = 0.1)
                           ),
                           column(width = 6,
                                  selectizeInput(paste0("irrigation_source_distance_", order, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                 choices = c("ft", "km", "m", "mi")
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  #textInput(paste0("irrigation_amount_", order), value="", label = "Irrigation amount")
                                  numericInput(paste0("irrigation_amount_", order), label = "Irrigation amount", value = "", min = 0, step = 0.1)
                           ),
                           column(width = 6,
                                  selectizeInput(paste0("irrigation_amount_", order, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                 choices = c("in", "mm")#, "cm", "m", "in", "ft", "ml", "L", "gal", "cu m", "cu in", "cu ft")
                                  )
                           )
                         ),
                         textAreaInput(paste0("irrigation_notes_", order), label = "Notes", value = "")

                         # fluidRow(
                         #   column(width = 6,
                         #          textInput(paste0("irrigation_area_covered_irrigation_system_", order), value="", label = "Area covered by the irrigation system")
                         #   ),
                         #    column(width = 6,
                         #          selectizeInput(paste0("irrigation_area_covered_irrigation_system_", order, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                         #                         choices = c("m2", "ha")
                         #          )
                         #    )
                         # )

                  )
             ))


  }

  ####################################################################

  ########## soil fertility ##################################################

  soilFertilityVar <- reactiveValues()
  soilFertilityVar$nApps <- 0
  soilFertilityVar$products <- c()
  soilFertilityVar$productsOther <- 0


  soilFertility_types <- list()
  soilFertility_types[["Biofertilizer"]] <- c('Arbuscular mycorrhizal fungi',
                                              'Rhizobium japonicum',
                                              'Rhizobium leguminosarum',
                                              'Rhizobium loti',
                                              'Rhizobium meliloti',
                                              'Rhizobium spp.',
                                              'Rhizobium trifolii',
                                              'Other')
  soilFertility_types[["Green manure"]] <- c('Lablab',
                                             'Mucuna',
                                             'Sesbania sp.',
                                             'Vigna sp.',
                                             'Other')
  soilFertility_types[["Inorganic"]] <- c('Ammonium nitrate',
                                          'Ammonium nitrate sulfate',
                                          'Ammonium polyphosphate',
                                          'Ammonium sulfate',
                                          'Anhydrous ammonia',
                                          'Aqua ammonia',
                                          'Calcitic limestone',
                                          'Calcium ammonium nitrate ',
                                          'Calcium hydroxide',
                                          'Calcium nitrate',
                                          'Diammonium phosphate',
                                          'Dolomitic limestone',
                                          'Liquid phosphoric acid',
                                          'Monoammonium phosphate',
                                          'NPK fertilizers',
                                          'Potassium chloride',
                                          'Potassium nitrate',
                                          'Potassium sulfate',
                                          'Rock phosphate',
                                          'Single super phosphate',
                                          'Triple super phosphate',
                                          'Urea',
                                          'Urea ammonium nitrate solution',
                                          'Urea super granules',
                                          'Other')
  soilFertility_types[["Lime"]] <- c('Calcitic limestone',
                                     'Crushed limestone',
                                     'Dolomitic limestone',
                                     'Other')
  soilFertility_types[["Organic"]] <- c('Alfalfa Meal',
                                        'Bagasse',
                                        'Biochar',
                                        'Biochar',
                                        'Chicken litter (Poultry manure)',
                                        'Compost',
                                        'Farmyard manure ',
                                        'Fish fertilizer',
                                        'Guano',
                                        'Liquid manure',
                                        'Oil cake',
                                        'Treated sewage sludge',
                                        'Vermicompost',
                                        'Other')

  observeEvent(input$soilFertility_typeFertilizer, {

    type_id <- isolate(input$soilFertility_typeFertilizer_id)
    type_value <- isolate(input$soilFertility_typeFertilizer_value)

    product_id <- gsub("fertilizerType", "product", type_id)

    if(type_value != ""){
      updateSelectizeInput(session, product_id, choices = soilFertility_types[[type_value]])
    }
    else{
      updateSelectizeInput(session, product_id, choices = c(""))

    }


  })

  observeEvent(input$soil_fertilizer_num_apps, {

    num <- input$soil_fertilizer_num_apps

    if(!is.numeric(num) || num < 1) return()


    if(soilFertilityVar$nApps < num){
      start <- soilFertilityVar$nApps +1
      for (i in start:num){
        insertUI(selector ="#fr_fertilizer_application",
                 where = "beforeBegin",
                 ui = drawRowSoilFertility(i))
      }

    }

    else if(soilFertilityVar$nApps > num){
      removeRowSoilFertility(num + 1, soilFertilityVar$nApps)
    }

    soilFertilityVar$nApps <- num

  })

  drawRowSoilFertility <- function(index){
    fluidRow(id= paste0("row_soilFertility_", index),
          column(12,
             column(1, style="padding:3px; text-align:center;", br(), h4(index)),
             column(10, style="padding:0px;",
                    column(6, style="padding:0px;",
                           column(3, style="padding:5px;",
                                  selectizeInput(paste0("select_fertilizerType_soil_table_row_", index), "",multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."),
                                                 choices= c(
                                                   "Biofertilizer",
                                                   "Green manure",
                                                   "Inorganic",
                                                   "Lime",
                                                   "Organic")
                                  )
                            ),
                           column(5, style="padding:5px;",
                                  selectizeInput(paste0("select_product_soil_table_row_", index), "",multiple = TRUE, options = list(placeholder ="Select one..."),
                                                 choices= c()
                                  ),
                                  hidden(textInput(paste0("select_product_soil_table_row_",index, "_other" ), ""))

                           ),
                           column(2, style="padding:5px;",
                                  #numericInput(paste0("input_productRate_soil_table_row", index), "", min=1, max=100, value=NULL, step=1)
                                  textInput(paste0("input_productRate_soil_table_row", index), "")
                                  ),
                           column(2, style="padding:5px;",
                                  selectizeInput(paste0("select_element_soil_table_row_", index), "",multiple = TRUE, options = list( placeholder ="Sel..."),
                                                 # choices= c(
                                                 #   "B",
                                                 #   "Ca",
                                                 #   "Cu",
                                                 #   "Fe",
                                                 #   "K",
                                                 #   "Mn",
                                                 #   "Mo",
                                                 #   "N",
                                                 #   "P",
                                                 #   "S",
                                                 #   "Zn",
                                                 #   "Other")
                                                 choices = c("Nitrogen",
                                                             "Phosphorus",
                                                             "Potassium",
                                                             "Boron",
                                                             "Calcium",
                                                             "Copper",
                                                             "Iron",
                                                             "Manganese",
                                                             "Molybdenum",
                                                             "Sulfur",
                                                             "Zinc",
                                                             "Other"
                                                 )
                                  ),
                                  hidden(textInput(paste0("select_element_soil_table_row_",index, "_other" ), ""))
                           )
                    ),
                    column(6,style="padding:0px;",
                           column(2, style="padding:5px;",
                                  #numericInput(paste0("input_elementRate_soil_table_row_", index), "", min=1, max=100, value=NULL, step=1)
                                  textInput(paste0("input_elementRate_soil_table_row_", index), "")
                                  ),
                           # column(3, style="padding:5px;",
                           #        selectizeInput(paste0("select_implement_soil_table_row_", index), "", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."),
                           #                       choices= c(
                           #                         "Backpack sprayer (airblast sprayer)",
                           #                         "Boom sprayer",
                           #                         "Broadcast spreader",
                           #                         "Hand sprayer",
                           #                         "Manual application",
                           #                         "Manure spreader",
                           #                         "Slurry injector",
                           #                         "Other"
                           #                       )
                           #        ),
                           #        hidden(textInput(paste0("select_implement_soil_table_row_",index, "_other" ), ""))
                           # ),
                           column(3, style="padding:5px;",
                                  dateInput(paste0("input_startdate_soil_table_row_", index), "", format = "yyyy/mm/dd")
                           ),
                           # column(3, style="padding:5px",
                           #        selectizeInput(paste0("select_traction_soil_table_row_", index), "", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."),
                           #                       choices= c(
                           #                         "Animal",
                           #                         "Manual",
                           #                         "2 wheel tractor",
                           #                         "4 wheel tractor",
                           #                         "Other"
                           #                       )
                           #        ),
                           #        hidden(textInput(paste0("select_traction_soil_table_row_",index, "_other" ), ""))
                           # ),
                           column(3, style="padding:5px",
                                  dateInput(paste0("input_enddate_soil_table_row_", index), "", format = "yyyy/mm/dd")
                           ),
                           column(4, style="padding:5px;",
                                  selectizeInput(paste0("select_techinque_soil_table_row_", index), "", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."),
                                                 choices= c(
                                                   "Band application on surface",
                                                   "Band application beneath surface",
                                                   "Broadcast incorparated",
                                                   "Broadcast surface",
                                                   "Contact placement with seed",
                                                   "Deep placement",
                                                   "Fertigation",
                                                   "Foliar application",
                                                   "Injection",
                                                   "Localized application (using mechanical or hand device)",
                                                   "Seed coating",
                                                   "Side dressing",
                                                   "Sub-soil placement (injection)",
                                                   "Other"
                                                 )
                                  ),
                                  hidden(textInput(paste0("select_techinque_soil_table_row_",index, "_other" ), ""))
                           )
                    )
             ),
             column(1,  style="padding:3px;",
                    textAreaInput(paste0("textArea_soil_table_row_", index), "")
             )
        )

    )

  }


  removeRowSoilFertility <- function(begin, end){
    for(i in begin:end){
      removeUI(
        selector =paste0("#row_soilFertility_", i),
        immediate = T
      )
    }

  }

  observeEvent(input$addproducts_soil,{
    num_others <- soilFertilityVar$productsOther + 1

    if(num_others > 6) return()

    insertUI(
      selector= "#fr_fertilizer_application_nutrient_title",
      where = "beforeBegin",
      ui =  column(1, id= paste0("col_input_soilProduct_other_", num_others, "_title" ), style = "padding:10px;",
                   textInput(paste0("input_soilProduct_other_", num_others ), "", placeholder = "Other (%)")
                   # fluidRow(id = paste0("fr_aux_soil_other_", num_others))
            )
    )

    insertUI(
      selector= "#fr_fertilizer_application_nutrient",
      where = "beforeBegin",
      ui =  column(1, id= paste0("col_input_soilProduct_other_", num_others ), style = "padding:10px;",
                   # textInput(paste0("input_soilProduct_other_", num_others ), "", placeholder = "Other (%)"),
                   fluidRow(id = paste0("fr_aux_soil_other_", num_others))
      )
    )

    soilFertilityVar$productsOther <- num_others
    len <- length(soilFertilityVar$products)
    if(len < 1) return()

    products <- soilFertilityVar$products

    for(i in 1:len){

      mid <- paste0("input_soil_nutrient_product_other_", num_others, "_", gsub("\\.", "", gsub(" ", "_", products[i])))
      selector <- paste0("#fr_input_soil_nutrient_product_other_", num_others ,"_", gsub("\\.", "",gsub(" ", "_", products[i-1])))


      if(i ==1) selector <- paste0("#fr_aux_soil_other_", num_others)

      insertUI(
        selector = selector,
        where = "afterEnd",
        ui = fluidRow( id = paste0("fr_", mid), column(12,
                                                       numericInput(mid, "", min=0, max=100, value=NULL)
        ))
      )
    }



  })

  observeEvent(input$delproducts_soil, {
    n_others <- soilFertilityVar$productsOther
    if(n_others  < 1) return()
    removeUI(
      selector =  paste0("#col_input_soilProduct_other_", n_others),
      immediate = T
    )
    removeUI(
      selector =  paste0("#col_input_soilProduct_other_", n_others, "_title"),
      immediate = T
    )

    soilFertilityVar$productsOther <- n_others - 1


  })

  observeEvent(input$soilFertility_product, {
    napps <- input$soil_fertilizer_num_apps
    default_products<- c("N", "P", "K")

    n_others <- soilFertilityVar$productsOther
    if(n_others>0){
      for( i in 1:n_others){
        default_products <- c(default_products, paste0("other_", i))
      }
    }

    newList <- c()

    for(i in 1:napps){
      newList <- c(newList, input[[paste0("select_product_soil_table_row_", i)]] )
    }

    newList <- sort(unique(newList))
    currList <- soilFertilityVar$products

    items_deleted <- currList[!(currList %in% newList)]
    items_added <- newList[!(newList %in% currList)]


    if(length(items_deleted) > 0){
      for(item in items_deleted){
        removeUI(
          selector = paste0("#fr_input_soil_nutrient_product_", gsub("\\.", "",gsub(" ", "_", item))),
          immediate = T
        )

        for(i in default_products){
          removeUI(
            selector = paste0("#fr_input_soil_nutrient_product_", i, "_", gsub("\\.", "",gsub(" ", "_", item))),
            immediate = T
          )
        }

      }
    }

    if( length(items_added)> 0){
      for(item in items_added){
        index <- match(item, newList)
        id_selector_name = "#fr_aux_soil_fertProduct"
        id_selector_prod = "#fr_aux_soil_XXXX"


        if(index > 1){
          b_item <- newList[index-1]
          id_selector_name <- paste0("#fr_input_soil_nutrient_product_", gsub("\\.", "",gsub(" ", "_", b_item)))
          id_selector_prod <- paste0("#fr_input_soil_nutrient_product_XXXX_", gsub("\\.", "",gsub(" ", "_", b_item)))
        }

        mid <- paste0("input_soil_nutrient_product_", gsub("\\.", "", gsub(" ", "_", item)))
        insertUI(
          selector = id_selector_name,
          where = "afterEnd",
          ui = fluidRow( id = paste0("fr_", mid),column(12,
                         disabled(textInput(mid, "", value=item)
                ))
          )
        )

        for(i in default_products){
          mid <- paste0("input_soil_nutrient_product_", i, "_", gsub("\\.", "", gsub(" ", "_", item)))
          insertUI(
            selector = gsub("XXXX", i,id_selector_prod ),
            where = "afterEnd",
            ui = fluidRow( id = paste0("fr_", mid), column(12,
              numericInput(mid, "", min=0, max=100, value=NULL)
              ))
          )
        }


      }
    }

    soilFertilityVar$products <- newList

  })


  # fr_fertilizer_application_nutrient



  ######## end soil fertility ###############################################


  ###########  nutrients ######################################################
  ## valor pra guardar cuantos boxes hay actualmente dibujados
  nutVar <- reactiveValues()
  nutVar$types <- list()

  observe( {
    if(is.null(input$appfTypeFertilizer) ){
      removeUI(
        selector = paste0("#Organic_fertilizer_box"),
        immediate = T
      )
      removeUI(
        selector = paste0("#Inorganic_fertilizer_box"),
        immediate = T
      )
      removeUI(
        selector = paste0("#Green_manure_fertilizer_box"),
        immediate = T
      )
      nutVar$types <- list()
    }
    else{
      l <- input$appfTypeFertilizer
        for (typ in l) {
          vtype = gsub(" ", "_", typ)
          if (!(vtype %in% nutVar$types)){
            insertUI(selector ="#fert123",
                     where = "beforeBegin",
                     ui = drawTypeFertBox(vtype))
          }
        }
      for (xvar in nutVar$types){
        vtype = gsub("_", " ", xvar)
        if (!(vtype %in% l)){
          removeUI(
            selector = paste0("#", gsub(" ","_", vtype), "_fertilizer_box"),
            immediate = T
          )
        }
      }
      nutVar$types <- gsub(" ", "_",l)

    }

  })

  drawTypeFertBox <- function(type){
    fluidRow(id = paste0(type, "_fertilizer_box"),
          box(title = gsub("_", " ", type),
                      width = 12,
                      solidHeader = TRUE, status = "primary", collapsible = T, collapsed = T,
              fluidRow(id = paste0(type, "_fertilizer_box_in"),
                column(width = 6,
                       numericInput(paste0("numApplications_", type), label  = "Number of applications", value = 1, min = 1, max = 5)

                )
            )
      )
    )

  }

  nutVar$nAppsOrg <- 1
  observeEvent(input$numApplications_Organic, {
    num <- input$numApplications_Organic
    if(is.numeric(num) &&  num>0){
      if(nutVar$nAppsOrg == 1 && num  == 1 ){

        insertUI(selector ="#Organic_fertilizer_box_in",
                 where = "afterEnd",
                 ui = drawBoxNutrients(1, "Organic"))
      }
      else if(nutVar$nAppsOrg == 0 && num  == 1 ){

        insertUI(selector ="#Organic_fertilizer_box_in",
                 where = "afterEnd",
                 ui = drawBoxNutrients(1, "Organic"))
      }
      else if(nutVar$nAppsOrg > num){
        removeBoxesNutrients(num+1, nutVar$nAppsOrg, "Organic")
        nutVar$nAppsOrg <- num
      }
      else if(nutVar$nAppsOrg < num){
        start <- nutVar$nAppsOrg + 1
        for (i in start:num ) {
          insertUI(selector = paste0("#box_nut_Organic_", i-1),
                   where = "afterEnd",
                   ui = drawBoxNutrients(i, "Organic")
          )
        }
        nutVar$nAppsOrg <- num
      }

    }
    else{
      removeBoxesNutrients(1, nutVar$nAppsOrg, "Organic")
      nutVar$nAppsOrg <- 0
    }

  })


  nutVar$nAppsInorg <- 1
  observeEvent(input$numApplications_Inorganic, {
    num <- input$numApplications_Inorganic
    if(is.numeric(num) &&  num>0){
      if(nutVar$nAppsInorg == 1 && num  == 1 ){

        insertUI(selector ="#Inorganic_fertilizer_box_in",
                 where = "afterEnd",
                 ui = drawBoxNutrients(1, "Inorganic"))
      }
      else if(nutVar$nAppsInorg == 0 && num  == 1 ){

        insertUI(selector ="#Inorganic_fertilizer_box_in",
                 where = "afterEnd",
                 ui = drawBoxNutrients(1, "Inorganic"))
      }
      else if(nutVar$nAppsInorg > num){
        removeBoxesNutrients(num+1, nutVar$nAppsInorg, "Inorganic")
        nutVar$nAppsInorg <- num
      }
      else if(nutVar$nAppsInorg < num){
        start <- nutVar$nAppsInorg + 1
        for (i in start:num ) {
          insertUI(selector = paste0("#box_nut_Inorganic_", i-1),
                   where = "afterEnd",
                   ui = drawBoxNutrients(i, "Inorganic")
          )
        }
        nutVar$nAppsInorg <- num
      }

    }
    else{
      removeBoxesNutrients(1, nutVar$nAppsInorg, "Inorganic")
      nutVar$nAppsInorg <- 0
    }

  })

  nutVar$nAppsGreenManure <- 1
  observeEvent(input$numApplications_Green_manure, {
    num <- input$numApplications_Green_manure
    if(is.numeric(num) &&  num>0){
      if(nutVar$nAppsGreenManure == 1 && num  == 1 ){

        insertUI(selector ="#Green_manure_fertilizer_box_in",
                 where = "afterEnd",
                 ui = drawBoxNutrients(1, "Green_manure"))
      }
      else if(nutVar$nAppsGreenManure == 0 && num  == 1 ){

        insertUI(selector ="#Green_manure_fertilizer_box_in",
                 where = "afterEnd",
                 ui = drawBoxNutrients(1, "Green_manure"))
      }
      else if(nutVar$nAppsGreenManure > num){
        removeBoxesNutrients(num+1, nutVar$nAppsGreenManure, "Green_manure")
        irrigVar$nAppsGreenManure <- num
      }
      else if(nutVar$nAppsGreenManure < num){
        start <- nutVar$nAppsGreenManure + 1
        for (i in start:num ) {
          insertUI(selector = paste0("#box_nut_Green_manure_", i-1),
                   where = "afterEnd",
                   ui = drawBoxNutrients(i, "Green_manure")
          )
        }
        nutVar$nAppsGreenManure <- num
      }

    }
    else{
      removeBoxesNutrients(1, nutVar$nAppsGreenManure, "Green_manure")
      nutVar$nAppsGreenManure <- 0
    }

  })


  removeBoxesNutrients <- function(begin, end, type){
    for(i in begin:end){
      removeUI(
        selector = paste0("#box_nut_", type, "_", i),
        immediate = T
      )
    }

  }

  drawBoxNutrients <- function(order, type){
    fluidRow(id= paste0("box_nut_", type, "_", order),
             box( title = paste0("Application #", order),
                  width = 12,
                  solidHeader = TRUE, status = "warning",
                  fluidRow(
                    column(width = 6,
                          fluidRow(
                            column(width = 6,
                                   dateInput(paste0("nutrient_start_date_", type, "_", order), label ="Start date", format = "yyyy/mm/dd")
                            ),
                            column(width = 6,
                                   dateInput(paste0("nutrient_end_date_", type, "_", order), label ="End date", format = "yyyy/mm/dd")
                            )
                          ),
                          fluidRow(
                            column(width = 6,
                                   textInput(paste0("nutrient_app_rate_", type, "_", order),  label = "Total application rate for the season", value="")
                            ),
                            column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                   selectizeInput(paste0("nutrient_app_rate_unit", type, "_", order), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                    c("g/sq m",
                                                      "kg/ha",
                                                      "lb/ac")
                                   )
                            )
                          )
                          #textInput(inputId= paste0("nutrient_app_rate_", type, "_", order), label="Total application rate for the season")

                    ),
                    column(width = 6,
                           fluidRow(
                              column(width = 6,
                                     textInput(inputId=paste0("nutrient_recommended_rate_", type, "_", order), label="Total recommended rate")
                              ),

                              column(width = 6,
                                     selectizeInput(paste0("nutrient_recommended_rate_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder="Select one..."), label= "Unit",
                                                    c("g/sq m",
                                                      "kg/ha",
                                                      "lb/ac")
                                      )
                              )
                            ),
                           textInput(paste0("perc_recomm_rate_", type, "_", order), "Percentage of recommended rate applied")
                    )
                  ),
                  h2(paste0("Fertilizer amount applied: ",gsub("_", " ", type))),

                  fluidRow(

                    column(width = 2,
                           fluidRow(
                             column(width = 5, br(),
                                           div(style="text-align:right", h4("Name"))),
                             column(width = 5,
                                    br(),
                                    div(style="text-align:center", h4("# of app"))
                              ),
                             column(width = 2,
                                    br(),
                                    div(style="text-align:center", h4("#")) )
                           )
                    ),
                    column(width = 1,
                           br(),
                           div(style="text-align:center", h4("Start date"))
                    ),
                    column(width = 1,
                           br(),
                           div(style="text-align:center", h4("End date"))
                    ),

                    column(width = 2,

                           column( width= 7,
                                   br(),
                            div(style="text-align:center", h4("Type"))
                           ),
                           column(width = 5,
                                  br(),
                                  div(style="text-align:center", h4("Type (Unit)"))
                            )
                    ),
                    column(width=3,
                           fluidRow(
                             column(width = 6,
                                    br(),
                                    div(style="text-align:center", h4("Technique"))
                             ),
                             column(width = 6,
                                    br(),
                                    div(style="text-align:center", h4("Implement"))
                             )
                           )
                    ),

                     column(width = 1,
                           br(),
                           div(style="text-align:center", h4("Rate"))
                     ),
                     column(width = 1,
                            br(),
                            div(style="text-align:center", h4("Rate (unit)"))
                     ),
                     column(width = 1,
                            br(),
                            div(style="text-align:center", h4("Nutrient Content"))
                     )

                  ),
                  fluidRow(
                    column(width = 2,
                           fluidRow(
                             column(width = 5,
                                    br(),
                                           div(style="text-align:right", "Nitrogen")
                              ),
                             column(width = 5,
                                    selectInput(paste0("nutrientApplied_nit_numApps1_", type, "_", order),"",c(1,2,3))

                              ),
                             column(width = 2,
                                    br(),
                                    div(style="text-align:center", h4("1"))
                              )
                           )
                      ),
                    column(width = 1,
                           dateInput(paste0("fert_nit_start_date1_", type, "_", order), label ="", format = "yyyy/mm/dd")
                    ),

                    column(width = 1,
                           dateInput(paste0("fert_nit_end_date1_", type, "_", order), label ="", format = "yyyy/mm/dd")
                    ),


                    column(width = 2,
                           if(type == "Green_manure"){
                             textInput(paste0("fert_nit_type1_", type, "_", order), "")

                           }
                           else if(type == "Inorganic"){
                             fluidRow(
                             column(width = 7,
                               selectizeInput(paste0("fert_nit_type1_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                 'Ammonium nitrate',
                                 'Ammonium nitrate sulfate',
                                 'Ammonium polyphosphate',
                                 'Ammonium sulfate',
                                 'Anhydrous ammonia',
                                 'Aqua ammonia',
                                 'Calcitic limestone',
                                 'Calcium ammonium nitrate solution',
                                 'Calcium hydroxide',
                                 'Calcium nitrate',
                                 'Diammonium phosphate',
                                 'Dolomitic limestone',
                                 'Liquid phosphoric acid',
                                 'Monoammonium phosphate',
                                 'Potassium chloride',
                                 'Potassium nitrate',
                                 'Potassium sulfate',
                                 'Rock phosphate',
                                 'Single super phosphate',
                                 'Triple super phosphate',
                                 'Urea',
                                 'Urea ammonium nitrate solution',
                                 'Urea super granules',
                                 'NPK fertilizers',
                                 'Other')
                               )
                             ),
                               column(width = 5,
                                      selectizeInput(paste0("fert_nit_type1_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                     c("g/sq m","kg/ha","lb/ac"))

                               ),

                                  column(width=12,
                                  hidden(textInput(paste0("fert_nit_type1_", type, "_", order, "_other"), ""))
                                  )

                             )

                           }
                           else{
                             fluidRow(
                               column(width = 7,
                                 selectizeInput(paste0("fert_nit_type1_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                   "Alfalfa Meal",
                                   "Bagasse",
                                   "Biochar",
                                   "Chicken litter (Poultry manure)",
                                   "Compost",
                                   "Farmyard manure",
                                   "Guano",
                                   "Liquid manure",
                                   "Oil cake",
                                   "Treated sewage sludge",
                                   "Vermicompost",
                                   "Fish fertilizer",
                                   "Other"
                                   )
                                 )
                               ),
                               column(width = 5,
                                      selectizeInput(paste0("fert_nit_type1_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                     c("g/sq m","kg/ha","lb/ac"))
                               ),
                               # conditionalPanel(paste0("input.fert_nit_type1_", type, "_", order, " == 'Other'"),
                               #                  column(width=12,
                               #                  textInput(paste0("fert_nit_type1_other_", type, "_", order), "")
                               #                  )
                               # )
                               column(width=12,
                                      hidden(textInput(paste0("fert_nit_type1_", type, "_", order, "_other"), ""))
                               )

                             )
                            }
                    ),
                    column(width = 3,
                      fluidRow(
                        column(width = 6,
                             selectizeInput(paste0("fertilizer_nit_application_technique1_", type, "_", order), "", multiple = T,
                                            options = list(maxItems = 1, placeholder ="Select one"),
                                            choices = c(
                                                        "Band application on surface",
                                                        "Band application incorporated (Band application beneath surface)",
                                                        "Broadcast surface",
                                                        "Broadcast incorporated",
                                                        "Contact placement (seed placement)",
                                                        "Deep placement",
                                                        "Fertigation",
                                                        "Foliar application",
                                                        "Injection",
                                                        "Placed with seed (seed placement)",
                                                        "Side dressing",
                                                        "Sub-soil placement (injection)",
                                                        "Localized application (using mechanical or hand device)",
                                                        "Other")
                             ),
                             conditionalPanel(paste0("input.fertilizer_nit_application_technique1_", type, "_", order, " == 'Other'"),
                                              textInput(paste0("fertilizer_nit_application_technique1_other_", type, "_", order), "")
                             )
                        ),
                        column(width = 6,
                             selectizeInput(paste0("fertilizer_nit_implement1_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                            choices = c("Backpack sprayer (airblast sprayer)",
                                                        "Boom sprayer",
                                                        "Broadcast spreader",
                                                        "Hand sprayer",
                                                        "Manure spreader",
                                                        "Slurry injector",
                                                        "Manual application",
                                                        "Other")
                             ),
                             conditionalPanel(paste0("input.fertilizer_nit_implement1_", type, "_", order, " == 'Other'"),
                                              textInput(paste0("fertilizer_nit_implement1_other_", type, "_", order), "")
                             )
                        )
                      )
                    ),
                   column(width = 1,
                          textInput(paste0("fert_nit_amountApplied1_", type, "_", order),"")
                   ),
                   column(width = 1,
                          selectizeInput(paste0("fert_nit_amountAppliedScale1_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                         c("g/sq m","kg/ha","lb/ac"))
                   ),
                   column(width = 1,
                          textInput(paste0("fert_nit_nutrientContent1_", type, "_", order),"")
                   )

                  ),
                  conditionalPanel(paste0("input.nutrientApplied_nit_numApps1_", type, "_", order,  " == 2 |
                                   input.nutrientApplied_nit_numApps1_", type, "_", order, " == 3 "),
                                          fluidRow(
                                            column(width = 2,
                                                   fluidRow(
                                                     column(width = 5,
                                                            br(),
                                                            div(style="text-align:right", "")
                                                     ),
                                                     column(width = 5,
                                                            br(),
                                                            div(style="text-align:right", "")

                                                     ),
                                                     column(width = 2,
                                                            br(),
                                                            div(style="text-align:center", h4("2"))
                                                     )
                                                   )
                                            ),
                                            column(width = 1,
                                                   dateInput(paste0("fert_nit_start_date2_", type, "_", order), label ="", format = "yyyy/mm/dd")
                                            ),

                                            column(width = 1,
                                                   dateInput(paste0("fert_nit_end_date2_", type, "_", order), label ="", format = "yyyy/mm/dd")
                                            ),


                                            column(width = 2,
                                                   if(type == "Green_manure"){
                                                     textInput(paste0("fert_nit_type2_", type, "_", order), "")
                                                   }
                                                   else if(type == "Inorganic"){
                                                     fluidRow(
                                                       column(width = 7,

                                                       selectizeInput(paste0("fert_nit_type2_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                         'Ammonium nitrate',
                                                         'Ammonium nitrate sulfate',
                                                         'Ammonium polyphosphate',
                                                         'Ammonium sulfate',
                                                         'Anhydrous ammonia',
                                                         'Aqua ammonia',
                                                         'Calcitic limestone',
                                                         'Calcium ammonium nitrate solution',
                                                         'Calcium hydroxide',
                                                         'Calcium nitrate',
                                                         'Diammonium phosphate',
                                                         'Dolomitic limestone',
                                                         'Liquid phosphoric acid',
                                                         'Monoammonium phosphate',
                                                         'Potassium chloride',
                                                         'Potassium nitrate',
                                                         'Potassium sulfate',
                                                         'Rock phosphate',
                                                         'Single super phosphate',
                                                         'Triple super phosphate',
                                                         'Urea',
                                                         'Urea ammonium nitrate solution',
                                                         'Urea super granules',
                                                         'NPK fertilizers',
                                                         "Other")
                                                       )
                                                        ),
                                                        column(width = 5,
                                                               selectizeInput(paste0("fert_nit_type2_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                              c("g/sq m","kg/ha","lb/ac"))

                                                        ),
                                                       conditionalPanel(paste0("input.fert_nit_type2_", type, "_", order, " == 'Other'"),
                                                                        column(width=12,
                                                                        textInput(paste0("fert_nit_type2_other_", type, "_", order), "")
                                                                        )
                                                       )

                                                      )
                                                   }
                                                   else{
                                                     fluidRow(
                                                       column(width = 7,

                                                         selectizeInput(paste0("fert_nit_type2_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                           "Alfalfa Meal",
                                                           "Bagasse",
                                                           "Biochar",
                                                           "Chicken litter (Poultry manure)",
                                                           "Compost",
                                                           "Farmyard manure",
                                                           "Guano",
                                                           "Liquid manure",
                                                           "Oil cake",
                                                           "Treated sewage sludge",
                                                           "Vermicompost",
                                                           "Fish fertilizer",
                                                           "Other")
                                                         )
                                                       ),
                                                       column(width = 5,
                                                              selectizeInput(paste0("fert_nit_type2_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                             c("g/sq m","kg/ha","lb/ac"))

                                                       ),
                                                       conditionalPanel(paste0("input.fert_nit_type2_", type, "_", order, " == 'Other'"),
                                                                        column(width=12,
                                                                        textInput(paste0("fert_nit_type2_other_", type, "_", order), "")
                                                                        )
                                                       )

                                                     )



                                                     }
                                            ),
                                            column(width =3,
                                              fluidRow(
                                                column(width = 6,
                                                       selectizeInput(paste0("fertilizer_nit_application_technique2_", type, "_", order), "", multiple = T,
                                                                      options = list(maxItems = 1, placeholder ="Select one"),
                                                                      choices = c(
                                                                        "Band application on surface",
                                                                        "Band application incorporated (Band application beneath surface)",
                                                                        "Broadcast surface",
                                                                        "Broadcast incorporated",
                                                                        "Contact placement (seed placement)",
                                                                        "Deep placement",
                                                                        "Fertigation",
                                                                        "Foliar application",
                                                                        "Injection",
                                                                        "Placed with seed (seed placement)",
                                                                        "Side dressing",
                                                                        "Sub-soil placement (injection)",
                                                                        "Localized application (using mechanical or hand device)",
                                                                        "Other")
                                                       ),
                                                       conditionalPanel(paste0("input.fertilizer_nit_application_technique2_", type, "_", order, " == 'Other'"),
                                                                        textInput(paste0("fertilizer_nit_application_technique2_other_", type, "_", order), "")
                                                       )
                                                ),
                                                column(width = 6,
                                                       selectizeInput(paste0("fertilizer_nit_implement2_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                                       choices = c("Backpack sprayer (airblast sprayer)",
                                                                                            "Boom sprayer",
                                                                                            "Broadcast spreader",
                                                                                            "Hand sprayer",
                                                                                            "Manure spreader",
                                                                                            "Slurry injector",
                                                                                            "Manual application",
                                                                                            "Other")
                                                       ),
                                                       conditionalPanel(paste0("input.fertilizer_nit_implement2_", type, "_", order, " == 'Other'"),
                                                                        textInput(paste0("fertilizer_nit_implement2_other_", type, "_", order), "")
                                                       )
                                                )
                                              )
                                            ),
                                           column(width = 1,
                                                  textInput(paste0("fert_nit_amountApplied2_", type, "_", order),"")
                                           ),
                                           column(width = 1,
                                                  selectizeInput(paste0("fert_nit_amountAppliedScale2_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                 c("kg/m2","kg/ha","t/ha"))
                                           ),
                                           column(width = 1,
                                                  textInput(paste0("fert_nit_nutrientContent2_", type, "_", order),"")
                                           )
                                          )

                  ),


                  conditionalPanel(paste0("input.nutrientApplied_nit_numApps1_" , type, "_", order, " == 3 "),
                                   fluidRow(
                                     column(width = 2,
                                            fluidRow(
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")
                                              ),
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")

                                              ),
                                              column(width = 2,
                                                     br(),
                                                     div(style="text-align:center", h4("3"))
                                              )
                                            )
                                     ),
                                     column(width = 1,
                                            dateInput(paste0("fert_nit_start_date3_", type, "_", order), label ="", format = "yyyy/mm/dd")
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_nit_end_date3_", type, "_", order), label ="", format = "yyyy/mm/dd")
                                     ),

                                     column(width = 2,
                                            if(type == "Green_manure"){
                                              textInput(paste0("fert_nit_type3_", type, "_", order), "")
                                            }
                                            else if(type == "Inorganic"){
                                              fluidRow(
                                                column(width = 7,

                                              selectizeInput(paste0("fert_nit_type3_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                'Ammonium nitrate',
                                                'Ammonium nitrate sulfate',
                                                'Ammonium polyphosphate',
                                                'Ammonium sulfate',
                                                'Anhydrous ammonia',
                                                'Aqua ammonia',
                                                'Calcitic limestone',
                                                'Calcium ammonium nitrate solution',
                                                'Calcium hydroxide',
                                                'Calcium nitrate',
                                                'Diammonium phosphate',
                                                'Dolomitic limestone',
                                                'Liquid phosphoric acid',
                                                'Monoammonium phosphate',
                                                'Potassium chloride',
                                                'Potassium nitrate',
                                                'Potassium sulfate',
                                                'Rock phosphate',
                                                'Single super phosphate',
                                                'Triple super phosphate',
                                                'Urea',
                                                'Urea ammonium nitrate solution',
                                                'Urea super granules',
                                                'NPK fertilizers',
                                                "Other")
                                              )
                                                ),
                                              column(width = 5,
                                                     selectizeInput(paste0("fert_nit_type3_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                    c("g/sq m","kg/ha","lb/ac"))

                                              ),
                                              conditionalPanel(paste0("input.fert_nit_type3_", type, "_", order, " == 'Other'"),
                                                               column(width=12,
                                                               textInput(paste0("fert_nit_type3_other_", type, "_", order), "")
                                                               )
                                              )

                                              )

                                            }
                                            else{
                                              fluidRow(
                                                column(width = 7,

                                                  selectizeInput(paste0("fert_nit_type3_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                    "Alfalfa Meal",
                                                    "Bagasse",
                                                    "Biochar",
                                                    "Chicken litter (Poultry manure)",
                                                    "Compost",
                                                    "Farmyard manure",
                                                    "Guano",
                                                    "Liquid manure",
                                                    "Oil cake",
                                                    "Treated sewage sludge",
                                                    "Vermicompost",
                                                    "Fish fertilizer",
                                                    "Other")
                                                  )
                                                ),
                                                column(width = 5,
                                                       selectizeInput(paste0("fert_nit_type3_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                      c("g/sq m","kg/ha","lb/ac"))
                                                ),
                                                conditionalPanel(paste0("input.fert_nit_type3_", type, "_", order, " == 'Other'"),
                                                                 column(width=12,
                                                                 textInput(paste0("fert_nit_type3_other_", type, "_", order), "")
                                                                 )
                                                )

                                              )


                                              }
                                     ),
                                     column(width=3,
                                        fluidRow(
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_nit_application_technique3_", type, "_", order),label = "", multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                                 choices = c(
                                                                   "Band application on surface",
                                                                   "Band application incorporated (Band application beneath surface)",
                                                                   "Broadcast surface",
                                                                   "Broadcast incorporated",
                                                                   "Contact placement (seed placement)",
                                                                   "Deep placement",
                                                                   "Fertigation",
                                                                   "Foliar application",
                                                                   "Injection",
                                                                   "Placed with seed (seed placement)",
                                                                   "Side dressing",
                                                                   "Sub-soil placement (injection)",
                                                                   "Localized application (using mechanical or hand device)",
                                                                   "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_nit_application_technique3_", type, "_", order, " == 'Other'"),
                                                                 textInput(paste0("fertilizer_nit_application_technique3_other_", type, "_", order), "")
                                                )
                                         ),
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_nit_implement3_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                               choices = c("Backpack sprayer (airblast sprayer)",
                                                                           "Boom sprayer",
                                                                           "Broadcast spreader",
                                                                           "Hand sprayer",
                                                                           "Manure spreader",
                                                                           "Slurry injector",
                                                                           "Manual application",
                                                                           "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_nit_implement3_", type, "_", order, " == 'Other'"),
                                                                 textInput(paste0("fertilizer_nit_implement3_other_", type, "_", order), "")
                                                )
                                         )
                                        )
                                     ),

                                    column(width = 1,
                                           textInput(paste0("fert_nit_amountApplied3_", type, "_", order),"")
                                    ),
                                    column(width = 1,
                                           selectizeInput(paste0("fert_nit_amountAppliedScale3_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                          c("kg/m2","kg/ha","t/ha"))
                                    ),
                                    column(width = 1,
                                           textInput(paste0("fert_nit_nutrientContent3_", type, "_", order),"")
                                    )
                                   )


                  ),#end conditional2

                  fluidRow(
                    column(width = 2,
                           fluidRow(
                             column(width = 5,
                                    br(),
                                    div(style="text-align:right", "Phosphorus")
                             ),
                             column(width = 5,
                                    selectInput(paste0("nutrientApplied_phos_numApps1_", type, "_", order),"",c(1,2,3))

                             ),
                             column(width = 2,
                                    br(),
                                    div(style="text-align:center", h4("1"))
                             )
                           )
                    ),
                    column(width = 1,
                           dateInput(paste0("fert_phos_start_date1_", type, "_", order), label ="", format = "yyyy/mm/dd")
                    ),

                    column(width = 1,
                           dateInput(paste0("fert_phos_end_date1_", type, "_", order), label ="", format = "yyyy/mm/dd")
                    ),


                    column(width = 2,
                           if(type == "Green_manure"){
                             textInput(paste0("fert_phos_type1_", type, "_", order), "")
                           }
                           else if(type == "Inorganic"){
                             fluidRow(
                               column(width = 7,

                             selectizeInput(paste0("fert_phos_type1_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                               'Ammonium nitrate',
                               'Ammonium nitrate sulfate',
                               'Ammonium polyphosphate',
                               'Ammonium sulfate',
                               'Anhydrous ammonia',
                               'Aqua ammonia',
                               'Calcitic limestone',
                               'Calcium ammonium nitrate solution',
                               'Calcium hydroxide',
                               'Calcium nitrate',
                               'Diammonium phosphate',
                               'Dolomitic limestone',
                               'Liquid phosphoric acid',
                               'Monoammonium phosphate',
                               'Potassium chloride',
                               'Potassium nitrate',
                               'Potassium sulfate',
                               'Rock phosphate',
                               'Single super phosphate',
                               'Triple super phosphate',
                               'Urea',
                               'Urea ammonium nitrate solution',
                               'Urea super granules',
                               'NPK fertilizers',
                               "Other")
                             )
                               ),
                             column(width = 5,
                                    selectizeInput(paste0("fert_phos_type1_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                   c("g/sq m","kg/ha","lb/ac"))

                             ),
                             conditionalPanel(paste0("input.fert_phos_type1_", type, "_", order, " == 'Other'"),
                                              column(width=12,
                                              textInput(paste0("fert_phos_type1_other_", type, "_", order), "")
                                              )
                             )

                             )

                           }
                           else{
                             fluidRow(
                               column(width = 7,
                                 selectizeInput(paste0("fert_phos_type1_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                   "Alfalfa Meal",
                                   "Bagasse",
                                   "Biochar",
                                   "Chicken litter (Poultry manure)",
                                   "Compost",
                                   "Farmyard manure",
                                   "Guano",
                                   "Liquid manure",
                                   "Oil cake",
                                   "Treated sewage sludge",
                                   "Vermicompost",
                                   "Fish fertilizer",
                                   "Other")
                                 )
                                ),
                                column(width = 5,
                                       selectizeInput(paste0("fert_phos_type1_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                      c("g/sq m","kg/ha","lb/ac"))
                                ),
                               conditionalPanel(paste0("input.fert_phos_type1_", type, "_", order, " == 'Other'"),
                                                column(width=12,
                                                textInput(paste0("fert_phos_type1_other_", type, "_", order), "")
                                                )
                               )

                              )
                             }
                    ),
                    column(width=3 ,
                      fluidRow(
                        column(width = 6,
                               selectizeInput(paste0("fertilizer_phos_application_technique1_", type, "_", order), "", multiple = T,
                                              options = list(maxItems = 1, placeholder ="Select one"),
                                              choices = c(
                                                "Band application on surface",
                                                "Band application incorporated (Band application beneath surface)",
                                                "Broadcast surface",
                                                "Broadcast incorporated",
                                                "Contact placement (seed placement)",
                                                "Deep placement",
                                                "Fertigation",
                                                "Foliar application",
                                                "Injection",
                                                "Placed with seed (seed placement)",
                                                "Side dressing",
                                                "Sub-soil placement (injection)",
                                                "Localized application (using mechanical or hand device)",
                                                "Other")
                               ),
                               conditionalPanel(paste0("input.fertilizer_phos_application_technique1_", type, "_", order, " == 'Other'"),
                                                textInput(paste0("fertilizer_phos_application_technique1_other_", type, "_", order), "")
                               )
                        ),
                        column(width = 6,
                               selectizeInput(paste0("fertilizer_phos_implement1_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                              choices = c("Backpack sprayer (airblast sprayer)",
                                                          "Boom sprayer",
                                                          "Broadcast spreader",
                                                          "Hand sprayer",
                                                          "Manure spreader",
                                                          "Slurry injector",
                                                          "Manual application",
                                                          "Other")
                               ),
                               conditionalPanel(paste0("input.fertilizer_phos_implement1_", type, "_", order, " == 'Other'"),
                                                textInput(paste0("fertilizer_phos_implement1_other_", type, "_", order), "")
                               )
                        )
                      )
                    ),
                   column(width = 1,
                          textInput(paste0("fert_phos_amountApplied1_", type, "_", order),"")
                   ),
                   column(width = 1,
                          selectizeInput(paste0("fert_phos_amountAppliedScale1_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                         c("kg/m2","kg/ha","t/ha"))
                   ),
                   column(width = 1,
                          textInput(paste0("fert_phos_nutrientContent1_", type, "_", order),"")
                   )
                  ),

                  conditionalPanel(paste0("input.nutrientApplied_phos_numApps1_", type, "_", order,  " == 2 |
                                          input.nutrientApplied_phos_numApps1_", type, "_", order, " == 3 "),
                                   fluidRow(
                                     column(width = 2,
                                            fluidRow(
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")
                                              ),
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")

                                              ),
                                              column(width = 2,
                                                     br(),
                                                     div(style="text-align:center", h4("2"))
                                              )
                                            )
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_phos_start_date2_", type, "_", order), label ="", format = "yyyy/mm/dd")
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_phos_end_date2_", type, "_", order), label ="", format = "yyyy/mm/dd")
                                     ),
                                     column(width = 2,
                                            if(type == "Green_manure"){
                                              textInput(paste0("fert_phos_type2_", type, "_", order), "")
                                            }
                                            else if(type == "Inorganic"){
                                              fluidRow(
                                                column(width = 7,

                                              selectizeInput(paste0("fert_phos_type2_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                'Ammonium nitrate',
                                                'Ammonium nitrate sulfate',
                                                'Ammonium polyphosphate',
                                                'Ammonium sulfate',
                                                'Anhydrous ammonia',
                                                'Aqua ammonia',
                                                'Calcitic limestone',
                                                'Calcium ammonium nitrate solution',
                                                'Calcium hydroxide',
                                                'Calcium nitrate',
                                                'Diammonium phosphate',
                                                'Dolomitic limestone',
                                                'Liquid phosphoric acid',
                                                'Monoammonium phosphate',
                                                'Potassium chloride',
                                                'Potassium nitrate',
                                                'Potassium sulfate',
                                                'Rock phosphate',
                                                'Single super phosphate',
                                                'Triple super phosphate',
                                                'Urea',
                                                'Urea ammonium nitrate solution',
                                                'Urea super granules',
                                                'NPK fertilizers',
                                                "Other")
                                              )
                                                ),
                                              column(width = 5,
                                                     selectizeInput(paste0("fert_phos_type2_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                    c("g/sq m","kg/ha","lb/ac"))
                                              ),
                                              conditionalPanel(paste0("input.fert_phos_type2_", type, "_", order, " == 'Other'"),
                                                               column(width=12,
                                                               textInput(paste0("fert_phos_type2_other_", type, "_", order), "")
                                                               )
                                              )
                                            )

                                            }
                                            else{
                                              fluidRow(
                                                column(width = 7,

                                            selectizeInput(paste0("fert_phos_type2_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                  "Alfalfa Meal",
                                                  "Bagasse",
                                                  "Biochar",
                                                  "Chicken litter (Poultry manure)",
                                                  "Compost",
                                                  "Farmyard manure",
                                                  "Guano",
                                                  "Liquid manure",
                                                  "Oil cake",
                                                  "Treated sewage sludge",
                                                  "Vermicompost",
                                                  "Fish fertilizer",
                                                  "Other")
                                                )
                                                ),
                                            column(width = 5,
                                                   selectizeInput(paste0("fert_phos_type2_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                  c("g/sq m","kg/ha","lb/ac"))
                                            ),
                                            conditionalPanel(paste0("input.fert_phos_type2_", type, "_", order, " == 'Other'"),
                                                             column(width=12,
                                                             textInput(paste0("fert_phos_type2_other_", type, "_", order), "")
                                                             )
                                            )

                                              )

                                              }
                                     ),
                                     column(width=3 ,
                                        fluidRow(
                                           column(width =6,
                                                  selectizeInput(paste0("fertilizer_phos_application_technique2_", type, "_", order), "", multiple = T,
                                                                 options = list(maxItems = 1, placeholder ="Select one"),
                                                                 choices = c(
                                                                   "Band application on surface",
                                                                   "Band application incorporated (Band application beneath surface)",
                                                                   "Broadcast surface",
                                                                   "Broadcast incorporated",
                                                                   "Contact placement (seed placement)",
                                                                   "Deep placement",
                                                                   "Fertigation",
                                                                   "Foliar application",
                                                                   "Injection",
                                                                   "Placed with seed (seed placement)",
                                                                   "Side dressing",
                                                                   "Sub-soil placement (injection)",
                                                                   "Localized application (using mechanical or hand device)",
                                                                   "Other")
                                                  ),
                                                  conditionalPanel(paste0("input.fertilizer_phos_application_technique2_", type, "_", order, " == 'Other'"),
                                                                   column(width=12,
                                                                   textInput(paste0("fertilizer_phos_application_technique2_other_", type, "_", order), "")
                                                                   )
                                                  )
                                         ),
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_phos_implement2_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                               choices = c("Backpack sprayer (airblast sprayer)",
                                                                           "Boom sprayer",
                                                                           "Broadcast spreader",
                                                                           "Hand sprayer",
                                                                           "Manure spreader",
                                                                           "Slurry injector",
                                                                           "Manual application",
                                                                           "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_phos_implement2_", type, "_", order, " == 'Other'"),
                                                                   textInput(paste0("fertilizer_phos_implement2_other_", type, "_", order), "")
                                                )
                                         )
                                        )
                                     ),
                                    column(width = 1,
                                           textInput(paste0("fert_phos_amountApplied2_", type, "_", order),"")
                                    ),
                                    column(width = 1,
                                           selectizeInput(paste0("fert_phos_amountAppliedScale2_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                          c("kg/m2","kg/ha","t/ha"))
                                    ),
                                    column(width = 1,
                                           textInput(paste0("fert_phos_nutrientContent2_", type, "_", order),"")
                                    )

                              )

                  ),


                  conditionalPanel(paste0("input.nutrientApplied_phos_numApps1_" , type, "_", order, " == 3 "),
                                   fluidRow(
                                     column(width = 2,
                                            fluidRow(
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")
                                              ),
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")

                                              ),
                                              column(width = 2,
                                                     br(),
                                                     div(style="text-align:center", h4("3"))
                                              )
                                            )
                                     ),
                                     column(width = 1,
                                            dateInput(paste0("fert_phos_start_date3_", type, "_", order), label ="", format = "yyyy/mm/dd")
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_phos_end_date3_", type, "_", order), label ="", format = "yyyy/mm/dd")
                                     ),


                                     column(width = 2,
                                            if(type == "Green_manure"){
                                              textInput(paste0("fert_phos_type3_", type, "_", order), "")
                                            }
                                            else if(type == "Inorganic"){
                                              fluidRow(
                                                column(width = 7,


                                              selectizeInput(paste0("fert_phos_type3_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                'Ammonium nitrate',
                                                'Ammonium nitrate sulfate',
                                                'Ammonium polyphosphate',
                                                'Ammonium sulfate',
                                                'Anhydrous ammonia',
                                                'Aqua ammonia',
                                                'Calcitic limestone',
                                                'Calcium ammonium nitrate solution',
                                                'Calcium hydroxide',
                                                'Calcium nitrate',
                                                'Diammonium phosphate',
                                                'Dolomitic limestone',
                                                'Liquid phosphoric acid',
                                                'Monoammonium phosphate',
                                                'Potassium chloride',
                                                'Potassium nitrate',
                                                'Potassium sulfate',
                                                'Rock phosphate',
                                                'Single super phosphate',
                                                'Triple super phosphate',
                                                'Urea',
                                                'Urea ammonium nitrate solution',
                                                'Urea super granules',
                                                'NPK fertilizers',
                                                "Other")
                                              )
                                                ),
                                              column(width = 5,
                                                     selectizeInput(paste0("fert_phos_type3_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                    c("g/sq m","kg/ha","lb/ac"))

                                              ),
                                              conditionalPanel(paste0("input.fert_phos_type3_", type, "_", order, " == 'Other'"),
                                                               column(width=12,
                                                               textInput(paste0("fert_phos_type3_other_", type, "_", order), "")
                                                               )
                                              )

                                              )

                                            }
                                            else{
                                              fluidRow(
                                                column(width = 7,

                                                selectizeInput(paste0("fert_phos_type3_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                  "Alfalfa Meal",
                                                  "Bagasse",
                                                  "Biochar",
                                                  "Chicken litter (Poultry manure)",
                                                  "Compost",
                                                  "Farmyard manure",
                                                  "Guano",
                                                  "Liquid manure",
                                                  "Oil cake",
                                                  "Treated sewage sludge",
                                                  "Vermicompost",
                                                  "Fish fertilizer",
                                                  "Other")
                                                )
                                                ),
                                                column(width = 5,
                                                       selectizeInput(paste0("fert_phos_type3_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                      c("g/sq m","kg/ha","lb/ac"))
                                                ),
                                                conditionalPanel(paste0("input.fert_phos_type3_", type, "_", order, " == 'Other'"),
                                                                 column(width=12,
                                                                 textInput(paste0("fert_phos_type3_other_", type, "_", order), "")
                                                                 )
                                                )

                                              )

                                            }
                                     ),
                                     column(width=3 ,
                                      fluidRow(
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_phos_application_technique3_", type, "_", order),label = "", multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                               choices = c(
                                                                 "Band application on surface",
                                                                 "Band application incorporated (Band application beneath surface)",
                                                                 "Broadcast surface",
                                                                 "Broadcast incorporated",
                                                                 "Contact placement (seed placement)",
                                                                 "Deep placement",
                                                                 "Fertigation",
                                                                 "Foliar application",
                                                                 "Injection",
                                                                 "Placed with seed (seed placement)",
                                                                 "Side dressing",
                                                                 "Sub-soil placement (injection)",
                                                                 "Localized application (using mechanical or hand device)",
                                                                 "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_phos_application_technique3_", type, "_", order, " == 'Other'"),
                                                                 textInput(paste0("fertilizer_phos_application_technique3_other_", type, "_", order), "")
                                                )
                                         ),
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_phos_implement3_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                               choices = c("Backpack sprayer (airblast sprayer)",
                                                                           "Boom sprayer",
                                                                           "Broadcast spreader",
                                                                           "Hand sprayer",
                                                                           "Manure spreader",
                                                                           "Slurry injector",
                                                                           "Manual application",
                                                                           "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_phos_implement3_", type, "_", order, " == 'Other'"),
                                                                 textInput(paste0("fertilizer_phos_implement3_other_", type, "_", order), "")
                                                )
                                         )
                                      )
                                     ),
                                    column(width = 1,
                                           textInput(paste0("fert_phos_amountApplied3_", type, "_", order),"")
                                    ),
                                    column(width = 1,
                                           selectizeInput(paste0("fert_phos_amountAppliedScale3_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                          c("kg/m2","kg/ha","t/ha"))
                                    ),
                                    column(width = 1,
                                           textInput(paste0("fert_phos_nutrientContent3_", type, "_", order),"")
                                    )

                                )


                  ),#end conditional2


                  fluidRow(
                    column(width = 2,
                           fluidRow(
                             column(width = 5,
                                    br(),
                                    div(style="text-align:right", "Potassium")
                             ),
                             column(width = 5,
                                    selectInput(paste0("nutrientApplied_potas_numApps1_", type, "_", order),"",c(1,2,3))

                             ),
                             column(width = 2,
                                    br(),
                                    div(style="text-align:center", h4("1"))
                             )
                           )
                    ),
                    column(width = 1,
                           dateInput(paste0("fert_potas_start_date1_", type, "_", order), label ="", format = "yyyy/mm/dd")
                    ),

                    column(width = 1,
                           dateInput(paste0("fert_potas_end_date1_", type, "_", order), label ="", format = "yyyy/mm/dd")
                    ),

                    column(width = 2,
                           if(type == "Green_manure"){
                             textInput(paste0("fert_potas_type1_", type, "_", order), "")
                           }
                           else if(type == "Inorganic"){
                             fluidRow(
                               column(width = 7,

                             selectizeInput(paste0("fert_potas_type1_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                               'Ammonium nitrate',
                               'Ammonium nitrate sulfate',
                               'Ammonium polyphosphate',
                               'Ammonium sulfate',
                               'Anhydrous ammonia',
                               'Aqua ammonia',
                               'Calcitic limestone',
                               'Calcium ammonium nitrate solution',
                               'Calcium hydroxide',
                               'Calcium nitrate',
                               'Diammonium phosphate',
                               'Dolomitic limestone',
                               'Liquid phosphoric acid',
                               'Monoammonium phosphate',
                               'Potassium chloride',
                               'Potassium nitrate',
                               'Potassium sulfate',
                               'Rock phosphate',
                               'Single super phosphate',
                               'Triple super phosphate',
                               'Urea',
                               'Urea ammonium nitrate solution',
                               'Urea super granules',
                               'NPK fertilizers',
                               "Other")
                             )
                               ),
                             column(width = 5,
                                    selectizeInput(paste0("fert_potas_type1_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                   c("g/sq m","kg/ha","lb/ac"))
                             ),
                             conditionalPanel(paste0("input.fert_potas_type1_", type, "_", order, " == 'Other'"),
                                              column(width=12,
                                              textInput(paste0("fert_potas_type1_other_", type, "_", order), "")
                                              )
                             )

                             )

                           }
                           else{
                             fluidRow(
                               column(width = 7,

                               selectizeInput(paste0("fert_potas_type1_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                 "Alfalfa Meal",
                                 "Bagasse",
                                 "Biochar",
                                 "Chicken litter (Poultry manure)",
                                 "Compost",
                                 "Farmyard manure",
                                 "Guano",
                                 "Liquid manure",
                                 "Oil cake",
                                 "Treated sewage sludge",
                                 "Vermicompost",
                                 "Fish fertilizer",
                                 "Other")
                               )
                               ),
                               column(width = 5,
                                      selectizeInput(paste0("fert_potas_type1_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                     c("g/sq m","kg/ha","lb/ac"))
                               ),
                               conditionalPanel(paste0("input.fert_potas_type1_", type, "_", order, " == 'Other'"),
                                                column(width=12,
                                                       textInput(paste0("fert_potas_type1_other_", type, "_", order), "")
                                                )
                               )

                             )

                             }
                    ),
                    column(width=3 ,
                      fluidRow(
                          column(width = 6,
                                 selectizeInput(paste0("fertilizer_potas_application_technique1_", type, "_", order), "", multiple = T,
                                                options = list(maxItems = 1, placeholder ="Select one"),
                                                choices = c(
                                                  "Band application on surface",
                                                  "Band application incorporated (Band application beneath surface)",
                                                  "Broadcast surface",
                                                  "Broadcast incorporated",
                                                  "Contact placement (seed placement)",
                                                  "Deep placement",
                                                  "Fertigation",
                                                  "Foliar application",
                                                  "Injection",
                                                  "Placed with seed (seed placement)",
                                                  "Side dressing",
                                                  "Sub-soil placement (injection)",
                                                  "Localized application (using mechanical or hand device)",
                                                  "Other")
                                 ),
                                 conditionalPanel(paste0("input.fertilizer_potas_application_technique1_", type, "_", order, " == 'Other'"),
                                                  textInput(paste0("fertilizer_potas_application_technique1_other_", type, "_", order), "")
                                 )
                          ),
                          column(width = 6,
                                 selectizeInput(paste0("fertilizer_potas_implement1_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                choices = c("Backpack sprayer (airblast sprayer)",
                                                            "Boom sprayer",
                                                            "Broadcast spreader",
                                                            "Hand sprayer",
                                                            "Manure spreader",
                                                            "Slurry injector",
                                                            "Manual application",
                                                            "Other")
                                 ),
                                 conditionalPanel(paste0("input.fertilizer_potas_implement1_", type, "_", order, " == 'Other'"),
                                                  textInput(paste0("fertilizer_potas_implement1__other_", type, "_", order), "")
                                 )

                          )
                        )
                    ),

                   column(width = 1,
                          textInput(paste0("fert_potas_amountApplied1_", type, "_", order),"")
                   ),
                   column(width = 1,
                          selectizeInput(paste0("fert_potas_amountAppliedScale1_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                         c("kg/m2","kg/ha","t/ha"))
                   ),
                   column(width = 1,
                          textInput(paste0("fert_potas_nutrientContent1_", type, "_", order),"")
                   )
                  ),

                  conditionalPanel(paste0("input.nutrientApplied_potas_numApps1_", type, "_", order,  " == 2 |
                                          input.nutrientApplied_potas_numApps1_", type, "_", order, " == 3 "),
                                   fluidRow(
                                     column(width = 2,
                                            fluidRow(
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")
                                              ),
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")

                                              ),
                                              column(width = 2,
                                                     br(),
                                                     div(style="text-align:center", h4("2"))
                                              )
                                            )
                                     ),
                                     column(width = 1,
                                            dateInput(paste0("fert_potas_start_date2_", type, "_", order), label ="", format = "yyyy/mm/dd")
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_potas_end_date2_", type, "_", order), label ="", format = "yyyy/mm/dd")
                                     ),

                                     column(width = 2,
                                            if(type == "Green_manure"){
                                              textInput(paste0("fert_potas_type2_", type, "_", order), "")
                                            }
                                            else if(type == "Inorganic"){
                                              fluidRow(
                                                column(width = 7,

                                              selectizeInput(paste0("fert_potas_type2_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                'Ammonium nitrate',
                                                'Ammonium nitrate sulfate',
                                                'Ammonium polyphosphate',
                                                'Ammonium sulfate',
                                                'Anhydrous ammonia',
                                                'Aqua ammonia',
                                                'Calcitic limestone',
                                                'Calcium ammonium nitrate solution',
                                                'Calcium hydroxide',
                                                'Calcium nitrate',
                                                'Diammonium phosphate',
                                                'Dolomitic limestone',
                                                'Liquid phosphoric acid',
                                                'Monoammonium phosphate',
                                                'Potassium chloride',
                                                'Potassium nitrate',
                                                'Potassium sulfate',
                                                'Rock phosphate',
                                                'Single super phosphate',
                                                'Triple super phosphate',
                                                'Urea',
                                                'Urea ammonium nitrate solution',
                                                'Urea super granules',
                                                'NPK fertilizers',
                                                "Other")
                                              )
                                                ),
                                              column(width = 5,
                                                     selectizeInput(paste0("fert_potas_type2_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                    c("g/sq m","kg/ha","lb/ac"))
                                              ),
                                              conditionalPanel(paste0("input.fert_potas_type2_", type, "_", order, " == 'Other'"),
                                                               column(width=12,
                                                                      textInput(paste0("fert_potas_type2_other_", type, "_", order), "")
                                                               )
                                              )

                                              )
                                            }
                                            else{
                                              fluidRow(
                                                column(width = 7,

                                                  selectizeInput(paste0("fert_potas_type2_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                    "Alfalfa Meal",
                                                    "Bagasse",
                                                    "Biochar",
                                                    "Chicken litter (Poultry manure)",
                                                    "Compost",
                                                    "Farmyard manure",
                                                    "Guano",
                                                    "Liquid manure",
                                                    "Oil cake",
                                                    "Treated sewage sludge",
                                                    "Vermicompost",
                                                    "Fish fertilizer",
                                                    "Other")
                                                  )
                                                ),
                                                column(width = 5,
                                                       selectizeInput(paste0("fert_potas_type2_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                      c("g/sq m","kg/ha","lb/ac"))

                                                ),
                                                conditionalPanel(paste0("input.fert_potas_type2_", type, "_", order, " == 'Other'"),
                                                                 column(width=12,
                                                                        textInput(paste0("fert_potas_type2_other_", type, "_", order), "")
                                                                 )
                                                )

                                              )
                                              }
                                     ),
                                     column(width=3 ,
                                      fluidRow(
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_potas_application_technique2_", type, "_", order), "", multiple = T,
                                                               options = list(maxItems = 1, placeholder ="Select one"),
                                                               choices = c(
                                                                 "Band application on surface",
                                                                 "Band application incorporated (Band application beneath surface)",
                                                                 "Broadcast surface",
                                                                 "Broadcast incorporated",
                                                                 "Contact placement (seed placement)",
                                                                 "Deep placement",
                                                                 "Fertigation",
                                                                 "Foliar application",
                                                                 "Injection",
                                                                 "Placed with seed (seed placement)",
                                                                 "Side dressing",
                                                                 "Sub-soil placement (injection)",
                                                                 "Localized application (using mechanical or hand device)",
                                                                 "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_potas_application_technique2_", type, "_", order, " == 'Other'"),
                                                                 textInput(paste0("fertilizer_potas_application_technique2_other_", type, "_", order), "")
                                                )
                                         ),
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_potas_implement2_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                               choices = c("Backpack sprayer (airblast sprayer)",
                                                                           "Boom sprayer",
                                                                           "Broadcast spreader",
                                                                           "Hand sprayer",
                                                                           "Manure spreader",
                                                                           "Slurry injector",
                                                                           "Manual application",
                                                                           "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_potas_implement2_", type, "_", order, " == 'Other'"),
                                                                 textInput(paste0("fertilizer_potas_implement2__other_", type, "_", order), "")
                                                )
                                         )
                                      )
                                     ),
                                    column(width = 1,
                                           textInput(paste0("fert_potas_amountApplied2_", type, "_", order),"")
                                    ),
                                    column(width = 1,
                                           selectizeInput(paste0("fert_potas_amountAppliedScale2_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                          c("kg/m2","kg/ha","t/ha"))
                                    ),
                                    column(width = 1,
                                           textInput(paste0("fert_potas_nutrientContent2_", type, "_", order),"")
                                    )
                                   )

                  ),


                  conditionalPanel(paste0("input.nutrientApplied_potas_numApps1_" , type, "_", order, " == 3 "),
                                   fluidRow(
                                     column(width = 2,
                                            fluidRow(
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")
                                              ),
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")

                                              ),
                                              column(width = 2,
                                                     br(),
                                                     div(style="text-align:center", h4("3"))
                                              )
                                            )
                                     ),
                                     column(width = 1,
                                            dateInput(paste0("fert_potas_start_date3_", type, "_", order), label ="", format = "yyyy/mm/dd")
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_potas_end_date3_", type, "_", order), label ="", format = "yyyy/mm/dd")
                                     ),

                                     column(width = 2,
                                            if(type == "Green_manure"){
                                              textInput(paste0("fert_potas_type3_", type, "_", order), "")
                                            }
                                            else if(type == "Inorganic"){
                                              fluidRow(
                                                column(width = 7,

                                              selectizeInput(paste0("fert_potas_type3_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                'Ammonium nitrate',
                                                'Ammonium nitrate sulfate',
                                                'Ammonium polyphosphate',
                                                'Ammonium sulfate',
                                                'Anhydrous ammonia',
                                                'Aqua ammonia',
                                                'Calcitic limestone',
                                                'Calcium ammonium nitrate solution',
                                                'Calcium hydroxide',
                                                'Calcium nitrate',
                                                'Diammonium phosphate',
                                                'Dolomitic limestone',
                                                'Liquid phosphoric acid',
                                                'Monoammonium phosphate',
                                                'Potassium chloride',
                                                'Potassium nitrate',
                                                'Potassium sulfate',
                                                'Rock phosphate',
                                                'Single super phosphate',
                                                'Triple super phosphate',
                                                'Urea',
                                                'Urea ammonium nitrate solution',
                                                'Urea super granules',
                                                'NPK fertilizers',
                                                "Other")
                                              )
                                                ),
                                              column(width = 5,
                                                     selectizeInput(paste0("fert_potas_type3_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                    c("g/sq m","kg/ha","lb/ac"))
                                              ),
                                              conditionalPanel(paste0("input.fert_potas_type3_", type, "_", order, " == 'Other'"),
                                                               column(width=12,
                                                                      textInput(paste0("fert_potas_type3_other_", type, "_", order), "")
                                                               )
                                              )
                                            )

                                            }
                                            else{
                                              fluidRow(
                                                column(width = 7,

                                                  selectizeInput(paste0("fert_potas_type3_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                    "Alfalfa Meal",
                                                    "Bagasse",
                                                    "Biochar",
                                                    "Chicken litter (Poultry manure)",
                                                    "Compost",
                                                    "Farmyard manure",
                                                    "Guano",
                                                    "Liquid manure",
                                                    "Oil cake",
                                                    "Treated sewage sludge",
                                                    "Vermicompost",
                                                    "Fish fertilizer",
                                                    "Other")
                                                  )
                                                ),
                                                column(width = 5,
                                                       selectizeInput(paste0("fert_potas_type3_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                      c("g/sq m","kg/ha","lb/ac"))
                                                ),
                                                conditionalPanel(paste0("input.fert_potas_type3_", type, "_", order, " == 'Other'"),
                                                                 column(width=12,
                                                                        textInput(paste0("fert_potas_type3_other_", type, "_", order), "")
                                                                 )
                                                )

                                              )
                                            }
                                     ),
                                     column(width=3 ,
                                      fluidRow(
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_potas_application_technique3_", type, "_", order),label = "", multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                               choices = c(
                                                                 "Band application on surface",
                                                                 "Band application incorporated (Band application beneath surface)",
                                                                 "Broadcast surface",
                                                                 "Broadcast incorporated",
                                                                 "Contact placement (seed placement)",
                                                                 "Deep placement",
                                                                 "Fertigation",
                                                                 "Foliar application",
                                                                 "Injection",
                                                                 "Placed with seed (seed placement)",
                                                                 "Side dressing",
                                                                 "Sub-soil placement (injection)",
                                                                 "Localized application (using mechanical or hand device)",
                                                                 "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_potas_application_technique3_", type, "_", order, " == 'Other'"),
                                                                 textInput(paste0("fertilizer_potas_application_technique3_other_", type, "_", order), "")
                                                )
                                         ),
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_potas_implement3_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                               choices = c("Backpack sprayer (airblast sprayer)",
                                                                           "Boom sprayer",
                                                                           "Broadcast spreader",
                                                                           "Hand sprayer",
                                                                           "Manure spreader",
                                                                           "Slurry injector",
                                                                           "Manual application",
                                                                           "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_potas_implement3_", type, "_", order, " == 'Other'"),
                                                                 textInput(paste0("fertilizer_potas_implement3_other_", type, "_", order), "")
                                                )
                                         )
                                      )
                                     ),
                                     column(width = 1,
                                            textInput(paste0("fert_potas_amountApplied3_", type, "_", order),"")
                                     ),
                                     column(width = 1,
                                            selectizeInput(paste0("fert_potas_amountAppliedScale3_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                           c("kg/m2","kg/ha","t/ha"))
                                     ),
                                     column(width = 1,
                                            textInput(paste0("fert_potas_nutrientContent3_", type, "_", order),"")
                                     )

                              )


                  )#end conditional2

             ))


  }

  ###################### end nutrients #####################################

  ###################### start Other focus #################################

  ### Project management entity
  # observeEvent(input$paste0("projEntity_", count),{
  #   if(input$paste0("projEntity_", count) == "Other"){
  #     session$sendCustomMessage(type="focus",message=paste0("contOtherCenter_", count))
  #   }
  #
  # })


  observeEvent(input$land_impl_type,{
    if(input$land_impl_type == "Other"){
      session$sendCustomMessage(type="focus",message="land_impl_type_other")
    }

  })


  ########### traits table ############################################

  # traitsVals <- reactiveValues()
  # traitsVals$aux <- data.frame()
  # traitsVals$selectedRows <- list()
  # traitsVals$Data <- data.table()
  #
  # dict <- data.frame(stringsAsFactors = FALSE,
  #     c("Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected"),
  #     c('Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Wheat','Wheat','Wheat','Wheat','Wheat','Wheat','Wheat','Wheat','Wheat','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Soybean','Soybean','Soybean','Soybean','Soybean'),
  #     c('Number of tubers planted','Number of emerged plants','Plant emergence proportion','Number of harvested plants','Proportion of plants harvested','Non-marketable tuber number','Tuber number','Tuber number per plant','Number of marketable tubers','Number of marketable tubers per plant','Non-marketable tuber weight','Tuber weight','Tuber weight per plant','Tuber yield no adjusted','Tuber yield adjusted','Marketable tuber weight','Marketable tuber weight per plant','Marketable tuber yield no adjusted','Marketable tuber yield adjusted','Average of tuber weight','Average of marketable tuber weight','Sprouting','Initial Vigor','Plant Stands Harvested','Root Number','Storage root weight','Root Yield','Root Yield','Root Yield','Stem weight','Stem number','Marketable root weight','Non marketable root weight','Number of rotten stem','Storage root weight','Storage root weight','Number of planted stakes','Seedling number','Non marketable root number','Marketable root number','Stock weight','Stem weight','Sprout count','Root Yield','Root Yield','Storage root weight','Storage root weight','Number of stakes','Aboveground biomass at maturity','Grain weight','Grain yield','Grain yield','Grain yield','Grain yield factor','Harvest index','In-season aboveground biomass','In-season aboveground biomass','Grain weight','Grain yield','Grain yield','Grain yield','Grain yield','Grain yield','Grain yield','Shelled cob weight','Grain test weight','Grain weight','Grain weight','Grain yield','Number of plants established','Number of plants planted','Number of plants harvested','Number of plants with storage roots','Number of commercial storage roots','Number of non-commercial storage roots','Total number of root','Total number of root','Weight of commercial storage roots','Weight of non-commercial storage roots','Weight of vines','Total root weight','Marketable root yield','Average commercial root weight','Yield of total roots','Yield of total roots','Percentage of marketable roots','Biomass yield','Relative Storage Root Yield','Storage Root Yield relative to check','Fodder Yield','Fodder Yield','Seed yield','Seed yield','Seed weight'),
  #     c('Count the number of planted tubers and record it','Count the number of emerged plants and record it','Compute the proportion of plants emerged over tubers planted using the formula','Count the number of harvested plants and record it','Compute the proportion of plant harvested over plant emerged using the formula','Count the number of non-marketable tubers per unit area and record it','Compute the total number of tubers per unit area using the formula','Compute the total number of tubers per plant using the formula','Compute the total number of marketable tubers per unit area using the formula','Compute the total number of marketable tubers per plant','Compute the weight of non-marketable tubers per unit area usihg the formula','Compute the total weight of tubers per unit area using the formula','Compute the total weight of tubers per plant using the formula','Compute the total tuber yield no adjusted per unit area using the formula','Compute the total tuber yield adjusted per unit area using the formula','Compute the total weight of marketable tubers per unit area using the formula','Compute the total weight of marketable tubers per plant using the formula','Compute the marketable tuber yield no adjusted per unit area using the formula','Compute the marketable tuber yield adjusted per unit area using the formula','Compute the average tuber weight in grams using the formula','Compute the average marketable tuber weight in grams using the formula','The number of germinated stakes divided by the total number of planted stakes scored one month after planting','Trait monitored by observing plant vigor one month after planting','Count the number of plant stands that are harvested in a plot','','Weigh harvested storage roots per plot at harvest','Calculated as weight of fresh storage roots expressed in tons per hectares per plant at harvest','Dry weight of harvested roots derived by multiplying fresh storage root yield by dry matter content expressed in tons per hectares.','Calculated as weight of foliage and stems expressed in tons per hectares per plot at harvest','Measured stem weight excluding leaves and stump','Count of the number of stems per plot','Measured weight of harvested cassava roots usually classified as large size and medium size excluding small sized roots','','Count of the rotted stems per plot at the time of harvest','Measured weight of cassava root samples (kg) between 4 - 5kg of each of the harvested plot to determine the dry matter by specific gravity','As part of the dry matter determination method by specific gravimetry','Count of the number of stakes planted per plot','Count of the number of emerging seedlings from each family in the pre-nursery done on a daily bases until its ready for transplanting','Count of the number of small or less than 1kg root size','Count of the number of big or more than 1kg root size','Measurement of the fresh weight of the planted part anchoring the storage root(kg)','Measurement of the fresh weight of harvested plant biomass excluding leaves','Count of the number of stakes germinated','Average yield per plant in a plot. It is estimated by dividing the total weight of roots by the number of plants harvested.','Annual root yield using yield per hectare as a function of the crop duration.','Fresh cassava roots are washed in water and weighed on a pan suspended to a weighing scale','This is the weight of peeled cassava roots using a pan suspended to a weighing scale','An estimated number of plantable stakes (about 20cm long','Cut all aboveground biomass in a predetermined area (A). Avoid border effects by sampling away from edges of plot. Biomass as other yield components can be calculated or measured individually (Bell and Fischer, 1994; Reynolds et al., 2001; Pask et al., 2012), decide which method suit better for your objectives.','Dry grains at 70oC and weigh.','Use formulae to calculate grain yield in g/m2','The weight of the grain harvested is registered on a scale, decide which method suit better for your objectives. In breeding trials, a sample area (rather than the whole plot) is generally used for estimating yield. Discard borders when combine harvest for a better estimation of yield.','Calculate grain yield of an entry as percentage over a local check.','Standard method for yield factor','Harvest index is expressed as a ratio and can be calculated as Harvest index = (Grain yield/Biomass).','Sampling is typically performed at sequential developmental stages/time intervals through crop growth. Cut all aboveground biomass in a predetermined area (A). Avoid border effects by sampling away from edges of plot.  In most cases, determinations of dry mass are made on representative sub-samples to reduce oven space requirement, take additional measurements (e.g., fertile culm count) etc. Several protocols available (Bell and Fischer, 1994; Reynolds et al., 2001; Pask et al., 2012), decide which method suit better for your objectives.','Standard method for In-season aboveground biomass.','Count and weigh grains randomly selected from the total grains.','Calculate shelled grain yield per unit area adjusted to 12.5% grain moisture.','Shell the grains (kernels) from the ears harvested per plot and put grains in a paper bag and  dry at 60-70∞C for 1-2 days, then measure and record the weight of dried grain.','Shell the grains (kernels) from the ears harvested per plot and record fresh (field) weight.','It is calculated as the numerical position of the progeny when yields are arrenged from highest to lowest.','Calculated as relative grain yield agaisnst the best check in percentage.','Relative grain yield expressed as percentage of the mean grain yield of the trial. Values above 100% indicate above-average performance; values below 100% indicate below-average performance.','Record shelled cob field weight.','Standard method for grain test weight.','Compute grain weigh adjuted to 12.5% moisture.','Count and weigh grains randomly selected from the total grains.','Shell the grains (kernels) from the ears harvested per plot and record fresh (field) weight.','Counting of established plants.','Counting plants/vines planted.','Visual estimation','Visual estimation','Visual estimation','Visual estimation','Number of commercial plus Number of non-commercial roots','Total number of root per plot / Number of plants harvested','Measured using scales','Measured using scales','Measured using scales','Weight of commercial storage roots plus weight of non-commercial storage roots','(Weight of commercial storage roots/ plot size)*10','(Weight of commercial storage roots/ Number of non-commercial roots','(Weight of commercial storage roots/ plot size)*10','(Weight total of root/ plot size)*10','Number of non-commercial roots/Total number of root after harvest*100','','','','Measure the dry weight of stover, haulm harvested after threshing plants harvested from the plot excluing the borders.','Measure the dry weight of stover, haulm harvested after threshing plants harvested from the plot excluing the borders. Then divide the measured harvested weight by the effectively harvested area of the plot','Weigh the amount of harvested grains (excluding the borders) adjusted to 13% moisture,','Weigh the amount of harvested grains (excluding the borders) adjusted to 13% moisture, and then divided by the area of the plot','Weigh 100 seeds'),
  #     c('tuber/plot-CO_330:0000265','tuber/plot-CO_330:0000268','%-CO_330:0000283','plants/plot-CO_330:0000287','%-CO_330:0000290','tuber/plot-CO_330:0000300','tuber/ plot-CO_330:0000304','tuber /plant-CO_330:0000305','tuber/plot-CO_330:0000293','tuber/plant-CO_330:0000297','kg/plot-CO_330:0000314','kg/plot-CO_330:0000317','kg/plant-CO_330:0000321','t/ha-CO_330:0000324','t/ha-CO_330:0000323','kg/plot-CO_330:0000308','kg/plant-CO_330:0000311','t/ha-CO_330:0000330','t/ha-CO_330:0000327','g-CO_330:0000333','g-CO_330:0000336','ratio-CO_334:0000008','7 pt scale-CO_334:0000009','Plant-CO_334:0000010','Count-CO_334:0000011','kg/plot-CO_334:0000012','t/ha-CO_334:0000013','t/ha-CO_334:0000014','t/ha-CO_334:0000017','kg/pl-CO_334:0000127','Stem-CO_334:0000129','kg/plot-CO_334:0000131','kg/plot-CO_334:0000132','Number-CO_334:0000133','kg/pl-CO_334:0000157','kg/plot-CO_334:0000158','Number-CO_334:0000159','Seedling-CO_334:0000166','plot-CO_334:0000168','plot-CO_334:0000169','kg-CO_334:0000170','kg-CO_334:0000171','1 month-CO_334:0000213,3 months-CO_334:0000214,6 months-CO_334:0000215,9 months-CO_334:0000216','kg/plant-CO_334:0000230','t/ha-CO_334:0000231','kg-CO_334:0000247','kg-CO_334:0000248','Count-CO_334:0000250','m2/kg-CO_321:0001034,kg/ha-CO_321:0001035,t/ha-CO_321:0001036,g/plant-CO_321:0001037,g/plot-CO_321:0001038,kg/plot-CO_321:0001039','g/1000 grain-CO_321:0001213,g/100 grain-CO_321:0001214,g/200 grain-CO_321:0001215','g/m2-CO_321:0001217,kg/ha-CO_321:0001218,t/ha-CO_321:0001219','g/plant-CO_321:0001220,g/plot-CO_321:0001221,kg/plot-CO_321:0001222','%-CO_321:0001223','num-CO_321:0001224','index-CO_321:0001231,%-CO_321:0001232','m2/kg-CO_321:0001246,kg/ha-CO_321:0001247,t/ha-CO_321:0001248,g/plot-CO_321:0001249,kg/plot-CO_321:0001250','1-5 scoring scale-CO_321:0001651','g/1000grain-CO_322:0000723,g/100grain-CO_322:0000725,g/200grain-CO_322:0000727','kg/ha-CO_322:0000730,t/ha-CO_322:0000731','g/plot-CO_322:0000734,kg/plot-CO_322:0000740,t/ha-CO_322:0000742,kg/ha-CO_322:0000737','g/plot-CO_322:0000744,kg/plot-CO_322:0000749,kg/ha-CO_322:0000747,t/ha-CO_322:0000751','%-CO_322:0000757','Rank number-CO_322:0000754','%-CO_322:0000756','g/plot-CO_322:0000928,kg/plot-CO_322:0000931','lb/bsh-CO_322:0001008','g/1000grain-CO_322:0001009,g/100grain-CO_322:0001010,g/200grain-CO_322:0001011','g/200grain-CO_322:0001012,g/1000grain-CO_322:0001013,g/100grain-CO_322:0001014','lb/plot-CO_322:0001016','plants/plot-CO_331:0000192','plants/plot-CO_331:0000678','plants/plot-CO_331:0000679','plants/plot-CO_331:0000211','roots/plot-CO_331:0000214','roots/plot-CO_331:0000217','roots/ plot-CO_331:0000233','roots/ plant-CO_331:0000230','kg/plot-CO_331:0000220','kg/plot-CO_331:0000223','kg/plot-CO_331:0000227','kg/plot-CO_331:0000237','t/ha-CO_331:0000218','t/ha-CO_331:0000680','t/ha-CO_331:0000681','t/ha-CO_331:0000296','%-CO_331:0000682','t/ha-CO_331:0000683','RtYldR 5 pt. scale-CO_331:0000791','%-CO_331:0000792','g/plot-CO_336:0000262','kg/ha-CO_336:0000340','g/plot-CO_336:0000261','kg/ha-CO_336:0000337','g-CO_336:0000333'),
  #     c('CO_330:0000265','CO_330:0000268','CO_330:0000283','CO_330:0000287','CO_330:0000290','CO_330:0000300','CO_330:0000304','CO_330:0000305','CO_330:0000293','CO_330:0000297','CO_330:0000314','CO_330:0000317','CO_330:0000321','CO_330:0000324','CO_330:0000323','CO_330:0000308','CO_330:0000311','CO_330:0000330','CO_330:0000327','CO_330:0000333','CO_330:0000336','CO_334:0000008','CO_334:0000009','CO_334:0000010','CO_334:0000011','CO_334:0000012','CO_334:0000013','CO_334:0000014','CO_334:0000017','CO_334:0000127','CO_334:0000129','CO_334:0000131','CO_334:0000132','CO_334:0000133','CO_334:0000157','CO_334:0000158','CO_334:0000159','CO_334:0000166','CO_334:0000168','CO_334:0000169','CO_334:0000170','CO_334:0000171','CO_334:0000213','CO_334:0000230','CO_334:0000231','CO_334:0000247','CO_334:0000248','CO_334:0000250','CO_321:0001034','CO_321:0001213','CO_321:0001217','CO_321:0001220','CO_321:0001223','CO_321:0001224','CO_321:0001231','CO_321:0001246','CO_321:0001651','CO_322:0000723','CO_322:0000730','CO_322:0000734','CO_322:0000744','CO_322:0000757','CO_322:0000754','CO_322:0000756','CO_322:0000928','CO_322:0001008','CO_322:0001009','CO_322:0001012','CO_322:0001016','CO_331:0000192','CO_331:0000678','CO_331:0000679','CO_331:0000211','CO_331:0000214','CO_331:0000217','CO_331:0000233','CO_331:0000230','CO_331:0000220','CO_331:0000223','CO_331:0000227','CO_331:0000237','CO_331:0000218','CO_331:0000680','CO_331:0000681','CO_331:0000296','CO_331:0000682','CO_331:0000683','CO_331:0000791','CO_331:0000792','CO_336:0000262','CO_336:0000340','CO_336:0000261','CO_336:0000337','CO_336:0000333'),
  #     c('tuber/plot','tuber/plot','%','plants/plot','%','tuber/plot','tuber/ plot','tuber /plant','tuber/plot','tuber/plant','kg/plot','kg/plot','kg/plant','t/ha','t/ha','kg/plot','kg/plant','t/ha','t/ha','g','g','ratio','7 pt scale','Plant','Count','kg/plot','t/ha','t/ha','t/ha','kg/pl','Stem','kg/plot','kg/plot','Number','kg/pl','kg/plot','Number','Seedling','plot','plot','kg','kg','1 month','kg/plant','t/ha','kg','kg','Count','m2/kg','g/1000 grain','g/m2','g/plant','%','num','index','m2/kg','1-5 scoring scale','g/1000grain','kg/ha','g/plot','g/plot','%','Rank number','%','g/plot','lb/bsh','g/1000grain','g/200grain','lb/plot','plants/plot','plants/plot','plants/plot','plants/plot','roots/plot','roots/plot','roots/ plot','roots/ plant','kg/plot','kg/plot','kg/plot','kg/plot','t/ha','t/ha','t/ha','t/ha','%','t/ha','RtYldR 5 pt. scale','%','g/plot','kg/ha','g/plot','kg/ha','g')
  # )
  # colnames(dict) <- c("Status","Crop", "Crop measurement", "Measurement method", "traitCode", "VariableId", "Scale")
  #
  # observe({
  #   if(!is.null(input$cropCommonNameMono)){
  #     traitsVals$selectedRows <- list()
  #     aux <- dplyr::filter(as.data.frame(dict),Crop==input$cropCommonNameMono[1])
  #     traitsVals$Data<-data.table(aux)
  #     output$uiTraitsList <- renderUI({
  #         #column(width=12,
  #             column(12,dataTableOutput("Main_table"),
  #
  #                 tags$script("$(document).on('change', '.selectRow', function () {
  #                     Shiny.onInputChange('selectRowClickId',this.id);
  #                     Shiny.onInputChange('selectRowClickChecked',this.checked);
  #                     Shiny.onInputChange('selectRowClick', Math.random())
  #                     });"
  #                 ),
  #                 tags$script("$(document).on('change', '.select_scale', function () {
  #                         Shiny.onInputChange('selectScaleClickId',this.id);
  #                         Shiny.onInputChange('selectScaleClickValue',this.value);
  #                         Shiny.onInputChange('selectScaleClick', Math.random())
  #                     });"
  #                 )
  #                    )
  #     })
  #
  #   }
  #   else{
  #     traitsVals$Data <- data.table()
  #     traitsVals$selectedRows <- list()
  #     output$uiTraitsList <- renderUI({
  #       column(width = 10,
  #
  #       h4("Select crop to show list of traits.")
  #       )
  #
  #     })
  #
  #   }
  #
  # })#end observe
  #
  #
  # output$Main_table <-renderDataTable({
  #   # as.list(lapply(1:nrow(traitsVals$Data), "drawComboInTable"))
  #
  #   DT= traitsVals$Data
  #   # DT[["Change scale"]] <- as.list(lapply(1:nrow(traitsVals$Data), "drawComboInTable"))
  #   DT[["Change scale"]] <- drawComboInTable()
  #
  #   # DT[["Select variable"]] <- lapply(1:nrow(traitsVals$Data), "drawButtonSelect")
  #   DT[["Select variable"]] <- drawButtonSelect()
  #   DT[["Variable ID"]] <- traitsVals$Data[,6]
  #   datatable(DT,
  #             escape=F,
  #             # selection = list(mode = 'multiple', selected = traitsVals$selectedRows),
  #             selection = list(mode = 'none'),
  #             options = list(
  #               scrollX = TRUE,
  #               pageLength = 25,
  #               columnDefs = list(list(visible=FALSE, targets=c(1,5,6)),list(width = '30%', targets = c(1)), list(className = 'dt-center', targets = c(7,8)))
  #             )
  #   )})
  #
  # observeEvent(input$selectRowClick, {
  #   selectedRow  <- as.numeric(gsub("selectRow_","",input$selectRowClickId))
  #   row <- traitsVals$Data[selectedRow,]
  #   if(input$selectRowClickChecked){
  #     # traitsVals$Data[[1]][selectedRow] <- "<font color='red'><b>Selected</b></font>"
  #     traitsVals$Data[[1]][selectedRow] <- "Selected"
  #   }
  #   else{
  #     # traitsVals$Data[[1]][selectedRow] <- "<font color='black'>Not selected</font>"
  #     traitsVals$Data[[1]][selectedRow] <- "Not selected"
  #
  #   }
  #
  # })
  #
  # observeEvent(input$selectScaleClick,{
  #   vv  <- strsplit(input$selectScaleClickValue, "-")[[1]]
  #   var <- list()
  #   if(length(vv) == 1){
  #     var[[1]] = vv
  #     var[[2]] = ""
  #   }
  #   else{
  #     var <- vv
  #   }
  #
  #   traitsVals$Data[[6]][as.numeric(gsub("select_scale_","",input$selectScaleClickId))]<- var[[1]]
  #   traitsVals$Data[[7]][as.numeric(gsub("select_scale_","",input$selectScaleClickId))]<- var[[2]]
  # })
  #
  # drawButtonSelect <- function(){
  #   n<- nrow(traitsVals$Data)
  #   l <- c()
  #   for(index in 1:n){
  #
  #     old_row <- traitsVals$Data[index,]
  #
  #     ckecked <-  ""
  #     if(old_row[[1]] %like% "Selected"){
  #       ckecked <- "checked"
  #     }
  #
  #     str <-  paste0('<div class="btn-group" role="group" aria-label="Basic example">
  #       <input style="width:100px; background-color:green; color:white;" type="checkbox" class="selectRow"  id=selectRow_',index ,' ',ckecked,  '></input>
  #       </div>')
  #     l<- c(l,str)
  #   }
  #   return(l)
  # }
  #
  # drawComboInTable <- function(){
  #   n<- nrow(traitsVals$Data)
  #   l <- c()
  #   for(index in 1:n){
  #     old_row = traitsVals$Data[index,]
  #     options = old_row[[5]]
  #     str  <- paste0('<select id="select_scale_' , index, '" class="select_scale" style="width:150px;">')
  #     arrOpt <- strsplit(options, ",")[[1]]
  #
  #     if(length(arrOpt) == 1){
  #       str <- ""
  #     }
  #     else{
  #       for(val in arrOpt){
  #         mval  <- strsplit(val, "-")[[1]]
  #         # if(mval[[2]] == old_row[[6]]) sel <- "selected" else sel <-""
  #         #
  #         # str <- paste0(str, '<option value="', mval[[1]], "-" , mval[[2]], '" ', sel,'> ', mval[[2]], '</option>')
  #         if(mval[[1]] == old_row[[7]]) sel <- "selected" else sel <-""
  #
  #         str <- paste0(str, '<option value="', mval[[2]], "-" , mval[[1]], '" ', sel,'> ', mval[[1]], '</option>')
  #       }
  #       str <- paste0(str, "</select>")
  #     }
  #     l <- c(l, str)
  #   }
  #   return(l)
  # }

  ############ end traits table #############################################################

  ### start crop measurement 1 ###

  traitsVals <- reactiveValues()
  traitsVals$aux <- data.frame()
  traitsVals$selectedRows <- list()
  traitsVals$Data <- data.table()

  #dict <- readRDS("/home/obenites/HIDAP_SB_1.0.0/hidap/inst/hidap_agrofims/www/internal_files/crop_measurements_v4.rds")
  dict <- readRDS(paste0(globalpath, "crop_measurements_v4.rds"))
  dict <- as.data.frame(dict, stringsAsFactors=FALSE)

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
    else if(input$croppingType == "Intercrop"){
      crop_selected <- input$cropsSelected
      aux <- dplyr::filter(as.data.frame(dict), Crop %in% crop_selected)
      # if("Other" %in% crop_selected && !is.null(input$jsCropCommonNameOtherVal)){
      if("Other" %in% crop_selected){
        newVal <- trim(cropsVar$varAuxOtherIntercrop)
        # newVal <- trim(input$jsCropCommonNameOtherVal)
        if( newVal == "" ) newVal <- "Other"
        aux$Crop<- gsub("Other", newVal, aux$Crop)
      }
    }

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
//alert("23234234")
                                if($("#" + this.id).is(":enabled")){
                                      Shiny.onInputChange("jsCropCommonNameOtherVal", this.value);
//alert("inter other")
                                      //Shiny.onInputChange("jsCropCommonNameOtherFlag",Math.random());
                                }

})
                                      '
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
      columnDefs = list(list(visible=FALSE, targets=c(1,6)),list(width = '30%', targets = c(1)), list(className = 'dt-center', targets = c(7,8)))
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

  dat <- reactive({traitsVals$Data})

  output$dt <- DT::renderDT(
    dat(),
    server = TRUE,
    escape = FALSE,
    options = list(
      scrollX = TRUE,
      pageLength = 25,
      columnDefs = list(list(visible=FALSE, targets=c(1,6)))
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


  # Design of variables #################################################################
  output$fbDesign_variables <- shiny::renderUI({

    crop <- input$designFieldbook_crop

    if(crop == "potato"){tbl <- table_module_potato } #dataset from fbdesign data folder
    if(crop == "sweetpotato"){tbl <- table_module_sweetpotato } #dataset from fbdesgin data folder

    #mdl <- tbl[tbl$crop == crop, c("module", "module_name")] #HiDAP v1.0 Built_1 (deprecated table form)
    #Filter by crop and select trial abbreviation and trial.
    mdl <- tbl[tbl$CROP == crop, c("TRIAL_ABBR", "TRIAL")] #HiDAP v1.0 Built_2

    mdl <- paste0(mdl[,2], " (", mdl[, 1],")")
    mdl <- sort(unique(mdl))

    #ids <- unlist(stringr::str_extract_all(mdl, "([A-Z]{2})"))
    ids <- str_trim(gsub("\\(.*","", mdl), side = "both")
    vls <- mdl
    mdl <- as.list(ids)
    names(mdl) <- vls
    #mdl1 <<- mdl
    #print(mdl)
    #shiny::selectInput("designFieldbook_module", label = "Assay (fieldbook module):",
    shiny::selectInput("designFieldbook_module", label = "Type of trial",
                       choices = mdl, selected = 1)
  })

  # ID or name of the field book #################################################################
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

  # RenderText for displaying the book's name #################################################################
  output$fbDesign_id <- shiny::renderText({
    fbdesign_id()
  })

  ## Reactive expression for reactive object with agrofims
  trait_agrofims <- reactive({

    trait_index <-  input$Main_table_rows_selected  #data from fbdesign for hidap-agrofims
    if(is.null(trait_index)){
      trait_selected <- data.table::data.table()
    } else {
      dt <- traitsVals$Data
      trait_selected <- dt[trait_index, ]
    }
    trait_selected
  })


  ## Agrofims Tree of traits
  output$designFieldbook_traits_hdagrofims <- shinyTree::renderTree({
    list(
      # root1 = "",
      Agronomic_features = list(
        Groups = list(
          Land_preparation= list(
            Land_levelling =list(
              Land_levelling_start_date = ' ' ,Land_levelling_end_date= '',Land_levelling_number_of_passes= '',Land_levelling_operations_order= '', Land_levelling_implement_picture= '', Land_levelling_implement_type = '', Land_leveling_implement_make= '', Land_leveling_implement_model= '', Land_leveling_implement_traction= ''
            ),
            Puddling= list(Penetrometer_in_field = ' ' ,Puddling_start_date= '',Puddling_end_date= '',Puddling_depth= '',Puddling_implement_picture= '',Puddling_implement_name= '',Puddling_implement_make= '',Puddling_implement_model= '',Puddling_implement_traction= ''
),
            Tillage = list(Conventional_tillage = ' ' ,Mulch_till= '',No_till= '',Other_specify= '',Puddling= '',Reduced_tillage= '',Strip_till= '',Tillage_implement_picture= '',Tillage_implement= '',Implement_make= '',Implement_model= '',Implement_traction= ''
),
            Liming= ''),
          Mulching_and_residue_management=' ' , Rhizobium_inoculation=' ' , Planting_and_Seeding=' ' , Nutrient_management_event=' ' , Irrigation_event=' ' , Disease_observation=' ' , Pest_observation_and_control=' ' , Harvest=' ' , Weather_information=' ' , Water_table_and_quality=' ' , Soil_Measurement=' ' , Crop_measurement=' '
)
        # SubListB = list(leafA = "", leafB = "")
      )
    )
  })


  ## Weather ShinyTree  #################################################################

  output$designFieldbook_weatherVar_agrofims <- shinyTree::renderTree({

    a<- weather_list #data from fbdesign for hidap-agrofims
    a
  })

  ## Soil ShinyTree #################################################################

  output$designFieldbook_soilVar_agrofims <- shinyTree::renderTree({

    a<- soil_list_prov #data from fbdesign for hidap-agrofims
    a
  })



  # SelectInput for split plot designs #################################################################
  output$fbdesign_split_cb <- shiny::renderUI({
    choices <- c(input$factor_name, "INSTN")
    shiny::selectInput("designFieldbook_split_cb",label = "Factor to Plots", choices =  choices, selected= "INSTN")


  })

  # Observed value for geographical information #################################################################

  shiny::observe({
    path <- fbglobal::get_base_dir()
    geodb_file <- "table_sites_agrofims.rds"
    path <- file.path(path, geodb_file)
    x_sites_data <- readRDS(file = path)
    values$sites_data <-  dplyr::filter(x_sites_data, userId==0)
  })


  observeEvent(input$refreshSiteList,{

    path <- fbglobal::get_base_dir()
    geodb_file <- "table_sites_agrofims.rds"
    path <- file.path(path, geodb_file)
    x_sites_data <- readRDS(file = path)

    if(session$userData$logged){
      values$sites_data <- dplyr::filter(x_sites_data, userId==session$userData$userId)
    }
    else{
      values$sites_data <-  dplyr::filter(x_sites_data, userId==0)
    }
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

  # Create an object with the list of file #####################################################################

  #Button for selecting material list
  output$fbDesign_selmlist <- shiny::renderUI({

    input$fdesign_list_refresh
    #res <- sel_list()
    res <- fbdesign_mtl_files() #this come from util.R fbdesign package

    selectizeInput(inputId = "designFieldbook_sel_mlist", label = "Select a factorial list", width="100%",
                   choices = res,
                   options = list(
                     placeholder = 'Select a material list',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))

  })

  #Conditional reactive value for displaying Standard Statistical Design or Genetic Design ########################
  # output$condition_selmlist <-  shiny::reactive({
  #
  #   mlist <- material_table()
  #
  #   #mlist <- input$designFieldbook_sel_mlist
  #   if(is.null(mlist) || mlist == ""){  out <- 0  }
  #   #is_parent <- is_parentList(mlist)
  #   tp <- get_type_list_ds(mlist)
  #   if(tp=="parental") {out <- 1}
  #   if(tp=="clonal")   {out <- 0}
  #   return(out)
  #
  # })

  #The output object will be suspended (not execute) when it is hidden on the web page ############################
  # outputOptions(output, 'condition_selmlist', suspendWhenHidden=FALSE)
  ### End of Create an object with the list of file


  # Number of plant per row (calculated variable) #################################################################
  react_plantxplot <-  shiny::reactive({

    plantxplot <- input$fbDesign_nplantsrow*input$fbDesign_nrowplot
    if(length(plantxplot)==0){plantxplot <- 0}
    plantxplot

  })


  ### Shiny UI for number of plants per plot #################################################################
  output$fbPlant_plot <- shiny::renderUI({

    rpplot <- react_plantxplot()
    shiny::numericInput("fbDesign_nplants",
                        "Number of plants per plot", rpplot , rpplot, rpplot)

  })


  ### Plot Size Values ########################################################################################
  react_psize <- reactive({
    plot_size <- input$fbDesign_nplantsrow*input$fbDesign_distPlants*input$fbDesign_nrowplot*input$fbDesign_distRows
    print(plot_size)
    if(length(plot_size)==0){plot_size <- 0}
    plot_size
  })


  ### Plot Size ###################################################################################
  output$fbPlanting_psize <- shiny::renderUI({
    #plot_size <- input$fbDesign_nplantsrow*input$fbDesign_distPlants*input$fbDesign_nrowplot*input$fbDesign_distRows
    plot_size <- react_psize()
    #if(length(plot_size)==0) plot_size <- 2.7
    shiny::numericInput(inputId = "fbDesign_psize", label = "Plot size (m2)",
                        value = plot_size, min = plot_size,max = plot_size)
  })



  ### Reactive Plant densisty #####################################################################
  react_pdensity <-  shiny::reactive({

    #plant_density <- (input$fbDesign_nplants/input$fbDesign_psize)*10000

    nplantxplot <- react_plantxplot()

    plant_density <- (nplantxplot/input$fbDesign_psize)*10000
    print(plant_density)
    if(length(plant_density)==0){plant_density <- 0}
    plant_density
  })


  ### Select Plant density #########################################################################
  output$fbPlanting_pdensity <- shiny::renderUI({
    plant_density <- react_pdensity()
    #if(length(plant_density)==0) plant_density <- 37037.037
    shiny::numericInput(inputId = "fbDesign_pdensity", label = "Plant density (plants/Ha)",
                        value = plant_density, min = plant_density, max = plant_density)
  })



  ### Message for Alpha Design ####################################################################
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


  ### Material List Export, ##################################################################

  output$fbDesign_mlistExport <- downloadHandler(
    filename = function() {
      paste("Material_List", '.xlsx', sep='')
    },
    content = function(file) {

      mt_list<- crop_template_xlsx$Material_List
      #mt_list <- material_list ##internal dataset
      #       hs <- createStyle(fontColour = "#060505", fontSize=12,
      #                         fontName="Arial Narrow", fgFill = "#4F80BD")
      hs <- createStyle(fontColour = "#000000", fontSize=12,
                        fontName="Calibri", fgFill = "orange")
      openxlsx::write.xlsx(mt_list, file, headerStyle = hs, sheetName="Material_List", colWidths="auto")
    }
  )


   ### Reactive Factor and Levels ############################################################

   ### Factor 1  ############################################################################
   factor1StartDateInputs <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_1
     fl <- list()

     sel1_1 <-	input$sel1_1 #group
     sel1_2<-	input$sel1_2 #subgroup
     sel1_3<-	input$sel1_3 #factor
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_start_date_", 1, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor1EndDateInputs <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_1
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_end_date_", 1, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor1NumericInputLevel <- reactive({
     #nfactors<- input$nfactors_hdafims
     #nlevels <- input$numLevels_1
     fl <- list()
     #print(nlevels)
     #print(p2)
     #for(i in 1:nfactors){
     fl[[1]] <-  input[[paste0("levels_", 1)]] #i = order
     #}
     fl <- unlist(fl)
   })
   factor1TextInputLevel <- reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 1)]] #i = order
     fl <- unlist(fl)

   })
   factor1TextInputUnits <-reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("units_", 1)]] #i = order
     fl <- unlist(fl)

   })
   factor1ComboboxLevel <- reactive({
     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 1)]] #i = order
     fl <- unlist(fl)

   })

   # Gathertin factor 1 inputs
   f1 <- reactive({

     out <- list(factor1StartDateInputs(),  factor1EndDateInputs(),  factor1NumericInputLevel(),  factor1TextInputLevel(),
                 factor1TextInputUnits(), factor1ComboboxLevel())
     names(out) <- c("Start date", "End date", "numeric", "text", "units", "combo")
     out
   })
   ###########################################################################################

   ### Factor 2  ############################################################################
   factor2StartDateInputs  <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_2
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_start_date_", 2, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor2EndDateInputs <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_2
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_end_date_", 2, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor2NumericInputLevel <- reactive({
     #nfactors<- input$nfactors_hdafims
     #nlevels <- input$numLevels_1
     fl <- list()
     #print(nlevels)
     #print(p2)
     #for(i in 1:nfactors){
     fl[[1]] <-  input[[paste0("levels_", 2)]] #i = order
     #}
     fl <- unlist(fl)
   })
   factor2TextInputLevel <- reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 2)]] #i = order
     fl <- unlist(fl)

   })
   factor2TextInputUnits <-reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("units_", 2)]] #i = order
     fl <- unlist(fl)

   })
   factor2ComboboxLevel <- reactive({
     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 2)]] #i = order
     fl <- unlist(fl)

   })

   # Gathertin factor 2 inputs
   f2 <- reactive({

     out <- list(factor2StartDateInputs(),  factor2EndDateInputs(),  factor2NumericInputLevel(),  factor2TextInputLevel(),
                 factor2TextInputUnits(), factor2ComboboxLevel())
     names(out) <- c("Start date", "End date", "numeric", "text", "units", "combo")
     out
   })
   ###########################################################################################

   ### Factor 3  ############################################################################
   factor3StartDateInputs  <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_3
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_start_date_", 3, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor3EndDateInputs <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_3
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_end_date_", 3, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor3NumericInputLevel <- reactive({
     #nfactors<- input$nfactors_hdafims
     #nlevels <- input$numLevels_1
     fl <- list()
     #print(nlevels)
     #print(p2)
     #for(i in 1:nfactors){
     fl[[1]] <-  input[[paste0("levels_", 3)]] #i = order
     #}
     fl <- unlist(fl)
   })
   factor3TextInputLevel <- reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 3)]] #i = order
     fl <- unlist(fl)

   })
   factor3TextInputUnits <-reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("units_", 3)]] #i = order
     fl <- unlist(fl)

   })
   factor3ComboboxLevel <- reactive({
     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 3)]] #i = order
     fl <- unlist(fl)

   })

   f3 <- reactive({

     out <- list(factor3StartDateInputs(),  factor3EndDateInputs(),  factor3NumericInputLevel(),  factor3TextInputLevel(),
                 factor3TextInputUnits(), factor3ComboboxLevel())
     names(out) <- c("Start date", "End date", "numeric", "text", "units", "combo")
     out

   })
   ###########################################################################################

   ### Factor 4  ############################################################################

   factor4StartDateInputs  <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_4
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_start_date_", 4, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor4EndDateInputs <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_4
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_end_date_", 4, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor4NumericInputLevel <- reactive({
     #nfactors<- input$nfactors_hdafims
     #nlevels <- input$numLevels_1
     fl <- list()
     #print(nlevels)
     #print(p2)
     #for(i in 1:nfactors){
     fl[[1]] <-  input[[paste0("levels_", 4)]] #i = order
     #}
     fl <- unlist(fl)
   })
   factor4TextInputLevel <- reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 4)]] #i = order
     fl <- unlist(fl)

   })
   factor4TextInputUnits <-reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("units_", 4)]] #i = order
     fl <- unlist(fl)

   })
   factor4ComboboxLevel <- reactive({
     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 4)]] #i = order
     fl <- unlist(fl)

   })

   f4 <- reactive({

     out <- list(factor4StartDateInputs(),  factor4EndDateInputs(),  factor4NumericInputLevel(),  factor4TextInputLevel(),
                 factor4TextInputUnits(), factor4ComboboxLevel())
     names(out) <- c("Start date", "End date", "numeric", "text", "units", "combo")
     out

   })
   ###########################################################################################

   ### Factor 5  ############################################################################
   factor5StartDateInputs  <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_5
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_start_date_", 5, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor5EndDateInputs <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_5
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_end_date_", 5, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor5NumericInputLevel <- reactive({
     #nfactors<- input$nfactors_hdafims
     #nlevels <- input$numLevels_1
     fl <- list()
     #print(nlevels)
     #print(p2)
     #for(i in 1:nfactors){
     fl[[1]] <-  input[[paste0("levels_", 5)]] #i = order
     #}
     fl <- unlist(fl)
   })
   factor5TextInputLevel <- reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 5)]] #i = order
     fl <- unlist(fl)

   })
   factor5TextInputUnits <-reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("units_", 5)]] #i = order
     fl <- unlist(fl)

   })
   factor5ComboboxLevel <- reactive({
     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 5)]] #i = order
     fl <- unlist(fl)

   })

   f5 <- reactive({

     out <- list(factor5StartDateInputs(),  factor5EndDateInputs(),  factor5NumericInputLevel(),  factor5TextInputLevel(),
                 factor5TextInputUnits(), factor5ComboboxLevel())
     names(out) <- c("Start date", "End date", "numeric", "text", "units", "combo")
     out

   })
   ###########################################################################################

## reactives soil


   ## Soil and Fertility  Design Inputs ##############################################

   #Number of aplications
   nappf1 <- reactive({
     ### #1 Factor
     napp1<- NULL
     if(input$sel1_3 =="Number of fertilizer applications"){
       # Levels
       print(input$numLevels_tabSoil_1)
       for(i in 1:input$numLevels_tabSoil_1){
         # # of applications
         napp1[i] <-  input[[paste0("numApps_tabSoil_factor_1_box_",i)]]
       }
     }
     napp1
   })
   nappf2 <- reactive({
     ### #2 Factor
     napp2<- NULL
     if(input$sel2_3 =="Number of fertilizer applications"){
       # Levels
       for(i in 1:input$numLevels_tabSoil_2){
         # # of applications
         napp2[i] <-  input[[paste0("numApps_tabSoil_factor_2_box_",i)]]
       }
     }
     napp2
   })
   nappf3 <- reactive({
     napp3<- NULL
     if(input$sel3_3 =="Number of fertilizer applications"){
       # Levels
       for(i in 1:input$numLevels_tabSoil_3){
         # # of applications
         napp3[i] <-  input[[paste0("numApps_tabSoil_factor_3_box_",i)]]
       }
     }
     napp3
   })
   nappf4 <- reactive({
     napp4<- NULL
     if(input$sel4_3 =="Number of fertilizer applications"){
       # Levels
       for(i in 1:input$numLevels_tabSoil_4){
         # # of applications
         napp4[i] <-  input[[paste0("numApps_tabSoil_factor_4_box_",i)]]
       }
     }
     napp4
   })
   nappf5 <- reactive({
     napp5<- NULL
     if(input$sel5_3 =="Number of fertilizer applications"){
       # Levels
       for(i in 1:input$numLevels_tabSoil_5){
         # # of applications
         napp5[i] <-  input[[paste0("numApps_tabSoil_factor_5_box_",i)]]
       }
     }
     napp5
   })


   #rate total producto
   rtf1<- reactive({

     if(input$sel1_3 =="Fertilizer product application rate" ||
        input$sel1_3 =="Biofertilizer" ||  input$sel1_3 =="Inorganic"||
        input$sel1_3 =="Green manure"|| input$sel1_3 =="Lime"||
        input$sel1_3 =="Organic"){
       # Levels
       rt1<-NULL
       for(i in 1:input$numLevels_tabSoil_1){
         rt1[i]<- input[[paste0("input_product_RateTotal_factor_1_level_",i)]]
       }
       rt1
     }
   })
   rtf2<- reactive({
     if(input$sel2_3 =="Fertilizer product application rate" ||
        input$sel2_3 =="Biofertilizer" ||  input$sel2_3 =="Inorganic"||
        input$sel2_3 =="Green manure"|| input$sel2_3 =="Lime"||
        input$sel2_3 =="Organic"){
       # Levels
       rt2<-NULL
       for(i in 1:input$numLevels_tabSoil_2){
         rt2[i]<- input[[paste0("input_product_RateTotal_factor_2_level_",i)]]
       }
       rt2
     }
   })
   rtf3<- reactive({
     if(input$sel3_3 =="Fertilizer product application rate" ||
        input$sel3_3 =="Biofertilizer" ||  input$sel3_3 =="Inorganic"||
        input$sel3_3 =="Green manure"|| input$sel3_3 =="Lime"||
        input$sel3_3 =="Organic"){
       # Levels
       rt3<-NULL
       for(i in 1:input$numLevels_tabSoil_3){
         rt3[i]<- input[[paste0("input_product_RateTotal_factor_3_level_",i)]]
       }
       rt3
     }
   })
   rtf4 <- reactive({

     if(input$sel4_3 =="Fertilizer product application rate" ||
        input$sel4_3 =="Biofertilizer" ||  input$sel4_3 =="Inorganic"||
        input$sel4_3 =="Green manure"|| input$sel4_3 =="Lime"||
        input$sel4_3 =="Organic"){
       # Levels
       rt4<-NULL
       for(i in 1:input$numLevels_tabSoil_4){
         rt4[i]<- input[[paste0("input_product_RateTotal_factor_4_level_",i)]]
       }
       rt4
     }
   })
   rtf5<- reactive({

     if(input$sel5_3 =="Fertilizer product application rate" ||
        input$sel5_3 =="Biofertilizer" ||  input$sel5_3 =="Inorganic"||
        input$sel5_3 =="Green manure"|| input$sel5_3 =="Lime"||
        input$sel5_3 =="Organic"){
       # Levels
       rt5<-NULL
       for(i in  1:input$numLevels_tabSoil_5){
         rt5[i]<- input[[paste0("input_product_RateTotal_factor_5_level_",i)]]
       }
       rt5
     }
   })

   #nutrient rate element

   ref1<- reactive({

     if(input$sel1_3 =="Nutrient element application rate"){
       # Levels
       re1<-NULL
       for(i in 1:input$numLevels_tabSoil_1){
         re1[i]<- input[[paste0("input_element_RateTotal_factor_1_level_",i)]]
       }
       re1
     }
   })
   ref2<- reactive({

     if(input$sel2_3 =="Nutrient element application rate"){
       # Levels
       re2<-NULL
       for(i in 1:input$numLevels_tabSoil_2){
         re2[i]<- input[[paste0("input_element_RateTotal_factor_2_level_",i)]]
       }
       re2
     }
   })
   ref3<- reactive({

     if(input$sel3_3 =="Nutrient element application rate"){
       # Levels
       re3<-NULL
       for(i in 1:input$numLevels_tabSoil_3){
         re3[i]<- input[[paste0("input_element_RateTotal_factor_3_level_",i)]]
       }
       re3
     }
   })
   ref4<- reactive({

     if(input$sel4_3 =="Nutrient element application rate"){
       # Levels
       re4<-NULL
       for(i in 1:input$numLevels_tabSoil_4){
         re4[i]<- input[[paste0("input_element_RateTotal_factor_4_level_",i)]]
       }
       re4
     }
     re4
   })
   ref5<- reactive({

     if(input$sel5_3 =="Nutrient element application rate"){
       # Levels
       re5<-NULL
       for(i in 1:input$numLevels_tabSoil_5){
         re5[i]<- input[[paste0("input_element_RateTotal_factor_5_level_",i)]]
       }
       re5
     }
     re5
   })



  # Fieldbook design #########################################################################

   fb_agrofims <- shiny::reactive({

     #Design inputs

     isfullFctl <- input$fullFactorialRB #Is  factorrial?

     design <- input$designFieldbook_agrofims #experimental design
     if(design == "CRD")  { design<- "crd"}
     if(design == "RCBD") { design<- "rcbd"}
     design <- design

     # Non Factorial -------------------------------------------------------------------
     if(isfullFctl == "No"){

       nonf <- as.numeric(input$nfactors_hdafims_n) # n factors non factorial
       nonr <- as.numeric(input$designFieldbook_agrofims_r_n) # replications factors non factorial

       dt <- treatmentValues$data #treatments table
       trt <- dt$TREATMENT #treatments

       if(design=="crd"){
         fb <- st4gi::cr.crd(geno = trt, nrep = nonr, nc = 5)$book #fieldbook
         names(fb) <-  c("PLOT",  "TREATMENT")
         #fb <- fb[,-c(2,3)] #remove row and column headers
       }

       if(design=="rcbd"){
         fb <- st4gi::cr.rcbd(geno = trt, nb = nonr, nc = 5)$book #fieldbook
         names(fb) <-  c("PLOT", "BLOCK", "TREATMENT")
         #fb <- fb[,-c(3,4)] #remove row and column headers
       }

       fb

       # end non full factorial -----------------------------------------------------------

     }

     # Full Factorial -----------------------------------------------------------
     else if(isfullFctl == "Yes" ){

       nf <- as.numeric(input$nfactors_hdafims_y) # n factors yes
       nr <- as.numeric(input$designFieldbook_agrofims_r_y) # n rep yes

       #Factor 1
       gr1 <- input$sel_1_1 ; sgr1<- input$sel_1_2; sf1<-input$sel_1_3  #gr: group, sgr: subgroup, sf: factor of group-subgroup
       numfct1 <- input$numLevels_1 # number of levels
       fct1 <- paste0(gr1, sf1) #factor1 label in the spreadshet
       lvl1 <- input$levelSelection_1 #Factor-levels 1
       f1Inputs <- getTrtInputs(group= input$sel1_1, subgroup = input$sel1_2, fct = input$sel1_3, dfr = f1())
       sf1 <- input$sel1_3
       if(is.null(input$sel1_3)){
         sf1 <- "foo"
       } else{
         sf1 <- input$sel1_3
       }
       if(sf1=="Number of fertilizer applications"){f1Inputs <- list(level = nappf1(), label= sf1 ) }
       if(sf1 =="Fertilizer product application rate" ||
          sf1 =="Biofertilizer" ||  sf1=="Inorganic"||
          sf1 =="Green manure"|| sf1 =="Lime"||
          sf1 =="Organic"){
         f1Inputs <- list(level = rtf1(), label= sf1)
       }
       if(sf1 =="Nutrient element application rate") {f1Inputs <- list(level = ref1(), label= sf1 ) }

       #print("error 1")
       #Factor 2
       gr2 <- input$sel_2_1;  sgr2 <- input$sel_2_2 ; sf2<-input$sel_2_3 #gr: group, sgr: subgroup, sf: factor of group-subgroup
       numfct2 <- input$numLevels_2 # number of levels
       fct2 <- paste0(gr1, sf1) #factor2 label in spreadshet
       lvl2 <- input$levelSelection_2 #Factor-levels 2
       f2Inputs <- getTrtInputs(group= input$sel2_1, subgroup = input$sel2_2, fct = input$sel2_3, dfr = f2())
       if(is.null(input$sel2_3)){
         sf2 <- "foo"
       } else{
         sf2 <- input$sel2_3
       }

       if(sf2=="Number of fertilizer applications"){f2Inputs <- list(level = nappf2(), label= sf2 ) }
       if(sf2 =="Fertilizer product application rate" ||
          sf2 =="Biofertilizer" ||  sf2=="Inorganic"||
          sf2 =="Green manure"|| sf2 =="Lime"||
          sf2 =="Organic"){
         f2Inputs <- list(level = rtf2(), label= sf2)
       }
       if(sf2 =="Nutrient element application rate") {f2Inputs <- list(level = ref2(), label= sf2) }


       #Factor 3
       gr3 <-input$sel_3_1 ; sgr3 <- input$sel_3_2;  sf3 <- input$sel_3_3 #gr: group, sgr: subgroup, sf: factor of group-subgroup
       numfct3 <- input$numLevels_3 # number of levels
       fct3 <- paste0(gr1, sf1) #factor3 label in spreadshet
       lvl3 <- input$levelSelection_3 #Factor-levels 3
       f3Inputs <- getTrtInputs(group= input$sel3_1, subgroup = input$sel3_2, fct = input$sel3_3, dfr = f3())
       if(is.null(input$sel3_3)){
         sf3 <- "foo"
       } else{
         sf3 <- input$sel3_3
       }
       if(sf3=="Number of fertilizer applications"){f3Inputs <- list(level = nappf3(), label= sf3  ) }
       if(sf3 =="Fertilizer product application rate" ||
          sf3 =="Biofertilizer" ||  sf3=="Inorganic"||
          sf3 =="Green manure"|| sf3 =="Lime"||
          sf3 =="Organic"){
         f3Inputs <- list(level = rtf3(), label= sf3 )
       }
       if(sf3 =="Nutrient element application rate") {f3Inputs <- list(level = ref3(), label= sf3 ) }



       #print("error 2")
       #Factor 4
       gr4 <-input$sel_4_1 ; sgr4 <- input$sel_4_2;  sf4 <- input$sel_4_3 #gr: group, sgr: subgroup, sf: factor of group-subgroup
       numfct4 <- input$numLevels_4 # number of levels
       lblfct4 <- paste0(gr4, sf4) #factor4 label in spreadshet
       lvl4 <- input$levelSelection_4#Factor-levels 3
       f4Inputs <- getTrtInputs(group= input$sel4_1, subgroup = input$sel4_2, fct = input$sel4_3, dfr = f4())
       #sf4 <- input$sel4_3
       if(is.null(input$sel4_3)){
         sf4 <- "foo"
       } else{
         sf4 <- input$sel4_3
       }
       if(sf4=="Number of fertilizer applications"){f4Inputs <- list(level = nappf4(), label= sf4 ) }
       if(sf4 =="Fertilizer product application rate" ||
          sf4 =="Biofertilizer" ||  sf4=="Inorganic"||
          sf4 =="Green manure"|| sf4 =="Lime"||
          sf4 =="Organic"){
         f4Inputs <- list(level = rtf4(), label= sf4)
       }
       if(sf4 =="Nutrient element application rate") {f4Inputs <- list(level = ref4(), label= sf4) }


       #Factor 5
       gr5 <-input$sel_5_1;  sgr5 <- input$sel_5_2;  sf5 <- input$sel_5_3 #gr: group, sgr: subgroup, sf: factor of group-subgroup
       numfct5 <- input$numLevels_5 # number of levels
       lblfct5 <- paste0(gr5, sf5) #factor5 label in spreadshet
       lvl5 <- input$levelSelection_5#Factor-levels 3
       f5Inputs <- getTrtInputs(group= input$sel5_1, subgroup = input$sel5_2, fct = input$sel5_3, dfr = f5())
       #sf5 <- input$sel5_3
       if(is.null(input$sel5_3)){
         sf5 <- "foo"
       } else{
         sf5 <- input$sel5_3
       }
       if(sf5=="Number of fertilizer applications"){f5Inputs <- list(level = nappf5(),label= sf5 ) }
       if(sf5 =="Fertilizer product application rate" ||
          sf5 =="Biofertilizer" ||  sf5=="Inorganic"||
          sf5 =="Green manure"|| sf5 =="Lime"||
          sf5 =="Organic"){
         f5Inputs <- list(level = rtf5(), label= sf5)
       }
       if(sf5 =="Nutrient element application rate") {f5Inputs <- list(level = ref5(), label= sf5) }


        print(f1Inputs$level)
        print(f1Inputs$label)
        print(f2Inputs$level)
        print(f2Inputs$label)

       #print("error 3")
       if(nf==2){
         fb <- try(st4gi::cr.f(fnames = c(f1Inputs$label, f2Inputs$label),
                               flevels = list(f1Inputs$level,f2Inputs$level), nrep = nr,
                               design = design, nc = 5)$book)

       }
       else if(nf==3){
         fb <- try(st4gi::cr.f(fnames = c(f1Inputs$label, f2Inputs$label, f3Inputs$label),
                               flevels = list(f1Inputs$level,f2Inputs$level, f3Inputs$level),
                               nrep = nr, design = design, nc = 5)$book)
       }
       else if(nf==4 ){
         fb <- try(st4gi::cr.f(fnames = c(f1Inputs$label, f2Inputs$label),
                               flevels = list(f1Inputs$level,f2Inputs$level, f3Inputs$level, f4Inputs$level),
                               nrep = nr, design = design, nc = 5)$book)
       }
       else if(nf==5){
         fb <- try(st4gi::cr.f(fnames = c(f1Inputs$label, f2Inputs$label),
                               flevels = list(f1Inputs$level,f2Inputs$level, f3Inputs$level, f4Inputs$level, f5Inputs$level),
                               nrep = nr, design = design, nc = 5)$book)
       }
    }

     if(is.element("plot", names(fb))){ colnames(fb)[grep("^plot$", colnames(fb))]<-"PLOT" }
     if(is.element("row", names(fb))){ colnames(fb)[grep("^row$", colnames(fb))]<-"ROW" }
     if(is.element("col", names(fb))){ colnames(fb)[grep("^col$", colnames(fb))]<-"COL" }
     if(is.element("treat", names(fb))) { colnames(fb)[grep("^treat$", colnames(fb))]<-"TREATMENT"}
     if(is.element("block", names(fb))){ colnames(fb)[grep("^block$", colnames(fb))]<-"BLOCK"}

     if(is.element("ROW", names(fb))) {    fb$ROW <- NULL }
     if(is.element("COL", names(fb))) {    fb$COL <- NULL }

     fb
   })

  # Fieldbook with traits #######################################################################
   fb_agrofims_traits <- reactive({


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

   })

 
  ### reactive table for installation info ########################################################
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

  ###############################Agrofeatures ######################################################

  ### Harvest  ##########################################################################
  dt_harvest <- reactive({

     #out <- fb_agrofims()
     #h_start_date	<-	paste(input$harvest_start_date) #dates
     h_start_date	<-	getDateInput(input$harvest_start_date) #dates
     h_end_date	<-	getDateInput(input$harvest_end_date) #dates
     h_cut_height	<-	input$harvest_cut_height
     h_cut_height_unit <- getAgrOper(input$harvest_cut_height_unit) #get units
     h_method <- getAgrOper(feature=input$harvest_method, other = input$harvest_method_value_other)
     h_method_value <- getAgrOper(feature=input$harvest_method_value)
     h_component_harvested <- getAgrOper(input$crop_component_harvested, input$crop_component_harvested_other)
     h_space_rows <- input$space_rows_harvested
     h_space_rows_unit <- getAgrOper(input$space_rows_harvested_unit) #get units
     h_totarea <- input$area_harvested
     h_totarea_unit <- getAgrOper(input$area_harvested_unit) #get units
     h_plants_area <- input$num_plants_area_harvested
     h_notes <- input$harvest_notes
     h_technique <- getAgrOper(input$harvest_technique)
     h_implement	<-	getAgrOper(input$harvest_implement,input$harvest_implement_other)
     h_traction	<-	getAgrOper(input$harvest_traction,input$harvest_traction_other)
     h_amount<- input$amount_harvested
     h_amount_unit <- getAgrOper(input$amount_harvested_unit) #get units

     harvNames <- c('Harvest Start date',
                    'Harvest End date',
                    'Harvest cut height',  'Harvest cut height Unit',
                    'Harvest method',
                    'Harvest method value',
                    'Crop component harvested',
                    'Space between rows harvested','Space between rows harvested Unit',
                    'Total area harvested',  'Total area harvested Unit',
                    'Number of plants in area harvested',
                    'Harvest Technique',
                    'Harvest implement',
                    'Harvest Traction',
                    'Amount harvested', 'Amount harvested Unit',
                    'Harvest Notes'
     )

     dtHarv <- data.frame( h_start_date,
                           h_end_date,
                           h_cut_height,  h_cut_height_unit,
                           h_method,
                           h_method_value,
                           h_component_harvested,
                           h_space_rows,h_space_rows_unit ,
                           h_totarea,h_totarea_unit,
                           h_plants_area,
                           h_technique ,
                           h_implement	,
                           h_traction,
                           h_amount, h_amount_unit,
                           h_notes
     )
     names(dtHarv) <- harvNames
     #out <- merge(out, dtHarv, by = 0, all = TRUE)[-1]
     out <- dtHarv
     out[out=="NULL"]<-NA
     out[out=="NA"]<-NA
out

   })


  ## Irrigation  #########################################################################
  dt_irrigation <- reactive({

   #Irrigation start date
   n <- as.numeric(input$numApplicationsIrrigation)

   irri_start_date <- paste(lapply(1:n, function(x) paste(eval(get_loop_AgrOper("irrigationevent_start_date_", n=n)[[x]])))) #dates
   irri_end_date <- paste(lapply(1:n, function(x) paste( eval(get_loop_AgrOper("irrigationevent_end_date_", n=n)[[x]])))) #dates
   irri_technique <- paste(lapply(1:n, function(x) eval(get_loop_AgrOper("irrigation_technique_",n=n)[[x]])))

   irri_technique_system <- unlist(lapply(1:n, function(x) eval(get_loop_irrigation_technique(irri_technique, module="irrigation")[[x]]) ))

   if(is.null(irri_technique_system)){
     irri_technique_system <- rep("NA", n)
   } else {
     irri_technique_system <- unlist(lapply(1:n, function(x) eval(get_loop_irrigation_technique_other(irri_technique, irri_technique_system)[[x]]) ))
   }


   #irri_technique_system <- unlist(lapply(1:n, function(x) eval(get_loop_irrigation_technique_other(irri_techinque, irri_technique_system)[[x]]) ))

   irri_source <- paste(lapply(1:n, function(x)  eval(get_loop_AgrOper("irrigation_source_", n=n)[[x]])))

   irri_source_dist <- paste(lapply(1:n, function(x)  eval(get_loop_AgrOper("irrigation_source_distance_", n =n)[[x]])))
   irri_source_dist_unit <- paste(lapply(1:n, function(x)  eval(get_loop_AgrOper("irrigation_source_distance_",n =n, label ="unit")[[x]]))) #unit

   irri_source_def <- paste( irri_source_dist, irri_source_dist_unit, sep="_" ) #measure+unit

   irri_amount <- paste(lapply(1:n, function(x)  eval(get_loop_AgrOper("irrigation_amount_", n =n)[[x]])))
   irri_amount_unit <- paste(lapply(1:n, function(x)  eval(get_loop_AgrOper("irrigation_amount_",n =n, label ="unit")[[x]]))) #unit

   irri_amount_def <- paste(irri_amount, irri_amount_unit, sep="_")  #measure+unit
   irri_notes <- paste(lapply(1:n, function(x)  eval(get_loop_AgrOper("irrigation_notes_", n=n)[[x]])))
   irriNames <- c("Number of irrigations",
                  "Irrigation start date (yyyy/mm/dd)" , "Irrigation end date (yyyy/mm/dd)", "Irrigation technique",  "Irrigation technique system",
                  "Irrigation source", "Irrigation source distance",
                  "Irrigation source distance Unit (ft; km; m; mi)", #unit label
                  "Irrigation amount",
                  "Irrigation amount Unit (in; mm)", #unit label
                  "Notes")


   dtIrri<- data.frame(1:n, irri_start_date, irri_end_date,
                       irri_technique,irri_technique_system,
                       irri_source,
                       irri_source_dist,
                       irri_source_dist_unit,
                       irri_amount,
                       irri_amount_unit,
                       irri_notes)

   names(dtIrri) <- irriNames
   dtIrri[dtIrri=="NULL"]<-NA
   dtIrri[dtIrri=="NA"]<-NA
   dtIrri[dtIrri=="character(0)"]<-NA
   dtIrri

})

  ## Land Preparation ####################################################################
  dt_land_description <- reactive({

     flag<-FALSE

     if(input$landLevelling_checkbox==TRUE){

       ll_start_date <- getDateInput(input$landLeveling_start_date) #dates
       ll_end_date <- getDateInput(input$landLeveling_end_date) #dates
       ll_npasses <- getAgrOper(input$numPasses)
       ll_notes <- input$landLeveling_notes
       ll_type <- getAgrOper(input$land_impl_type, input$land_impl_type_other)
       ll_traction <- getAgrOper(input$land_traction, input$land_traction_other)
       lldt <- data.frame(ll_start_date, ll_end_date, ll_npasses, ll_notes, ll_type,  ll_traction)

       llNames<- c("Land levelling start date (yyyy/mm/dd)", "Land levelling end date (yyyy/mm/dd)", "Land levelling Total number of levelling passes", "Land levelling Notes",
                   "Land levelling Type", "Land levelling traction")

       names(lldt) <- llNames
       flag  <- TRUE
       out   <- lldt
     }

     if(input$puddling_checkbox==TRUE){
       lp_start_date <- getDateInput(input$puddling_start_date)
       lp_end_date <- getDateInput(input$puddling_end_date)

       lp_depth_val <- getAgrOper(input$puddling_depth_val)
       lp_depth_unit <- getAgrOper(input$puddling_depth_unit)
       #lp_depth_lbl <- paste("Puddling depth", lp_depth_unit, sep="_") #label

       lp_npasses <- getAgrOper(input$puddling_total_number_puddling_passes)
       lp_notes <- input$puddling_notes
       lp_type <- getAgrOper(input$pud_impl_type, input$pud_impl_type_other)
       lp_traction <- getAgrOper(input$pud_traction, input$pud_traction_other)

       lpNames <- c("Puddling start date (yyyy/mm/dd)", "Puddling end date (yyyy/mm/dd)",
                    "Puddling depth", "Puddling depth Unit (cm; ft; in; m)",
                    "Puddling Total number of puddling passes",
                    "Puddling notes", "Puddling type", "Puddling traction")

       lpdt <- data.frame(lp_start_date, lp_end_date,
                          lp_depth_val,
                          lp_depth_unit, #depth unit
                          lp_npasses, lp_notes, lp_type ,  lp_traction )

       names(lpdt) <- lpNames

       if(flag==TRUE){
         out <- cbind(out, lpdt)
         flag <- TRUE
       }
       else{
         flag <- FALSE
         out  <- lpdt
       }
       flag <- flag
       out  <-out
       out[out=="NULL"]<-NA
       out[out=="NA"]<-NA

     }

     if(input$tillage_checkbox==TRUE){

       lt_start_date <- getDateInput(input$tillage_start_date)
       lt_end_date  <-  getDateInput(input$tillage_end_date)
       lt_technique  <- getAgrOper(input$till_technique, input$till_technique_other)
       lt_depth_method  <- getAgrOper(input$till_depth_method)

       lt_depth  <- getAgrOper(input$tillage_depth)
       lt_depth_unit  <- getAgrOper(input$tillage_depth_unit)
       #lt_depth_lbl <- paste("Tillage depth", lt_depth_unit, sep="_") #label

       lt_npasses  <- getAgrOper(input$total_number_tillage_passes)
       lt_notes  <- input$tillage_notes
       lt_type  <-  getAgrOper(input$till_impl_type, input$till_impl_type_other)
       lt_traction <- getAgrOper(input$till_traction, input$till_traction_other)

       ltNames <- c("Tillage start date (yyyy/mm/dd)", "Tillage end date (yyyy/mm/dd)", "Tillage technique", "Tillage depth measurement method",
                    "Tillage depth", "Tillage depth Unit (cm; ft; in; m)", #unit label
                    "Total number of tillage passes", "Tillage Notes", "Tillage Type", "Tillage Traction")

       ltdt <- data.frame(lt_start_date, lt_end_date, lt_technique, lt_depth_method,
                          lt_depth,lt_depth_unit,
                          lt_npasses ,lt_notes, lt_type, lt_traction )


       names(ltdt) <- ltNames

       if(flag==TRUE){
         out  <- cbind(out, ltdt)
         flag <-TRUE
       }
       else{
         flag<-FALSE
         out <- ltdt
       }
       flag <- flag
       out  <- out
       out[out=="NULL"]<-NA
       out[out=="NA"]<-NA

     }

     out <- out
     out[out=="NULL"]<-NA
     out[out=="NA"]<-NA
     out

   })

  ## Mulching and residue ################################################################
  dt_mulching <- reactive({

     m_start_date <- getDateInput(input$mulch_start_date) #dates
     m_end_date <- getDateInput(input$mulch_end_date) #dates
     m_type <- getAgrOper(input$mulch_type, input$mulch_type_other)
     m_thickness <-  getAgrOper(input$mulch_thickness)
     m_thickness_unit <-  getAgrOper(input$mulch_thickness_unit) #unit
     #m_thickness_lbl<- paste("Mulch thickness", m_thickness_unit, sep= "_") #label
     m_mulchSq  <-  getAgrOper(input$mulch_amountPerSq)
     m_mulchSq_unit  <-  getAgrOper(input$mulch_amountPerSq_unit)#unit
     #m_mulchSq_lbl <- paste("Mulch amount", m_mulchSq_unit, sep= "_")#label
     m_color <- input$mulch_color
     m_cov <- getAgrOper(input$mulch_percCoverage)
     m_cov_unit <-  getAgrOper(input$mulch_percCoverage_unit) #unit
     #m_pgt_lbl <- paste("Percentage of coverage", m_cov_unit, sep= "_")#label
     m_rem_start_date <- getDateInput(input$mulch_remove_start_date)
     m_rem_end_date <- getDateInput(input$mulch_remove_end_date)
     m_mgm_notes <-  getAgrOper(input$mulching_management_notes)
     m_implement <-  getAgrOper(input$mulch_implement_type)
     m_traction <-  getAgrOper(input$mulch_traction, input$mulch_traction_other)

     muNames <- c("Mulch start date (yyyy/mm/dd)" ,"Mulch end date (yyyy/mm/dd)", "Mulch type",
                  "Mulch thickness" ,"Mulch thickness Unit (cm; ft; in; m)",
                  "Mulch amount", "Mulch amount (g/ft2; g/m2; kg/ha; kf/m2: lb/ac)",
                  "Mulch color",
                  "Mulch percent coverage", "Mulch percent coverage Unit (%)",
                  "Mulch removal start date (yyyy/mm/dd)",
                  "Mulch removal end date (yyyy/mm/dd)", "Notes", "Mulch implement type", "Mulching implement traction")

     mudt <- data.frame(m_start_date, m_end_date, m_type,
                        m_thickness, m_thickness_unit , #unit
                        m_mulchSq, m_mulchSq_unit,
                        m_color,
                        m_cov,  m_cov_unit, #unit
                        m_rem_start_date,  m_rem_end_date,
                        m_mgm_notes, m_implement, m_traction )


     names(mudt)<- muNames
     mudt[mudt=="NULL"]<-NA
     mudt[mudt=="NA"]<-NA
     mudt
   })
  dt_residue <- reactive({

     r_start_date<- getDateInput(input$residue_start_date) #dates
     r_end_date<- getDateInput(input$residure_end_date) #dates
     r_part <- getAgrOper(input$residue_plantPart, input$residue_plantPart_other)
     r_technique <- getAgrOper(input$residue_technique, input$residue_technique_other)
     r_traction <- getAgrOper(input$residue_traction, input$residue_traction_other)

     r_thick <- input$crop_residue_thick
     r_thick_unit <- getAgrOper(input$crop_residue_thick_unit)#unit
     #r_thick_lbl <- paste("Crop residue thickness", r_thick_unit, sep="_") #label

     r_amount <- input$crop_residue_amount_sqm
     r_amount_unit <- getAgrOper(input$crop_residue_amount_sqm_unit)#unit
     #r_amount_lbl <- paste("Crop residue amount", r_amount_unit, sep="_")  #label

     r_cov <- getAgrOper(input$crop_residue_perc_cov)
     r_cov_unit <- getAgrOper(input$crop_residue_perc_cov_unit)#unit
     #r_cov_lbl <-  paste("Crop residue percent of coverage", r_amount_unit, sep="_")  #label

     r_depth <- getAgrOper(input$residue_inc_depth)
     r_depth_unit <- getAgrOper(input$residue_inc_depth_unit)#unit
     #r_depth_lbl <- paste("Residue incorporation depth", r_depth_unit,sep="_")  #label

     r_moisture <- getAgrOper(input$crop_residue_moisture)
     r_notes <- input$residue_management_notes

     resNames<- c("Residue management start date (yyyy/mm/dd)", "Residue management end date (yyyy/mm/dd)",
                  "Residue management plant part", "Residue management technique", "Residue management traction implement",
                  "Crop residue thickness", "Crop residue thickness Unit (cm; ft; in; m)",
                  "Crop residue amount", "Crop residue amount Unit (g/ft2; g/m2; kg/ha; kf/m2: lb/ac)",
                  "Crop residue percent coverage", "Crop residue percent coverage Unit (%)",
                  "Residue incorporation depth", "Residue incorporation depth Unit (cm; ft; in; m)",
                  "Crop residue moisture", "Residue notes")

     dtres <- data.frame( r_start_date, r_end_date, r_part, r_technique, r_traction,
                          r_thick, r_thick_unit,
                          r_amount, r_amount_unit,
                          r_cov,r_cov_unit,
                          r_depth,r_depth_unit,
                          r_moisture,
                          r_notes )

     names(dtres)<- resNames
     dtres[dtres=="NULL"]<-NA
     dtres[dtres=="NA"]<-NA

     dtres

   })
  #Total
  dt_mures <- reactive({
    print("mu1")
    if(input$mulchManag_checkbox==TRUE && input$residueManag_checkbox==FALSE){
      print("mu2")
      dt_mr <- dt_mulching()
    } else if(input$mulchManag_checkbox==FALSE && input$residueManag_checkbox==TRUE){
      print("mu3")
      dt_mr <- dt_residue()
    } else if( input$mulchManag_checkbox==TRUE  && input$residueManag_checkbox==TRUE ){
      print("mu4")
      dt_mr <- cbind(dt_mulching(), dt_residue())
    }

    dt_mr[dt_mr=="NULL"]<-NA
    dt_mr[dt_mr=="NA"]<-NA
    dt_mr
  })

  ### Planting   ########################################################################
  dt_directSeed <- reactive({

     # Direct Seeding#################################
     pl_start_date <- getDateInput(input$planting_start_date) #dates
     pl_end_date <- getDateInput(input$planting_end_date) #dates

     pl_env <-  getAgrOper(input$seeding_environment)
     pl_technique <-  getAgrOper(input$seeding_technique)
     pl_trt <-  getAgrOper(input$seed_treatment)

     pl_type <- getAgrOper(input$seeding_implement_type, input$seeding_implement_type_other)
     pl_traction <- getAgrOper(input$seeding_traction,  input$seeding_traction_other)

     pl_row <-  getAgrOper(input$distance_rows)
     pl_row_unit <- getAgrOper(input$distance_rows_unit)
     #pl_row_lbl <- paste("Direct seeding distance between rows",pl_row_unit,sep="_") #label

     pl_rate <- getAgrOper(input$seeding_rate)
     pl_rate_unit <- getAgrOper(input$seeding_rate_unit)
     #pl_rate_lbl <- paste("Direct seeding rate",pl_rate_unit, sep="_") #label

     pl_dist <- getAgrOper(input$distance_plants)
     pl_dist_unit <- getAgrOper(input$distance_plants_unit)
     #pl_dist_lbl<- paste("Direct seeding distance between plants",pl_dist_unit, sep="_") #label

     pl_nrow <- getAgrOper(input$seeding_density_number_rows)

     pl_den <- getAgrOper(input$seeding_plant_density)
     pl_den_unit <- getAgrOper(input$seeding_plant_density_unit)
     #pl_den_lbl <- paste("Direct seeding plant density",pl_den_unit, sep="_") #label

     pl_notes <- getAgrOper(input$direct_seeding_notes)

     plNames<- c("Seeding begin date (yyyy/mm/dd)", "Seeding end environment (yyyy/mm/dd)", "Seeding environment", "Seeding technique",
                 "Seed treatment", "Seeding Type", "Seeding Traction",
                 "Seeding distance between rows",	"Seeding distance between rows Unit (cm; ft; in; m)",
                 "Seeding rate",	"Seeding rate Unit (kg/ha; lb/ac; plants/pot)",
                 "Seeding distance between plants",	"Seeding distance between plants Unit (cm; ft; in; m)",
                 "Seeding number of rows",
                 "Seeding plant density"	,
                 "Seeding plant density Unit (plants/hill; plants/m2; plants/pot; plants/row)",
                 "Seeding density notes")


     dtpl <- data.frame(pl_start_date, pl_end_date, pl_env, pl_technique, pl_trt, pl_type, pl_traction,
                        pl_row,  pl_row_unit,
                        pl_rate, pl_rate_unit,
                        pl_dist, pl_dist_unit,
                        pl_nrow,
                        pl_den, pl_den_unit,
                        pl_notes)

     names(dtpl)<- plNames
     dtpl[dtpl=="NULL"]<-NA
     dtpl[dtpl=="NA"]<-NA
     dtpl
   })
  dt_transPlant <- reactive ({

     #Transplanting  ######################################
     tr_start_date <- getDateInput(input$transplanting_start_date) #dates
     tr_end_date<- getDateInput(input$transplanting_end_date) #dates
     tr_age <- getAgrOper(input$age_seedling)

     tr_env <- getAgrOper(input$transplanting_environment, input$transplanting_environment_other)
     tr_technique <- getAgrOper(input$transplanting_technique, input$transplanting_technique_other)

     tr_traction <- getAgrOper(input$trans_traction,  input$trans_traction_other)

     tr_treatment<- getAgrOper(input$transplanting_treatment)

     tr_row <- getAgrOper(input$trans_distance_rows)
     tr_row_unit <- getAgrOper(input$trans_distance_rows_unit)
     #tr_row_lbl <- paste("Transplanting Distance between rows", tr_row_unit,sep="_")

     tr_den <- getAgrOper(input$trans_seeding_density)
     tr_den_unit <- getAgrOper(input$trans_seeding_density_unit)
     #tr_den_lbl <- paste("Transplanting Seeding density", tr_den_unit, sep="_" )

     tr_nrow <- getAgrOper(input$trans_num_rows)

     tr_distplant  <- getAgrOper(input$trans_distance_plants)
     tr_distplant_unit <- getAgrOper(input$trans_distance_plants_unit)
     #tr_displant_lbl <- paste("Transplanting Distance between plants", tr_distplant_unit, sep="_")

     tr_notes <- input$transplanting_density_notes

     trNames <- c("Transplanting start date (yyyy/mm/dd)", "Transplanting end date (yyyy/mm/dd)", "Transplanting age of seedling (days)",
                  "Transplanting Seedling environment", "Transplanting Technique",
                  "Transplanting Seed treatment", "Transplanting Traction",
                  "Transplanting Distance between rows", "Transplanting distance between rows Unit (cm; ft; in; m)",
                  "Transplanting Seeding density", "Transplanting seedling density Unit (plants/hill; plants/m2; plants/pot; plants/row)",
                  "Transplanting Number of rows",
                  "Transplanting Distance between plants", "Transplanting distance between plants Unit (m)",
                  "Notes")

     dttr<- data.frame( tr_start_date, tr_end_date, tr_age, tr_env, tr_technique,
                        tr_treatment, tr_traction,
                        tr_row, tr_row_unit,
                        tr_den, tr_den_unit,
                        tr_nrow,
                        tr_distplant , tr_distplant_unit,
                        tr_notes)


     names(dttr) <- trNames
     dttr[dttr=="NULL"]<-NA
     dttr[dttr=="NA"]<-NA

     dttr
   })
  #Total
  dt_planting <- reactive({
     if(input$directSeeding_checkbox==TRUE && input$transplanting_checkbox==FALSE){
       dt_plant <- dt_directSeed()
     }else if (input$directSeeding_checkbox==FALSE && input$transplanting_checkbox==TRUE){
       dt_plant <- dt_transPlant()
     } else if(input$directSeeding_checkbox==TRUE && input$transplanting_checkbox==TRUE){
       dt_plant <- cbind(dt_directSeed(),dt_transPlant())
     }
    dt_plant[dt_plant=="NULL"]<-NA
    dt_plant[dt_plant=="NA"]<-NA
     dt_plant
   })


  ## Weeding ##################################################################
  dt_weeding <- reactive({
     n<- input$numWeeding
     weed_start_date <- paste(lapply(1:n, function(x) paste(eval(get_loop_AgrOper("weeding_start_date_", n=n)[[x]]))) ) #dates
     weed_end_date <-   paste(lapply(1:n, function(x) paste(eval(get_loop_AgrOper("weeding_end_date_", n=n)[[x]]))) ) #dates
     weed_techinque <-  paste(lapply(1:n, function(x) eval(get_loop_AgrOper("weeding_technique_",n=n)[[x]])))
     weed_type <- paste(lapply(1:n, function(x) eval(get_loop_AgrOper("weeding_type_",n=n)[[x]])))
     #add other case
     weed_traction <- paste(lapply(1:n, function(x) eval(get_loop_AgrOper("weeding_traction_",n=n)[[x]])))
     #add other case
     weedNames <- c("Number of weedings", "Weeding start date (yyyy/mm/dd)", "Weeding end date (yyyy/mm/dd)",
                    "Weeding technique", "Weeding implment type",
                    "Weeding implement traction")

     dtweed <- data.frame(1:n, weed_start_date, weed_end_date, weed_techinque, weed_type, weed_traction)
     names(dtweed)<- weedNames

     dtweed[dtweed=="NULL"]<-NA
     dtweed[dtweed=="NA"]<-NA

     dtweed

   })


  ## Soil Fertility
  dt_soilFertility <- reactive({

    nsoilFert<- as.numeric(input$soil_fertilizer_num_apps)

    #get_loop_AgrOper(feature = "select_fertilizerType_soil_table_row_",3)
    ferType <- prodType <- prodRate <- ferEle <- ferEleRate <- imple<- traction<- technique<- ferNotes<- NULL
    totProd<- totEle <- startD<- endD<- NULL
    Nprod<- Pprod <- Kprod<- NULL

    for(i in 1:nsoilFert){

      if(is.null(input[[paste0("select_fertilizerType_soil_table_row_", i)]])){
        ferType[i]<-""
      } else{
        ferType[i] <- input[[paste0("select_fertilizerType_soil_table_row_", i)]]
      }
      print("1")
      if(is.null(input[[paste0("select_product_soil_table_row_",i)]])){
        prodType[i]<- ""
      } else if(input[[paste0("select_product_soil_table_row_",i)]]=="Other"  ){
        prodType[i]<- input[[paste0("select_product_soil_table_row_",i,"_other")]]
      } else{
        prodType[i]<- input[[paste0("select_product_soil_table_row_",i)]]
      }
      print("2")
      prodRate[i]<- input[[paste0("input_productRate_soil_table_row",i)]]
      print("3")
      if(is.null(input[[paste0("select_element_soil_table_row_",i)]])){
        ferEle[i]<-  ""
      } else if(input[[paste0("select_element_soil_table_row_", i, "_other")]]=="Other"){
        ferEle[i] <- input[[paste0("select_element_soil_table_row_", i, "_other")]]
      }else {
        ferEle[i] <- input[[paste0("select_element_soil_table_row_",i)]]
      }
      print("4")
      ferEleRate[i] <- input[[paste0("input_elementRate_soil_table_row_",i)]]
      print("5")

      startD[i]<- paste(input[[paste0("input_startdate_soil_table_row_",i)]])
      endD[i]<- paste(input[[paste0("input_enddate_soil_table_row_",i)]])

      # if(is.null(input[[paste0("select_implement_soil_table_row_", i)]])){
      #   imple[i] <- ""
      # } else if (input[[paste0("select_implement_soil_table_row_", i)]]=="Other" ){
      #   imple[i] <-  input[[paste0("select_implement_soil_table_row_",i,"_other")]]
      # } else {
      #   imple[i] <- input[[paste0("select_implement_soil_table_row_", i)]]
      # }

      print("6")

      # if(is.null(input[[paste0("select_traction_soil_table_row_",i)]])){
      #   traction[i] <- ""
      # } else if(input[[paste0("select_traction_soil_table_row_",i)]]=="Other"){
      #   traction[i] <-input[[paste0("select_traction_soil_table_row_",i,"_other")]]
      # } else {
      #   traction[i]<- input[[paste0("select_traction_soil_table_row_",i)]]
      # }
      print("7")
      if(is.null(input[[paste0("select_techinque_soil_table_row_",i)]])){
        technique[i] <- ""
      } else if(input[[paste0("select_techinque_soil_table_row_",i)]]=="Other"){
        technique[i] <- input[[paste0("select_techinque_soil_table_row_",i,"_other")]]
      }else {
        technique[i] <- input[[paste0("select_techinque_soil_table_row_",i)]]
      }
      print("8")
      ferNotes[i]<- input[[paste0("textArea_soil_table_row_",i)]]
    }
    print("9")
    totProd <- rep(input$soil_fertilizer_totalAppRate1, nsoilFert)
    totEle  <- rep(input$soil_fertilizer_totalAppRate2, nsoilFert)
    print("10")
    for(i in 1:nsoilFert){
      if(length(input[[paste0("input_soil_nutrient_product","_N_", prodType[i])]])==0){
        Nprod[i] <-0
      }else {
        Nprod[i] <- input[[paste0("input_soil_nutrient_product","_N_", prodType[i])]]
      }
      if( length(input[[paste0("input_soil_nutrient_product","_P_", prodType[i])]])==0 ){
        Pprod[i] <- 0
      }else {
        Pprod[i] <- input[[paste0("input_soil_nutrient_product","_P_", prodType[i])]]
      }
      if( length(input[[paste0("input_soil_nutrient_product","_K_", prodType[i])]])==0){
        Kprod[i] <-0
      }else {
        Kprod[i] <-input[[paste0("input_soil_nutrient_product","_K_", prodType[i])]]
      }
    }
    print("11")
    soilNames <- c("Number of fertilizer applications","Fertilizer type","Fertilizer product",
                   "Fertilizer product rate (kg/ha)", "Nutrient element","Nutrient element rate (kg/ha)",

                   #"Fertilizer implement type", "Fertilizer traction implement",
                   "Fertilizer start date (yyyy/mm/dd)", "Fertilizer end date (yyyy/mm/dd)",

                   "Fertilizer application technique",
                   "Fertilizer application notes",
                   "Total product calculated application", "Total element calculated application",
                   "N(%)", "P(%)", "K(%)")

    soildt<- data.frame(1:nsoilFert, ferType,  prodType , prodRate , ferEle , ferEleRate ,
                        #imple, traction,
                        startD, endD,
                        technique, ferNotes,
                        totProd, totEle, Nprod, Pprod , Kprod )




    names(soildt) <- soilNames
    soildt[soildt=="NULL"]<-NA
    soildt[soildt=="NA"]<-NA
    soildt

  })


    ################################End agrofeatures ######################################


  ##reactive weather   #################################################################
  dt_weather_agrofims <- shiny::reactive({

    weather_vars <- unlist(shinyTree::get_selected(input$designFieldbook_weatherVar_agrofims))
    weather_vars <- gsub(pattern = ":.*",replacement = "",x = weather_vars)
    weather_vars <- stringr::str_trim(weather_vars,side = "both")

    if(!is.null( weather_vars)){
      dt  <-  matrix(nrow = 50, ncol = length(weather_vars) )
      dt  <- data.frame(dt)
      names(dt)  <-  weather_vars
    } else{
      dt <- data.frame()
    }

    dt
  })


  ##reactive soil  #####################################################################
  dt_soil_agrofims <- shiny::reactive({

    soil_vars <- unlist(shinyTree::get_selected(input$designFieldbook_soilVar_agrofims))

    if(!is.null(soil_vars)){
      dt  <-  matrix(nrow = 50, ncol = length(soil_vars))
      dt  <- data.frame(dt)
      names(dt)  <-  soil_vars
    } else{
      dt <- data.frame()
    }
    dt
  })



  #############  metadata_dt2 ###################################################################


  metadata_dt2 <- reactive({

    expid <- input$experimentId  #c('Experiment ID', input$experimentId)
    expname <- input$experimentName #c('Experiment name', input$experimentName )
    expProjName <- input$experimentProjectName #c('Experiment project name', input$experimentProjectName)
    expBeginDate <- paste(input$fbDesign_project_time_line[1]) #c('Experiment start date', paste(input$fbDesign_project_time_line[1]) )
    expEnDate <- paste(input$fbDesign_project_time_line[2]) #c('Experiment end date', paste(input$fbDesign_project_time_line[2]))

    expTypeExp <- agdesign::getAgrOper(input$designFieldbook_typeExperiment) #type experiment
    expObj <- agdesign::getAgrOper(input$experimentObj) #experiment objective
    expAgType <- NULL #funding Agency Type

    # Funding agency type
    if(is.null(input$designFieldbook_fundAgencyType)){
      expAgType <- ""
      nfundagen<-1
    } #funding Agency Type
    else {
       for(i in 1:length(input$designFieldbook_fundAgencyType)){
         expAgType[i] <- input[[paste0("fundName_", i)]] # Funding agency type
       }
      nfundagen<- length(input$designFieldbook_fundAgencyType)
    }

    #Number of project management entities
    if(is.null(input$numProjEntity)|| is.na(input$numProjEntity)){
      nexpProjEnt <- 1
    } else{
      nexpProjEnt <- as.numeric(input$numProjEntity)
    }
    projEntity <- contCenter<- contCRP<- NULL

    for(i in 1:nexpProjEnt){
      if(is.null(input[[paste0("projEntity_", i)]])){
        projEntity[i] <- ""
        contCenter[i] <- ""
        contCRP[i]    <- ""

      } else if( !is.null(input[[paste0("projEntity_", i)]])  && is.null(input[[paste0("contCenter_", i)]]) ){
        projEntity[i] <- input[[paste0("projEntity_", i)]]
        contCenter[i] <- ""
        contCRP[i] <- ""

      } else if( !is.null(input[[paste0("projEntity_", i)]])  && !is.null(input[[paste0("contCenter_", i)]]) && is.null(input[[paste0("contCRP_",  i)]]) ){
        projEntity[i] <- input[[paste0("projEntity_", i)]] #Project entity
        contCenter[i] <- input[[paste0("contCenter_", i)]] #Contributor center
        contCRP[i] <- ""  #contributor crp

      } else if(input[[paste0("projEntity_", i)]]=="Other"){
        projEntity[i] <- input[[paste0("projEntity_", i,"_other")]]
        contCenter[i] <-  ""
        contCRP[i] <- ""

      } else {

        projEntity[i]<- input[[paste0("projEntity_", i)]] #Project entity
        contCenter[i]<- input[[paste0("contCenter_", i)]] #Contributor center
        contCRP[i]<- input[[paste0("contCRP_",  i)]] #contributor crp
      }
    }

    #Experiment duration
    xdur <- interval(ymd(input$fbDesign_project_time_line[1]),ymd(input$fbDesign_project_time_line[2]))
    xdur <- xdur %/% months(1)
    xdur <- paste(xdur," months", sep = "")
    expDuration <-  xdur

    #Leader
    nexpleads<- as.numeric(input$numLeads)
    projLeadEnt <- tLeadCenter<- expLead<- NULL

    for(i in 1:nexpleads){

     if(is.null(input[[paste0("projLeadEnt_",  i)]])){
        projLeadEnt[i] <- ""
        tLeadCenter[i] <-""
        expLead[i] <- ""
     } else if(input[[paste0("projLeadEnt_",  i)]]=="CGIAR center"){
        projLeadEnt[i] <- input[[paste0("projLeadEnt_",  i)]]
        tLeadCenter[i]  <- input[[paste0("tLeadCenter_", i)]]
        expLead[i] <-   input[[paste0("expLead_",i)]]
     } else {
       if(is.null(input[[paste0("lead_org_type_1_", i)]])){
           projLeadEnt[i]<- ""
           tLeadCenter[i]<- ""
           expLead[i]<- ""
          } else {
           projLeadEnt[i]<- input[[paste0("lead_org_type_1_", i)]]
           tLeadCenter[i]<- input[[paste0("leadNameOther_",i)]]
           expLead[i] <-   input[[paste0("expLead_",i)]]
        }
       }
    }


    if(nexpProjEnt>nexpleads){
      r <- nexpProjEnt - nexpleads
      projLeadEnt <- append(projLeadEnt, rep("", r))
      tLeadCenter <- append(tLeadCenter, rep("", r))
      expLead <- append(expLead, rep("", r))
      nvals<- length(expLead)
    } else if(nexpProjEnt<nexpleads){
      r <- nexpleads - nexpProjEnt
      projEntity <- append(projEntity, rep("", r))
      contCenter <- append(contCenter, rep("", r))
      contCRP <- append(contCRP, rep("", r))
      nvals<- length(contCRP)
    } else {
      nvals <- length(contCRP)
    }

    if(nfundagen!=nvals){
      r <- abs(nvals-nfundagen)
      expAgType <- append(expAgType, rep("", r))
    }


    dt<- data.frame(expid, expname, expProjName, expBeginDate, expEnDate, expDuration,
                    expTypeExp, expObj, expAgType ,projEntity,
                    contCenter, contCRP, projLeadEnt,tLeadCenter, expLead)

    dtNames<- c("Experiment ID","Experiment name", "Experiment project name", "Experiment start date",
                "Experiment end date", "Experiment duration",  "Type of experiment",
                "Experiment objective", "Funding agency type",
                "Project management entity", "Contributor Center", "Contributor CRP",
                "Experiment, lead organization type", "Experiment, lead center", "Experiment lead person / Primary Investigator"
                )

    names(dt) <- dtNames
    dt <- as.data.frame(t(dt))
    out <- tibble::rownames_to_column(dt)
    ncolums<-ncol(out)-1
    names(out) <- c("Factor", paste("Value",1:ncolums, sep=""))
    if(input$numProjEntity==1 && input$numLeads==1){
      out<- out[,-3]
    }
    out

  })
  personnel_dt <- reactive({

    np <- as.numeric(input$npersons)
    vperType <- vperfname <- vperlname <-  vperemail <- vperAff <- vorgName <- vperOrcid <-  c()

    for(i  in 1:np){
      if(is.null(input[[paste0("personnel",i,"Type")]])) vperType <- c(vperType, "")
      else vperType <- c(vperType, input[[paste0("personnel",i,"Type")]])

      vperfname <- c(vperfname, input[[paste0("person",i,"FirstName")]])
      vperlname <- c(vperlname, input[[paste0("person",i,"LastName")]])

      if(is.null(input[[paste0("person",i,"Affiliation")]])) vperAff <- c(vperAff, "")
      else{
        if(input[[paste0("person",i,"Affiliation")]] == "CGIAR Center"){
          if(is.null(input[[paste0("person",i,"Center")]])) vperAff <- c(vperAff, "CGIAR Center")
          else vperAff <- c(vperAff, input[[paste0("person",i,"Center")]])
        }
        else{
          vperAff <- c(vperAff, input[[paste0("person",i,"CenterOther")]])
        }
      }
      #Organization Name
      if(is.null(input[[paste0("person",i,"Center")]])){
        vorgName<- c(vorgName, "")
      }else{ vorgName <- c(vorgName, input[[paste0("person",i,"Center")]])
      }

      vperemail <- c(vperemail, input[[paste0("person",i,"Email")]])
      vperOrcid <- c(vperOrcid, input[[paste0("person",i,"ORCID")]])

    }

    personNames <- c('Person type','Person, first name', 'Person, last name', 'Person, email',
                     'Person, affiliation', 'Person, organization name' , 'Person, ORCID')

    vp<- vp2 <- out <- out2<- NULL
    for(i in 1:np){
      vp  <- c(vperType[i], vperfname[i], vperlname[i], vperemail[i] , vperAff[i], vorgName[i], vperOrcid[i])
      vp2 <- paste(personNames, i)
      out <- append(out, vp)
      out2 <- append(out2, vp2)
    }

    dt_person <- data.frame(out2, out, stringsAsFactors = FALSE)
    names(dt_person) <- c("Factor","Value1")
    print(dt_person)
    dt_person
  })
  crop_dt <- reactive({

    #Crop Type
    cropType <- input$croppingType
    PrevCropName <- getAgrOper(input$prevCropName, input$prevCropName_other)

    if(cropType=="Monocrop"){
      #Monocrop
      CropCommon <- ""
      if(!is.null(input$cropCommonNameMono)) {
        CropCommon <- input$cropCommonNameMono
      } else {
        CropCommon <- ""
      }
      # if(!is.null(input$cropVarietyNameMono)){
      #   CropVarName <-input$cropVarietyNameMono
      # } else {
      #   CropVarName <- ""
      # }
      if(is.null(input$cultivarNameMono)){
        cultivarNameMono <- ""
      }
        if(length(input$cultivarNameMono)==1){
        cultivarNameMono <- paste(input$cultivarNameMono, collapse="")
      }
      if(length(input$cultivarNameMono)>1){
        cultivarNameMono <- paste(input$cultivarNameMono, collapse=", ")
      }

      cNames <- c("Cropping type","Crop common name","Variety name","Previous crop or fallow")
      cVal <- c(cropType,CropCommon, cultivarNameMono, PrevCropName)
      out <- data.frame(cNames, cVal)

    } else {
      #Intercrop
      cropSel<-""
      if(!is.null(input$cropsSelected)){ cropSel<-input$cropsSelected }
      #Crop common name
      CropVarName <- RowCrop <-NULL
      for(i in 1:length(cropSel)){
        #CropVarName[i] <- input[[paste0("cropVarietyName",i)]]
        if(is.null( input[[paste0("cropVarietyName",i)]] )){
          CropVarName[i]<- ""
        }else {
          CropVarName[i] <- paste(input[[paste0("cropVarietyName",i)]], collapse = " ")
        }
        RowCrop[i] <- input[[paste0("intercropValue_row_crop_",i)]]
      }

      if(length(cropSel)==1){
        c1 <- paste(cropSel, collapse = " ")
      } else {
        c1 <- paste(cropSel, collapse = ", ")
      }
      c2 <- paste(CropVarName, collapse = " ")
      if(length(RowCrop)==1){
        c3 <- paste(RowCrop, collapse = "" )
      } else{
        c3 <- paste(RowCrop, collapse = ", ")
      }

      cNames <- c("Cropping type","Crop common name","Variety name","Row geometry","Previous crop or fallow")
      cVal <- c(cropType, c1, c2, c3, PrevCropName)
      out <- data.frame(cNames, cVal)
    }

    names(out) <- c("Factor","Value1")
    out

  })
  fctdsg_dt <- reactive({

    design <- input$designFieldbook_agrofims

    if(input$fullFactorialRB=="Yes"){
      nfactor <- as.numeric(input$nfactors_hdafims_y)
      nrep <- as.numeric(input$designFieldbook_agrofims_r_y)
    } else {
      nfactor <- as.numeric(input$nfactors_hdafims_n)
      nrep <- as.numeric(input$designFieldbook_agrofims_r_n)
    }

    vinfExp <-""
    if(!is.null(input$info_experiment_unit)) vinfExp <- input$info_experiment_unit

    c1 <- c('Information on experimental unit',vinfExp )

    vfarea <-""
    vfexpmaxwidth <- ""
    vfexpmaxlength <- ""
    vpdiam <- ""
    vpdpth <- ""
    if(vinfExp == "plot"  ){
      vfarea <- vinfExp
      wunit <- ""
      lunit <- ""
      if(!is.null(input$expt_plot_width_unit))  wunit <- input$expt_plot_width_unit
      if(!is.null(input$expt_plot_length_unit))  lunit <- input$expt_plot_length_unit
      vfexpmaxwidth <- paste0(input$expt_plot_width, " " , wunit)
      vfexpmaxlength <- paste0(input$expt_plot_length, " " , lunit)
    }
    else if(vinfExp == "field"){
      vfarea <- vinfExp
      wunit <- ""
      lunit <- ""
      if(!is.null(input$expt_field_width_unit))  wunit <- input$expt_field_width_unit
      if(!is.null(input$expt_field_length_unit))  lunit <- input$expt_field_length_unit
      vfexpmaxwidth <- paste0(input$expt_field_width, " " , wunit)
      vfexpmaxlength <- paste0(input$expt_field_length, " " , lunit)
    }
    else if(vinfExp == "pot"){
      wunit <- ""
      lunit <- ""
      if(!is.null(input$pot_diameter_unit))  wunit <- input$pot_diameter_unit
      if(!is.null(input$pot_depth_unit))  lunit <- input$pot_depth_unit
      vpdiam <- paste0(input$pot_diameter, " " , wunit)
      vpdpth <- paste0(input$pot_depth, " " , lunit)

    }

    if(vinfExp != "pot"){

      c5 <- c( paste('Experimental ', vinfExp  ,' width',sep= ""), vfexpmaxwidth)
      c6 <- c(paste('Experimental ', vinfExp ,' length',sep=""), vfexpmaxlength)
    } else {
      c5 <- c('Pot diameter',vpdiam )
      c6 <- c('Pot depth',vpdpth )
    }

    if(input$designFieldbook_agrofims=="RCBD"){ td<-"Randomized Complete Block Design"   }
    if(input$designFieldbook_agrofims=="CRD"){ td<-"Completely Randomized Design"   }
    c7 <- c('Experimental design', td)
    c8 <- c('Experimental design abbreviation', input$designFieldbook_agrofims)
    c9 <- c('Number of replications', nrep)
    c40 <- c('Number of factors', nfactor)

    levels1 <- c("NA", "NA", "NA", "NA","NA")
    levels2 <- c("NA", "NA", "NA", "NA","NA")
    levels3 <- c("NA", "NA", "NA", "NA","NA")
    levels4 <- c("NA", "NA", "NA", "NA","NA")
    levels5 <- c("NA", "NA", "NA", "NA","NA")
    levelsDt <- data.table(levels1,levels2,levels3,levels4,levels5)


    nf <- nfactor

    factors <- c("NA", "NA", "NA", "NA","NA")
    for(i in 1:nf){
      g1 <- input[[paste0("sel" , i, "_2" )]]
      g2 <- input[[paste0("sel" , i, "_3" )]]
      if(!is.null(g1) && !is.null(g2)){
        factors[i] <- paste0(g1, " ", g2)

        g3 <- input[[paste0("sel" , i, "_3" )]]
        ls1 <- input[[paste0("numLevels_", i)]]
        if(is_numeric(ls1) && !is.null(g3)){
          if (ls1>5) ls1 <- 5 #max5
          if(g3 %like% "date"){
            for(j in 1:ls1){
              sdate <- input[[paste0("factor_start_date_",i, "_", j)]]
              edate <- input[[paste0("factor_end_date_",i, "_", j)]]
              levelsDt[i,j] <- paste0(sdate, " - ", edate)
            }
          }
          else{
            nl <- input[[paste0("levels_",i)]]
            count <- 1
            for(lv in nl){
              if(count <= 5){
                if(is.null(input[[paste0("funits_", i)]])){
                  levelsDt[i,count] <- lv
                }
                else{
                  levelsDt[i,count]<- paste0(lv, " ", input[[paste0("funits_", i)]])
                }
              }
              count <- count + 1
            }
          }
        }

      }

    }

      if(is.null(input$sel1_1)|| input$sel1_1=="Soil fertility" ){
        p1<-""
      } else{
        p1<-"foo"
      }
    if(is.null(input$sel2_1)|| input$sel2_1=="Soil fertility" ){
      p2<-""
    } else{
      p2<-"foo"
    }

    if(is.null(input$sel3_1)|| input$sel3_1=="Soil fertility" ){
      p3<-""
    } else{
      p3<-"foo"
    }

    if(is.null(input$sel4_1)|| input$sel4_1=="Soil fertility" ){
      p4<-""
    } else{
      p4<-"foo"
    }
    if(is.null(input$sel5_1)|| input$sel5_1=="Soil fertility" ){
      p5<-""
    } else{
      p5<-"foo"
    }


    if(p1==""|| p1==""|| p3==""|| p4==""|| p5==""){

    c10 <- c('Factor 1',factors[1])
    c11 <- c('Factor 1 - level 1',levelsDt[1,1])
    c12 <- c('Factor 1 - level 2',levelsDt[1,2] )
    c13 <- c('Factor 1 - level 3',levelsDt[1,3])
    c14 <- c('Factor 1 - level 4',levelsDt[1,4] )
    c15 <- c('Factor 1 - level 5',levelsDt[1,5] )

    c16 <- c('Factor 2', factors[2])
    c17 <- c('Factor 2 - level 1',levelsDt[2,1])
    c18 <- c('Factor 2 - level 2',levelsDt[2,2])
    c19 <- c('Factor 2 - level 3',levelsDt[2,3])
    c20 <- c('Factor 2 - level 4',levelsDt[2,4] )
    c21 <- c('Factor 2 - level 5',levelsDt[2,5] )

    c22 <- c('Factor 3', factors[3])
    c23 <- c('Factor 3 - level 1',levelsDt[3,1] )
    c24 <- c('Factor 3 - level 2',levelsDt[3,2] )
    c25 <- c('Factor 3 - level 3',levelsDt[3,3] )
    c26 <- c('Factor 3 - level 4',levelsDt[3,4] )
    c27 <- c('Factor 3 - level 5',levelsDt[3,5] )

    c28 <- c('Factor 4', factors[4] )
    c29 <- c('Factor 4 - level 1',levelsDt[4,1])
    c30 <- c('Factor 4 - level 2',levelsDt[4,2] )
    c31 <- c('Factor 4 - level 3',levelsDt[4,3])
    c32 <- c('Factor 4 - level 4',levelsDt[4,4] )
    c33 <- c('Factor 4 - level 5',levelsDt[4,5])

    c34 <- c('Factor 5', factors[5])
    c35 <- c('Factor 5 - level 1',levelsDt[5,1] )
    c36 <- c('Factor 5 - level 2',levelsDt[5,2])
    c37 <- c('Factor 5 - level 3',levelsDt[5,3])
    c38 <- c('Factor 5 - level 4',levelsDt[5,4] )
    c39 <- c('Factor 5 - level 5',levelsDt[5,5])

    }  else {


    c10 <- c('Factor 1',factors[1])
    c11 <- c('Factor 1 - level 1',levelsDt[1,1])
    c12 <- c('Factor 1 - level 2',levelsDt[1,2] )
    c13 <- c('Factor 1 - level 3',levelsDt[1,3])
    c14 <- c('Factor 1 - level 4',levelsDt[1,4] )
    c15 <- c('Factor 1 - level 5',levelsDt[1,5] )

    c16 <- c('Factor 2', factors[2])
    c17 <- c('Factor 2 - level 1',levelsDt[2,1])
    c18 <- c('Factor 2 - level 2',levelsDt[2,2])
    c19 <- c('Factor 2 - level 3',levelsDt[2,3])
    c20 <- c('Factor 2 - level 4',levelsDt[2,4] )
    c21 <- c('Factor 2 - level 5',levelsDt[2,5] )

    c22 <- c('Factor 3', factors[3])
    c23 <- c('Factor 3 - level 1',levelsDt[3,1] )
    c24 <- c('Factor 3 - level 2',levelsDt[3,2] )
    c25 <- c('Factor 3 - level 3',levelsDt[3,3] )
    c26 <- c('Factor 3 - level 4',levelsDt[3,4] )
    c27 <- c('Factor 3 - level 5',levelsDt[3,5] )

    c28 <- c('Factor 4', factors[4] )
    c29 <- c('Factor 4 - level 1',levelsDt[4,1])
    c30 <- c('Factor 4 - level 2',levelsDt[4,2] )
    c31 <- c('Factor 4 - level 3',levelsDt[4,3])
    c32 <- c('Factor 4 - level 4',levelsDt[4,4] )
    c33 <- c('Factor 4 - level 5',levelsDt[4,5])

    c34 <- c('Factor 5', factors[5])
    c35 <- c('Factor 5 - level 1',levelsDt[5,1] )
    c36 <- c('Factor 5 - level 2',levelsDt[5,2])
    c37 <- c('Factor 5 - level 3',levelsDt[5,3])
    c38 <- c('Factor 5 - level 4',levelsDt[5,4] )
    c39 <- c('Factor 5 - level 5',levelsDt[5,5])

}
    #dt_fctInfo <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c40)# deprecated
    dt_fctInfo <- data.frame(c1,c5,c6,c7,c8,c9,c40)
    dt_fctInfo <- as.data.frame(t(dt_fctInfo), stringsAsFactors=FALSE)
    names(dt_fctInfo) <- c("Factor", "Value1")

    dt_fctdsg <-   data.frame(c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,
                              c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,
                              c31,c32,c33,c34,c35,c36,c37,c38,c39)

    dt_fctdsg <-as.data.frame(t(dt_fctdsg), stringsAsFactors=FALSE)
    names(dt_fctdsg) <- c("Factor", "Value1")
    dt_fctdsg<- dt_fctdsg %>%  dplyr::filter( Value1!="NA")

    out<- rbind(dt_fctInfo, dt_fctdsg)

    names(out) <- c("Factor", "Value1")
    out

  })
  dt_site<- reactive({

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
    names(out)<- c("Factor", "Value1")
    out

  })

  globalMetadata<- reactive({

    glist <- list(metadata_dt2(), personnel_dt(), crop_dt(), fctdsg_dt(), dt_site())
    gtable<- data.table::rbindlist(glist,fill = TRUE)
    gtable <- as.data.frame(gtable,stringAsFactors=FALSE)
    names(gtable)[1]<- "Parameter"
    gtable
  })

  ############# phenology #######################################################################

  phenology_dt <- reactive({

    pheNames<- c("Planting date",	"Transplanting date",	"Planting Emergence date",
                 "Planting and transplanting notes", "Sowing date", "Sowing Emergence date" ,
                 "Sowing Notes", "Flowering date", "Flowering 50% flowering date" ,
                 "Flowering End date", "Flowering Notes", "Grain filling Start date" ,
                 "Grain filling End date" , "Grain filling Notes", "Fruit development start date",
                 "50% fruit development date",  "Fruit development end date", "Fruit ripening date",
                 "Fruit development notes", "Maturity start date",	"50% maturity date",
                 "Maturity end date",	"Maturity Notes", "Senescence start date",
                 "50% senescence date", "Senescence end date", "Other phenological stage Name",
                 "Other phenological stage Start date", "Other phenological stage End date", "Other phenological stage Notes"
    )

    phedt <- data.frame(
      getDateInput(input$cropPheno_planting_date),
      getDateInput(input$cropPheno_transpanting_date),#2
      getDateInput(input$cropPhen_plantingEmergence_date), #3
      input$cropPheno_sowEmerg_notes, #4
      getDateInput(input$cropPheno_sowing_date), #5
      getDateInput(input$cropPheno_emergence_date),#6
      input$cropPheno_sowEmerg_notes,#7

      getDateInput(input$cropPheno_flowering_sdate), getDateInput(input$cropPheno_flowering_50date), # 8 9
      getDateInput(input$cropPheno_flowering_edate), getDateInput(input$cropPheno_flowering_notes), # 10-11

      getDateInput(input$cropPheno_grainFilling_sdate), getDateInput( input$cropPheno_grainFilling_edate),   input$cropPheno_grainFilling_notes, # 12-14

      getDateInput(input$cropPheno_fruitDev_date), getDateInput(input$cropPheno_fruit50dev_date), #15 -16
      getDateInput(input$cropPheno_fruitDevEnd_date),  getDateInput(input$cropPheno_fruitRip_date), #17-18
      input$cropPheno_fruitDev_notes, getDateInput(input$cropPheno_maturity_start_date), #19-20
      getDateInput(input$cropPheno_maturity_50_date), getDateInput(input$cropPheno_maturity_end_date), #21-22
      getDateInput(input$cropPheno_senescence_start_date), #23
      getDateInput(input$cropPheno_senescence_50_date),#24
      getDateInput(input$cropPheno_senescence_end_date), #25
      input$cropPheno_maturity_notes,#26

      input$cropPheno_otherPheno_name,#27
      getDateInput(input$cropPheno_otherPheno_start_date),#28
      getDateInput(input$cropPheno_otherPheno_end_date),#29
      input$cropPheno_otherPheno_notes#20
    )

    names(phedt)<-pheNames
    phedt
  })
  #############  factor_dt2 ######################################################################
  factor_dt2 <- reactive({


    vinfExp <-""
    if(!is.null(input$info_experiment_unit)) vinfExp <- input$info_experiment_unit


    c1 <- c('Information on experimental unit',vinfExp )


    vfarea <-""
    vfexpmaxwidth <- ""
    vfexpmaxlength <- ""
    vpdiam <- ""
    vpdpth <- ""
    if(vinfExp == "plot"  ){
      vfarea <- vinfExp
      wunit <- ""
      lunit <- ""
      if(!is.null(input$expt_plot_width_unit))  wunit <- input$expt_plot_width_unit
      if(!is.null(input$expt_plot_length_unit))  lunit <- input$expt_plot_length_unit
      vfexpmaxwidth <- paste0(input$expt_plot_width, " " , wunit)
      vfexpmaxlength <- paste0(input$expt_plot_length, " " , lunit)
    }
    else if(vinfExp == "field"){
      vfarea <- vinfExp
      wunit <- ""
      lunit <- ""
      if(!is.null(input$expt_field_width_unit))  wunit <- input$expt_field_width_unit
      if(!is.null(input$expt_field_length_unit))  lunit <- input$expt_field_length_unit
      vfexpmaxwidth <- paste0(input$expt_field_width, " " , wunit)
      vfexpmaxlength <- paste0(input$expt_field_length, " " , lunit)
    }
    else if(vinfExp == "pot"){
      wunit <- ""
      lunit <- ""
      if(!is.null(input$pot_diameter_unit))  wunit <- input$pot_diameter_unit
      if(!is.null(input$pot_depth_unit))  lunit <- input$pot_depth_unit
      vpdiam <- paste0(input$pot_diameter, " " , wunit)
      vpdpth <- paste0(input$pot_depth, " " , lunit)

    }

    c2 <- c('Field area',vfarea )
    c3 <- c('Experimental field maximum width', vfexpmaxwidth)
    c4 <- c('Experimental field maximum length', vfexpmaxlength)
    c5 <- c('Pot diameter',vpdiam )
    c6 <- c('Pot depth',vpdpth )
    c7 <- c('Experimental design', input$designFieldbook_agrofims)
    c8 <- c('Experimental design abbreviation', "")
    c9 <- c('Number of replications', input$designFieldbook_agrofims_r)
    c40 <- c('Number of factors', input$nfactors_hdafims)

    levels1 <- c("NA", "NA", "NA", "NA","NA")
    levels2 <- c("NA", "NA", "NA", "NA","NA")
    levels3 <- c("NA", "NA", "NA", "NA","NA")
    levels4 <- c("NA", "NA", "NA", "NA","NA")
    levels5 <- c("NA", "NA", "NA", "NA","NA")
    levelsDt <- data.table(levels1,levels2,levels3,levels4,levels5)


    nf <- input$nfactors_hdafims

    factors <- c("NA", "NA", "NA", "NA","NA")
    for(i in 1:nf){
      g1 <- input[[paste0("sel" , i, "_2" )]]
      g2 <- input[[paste0("sel" , i, "_3" )]]
      if(!is.null(g1) && !is.null(g2)){
        factors[i] <- paste0(g1, " ", g2)

        g3 <- input[[paste0("sel" , i, "_3" )]]
        ls1 <- input[[paste0("numLevels_", i)]]
        if(is_numeric(ls1) && !is.null(g3)){
          if (ls1>5) ls1 <- 5 #max5
          if(g3 %like% "date"){
            for(j in 1:ls1){
              sdate <- input[[paste0("factor_start_date_",i, "_", j)]]
              edate <- input[[paste0("factor_end_date_",i, "_", j)]]
              levelsDt[i,j] <- paste0(sdate, " - ", edate)
            }
          }
          else{
            nl <- input[[paste0("levels_",i)]]
            count <- 1
            for(lv in nl){
              if(count <= 5){
                if(is.null(input[[paste0("funits_", i)]])){
                  levelsDt[i,count] <- lv
                }
                else{
                  levelsDt[i,count]<- paste0(lv, " ", input[[paste0("funits_", i)]])
                }
              }
              count <- count + 1
            }
          }
        }

      }

    }

    vCropCommon <- ""
    if(!is.null(input$cropVarietyNameMono)) vCropCommon <- input$cropVarietyNameMono
    cropAsFactor<- input$setCropFactor #by defult false or unselect checkbox
    print(cropAsFactor)
    print(vCropCommon)

    print(input$nfactors_hdafims)

    if( factors[1]== "NA" &&  cropAsFactor==TRUE && length(vCropCommon)>=2 ){

      c10 <- c('Factor 1', "VARIETIES")
      c11 <- c('Factor 1 - level 1', vCropCommon[1])
      c12 <- c('Factor 1 - level 2', vCropCommon[2])
      c13 <- c('Factor 1 - level 3', vCropCommon[3])
      c14 <- c('Factor 1 - level 4', vCropCommon[4] )
      c15 <- c('Factor 1 - level 5', vCropCommon[5] )
    } else {
      c10 <- c('Factor 1',factors[1])
      c11 <- c('Factor 1 - level 1',levelsDt[1,1])
      c12 <- c('Factor 1 - level 2',levelsDt[1,2] )
      c13 <- c('Factor 1 - level 3',levelsDt[1,3])
      c14 <- c('Factor 1 - level 4',levelsDt[1,4] )
      c15 <- c('Factor 1 - level 5',levelsDt[1,5] )

    }

    print(input$nfactors_hdafims)
    print(factors[2])
    print(cropAsFactor)
    print(length(vCropCommon))
    nfactor <- as.numeric(input$factors_hdafims)
    flag_variety <- TRUE

    if( factors[2]== "NA" && cropAsFactor==TRUE && length(vCropCommon)>=2 ){

      c16 <- c('Factor 2', "VARIETIES")
      c17 <- c('Factor 2 - level 1', vCropCommon[1])
      c18 <- c('Factor 2 - level 2', vCropCommon[2])
      c19 <- c('Factor 2 - level 3', vCropCommon[3])
      c20 <- c('Factor 2 - level 4', vCropCommon[4] )
      c21 <- c('Factor 2 - level 5', vCropCommon[5] )
      flag_variety <- FALSE

    } else {

      c16 <- c('Factor 2', factors[2])
      c17 <- c('Factor 2 - level 1',levelsDt[2,1])
      c18 <- c('Factor 2 - level 2',levelsDt[2,2])
      c19 <- c('Factor 2 - level 3',levelsDt[2,3])
      c20 <- c('Factor 2 - level 4',levelsDt[2,4] )
      c21 <- c('Factor 2 - level 5',levelsDt[2,5] )
    }

    if( flag_variety == TRUE && factors[3]== "NA" &&   cropAsFactor==TRUE && length(vCropCommon)>=2 ){
      c22 <- c('Factor 3', "VARIETIES")
      c23 <- c('Factor 3 - level 1', vCropCommon[1])
      c24 <- c('Factor 3 - level 2', vCropCommon[2])
      c25 <- c('Factor 3 - level 3', vCropCommon[3])
      c26 <- c('Factor 3 - level 4', vCropCommon[4] )
      c27 <- c('Factor 3 - level 5', vCropCommon[5] )
      flag_variety <- FALSE
    } else {

      c22 <- c('Factor 3', factors[3])
      c23 <- c('Factor 3 - level 1',levelsDt[3,1] )
      c24 <- c('Factor 3 - level 2',levelsDt[3,2] )
      c25 <- c('Factor 3 - level 3',levelsDt[3,3] )
      c26 <- c('Factor 3 - level 4',levelsDt[3,4] )
      c27 <- c('Factor 3 - level 5',levelsDt[3,5] )
    }

    if( flag_variety == TRUE &&  factors[4]== "NA" &&   cropAsFactor==TRUE && length(vCropCommon)>=2 ){
      c28 <- c('Factor 4', "VARIETIES")
      c29 <- c('Factor 4 - level 1', vCropCommon[1])
      c30 <- c('Factor 4 - level 2', vCropCommon[2])
      c31 <- c('Factor 4 - level 3', vCropCommon[3])
      c32 <- c('Factor 4 - level 4', vCropCommon[4] )
      c33 <- c('Factor 4 - level 5', vCropCommon[5] )
      flag_variety <- FALSE
    } else {

      c28 <- c('Factor 4', factors[4] )
      c29 <- c('Factor 4 - level 1',levelsDt[4,1])
      c30 <- c('Factor 4 - level 2',levelsDt[4,2] )
      c31 <- c('Factor 4 - level 3',levelsDt[4,3])
      c32 <- c('Factor 4 - level 4',levelsDt[4,4] )
      c33 <- c('Factor 4 - level 5',levelsDt[4,5])
    }

    if( flag_variety == TRUE && factors[5]== "NA" &&   cropAsFactor==TRUE && length(vCropCommon)>=2 ){

      c34 <- c('Factor 5', "VARIETIES")
      c35 <- c('Factor 5 - level 1', vCropCommon[1])
      c36 <- c('Factor 5 - level 2', vCropCommon[2])
      c37 <- c('Factor 5 - level 3', vCropCommon[3])
      c38 <- c('Factor 5 - level 4', vCropCommon[4] )
      c39 <- c('Factor 5 - level 5', vCropCommon[5] )
      flag_variety <- FALSE
    } else {

      c34 <- c('Factor 5', factors[5])
      c35 <- c('Factor 5 - level 1',levelsDt[5,1] )
      c36 <- c('Factor 5 - level 2',levelsDt[5,2])
      c37 <- c('Factor 5 - level 3',levelsDt[5,3])
      c38 <- c('Factor 5 - level 4',levelsDt[5,4] )
      c39 <- c('Factor 5 - level 5',levelsDt[5,5])

    }

    df_metadata <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c40,c10,
                              c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,
                              c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,
                              c31,c32,c33,c34,c35,c36,c37,c38,c39)
    var_metadata <-  t(df_metadata)
    # print(df_metadata)

  })



  #############  metadata_dt ##########################################################
  metadata_dt <- function(){

     c1 <- c('Experiment ID', input$experimentId)
     c2 <- c('Experiment name', input$experimentName )
     c3 <- c('Experiment project name', input$experimentProjectName)
     c4 <- c('Experiment start date', paste(input$fbDesign_project_time_line[1]) )
     c5 <- c('Experiment end date', paste(input$fbDesign_project_time_line[2]))

     xdur <- interval(ymd(input$fbDesign_project_time_line[1]),ymd(input$fbDesign_project_time_line[2]))
     xdur <- xdur %/% months(1)
     xdur <- paste(xdur," months", sep = "")


     c6 <- c('Experiment duration', xdur)
     vTypeExperiment <- ""
     if(!is.null(input$designFieldbook_typeExperiment)) vTypeExperiment <- input$designFieldbook_typeExperiment

     c7 <- c('Type of experiment', vTypeExperiment)
     c8 <- c('Experiment objective', input$experimentObj)

     vfundAgenType <- ""
     vfundName <-""
     if(!is.null(input$designFieldbook_fundAgencyType)) {
       vfundAgenType <- paste(input$designFieldbook_fundAgencyType, collapse = ",")
       vn <- length(input$designFieldbook_fundAgencyType)
       vfundName <- input[[paste0("fundName_", 1)]]

       for(i in 2:vn){
         vfundName <- paste(vfundName, ",", input[[paste0("fundName_", i)]])
       }
     }

     c9 <- c('Funding agency type', vfundAgenType)
     c10 <- c('Funding agency name', vfundName)

     vNumPrEnt <- ""
     vPrEnt <- c()
     vContCenter <- c()
     vcontCRP <- c()
     vPrName <- c()



     if(is.numeric(input$numProjEntity)){
       vNumPrEnt <- input$numProjEntity
       vn <- input$numProjEntity

       for(i in 1:vn){
         aux <- input[[paste0("projEntity_", i)]]
         if(!is.null(aux)){
           vPrEnt <- c(vPrEnt,aux)
           if(aux == "Other"){
             vPrName <- c(vPrName, input[[paste0("contOtherCenter_", i)]])
           }
           else{
             if(!is.null(input[[paste0("contCenter_", i)]])){
               vContCenter <- c(vContCenter, input[[paste0("contCenter_", i)]])
             }
             else{
               vContCenter <- c(vContCenter, "")
             }
             if(!is.null(input[[paste0("contCRP_", i)]])){
               vcontCRP <- c(vcontCRP, input[[paste0("contCRP_", i)]])
             }
             else{
               vcontCRP <- c(vcontCRP, "")
             }
           }
         }
       }
       vPrEnt <- paste(vPrEnt, collapse = ",")
       vContCenter <- paste(vContCenter, collapse = ",")
       vcontCRP <- paste(vcontCRP, collapse = ",")
       vPrName <- paste(vPrName, collapse = ",")
     }

     c11 <- c('Number of project management entities',vNumPrEnt )
     c12 <- c('Project management entity',vPrEnt )
     c13 <- c('Contribuitor center',vContCenter )
     c14 <- c('Contribuitor CRP', vcontCRP)
     c15 <- c('Project management entity name', vPrName )



     vleadOrgType <- c()
     vleadPerson <- c()
     vleadOrgName <- c()

     if(is.numeric(input$numLeads)){
       vn <- input$numProjEntity
       for(i in 1:vn){
         aux <- input[[paste0("projLeadEnt_", i)]]
         if(!is.null(aux)){
           if(aux == "Other"){
             if(is.null(input[[paste0("lead_org_type_1_", i)]])) vleadOrgType <- c(vleadOrgType,aux)
             else vleadOrgType <- c(vleadOrgType, input[[paste0("lead_org_type_1_", i)]])
             vleadOrgName <- c(vleadOrgName, input[[paste0("leadNameOther_", i)]])
           }
           else{
             vleadOrgType <- c(vleadOrgType,aux)
             if(!is.null(input[[paste0("tLeadCenter_", i)]])){
               vleadOrgName <- c(vleadOrgName, input[[paste0("tLeadCenter_", i)]])
             }
             else{
               vleadOrgName <- c(vleadOrgName, "")
             }
           }
           vleadPerson <- c(vleadPerson, input[[paste0("expLead_", i)]])
         }
       }

       vleadOrgType <- paste(vleadOrgType, collapse = ";")
       vleadOrgName <- paste(vleadOrgName, collapse = ";")
       vleadPerson <- paste(vleadPerson, collapse = ";")
     }

     c16 <- c('Experiment, lead organization type',vleadOrgType )
     c17 <- c('Experiment lead person / Primary Investigator', vleadOrgName)
     c18 <- c('Experiment, lead organization name',vleadPerson )



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
       # print(xaux)
       # xstart <- which(strsplit(xaux, "")[[1]]=="(")
       # vsiteId <- substr(xaux, xstart+1, nchar(xaux)-1)
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

     c40 <- c('Cropping type', input$croppingType )

     vCropCommon <- ""
     if(!is.null(input$cropCommonNameMono)) vCropCommon <- input$cropCommonNameMono

     c41 <- c('Crop common name',vCropCommon )
     c42 <- c('Crop latin name', input$cropLatinNameMono)

     vCropVarName <- ""
     if(!is.null(input$cropVarietyNameMono)) vCropVarName <- paste(input$cropVarietyNameMono, collapse = ",")

     c43 <- c('Crop variety name', vCropVarName)
     c44 <- c('Cultivar name',input$cultivarNameMono )
     c45 <- c('Crop local name', input$monoCropLocalName)

     nCropPrevCrop <- ""
     nprevCropName <- ""
     nprevCropVar <- ""

     if(is.numeric(input$numPreviousCrop)) nCropPrevCrop <-input$numPreviousCrop
     if(!is.null(input$prevCropName)) nprevCropName <- paste(input$prevCropName, collapse = ",")
     if(!is.null(input$prevCropVar)) nprevCropVar <- paste(input$prevCropVar, collapse = ",")

     c46 <- c('Number of previous crop', nCropPrevCrop)
     c47 <- c('Previous crop name', nprevCropName)
     c48 <- c('Previous crop variety',nprevCropVar )

     # c56 <- c('Subject', )
     # c57 <- c('Keywords', )
     # c58 <- c('Embargo end date', )

     df_metadata <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,
                               c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,
                               c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,
                               c31,c32,c33,c34,c35,c36,c37,c38,c39,c40,
                               c41,c42,c43,c44,c45,c46,c47, c48)
     var_metadata <-  t(df_metadata)

     # print(df_metadata)

   }



  #############  factor_dt ##########################################################
  factor_dt <- function(){


     vinfExp <-""
     if(!is.null(input$info_experiment_unit)) vinfExp <- input$info_experiment_unit


     c1 <- c('Information on experimental unit',vinfExp )

     vfarea <-""
     vfexpmaxwidth <- ""
     vfexpmaxlength <- ""
     vpdiam <- ""
     vpdpth <- ""
     if(vinfExp == "plot"  ){
       vfarea <- vinfExp
       wunit <- ""
       lunit <- ""
       if(!is.null(input$expt_plot_width_unit))  wunit <- input$expt_plot_width_unit
       if(!is.null(input$expt_plot_length_unit))  lunit <- input$expt_plot_length_unit
       vfexpmaxwidth <- paste0(input$expt_plot_width, " " , wunit)
       vfexpmaxlength <- paste0(input$expt_plot_length, " " , lunit)
     }
     else if(vinfExp == "field"){
       vfarea <- vinfExp
       wunit <- ""
       lunit <- ""
       if(!is.null(input$expt_field_width_unit))  wunit <- input$expt_field_width_unit
       if(!is.null(input$expt_field_length_unit))  lunit <- input$expt_field_length_unit
       vfexpmaxwidth <- paste0(input$expt_field_width, " " , wunit)
       vfexpmaxlength <- paste0(input$expt_field_length, " " , lunit)
     }
     else if(vinfExp == "pot"){
       wunit <- ""
       lunit <- ""
       if(!is.null(input$pot_diameter_unit))  wunit <- input$pot_diameter_unit
       if(!is.null(input$pot_depth_unit))  lunit <- input$pot_depth_unit
       vpdiam <- paste0(input$pot_diameter, " " , wunit)
       vpdpth <- paste0(input$pot_depth, " " , lunit)

     }

     c2 <- c('Field area',vfarea )
     c3 <- c('Experimental field maximum width', vfexpmaxwidth)
     c4 <- c('Experimental field maximum length', vfexpmaxlength)
     c5 <- c('Pot diameter',vpdiam )
     c6 <- c('Pot depth',vpdpth )
     c7 <- c('Experimental design', input$designFieldbook_agrofims)
     c8 <- c('Experimental design abbreviation', "")
     c9 <- c('Number of replications', input$designFieldbook_agrofims_r)
     c40 <- c('Number of factors', input$nfactors_hdafims)

     levels1 <- c("NA", "NA", "NA", "NA","NA")
     levels2 <- c("NA", "NA", "NA", "NA","NA")
     levels3 <- c("NA", "NA", "NA", "NA","NA")
     levels4 <- c("NA", "NA", "NA", "NA","NA")
     levels5 <- c("NA", "NA", "NA", "NA","NA")
     levelsDt <- data.table(levels1,levels2,levels3,levels4,levels5)


     nf <- input$nfactors_hdafims

     factors <- c("NA", "NA", "NA", "NA","NA")
     for(i in 1:nf){
       g1 <- input[[paste0("sel" , i, "_2" )]]
       g2 <- input[[paste0("sel" , i, "_3" )]]
       if(!is.null(g1) && !is.null(g2)){
         factors[i] <- paste0(g1, " ", g2)

         g3 <- input[[paste0("sel" , i, "_3" )]]
         ls1 <- input[[paste0("numLevels_", i)]]
         if(is_numeric(ls1) && !is.null(g3)){
           if (ls1>5) ls1 <- 5 #max5
           if(g3 %like% "date"){
             for(j in 1:ls1){
               sdate <- input[[paste0("factor_start_date_",i, "_", j)]]
               edate <- input[[paste0("factor_end_date_",i, "_", j)]]
               levelsDt[i,j] <- paste0(sdate, " - ", edate)
             }
           }
           else{
             nl <- input[[paste0("levels_",i)]]
             count <- 1
             for(lv in nl){
               if(count <= 5){
                 if(is.null(input[[paste0("funits_", i)]])){
                   levelsDt[i,count] <- lv
                 }
                 else{
                   levelsDt[i,count]<- paste0(lv, " ", input[[paste0("funits_", i)]])
                 }
               }
               count <- count + 1
             }
           }
         }

       }

     }


     c10 <- c('Factor 1',factors[1])
     c11 <- c('Factor 1 - level 1',levelsDt[1,1])
     c12 <- c('Factor 1 - level 2',levelsDt[1,2] )
     c13 <- c('Factor 1 - level 3',levelsDt[1,3])
     c14 <- c('Factor 1 - level 4',levelsDt[1,4] )
     c15 <- c('Factor 1 - level 5',levelsDt[1,5] )

     c16 <- c('Factor 2', factors[2])
     c17 <- c('Factor 2 - level 1',levelsDt[2,1])
     c18 <- c('Factor 2 - level 2',levelsDt[2,2])
     c19 <- c('Factor 2 - level 3',levelsDt[2,3])
     c20 <- c('Factor 2 - level 4',levelsDt[2,4] )
     c21 <- c('Factor 2 - level 5',levelsDt[2,5] )

     c22 <- c('Factor 3', factors[3])
     c23 <- c('Factor 3 - level 1',levelsDt[3,1] )
     c24 <- c('Factor 3 - level 2',levelsDt[3,2] )
     c25 <- c('Factor 3 - level 3',levelsDt[3,3] )
     c26 <- c('Factor 3 - level 4',levelsDt[3,4] )
     c27 <- c('Factor 3 - level 5',levelsDt[3,5] )

     c28 <- c('Factor 4', factors[4] )
     c29 <- c('Factor 4 - level 1',levelsDt[4,1])
     c30 <- c('Factor 4 - level 2',levelsDt[4,2] )
     c31 <- c('Factor 4 - level 3',levelsDt[4,3])
     c32 <- c('Factor 4 - level 4',levelsDt[4,4] )
     c33 <- c('Factor 4 - level 5',levelsDt[4,5])

     c34 <- c('Factor 5', factors[5])
     c35 <- c('Factor 5 - level 1',levelsDt[5,1] )
     c36 <- c('Factor 5 - level 2',levelsDt[5,2])
     c37 <- c('Factor 5 - level 3',levelsDt[5,3])
     c38 <- c('Factor 5 - level 4',levelsDt[5,4] )
     c39 <- c('Factor 5 - level 5',levelsDt[5,5])


     df_metadata <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c40,c10,
                               c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,
                               c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,
                               c31,c32,c33,c34,c35,c36,c37,c38,c39)
     var_metadata <-  t(df_metadata)
     # print(df_metadata)

   }

  ## Trait Table ####################################################################
  traits_dt <- function(){
    a<- traitsVals$Data
    if(nrow(traitsVals$Data) >0){
      row_select <- input$dt_rows_selected
      row_select <- sort(row_select)
      #aux_dt <- dplyr::filter(traitsVals$Data, Status=="Selected")
      aux_dt<- a[row_select,]
      a<- aux_dt
    }

    return(a)
  }

  ### Book preview #############################################################
  shiny::observeEvent(input$fbDesign_draft_agrofims, {

     withProgress(message = 'Fieldbook Preview', value = 0, {

      incProgress(1/10,message = "...")



       flag <- TRUE

       if(input$fullFactorialRB=="Yes" &&  as.numeric(input$nfactors_hdafims_y)==1){
         shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Full factorial needs at least 2 factors "), styleclass = "danger")
         flag<-FALSE
       }

       else if(class(fb_agrofims())=="try-error"){

         shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Select factors and levels "), styleclass = "danger")
         flag<-FALSE
       }

       # else if( input$sel1_1=="Soil fertility" ||   input$sel2_1=="Soil fertility" ||
       #          input$sel3_1=="Soil fertility" ||   input$sel4_1=="Soil fertility" ||
       #          input$sel5_1=="Soil fertility") {
       #
       #   if(is.null(input$numLevels_tabSoil_1)){
       #     flag<-FALSE
       #     shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Select factors and levels "), styleclass = "danger")
       #
       #   } else if(input$numLevels_tabSoil_1==1){
       #     flag<-FALSE
       #     shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Select factors and levels "), styleclass = "danger")
       #
       #   }
       #
       #   if(is.null(input$numLevels_tabSoil_2)){
       #     flag<-FALSE
       #     shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Select factors and levels "), styleclass = "danger")
       #
       #   }else if(input$numLevels_tabSoil_2==1){
       #     flag<-FALSE
       #     shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Select factors and levels "), styleclass = "danger")
       #
       #   }
       #
       #   if(is.null(input$numLevels_tabSoil_3)){
       #     flag<-FALSE
       #     shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Select factors and levels "), styleclass = "danger")
       #
       #   }else if(input$numLevels_tabSoil_3==1){
       #     flag<-FALSE
       #     shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Select factors and levels "), styleclass = "danger")
       #
       #   }
       #
       #   if(is.null(input$numLevels_tabSoil_4)){
       #     flag<-FALSE
       #     shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Select factors and levels "), styleclass = "danger")
       #
       #   }else if(input$numLevels_tabSoil_4==1){
       #     flag<-FALSE
       #     shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Select factors and levels "), styleclass = "danger")
       #
       #   }
       #
       #   if(is.null(input$numLevels_tabSoil_5)){
       #     flag<-FALSE
       #     shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Select factors and levels "), styleclass = "danger")
       #
       #   }else if(input$numLevels_tabSoil_5==1){
       #     flag<-FALSE
       #     shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Select factors and levels "), styleclass = "danger")
       #
       #   }
       #   flag <- flag
       # }

       if(flag){

       fb  <- fb_agrofims_traits()
       output$fbDesign_table_agrofims <- rhandsontable::renderRHandsontable({
         rhandsontable::rhandsontable(fb , readOnly = T)})
       }

       incProgress(9/10,message = "...")
       incProgress(10/10,message = "...")

     })

   })


  ############# donwload fieldbook ###############################################################
   output$downloadData <- downloadHandler(
     filename = "fileNameBook.xlsx",
     content = function(file) {

       withProgress(message = 'Downloading fieldbook', value = 0, {

         #print(soil_design())
         # n<-3
         # a11<<- paste(lapply(1:n, function(x) eval(get_loop_AgrOper("irrigation_technique_",n=n)[[x]])))
         # irri_technique <- paste(lapply(1:n, function(x) eval(get_loop_AgrOper("irrigation_technique_",n=n)[[x]])))
         # irri23 <<- unlist(lapply(1:n, function(x) eval(get_loop_irrigation_technique(irri_technique, module="irrigation")[[x]]) ))

         n <- as.numeric(input$numApplicationsIrrigation)
         fb_traits <- fb_agrofims_traits()
         gmetadata <- globalMetadata() #metadata_dt2()
         phenology <- phenology_dt()
         trait_agrofims_dt <- traits_dt()[,-1]
         weather <- dt_weather_agrofims()
         soil_vars <- dt_soil_agrofims()
         fname <- paste(file,"xlsx",sep=".")

         print("inicio")
         wb <- createWorkbook()

         incProgress(2/20,message = "Downloading data...")

         incProgress(6/20,message = "Metadata metadata sheet...")
        openxlsx::addWorksheet(wb, "Metadata", gridLines = TRUE)
        openxlsx::writeDataTable(wb, "Metadata", x = gmetadata,
                                 colNames = TRUE, withFilter = FALSE)

        print("phenology")
         openxlsx::addWorksheet(wb, "Phenology", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Phenology", x = phenology,
                                  colNames = TRUE, withFilter = FALSE)
         print("feno2")
         #
         # incProgress(7/20,message = "Adding installation sheet...")
         #
         # print("error4")

         if(input$croppingType=="Monocrop"){

         incProgress(7/20,message = "Adding fieldbook data...")
         openxlsx::addWorksheet(wb, "Fieldbook", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Fieldbook", x = fb_traits,
                                  colNames = TRUE, withFilter = FALSE)

         }

         if(input$croppingType=="Intercrop"){

           incProgress(7/20,message = "Adding fieldbook data...")

           if(is.null(input$cropsSelected)){
             print("no crops selected")
           } else {


             crops <- input$cropsSelected
             cropsito<- NULL
             for(i in 1:length(crops)){
             cropsito[i] <- input[[paste0("cropCommonName",i)]]
             }
             interTrait <- out<-list()
             for(i in 1:length(crops)){
               interTrait[[i]] <- traits_dt() %>%  dplyr::filter(Crop==cropsito[i])
               interTrait[[i]] <- as.data.frame(interTrait[[i]], stringsAsFactors=FALSE)
                print(interTrait[[i]])

               fb <- fb_agrofims()
               trait <- interTrait[[i]]
               cr<- trait$Crop

               cm <- trait$`Crop measurement`
               sb<- trait$Subgroup
               sc <- trait$Scale
               sc[is.na(sc)] <- "unitless"
               #cs <- paste(cr, cm, sc, sep="-")
               cs <- paste(cr,sb, cm, sc, sep="-")
               trait_selected <- cs

               if(!is.null(trait_selected) || length(trait_selected)==0 ){
                 mm  <-  matrix(nrow = nrow(fb), ncol = length(trait_selected) )
                 nm  <-  c(names(fb), trait_selected)
                 out[[i]]  <-  cbind(fb, mm)
                 names(out[[i]])  <-  nm
               }

               openxlsx::addWorksheet(wb, paste("Fieldbook",cropsito[i],sep="-") , gridLines = TRUE)
               openxlsx::writeDataTable(wb, paste("Fieldbook",cropsito[i],sep="-") , x = out[[i]],
                                        colNames = TRUE, withFilter = FALSE)
             }

           } #end if
         }

         agroFeaSelected <- input$selectAgroFeature
         #
         #print("error5")
         if(is.element("Irrigation", agroFeaSelected)) {
           print("irri")
           incProgress(14/20,message = "Adding irrigation data...")
           dt_irri <- dt_irrigation()
           print(dt_irri)
           openxlsx::addWorksheet(wb, "Irrigation", gridLines = TRUE)
           openxlsx::writeDataTable(wb, "Irrigation", x = dt_irri,
                                    colNames = TRUE, withFilter = FALSE)

         }
         #print("error6")
         if(is.element("Harvest", agroFeaSelected)) {
           # print("har")

           if(input$croppingType!="Intercrop"){

           incProgress(13/20,message = "Adding harvest data...")
           dt_harv <- dt_harvest()
           openxlsx::addWorksheet(wb, "Harvest", gridLines = TRUE)
           openxlsx::writeDataTable(wb, "Harvest", x = dt_harv,
                                    colNames = TRUE, withFilter = FALSE)
           } else {

             crops <- input$cropsSelected
             dt_harv<-list()
             #interTrait <- out<-list()
              cropsito<- NULL
             for(i in 1:length(crops)){
               cropsito[i] <- input[[paste0("cropCommonName",i)]]
             }


             for(i in 1:length(crops)){

             incProgress(13/20,message = "Adding harvest data...")
             dt_harv[[i]] <- dt_harvest()
             names(dt_harv[[i]]) <- paste(cropsito[i], names(dt_harv[[i]]), sep="-")
             openxlsx::addWorksheet(wb, paste("Harvest",cropsito[i],sep="-") , gridLines = TRUE)
             openxlsx::writeDataTable(wb, paste("Harvest",cropsito[i],sep="-") , x = dt_harv[[i]],
                                      colNames = TRUE, withFilter = FALSE)
            }

           }


         }
         #print("error7")
         if(is.element("Land preparation", agroFeaSelected)) {
            incProgress(10/20,message = "Adding land preparation sheet...")

           print(input$landLevelling_checkbox)
           print(input$puddling_checkbox)
           print(input$tillage_checkbox)

           if(input$landLevelling_checkbox==TRUE && input$puddling_checkbox==TRUE
                                                  && input$tillage_checkbox==TRUE){

            dt_land <- dt_land_description()

            openxlsx::addWorksheet(wb, "Land preparation", gridLines = TRUE)
            openxlsx::writeDataTable(wb, "Land preparation", x = dt_land ,
                                     colNames = TRUE, withFilter = FALSE)
              }
          }
         #print("error9")
         if(is.element("Mulching and residue", agroFeaSelected)) {
           print("mu")
           if(input$mulchManag_checkbox==TRUE && input$residueManag_checkbox==TRUE){

           incProgress(11/20,message = "Adding mulching data...")

           dt_mr <- dt_mures()

           openxlsx::addWorksheet(wb, "Mulching and residue", gridLines = TRUE)
           openxlsx::writeDataTable(wb, "Mulching and residue", x = dt_mr,
                                    colNames = TRUE, withFilter = FALSE)

           }

         }
         #print("error10")
         if(is.element("Planting and transplanting", agroFeaSelected)) {
            print("plant")

           if(input$directSeeding_checkbox==TRUE && input$transplanting_checkbox==TRUE){

           incProgress(12/20,message = "Adding planting and transplanting data...")

           dt_plant <- dt_planting()

           openxlsx::addWorksheet(wb, "Planting and transplanting", gridLines = TRUE)
           openxlsx::writeDataTable(wb, "Planting and transplanting", x = dt_plant,
                                    colNames = TRUE, withFilter = FALSE)

           }

         }
         #print("error11")
         if(is.element("Weeding", agroFeaSelected)){
            print("wed")
            incProgress(13/20,message = "Adding Weeding data...")

            dt_weed <- dt_weeding()

            openxlsx::addWorksheet(wb, "Weeding", gridLines = TRUE)
            openxlsx::writeDataTable(wb, "Weeding", x = dt_weed,
                                     colNames = TRUE, withFilter = FALSE)
         }

         if(is.element("Soil fertility", agroFeaSelected)){

           dt_soil<- dt_soilFertility()
           openxlsx::addWorksheet(wb, "Soil fertility", gridLines = TRUE)
           openxlsx::writeDataTable(wb, "Soil fertility", x = dt_soil,
                                    colNames = TRUE, withFilter = FALSE)

         }


     #
         incProgress(9/20,message = "Adding trait list sheet...")

         openxlsx::addWorksheet(wb, "Trait list", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Trait list", x = trait_agrofims_dt,
                                  colNames = TRUE, withFilter = FALSE)

         print("trAITS")
         if(is.null(weather) || length(weather)==0 || nrow(weather)==0  ){
           print("there is no weather data")

         }
         else {

           incProgress(8/10,message = "Adding weather variables sheet...")

           openxlsx::addWorksheet(wb, "Weather", gridLines = TRUE)
           openxlsx::writeDataTable(wb, "Weather", x = weather,
                                    colNames = TRUE, withFilter = FALSE)
         }
         #print("error13")
         if(is.null(soil_vars) || length(soil_vars)==0 || nrow(soil_vars)==0 ){
           print("there is no soil data")

         }
         else{
               incProgress(9/10,message = "Adding soil variables sheet...")
               openxlsx::addWorksheet(wb, "Soil", gridLines = TRUE)
               openxlsx::writeDataTable(wb, "Soil", x = soil_vars,
                                            colNames = TRUE, withFilter = FALSE)
         }
         incProgress(19/20,message = "Downloading file...")
         saveWorkbook(wb, file = fname , overwrite = TRUE)
         file.rename(fname, file)


       })

     },
     contentType="application/xlsx"
   )

}
