#'
#' Design field book for HIDAP-AGROFIMS
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
## @param dom target dom element name
#' @param values reactive values
#' @author Omar Benites / Ivan Perez / Raul Arias
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
 
  # GLOBAL PATH donde se aloja las sessiones y backups
  sessionpath <- "/home/obenites/AGROFIMS/savesession/"
  sessionpathbk <- "/home/obenites/AGROFIMS/savesession_bk/"
   
    #Funcion que crea lista de inputs a guardar: Experiment
    inputsExperiment <- function() {
      a1 <- a2 <- a3 <- c()
      b1 <- b2 <- b3 <- b4 <- b5 <- b6 <- b7 <- b8 <- b9 <- b10 <- b11 <- b12 <- c()
      c1 <- c2 <- c3 <- c4 <- c5 <- c6 <- c7 <- c8 <- c9 <- c10 <- c11 <- c12 <- c13 <- c14 <- c15 <- c16 <- c17 <- c18 <- c()

      inputRds <- readRDS(paste0(globalpath, "inputId1_v3.rds"))
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
      
      inputRds <- readRDS(paste0(globalpath, "inputId1_v3.rds"))
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
    # observeEvent(input$xtest, {
    #   print(inputsExpConHarvest())
    # })
    
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
        
        inputs_to_save <- rbind(inputsExperiment(),
                                inputsPersonnel(),
                                inputsSite(),
                                inputsCrop(),
                                inputsDesign(),
                                inputsExpCon())
        
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
  
  
  

  # funcion que imprime Experiment ID
  expIdgenerator <- function() {
    # FMCassava200910_LaMolina
    
    if (input$croppingType == "Monocrop") {
      m <- as.character(input$croppingType)
      m <- substring(m, 1, 1)
      c <- input$cropCommonNameMono
      y <- input$fbDesign_project_start_date
      y <- substring(y[1], 1, 7)
      y <- gsub("-", "", y)
      l <- input$fbDesign_countryTrial
      #l <- "LocationName"
      
      id <- paste0("F", m, c, y, "_", l)
    } else if (input$croppingType == "Intercrop") {
      m <- as.character(input$croppingType)
      m <- substring(m, 1, 1)
      c <- c()
      c <- paste0(input$cropCommonName1,
                  input$cropCommonName2,
                  input$cropCommonName3,
                  input$cropCommonName4,
                  input$cropCommonName5,
                  input$cropCommonName6,
                  input$cropCommonName7)
      
      y <- input$fbDesign_project_start_date
      y <- substring(y[1], 1, 7)
      y <- gsub("-", "", y)
      l <- input$fbDesign_countryTrial
      #l <- "LocationName"
      
      id <- paste0("F", m, c, y, "_", l)
    }
    id
  }
  
  # input "Experiment ID" Ej. FMCassava200910_LaMolina
  output$experimentIdUI <- renderUI({
    disabled(textInput(inputId = "experimentId", label = "Experiment ID",
                       value = expIdgenerator()))
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
  
  # Funcion que verifica input antes de dibujar el qr
  veriqr <- function() {
    if (input$uniqueId != "") {
      a <- input$uniqueId
      #print("ëntro")
    } else {
      a <- "NoId"
      #print("no")
    }
    a
  }
  
  # Funcion que renderiza en imagen png el codigo qr
  output$myqr <- renderImage({
    # delay(
    #   1000,
    validate(need(input$uniqueId, ""))
    
      if (input$uniqueId != "" || !is.null(input$uniqueId)) {
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

  # Funcion que renderiza los air picker en exp conditions
  ## Harvest
  # # harvest_start_date
  # output$h_start_date <- renderUI({
  #   if (!is.null(input$fbDesign_project_time_line)) {
  #     airDatepickerInput("harvest_start_date",
  #                        "Start date",
  #                        clearButton = T,
  #                        autoClose = T,
  #                        value = as.Date(input$fbDesign_project_time_line[1]) + 1,
  #                        placeholder = "yyyy-mm-dd",
  #                        minDate = as.Date(input$fbDesign_project_time_line[1]) + 1,
  #                        maxDate = as.Date(input$fbDesign_project_time_line[2]) + 1
  #                        
  #     )
  #   } else {
  #     airDatepickerInput("harvest_start_date",
  #                        "Start date",
  #                        clearButton = T,
  #                        autoClose = T,
  #                        placeholder = "yyyy-mm-dd"
  #     )
  #   }
  # })
  # 
  # # harvest_end_date
  # output$h_end_date <- renderUI({
  #   if (!is.null(input$fbDesign_project_time_line)) {
  #     airDatepickerInput("harvest_end_date",
  #                        "End date",
  #                        clearButton = T,
  #                        autoClose = T,
  #                        value = as.Date(input$fbDesign_project_time_line[1]) + 1,
  #                        placeholder = "yyyy-mm-dd",
  #                        minDate = as.Date(input$fbDesign_project_time_line[1]) + 1,
  #                        maxDate = as.Date(input$fbDesign_project_time_line[2]) + 1
  #                        
  #     )
  #   } else {
  #     airDatepickerInput("harvest_end_date",
  #                        "End date",
  #                        clearButton = T,
  #                        autoClose = T,
  #                        placeholder = "yyyy-mm-dd"
  #     )
  #   }
  # })
  
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
  
  # landLeveling_end_date
  output$landLev_end_date <- renderUI({
    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
      airDatepickerInput("landLeveling_end_date",
                         "End date",
                         clearButton = T,
                         autoClose = T,
                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                         placeholder = "yyyy-mm-dd",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("landLeveling_end_date",
                         "End date",
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
  
  # puddling_end_date
  output$pud_end_date <- renderUI({
    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
      airDatepickerInput("puddling_end_date",
                         "End date",
                         clearButton = T,
                         autoClose = T,
                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                         placeholder = "yyyy-mm-dd",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("puddling_end_date",
                         "End date",
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
  
  # tillage_end_date
  output$till_end_date <- renderUI({
    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
      airDatepickerInput("tillage_end_date",
                         "End date",
                         clearButton = T,
                         autoClose = T,
                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                         placeholder = "yyyy-mm-dd",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("tillage_end_date",
                         "End date",
                         clearButton = T,
                         autoClose = T,
                         placeholder = "yyyy-mm-dd"
      )
    }
  })
  
  ## Mulching and Residue
  # mulch_start_date 
  output$mul_start_date <- renderUI({
    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
      airDatepickerInput("mulch_start_date",
                         "Start date",
                         clearButton = T,
                         autoClose = T,
                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                         placeholder = "yyyy-mm-dd",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("mulch_start_date",
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
      airDatepickerInput("mulch_remove_start_date",
                         "Mulch removal start date",
                         clearButton = T,
                         autoClose = T,
                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                         placeholder = "yyyy-mm-dd",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("mulch_remove_start_date",
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
      airDatepickerInput("mulch_remove_end_date",
                         "Mulch removal end date",
                         clearButton = T,
                         autoClose = T,
                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                         placeholder = "yyyy-mm-dd",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("mulch_remove_end_date",
                         "Mulch removal end date",
                         clearButton = T,
                         autoClose = T,
                         placeholder = "yyyy-mm-dd"
      )
    }
  })
  
  # residue_start_date 
  output$res_start_date <- renderUI({
    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
      airDatepickerInput("residue_start_date",
                         "Start date",
                         clearButton = T,
                         autoClose = T,
                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                         placeholder = "yyyy-mm-dd",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("residue_start_date",
                         "Start date",
                         clearButton = T,
                         autoClose = T,
                         placeholder = "yyyy-mm-dd"
      )
    }
  })
  
  # residure_end_date
  output$res_end_date <- renderUI({
    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
      airDatepickerInput("residure_end_date",
                         "End date",
                         clearButton = T,
                         autoClose = T,
                         #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                         placeholder = "yyyy-mm-dd",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("residure_end_date",
                         "End date",
                         clearButton = T,
                         autoClose = T,
                         placeholder = "yyyy-mm-dd"
      )
    }
  })
  
  ## Planting and Transplanting
  # planting_start_date 
  # output$pl_start_date <- renderUI({
  #   if (!is.null(input$fbDesign_project_time_line)) {
  #     airDatepickerInput("planting_start_date",
  #                        "Start date",
  #                        clearButton = T,
  #                        autoClose = T,
  #                        value = as.Date(input$fbDesign_project_time_line[1]) + 1,
  #                        placeholder = "yyyy-mm-dd",
  #                        minDate = as.Date(input$fbDesign_project_time_line[1]) + 1,
  #                        maxDate = as.Date(input$fbDesign_project_time_line[2]) + 1
  #                        
  #     )
  #   } else {
  #     airDatepickerInput("planting_start_date",
  #                        "Start date",
  #                        clearButton = T,
  #                        autoClose = T,
  #                        placeholder = "yyyy-mm-dd"
  #     )
  #   }
  # })
  # 
  # # planting_end_date
  # output$pl_end_date <- renderUI({
  #   if (!is.null(input$fbDesign_project_time_line)) {
  #     airDatepickerInput("planting_end_date",
  #                        "End date",
  #                        clearButton = T,
  #                        autoClose = T,
  #                        value = as.Date(input$fbDesign_project_time_line[1]) + 1,
  #                        placeholder = "yyyy-mm-dd",
  #                        minDate = as.Date(input$fbDesign_project_time_line[1]) + 1,
  #                        maxDate = as.Date(input$fbDesign_project_time_line[2]) + 1
  #                        
  #     )
  #   } else {
  #     airDatepickerInput("planting_end_date",
  #                        "End date",
  #                        clearButton = T,
  #                        autoClose = T,
  #                        placeholder = "yyyy-mm-dd"
  #     )
  #   }
  # })
  
  # transplanting_start_date 
  # output$trans_start_date <- renderUI({
  #   if (!is.null(input$fbDesign_project_time_line)) {
  #     airDatepickerInput("transplanting_start_date",
  #                        "Start date",
  #                        clearButton = T,
  #                        autoClose = T,
  #                        value = as.Date(input$fbDesign_project_time_line[1]) + 1,
  #                        placeholder = "yyyy-mm-dd",
  #                        minDate = as.Date(input$fbDesign_project_time_line[1]) + 1,
  #                        maxDate = as.Date(input$fbDesign_project_time_line[2]) + 1
  #                        
  #     )
  #   } else {
  #     airDatepickerInput("transplanting_start_date",
  #                        "Start date",
  #                        clearButton = T,
  #                        autoClose = T,
  #                        placeholder = "yyyy-mm-dd"
  #     )
  #   }
  # })
  # 
  # # transplanting_end_date
  # output$trans_end_date <- renderUI({
  #   if (!is.null(input$fbDesign_project_time_line)) {
  #     airDatepickerInput("transplanting_end_date",
  #                        "End date",
  #                        clearButton = T,
  #                        autoClose = T,
  #                        value = as.Date(input$fbDesign_project_time_line[1]) + 1,
  #                        placeholder = "yyyy-mm-dd",
  #                        minDate = as.Date(input$fbDesign_project_time_line[1]) + 1,
  #                        maxDate = as.Date(input$fbDesign_project_time_line[2]) + 1
  #                        
  #     )
  #   } else {
  #     airDatepickerInput("transplanting_end_date",
  #                        "End date",
  #                        clearButton = T,
  #                        autoClose = T,
  #                        placeholder = "yyyy-mm-dd"
  #     )
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



  ################ Experiment ###########################################################

  experimentVars <- reactiveValues()
  
  # Funding Agency
  experimentVars$num_FA <- 0 #porque todavia no dibujo
  experimentVars$DEFAULT_FA <- 1 #numero por defecto combos
  experimentVars$ids_FA <- c()
  
  # Project Entities  
  experimentVars$num_PE <- 0
  experimentVars$DEFAULT_PE <- 1
  experimentVars$ids_PE <- c()
  
  # Experiment Leads
  experimentVars$num_EL <- 0
  experimentVars$DEFAULT_EL <- 1
  experimentVars$ids_EL <- c()
  
  observeEvent(input$addFundingAgency,{
    defaultBoxes = experimentVars$DEFAULT_FA
    if(experimentVars$num_FA >= 1){
      insertBoxFundingAgency(experimentVars$num_FA+ 1)
    }
  })
  
  observeEvent(input$addManagEntity,{
    defaultBoxes = experimentVars$DEFAULT_PE
    if(experimentVars$num_PE  >= 1){
      insertBoxManagEntity(experimentVars$num_PE + 1)
    }
  })
  
  observeEvent(input$addExperimentLeads,{
    defaultBoxes = experimentVars$DEFAULT_EL
    if(experimentVars$num_EL >= 1){
      insertBoxExperimentLead(experimentVars$num_EL + 1)
    }
  })
  
  observe({
    if(experimentVars$num_FA == 0){
      default <- experimentVars$DEFAULT_FA
      for(i in 1:default){
        insertBoxFundingAgency(i)
      }
    }
    if(experimentVars$num_PE == 0){
      default <- experimentVars$DEFAULT_PE
      for(i in 1:default){
        insertBoxManagEntity(i)
      }
    }
    if(experimentVars$num_EL == 0){
      default <- experimentVars$DEFAULT_EL
      for(i in 1:default){
        insertBoxExperimentLead(i)
      }
    }
  })
  
  insertBoxFundingAgency <- function(index){
     insertUI(
       selector = "#fr_fundingAgency_boxes",
       where = "beforeBegin",
       ui = getUiFundingAgency(index)
     )
    # output[[paste0("box_title_", experimentVars$ids_FA[index])]] <- renderText(paste0("Funding Agency #", index))
    experimentVars$num_FA <- experimentVars$num_FA + 1
  }
  
  insertBoxManagEntity <- function(index){
    insertUI(
      selector = "#fr_managementEntities_boxes",
      where = "beforeBegin",
      ui = getUiProjectEntity(index)
    )
    # output[[paste0("box_title_", fundingAgency$ids[index])]] <- renderText(paste0("Funding Agency #", index))
    experimentVars$num_PE <- experimentVars$num_PE + 1
  }
  
  insertBoxExperimentLead <- function(index){
    insertUI(
      selector = "#fr_experimentLeads_boxes",
      where = "beforeBegin",
      ui = getUiExperimentLead(index)
    )
    # output[[paste0("box_title_", fundingAgency$ids[index])]] <- renderText(paste0("Funding Agency #", index))
    experimentVars$num_EL <- experimentVars$num_EL + 1
  }
  
  getUiFundingAgency <- function(index){
    str_id <- stri_rand_strings(1, 8,  '[A-Z]')
    experimentVars$ids_FA <- c(experimentVars$ids_FA, paste0("FA_", str_id))
    
    fluidRow(id= paste0("fl_box_fundingAgency_", str_id), 
             # box(title = uiOutput(paste0("box_title_FA_", str_id)), solidHeader = TRUE, status = "warning", width=12,
             box(title = "Funding Agency", solidHeader = TRUE, status = "warning", width=12,
                 column(12, offset = 0, style='padding:0px; text-align:right; ',  actionButton(paste0("closeBox_FA_", str_id), "", icon("close"))),
                 fluidRow(
                   column(6, 
                          selectizeInput(paste0("designFieldbook_fundAgencyType_", str_id), "Funding agency type", multiple = TRUE,
                                         options = list(placeholder ="Select...", maxItems =1),
                                         choices = c("Academic institution",
                                                     "CGIAR center",
                                                     "Farmer organization",
                                                     "Finance or insurance entity",
                                                     "Foundation or public charity",
                                                     "Government or government agency",
                                                     "International NGO",
                                                     "National NGO",
                                                     "Private sector entity",
                                                     "Other")
                          ),
                          hidden(textInput(paste0("designFieldbook_fundAgencyType_", str_id, "_other"), "", value = ""))
                          
                    ), 
                    column(6,
                      textInput(paste0("designFieldbook_fundAgencyType_name_", str_id), "Funding agency name")
                    )
                 )
             )
      
    )
    
  }
  
  getUiProjectEntity <- function(index){
    str_id <- stri_rand_strings(1, 8,  '[A-Z]')
    experimentVars$ids_PE <- c(experimentVars$ids_PE, paste0("PE_", str_id))
    
    fluidRow(id = paste0("fl_box_exp_ent_", str_id),
             box(title = "Project management", solidHeader = TRUE, status = "warning", width=12,
                 column(12, offset = 0, style='padding:0px; text-align:right; ',  actionButton(paste0("closeBox_PE_", str_id), "", icon("close"))),
                 fluidRow(
                   column(width = 4,
                          selectizeInput(paste0("projEntity_", str_id), "Project management entity", multiple =T, options = list(maxItems =1, placeholder="Select one.."), choices=
                                           c("CGIAR center",
                                             "Other"
                                           )
                          )
                   ),
                   
                   conditionalPanel(paste0("input.projEntity_", str_id, " == 'CGIAR center'"),
                                    column(width = 4,
                                           selectizeInput(paste0("contCenter_", str_id), "Choose CGIAR center", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c(
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
                                           selectizeInput(paste0("contCRP_", str_id), "Contributor CRP", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = sort(c(
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
                          hidden(textInput(paste0("projEntity_", str_id, "_other"), "", value = ""))
                   )
                 )
             ) #end box
    )
  }
  
  getUiExperimentLead <- function(index){
    str_id <- stri_rand_strings(1, 8,  '[A-Z]')
    experimentVars$ids_EL <- c(experimentVars$ids_EL, paste0("EL_", str_id))
    
    fluidRow(
      id = paste0("fl_box_exp_lead_", str_id),
      box(title = "Experiment lead organization, if different from project management entity", solidHeader = TRUE, status = "warning", width=12,
          column(12, offset = 0, style='padding:0px; text-align:right; ',  actionButton(paste0("closeBox_EL_", str_id), "", icon("close"))),
          fluidRow(
            column(width = 6,
                   
                   selectizeInput(paste0("projLeadEnt_", str_id), "Experiment, lead organization type", selected="CGIAR center", multiple =T, options = list(maxItems =1, placeholder="Select one..."), choices=
                                    c("CGIAR center",
                                      "Other"
                                    )
                   ),
                   
                   conditionalPanel(paste0("input.projLeadEnt_", str_id, " == 'CGIAR center'"),
                                    
                                    selectizeInput(paste0("tLeadCenter_", str_id), "Choose CGIAR center", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c(
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
                   conditionalPanel(paste0("input.projLeadEnt_", str_id, " == 'Other'"),
                                    selectizeInput(paste0("lead_org_type_1_", str_id), "",multiple = TRUE,  options = list(maxItems =1, placeholder ="Select one..."),
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
                                    hidden(textInput(paste0("lead_org_type_1_", str_id, "_other"), "")),
                                    textInput(paste0("leadNameOther_", str_id), "Experiment, lead organization name", value = "")
                   ),
                   textInput(inputId = paste0("expLead_", str_id), label = "Experiment lead person / Primary Investigator", value = "")
            )
          )
      ) #end box
      
    )
    
  }


  ################# fin experiment ######################################################



  ################# personnel ######################################################
  
  personnelVars <- reactiveValues()
  personnelVars$num <- 0
  personnelVars$DEFAULT <- 1
  personnelVars$ids <- c()
  
  observeEvent(input$btLoadMyInfoPersonnel, {
    print(personnelVars$ids[1])
     if(session$userData$logged && length(personnelVars$ids)>0){ 
       var <- unlist(strsplit(personnelVars$ids[1],"_"))
       updateTextInput(session, paste0("person_firstName_", var[2]), value=session$userData$userFname)
       updateTextInput(session, paste0("person_lastName_", var[2]), value=session$userData$userLname)
       updateTextInput(session, paste0("person_email_", var[2]), value=session$userData$userMail)
     }
   })

  observeEvent(input$addPersonnel,{
    defaultBoxes = personnelVars$DEFAULT
    if(personnelVars$num >= 1){
      insertBoxPersonnel(personnelVars$num + 1)
    }
  })
  
  insertBoxPersonnel <- function(index){
    insertUI(
      selector = "#fr_personnel_boxes",
      where = "beforeBegin",
      ui = getUiPersonnel(index)
    )
    personnelVars$num <- personnelVars$num + 1
  }
  
  getUiPersonnel <- function(index){
    str_id <- stri_rand_strings(1, 8,  '[A-Z]')
    personnelVars$ids <- c(personnelVars$ids, paste0("PERS_", str_id))
    
    fluidRow(id = paste0("fr_personnel_box_", str_id),
      box(
        title = tagList(shiny::icon("user"), "Personnel"), solidHeader = TRUE, status = "warning", width=12,
        column( id= paste0("col_close_PERS", str_id), 12, offset = 0, style='padding:0px; text-align:right; ',  actionButton(paste0("closeBox_PERS_", str_id), "", icon("close"))),
        fluidRow(
          column(width=6,
            selectizeInput(paste0("personnel_type_", str_id), "Person type", multiple=TRUE,
                           options = list(maxItems =1, placeholder= "Select one..."),
                           choices = c("Farmer",
                                       "Researcher",
                                       "Student",
                                       "Research station worker",
                                       "Extension agent",
                                       "Faculty member",
                                       "Other")
            ),
            
            hidden(textInput(paste0("personnel_type_", str_id, "_other"), "", value = "")),
            textInput(paste0("person_firstName_", str_id), "Person, first name", value = ""),
            textInput(paste0("person_lastName_", str_id), "Person, last name", value = "")
          ),
          
          column(
            width=6,
            textInput(paste0("person_email_", str_id), "Person email", value = ""),
            selectizeInput(paste0("person_affiliation_", str_id), "Person, affiliation", multiple =T,
                           options = list(maxItems =1, placeholder="Select one.."),
                           choices = c("CGIAR Center",
                                       "Other")
            ),
            
            conditionalPanel(paste0("input.person_affiliation_", str_id," == 'CGIAR Center'" ),
                             selectizeInput(paste0("person_center_", str_id), "Organization name", multiple = TRUE,
                                            options = list(maxItems =1, placeholder ="Select one..."),
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
            
            hidden(textInput(paste0("person_email_", str_id, "_other"), "", value = "")),
            textInput(inputId = paste0("person_orcid_", str_id),
                      label = HTML("Person, ORCID id if available (if not, consider <a href='https://orcid.org/register' target='_blank'>registering</a>!)"),
                      value = ""
            )
          )
          
          
        )
      )#end Box
    )
    
  }
  
  observe({
    if(personnelVars$num == 0){
      default <- personnelVars$DEFAULT
      for(i in 1:default){
        insertBoxPersonnel(i)
      }
    }
  })
  
  ################# fin personnel ######################################################

  ## listening when a close button of a box is pressed
  observeEvent(input$closeBox_button, {
    var <- unlist(strsplit(input$closeBox_button_id,"_"))
    arr_keys <- c()
    len <- 0 
    
    mselector <- "XXXXXXXXXX"
    mselector2 <- "XXXXXXXXXX"
    title <- ""
    if(var[2] == "FA" && experimentVars$num_FA > experimentVars$DEFAULT_FA){
      mselector = paste0("#fl_box_fundingAgency_", var[3])
      experimentVars$num_FA <- experimentVars$num_FA - 1
      aux <- experimentVars$ids_FA
      experimentVars$ids_FA <- aux[! aux %in% paste0("FA_",var[3])]
      arr_keys <- experimentVars$ids_FA
      len <- length(arr_keys)
      title <- "Fundind Agency"
    }
    
    if(var[2] == "PE" && experimentVars$num_PE > experimentVars$DEFAULT_PE){
      mselector = paste0("#fl_box_exp_ent_", var[3])
      experimentVars$num_PE <- experimentVars$num_PE - 1
      aux <- experimentVars$ids_PE
      experimentVars$ids_PE <- aux[! aux %in% paste0("PE_",var[3])]
      arr_keys <- experimentVars$num_PE
      len <- length(arr_keys)
      title <- "Project Entity"
    }
    
    if(var[2] == "EL" && experimentVars$num_EL > experimentVars$DEFAULT_EL){
      mselector = paste0("#fl_box_exp_lead_", var[3])
      experimentVars$num_EL <- experimentVars$num_EL - 1
      aux <- experimentVars$ids_EL
      experimentVars$ids_EL <- aux[! aux %in% paste0("EL_",var[3])]
      arr_keys <- experimentVars$num_EL
      len <- length(arr_keys)
      title <- "Experiment Lead"
    }
    
    if(var[2] == "PERS" && personnelVars$num > personnelVars$DEFAULT){
      mselector = paste0("#fr_personnel_box_", var[3])
      personnelVars$num <- personnelVars$num - 1
      aux <- personnelVars$ids
      personnelVars$ids <- aux[! aux %in% paste0("PERS_",var[3])]
      arr_keys <- personnelVars$ids
      len <- length(arr_keys)
      title <- "Personnel"
    }
    
    if(var[2] == "IC" && intercropVars$num > intercropVars$DEFAULT ){
      mselector = paste0("#fr_box_intercrop_", var[3])
      mselector2 = paste0("#intercrop_rows_crop_", var[3])
      intercropVars$num <- intercropVars$num - 1
      aux <- intercropVars$ids
      intercropVars$ids <- aux[! aux %in% paste0("IC_",var[3])]
      arr_keys <- intercropVars$ids
      len <- length(arr_keys)
      last <-unlist(strsplit(intercropVars$ids[intercropVars$num],"_"))
      output[[paste0("intercropX_row_crop_", last[2])]] <- renderText("")
      cropsVar$CropsSelectedInterCrop[[paste0('C', var[3])]] <- NULL
      title <- "Crop"
      
      removeTab(inputId = "intercropPhenoTabs",target = paste0("intercrop_tab_pheno_",var[3])) 
      removeTab(inputId = "intercropMeasuTabs",target = paste0("intercrop_tab_measu_",var[3]))
      removeAgroBoxes(var[3])
      intercropVars$pheno[[var[3]]] <- NULL
                                    
    }
    
    if(var[2] == "FF" && designVars$num_FULL > designVars$DEFAULT_FULL){
      mselector = paste0("#full_factor_box_", var[3])
      designVars$num_FULL <- designVars$num_FULL - 1
      aux <- designVars$ids_FULL
      designVars$ids_FULL <- aux[! aux %in% paste0("FF_",var[3])]
      arr_keys <- designVars$ids_FULL
      len <- length(arr_keys)
      designVars$choices_2[[var[3]]] <- NULL
      designVars$choices_3[[var[3]]] <- NULL
      removeTabSoilFertility(var[3])
      title <- "Factor"
    }
    
    if(var[2] == "NFF" && designVars$num_NFULL > designVars$DEFAULT_NFULL){
      mselector = paste0("#not_full_factor_box_", var[3])
      designVars$num_NFULL <- designVars$num_NFULL - 1
      aux <- designVars$ids_NFULL
      designVars$ids_NFULL <- aux[! aux %in% paste0("NFF_",var[3])]
      arr_keys <- designVars$ids_NFULL
      len <- length(arr_keys)
      designVars$choices_2[[var[3]]] <- NULL
      designVars$choices_3[[var[3]]] <- NULL
      title <- "Factor"
      removeTabSoilFertility(var[3])
      
      removeUI(
        selector = paste0("#col_NFF_", var[3]),
        immediate = T
      )
      
      updateSummaryAll()
      
    }
    
    
    removeUI(
      selector = mselector, 
      immediate = T
    )
    
    if(mselector2 != "XXXXXXXXXX"){
      removeUI(
        selector = mselector2, 
        immediate = T
      )
    }
    
    # if(len > 0){
    #   for(mkey in arr_keys){
    #     # print(paste0(mkey , "----", cnt))
    #     # str <- toString(cnt)
    #     # output[[paste0("box_title_", arr_keys[mkey])]] <- renderText(paste0("Funding Agency #", mkey))
    #     output[[paste0("box_title_", mkey)]] <-  renderText(paste0(title, " #", match(mkey, arr_keys)))
    #     # isolate(updateBoxTitle(paste0("box_title_", arr_keys[mkey]), paste0(title, " #", match(mkey, arr_keys))))
    #   }
    # }
    
    
    # if(len > 0){
    #   shinyjs::hidden(paste0("closeBox_", var[2], "_", arr_keys[1]))
    #   shinyjs::hidden(paste0("closeBox_", var[2], "_", arr_keys[2]))
    #   for(mkey in 2:len){
    #     shinyjs::show(paste0("closeBox_", var[2], "_", arr_keys[mkey]))
    #   }
    # }
    
  })
  
  ################# site ######################################################

  ### to add when making it dynamic

  ################# fin site ######################################################



  ################# Crop ######################################################

  ### observe for selectize of crops for intercropping
  
  
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
  intercropVars$ids <- c()
  
  intercropVars$pheno <- list()
  intercropVars$measu <- list()
  
  
  ## listening the ADD intercrop button
  observeEvent(input$addIntercrop,{
    defaultBoxes = intercropVars$DEFAULT
    if(intercropVars$num >= 1){
      insertBoxIntercrop(intercropVars$num + 1)
      tt <- unlist(str_split(intercropVars$ids[intercropVars$num], "_"))
      xt <- unlist(str_split(intercropVars$ids[intercropVars$num-1], "_"))
      mtarget <- paste0("intercrop_tab_measu_",xt[2])
      ptarget <- paste0("intercrop_tab_pheno_",xt[2])
      isolate(insertTabInterCrop(tt[2], mtarget, ptarget))
      isolate(drawAgroBoxes(tt[2]))
    }
  })
  
  ## drawing default number of boxes
  observe({
    if(intercropVars$num == 0){
      default <- intercropVars$DEFAULT
      for(i in 1:default){
        insertBoxIntercrop(i)
      }
    }
  })
  
  ## function to insert intercrop box
  insertBoxIntercrop <- function(index){
    str_id <- stri_rand_strings(1, 8,  '[A-Z]')
    intercropVars$ids <- c(intercropVars$ids, paste0("IC_", str_id))
    
    intercropVars$num <- intercropVars$num + 1
    prev <-unlist(strsplit(intercropVars$ids[intercropVars$num -1],"_"))
    output[[paste0("intercropName_row_crop_", str_id)]] <- renderText(paste0("Crop"))
    output[[paste0("intercropX_row_crop_", prev[2])]] <- renderText("X")
    
    insertUI(
      selector = "#fr_intercrop_boxes",
      where = "beforeBegin",
      ui = getUiIntercropBox(index,str_id)
    )
    
    insertUI(
      selector = "#fr_intercrop_geometry_boxes",
      where = "beforeBegin",
      ui = getUiIntercropGeometryCol(index,str_id)
    )
  }
  
  
  getUiIntercropBox <- function(index, str_id){
    fluidRow(id= paste0("fr_box_intercrop_", str_id),
             box(
               title = paste0("Crop #"), solidHeader = TRUE, status = "warning", width=12,
               column(12, offset = 0, style='padding:0px; text-align:right; ',  actionButton(paste0("closeBox_IC_", str_id), "", icon("close"))),
               fluidRow(
                 column(
                   width = 6,
                   selectizeInput(paste0("cropCommonNameInter_", str_id), label="Select crop", selected=NULL, multiple = T , options = list(maxItems =1, placeholder ="Select crop"),
                                  choices = c("Cassava",
                                              "Common bean",
                                              "Maize",
                                              "Potato",
                                              "Rice",
                                              "Sweetpotato",
                                              "Wheat",
                                              "Other")
                   ),
                   hidden(textInput(paste0("cropCommonNameInter_", str_id, "_other"), "", value = ""))
                 ),
                 column(
                   width = 6,
                   
                   selectizeInput(inputId = paste0("cropVarietyName_", str_id), label = "Crop variety name(s)", choices = c(), multiple = T,
                                  options = list('create' = TRUE)
                   )
                 )
               )
             )
    )
  }
  
  getUiIntercropGeometryCol <- function(index, str_id){
    column(3, id= paste0("intercrop_rows_crop_", str_id), style='padding:0px;',
           column(5, offset = 0, style='padding:25px 2px 0px 0px; text-align:center; word-wrap: break-word;', uiOutput(paste0("intercropName_row_crop_", str_id))),
           column(4, offset = 0, style='padding:0px; text-align:left; ', textInput(paste0("intercropValue_row_crop_", str_id), "")),
           column(3, offset = 0, style='padding:25px 0px 0px 20px; text-align:center; word-wrap: break-word;',
                  fluidRow(
                    column(9, offset = 0, style='padding:0px; text-align:center;', "row(s)"),
                    column(3, offset = 0, style='padding:0px; text-align:center;',uiOutput(paste0("intercropX_row_crop_", str_id)))
                  )
           )
    )
  }
  
  
  ## when intercrop is selected
  observeEvent(input$cropBoxInterVar, {
    crop_order <- gsub('cropCommonNameInter_', '', input$cropBoxInterVarId)
    value <- input[[input$cropBoxInterVarId]]
    xtitle <- "Crop"
    if(is.null(value)){
      output[[paste0("intercropName_row_crop_", crop_order)]] <- renderText(paste0("Crop"))
      cropsVar$CropsSelectedInterCrop[[paste0('C', crop_order)]] <- NULL
    }
    else{
      if(value == "Other"){
        if(input[[paste0(input$cropBoxInterVarId, "_other")]] == ''){
          output[[paste0("intercropName_row_crop_", crop_order)]] <- renderText(value)
          xtitle <- "Other"
        }
        else {
          output[[paste0("intercropName_row_crop_", crop_order)]] <- renderText(input[[paste0(input$cropBoxInterVarId, "_other")]])
          xtitle <- input[[paste0(input$cropBoxInterVarId, "_other")]]
        }
      }
      else{
        output[[paste0("intercropName_row_crop_", crop_order)]] <- renderText(value)
        xtitle <- value
      }
      cropsVar$CropsSelectedInterCrop[[paste0('C', crop_order)]] <- value
    }
    
    isolate(renameTab(paste0("intercrop_tab_measu_title_",crop_order), paste0(xtitle, " Measurement")))
    isolate(renameTab(paste0("intercrop_tab_pheno_title_",crop_order), paste0(xtitle, " Phenology")))
    renameAgroBoxes(crop_order,crop_order, xtitle)
    updateCropMeasurementTable(crop_order,value)
    
  })
  
  ## when 'other crop' name is filled
  observeEvent(input$cropBoxInterVarOther,{
    crop_order <- gsub('cropCommonNameInter_', '', input$cropBoxInterVarOtherId)
    crop_order <- gsub('_other', '', crop_order)
    
    mtext <- input[[input$cropBoxInterVarOtherId]]
    if(mtext == "") mtext <- "Other"
    output[[paste0("intercropName_row_crop_", crop_order)]] <- renderText(mtext)
    
    
    mtitle <- paste0(mtext, " Measurement")
    ptitle <- paste0(mtext, " Phenology")
    
    
    isolate(renameTab(paste0("intercrop_tab_measu_title_",crop_order), mtitle))
    isolate(renameTab(paste0("intercrop_tab_pheno_title_",crop_order), ptitle))
    renameAgroBoxes(crop_order,crop_order, mtext)
    updateCropMeasurementTable(crop_order, "Other")
    
  })


  ################# fin crop ######################################################



  ################# Design ######################################################

  path <- fbglobal::get_base_dir()
  # field operations as list of factors
  fp <- file.path(path, "listFactors_v7.rds")
  factors <- as.data.frame(readRDS(fp))

  ### para la tabla del treatment description cuando
  ### no es full factorial
  treatmentValues <- reactiveValues()
  
  designVars <- reactiveValues()
  designVars$num_FULL <- 0
  designVars$num_NFULL <- 0
  designVars$DEFAULT_FULL <- 2
  designVars$DEFAULT_NFULL <- 1
  designVars$DEFAULT_TREAT_NFULL <- 2
  designVars$ids_FULL <- c()
  designVars$ids_NFULL <- c()
  designVars$choices_2 <- list()
  designVars$choices_3 <- list()
  designVars$soilLevels <- list()
  designVars$main_list <- unique(factors$GROUP)
  
  ### variables para manejo de el numero de factores seleccionados
  numFactors <- reactiveValues()
  numFactors$numFull <- 0
  numFactors$numNotFull <- 0
  
  ## variables para numero de treatments seleccionados en NOT FULL FACTORIAL
  num <- reactiveValues()
  num$currNumReplications <- 2 ## valor por defecto
  num$numRepAux <- 0

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

  observeEvent(input$btAddFullFactorial,{
    if(designVars$num_FULL >= 1){
      insertBoxFullFactorial(designVars$num_FULL + 1)
    }
  })
  
  observeEvent(input$btAddNotFullFactorial,{
    if(designVars$num_NFULL >= 1){
      insertBoxNotFullFactorial(designVars$num_NFULL + 1)
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
    
    designVars$choices_2 <- list()
    designVars$choices_3 <- list()
    
    ## verificando si es o no full factorial
    if(input$fullFactorialRB == "Yes"){
      num$numRepAux <- 0
      
      nfull_ids <- designVars$ids_NFULL
      
      for(id_val in nfull_ids){
        id <-unlist(strsplit(id_val,"_"))
        removeUI(
          selector = paste0("#not_full_factor_box_", id[2]),
          immediate = T
        )
      }
      designVars$num_NFULL <- 0
      designVars$ids_NFULL <- c()
      
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
                             shiny::selectInput("designFieldbook_agrofims_r_y", rep_title , 2:1000, 2 ) #issues16: https://github.com/AGROFIMS/hagrofims/issues/16
                      ),
                      fluidRow(id="full_factor_input"),
                      column(12,actionButton("btAddFullFactorial", "Add factor"))
               )
        )
      )
    }
    
    else if(input$fullFactorialRB == "No"){
      full_ids <- designVars$ids_FULL
      
      for(id_value in full_ids){
        id <-unlist(strsplit(id_value,"_"))
        removeUI(
          selector = paste0("#full_factor_box_", id[2]),
          immediate = T
        )
      }
      
      designVars$num_FULL <- 0
      designVars$ids_FULL <- c()
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
                               shiny::selectInput("designFieldbook_agrofims_t_n", "Number of treatments", 2:100, designVars$DEFAULT_TREAT_NFULL )
                        ),
                        column(width = 4,
                               shiny::selectInput("designFieldbook_agrofims_r_n", rep_title, 2:1000, 2 ) #issue 16: https://github.com/AGROFIMS/hagrofims/issues/16
                        ),
                        fluidRow(id="not_full_factor_input"),
                        fluidRow(column(12,column(12, actionButton("btAddNotFullFactorial", "Add factor")))),
                        br(), br(),
                        column(id="col_NFF_consolid", width=4,
                               HTML("<center>"),"Treatment",HTML("</center>"), 
                               fluidRow(id="fr_col_NFF_cons")
                        ),
                        fluidRow(id="not_full_factor_table")
                  )
        )
      )
      
    }
    
    deleteAllTabsSoilFertility() ## cleaning all soil fertility tabs
    
  })
  
  ## draw default values
  observe({
    if(designVars$num_NFULL == 0 && input$fullFactorialRB == "No"){
      default <- designVars$DEFAULT_NFULL
      for(i in 1:default){
        insertBoxNotFullFactorial(i)
      }
      
    }
    if(designVars$num_FULL == 0 && input$fullFactorialRB == "Yes"){
      default <- designVars$DEFAULT_FULL
      for(i in 1:default){
        insertBoxFullFactorial(i)
      }
      
    }
  })
  
  insertBoxFullFactorial <- function(index){
    str_id <- stri_rand_strings(1, 8,  '[A-Z]')
    designVars$ids_FULL <- c(designVars$ids_FULL, paste0("FF_", str_id))
    insertUI(
      selector = "#full_factor_input",
      where = "beforeBegin",
      ui = getUiBoxFullFactorial(index,str_id)
    )
    designVars$num_FULL <- designVars$num_FULL + 1
    designVars$choices_2[[str_id]] <- NULL
    designVars$choices_3[[str_id]] <- NULL
  }
  
  insertBoxNotFullFactorial <- function(index){
    str_id <- stri_rand_strings(1, 8,  '[A-Z]')
    designVars$ids_NFULL <- c(designVars$ids_NFULL, paste0("NFF_", str_id))
    
    insertUI(
      selector = "#not_full_factor_input",
      where = "beforeBegin",
      ui = getUiBoxNotFullFactorial(index,str_id)
    )
    designVars$num_NFULL <- designVars$num_NFULL + 1
    designVars$choices_2[[str_id]] <- NULL
    designVars$choices_3[[str_id]] <- NULL
    
    drawNewColumnNFF(str_id)
  }
  
  
  drawNewColumnNFF <- function(index){
    
    if(num$numRepAux == 0){
      for(i in 1:num$currNumReplications){
        insertSummaryEntry(i)
      }
      num$numRepAux <- 1
    }
    
    insertUI(
      selector = "#not_full_factor_table",
      where = "beforeBegin",
      ui = column(id=paste0("col_NFF_", index), width=2,
                  HTML("<center>"),uiOutput(paste0("title_col_NFF_", index)),HTML("</center>"), 
                  fluidRow(id=paste0("fr_col_NFF_", index))
      )
    )
    

    
    value <- designVars$choices_3[[index]]
    
    if(is.null(value)) value <- "Factor"
    output[[paste0("title_col_NFF_", index)]] <- renderText({value})
    
    num_treat <- input$designFieldbook_agrofims_t_n
    if(is.null(num_treat)) num_treat <- designVars$DEFAULT_TREAT_NFULL
    else num_treat <- as.integer(num_treat)
    if( num_treat < 1) return()
    
    for(i in 1:num_treat){
      insertUI(
        selector = paste0("#fr_col_NFF_", index),
        where = "beforeBegin", 
        ui = column(id=paste0("aux_col_NFF_", index, "_", i), width=12, uiOutput(paste0("ui_col_NFF_", index, "_", i)))
      )
      isolate(drawInputNFF(index, i))
      updateSummary(i)
     
    }
  }
  
  deleteSummaryEntry <- function(treat_num){
    removeUI(
      selector = paste0("#row_NFF_summ_", treat_num), 
      immediate = T
    )
  }
  
  insertSummaryEntry <- function(treat_num){
    cn <- length(designVars$ids_NFULL)
    if(!is.null(cn) && cn > 0 ){
      repl <- rep("-", cn)
      xvalue <- paste(repl, collapse = " / ")
    }
    else{
      xvalue <- ''
    }
    
    insertUI(
      selector = "#fr_col_NFF_cons", 
      where = "beforeBegin",
      ui = column(12, id = paste0("row_NFF_summ_", treat_num), 
                HTML('<center>'),  disabled(textInput(paste0("ui_NFF_summ_", treat_num), "" , value=xvalue)), HTML('</center>')
            )
    )
     
      
  }
  
  observeEvent(input$changeInputSummary,{
    id <-input$changeInputSummaryId
    vals <- unlist(str_split(id, "_"))
    updateSummary(vals[5])
  })
  
  updateSummaryAll <- function(num = NULL){
    if(is.null(num)){
      num_treat <- input$designFieldbook_agrofims_t_n
      if(is.null(num_treat)) num_treat <- designVars$DEFAULT_TREAT_NFULL
    }
    else{
      num_treat <-  num
    }
    
    for(i in 1:num_treat) {
      updateSummary(i)
    }
  }
  
  duplicateSummary <- function(prev, index, treat_index){
    ids <- designVars$ids_NFULL
    l <- c()
    for(id in ids){
      vals <- unlist(str_split(id, "_"))
      if(vals[2] == index ){
        val <-   val <- input[[paste0("input_factor_treatment_", prev, "_", treat_index)]]
      }
      else{
        val <- input[[paste0("input_factor_treatment_", vals[2], "_", treat_index)]]  
      }
      
      
      if(typeof(val) == 'double') {
        val = as.character(val)
      }
      
      if(is.null(val) || val == '')  val <- '-'
      l<- c(l, val)
    }
    updateTextInput(session, inputId = paste0("ui_NFF_summ_", treat_index), value = paste(l, collapse = " / "))
  }
  
  updateSummary <- function(treat_index){
    
    ids <- designVars$ids_NFULL
     l <- c()
     for(id in ids){
       vals <- unlist(str_split(id, "_"))
       val <- input[[paste0("input_factor_treatment_", vals[2], "_", treat_index)]]
       
       if(typeof(val) == 'double') {
         val = as.character(val)
       }
       
       if(is.null(val) || val == '')  val <- '-'
       l<- c(l, val)
     }
     updateTextInput(session, inputId = paste0("ui_NFF_summ_", treat_index), value = paste(l, collapse = " / "))
  }
  
  drawInputNFF <- function(index, order){
    
    sel_1 <- input[[paste0("sel_factor_", index, "_1")]]
    sel_2 <- input[[paste0("sel_factor_", index, "_2")]]
    sel_3 <- input[[paste0("sel_factor_", index, "_3")]]
    
    
    if(!is.null(sel_1)  && sel_1 == "Soil fertility"){
      nLevels <- input[[paste0("numLevels_tabSoil_", index)]]
      
      if(is.null(nLevels)) nLevels <- 2
      options_str <- c()
      
      for(i in 1:nLevels){
        options_str <- c(options_str, paste0("Level ", i))
      }
      
      output[[paste0("ui_col_NFF_", index, "_", order)]] <- renderUI(
        fluidRow(column(12, style="margin-top:-3px;margin-bottom:-1.5px;",
        selectizeInput(paste0("input_factor_treatment_", index, '_', order), label = "", multiple = TRUE, 
                              options = list(maxItems =1, placeholder ="Select one..."), choices = options_str)
        ))
      )
      return()
    }
    
    if(is.null(sel_2)){
      updateSelectInput(session,  inputId = paste0("sel_factor_", index, "_3"), choices = NULL)
    }
    
    
    if(is.null(sel_1) || is_null(sel_2) || is_null(sel_3)){
      output[[paste0("ui_col_NFF_", index, "_", order)]] <- renderUI(disabled(textInput(paste0("input_factor_treatment_", index, "_", order), "")))
      return()
    }
    
    aux <- dplyr::filter(factors,GROUP==sel_1 & SUBGROUP==sel_2 & FACTOR==sel_3)
    
    if(is.null(aux) || nrow(aux)<1){
        output[[paste0("ui_col_NFF_", index, "_", order)]] <- renderUI(disabled(textInput(paste0("input_factor_treatment_", index, "_", order), "")))
        return()
    }

    if(aux$FORM == "combo box"){
      opts <- strsplit(aux$LEVEL, ";")[[1]]
      output[[paste0("ui_col_NFF_", index, "_", order)]] <- renderUI(
        fluidRow(column(12, style="margin-top:-3px;margin-bottom:-1.5px;",
              
        selectizeInput(paste0("input_factor_treatment_", index, '_', order), label = "", multiple = TRUE, 
                       options = list(maxItems =1, placeholder ="Select one..."), choices = opts)
        ))
      )
    }
    else if( aux$FORM=="date"){
      
        output[[paste0("ui_col_NFF_", index, "_", order)]] <- renderUI(
          airDatepickerInput(paste0("input_factor_treatment_", index, "_", order), "",clearButton = T,autoClose = T)
        )
    }
    else{
      output[[paste0("ui_col_NFF_", index, "_", order)]] <- renderUI(textInput(paste0("input_factor_treatment_", index, "_", order), ""))
    }
  }
  
  
  


  ### function para dibujar  box con los select cuando es YES FULL FACTORIAL
  getUiBoxFullFactorial <- function(order, str_id, value_1 = NULL,  list_2 = NULL, value_2= NULL, list_3 = NULL, value_3= NULL, nlevels=2){
      fluidRow(id = paste0("full_factor_box_", str_id),
         column(width = 12,
                box(
                  # title = paste0("#", " Factor"),
                  #   column(12, offset = 0, style='padding:0px; text-align:right; ',  actionButton(paste0("closeBox_FF_", str_id), "Remove")),
                    column(12, offset = 0, 
                           column(6, HTML("<b><h4># Factor</h4></b>")
                           ),
                           column(6, 
                                  style='padding:0px; text-align:right; ',  actionButton(paste0("closeBox_FF_", str_id), "Remove")
                           )
                           
                    ),
                    width = 12,
                    solidHeader = TRUE, status = "warning",
                    column(width = 12,
                           fluidRow(
                             column( width = 6,
                                 fluidRow(
                                   fluidRow(
                                     column(width = 4,
                                            selectizeInput(paste0("sel_factor_", str_id, "_1"), "", choices = designVars$main_list,selected = value_1, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
                                     ),
                                     column(width = 4,
                                            selectizeInput(paste0("sel_factor_", str_id, "_2"), "", choices = list_2,  multiple =T, selected = value_2,  options = list(maxItems =1, placeholder ="Select..."))
                                     ),
                                     column(width = 4,
                                            selectizeInput(paste0("sel_factor_", str_id, "_3"), "", choices =  list_3, multiple =T, selected = value_3,  options = list(maxItems =1, placeholder ="Select..."))
                                     )
                                   )
                                 )

                             ),
                             column(width = 6,
                                fluidRow(
                                  column(width = 6,
                                         fluidRow(id=paste0("fl_title_factor_aux_", str_id))

                                  ),
                                  column(width = 6,
                                         hidden(numericInput(paste0("numLevels_", str_id), HTML("Number of levels"), max = 5, min = 2, value = nlevels))
                                  )
                                ),
                                fluidRow(id= paste0("levelSelection_", str_id))
                             ),
                             column(12, 
                                    fluidRow(id=paste0("fr_factorSoil_", str_id))
                             ),
                             column(12, style="text-align:right",
                                      actionButton(paste0("btDuplicate_FF_", str_id), "Duplicate")
                             )
                           )
                    )
                )
         ))

  }

  ### function para dibujar  box con los select cuando es NO FULL FACTORIAL
  getUiBoxNotFullFactorial <- function(order, str_id, value_1 = NULL,  list_2 = NULL, value_2= NULL, list_3 = NULL, value_3= NULL){
    fluidRow(id = paste0("not_full_factor_box_", str_id),
         column(width = 12,
                box(
                  # column(12, offset = 0, style='padding:0px; text-align:right; ',  actionButton(paste0("closeBox_NFF_", str_id), "Remove")),
                  column(12, offset = 0, 
                         column(6, HTML("<b><h4># Factor</h4></b>")
                         ),
                         column(6, 
                                style='padding:0px; text-align:right; ',  actionButton(paste0("closeBox_NFF_", str_id), "Remove")
                          )
                         
                  ),
                  # title = paste0("#", " Factor"),
                  width = 12,
                  solidHeader = TRUE, status = "warning",
                  
                  
                  column(width = 12,
                         
                         fluidRow(
                           fluidRow(
                             column(width = 4,
                                    selectizeInput(paste0("sel_factor_", str_id, "_1"), "", choices = designVars$main_list,selected=value_1, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
                             ),
                             column(width = 4,
                                    selectizeInput(paste0("sel_factor_", str_id, "_2"), "", choices = list_2,selected = value_2, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
                             ),
                             column(width = 4,
                                    selectizeInput(paste0("sel_factor_", str_id, "_3"), "", choices = list_3,selected =value_3 , multiple =T, options = list(maxItems =1, placeholder ="Select..."))
                             )
                           )
                         )
                  ),
                  column(12, style="text-align:right",
                         actionButton(paste0("btDuplicate_NFF_", str_id), "Duplicate")
                  )
                )
         )
    )
  }
  
  
  observeEvent(input$selectFactor, {
    
    vars <- unlist(strsplit(input$selectFactorID, "_"))
    factor <- vars[3]
    selector <- vars[4]
    
    value <-  input[[input$selectFactorID]]
    
    if(selector == "1"){
      
      if(!is.null(value)){
        aux <- dplyr::filter(factors,GROUP==value)
        mchoices_2 <- unique(aux$SUBGROUP)
        if(value == "Soil fertility"){
          addTabSoilFertility(factor)
        }
        else{
          removeTabSoilFertility(factor)
        }
      }
      else{
        removeTabSoilFertility(factor)
        mchoices_2 <- NULL
      }
      
      mchoices_3 <- NULL
      
      
      list()
      shinyjs::hide(id= paste0("numLevels_", factor))
      fUpdateSelect(paste0("sel_factor_", factor, "_2"), mchoices_2)
      designVars$choices_2[[factor]] <- mchoices_2
      designVars$choices_3[[factor]] <- mchoices_3
      
      
      removeUI(selector = paste0("#fluid_levels_", factor), immediate = T)
      isolate(convertListToHTMLSelect(factor))
      removeUI( selector = paste0("#fl_title_factor_", factor), immediate = T )
    }
    else if(selector == "2"){
      choice_1 <- input[[paste0("sel_factor_", factor, "_1")]]
      if(!is.null(value)){
        aux <- dplyr::filter(factors,GROUP==choice_1 & SUBGROUP == value)
        mchoices_3 <- unique(aux$FACTOR)
      }
      else{
        mchoices_3 <- NULL
      }
      
      fUpdateSelect(paste0("sel_factor_", factor, "_3"), mchoices_3)
      designVars$choices_3[[factor]] <- mchoices_3
      shinyjs::hide(id= paste0("numLevels_", factor))
      isolate(convertListToHTMLSelect(factor))
      isolate(if(!is.null(choice_1) && choice_1 == "Soil fertility") generateListLevelsSoilTab(factor))
      removeUI(selector = paste0("#fluid_levels_", factor), immediate = T)
      removeUI( selector = paste0("#fl_title_factor_", factor), immediate = T )
    }
    else if(selector == "3"){
      choice_2 <- input[[paste0("sel_factor_", factor, "_2")]]
      if(is.null(choice_2)) return()
      
      removeUI( selector = paste0("#fl_title_factor_", factor), immediate = T )
      choice_1 <- input[[paste0("sel_factor_", factor, "_1")]]
      choice_2 <- input[[paste0("sel_factor_", factor, "_2")]]
      if(!is.null(value) && !is.null(choice_2)){
        if(!is.null(choice_1) && choice_1 != "Soil fertility") shinyjs::show(id= paste0("numLevels_", factor))
        isolate(updateLevelSelection(factor))
      }
      else{
        isolate(convertListToHTMLSelect(factor))
        shinyjs::hide(id= paste0("numLevels_", factor))
        removeUI(selector = paste0("#fluid_levels_", factor), immediate = T)
        isolate(if(!is.null(choice_1) && choice_1 == "Soil fertility") {generateListLevelsSoilTab(factor)})
      }
    }
    
  })
  
  observeEvent(input$duplicateFactor, {
    vars <- unlist(strsplit(input$duplicateFactorID, "_"))
    type <- vars[2]
    factor <- vars[3]
    if(type == "FF"){
      insertBoxFullFactorialDuplicate(factor)
    }
    else if(type == "NFF"){
      insertBoxNotFullFactorialDuplicate(factor)
    }
    
  })
 
  insertBoxFullFactorialDuplicate <- function(factor){ #FUNCION DE DUPLICAR
    str_id <- stri_rand_strings(1, 8,  '[A-Z]')
    id <- match(paste0("FF_", factor), designVars$ids_FULL) #encuentra la posicion del factor de la lista de los IDs
    
    len <- length(designVars$ids_FULL)
    left_side <- designVars$ids_FULL[1:id]
    right_side <- NULL
    if(len > 1 && id < len){
      rg <- id + 1
      right_side <- designVars$ids_FULL[rg:len]
    }
    
    
    designVars$ids_FULL <- c(left_side, paste0("FF_", str_id), right_side) #junta todos los casos
    value_1 <-input[[ paste0("sel_factor_", factor, "_1")]]
    value_2 <-input[[ paste0("sel_factor_", factor, "_2")]]
    value_3 <-input[[ paste0("sel_factor_", factor, "_3")]]
    nlevels <-as.numeric(input[[paste0("numLevels_", factor)]])
    
    
    insertUI(
      selector = paste0("#full_factor_box_", factor),
      where = "afterEnd",
      ui = getUiBoxFullFactorial(factor,str_id, value_1, designVars$choices_2[[factor]], value_2, designVars$choices_3[[factor]], value_3, nlevels),
      immediate = T
    )
    designVars$num_FULL <- designVars$num_FULL + 1
    designVars$choices_2[[str_id]] <- designVars$choices_2[[factor]]
    designVars$choices_3[[str_id]] <- designVars$choices_3[[factor]]
    
    if(!is.null(value_1) && value_1 == "Soil fertility"){
        addTabSoilFertility(str_id)
      
    }
    if(!is.null(value_3)){
      if(value_1 != "Soil fertility") {
        shinyjs::show(id= paste0("numLevels_", str_id))
      }
      isolate(updateLevelSelection(str_id, value_1, value_2, value_3, nlevels))
    }
    
  }
  
  
  insertBoxNotFullFactorialDuplicate <- function(factor){
    str_id <- stri_rand_strings(1, 8,  '[A-Z]')
    
    
    id <- match(paste0("NFF_", factor), designVars$ids_NFULL)
    
    len <- length(designVars$ids_NFULL)
    left_side <- designVars$ids_NFULL[1:id]
    right_side <- NULL
    if(len > 1 && id < len){
      rg <- id + 1
      right_side <- designVars$ids_NFULL[rg:len]
    }
    
    
    designVars$ids_NFULL <- c(left_side, paste0("NFF_", str_id), right_side)
    
    value_1 <-input[[ paste0("sel_factor_", factor, "_1")]]
    value_2 <-input[[ paste0("sel_factor_", factor, "_2")]]
    value_3 <-input[[ paste0("sel_factor_", factor, "_3")]]
    
    insertUI(
      selector = paste0("#not_full_factor_box_", factor),
      where = "afterEnd",
      ui = getUiBoxNotFullFactorial(factor,str_id, value_1, designVars$choices_2[[factor]], value_2, designVars$choices_3[[factor]], value_3),
      immediate = T
    )
    designVars$num_NFULL <- designVars$num_NFULL + 1
    designVars$choices_2[[str_id]] <- designVars$choices_2[[factor]]
    designVars$choices_3[[str_id]] <- designVars$choices_3[[factor]]
    if(!is.null(value_1) && value_1 == "Soil fertility"){
      addTabSoilFertility(str_id, factor)
    }

    drawNewColumnNFFDuplicate(factor, str_id, value_1, value_2, value_3)
  }
  
  
  drawNewColumnNFFDuplicate <- function(prev, index, val_1=NULL, val_2=NULL, val_3=NULL){
    
    insertUI(
      selector = paste0("#col_NFF_", prev),
      where = "afterEnd",
      ui = column(id=paste0("col_NFF_", index), width=2,
                  HTML("<center>"),uiOutput(paste0("title_col_NFF_", index)),HTML("</center>"), 
                  fluidRow(id=paste0("fr_col_NFF_", index))
      )
    )
    
    value <- val_3
    
    if(is.null(value)) value <- "Factor"
    
    output[[paste0("title_col_NFF_", index)]] <- renderText({value})
    
    num_treat <- input$designFieldbook_agrofims_t_n
    if(is.null(num_treat)) num_treat <- designVars$DEFAULT_TREAT_NFULL
    else num_treat <- as.integer(num_treat)
    if( num_treat < 1) return()
    
    for(i in 1:num_treat){
      insertUI(
        selector = paste0("#fr_col_NFF_", index),
        where = "beforeBegin", 
        ui = column(id=paste0("aux_col_NFF_", index, "_", i), width=12,uiOutput(paste0("ui_col_NFF_", index, "_", i)))
      )
      drawInputNFFDuplicate(prev, index, i, val_1, val_2, val_3) 
      duplicateSummary(prev,index, i)
    }
    
    if(!is.null(val_3)){
      aux <- dplyr::filter(factors,GROUP==val_1 & SUBGROUP==val_2 & FACTOR==val_3)
      if(length(aux) >0)
        generateListLevelsSoilTab(index, aux$FORM, aux$LEVEL, val_3)
    }
  
  }
  
  drawInputNFFDuplicate <- function(prev, index, order, sel_1=NULL, sel_2=NULL, sel_3=NULL){
    if(!is.null(sel_1)  && sel_1 == "Soil fertility"){
      nLevels <- input[[paste0("numLevels_tabSoil_", index)]]
      
      if(is.null(nLevels)) nLevels <- 2
      options_str <- c()
      
      for(i in 1:nLevels){
        options_str <- c(options_str, paste0("Level ", i))
      }
        output[[paste0("ui_col_NFF_", index, "_", order)]] <- renderUI(
          fluidRow(column(12, style="margin-top:-3px;margin-bottom:-1.5px;",
          selectizeInput(paste0("input_factor_treatment_", index, '_', order), label = "", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = options_str)
          ))
        )
      
      return()
    }
    
    value <- sel_3
    if(is.null(value)){
      output[[paste0("ui_col_NFF_", index, "_", order)]] <- renderUI(disabled(textInput(paste0("input_factor_treatment_", index, "_", order), "")))
      return()
    }
    
    aux <- dplyr::filter(factors,GROUP==sel_1 & SUBGROUP==sel_2 & FACTOR==sel_3)
    
    if(!is.null(aux$FORM) && aux$FORM == "combo box"){
      value_sel <- input[[paste0("input_factor_treatment_", prev, '_', order)]]
      opts <- strsplit(aux$LEVEL, ";")[[1]]
      output[[paste0("ui_col_NFF_", index, "_", order)]] <- renderUI(
        fluidRow(column(12, style="margin-top:-3px;margin-bottom:-1.5px;",
        selectizeInput(paste0("input_factor_treatment_", index, '_', order), label = "", multiple = TRUE, selected = value_sel,
                       options = list(maxItems =1, placeholder ="Select one..."), choices = opts)
        ))
      )
    }
    else if(!is.null(aux$FORM) && aux$FORM=="date"){
      value_sel <- input[[paste0("input_factor_treatment_", prev, '_', order)]]
      if(!is.null(value_sel)) value_sel <- value_sel +1
      output[[paste0("ui_col_NFF_", index, "_", order)]] <- renderUI(
        airDatepickerInput(paste0("input_factor_treatment_", index, "_", order), "",clearButton = T,autoClose = T, value = value_sel)
      )
      # output[[paste0("ui_col_NFF_", index, "_", order)]] <- renderUI(dateInput(paste0("date_factor_treatment_", index, "_", order), "", value = NULL))
    }
    else{
      value_sel <- input[[paste0("input_factor_treatment_", prev, '_', order)]]
      output[[paste0("ui_col_NFF_", index, "_", order)]] <- renderUI(textInput(paste0("input_factor_treatment_", index, "_", order), "", value=value_sel))
    }
  }
  
  
  convertListToHTMLSelect <- function(index, myList="", form="", colname = ""){
    
    if(is.null(input[["fullFactorialRB"]]) || input[["fullFactorialRB"]] == "Yes" ) return()
    
    updateSummaryAll()
    numTreatments <- isolate(input$designFieldbook_agrofims_t_n)
    numTreatments <-  as.integer(numTreatments)
    
    factor_sel_1 <- input[[paste0("sel_factor_", index, "_1")]]
    factor_sel_2 <- input[[paste0("sel_factor_", index, "_2")]]
    factor_sel_3 <- input[[paste0("sel_factor_", index, "_3")]]
    
    if(colname == "" || is.null(factor_sel_2)|| is.null(factor_sel_3) || is.null(factor_sel_1) ) mtitle <- "Factor"
    else mtitle <- colname
    output[[paste0("title_col_NFF_", index)]] <- renderText({mtitle})
    
    
    if(is.null(factor_sel_1) || numTreatments  < 1 ){
  
      return()
    }
    if(!is.null(input[[paste0("sel_factor_", index, "_3")]])) colname <- input[[paste0("sel_factor_", index, "_3")]]
    
    opt <- NULL
    
    for(i in 1:numTreatments){
      drawInputNFF(index, i)
      updateSummary(i)
    }
    
   
  }
  
  updateLevelSelection <- function(index, in_sel1=NULL, in_sel2=NULL, in_sel3=NULL, in_nlevels = NULL){
    num_levels  <- NULL
    
    if(!is_null(in_sel1)){
      sel_1 <- in_sel1
      sel_2 <- in_sel2
      sel_3 <- in_sel3
      num_levels <- in_nlevels
    }
    else{
      sel_1 <- input[[paste0("sel_factor_", index, "_1")]]
      sel_2 <- input[[paste0("sel_factor_", index, "_2")]]
      sel_3 <- input[[paste0("sel_factor_", index, "_3")]]
      num_levels <- input[[paste0("numLevels_", index)]]
    }
    
    if(is.null(sel_1) || is.null(sel_2) || is.null(sel_3)) return()

    aux <- dplyr::filter(factors,GROUP==sel_1 & SUBGROUP==sel_2 & FACTOR==sel_3)
    
    removeUI(selector = paste0("#fl_title_factor_", index), immediate = T)
    
    if(nrow(aux) > 0){

      if(isolate(input$fullFactorialRB == "No" )){
        
        if(!is.null(aux$FORM) &&aux$FORM == "combo box"){
          isolate(convertListToHTMLSelect(index, aux$LEVEL, aux$FORM, sel_3))
        }
        else{
          isolate(convertListToHTMLSelect(index, sel_3, aux$FORM, sel_3))
        }
        updateSummaryAll()

      }
      
      isolate(
        if(!is.null(sel_1) && sel_1 == "Soil fertility"){
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
          
          if(isolate(is.numeric(num_levels))){
            if(!is.null(aux$FORM) && aux$FORM == "combo box"){
              drawComboboxLevel(index,  num_levels , aux$LEVEL)
            }
            else if(!is.null(aux$FORM) && aux$FORM == "text input"){
              drawTextInputLevel(index,  num_levels , aux$UNIT)
            }
            else if(!is.null(aux$FORM) && aux$FORM == "numeric input"){
              drawNumericInputLevel(index,num_levels)
            }
            
            else if(!is.null(aux$FORM) && aux$FORM == "date"){
              drawDateLevel(index,num_levels)
            }
            
          }
          insertUI(
            selector = paste0("#fluid_levels_", index),
            where = "beforeEnd",
            ui = column(width = 12,id=paste0("factor_note_", index), textAreaInput(paste0("note_factor_", index), "Note")
            )
          )
          
        }
      )
    }
    else{
      removeUI(selector = paste0("#fluid_levels_", index), immediate = T)
      isolate(if(!is.null(sel_1) && sel_1 == "Soil fertility") {generateListLevelsSoilTab(index)})
    }
    
  }
  
  fUpdateSelect <- function(selID, in_choices = NULL, value = NULL){
    updateSelectInput(session, selID, choices = in_choices, selected = value)
  }
  fUpdateSelectValue <- function(selID, value = NULL){
    updateSelectInput(session, selID, selected = value)
  }
  
  observeEvent(input$levelsSoilFertility, {
    vars <- unlist(strsplit(input$levelsSoilFertilityID, "_"))
    factor <- vars[3]

    if(!is.null(input[[input$levelsSoilFertilityID]])){
        drawLevelsSoilTab(vars[3],as.numeric(input[[input$levelsSoilFertilityID]]))
        isolate(convertListToHTMLSelect(factor))
    }
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


  ### evento cuando se cambia el numero de tratamientos en NO FULL FACTORIAL
  observeEvent(input$designFieldbook_agrofims_t_n, {
    rep <- as.numeric(input$designFieldbook_agrofims_t_n)
    ids <- designVars$ids_NFULL
    
    
    if(num$currNumReplications > rep  && !is.na(rep)){
      start<- rep +1
      for(i in num$currNumReplications:start){
        deleteSummaryEntry(i)
      }
    }
    else if(num$currNumReplications < rep && !is.na(rep)){
      start  <- num$currNumReplications +1
      for(i in start:rep){
        insertSummaryEntry(i)
        updateSummary(i)
      }
    }
    
    
    for (id in ids){
      vars <- unlist(strsplit(id, "_"))
      if(num$currNumReplications > rep  && !is.na(rep)){
        start<- rep +1
        for(i in num$currNumReplications:start){
          
          removeUI(
            selector = paste0("#aux_col_NFF_", vars[2], "_", i),
            immediate = T
          )
          
        }
      }
      else if(num$currNumReplications < rep && !is.na(rep)){
        start  <- num$currNumReplications +1
        for(i in start:rep){
          
          insertUI(
            selector = paste0("#fr_col_NFF_", vars[2]),
            where = "beforeBegin", 
            ui = column(id=paste0("aux_col_NFF_", vars[2], "_", i), width=12, uiOutput(paste0("ui_col_NFF_", vars[2], "_", i)))
          )
          
          drawInputNFF(vars[2], i)
        }
      }
      
    }
    num$currNumReplications <- rep
    updateSummaryAll(rep)
    
  })

  ### variable to keep track of soils tabs
  numSoilPanels <- reactiveValues()
  numSoilPanels$current <- c()
  numSoilPanels$levels <- c() ## to control how many levels each tab has
  numSoilPanels$appList <-list() ## to control the list inside comboboxes for applications in soil fertility tabs
  
  
  ### function to add tabs for soil fertility
  addTabSoilFertility <- function(index, prev = NULL){
    
    ind <- match(index, numSoilPanels$current)
    if(!is.na(ind)) return()
    
    len <- length(numSoilPanels$current)
    mtarget <- "tabTreatmentFactors" ## default if list is empty or the tab goes first
    
    if(!is.null(prev)){
      ind_p <- match(prev, numSoilPanels$current)
      if(len==ind_p){
        numSoilPanels$current <- c(numSoilPanels$current, index)
        numSoilPanels$levels <- c(numSoilPanels$levels, 0)
      }
      else{
        end <- ind_p + 1
        numSoilPanels$current <- c(numSoilPanels$current[1:ind_p], index, numSoilPanels$current[end:len])
        numSoilPanels$levels <- c(numSoilPanels$levels[1:ind_p], 0, numSoilPanels$levels[end:len])
      }
    }
    else{
      if(len == 0){
        numSoilPanels$current <- c(numSoilPanels$current, index)
        numSoilPanels$levels <- c(numSoilPanels$levels, 0)
      }
      else{
        pre <- "FF_"
        isolate(ids <- designVars$ids_FULL)
        isolate(ff <- input$fullFactorialRB)
        if(ff == "No") {
          isolate(ids <- designVars$ids_NFULL)
          pre <- "NFF_"
          }
          
        mid <- match(paste0(pre, index), ids)
        curr_ids <- match(paste0(pre,numSoilPanels$current), ids)
        
        if(length(curr_ids) ==1){
          if(mid > curr_ids){
            numSoilPanels$current <- c(numSoilPanels$current, index)
            numSoilPanels$levels <- c(numSoilPanels$levels, 0)
          }
          else{
            numSoilPanels$current <- c(index, numSoilPanels$current)
            numSoilPanels$levels <- c(0,numSoilPanels$levels)
          }
        }
        else{
          
          pos <- 0
          for(i in curr_ids){
            if(mid < i) break
            pos <- pos + 1
          }
          if(pos == 0){
            numSoilPanels$current <- c(index, numSoilPanels$current)
            numSoilPanels$levels <- c(0,numSoilPanels$levels)
          }
          else if(pos == len){
            numSoilPanels$current <- c(numSoilPanels$current, index)
            numSoilPanels$levels <- c(numSoilPanels$levels, 0)
          }
          else {
            left_side <-  numSoilPanels$current[1:pos]
            end <- pos + 1
            right_side <- numSoilPanels$current[end:len]
            numSoilPanels$current <- c(left_side, index, right_side)
            numSoilPanels$levels <- c(left_side, 0, right_side)
          }
        }
      }
      
    }
    
    numSoilPanels$appList[[paste0("f", index)]] <- list("void", c()) ## user has not chosen factor yet
    
    ind <- match(index, numSoilPanels$current)
    
    
    if(is.numeric(ind) && ind != 1){
      aux <- numSoilPanels$current[ind-1]
      mtarget <- paste0("panelTreatment_soilFertility_",  aux)
    }
    
    insertTab(inputId = "treatmentSetPanel",
              tabPanel(paste0("Soil fertility details - factor "),  value = paste0("panelTreatment_soilFertility_", index),
                       column(12, br(),
                              fluidRow(
                                column(6,
                                       uiOutput(paste0("uiFactorName_tabSoil_", index))
                                ),
                                column(6,
                                       column(4,
                                              numericInput(paste0("numLevels_tabSoil_", index), "Levels", min =1, max=100, value=2)
                                       )
                                )
                              ),
                              fluidRow(id=paste0("fluidRow_levelsTabSoil_", index))
                       )
              ),
              position = "after",
              target = mtarget
    )
    updateNumericInput(session,paste0("numLevels_tabSoil_", index), value = 2 )
    
  }

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
          updateSelectizeInput(session,paste0("select_product_factor_", index, "_level_", i, "_app_", j), choices = getList(values))
        }

      }
    }

  }
  getList <- function(str){
    if(is.character(str)) return(unlist(strsplit( str, ";")))
    else{ return(c(""))}
  }
  
  observeEvent(input$levelsFF,{
    vars <- unlist(strsplit(input$levelsFFID, "_"))
    if(length(vars) != 2) return()
    index <- vars[2]
    
    sel_1 <- input[[paste0("sel_factor_", index, "_1")]]
    sel_2 <- input[[paste0("sel_factor_", index, "_2")]]
    sel_3 <- input[[paste0("sel_factor_", index, "_3")]]
    num_levels <- input[[paste0("numLevels_", index)]]
    if(is.null(sel_1) || is.null(sel_2) || is.null(sel_3) || !is.numeric(num_levels)) return()
    
    aux <- dplyr::filter(factors,GROUP==sel_1 & SUBGROUP==sel_2 & FACTOR==sel_3)
    
    if(is.numeric(num_levels) && nrow(aux)>0){
      if(!is.null(aux$FORM) && aux$FORM == "combo box"){
        drawComboboxLevel(index,  num_levels , aux$LEVEL)
      }
      else if(!is.null(aux$FORM) && aux$FORM == "text input"){
        drawTextInputLevel(index,  num_levels , aux$UNIT)
      }
      else if(!is.null(aux$FORM) && aux$FORM == "numeric input"){
        drawNumericInputLevel(index,num_levels)
      }
      
      else if(!is.null(aux$FORM) && aux$FORM == "date"){
        drawDateLevel(index,num_levels)
      }
      insertUI(
        selector = paste0("#fluid_levels_", index),
        where = "beforeEnd",
        ui = column(width = 12,id=paste0("factor_note_", index), textAreaInput(paste0("note_factor_", index), "Note")
        )
      )
      
    }
    
    
  })


  ################# fin design ######################################################


  ###########################################################

  # featNames <- names(Agronomic_features$`Agronomic features`)




  # }) End agronomic trait shinyTree  ####################################


  #### factors ####################################################################################


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
                               dateInput(paste0("factor_start_date_", order, "_1"), HTML("#1 Start date"),format = "yyyy-mm-dd")
                               ),
                        column(width = 6,
                               dateInput(paste0("factor_end_date_", order, "_1"), HTML("#1 End date"),format = "yyyy-mm-dd")
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
                          dateInput(paste0("factor_start_date_", order, "_", i), HTML(paste0("#",i, " Start date")),format = "yyyy-mm-dd")
                   ),
                   column(width = 6,
                          dateInput(paste0("factor_end_date_", order, "_", i), HTML(paste0("#", i, " End date")),format = "yyyy-mm-dd")
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
                  "Seedbed preparation" = "tabSeedbed",
                  "Mulch management" ="tabMulching",
                  "Residue management" ="tabResidue",
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
    hideTab("nutrienTabPanels", "tabSeedbed")
    hideTab("nutrienTabPanels", "tabMulching")
    hideTab("nutrienTabPanels", "tabPlanting")
    hideTab("nutrienTabPanels", "tabNutrient")
    hideTab("nutrienTabPanels", "tabWeeding")
    hideTab("nutrienTabPanels", "tabResidue")
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

  ################# fin de tabs en field operations ################################




  ###########  biofertilizer ####################################################

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
    box( title = paste0("Application", order),
        width = 12,
        solidHeader = TRUE, status = "warning",
        column(width = 6,


               fluidRow(
                 column(width = 6,
                        dateInput(paste0("biofertilizer_landLeveling_start_date_", order), label ="Start date", format = "yyyy-mm-dd")
                 ),
                 column(width = 6,
                        dateInput(paste0("biofertilizer_landLeveling_end_date",  order), label ="End date", format = "yyyy-mm-dd")
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

  ############ end biofertilizer ###############################################





  ###########  Pest and Disease ################################################
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
             box( title = paste0("Application", order),
                  width = 12,
                  solidHeader = TRUE, status = "warning",
                  column(width = 6,

                                  fluidRow(
                                    column(width = 6,
                                           dateInput(paste0("pestcontrol_start_date_",order), label ="Start date", format = "yyyy-mm-dd")
                                    ),
                                    column(width = 6,
                                           dateInput(paste0("pestcontrol_end_date_",order), label ="End date", format = "yyyy-mm-dd")
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

  ############# end Pest and Disease ###########################################


  
  observeEvent(input$closeBox_button, {
    var <- unlist(strsplit(input$closeBox_button_id,"_"))
    arr_keys <- c()
    len <- 0 
    
    mselector <- "XXXXXXXXXX"
   
    
    if(var[2] == "ECWE" && expCondsVars$num_weed > expCondsVars$DEFAULT_weed){
      mselector = paste0("#box_weeding_", var[3])
      expCondsVars$num_weed <- expCondsVars$num_weed - 1
      aux <- expCondsVars$ids_weed
      expCondsVars$ids_weed <- aux[! aux %in% paste0("ECWE_",var[3])]
    }
    
    if(var[2] == "ECIR" && expCondsVars$num_irri > expCondsVars$DEFAULT_irri){
      mselector = paste0("#fr_irrigation_box_", var[3])
      expCondsVars$num_irri <- expCondsVars$num_irri - 1
      aux <- expCondsVars$ids_irri
      expCondsVars$ids_irri <- aux[! aux %in% paste0("ECIR_",var[3])]
    }
    
    if(var[2] == "HARV" && expCondsVars$num_harvest > expCondsVars$DEFAULT_harvest){
      mselector = paste0("#fr_harvestbox_", var[3])
      expCondsVars$num_harvest <- expCondsVars$num_harvest - 1
      aux <- expCondsVars$ids_harvest
      expCondsVars$ids_harvest <- aux[! aux %in% var[3]]
      arr_keys <- expCondsVars$num_harvest
      len <- length(arr_keys)
    }
    
    
    removeUI(
      selector = mselector, 
      immediate = T
    )
    
   
    
  })

  ###########  irrigation ##########################################
  ## valor pra guardar cuantos boxes hay actualmente dibujados
  
  expCondsVars  <- reactiveValues()
  
  
  expCondsVars$num_irri <- 0
  expCondsVars$DEFAULT_irri <- 1
  expCondsVars$ids_irri <- c()
  
  observeEvent(input$addIrrigation,{
    defaultBoxes = expCondsVars$DEFAULT_irri
    if(expCondsVars$num_irri >= 1){
      insertBoxIrrigation(expCondsVars$num_irri + 1)
    }
  })
  
  insertBoxIrrigation <- function(index){
    insertUI(
      selector = "#fr_irrigation_boxes",
      where = "beforeBegin",
      ui = getUiIrrigation(index)
    )
    expCondsVars$num_irri <- expCondsVars$num_irri + 1
  }
  
  getUiIrrigation <- function(index){
    str_id <- stri_rand_strings(1, 8,  '[A-Z]')
    expCondsVars$ids_irri <- c(expCondsVars$ids_irri, paste0("ECIR_", str_id))
    
    fluidRow(
      id= paste0("fr_irrigation_box_", str_id),
          box(
               column(12, offset = 0, 
                      column(6,style='padding:0px; text-align:left;',  HTML("<b><h4>Irrigation</h4></b>")
                      ),
                      column(6, 
                             style='padding:0px; text-align:right; ',  actionButton(paste0("closeBox_ECIR_", str_id), "Remove")
                      )
                      
               ),
               width = 12, solidHeader = TRUE, status = "warning",
             column(width = 6,
                    fluidRow(
                      column(width = 6,
                             #dateInput(paste0("irrigationevent_start_date_", order), label ="Start date", value = NA, format = "yyyy-mm-dd")
                             #airDatepickerInput(paste0("irrigationevent_start_date_", order), "Start date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                             # irrigationevent_start_date_
                             if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                               airDatepickerInput(paste0("irrigationevent_start_date_", str_id),
                                                  "Start date",
                                                  clearButton = T,
                                                  autoClose = T,
                                                  #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                                                  placeholder = "yyyy-mm-dd",
                                                  minDate = as.Date(input$fbDesign_project_start_date) + 1,
                                                  maxDate = as.Date(input$fbDesign_project_end_date) + 1
                                                  
                               )
                             } else {
                               airDatepickerInput(paste0("irrigationevent_start_date_", str_id),
                                                  "Start date",
                                                  clearButton = T,
                                                  autoClose = T,
                                                  placeholder = "yyyy-mm-dd"
                               )
                             }
                             
                      ),
                      column(width = 6,
                             #dateInput(paste0("irrigationevent_end_date_", order), label = "End date", value = NA, format = "yyyy-mm-dd")
                             #airDatepickerInput(paste0("irrigationevent_end_date_", order), "End date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                             if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                               airDatepickerInput(paste0("irrigationevent_end_date_", str_id),
                                                  "End date",
                                                  clearButton = T,
                                                  autoClose = T,
                                                  #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                                                  placeholder = "yyyy-mm-dd",
                                                  minDate = as.Date(input$fbDesign_project_start_date) + 1,
                                                  maxDate = as.Date(input$fbDesign_project_end_date) + 1
                                                  
                               )
                             } else {
                               airDatepickerInput(paste0("irrigationevent_end_date_", str_id),
                                                  "End date",
                                                  clearButton = T,
                                                  autoClose = T,
                                                  placeholder = "yyyy-mm-dd"
                               )
                             }
                      )
                    ),
                    selectizeInput(paste0("irrigation_technique_", str_id), label = "Irrigation technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                     c("Sprinkler irrigation",
                                       "Localized",
                                       "Surface",
                                       #"Sub-irrigation",
                                       "Other")
                    ),
                    hidden(textInput(paste0("irrigation_technique_", str_id, "_other"), "")),
                    conditionalPanel(paste0("input.irrigation_technique_", str_id, "== 'Surface'"),
                                     selectizeInput(paste0("surface_irrigation_technique_", str_id), label = "Surface irrigation technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                      c("Basin irrigation",
                                                        "Border irrigation",
                                                        "Continuous flood",
                                                        "Furrow irrigation",
                                                        "Uncontrolled flooding",
                                                        "Other")
                                     ),
                                     hidden(textInput(paste0("surface_irrigation_technique_", str_id, "_other"), ""))
                    ),
                    conditionalPanel(paste0("input.irrigation_technique_", str_id, "== 'Localized'"),
                                     
                                     selectizeInput(paste0("localized_irrigation_technique", str_id), label = "Localized irrigation technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                      c("Bubbler irrigation",
                                                        "Drip irrigation",
                                                        "Mist irrigation",
                                                        "Pitcher irrigation",
                                                        "Subsurface drip irrigation",
                                                        "Subsurface textile irrigation",
                                                        "Other")
                                     ),
                                     hidden(textInput(paste0("localized_irrigation_technique", str_id, "_other"), ""))
                    ),
                    conditionalPanel(paste0("input.irrigation_technique_", str_id, "== 'Sprinkler irrigation'"),
                                     
                                     selectizeInput(paste0("irrigation_using_sprinkler_systems_", str_id), label = "Sprinkler irrigation system", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                      c("Center pivot irrigation",
                                                        "Irrigation by lateral move",
                                                        "Irrigation by side move",
                                                        "Other")
                                     ),
                                     hidden(textInput(paste0("irrigation_using_sprinkler_systems_", str_id, "_other"), ""))
                    ),
                    
                    
                    #Sacar myFile upload
                    # fileInput(paste0("myFile", "Irrigation system picture_", order), accept = c('image/png', 'image/jpeg')),
                    # textInput(paste0("irrigation_water_source_", order), value="", label = "Water source"),
                    selectizeInput(paste0("irrigation_source_", str_id), label = "Irrigation source", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                     c("Drainage",
                                       "Groundwater",
                                       "Lake",
                                       "Reservoir",
                                       "River",
                                       "Spring",
                                       "Other")
                    ),
                    hidden(textInput(paste0("irrigation_source_", str_id,  "_other"), ""))#,
             ),
             column(width = 6,
                    fluidRow(
                      column(width = 6,
                             #textInput(paste0("irrigation_source_distance_", order), value="", label = "Irrigation source distance")
                             numericInput(paste0("irrigation_source_distance_", str_id), label = "Irrigation source distance", value = "", min = 0, step = 0.1)
                      ),
                      column(width = 6,
                             selectizeInput(paste0("irrigation_source_distance_", str_id, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                            choices = c("ft", "km", "m", "mi")
                             )
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             #textInput(paste0("irrigation_amount_", order), value="", label = "Irrigation amount")
                             numericInput(paste0("irrigation_amount_", str_id), label = "Irrigation amount", value = "", min = 0, step = 0.1)
                      ),
                      column(width = 6,
                             selectizeInput(paste0("irrigation_amount_", str_id, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                            choices = c("in", "mm")#, "cm", "m", "in", "ft", "ml", "L", "gal", "cu m", "cu in", "cu ft")
                             )
                      )
                    ),
                    textAreaInput(paste0("irrigation_notes_", str_id), label = "Notes", value = "")
             )
        )
    )
    
  }
  
  observe({
    if(expCondsVars$num_irri == 0){
      default <- expCondsVars$DEFAULT_irri
      for(i in 1:default){
        insertBoxIrrigation(i)
      }
    }
  })
  

  ####################################################################
  
  
  ########### weeding ##############################################
  
  expCondsVars$num_weed <- 0
  expCondsVars$DEFAULT_weed <- 1
  expCondsVars$ids_weed <- c()
  
  
  observeEvent(input$addWeeding,{
    defaultBoxes = expCondsVars$DEFAULT_weed
    if(expCondsVars$num_weed >= 1){
      insertBoxWeeding(expCondsVars$num_weed + 1)
    }
  })
  
  insertBoxWeeding <- function(index){
    insertUI(
      selector = "#fr_weeding_boxes",
      where = "beforeBegin",
      ui = getUiWeeding(index)
    )
    expCondsVars$num_weed <- expCondsVars$num_weed + 1
  }
  
  getUiWeeding <- function(index){
    str_id <- stri_rand_strings(1, 8,  '[A-Z]')
    expCondsVars$ids_weed <- c(expCondsVars$ids_weed, paste0("ECWE_", str_id))
    
    startDate <- NULL
    mtechnique <- NULL
    mnotes <- NULL
    mtype <- NULL
    mtraction <- NULL
    
    if(length(expCondsVars$ids_weed) > 1){
      vars <- unlist(strsplit(expCondsVars$ids_weed[1],"_"))
      startDate <- input[[paste0("weeding_start_date_", vars[2])]]
      mtechnique <- input[[paste0("weeding_technique_", vars[2])]]
      mnotes <- input[[paste0("weeding_notes_", vars[2])]]
      mtype <- input[[paste0("weeding_type_", vars[2])]]
      mtraction <- input[[paste0("weeding_traction_", vars[2])]]
      
      if(!is.null(startDate)) startDate <- startDate +1
      
    }
    
    fluidRow(
      id= paste0("box_weeding_", str_id),
      box(
                          
            column(12, offset = 0, 
                   column(6,style='padding:0px; text-align:left;',  HTML("<b><h4>Weeding</h4></b>")
                   ),
                   column(6, 
                          style='padding:0px; text-align:right; ',  actionButton(paste0("closeBox_ECWE_", str_id), "Remove")
                   )
                   
            ),
           width = 12,
           solidHeader = TRUE, status = "warning",
           column(width = 6,
                  h4(HTML(" ")),
                  fluidRow(
                    column(6, 
                           #dateInput(paste0("weeding_start_date_", index), "Start date", format = "yyyy-mm-dd")
                           #airDatepickerInput(paste0("weeding_start_date_", index), "Start date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                           
                           if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                             airDatepickerInput(paste0("weeding_start_date_", str_id),
                                                "Start date",
                                                clearButton = T,
                                                autoClose = T,
                                                value = startDate,
                                                placeholder = "yyyy-mm-dd",
                                                minDate = as.Date(input$fbDesign_project_start_date) + 1,
                                                maxDate = as.Date(input$fbDesign_project_end_date) + 1
                                                
                             )
                           } else {
                             airDatepickerInput(paste0("weeding_start_date_", str_id),
                                                "Start date",
                                                clearButton = T,
                                                value = startDate,
                                                autoClose = T,
                                                placeholder = "yyyy-mm-dd"
                             )
                           }
                    )
                  ),
                  
                  #selectInput(paste0("weeding_technique_", index), "Technique", c("Chemical", "Manual", "Mechanized"))
                  selectizeInput(paste0("weeding_technique_", str_id), "Technique", multiple = TRUE, selected = mtechnique, options = list(maxItems =1, placeholder ="Select one..."),
                                 choices =  c(
                                   "Chemical",
                                   "Manual",
                                   "Mechanized")
                  ), 
                  textAreaInput(paste0("weeding_notes_", str_id), "Notes", value = mnotes)
           ),
           column(6,
                  h4("Implement"),
                  selectizeInput(paste0("weeding_type_",str_id ), "Type", multiple = TRUE,selected = mtype, options = list(maxItems =1, placeholder ="Select one..."),
                                 choices =  c(
                                   "Cultivator",
                                   "Manual",
                                   "Sprayer",
                                   "Weed cutter/puller",
                                   "Other")
                  ),
                  hidden(textInput(paste0("weeding_type_",str_id, "_other" ), "")
                         
                  ),
                  selectizeInput(paste0("weeding_traction_", str_id), "Traction",multiple = TRUE,selected=mtraction, options = list(maxItems =1, placeholder ="Select one..."),
                                 choices= c(
                                   "Animal",
                                   "Manual",
                                   "2 wheel tractor",
                                   "4 wheel tractor",
                                   "Other")
                  ),
                  hidden(textInput(paste0("weeding_traction_",str_id, "_other" ), ""))
           )
      )
    )
    
  }
  
  observe({
    if(expCondsVars$num_weed == 0){
      default <- expCondsVars$DEFAULT_weed
      for(i in 1:default){
        insertBoxWeeding(i)
      }
    }
  })
  
  
  #################### end weeding #############################################

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
                                  #dateInput(paste0("input_startdate_soil_table_row_", index), "", format = "yyyy-mm-dd")
                                  #airDatepickerInput(paste0("input_startdate_soil_table_row_", index), "", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                  
                                  
                                  if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                                    airDatepickerInput(paste0("input_startdate_soil_table_row_", index),
                                                       "End date",
                                                       clearButton = T,
                                                       autoClose = T,
                                                       #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                                                       placeholder = "yyyy-mm-dd",
                                                       minDate = as.Date(input$fbDesign_project_start_date) + 1,
                                                       maxDate = as.Date(input$fbDesign_project_end_date) + 1
                                                       
                                    )
                                  } else {
                                    airDatepickerInput(paste0("input_startdate_soil_table_row_", index),
                                                       "End date",
                                                       clearButton = T,
                                                       autoClose = T,
                                                       placeholder = "yyyy-mm-dd"
                                    )
                                  }
                                  
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
                                  #dateInput(paste0("input_enddate_soil_table_row_", index), "", format = "yyyy-mm-dd")
                                  #airDatepickerInput(paste0("input_enddate_soil_table_row_", index), "", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                  
                                  
                                  if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                                    airDatepickerInput(paste0("input_enddate_soil_table_row_", index),
                                                       "End date",
                                                       clearButton = T,
                                                       autoClose = T,
                                                       #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                                                       placeholder = "yyyy-mm-dd",
                                                       minDate = as.Date(input$fbDesign_project_start_date) + 1,
                                                       maxDate = as.Date(input$fbDesign_project_end_date) + 1
                                                       
                                    )
                                  } else {
                                    airDatepickerInput(paste0("input_enddate_soil_table_row_", index),
                                                       "End date",
                                                       clearButton = T,
                                                       autoClose = T,
                                                       placeholder = "yyyy-mm-dd"
                                    )
                                  }
                                  
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
             box( title = paste0("Application", order),
                  width = 12,
                  solidHeader = TRUE, status = "warning",
                  fluidRow(
                    column(width = 6,
                          fluidRow(
                            column(width = 6,
                                   dateInput(paste0("nutrient_start_date_", type, "_", order), label ="Start date", format = "yyyy-mm-dd")
                            ),
                            column(width = 6,
                                   dateInput(paste0("nutrient_end_date_", type, "_", order), label ="End date", format = "yyyy-mm-dd")
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
                           dateInput(paste0("fert_nit_start_date1_", type, "_", order), label ="", format = "yyyy-mm-dd")
                    ),

                    column(width = 1,
                           dateInput(paste0("fert_nit_end_date1_", type, "_", order), label ="", format = "yyyy-mm-dd")
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
                                                   dateInput(paste0("fert_nit_start_date2_", type, "_", order), label ="", format = "yyyy-mm-dd")
                                            ),

                                            column(width = 1,
                                                   dateInput(paste0("fert_nit_end_date2_", type, "_", order), label ="", format = "yyyy-mm-dd")
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
                                            dateInput(paste0("fert_nit_start_date3_", type, "_", order), label ="", format = "yyyy-mm-dd")
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_nit_end_date3_", type, "_", order), label ="", format = "yyyy-mm-dd")
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
                           dateInput(paste0("fert_phos_start_date1_", type, "_", order), label ="", format = "yyyy-mm-dd")
                    ),

                    column(width = 1,
                           dateInput(paste0("fert_phos_end_date1_", type, "_", order), label ="", format = "yyyy-mm-dd")
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
                                            dateInput(paste0("fert_phos_start_date2_", type, "_", order), label ="", format = "yyyy-mm-dd")
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_phos_end_date2_", type, "_", order), label ="", format = "yyyy-mm-dd")
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
                                            dateInput(paste0("fert_phos_start_date3_", type, "_", order), label ="", format = "yyyy-mm-dd")
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_phos_end_date3_", type, "_", order), label ="", format = "yyyy-mm-dd")
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
                           dateInput(paste0("fert_potas_start_date1_", type, "_", order), label ="", format = "yyyy-mm-dd")
                    ),

                    column(width = 1,
                           dateInput(paste0("fert_potas_end_date1_", type, "_", order), label ="", format = "yyyy-mm-dd")
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
                                            dateInput(paste0("fert_potas_start_date2_", type, "_", order), label ="", format = "yyyy-mm-dd")
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_potas_end_date2_", type, "_", order), label ="", format = "yyyy-mm-dd")
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
                                            dateInput(paste0("fert_potas_start_date3_", type, "_", order), label ="", format = "yyyy-mm-dd")
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_potas_end_date3_", type, "_", order), label ="", format = "yyyy-mm-dd")
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
  dict <- readRDS(paste0(globalpath, "crop_measurements_v5.rds"))
  dict <- as.data.frame(dict, stringsAsFactors=FALSE)

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
  
  
  expCondsVars$num_harvest <- 0
  expCondsVars$DEFAULT_harvest <- 1
  expCondsVars$ids_harvest <- c()

  observeEvent(input$croppingType,{
    
    if(input$croppingType == "Monocrop"){
      removeTab(inputId = "fbDesignNav",target = "Crop_Measurement_intercrop")
      removeTab(inputId = "fbDesignNav",target = "Crop_Phenology_intercrop")
      
      insertTab(inputId = "fbDesignNav",
                tabPanel("Crop Measurement",  value = "Crop_Measurement_monocrop", icon = shiny::icon("leaf"), 
                         column(width = 12,
                                h2("Crop measurement"),
                                p(class = "text-muted", style="text-align:justify",
                                  paste("Please, select measurement by click.")
                                ),
                                column(12, align = "center", checkboxInput("dt_sel", "Select all"))
                                
                         ),
                         uiOutput("uiTraitsList3"),
                         sidebarPanel(id="sidebar", width = 12,
                                      actionButton("btnNextEnv", "Next", class = "btn-primary",style="color: #fff;", href="#top")
                         )
                ),
                position =  "after",
                target = "tabAgroFeat")
      insertTab(inputId = "fbDesignNav",
                tabPanel("Crop Phenology", value = "Crop_Phenology_monocrop",  icon = shiny::icon("envira"), DTOutput('phenoDT')),
                position =  "after",
                target = "Crop_Measurement_monocrop")

      isolate(removeAgroBoxesIntercrop())
      isolate(drawAgroBoxes(1))
      shinyjs::show(id="addHarvest")
      
    }
    else if(input$croppingType == "Intercrop"){
      
      removeTab(inputId = "fbDesignNav",target = "Crop_Measurement_monocrop")
      removeTab(inputId = "fbDesignNav",target = "Crop_Phenology_monocrop")
    
      
      isolate(ids <- intercropVars$ids)
      tt <- unlist(strsplit(ids[1],"_"))
      
      insertTab(inputId = "fbDesignNav",
             tabPanel("Crop Measurement",  value = "Crop_Measurement_intercrop", icon = shiny::icon("leaf"),
                      column(12, h2("Crop Measurement")),
                      tabsetPanel( id= "intercropMeasuTabs",
                                   tabPanel(title = textOutput(paste0("intercrop_tab_measu_title_",tt[2])), value = paste0("intercrop_tab_measu_",tt[2]), 
                                            br(),
                                            column(12,DTOutput(paste0("crop_measurement_table_", tt[2])))
                                   )
                      )
                      
             ),
            position =  "after",
            target = "tabAgroFeat")
      
      insertTab(inputId = "fbDesignNav",
              tabPanel("Crop Phenology",  value = "Crop_Phenology_intercrop", icon = shiny::icon("envira"),
                       column(12, h2("Crop Phenology")),
                        tabsetPanel( id= "intercropPhenoTabs", 
                                     tabPanel(title = textOutput(paste0("intercrop_tab_pheno_title_",tt[2])), value = paste0("intercrop_tab_pheno_",tt[2]), 
                                              br(),
                                              column(12,renderDataTable(pheno_vars , options = list(lengthChange = FALSE)))
                                     )
                        )
               ),
              position =  "after",
              target = "Crop_Measurement_intercrop")
      
     isolate(drawTabsIntercrop(ids))
     isolate(removeAgroBoxesMonocrop())
     expCondsVars$ids_harvest <- c()
     isolate(drawAgroBoxesIntercrop())
     shinyjs::hide(id="addHarvest")
    }
  })
  
  drawTabsIntercrop <- function(ids){
    tt <- unlist(strsplit(ids[1],"_"))
    
    mtarget <- paste0("intercrop_tab_measu_",tt[2])
    ptarget <- paste0("intercrop_tab_pheno_",tt[2])
    intercropVars$pheno[[tt[2]]] <- pheno_vars
    
    xtitle <- input[[paste0("cropCommonNameInter_", tt[2])]]
    mcrop <- xtitle
    if(is.null(mcrop)) mcrop <- "Crop" 
    
    updateCropMeasurementTable(tt[2],mcrop)
    
    if(!is.null(xtitle)){
      if(xtitle == "Other"  && input[[paste0("cropCommonNameInter_", tt[2]), "_other"]] != "")  
        xtitle <- input[[paste0("cropCommonNameInter_", tt[2]), "_other"]]
      else xtitle <- input[[paste0("cropCommonNameInter_", tt[2])]]
    }
    else {
      xtitle= "Crop"
    }
    
    
    mtitle <- paste0(xtitle, " Measurement")
    ptitle <- paste0(xtitle, " Phenology")
    
  
    isolate(renameTab(paste0("intercrop_tab_measu_title_",tt[2]), mtitle))
    isolate(renameTab(paste0("intercrop_tab_pheno_title_",tt[2]), ptitle))
    
    len <- length(ids)
    if(len < 2) return()
    
    for(ind  in 2:len){
      vars <- unlist(strsplit(ids[ind],"_")) 
      isolate(insertTabInterCrop(vars[2], mtarget, ptarget))
      ptarget <- paste0("intercrop_tab_pheno_",vars[2])
      mtarget <- paste0("intercrop_tab_measu_",vars[2])
    }
  }
  
  insertTabInterCrop <- function(index, mtarget, ptarget){
    
    xtitle <- input[[paste0("cropCommonNameInter_", index)]]
    
    mcrop <- xtitle
    if(is.null(mcrop)) mcrop <- "Crop"
    
    if(!is.null(xtitle)){
      if(xtitle == "Other"  && input[[paste0("cropCommonNameInter_", index, "_other")]] != "")  
        xtitle <- input[[paste0("cropCommonNameInter_", index, "_other")]]
      else xtitle <- input[[paste0("cropCommonNameInter_", index)]]
    }
    else {
      xtitle= "Crop"
    }
    
    
    mtitle <- paste0(xtitle, " Measurement")
    ptitle <- paste0(xtitle, " Phenology")
    
    intercropVars$pheno[[index]] <- pheno_vars
    insertTab(inputId = "intercropPhenoTabs",
              tabPanel(title = textOutput(paste0("intercrop_tab_pheno_title_",index)) , value = paste0("intercrop_tab_pheno_",index), 
                       br(),
                       column(12,renderDataTable(intercropVars$pheno[[index]] , options = list(lengthChange = FALSE)))
              ), 
              position="after",
              target = ptarget
    )
    
    insertTab(inputId = "intercropMeasuTabs",
              tabPanel(title = textOutput(paste0("intercrop_tab_measu_title_",index)) , value = paste0("intercrop_tab_measu_",index),
                       br(),
                       column(12,DTOutput(paste0("crop_measurement_table_", index)))
              ), 
              position="after",
              target = mtarget
    )
    
    
    isolate(renameTab(paste0("intercrop_tab_measu_title_",index), mtitle))
    isolate(renameTab(paste0("intercrop_tab_pheno_title_",index), ptitle))
    updateCropMeasurementTable(index,mcrop)
  }
  
  updateCropMeasurementTable <- function(index, crop_in){
    aux <- dplyr::filter(as.data.frame(dict),Crop==crop_in)

    if(crop_in == "Other") {
      newVal <- trim(input[[paste0("cropCommonNameInter_", index, "_other")]])
      if(newVal == "") newVal <- "Other"
      aux$Crop<- rep(newVal, length(aux$Crop))
    }
    
    
    output[[paste0("crop_measurement_table_", index)]] <- renderDataTable(
      data.table(aux),
      escape = FALSE,
      options = list(
        scrollX = TRUE,
        pageLength = 25,
        columnDefs = list(list(visible=FALSE, targets=c(1,6)))
      )
    )
  }
  
  
  renameTab <- function(id, name){
    output[[id]] <- renderText({name})
  }
  
  output$uiWeatherTab <- renderUI({
    column(12, 
           br(),
           fluidRow(
             box(status = "primary", width = 12,collapsible = TRUE, collapsed = TRUE,solidHeader = TRUE,
                 id="manual_measurement_boxid",
                 title = checkboxInput("manualMeasurement_checkbox", actionLink("manual_measurement_titleId", "Manual measurement"), F),
                 DTOutput('weatherManualDT')
             )
             
           ),
           br(),
           fluidRow(
             box(status = "primary",width = 12, collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
                 id="station_measurement_boxid",
                 title = checkboxInput("stationMeasurement_checkbox", actionLink("station_measurement_titleId", "Weather station measurement"), F),
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
  
 

  drawAgroBoxes <- function(index){
    ## adding harvest box
    
    index_harv <- index
    crop <- NULL
    xtitle <- "Crop"
    
    isolate(
      if(input$croppingType == 'Monocrop'){
        expCondsVars$ids_harvest <- c()
        str_id <- stri_rand_strings(1, 8,  '[A-Z]')
        expCondsVars$ids_harvest <- c(expCondsVars$ids_harvest, paste0(str_id))
        index_harv <- str_id
        expCondsVars$num_harvest <- 1
      }
      else{
        crop <- input[[paste0("cropCommonNameInter_", index)]]
        if(!is.null(crop)){
          if(crop == "Other"){
            if(input[[paste0("cropCommonNameInter_", index, "_other")]] == ''){
              xtitle <- "Other"
            }
            else {
              xtitle <- input[[paste0("cropCommonNameInter_", index, "_other")]]
            }
          }
          else{
            xtitle <- crop
          }
        }
      }
    )
    
    drawBoxHarvest(index_harv)
    
    ## adding planting transplanting box
    insertUI(
      selector = "#fr_plantingTransplating_reference_point",
      where = "beforeBegin",
      ui <- uiPlantingTransplantingBox(index)
    )
    if(is.null(crop)){
      renameAgroBoxes(index_harv, index)  
    }
    else{
      renameAgroBoxes(index_harv, index, xtitle)
    }
    
    
    
  }
  
  drawAgroBoxesIntercrop <- function(){
    isolate(ids <- intercropVars$ids)
    for( id in ids){
      vars <- unlist(strsplit(id,"_")) 
      drawAgroBoxes(vars[2])
    }
  }
  renameAgroBoxes <- function(index_harvest, index_planting, crop = ""){
    titleHarvest <- "Harvest details"
    titlePlanting <- "Planting & Transplanting details"
    isolate(
      if(input$croppingType == 'Intercrop'){
          if(crop == ""){
            titleHarvest <- "Harvest details: Crop"
            titlePlanting <- "Planting & Transplanting details: Crop"
          }
          else{
            titleHarvest <- paste0("Harvest details: ", crop)
            titlePlanting <- paste0("Planting & Transplanting details: ", crop)
          }
          
        }
     )
      
      output[[paste0("planting_title_", index_planting)]] <- renderText({titlePlanting})
      output[[paste0("harvest_title_", index_harvest)]] <- renderText({titleHarvest})
    
  }
  
  drawBoxHarvest <- function(index){
    insertUI(
      selector = "#fr_harvest_reference_point",
      where = "beforeBegin",
      ui = uiHarvestBox(index)
    )
  }
  
  
  
  uiHarvestBox <- function(index){
    fluidRow( id = paste0("fr_harvestbox_", index),
              box(id=paste0("desc_harvest_boxid_",index ),
                  title = actionLink(paste0("desc_harvest_titleId_", index), uiOutput(paste0("harvest_title_", index))),
                  # title = div(id=paste0("desc_harvest_titleId_", index), "Harvest detailssss"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12, collapsible = TRUE, collapsed = T,
                  fluidRow(
                    column(width = 6,
                           fluidRow(
                             column(width = 6,
                                    #dateInput(paste0("harvest_start_date_", index), label ="Start date", format = "yyyy-mm-dd")
                                    #airDatepickerInput("harvest_start_date", "Start date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                    #uiOutput("h_start_date")
                                    
                                    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                                      airDatepickerInput(paste0("harvest_start_date_", index),
                                                         "Start date",
                                                         clearButton = T,
                                                         autoClose = T,
                                                         #value = as.Date(input$fbDesign_project_start_date) + 1,
                                                         placeholder = "yyyy-mm-dd",
                                                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                                                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                                                         
                                      )
                                    } else {
                                      airDatepickerInput(paste0("harvest_start_date_", index),
                                                         "Start date",
                                                         clearButton = T,
                                                         autoClose = T,
                                                         placeholder = "yyyy-mm-dd"
                                      )
                                    }
                             ),
                             column(width = 6,
                                    #dateInput(paste0("harvest_end_date_", index), label ="End date", format = "yyyy-mm-dd")
                                    #airDatepickerInput("harvest_end_date", "End date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                    #uiOutput("h_end_date")
                                    
                                    if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                                      airDatepickerInput(paste0("harvest_end_date_", index),
                                                         "End date",
                                                         clearButton = T,
                                                         autoClose = T,
                                                         #value = as.Date(input$fbDesign_project_start_date) + 1,
                                                         placeholder = "yyyy-mm-dd",
                                                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                                                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                                                         
                                      )
                                    } else {
                                      airDatepickerInput(paste0("harvest_end_date_", index),
                                                         "End date",
                                                         clearButton = T,
                                                         autoClose = T,
                                                         placeholder = "yyyy-mm-dd"
                                      )
                                    }
                             )
                           ),
                           
                           fluidRow(
                             column(6,
                                    selectizeInput(paste0("harvest_method_", index), label = "Harvest method", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                     c("Baling", "Cutting", "Mowing", "Haymaking", "Picking", "Threshing", "Trussing", "Windrowing","Winnowing","Other")
                                    ))
                             
                             
                           ),
                           
                           hidden(textInput(paste0("harvest_method_", index,"_other"), "")),
                           selectizeInput(paste0("crop_component_harvested_", index), label = "Crop component harvested", multiple = TRUE, options = list(maxItems =11, placeholder ="Select one..."), choices =
                                            c("Canopy", "Aboveground biomass","Leaves","Stems","Seed","Pod", "Grain", "Tuber","Roots (excluding storage roots)", "Storage roots",
                                              "Other")
                           ),
                           
                           hidden(textInput(paste0("crop_component_harvested_",index,"_other"), "")),
                           
                           selectizeInput(paste0("crop_harvestable_area_", index), label = "Harvestable area", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                            c("m2 units", "Individual plants","Rows","Entire plot","Other")
                           ),
                           
                           
                           conditionalPanel(paste0("input.crop_harvestable_area_",index, " == 'm2 units'"),
                                            textInput(paste0("crop_component_harvested_m2_",index), "Number of m2 units harvested")
                           ),
                           conditionalPanel(paste0("input.crop_harvestable_area_",index, " == 'Individual plants'"),
                                            textInput(paste0("crop_component_harvested_ip_",index), "Number of plants harvested")
                           ),
                           conditionalPanel(paste0("input.crop_harvestable_area_",index, " == 'Rows'"),
                                            fluidRow(
                                              column(6,
                                                     textInput(paste0("crop_component_harvested_num_",index), "Number of rows harvested")
                                              )
                                            ),
                                            
                                            
                                            fluidRow(
                                              column(6,
                                                     textInput(paste0("crop_component_harvested_len_",index), "Length of rows harvested")
                                                     
                                              ),
                                              column(6,
                                                     selectizeInput(paste0("crop_component_harvested_lenunit_",index),  label ="Unit", multiple = TRUE, options = list(maxItems =11, placeholder ="Select one..."), choices =
                                                                      c("cm", "m", "in","ft"))
                                              )
                                            ),
                                            fluidRow(
                                              column(6,
                                                     textInput(paste0("crop_component_harvested_width_",index), "Width within rows harvested")
                                              ),
                                              column(6,
                                                     selectizeInput(paste0("crop_component_harvested_widthunit_",index),  label ="Unit", multiple = TRUE, options = list(maxItems =11, placeholder ="Select one..."), choices =
                                                                      c("cm", "m", "in","ft"))
                                              )
                                            ),
                                            fluidRow(
                                              column(6,
                                                     numericInput(paste0("space_rows_harvested_", index), "Space between rows harvested", value = "", min = 0, step = 0.1)
                                              ),
                                              column(6,
                                                     selectizeInput(paste0("crop_component_harvested_spaceunit_",index),  label ="Unit", multiple = TRUE, options = list(maxItems =11, placeholder ="Select one..."), choices =
                                                                      c("cm", "m", "in","ft"))
                                              )
                                            )
                           ),
                           conditionalPanel(paste0("input.crop_harvestable_area_",index, " == 'Entire plot'"),
                                            fluidRow(
                                              column(6,
                                                     textInput(paste0("crop_component_harvested_entire_",index), "Plot area harvested")
                                              ),
                                              column(6,
                                                     selectizeInput(paste0("crop_component_harvested_entireunit_",index),  label ="Unit", multiple = TRUE, options = list(maxItems =11, placeholder ="Select one..."), choices =
                                                                      c("m2", "ha", "ft2","ac"))
                                              )
                                            )
                           ),
                           hidden(textInput(paste0("crop_harvestable_area_", index,"_other"), "")),
                           fluidRow(
                             column(width = 6,
                                    numericInput(paste0("amount_harvested_", index), "Amount harvested", value = "", min = 0, step = 0.1)
                             ),
                             column(width = 6,#IMPLEMENTAR EN EXCEL
                                    selectizeInput(paste0("amount_harvested_unit_", index), label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("g", "kg", "lb", "t"))
                             )
                           ),
                           
                           
                           
                           #textInput("num_rows_harvested", "Number of rows harvested"),
                           
                           # textInput(paste0("num_plants_area_harvested_", index), "Number of plants in area harvested"),
                           
                           fluidRow(
                             column(width = 6,
                                    numericInput(paste0("harvest_cut_height_", index), "Harvest cut height", value = "", min = 0, step = 0.1)
                             ),
                             column(width = 6,
                                    selectizeInput(paste0("harvest_cut_height_unit_", index), label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("cm", "ft", "in", "m"))
                             )
                           ),
                           textAreaInput(inputId = paste0("harvest_notes_", index), label = "Notes", value = "")
                           
                    ),
                    column(width = 6,
                           br(),
                           isolate(
                             if(input$croppingType == 'Monocrop'){
                               column(12, 
                                      style='padding:0px; text-align:right; ',  actionButton(paste0("closeBox_HARV_",index ), "Remove"),
                                      br(),br()
                               )                               
                             }
                           ),
                           
                          
                           
                           fluidRow(
                             box(
                               title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                               # selectizeInput(paste0("harvest_technique_", index), label = "Technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                               #                  c("Manual",
                               #                    "Mechanized")
                               # ),
                               selectizeInput(paste0("harvest_implement_", index), label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                c("Baler",
                                                  "Chopper",
                                                  "Combine",
                                                  "Digger",
                                                  "Mower",
                                                  "Reaper",
                                                  "Roller",
                                                  "Sickle",
                                                  "Other")
                               ),
                               hidden(textInput(paste0("harvest_implement_", index, "_other"), "")),
                               # textInput("harvest_make", value="", label = "Implement make"),
                               # textInput("harvest_model", value="", label = "Implement model"),
                               selectizeInput(paste0("harvest_traction_" , index), label = "Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                c("Animal",
                                                  "Manual",
                                                  "2 wheel tractor",
                                                  "4 wheel tractor",
                                                  "Other"
                                                )
                               ),
                               hidden(textInput(paste0("harvest_traction_",index,"_other"), ""))
                               
                             ))
                    ))
              ))#end box description harvest
  }
  
  uiPlantingTransplantingBox <- function(index){
    fluidRow(id= paste0("fr_plantingTrasplanting_", index),
             box( id = paste0("plantingTransplanting_boxid_", index),
                  title = actionLink(paste0("plantingTransplanting_titleId_", index), uiOutput(paste0("planting_title_", index)) ),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12, collapsible = TRUE,  collapsed = TRUE,
                  fluidRow(
                    
                    box(id=paste0("direct_seeding_boxid_", index),
                        title = checkboxInput(paste0("directSeeding_checkbox_", index), actionLink(paste0("direct_seeding_titleId_", index), "Direct seeding"), F),
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12, collapsible = TRUE,  collapsed = TRUE,
                        
                        fluidRow(
                          column(width = 6,
                                 fluidRow(
                                   column(width = 6,
                                          #dateInput(paste0("planting_start_date_", index), label ="Start date", format = "yyyy-mm-dd")
                                          #airDatepickerInput("planting_start_date", "Start date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                          # uiOutput("pl_start_date")
                                          
                                          if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                                            airDatepickerInput(paste0("planting_start_date_", index),
                                                               "Start date",
                                                               clearButton = T,
                                                               autoClose = T,
                                                               #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                                                               placeholder = "yyyy-mm-dd",
                                                               minDate = as.Date(input$fbDesign_project_start_date) + 1,
                                                               maxDate = as.Date(input$fbDesign_project_end_date) + 1
                                                               
                                            )
                                          } else {
                                            airDatepickerInput(paste0("planting_start_date_", index),
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
                          
                          column(width = 6,
                                 fluidRow(
                                   box(
                                     title = "Planting, transplanting method", solidHeader = TRUE, status = "warning", width=12,
                                     
                                     # textInput("planting_directSeeding", value="", label = "Direct seeding"),
                                     selectizeInput(paste0("seeding_environment_", index), label = "Seeding environment", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                      c("Flat seed bed",
                                                        "Hill",
                                                        "Ridge", 
                                                        "Other")
                                     ),
                                     hidden(textInput(paste0("seeding_environment_", index, "_other"), "", value="")),
                                     selectizeInput(paste0("seeding_technique_", index), label = "Seeding technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                      c("Broadcasting",
                                                        "Line sowing",
                                                        "Dibbling"
                                                      )
                                     ),
                                     hidden(textInput(paste0("seeding_technique_", index,"_other"), "", value="")),
                                     textInput(paste0("seed_treatment_", index), value="", label = "Seed treatment")
                                     
                                   )),
                                 
                                 # column(width = 6,
                                 fluidRow(
                                   box(
                                     title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                     selectizeInput(paste0("seeding_implement_type_", index), label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                      c("Bucket broadcaster",
                                                        "Dibbling stick",
                                                        "Drum seeder",
                                                        "Jab planter",
                                                        "Seed drill",
                                                        "Other"
                                                      )
                                     ),
                                     hidden(textInput(paste0("seeding_implement_type_", index, "_other"), "", value="")),
                                     selectizeInput(paste0("seeding_traction_", index), label = "Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                      c("Animal",
                                                        "Manual",
                                                        "2 wheel tractor",
                                                        "4 wheel tractor",
                                                        "Other"
                                                      )
                                     ),
                                     hidden(textInput(paste0("seeding_traction_", index, "_other"), "", value=""))
                                   )
                                 )
                                 # )
                          ),
                          column(width = 6,
                                 fluidRow(
                                   box(
                                     title = "Seeding density", solidHeader = TRUE, status = "warning", width=12,
                                     fluidRow(
                                       column(width = 6,
                                              numericInput(paste0("distance_rows_", index),  label = "Distance between rows", min=0, max=100, step=0.1,value=NULL)
                                       ),
                                       column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                              selectizeInput(paste0("distance_rows_unit_", index), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                               c("cm",
                                                                 "ft",
                                                                 "in",
                                                                 "m")
                                              )
                                       )
                                     ),
                                     fluidRow(
                                       column(width = 6,
                                              numericInput(paste0("seeding_rate_", index),  label = "Seeding rate", min=0, max=100, step=1,value=NULL)
                                       ),
                                       column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                              selectizeInput(paste0("seeding_rate_unit_", index), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                               c("kg/ha",
                                                                 "lb/ac",
                                                                 "plants/pot")
                                              )
                                       )
                                     ),
                                     # textInput("seeds_per_hil", "Seeds/seedlings per hill", value =""),
                                     fluidRow(
                                       column(width = 6,
                                              numericInput(paste0("distance_plants_", index),  label = "Distance between plants", min=0, max=100, step=0.1,value=NULL)
                                       ),
                                       column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                              selectizeInput(paste0("distance_plants_unit_",index), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                               c("cm",
                                                                 "ft",
                                                                 "in",
                                                                 "m")
                                              )
                                       )
                                     ),
                                     numericInput(paste0("seeding_density_number_rows_", index),  label = "Number of rows", min=0, max=100, step=1, value=NULL),
                                     fluidRow(
                                       column(width = 6,
                                              numericInput(paste0("seeding_plant_density_", index),  label = "Plant density", min=0, max=100, step=0.1, value=NULL)
                                       ),
                                       column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                              selectizeInput(paste0("seeding_plant_density_unit_", index), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                               c("plants/hill",
                                                                 "plants/m2",
                                                                 "plants/pot",
                                                                 "plants/row")
                                              )
                                       )
                                     ),
                                     textAreaInput(paste0("direct_seeding_notes_", index), label="Notes", value="")
                                     
                                   )
                                 )
                          )
                        )
                        
                        
                    )
                  ),
                  fluidRow(
                    box(id=paste0("transplanting_boxid_", index),
                        title = checkboxInput(paste0("transplanting_checkbox_", index), actionLink(paste0("transplanting_titleId_", index), "Transplanting"), F),
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12, collapsible = TRUE, collapsed = TRUE,
                        fluidRow(
                          
                          column(width = 6,
                                 fluidRow(
                                   column(width = 6,
                                          #dateInput(paste0("transplanting_start_date_", index), label ="Start date", format = "yyyy-mm-dd")
                                          #airDatepickerInput("transplanting_start_date", "Start date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                          # uiOutput("trans_start_date")
                                          
                                          if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                                            airDatepickerInput(paste0("transplanting_start_date_", index),
                                                               "Start date",
                                                               clearButton = T,
                                                               autoClose = T,
                                                               #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                                                               placeholder = "yyyy-mm-dd",
                                                               minDate = as.Date(input$fbDesign_project_start_date) + 1,
                                                               maxDate = as.Date(input$fbDesign_project_end_date) + 1
                                                               
                                            )
                                          } else {
                                            airDatepickerInput(paste0("transplanting_start_date_", index),
                                                               "Start date",
                                                               clearButton = T,
                                                               autoClose = T,
                                                               placeholder = "yyyy-mm-dd"
                                            )
                                          }
                                          
                                   ),
                                   column(width = 6,
                                          #dateInput(paste0("transplanting_end_date_", index), label ="End date", format = "yyyy-mm-dd")
                                          #airDatepickerInput("transplanting_end_date", "End date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                          # uiOutput("trans_end_date")
                                          
                                          if (!is.null(input$fbDesign_project_start_date) && !is.null(input$fbDesign_project_end_date)) {
                                            airDatepickerInput(paste0("transplanting_end_date_", index),
                                                               "End date",
                                                               clearButton = T,
                                                               autoClose = T,
                                                               #value = as.Date(input$fbDesign_project_time_line[1]) + 1,
                                                               placeholder = "yyyy-mm-dd",
                                                               minDate = as.Date(input$fbDesign_project_start_date) + 1,
                                                               maxDate = as.Date(input$fbDesign_project_end_date) + 1
                                                               
                                            )
                                          } else {
                                            airDatepickerInput(paste0("transplanting_end_date_", index),
                                                               "End date",
                                                               clearButton = T,
                                                               autoClose = T,
                                                               placeholder = "yyyy-mm-dd"
                                            )
                                          }
                                   )),
                                 numericInput(paste0("age_seedling_", index), value="", label = "Age of seedling (days)", min=0, max=100, step=1),
                                 selectizeInput(paste0("transplanting_environment_", index), label = "Seedling environment", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                  c("Flat seed bed",
                                                    "Hill",
                                                    "Ridge",
                                                    "Other")
                                 ),
                                 hidden(textInput(paste0("transplanting_environment_", index, "_other"), "", value="")),
                                 selectizeInput(paste0("transplanting_technique_", index), label = "Technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                  c("Manual",
                                                    "Mechanical",
                                                    "Other")
                                 ),
                                 hidden(textInput(paste0("transplanting_technique_", index, "_other"), "", value="")),
                                 textInput(paste0("transplanting_treatment_", index), value="", label = "Seed treatment"),
                                 selectizeInput(paste0("trans_traction_", index), label = "Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                  c("Animal",
                                                    "Traction",
                                                    "2 wheel tractor",
                                                    "4 wheel tractor",
                                                    "Other"
                                                  )
                                 ),
                                 hidden(textInput(paste0("trans_traction_", index,"_other"), "", value=""))
                          ),
                          
                          
                          column(width = 6,
                                 fluidRow(
                                   box(
                                     title = "Transplanting density", solidHeader = TRUE, status = "warning", width=12,
                                     br(),
                                     fluidRow(
                                       column(width = 6,
                                              numericInput(paste0("trans_distance_rows_", index),  label = "Distance between rows", value="", min=0, max=100, step=0.1)
                                       ),
                                       column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                              selectizeInput(paste0("trans_distance_rows_unit_", index), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                               c("cm",
                                                                 "ft",
                                                                 "in",
                                                                 "m")
                                              )
                                       )
                                     ),
                                     fluidRow(
                                       column(width = 6,
                                              numericInput(paste0("trans_seeding_density_", index),  label = "Seedling density", value="", min=0, max=100, step=1)
                                       ),
                                       column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                              selectizeInput(paste0("trans_seeding_density_unit_", index), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                               c("plants/hill",
                                                                 "plants/m2",
                                                                 "plants/pot",
                                                                 "plants/row")
                                              )
                                       )
                                     ),
                                     numericInput(paste0("trans_num_rows_", index), "Number of rows", value ="", min=0, max=100, step=1),
                                     fluidRow(
                                       column(width = 6,
                                              numericInput(paste0("trans_distance_plants_", index),  label = "Distance between rows", value="", min=0, max=100, step=0.1)
                                       ),
                                       column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                              selectizeInput(paste0("trans_distance_plants_unit_", index), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), selected="m", choices =
                                                               c("m")
                                              )
                                       )
                                     ),
                                     textAreaInput(paste0("transplanting_density_notes_", index), label="Notes", value="")
                                     
                                   )
                                 )
                                 
                                 
                          )
                        ) #end fluidrow,
                        
                        
                    ) #end box sowing
                  )
             )
    )
  }
  
  
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


  #Phenomic DataTable ###################################################################
  output$phenoDT = renderDT(
    pheno_vars %>% dplyr::select(Level, Sublevel)  , options = list(lengthChange = FALSE)
  )
  
  #Weather Manual DataTable #############################################################
  output$weatherManualDT = renderDT(
    
    dplyr::filter(weather_vars, Group == "Manual measurement") %>% dplyr::select(Variable, Unit) ,  options = list(lengthChange = FALSE) 
  )
  #Weather Station DataTable #############################################################
  output$weatherStationDT = renderDT(
    dplyr::filter(weather_vars, Group == "Weather station") %>% dplyr::select(Variable, Unit) , options = list(lengthChange = FALSE)
  )
  
  #Soil## Station DataTable #############################################################
  output$soilDT = renderDT(
    soil_vars %>% dplyr::select(Variable, Unit) , options = list(lengthChange = FALSE)
  )

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

  
  #The output object will be suspended (not execute) when it is hidden on the web page ####
  # outputOptions(output, 'condition_selmlist', suspendWhenHidden=FALSE)
  ### End of Create an object with the list of file


  
  # Number of plant per row (calculated variable) #########################################
  react_plantxplot <-  shiny::reactive({

    plantxplot <- input$fbDesign_nplantsrow*input$fbDesign_nrowplot
    if(length(plantxplot)==0){plantxplot <- 0}
    plantxplot

  })

  ### Shiny UI for number of plants per plot #############################################
  output$fbPlant_plot <- shiny::renderUI({

    rpplot <- react_plantxplot()
    shiny::numericInput("fbDesign_nplants",
                        "Number of plants per plot", rpplot , rpplot, rpplot)

  })

  ### Plot Size Values ###################################################################
  react_psize <- reactive({
    plot_size <- input$fbDesign_nplantsrow*input$fbDesign_distPlants*input$fbDesign_nrowplot*input$fbDesign_distRows
    print(plot_size)
    if(length(plot_size)==0){plot_size <- 0}
    plot_size
  })


  
  ### Plot Size #########################################################################
  output$fbPlanting_psize <- shiny::renderUI({
    #plot_size <- input$fbDesign_nplantsrow*input$fbDesign_distPlants*input$fbDesign_nrowplot*input$fbDesign_distRows
    plot_size <- react_psize()
    #if(length(plot_size)==0) plot_size <- 2.7
    shiny::numericInput(inputId = "fbDesign_psize", label = "Plot size (m2)",
                        value = plot_size, min = plot_size,max = plot_size)
  })



  ### Reactive Plant densisty ###########################################################
  react_pdensity <-  shiny::reactive({

    #plant_density <- (input$fbDesign_nplants/input$fbDesign_psize)*10000

    nplantxplot <- react_plantxplot()

    plant_density <- (nplantxplot/input$fbDesign_psize)*10000
    print(plant_density)
    if(length(plant_density)==0){plant_density <- 0}
    plant_density
  })


  ### Select Plant density #############################################################
  output$fbPlanting_pdensity <- shiny::renderUI({
    plant_density <- react_pdensity()
    #if(length(plant_density)==0) plant_density <- 37037.037
    shiny::numericInput(inputId = "fbDesign_pdensity", label = "Plant density (plants/Ha)",
                        value = plant_density, min = plant_density, max = plant_density)
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


  ### Material List Export, ##########################################################

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


   ### Reactive Factor and Levels ####################################################

   ### Factor 1  #####################################################################
   factor1StartDateInputs <- reactive({

     if(input$fullFactorialRB=="Yes"){
       nfactors<- input$nfactors_hdafims_y
       nlevels <- input$numLevels_1
     } else {
       nfactors<- input$nfactors_hdafims_n
       nlevels <- 1
     }
     
    
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

     if(input$fullFactorialRB=="Yes"){
       nfactors<- input$nfactors_hdafims_y
       nlevels <- input$numLevels_1
     } else {
       nfactors<- input$nfactors_hdafims_n
       nlevels <- 1
     }
     
     
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
   ###################################################################################

   ### Factor 2  #####################################################################
   factor2StartDateInputs  <- reactive({
      
     if(input$fullFactorialRB=="Yes"){
       nfactors<- input$nfactors_hdafims_y
       nlevels <- input$numLevels_2
     } else {
       nfactors<- input$nfactors_hdafims_n
       nlevels <- 1
     }
     
     
     fl <- list()
     print("f2 start date levels and factors")
     print(nlevels)
     print(nfactors)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_start_date_", 2, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor2EndDateInputs <- reactive({

     if(input$fullFactorialRB=="Yes"){
       nfactors<- input$nfactors_hdafims_y
       nlevels <- input$numLevels_2
     } else {
       nfactors<- input$nfactors_hdafims_n
       nlevels <- 1
     }
     
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
   ##################################################################################

   ### Factor 3  #####################################################################
   factor3StartDateInputs  <- reactive({

     if(input$fullFactorialRB=="Yes"){
       nfactors<- input$nfactors_hdafims_y
       nlevels <- input$numLevels_3
     } else {
       nfactors<- input$nfactors_hdafims_n
       nlevels <- 1
     }
    
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_start_date_", 3, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor3EndDateInputs <- reactive({

     if(input$fullFactorialRB=="Yes"){
       nfactors<- input$nfactors_hdafims_y
       nlevels <- input$numLevels_3
     } else {
       nfactors<- input$nfactors_hdafims_n
       nlevels <- 1
    }
     
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
   ###################################################################################

   ### Factor 4  #####################################################################

   factor4StartDateInputs  <- reactive({

     if(input$fullFactorialRB=="Yes"){
       nfactors<- input$nfactors_hdafims_y
       nlevels <- input$numLevels_4
     } else {
       nfactors<- input$nfactors_hdafims_n
       nlevels <- 1
     }
     
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_start_date_", 4, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor4EndDateInputs <- reactive({

     if(input$fullFactorialRB=="Yes"){
       nfactors<- input$nfactors_hdafims_y
       nlevels <- input$numLevels_4
     } else {
       nfactors<- input$nfactors_hdafims_n
       nlevels <- 1
     }
     
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
   ###################################################################################

   ### Factor 5  #####################################################################
   factor5StartDateInputs  <- reactive({

     if(input$fullFactorialRB=="Yes"){
       nfactors<- input$nfactors_hdafims_y
       nlevels <- input$numLevels_5
     } else {
       nfactors<- input$nfactors_hdafims_n
       nlevels <- 1
     }
     
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_start_date_", 5, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor5EndDateInputs <- reactive({

     if(input$fullFactorialRB=="Yes"){
       nfactors<- input$nfactors_hdafims_y
       nlevels <- input$numLevels_5
     } else {
       nfactors<- input$nfactors_hdafims_n
       nlevels <- 1
     }
     
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
   ###################################################################################

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



   
  # Fieldbook design ################################################################

   #Experimental design inputs
   #design inputs from experimental design tab
   dffinputs <-  function(){
     
     f1inputs <- fctinputs(gr1= input$sel1_1, gr2= input$sel1_2, gr3= input$sel1_3, napp= nappf1(), rtf = rtf1(),  ref= ref1(), dfr= f1())
     #Factor 2
     f2inputs <- fctinputs(gr1= input$sel2_1, gr2= input$sel2_2, gr3= input$sel2_3, napp= nappf2(), rtf = rtf2(),  ref= ref2(), dfr= f2())
     #Factor 3
     f3inputs <- fctinputs(gr1= input$sel3_1, gr2= input$sel3_2, gr3= input$sel3_3, napp= nappf3(), rtf = rtf3(),  ref= ref3(), dfr= f3())
     #Factor 4
     f4inputs <- fctinputs(gr1= input$sel4_1, gr2= input$sel4_2, gr3= input$sel4_3, napp= nappf4(), rtf = rtf4(),  ref= ref4(), dfr= f4())
     #Factor 5
     f5inputs <- fctinputs(gr1= input$sel5_1, gr2= input$sel5_2, gr3= input$sel5_3, napp= nappf5(), rtf = rtf5(),  ref= ref5(), dfr= f5())
 
     out<- list(f1inputs =f1inputs, f2inputs =f2inputs,f3inputs =f3inputs,f4inputs =f4inputs,f5inputs =f5inputs)
   }
   #fb_agrofims <- shiny::reactive({
     
 
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

  ###############################Agrofeatures #######################################
  
  AllInputs <- reactive({
     x <- reactiveValuesToList(input)
     for(i in 1:length(names(x))){
       #if(is.null(x[[i]])){
       #  x[[i]]<- "novalue"
       #} else if(length(x[[i]])>1) {
       x[[i]]<-paste(x[[i]], collapse=", ")
       #}
     }
     data.frame(
       id = names(x),
       values = unlist(x, use.names = FALSE)
     )
   }) 
   
  #Residue management ###############################################################
  residue <- reactive({
     
    dt <- AllInputs() %>% filter(str_detect(id, "residue_"))
    dt <- t(dt) %>% as.data.frame()
    names(dt) <- paste(names(dt), 1:ncol(dt))
    dt
    
    # plantp<- map_singleform_values(input = input$residue_plantPart, input_other = input$residue_plantPart_other, 
    #                            type = "select", format = "data.frame", label = "Factor") 
    # cmoi <- map_singleform_values(input =input$crop_residue_moisture, type = "select", format = "data.frame", label = "Factor") 
    # 
    # r_thick <-  map_singleform_values(input =input$crop_residue_thick , type = "numeric", format = "data.frame", label = "Factor")
    # r_thick_unit <- map_singleform_values(input =input$crop_residue_thick_unit, type = "select", format = "data.frame", label = "Factor") #unit
    # 
    # r_amount <-  map_singleform_values(input =input$crop_residue_amount_sqm, type = "numeric", format = "data.frame", label = "Factor")
    # r_amount_unit <- map_singleform_values(input =input$crop_residue_amount_sqm_unit,  type = "select", format = "data.frame", label = "Factor") #unit
    # 
    # r_cov <-  map_singleform_values(input =input$crop_residue_perc_cov, type = "numeric", format = "data.frame", label = "Factor")
    # r_cov_unit <- map_singleform_values(input =input$crop_residue_perc_cov_unit, type = "select", format = "data.frame", label = "Factor")  #unit
    # 
    # r_notes <- input$residue_management_notes
    # 
    # dt<- rbind(plantp, cmoi, r_thick, r_thick_unit, r_amount, r_amount_unit, r_cov, r_cov_unit)
    # dt<- t(dt) %>% as.data.frame()
    # dt
    # 
    # namesdt<-  c("Plant part", "Crop residue moisture", "Crop residue thickness", "Crop residue amount",
    #         "Crop residue percent of coverage","Notes")
    # 
            
    
  })
  res_mgt <- reactive({
     r_start_date<-  map_singleform_values(input =input$residue_start_date, type = "date",
                                           format = "data.frame", label = "Factor")#dates
     r_technique <-  map_singleform_values(input =input$residue_technique, input$residue_technique_other, 
                                           type = "select",format = "data.frame", label = "Factor")#dates
     r_traction <-  map_singleform_values(input =input$residue_traction, input$residue_traction_other,
                                          type = "select",format = "data.frame", label = "Factor")#dates
     r_notes <- input$residue_management_notes
     
     dt<-rbind(r_start_date, r_technique, r_traction, r_notes)
     dt<- t(dt) %>% as.data.frame()
     dt
     
   })
  
  #seedbed preparation  #############################################################
  seedbed <- reactive({
    
    flag<-FALSE
    
    if(input$landLevelling_checkbox==TRUE){
      
      ll_start_date <- map_singleform_values(input =input$landLeveling_start_date, type = "date", format = "data.frame", label = "Factor" ) #dates
      #ll_end_date <- getDateInput(input$landLeveling_end_date) #dates
      ll_npasses <- map_singleform_values(input =input$numPasses,  type = "numeric", format = "data.frame", 
                                          label = "Factor" )
      ll_notes <-  map_singleform_values(input =input$landLeveling_notes,  type = "text", format = "data.frame",
                                         label = "Factor")
      ll_type <-  map_singleform_values(input$land_impl_type, type = "select", format = "data.frame", label = "Factor")
      ll_traction <-  map_singleform_values(input$land_traction, input$land_traction_other, type = "select", format = "data.frame", label = "Factor")
      
      llNames<- c("Land levelling start date (yyyy/mm/dd)", "Land levelling end date (yyyy/mm/dd)", 
                  "Land levelling Total number of levelling passes", "Land levelling Notes",
                  "Land levelling Type", "Land levelling traction")
      
      names(lldt) <- llNames
      flag  <- TRUE
      out   <- lldt
    }
    
    if(input$puddling_checkbox==TRUE){
      lp_start_date <-  map_singleform_values(input =input$puddling_start_date,
                                              type = "date", format = "data.frame", label = "Factor" )
      
      lp_depth_val <- map_singleform_values(input = input$puddling_depth_val, 
                                            type = "numeric", format = "data.frame", label = "Factor" )
      lp_depth_unit <- map_singleform_values(input = input$puddling_depth_unit, 
                                             type = "select", format = "data.frame", label = "Factor")
      
      lp_npasses <- map_singleform_values(input = input$puddling_total_number_puddling_passes,  
                                          type = "numeric", format = "data.frame", label = "Factor")
      lp_notes <- map_singleform_values(input =input$puddling_notes, 
                                        type = "text", format = "data.frame", label = "Factor")
      lp_type <- map_singleform_values(input = input$pud_impl_type, input$pud_impl_type_other, 
                                       type = "select", format = "data.frame", label = "Factor")
      lp_traction <- map_singleform_values(input$pud_traction, input$pud_traction_other, 
                                           type = "select", format = "data.frame", label = "Factor")
      
      lpNames <- c("Puddling start date (yyyy/mm/dd)", "Puddling end date (yyyy/mm/dd)",
                   "Puddling depth", "Puddling depth Unit (cm; ft; in; m)",
                   "Puddling Total number of puddling passes",
                   "Puddling notes", "Puddling type", "Puddling traction")
      
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
    }
    
    if(input$tillage_checkbox==TRUE){
      
      lt_start_date <- map_singleform_values(input = input$tillage_start_date, type = "date", format = "data.frame", label = "Factor" )
      #lt_end_date  <-  getDateInput(input$tillage_end_date)
      lt_technique  <- map_singleform_values(input =input$till_technique, input$till_technique_other,
                                             type = "select", format = "data.frame", label = "Factor")
      #lt_depth_method  <- getAgrOper(input$till_depth_method)
      
      lt_depth  <- map_singleform_values(input = input$tillage_depth,  type = "numeric", format = "data.frame", label = "Factor")
      lt_depth_unit  <- map_singleform_values(input = input$tillage_depth_unit,type = "select", format = "data.frame", label = "Factor" )
      #lt_depth_lbl <- paste("Tillage depth", lt_depth_unit, sep="_") #label
      
      lt_npasses  <- map_singleform_values(input =input$total_number_tillage_passes, type = "numeric", format = "data.frame", label = "Factor")
      lt_notes  <- map_singleform_values(input$tillage_notes, type = "text", format = "data.frame",
                                         label = "Factor")
      lt_type  <-  map_singleform_values(input =input$till_impl_type, input$till_impl_type_other, 
                                         type = "select", format = "data.frame", label = "Factor")
      lt_traction <- map_singleform_values(input =input$till_traction, input$till_traction_other,
                                           type = "select", format = "data.frame", label = "Factor")
      
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
    
    if(input$landLevelling_checkbox==FALSE && input$puddling_checkbox==FALSE && input$tillage_checkbox== FALSE){
      out<-data.frame()
    }
    
    out <- out
    
  })

  ## Soil Fertility
  dt_soilFertility <- reactive({
    
    ft <- AllInputs() %>% filter(str_detect(id, "^select_fertilizerType_soil_table_row_"))
    ps <- AllInputs() %>% filter(str_detect(id, "^select_product_soil_table_row_"))
    pr<- AllInputs() %>% filter(str_detect(id, "^input_productRate_soil_table_"))
    el <- AllInputs() %>% filter(str_detect(id, "^select_element_soil_table_row_")) 
    er <- AllInputs() %>% filter(str_detect(id, "^input_elementRate_soil_table_row_"))  
    startD <- AllInputs() %>% filter(str_detect(id, "^input_startdate_soil_table_row_"))   
    endD <- AllInputs() %>% filter(str_detect(id, "^input_enddate_soil_table_row_"))  
    tech <- AllInputs() %>% filter(str_detect(id, "^select_techinque_soil_table_row_"))   
    txtA <-  AllInputs() %>% filter(str_detect(id, "^textArea_soil_table_row_"))    
    
    dt<- rbind(ft, ps, pr, el, er, startD, endD, tech, txtA)
    dt <- t(dt) %>% as.data.frame()
    names(dt) <- paste(names(dt), 1:ncol(dt))
    dt
  })
  
  ### Planting & Transplanting   #####################################################################
  #direct seeding
  plating_trans <- reactive({
    dt1 <- AllInputs() %>% filter(str_detect(id, "^select_fertilizerType_soil_table_row"))
    dt2<-  AllInputs() %>% filter(str_detect(id, "^distance_rows"))
    dt3 <- AllInputs() %>%  filter(str_detect(id, "^trans"))
    dt<- rbind(dt1,dt2,dt3)
    dt<- t(dt) %>% as.data.frame()
    names(dt) <- paste(names(dt), 1:ncol(dt))
    dt
  })
 
  ## Mulching and residue ############################################################
  dt_mulching <- reactive({
    dt<- AllInputs() %>% filter(str_detect(id, "^mulch_"))
    dt<- t(dt) %>% as.data.frame()
    names(dt) <- paste(names(dt), 1:ncol(dt))
    dt
   })
 
  ## Irrigation  #####################################################################
  dt_irrigation <- reactive({
    dt<- AllInputs() %>% filter(str_detect(id, "^irrigation_"))
    dt<- t(dt) %>% as.data.frame()
    names(dt) <- paste(names(dt), 1:ncol(dt))
    dt
  })
 
  ## Weeding #########################################################################
  dt_weeding <- reactive({
    dt<- AllInputs() %>% filter(str_detect(id, "^weeding_"))
    dt<- t(dt) %>% as.data.frame()
    names(dt) <- paste(names(dt), 1:ncol(dt))
    dt
   })

  ### Harvest  ######################################################################
  dt_harvest <- reactive({
    
    dt<-AllInputs() %>% filter(str_detect(id, "^harvest_"))
    dt<- t(dt) %>% as.data.frame()
    names(dt) <- paste(names(dt), 1:ncol(dt))
    dt
  })
  
  
  ################################End agrofeatures ###################################

  
  ##################### Phenolgy,  Weather and Soil tables #######################################
  
  ##phenology  ############################################
  pheno_dt <- reactive({
   
      row_select <- input$phenoDT_rows_selected
      a<- pheno_vars[row_select, ]
      if(nrow(a)>0){
        a<- a[, c("Level", "Sublevel")]
      } else {
        a <- data.frame(Level = NA, Sublevel = NA)
      } 
      a
      
  })
  
  ##reactive weather   ####################################
  weather_dt <- reactive({
    
   a1 <- data.frame(Variable = NA, Unit = NA)
   a2 <- data.frame(Variable = NA, Unit = NA)
   
   if(!is.null(input$manualMeasurement_checkbox)){ #!is.null: when users do not click on Weather tab
      if(input$manualMeasurement_checkbox==TRUE){
       mstation <-  dplyr::filter(weather_vars, Group == "Manual measurement") %>% dplyr::select(Variable, Unit)
       
       row_select <- input$weatherManualDT_rows_selected
       a1 <- mstation[row_select, ]
       if(nrow(a1)>0){
         a1 <- a1[, c("Variable", "Unit")]
       }
       a1<-a1
       
     } 
   }

   if(!is.null(input$stationMeasurement_checkbox)){ #!is.null: when users do not click on Weather tab
      if(input$stationMeasurement_checkbox==TRUE){
     wstation <- dplyr::filter(weather_vars, Group == "Weather station") %>% dplyr::select(Variable, Unit)
     
     row_select <- input$weatherStationDT_rows_selected
     a2<- wstation[row_select, ]
     if(nrow(a2)>0){
       a2<- a2[, c("Variable", "Unit")]
     }
     print("entro 21")
     a2 <- a2
   } 
   }  
   
   dt<- rbind(a1,a2)
    
    
  })
  
  ##reactive soil  ########################################
  soil_dt<- reactive({
    
    row_select <- input$soilDT_rows_selected
    a<- soil_vars[row_select, ]
    if(nrow(a)>0){
      a<- a[, c("Variable", "Unit")]
    } else{
      a <- data.frame(Variable = NA, Unit = NA)
    }
    a
    
  })

  #############  metadata_dt2 ########################################################
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
    id_rand_pers <-  getAddInputId(personnelVars$ids, "PERS_", "") 
    pst <- map_values(input, id_chr=" personnel_type_", id_rand_pers,format = "data.frame", lbl= "Person type")
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
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector") 
    print(ct)
    ctd <- map_singleform_values(input$croppingType, type = "combo box", format = "data.frame", "Cropping type") 
    print(ctd)
    #Crop
    #TODO: saber como hacer match entre la tabla circm y cirvar.
    if(ct=="Monocrop"){
      crp <- map_singleform_values(input$cropCommonNameMono, type= "combo box", format = "data.frame",label = "Crop")
      var<- map_singleform_values(input$cultivarNameMono, type= "combo box", format = "data.frame",label = "Crop variety(s)",collapsed = TRUE)
      out <- rbind(ctd, crp, var)
    }
    if(ct=="Intercrop"){
      id_ic_rand <- getAddInputId(intercropVars$ids, "IC_", "") 
      circm <- map_values(input, id_chr="cropCommonNameInter_", id_ic_rand, format = "data.frame", lbl= "Select crop")
      cirvar <- map_values(input, id_chr="cropVarietyName_", id_ic_rand,format = "data.frame", lbl= "Crop variety(s)")
      row <- map_values(input, id_chr="intercropValue_row_crop_", id_ic_rand, format = "data.frame", lbl= "Row geometry")
      out <- rbind(ctd, circm, cirvar, row) 
    }
    pvc<- map_singleform_values(input$prevCropName, type= "combo box", format = "data.frame",  label= "Previous crop")
    out <- rbind(out, pvc)
    names(out)<- c("Factor", "Value")
    print("out")
    print(out)
    out
  })
  
  
  #### Design tab ####################################################################
  #Unit in design
  infounit<- reactive({
    ifunit<- agdesign::map_singleform_values(input$info_experiment_unit, type="select")
    if(ifunit == "plot"  ){
     
      wi <- map_singleform_values(input = input$expt_plot_width_unit , type = "text",format = "vector", label = "Factor") 
      wunit <- map_singleform_values(input = input$expt_plot_width_unit, type = "combo box",format = "vector", label = "Factor")
      len <- map_singleform_values(input = input$expt_plot_length_unit   , type = "text",format = "vector", label = "Factor") 
      lunit <-  map_singleform_values(input = input$expt_plot_length_unit, type = "combo box",format = "vector", label = "Factor")
      wif<- paste(wi, wunit)
      lenf<-  paste(len, lunit)
      ow<- data.frame(Factor = "Width", Value = wif )
      ol<- data.frame(Factor = "Length",Value = lenf )
      out<- rbind(ow, ol)
    }
    else if(ifunit == "field"){
   
      wi <- map_singleform_values(input = input$expt_field_width_unit , type = "text",format = "vector", label = "Factor") 
      wunit <- map_singleform_values(input = input$expt_field_width_unit, type = "combo box",format = "vector", label = "Factor")
      len <- map_singleform_values(input = input$expt_field_length_unit   , type = "text",format = "vector", label = "Factor") 
      lunit <-  map_singleform_values(input = input$expt_field_length_unit, type = "combo box",format = "vector", label = "Factor")
      wif<- paste(wi, wunit)
      lenf<-  paste(len, lunit)
      ow<- data.frame(Factor = "Width", Value = wif )
      ol<- data.frame(Factor = "Length", Value = lenf )
      out<- rbind(ow, ol)
     
    }
    else if(ifunitp == "pot"){
      
      di <- map_singleform_values(input = input$pot_diameter_unit , type = "text",format = "vector", label = "Factor") 
      dunit <- map_singleform_values(input = input$pot_diameter_unit, type = "combo box",format = "vector", label = "Factor")
      de <- map_singleform_values(input = input$pot_depth_unit   , type = "text",format = "vector", label = "Factor") 
      deunit <-  map_singleform_values(input = input$pot_depth_unit, type = "combo box",format = "vector", label = "Factor")
      dif<- paste(di, dunit)
      def<-  paste(de, deunit)
      ow<- data.frame(Factor = "Width", Value = dif )
      ol<- data.frame(Factor = "Length", Value = def )
      out<- rbind(ow, ol)
      
    }
    
  })
  
  #List of Factors and Levels
  fct_lvl <- reactive({
      #Type of design                                         
      dsg <- agdesign::map_singleform_values(input$designFieldbook_agrofims, type="select",default = "CRD") %>% tolower()
      tf <- agdesign::map_singleform_values(input$fullFactorialRB,type = "select", default = "Yes") %>% tolower()
      
      path <- fbglobal::get_base_dir()
      fp <- file.path(path, "listFactors_v7.rds")
      factors <- as.data.frame(readRDS(fp))
    
    
      if(tf=="yes"){
        #nrep <-   agdesign::map_singleform_values(input$designFieldbook_agrofims_r_y, type = "combo box", default = "2" ) %>% as.numeric()
        id_ff_rand <- getAddInputId(designVars$ids_FULL, "FF_", "") 
        #id_rand<- id_ff_rand
        fg <- map_fgroup_values(input= input, id_chr ="sel_factor_", id_rand = id_ff_rand, lbl = "Factor")
        id_type_dt <- dplyr::left_join( fg, factors) #get a table with the intersection
        flvl <- map_level_values(input= input, isf = tf , id_type_dt= id_type_dt, id_rand = id_ff_rand, lbl= "Level")
      }  
      if(tf=="no"){ 
        #nrep <- agdesign::map_singleform_values(input$designFieldbook_agrofims_r_n, type = "combo box", default = "2" ) %>% as.numeric() #for blocks and reps (crd and rcbd)
        ntrt <- agdesign::map_singleform_values(input$designFieldbook_agrofims_t_n, type = "combo box", default = "2" ) %>% as.numeric()
        id_nff_rand <- getAddInputId(designVars$ids_NFULL, "NFF_", "") 
        #id_rand <- id_nff_rand
        fg<- map_fgroup_values(input= input, id_chr ="sel_factor_", id_rand = id_nff_rand, lbl = "Factor") 
        id_type_dt <- dplyr::left_join( fg, factors) #get a table with the intersection
        flvl <- map_level_values(input= input,isf = tf, id_type_dt= id_type_dt, id_rand = id_nff_rand, ntrt= ntrt , lbl= "Level")
      }
      out<- list(fg=fg, flvl= flvl)
      
  })
  
  #field design (statistical design)
  fbdesign <- function(){
    
    dsg <- agdesign::map_singleform_values(input$designFieldbook_agrofims, type="select",default = "CRD") %>% tolower()
    print(dsg)
    tf <- agdesign::map_singleform_values(input$fullFactorialRB,type = "select", default = "Yes") %>% tolower()
    print(tf)
    fct<- fct_lvl()$fg$FACTOR #get factor labels
    print(fct)
    flvl<- fct_lvl()$flvl #get factor's levels
    print(flvl)
    
    try({
        if(tf=="yes"){
          
         if(dsg!="sprcbd"){
            nrep <- as.numeric(input$designFieldbook_agrofims_r_y) #nrep
            fb <- try(st4gi::cr.f(fnames = fct, flevels = flvl, design = dsg, nrep = nrep, nc = 10)$book)
            if(dsg=="crd"){
              names(fb)[1:4] <- c("PLOT","ROW","COL","TREATMENT") #rename first 4 cols
            } 
            if(dsg=="rcbd"){
               names(fb)[1:5] <- c("PLOT","BLOCK" ,"ROW","COL","TREATMENT")  #rename first 5 cols
            }
          }
          
          if(dsg=="sprcbd"){
            nrep <- as.numeric(input$designFieldbook_agrofims_r_y) #nrep
            fb <- try( st4gi::cr.spld(fnames = fct, flevels = flvl, nb = nrep)$book)
            names(fb)[1:6] <- c("BLOCK" ,"PLOT","SUBPLOT","ROW","COL","TREATMENT")  #rename first 5 cols
          }
          # if(dsg=="strip"){
          #   nrep <- as.numeric(input$designFieldbook_agrofims_r_y) #nrep
          #   fb <- try( st4gi::cr.strd(fnames = fct, flevels = flvl, nb = nrep)$book)
          #   names(fb)[1:6] <- c("BLOCK" ,"PLOT","SUBPLOT","ROW","COL","TREATMENT")  #rename first 5 cols
          # }
          
          
        }
        if(tf=="no"){
          
          nrep <- as.numeric(input$designFieldbook_agrofims_r_n)  #for blocks and reps (crd and rcbd)
          ntrt <- agdesign::map_singleform_values(input$designFieldbook_agrofims_t_n, type = "combo box", default = "2" ) %>% as.numeric()
          nfactor <- length(fct) #number of factors
          print("n factor")
          print(nfactor)
          if(nfactor==1){
             trt <- unlist(flvl)
             if(dsg=="crd"){
               fb<- try(st4gi::cr.crd(geno = trt,nrep = nrep ,nc = 10)$book)
               names(fb)[1:4] <- c("PLOT","ROW","COL","TREATMENT")
             } 
             if(dsg=="rcbd"){
               fb<- try(st4gi::cr.rcbd(geno = trt, nb = nrep ,nc = 10)$book)
               names(fb)[1:5] <- c("PLOT","BLOCK" ,"ROW","COL","TREATMENT")
             }
          } else{
               fb<- try(st4gi::cr.f(fnames = fct,flevels = flvl, design = dsg, nrep = nrep, nc = 10)$book)
               if(dsg=="crd"){
                  fb <- fb[,1:4]
                  names(fb) <- c("PLOT","ROW","COL","TREATMENT")
               } 
               if(dsg=="rcbd"){
                  fb <- fb[,1:5]
                  names(fb) <- c("PLOT","BLOCK" ,"ROW","COL","TREATMENT")
               }
          }  
        }
        fb
    })
  }
  
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
  
  globalMetadata<- reactive({

    gtable <- rbind( exp_dt(), fa_dt(), pe(), epl(), pers_dt(),crop_dt(), site_dt())
    #gtable<- data.table::rbindlist(glist,fill = TRUE)
    #gtable <- as.data.frame(gtable,stringAsFactors=FALSE)
    names(gtable)[1]<- "Parameter"
    gtable
  })

  #############  factor_dt2 ##########################################################
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
  fbdesign_traits <- reactive({
    
    fb <- fbdesign()
    trait <- traits_dt()
    #print(trait)
    cr<- trait$Crop
    cm <- trait$Crop.measurement
    sb<- trait$Subgroup
    sc <- trait$Scale
    sc[is.na(sc)] <- "unitless"
    #co <- trait$VariableId
    cs <- paste(cr,sb, cm, sc, sep="-")
    
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
  
  
  
  ### Book preview ##################################################################
  shiny::observeEvent(input$fbDesign_draft_agrofims, {

     withProgress(message = 'Fieldbook Preview', value = 0, {

      incProgress(1/10,message = "...")
       
       # #Flag variable to know if everythin is ok
        flag <- TRUE
      
       if(class(fbdesign())=="try-error"){
         shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Select factors and levels properly"), styleclass = "danger")
         flag<-FALSE
       }

       if(flag){

       fb  <- fbdesign_traits()# fb_agrofims_traits()
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

         # n <- as.numeric(input$numApplicationsIrrigation)
         # fb_traits <- fb_agrofims_traits()
         gmetadata <- globalMetadata() #metadata_dt2()
         # trait_agrofims_dt <- traits_dt()[,-1]
         # weather <- dt_weather_agrofims()
         # soil_vars <- dt_soil_agrofims()
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
         print("inicio4")
         # print("phenology")
         # openxlsx::addWorksheet(wb, "Phenology", gridLines = TRUE)
         # openxlsx::writeDataTable(wb, "Phenology", x = phenology,
         #                          colNames = TRUE, withFilter = FALSE)
   

         if(input$croppingType=="Monocrop"){

         incProgress(7/20,message = "Adding fieldbook data...")
         openxlsx::addWorksheet(wb, "Fieldbook", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Fieldbook", x = fb,
                                  colNames = TRUE, withFilter = FALSE)
         }
         
         #Experimental conditions
        
         print("Adding residue management")
         incProgress(7/20,message = "Adding residue management")
         openxlsx::addWorksheet(wb, "Residue management", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Residue management", x = residue(),
                                 colNames = TRUE, withFilter = FALSE)

         ai <<- AllInputs()
         
         print("Addin planting")
         incProgress(7/20,message = "Adding Planting and transplating")
         openxlsx::addWorksheet(wb, "Planting_transplating", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Planting_transplating", x = plating_trans(),
                                  colNames = TRUE, withFilter = FALSE)
         
         print("mulch")
         print(dt_mulching())
         incProgress(7/20,message = "Adding mulching sheet")
         openxlsx::addWorksheet(wb, "Mulching", gridLines = TRUE)
       
         openxlsx::writeDataTable(wb, "Mulching", x = dt_mulching(),
                                  colNames = TRUE, withFilter = FALSE)
        
         print("irrigation")
         incProgress(7/20,message = "Adding irrigation sheet")
         openxlsx::addWorksheet(wb, "Irrigation", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Irrigation", x = dt_irrigation(),
                                  colNames = TRUE, withFilter = FALSE)
         
         print("weeding")
         incProgress(7/20,message = "Adding weeding sheet")
         openxlsx::addWorksheet(wb, "Weeding", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Weeding", x = dt_weeding(),
                                  colNames = TRUE, withFilter = FALSE)
         
         print("harvest")
         incProgress(7/20,message = "Adding harvest sheet")
         openxlsx::addWorksheet(wb, "Harvest", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Harvest", x = dt_harvest(),
                                  colNames = TRUE, withFilter = FALSE)
         
         print("soil")
         incProgress(7/20,message = "Adding soilt and fertility")
         openxlsx::addWorksheet(wb, "Soil fertility", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Soil fertility", x = dt_soilFertility(),
                                  colNames = TRUE, withFilter = FALSE)
         
         
         print("inicio6")
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

         
         incProgress(9/20,message = "Adding trait list sheet...")
         
         print("inicio7")
         openxlsx::addWorksheet(wb, "Trait list", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Trait list", x = traits_dt(),
                                  colNames = TRUE, withFilter = FALSE)
         
         print("inicio8")
         openxlsx::addWorksheet(wb, "Phenology", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Phenology", x = pheno_dt(),
                                  colNames = TRUE, withFilter = FALSE)
         
         print("inicio9")
         print(weather_dt())
         print(nrow(weather_dt()))
         openxlsx::addWorksheet(wb, "Weather", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Weather", x = weather_dt(),
                                  colNames = TRUE, withFilter = FALSE)
         
         print("inicio10")
         openxlsx::addWorksheet(wb, "Soil", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Soil", x = soil_dt(),
                                  colNames = TRUE, withFilter = FALSE)
         
         print("inicio11")
         
      
        
         #saveRDS(x, file = "allinputs.rds")
         x <- reactiveValuesToList(input)
         saveRDS(x, file = "allinputs.rds")
         openxlsx::addWorksheet(wb, "Inputs", gridLines = TRUE)
         openxlsx::writeDataTable(wb, "Inputs", x = AllInputs(),
                                  colNames = TRUE, withFilter = FALSE)
         
         
         # agroFeaSelected <- input$selectAgroFeature
         # #
         # #print("error5")
         # if(is.element("Irrigation", agroFeaSelected)) {
         #   print("irri")
         #   incProgress(14/20,message = "Adding irrigation data...")
         #   dt_irri <- dt_irrigation()
         #   print(dt_irri)
         #   openxlsx::addWorksheet(wb, "Irrigation", gridLines = TRUE)
         #   openxlsx::writeDataTable(wb, "Irrigation", x = dt_irri,
         #                            colNames = TRUE, withFilter = FALSE)
         # 
         # }
         # #print("error6")
         # if(is.element("Harvest", agroFeaSelected)) {
         #   # print("har")
         # 
         #   if(input$croppingType!="Intercrop"){
         # 
         #   incProgress(13/20,message = "Adding harvest data...")
         #   dt_harv <- dt_harvest()
         #   openxlsx::addWorksheet(wb, "Harvest", gridLines = TRUE)
         #   openxlsx::writeDataTable(wb, "Harvest", x = dt_harv,
         #                            colNames = TRUE, withFilter = FALSE)
         #   } else {
         # 
         #     crops <- input$cropsSelected
         #     dt_harv<-list()
         #     #interTrait <- out<-list()
         #      cropsito<- NULL
         #     for(i in 1:length(crops)){
         #       cropsito[i] <- input[[paste0("cropCommonName",i)]]
         #     }
         # 
         # 
         #     for(i in 1:length(crops)){
         # 
         #     incProgress(13/20,message = "Adding harvest data...")
         #     dt_harv[[i]] <- dt_harvest()
         #     names(dt_harv[[i]]) <- paste(cropsito[i], names(dt_harv[[i]]), sep="-")
         #     openxlsx::addWorksheet(wb, paste("Harvest",cropsito[i],sep="-") , gridLines = TRUE)
         #     openxlsx::writeDataTable(wb, paste("Harvest",cropsito[i],sep="-") , x = dt_harv[[i]],
         #                              colNames = TRUE, withFilter = FALSE)
         #    }
         # 
         #   }
         # 
         # 
         # }
         # #print("error7")
         # if(is.element("Land preparation", agroFeaSelected)) {
         #    incProgress(10/20,message = "Adding land preparation sheet...")
         # 
         #   print(input$landLevelling_checkbox)
         #   print(input$puddling_checkbox)
         #   print(input$tillage_checkbox)
         # 
         #   if(input$landLevelling_checkbox==TRUE && input$puddling_checkbox==TRUE
         #                                          && input$tillage_checkbox==TRUE){
         # 
         #    dt_land <- dt_land_description()
         # 
         #    openxlsx::addWorksheet(wb, "Land preparation", gridLines = TRUE)
         #    openxlsx::writeDataTable(wb, "Land preparation", x = dt_land ,
         #                             colNames = TRUE, withFilter = FALSE)
         #      }
         #  }
         # #print("error9")
         # if(is.element("Mulching and residue", agroFeaSelected)) {
         #   print("mu")
         #   if(input$mulchManag_checkbox==TRUE && input$residueManag_checkbox==TRUE){
         # 
         #   incProgress(11/20,message = "Adding mulching data...")
         # 
         #   dt_mr <- dt_mures()
         # 
         #   openxlsx::addWorksheet(wb, "Mulching and residue", gridLines = TRUE)
         #   openxlsx::writeDataTable(wb, "Mulching and residue", x = dt_mr,
         #                            colNames = TRUE, withFilter = FALSE)
         # 
         #   }
         # 
         # }
         # #print("error10")
         # if(is.element("Planting and transplanting", agroFeaSelected)) {
         #    print("plant")
         # 
         #   if(input$directSeeding_checkbox==TRUE && input$transplanting_checkbox==TRUE){
         # 
         #   incProgress(12/20,message = "Adding planting and transplanting data...")
         # 
         #   dt_plant <- dt_planting()
         # 
         #   openxlsx::addWorksheet(wb, "Planting and transplanting", gridLines = TRUE)
         #   openxlsx::writeDataTable(wb, "Planting and transplanting", x = dt_plant,
         #                            colNames = TRUE, withFilter = FALSE)
         # 
         #   }
         # 
         # }
         # #print("error11")
         # if(is.element("Weeding", agroFeaSelected)){
         #    print("wed")
         #    incProgress(13/20,message = "Adding Weeding data...")
         # 
         #    dt_weed <- dt_weeding()
         # 
         #    openxlsx::addWorksheet(wb, "Weeding", gridLines = TRUE)
         #    openxlsx::writeDataTable(wb, "Weeding", x = dt_weed,
         #                             colNames = TRUE, withFilter = FALSE)
         # }
         # 
         # if(is.element("Soil fertility", agroFeaSelected)){
         # 
         #   dt_soil<- dt_soilFertility()
         #   openxlsx::addWorksheet(wb, "Soil fertility", gridLines = TRUE)
         #   openxlsx::writeDataTable(wb, "Soil fertility", x = dt_soil,
         #                            colNames = TRUE, withFilter = FALSE)
         # 
         # }


     #
         
         
         
         # openxlsx::addWorksheet(wb, "Weather", gridLines = TRUE)
         # openxlsx::writeDataTable(wb, "Weather", x = weather_dt(),
         #                          colNames = TRUE, withFilter = FALSE)
         
         
         # if(is.null(weather) || length(weather)==0 || nrow(weather)==0  ){
         #   print("there is no weather data")
         # 
         # }
         # else {
         # 
         #   incProgress(8/10,message = "Adding weather variables sheet...")
         # 
         #   openxlsx::addWorksheet(wb, "Weather", gridLines = TRUE)
         #   openxlsx::writeDataTable(wb, "Weather", x = weather,
         #                            colNames = TRUE, withFilter = FALSE)
         # }
         # #print("error13")
         # if(is.null(soil_vars) || length(soil_vars)==0 || nrow(soil_vars)==0 ){
         #   print("there is no soil data")
         # 
         # }
         # else{
         #       incProgress(9/10,message = "Adding soil variables sheet...")
         #       openxlsx::addWorksheet(wb, "Soil", gridLines = TRUE)
         #       openxlsx::writeDataTable(wb, "Soil", x = soil_vars,
         #                                    colNames = TRUE, withFilter = FALSE)
         # }
         incProgress(19/20,message = "Downloading file...")
         saveWorkbook(wb, file = fname , overwrite = TRUE)
         file.rename(fname, file)


       })

     },
     contentType="application/xlsx"
   )

}
