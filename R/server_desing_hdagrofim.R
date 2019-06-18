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
  
  ##########################################################################################
  ############################### START SERVER: SAVE SESSION ###############################
  
  # observeEvent(input$load_inputs, {
  #   n <- 5
  #   for (i in 1:n) {
  #     Sys.sleep(0.1)
  #     shinyjs::click("addFundingAgency")
  #   }
  # })
  
  observeEvent(input$savetest4, {
    id_rand_fa <- getAddInputId(experimentVars$ids_FA, "FA_", "")
    #print(id_rand_fa)
    
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
    #print(z)
    
    bb <- AllInputs() %>% dplyr::filter(id %in% z$id)
    #print(bb)
    
    # c <- rbind(a, bb)
    # print(c)
    
    resall <- arrange_by_pattern(bb, id_rand_fa)
    #print(resall)
    
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
    #print(id_rand_fa)
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
  globalpath <- "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/www/db_save_session/" # base de datos de los Ids
  
  # path del template para new fieldbook
  templatepath <- "/home/obenites/AGROFIMS/template/"
  #################### END: PATHS GENERALES ####################
  
  #################### START: GUARDAR SESION DEL FIELDBOOK ####################
  
  # GLOBAL PATH donde se aloja las sessiones y backups
  sessionpath <- "/home/obenites/AGROFIMS/savesession/"
  sessionpathbk <- "/home/obenites/AGROFIMS/savesession_bk/"
  
  sessionVals <- reactiveValues()
  sessionVals$aux <- data.frame()
  
  #Funcion que crea lista de inputs a guardar: Experiment
  inputsExperiment <- function() {
    a1 <- a2 <- a3 <- a4 <- a5 <- a6 <- a7 <- a8 <- a9 <- a10 <- a11 <- a12 <- c()
    b1 <- b2 <- b3 <- b4 <- b5 <- b6 <- b7 <- b8 <- b9 <- b10 <- b11 <- b12 <- c()
    c1 <- c2 <- c3 <- c4 <- c5 <- c6 <- c7 <- c8 <- c9 <- c10 <- c11 <- c12 <- c13 <- c14 <- c15 <- c16 <- c17 <- c18 <- c19 <- c20 <- c21 <- c22 <- c23 <- c24 <- c()
    
    
    
    inputRds <- readRDS(paste0(globalpath, "inputId1_v3.rds"))
    
    inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Experiment")
  
    df1 <- inputRds[c(4, 5, 6,7)]
    
    # inputs para: Funding Agency
    id_rand_fa <- getAddInputId(experimentVars$ids_FA, "FA_", "")
    
    #Variables auxiliares
    aux1 <- aux2 <- aux3 <- aux4 <- c()
    for (i in 1:length(id_rand_fa)) {

      aux1[i] <- paste0("designFieldbook_fundAgencyType_", id_rand_fa[i])
      aux2[i] <- paste0("designFieldbook_fundAgencyType_", id_rand_fa[i], "_other")
      aux3[i] <- paste0("designFieldbook_fundAgencyType_name_", id_rand_fa[i])
      aux4[i] <- paste0("designFieldbook_fundAgencyType_cgiar_", id_rand_fa[i])
      
      
      a1[i] <- paste0("designFieldbook_fundAgencyType_", i)
      a2[i] <- paste0("designFieldbook_fundAgencyType_", i, "_other")
      a3[i] <- paste0("designFieldbook_fundAgencyType_name_", i)
      a4[i] <- paste0("designFieldbook_fundAgencyType_cgiar_", i)
      
      
      a1[i] <- paste0("designFieldbook_fundAgencyType_", i)
      a2[i] <- paste0("designFieldbook_fundAgencyType_", i, "_other")
      a3[i] <- paste0("designFieldbook_fundAgencyType_name_", i)
      a4[i] <- paste0("designFieldbook_fundAgencyType_cgiar_", i)
      
      a5[i] <- "selectizeInput"
      a6[i] <- "textInput"
      a7[i] <- "textInput"
      a8[i] <- "selectizeInput"
      
      a9[i] <- "n"
      a10[i] <- "n"
      a11[i] <- "n"
      a12[i] <- "n"
    }
    
    df2 <- data.frame(originalInputId = c(aux1,aux2,aux3,aux4),
                      inputId = c(a1, a2, a3, a4),
                      type = c(a5, a6, a7, a8),
                      create = c(a9, a10, a11, a12),
                      stringsAsFactors = F)
  
    
    # inputs para: Project Management Entities
    id_rand_pe <- getAddInputId(experimentVars$ids_PE, "PE_", "")
    
    #Variables auxiliares
    aux1 <- aux2 <- aux3 <- aux4 <- c()
    for (i in 1:length(id_rand_pe)) {
      
      aux1[i] <- paste0("projEntity_", id_rand_pe[i])
      aux2[i] <- paste0("projEntity_", id_rand_pe[i], "_other")
      aux3[i] <- paste0("contCenter_", id_rand_pe[i])
      aux4[i] <- paste0("contCRP_", id_rand_pe[i])
      
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
    
    df3 <- data.frame(originalInputId = c(aux1,aux2,aux3,aux4),
                      inputId = c(b1, b2, b3, b4),
                      type = c(b5, b6, b7, b8),
                      create = c(b9, b10, b11, b12),
                      stringsAsFactors = F)
    
    # inputs para: Experiment Leads
    id_rand_el <- getAddInputId(experimentVars$ids_EL, "EL_", "")
    
    #Variables auxiliares
    aux1 <- aux2 <- aux3 <- aux4 <- aux5 <- aux6 <- aux7 <- aux8 <- c()
    for (i in 1:length(id_rand_el)) {
      aux1[i] <- paste0("projLeadEnt_", id_rand_el[i])
      aux2[i] <- paste0("projLeadEnt_",id_rand_el[i],"_other")  
      aux3[i] <- paste0("tLeadCenter_", id_rand_el[i])
      aux4[i] <- paste0("lead_org_type_1_", id_rand_el[i])
      aux5[i] <- paste0("lead_org_type_1_", id_rand_el[i], "_other")
      aux6[i] <- paste0("leadNameOther_", id_rand_el[i])
      aux7[i] <- paste0("expLead_", id_rand_el[i])
      aux8[i] <- paste0("tLeadContCRP_", id_rand_el[i])
      
      c1[i] <- paste0("projLeadEnt_", i)
      c2[i] <- paste0("projLeadEnt_",i,"_other")  
      c3[i] <- paste0("tLeadCenter_", i)
      c4[i] <- paste0("lead_org_type_1_", i)
      c5[i] <- paste0("lead_org_type_1_", i, "_other")
      c6[i] <- paste0("leadNameOther_", i)
      c7[i] <- paste0("expLead_", i)
      c8[i] <- paste0("tLeadContCRP_", i)
      
      c9[i] <- "selectizeInput"
      c10[i] <-"textInput"
      c11[i] <- "selectizeInput"
      c12[i] <- "selectizeInput"
      c13[i] <- "textInput"
      c14[i] <- "textInput"
      c15[i] <- "textInput"
      c16[i] <- "selectizeInput"
      
      c17[i] <- "n"
      c18[i] <- "n"
      c19[i] <- "n"
      c20[i] <- "n"
      c21[i] <- "n"
      c22[i] <- "n"
      c23[i] <- "n"
      c24[i] <- "n"
    }
    
    df4 <- data.frame(originalInputId = c(aux1,aux2,aux3,aux4,aux5,aux6,aux7,aux8),
                      inputId = c(c1, c2, c3, c4, c5, c6, c7, c8),
                      type = c(c9, c10, c11, c12, c13, c14, c15, c16),
                      create = c(c17, c18, c19, c20, c21, c22, c23, c24),
                      stringsAsFactors = F)
    
    # Union de todos los resultados
    res <- rbind(df1, df2, df3, df4)
    res
  }
  
  # Funcion que crea lista de inputs a guardar: Personnel
  inputsPersonnel <- function() {
    a1 <- a2 <- a3 <- a4 <- a5 <- a6 <- a7 <- a8 <- a9 <- a10 <- a11 <- a12 <- a13 <- a14 <- a15 <- a16 <- a17 <- a18 <- a19 <- a20 <- a21 <- a22 <- a23 <- a24 <- a25 <- a26 <- a27 <- c()
    
    # inputs para: Personnel
    id_rand_pers <-  getAddInputId(personnelVars$ids_PERS, "PERS_", "") 
    
    #Variables auxiliares
    aux1<-aux2<-aux3<-aux4<-aux5<-aux6<-aux7<-aux8<-aux9 <- c()
    for (i in 1:length(id_rand_pers)) {

      aux1[i] <- paste0("personnel_type_", id_rand_pers[i])
      aux2[i] <- paste0("personnel_type_", id_rand_pers[i], "_other")
      aux3[i] <- paste0("person_firstName_", id_rand_pers[i])
      aux4[i] <- paste0("person_lastName_", id_rand_pers[i])
      aux5[i] <- paste0("person_email_", id_rand_pers[i])
      aux6[i] <- paste0("person_affiliation_", id_rand_pers[i])
      aux7[i] <- paste0("person_center_", id_rand_pers[i])
      aux8[i] <- paste0("person_affiliation_", id_rand_pers[i], "_other")
      aux9[i] <- paste0("person_orcid_", id_rand_pers[i])
      
      a1[i] <- paste0("personnel_type_", i)
      a2[i] <- paste0("personnel_type_", i, "_other")
      a3[i] <- paste0("person_firstName_", i)
      a4[i] <- paste0("person_lastName_", i)
      a5[i] <- paste0("person_email_", i)
      a6[i] <- paste0("person_affiliation_", i)
      a7[i] <- paste0("person_center_", i)
      a8[i] <- paste0("person_affiliation_", i, "_other")
      a9[i] <- paste0("person_orcid_", i)
      
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
    df1 <- data.frame(originalInputId = c(aux1,aux2,aux3,aux4,aux5,aux6,aux7,aux8,aux9),
                      inputId = c(a1, a2, a3, a4, a5, a6, a7, a8, a9),
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
    df1 <- inputRds[c(4, 5, 6, 7)]
    
    res <- df1
    res
  }
  
  # Funcion que crea lista de inputs a guardar: Crop
  inputsCrop <- function() {
    df2 <- df3 <- df4 <- df5 <- data.frame()
    a1 <- a2 <- a3 <- a4 <- a5 <- a6 <- a7 <- a8 <- a9 <- a10 <- a11 <- a12 <- c()
    b1 <- b2 <- b3 <- b4 <- b5 <- b6 <- b7 <- b8 <- b9 <- c()
    c1 <- c2 <- c3 <- c4 <- c5 <- c6 <- c7 <- c8 <- c9 <- c10 <- c11 <- c12 <- c()
    
    inputRds <- readRDS(paste0(globalpath, "inputId1_v3.rds"))
    inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Crop")
    df1 <- dplyr::filter(inputRds, is.na(category))
    df1 <- df1[c(4, 5, 6,7)]
    
    # inputs para: Monocrop
    if (!is.null(input$croppingType) && !is.na(input$croppingType) && input$croppingType == "Monocrop") {
      df2 <- dplyr::filter(inputRds, category == "Monocrop")
      df2 <- df2[c(4, 5, 6,7)]
    }
    


    # inputs para: Intercrop
    if (!is.null(input$croppingType) && !is.na(input$croppingType) && input$croppingType == "Intercrop") {
      
      id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
      aux1 <- aux2<- aux3 <- aux4 <- c()
      
      for (i in 1:length(id_ic_rand)) {
        
        aux1[i] <- paste0("int_cropCommonName_", id_ic_rand[i])
        aux2[i] <- paste0("int_cropCommonName_", id_ic_rand[i], "_other")
        aux3[i] <- paste0("int_cropVarietyName_", id_ic_rand[i])
        aux4[i] <- paste0("intercropValue_row_crop_", id_ic_rand[i])
        
        a1[i] <- paste0("int_cropCommonName_", i)
        a2[i] <- paste0("int_cropCommonName_", i, "_other")
        a3[i] <- paste0("int_cropVarietyName_", i)
        a4[i] <- paste0("intercropValue_row_crop_", i)
        
        a5[i] <- "selectizeInput"
        a6[i] <- "textInput"
        a7[i] <- "selectizeInput"
        a8[i] <- "textInput"

        a9[i] <- "n"
        a10[i] <- "n"
        a11[i] <- "y"
        a12[i] <- "n"
      }
      
      df3 <- data.frame(originalInputId= c(aux1,aux2,aux3,aux4) , inputId = c(a1, a2, a3, a4), type = c(a5, a6, a7,a8), create = c(a9, a10, a11, a12), stringsAsFactors = F)
    }

    # inputs para: Relay crop
    if (!is.null(input$croppingType) && !is.na(input$croppingType) && input$croppingType == "Relay crop") {
      
      id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
      aux1 <- aux2<- aux3 <- c()
      
      for (i in 1:length(id_re_rand)) {
        
        aux1[i] <- paste0("rel_cropCommonName_", id_re_rand[i])
        aux2[i] <- paste0("rel_cropCommonName_", id_re_rand[i], "_other")
        aux3[i] <- paste0("rel_cropVarietyName_", id_re_rand[i])
        
        b1[i] <- paste0("rel_cropCommonName_", i)
        b2[i] <- paste0("rel_cropCommonName_", i, "_other")
        b3[i] <- paste0("rel_cropVarietyName_", i)

        b4[i] <- "selectizeInput"
        b5[i] <- "textInput"
        b6[i] <- "selectizeInput"

        b7[i] <- "n"
        b8[i] <- "n"
        b9[i] <- "y"
      }
      df4 <- data.frame(originalInputId = c(aux1,aux2,aux3), inputId = c(b1, b2, b3), type = c(b4, b5, b6), create = c(b7, b8, b9), stringsAsFactors = F)
    }
    
    # inputs para: Rotation crop
    if (!is.null(input$croppingType) && !is.na(input$croppingType) && input$croppingType == "Rotation") {
      
      id_rot_rand <- getAddInputId(rotationcropVars$ids, "rot_", "")
      aux1 <- aux2<- aux3 <- aux4 <- c()
      
      for (i in 1:length(id_rot_rand)) {
        
        aux1[i] <- paste0("rot_cropCommonName_", id_rot_rand[i])
        aux2[i] <- paste0("rot_cropCommonName_", id_rot_rand[i], "_other")
        aux3[i] <- paste0("rot_cropVarietyName_", id_rot_rand[i])
        aux4[i] <- paste0("rot_orderRotation_", id_rot_rand[i])
        
        c1[i] <- paste0("rot_cropCommonName_", i)
        c2[i] <- paste0("rot_cropCommonName_", i, "_other")
        c3[i] <- paste0("rot_cropVarietyName_", i)
        c4[i] <- paste0("rot_orderRotation_", i)
        
        c5[i] <- "selectizeInput"
        c6[i] <- "textInput"
        c7[i] <- "selectizeInput"
        c8[i] <- "selectizeInput"
        
        c9[i] <- "n"
        c10[i] <- "n"
        c11[i] <- "y"
        c12[i] <- "y"
      }
      df5 <- data.frame(originalInputId = c(aux1,aux2,aux3,aux4), 
                        inputId = c(c1, c2, c3, c4), 
                        type = c(c5, c6, c7, c8), 
                        create = c(c9, c10, c11, c12), stringsAsFactors = F)
      
      View(df5)
    }
    
    res <- rbind(df1, df2, df3, df4, df5)
    res
  }
  
  # Funcion que crea lista de inputs a guardar: Soil
  inputsSoil <- function(){
    df2 <- df3 <- df4 <- df5 <- data.frame()
    a1 <- a2 <- a3 <- a4 <- a5 <- a6 <- a7 <- a8 <- a9 <- c()
    b1 <- b2 <- b3 <- b4 <- b5 <- b6 <- b7 <- b8 <- b9 <- c()
    c1 <- c2 <- c3 <- c4 <- c5 <- c6 <- c7 <- c8 <- c9 <- c()
    
    # inputs para: Soil
    id_rand_soil <-  getAddInputId(soilVars$ids, "soil_", "") 

    #Variables auxiliares
    aux1<-aux2<-aux3<-aux4<-aux5<-aux6<-aux7<-aux8<-aux9 <- c()
    for (i in 1:length(id_rand_soil)) {
      
      aux1[i] <- paste0("soil_mea_", id_rand_soil[i])
      aux2[i] <- paste0("soil_unit_", id_rand_soil[i])
      aux3[i] <- paste0("soil_depth_", id_rand_soil[i])
      aux4[i] <- paste0("soil_depthunit_", id_rand_soil[i])
      aux5[i] <- paste0("soil_per_season_", id_rand_soil[i])
      aux6[i] <- paste0("soil_per_plot_", id_rand_soil[i])
      aux7[i] <- paste0("soil_timing_", id_rand_soil[i])
      aux8[i] <- paste0("soil_timingNumLevels_", id_rand_soil[i])
      
      # numlvls <- input[[paste0("soil_timingNumLevels_", id_rand_pers[i])]]
      # 
      # if(!is.na(numlvls) && numlvls != "" ){
      #   for( j in 1:numlvls){
      #     aux9[j] <- paste0("soil_timingValue_", id_rand_pers[j],"_",j)
      #   }
      # }

      a1[i] <- paste0("soil_mea_", i)
      a2[i] <- paste0("soil_unit_", i)
      a3[i] <- paste0("soil_depth_", i)
      a4[i] <- paste0("soil_depthunit_", i)
      a5[i] <- paste0("soil_per_season_", i)
      a6[i] <- paste0("soil_per_plot_", i)
      a7[i] <- paste0("soil_timing_", i)
      a8[i] <- paste0("soil_timingNumLevels_", i)

      # if(!is.na(numlvls) && numlvls != "" ){
      #   for( j in 1:numlvls){
      #     a9[j] <- paste0("soil_timingValue_", id_rand_pers[i],"_",j)
      #   }
      # }

      b1[i] <- "textInput"
      b2[i] <- "selectizeInput"
      b3[i] <- "selectizeInput"
      b4[i] <- "selectizeInput"
      b5[i] <- "textInput"
      b6[i] <- "textInput"
      b7[i] <- "selectizeInput"
      b8[i] <- "selectizeInput"
      
      # if(!is.na(numlvls) && numlvls != "" ){
      #   for( j in 1:numlvls){
      #     if(input[[paste0("soil_timingValue_", id_rand_pers[i])]]=="Date"){
      #       b9[i] <- "dateInput"
      #     }else if (input[[paste0("soil_timingValue_", id_rand_pers[i])]]=="Day after planting" ||
      #               input[[paste0("soil_timingValue_", id_rand_pers[i])]]=="Growth stage")
      #     {
      #       b9[i] <- "selectizeInput"
      #     }
      #   }
      # }


      c1[i] <- "n"
      c2[i] <- "n"
      c3[i] <- "y" 
      c4[i] <- "n"
      c5[i] <- "n"
      c6[i] <- "n"
      c7[i] <- "n"
      c8[i] <- "n"
      #c9[i] <- "n"

    }
    df1 <- data.frame(originalInputId = c(aux1,aux2,aux3,aux4,aux5,aux6,aux7,aux8),
                      inputId = c(a1, a2, a3, a4, a5, a6, a7, a8),
                      type = c(b1, b2, b3, b4, b5, b6, b7, b8),
                      create = c(c1, c2, c3, c4, c5, c6, c7, c8),
                      stringsAsFactors = F)
    
    res <- df1
    res
  }
  
  # Funcion que crea lista de inputs a guardar de weather
  inputsWeather <- function(){
    df2 <- df3 <- df4 <- df5 <- data.frame()
    a1 <- a2 <- a3 <- a4 <- a5 <- a6 <- a7 <- a8 <- a9 <- c()
    b1 <- b2 <- b3 <- b4 <- b5 <- b6 <- b7 <- b8 <- b9 <- c()
    c1 <- c2 <- c3 <- c4 <- c5 <- c6 <- c7 <- c8 <- c9 <- c()
    
    # inputs para: Soil
    id_rand_weather<-  getAddInputId(weatherVars$ids, "weather_", "") 
    
    #Variables auxiliares
    aux1<-aux2<-aux3<-aux4<-aux5<- c()
    for (i in 1:length(id_rand_weather)) {
      
      aux1[i] <- paste0("weather_mea_", id_rand_weather[i])
      aux2[i] <- paste0("weather_unit_", id_rand_weather[i])
      aux3[i] <- paste0("weather_per_season_", id_rand_weather[i])
      aux4[i] <- paste0("weather_timing_", id_rand_weather[i])
      aux5[i] <- paste0("weather_timingNumLevels_", id_rand_weather[i])
      
      # numlvls <- input[[paste0("soil_timingNumLevels_", id_rand_pers[i])]]
      # 
      # if(!is.na(numlvls) && numlvls != "" ){
      #   for( j in 1:numlvls){
      #     aux9[j] <- paste0("soil_timingValue_", id_rand_pers[j],"_",j)
      #   }
      # }
      
      a1[i] <- paste0("weather_mea_", i)
      a2[i] <- paste0("weather_unit_", i)
      a3[i] <- paste0("weather_per_season_", i)
      a4[i] <- paste0("weather_timing_", i)
      a5[i] <- paste0("weather_timingNumLevels_", i)

      # if(!is.na(numlvls) && numlvls != "" ){
      #   for( j in 1:numlvls){
      #     a9[j] <- paste0("soil_timingValue_", id_rand_pers[i],"_",j)
      #   }
      # }
      
      b1[i] <- "textInput"
      b2[i] <- "selectizeInput"
      b3[i] <- "textInput"
      b4[i] <- "selectizeInput"
      b5[i] <- "selectizeInput"

      
      # if(!is.na(numlvls) && numlvls != "" ){
      #   for( j in 1:numlvls){
      #     if(input[[paste0("soil_timingValue_", id_rand_pers[i])]]=="Date"){
      #       b9[i] <- "dateInput"
      #     }else if (input[[paste0("soil_timingValue_", id_rand_pers[i])]]=="Day after planting" ||
      #               input[[paste0("soil_timingValue_", id_rand_pers[i])]]=="Growth stage")
      #     {
      #       b9[i] <- "selectizeInput"
      #     }
      #   }
      # }
      
      
      c1[i] <- "n"
      c2[i] <- "n"
      c3[i] <- "n" 
      c4[i] <- "n"
      c5[i] <- "n"
      #c9[i] <- "n"
      
    }
    df1 <- data.frame(originalInputId = c(aux1,aux2,aux3,aux4,aux5),
                      inputId = c(a1, a2, a3, a4, a5),
                      type = c(b1, b2, b3, b4, b5),
                      create = c(c1, c2, c3, c4, c5),
                      stringsAsFactors = F)
    
    res <- df1
    res
  }
  
  # Funcion que crea lista de inputs a guardar de Crop Measurement
  inputsCropMeasurement <- function(){
    df2 <- df3 <- df4 <- df5 <- data.frame()
    a1 <- a2 <- a3 <- a4 <- a5 <- a6 <- a7 <- a8 <- a9 <- c()
    b1 <- b2 <- b3 <- b4 <- b5 <- b6 <- b7 <- b8 <- b9 <- c()
    c1 <- c2 <- c3 <- c4 <- c5 <- c6 <- c7 <- c8 <- c9 <- c()
    
    prefix <- NULL
    numCropVars <- NULL
    id_rand_cropMeasurement <- NULL
    
    id_int_cropMeasurement_1 <- NULL 
    id_int_cropMeasurement_2 <- NULL
    id_int_cropMeasurement_3 <- NULL
    id_int_cropMeasurement_4 <- NULL
    id_int_cropMeasurement_5 <- NULL
    
    if(input$croppingType == "Monocrop"){
      prefix <- "mono"
      id_rand_cropMeasurement <-  getAddInputId(meaMONO$ids, "mono_mea_1_fluidRow_", "")
    }else if(input$croppingType == "Intercrop"){
      prefix <- "int"
      numCropVars <- length(intercropVars$ids)
      
      for (i in 1:numCropVars){
        id_int_cropMeasurement_1 <- getAddInputId(meaINT[i]$ids, paste0("int_mea_",i,"_fluidRow_", ""))
        
        print(id_int_cropMeasurement_1)
      }
      
    }else if (input$croppingType == "Relay crop"){
      prefix <- "rel"
      numCropVars <- length(relaycropVars$ids)
      id_rand_cropMeasurement <-  getAddInputId(meaMONO$ids, "mono_", "")
    }else if (input$croppingType == "Rotation"){
      prefix <- "rot"
      numCropVars <- length(rotcropVars$ids)
      id_rand_cropMeasurement <-  getAddInputId(meaMONO$ids, "mono_", "") 
    }
      
    #Variables auxiliares
    aux1<-aux2<-aux3<-aux4<-aux5<-aux6<- c()
    for (i in 1:length(id_rand_cropMeasurement)) {
      
      aux1[i] <- paste0("mono_mea_1_measurement_", id_rand_cropMeasurement[i])
      aux2[i] <- paste0("mono_mea_1_parmea_", id_rand_cropMeasurement[i])
      aux3[i] <- paste0("mono_mea_1_unit_", id_rand_cropMeasurement[i])
      aux4[i] <- paste0("mono_mea_1_per_season_", id_rand_cropMeasurement[i])
      aux5[i] <- paste0("mono_mea_1_per_plot_", id_rand_cropMeasurement[i])
      aux6[i] <- paste0("mono_mea_1_timing_", id_rand_cropMeasurement[i])
      
      # numlvls <- input[[paste0("soil_timingNumLevels_", id_rand_pers[i])]]
      # 
      # if(!is.na(numlvls) && numlvls != "" ){
      #   for( j in 1:numlvls){
      #     aux9[j] <- paste0("soil_timingValue_", id_rand_pers[j],"_",j)
      #   }
      # }
      
      a1[i] <- paste0("mono_mea_1_measurement_", i)
      a2[i] <- paste0("mono_mea_1_parmea_", i)
      a3[i] <- paste0("mono_mea_1_unit_", i)
      a4[i] <- paste0("mono_mea_1_per_season_", i)
      a5[i] <- paste0("mono_mea_1_per_plot_", i)
      a6[i] <- paste0("mono_mea_1_timing_", i)
      
      # if(!is.na(numlvls) && numlvls != "" ){
      #   for( j in 1:numlvls){
      #     a9[j] <- paste0("soil_timingValue_", id_rand_pers[i],"_",j)
      #   }
      # }
      
      b1[i] <- "textInput"
      b2[i] <- "selectizeInput"
      b3[i] <- "selectizeInput"
      b4[i] <- "textInput"
      b5[i] <- "textInput"
      b6[i] <- "selectizeInput"
      
      
      # if(!is.na(numlvls) && numlvls != "" ){
      #   for( j in 1:numlvls){
      #     if(input[[paste0("soil_timingValue_", id_rand_pers[i])]]=="Date"){
      #       b9[i] <- "dateInput"
      #     }else if (input[[paste0("soil_timingValue_", id_rand_pers[i])]]=="Day after planting" ||
      #               input[[paste0("soil_timingValue_", id_rand_pers[i])]]=="Growth stage")
      #     {
      #       b9[i] <- "selectizeInput"
      #     }
      #   }
      # }
      
      
      c1[i] <- "n"
      c2[i] <- "n"
      c3[i] <- "n" 
      c4[i] <- "n"
      c5[i] <- "n"
      c6[i] <- "n"
      
    }
    df1 <- data.frame(originalInputId = c(aux1,aux2,aux3,aux4,aux5,aux6),
                      inputId = c(a1, a2, a3, a4, a5, a6),
                      type = c(b1, b2, b3, b4, b5, b6),
                      create = c(c1, c2, c3, c4, c5, c6),
                      stringsAsFactors = F)
    
    res <- df1
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
      
      inputs_to_save <- rbind(inputsExperiment(),
                              inputsPersonnel(),
                              inputsSite(),
                              inputsCrop(),
                              inputsSoil(),
                              inputsWeather(),
                              inputsCropMeasurement())
      #inputsDesign(),
      #inputsExpCon())
      
      View(inputs_to_save)
      
      case1p <- dplyr::filter(inputs_to_save, type == "textInput" |
                                type == "numericInput" |
                                type == "textAreaInput" |
                                type == "checkboxInput" |
                                type == "dateInput")
      
      
      case1InputAux <- case1p[[1]]
      case1 <- case1p[[2]]
      case1_type <- case1p[[3]]
      case1_create <- case1p[[4]]
      
      case2p <- dplyr::filter(inputs_to_save, type == "dateRangeInput")
      case2 <- case2p[[1]]
      case2_type <- case2p[[2]]
      case2_create <- case2p[[3]]
      
      case3p <- dplyr::filter(inputs_to_save, type == "selectizeInput" | type == "selectInput")
      
      case3InputAux <- case3p[[1]]
      case3 <- case3p[[2]]
      case3_type <- case3p[[3]]
      case3_create <- case3p[[4]]
      
      for (i in 1:length(case1)) {
        # Fill values for textInput && numericInput && textAreaInput && checkboxInput && dateinput
        if (is.null(input[[paste0(case1InputAux[i])]]) || is.na(input[[paste0(case1InputAux[i])]])) {
          inputs1[i] <- ""
        } else {
          inputs1[i] <- as.character(input[[paste0(case1InputAux[i])]])
        }
      }
      
      #Crea el dataframe con los valores capturados para text, numeric, textArea, checkBox y date Input
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
        #  Fill values for selectizeInput && selectInput
        if (is.null(input[[paste0(case3InputAux[i])]]) || is.na( input[[paste0(case3InputAux[i])]])) {
          inputs3[i] <- ""
        } else {
          inputs3[i] <- paste(input[[paste0(case3InputAux[i])]], collapse = "&")
        }
      }
      #Crea el dataframe con los valores capturados para selectize y select Input
      inputs_data_frame3 <- data.frame(inputId = case3, type = case3_type, create = case3_create, value = inputs3)
      
      
      #Eliminar sesion
      # print("no sale")
      # print(paste(input[["fbDesign_inSiteVegetation_other"]],collapse="$"))
      # 
      # print("correcto")
      # print(paste(input[["fbDesign_inSiteVegetation"]],collapse="$"))
      
      
      #inputs_data_frame <- rbind(inputs_data_frame1, inputs_data_frame2, inputs_data_frame3)
      
      #Une los dataframe y los consolida en uno.
      inputs_data_frame <- rbind(inputs_data_frame1, inputs_data_frame3)
      
      #Agrega 3 filas adicionales con información sobre log (usuario,creacion,modificacion)
      nr <- data.frame(inputId = "user", type = "", create = "", value = session$userData$userMail)
      nr2 <- data.frame(inputId = "datec", type = "", create = "", value = datecreate)
      nr3 <- data.frame(inputId = "datem", type = "", create = "", value = datemodified)
      

      #Agregamos información de inputs dinamicos
      expRow   <- experimentRowsSaveSession()
      persRow  <- personnelRowsSaveSession()
      
      cropIC   <- cropICRowsSaveSession()
      cropREL  <- cropRELRowsSaveSession()
      cropROT  <- cropROTRowsSaveSession()
      
      soilRow  <- soilRowsSaveSession()
      weatherRow <- weatherRowsSaveSession()
      
      #Unimos todos los dataframe en uno solo
      final_inputs_df <- rbind(nr, nr2, nr3, inputs_data_frame, 
                              expRow, persRow, cropIC, cropREL, cropROT,soilRow, weatherRow)
      
      View(final_inputs_df)

      #Almacena archivos en 2 csv's
      #write.csv(final_inputs_df, paste0(sessionpath, input$uniqueId, ".csv"), row.names = FALSE)
      #write.csv(final_inputs_df, paste0(sessionpathbk, input$uniqueId, ".csv"), row.names = FALSE)
      
      updateTextInput(session,
                      inputId = "uniqueId",
                      value = "")
      updateTextInput(session,
                      inputId = "uniqueId",
                      value = expid)
      
      shinyalert("Saved successfully", type = "success", timer = 1500, showConfirmButton = F)
    } else {
      shinyalert("Sorry", "You must login to save advance", type = "info", timer = 1500, showConfirmButton = F)
    }
  }
  
  # Experiment Rows SaveSession
  experimentRowsSaveSession <- function(){
    #Funding Agency
    nrowValFA <- as.character(length(experimentVars$ids_FA))
    nrowFundingAgency <- data.frame(inputId = "nrowFundingAgency", type = "", create = "", value = nrowValFA)
    
    #Agregamos información de inputs dinamicos
    #Funding Agency
    nrowValPE <- as.character(length(experimentVars$ids_PE))
    nrowProjectEntities <- data.frame(inputId = "nrowProjectEntities", type = "", create = "", value = nrowValPE)
    
    #Agregamos información de inputs dinamicos
    #Experiment Leads 
    nrowValEL <- as.character(length(experimentVars$ids_EL))
    nrowExperimentLeads <- data.frame(inputId = "nrowExperimentLeads", type = "", create = "", value = nrowValEL)
    
    return (rbind(nrowFundingAgency,nrowProjectEntities,nrowExperimentLeads))
  }
  
  # Personnel Rows SaveSession
  personnelRowsSaveSession <- function(){
    nrowValPERS <- as.character(length(personnelVars$ids_PERS))
    nrowPersonnel <- data.frame(inputId = "nrowPersonnel", type = "", create = "", value = nrowValPERS)
    
    return (rbind(nrowPersonnel))
  }
  
  # Crop Intercrop Rows SaveSession
  cropICRowsSaveSession <- function(){
    nrowCropIC <- as.character(length(intercropVars$ids))
    nrowCropIC <- data.frame(inputId = "nrowCropIC", type = "", create = "", value = nrowCropIC)
  }
  
  # Crop Relay Rows SaveSession
  cropRELRowsSaveSession <- function(){
    nrowCropREL <- as.character(length(relaycropVars$ids))
    nrowCropREL <- data.frame(inputId = "nrowCropREL", type = "", create = "", value = nrowCropREL)
  }
  
  #Crop Rotation Rows SaveSession
  cropROTRowsSaveSession <- function(){
    nrowCropROT <- as.character(length(rotationcropVars$ids))
    nrowCropROT <- data.frame(inputId = "nrowCropROT", type = "", create = "", value = nrowCropROT)
  }
  
  #Soil Rows SaveSession
  soilRowsSaveSession <- function(){
    nrowSOIL <- as.character(length(soilVars$ids))
    nrowSOIL <- data.frame(inputId = "nrowSOIL", type = "", create = "", value = nrowSOIL)
  }
  
  #Weather Rows SaveSession
  weatherRowsSaveSession  <- function(){
    nrowWEA <- as.character(length(weatherVars$ids))
    nrowWEA <- data.frame(inputId = "nrowWEA", type = "", create = "", value = nrowWEA)
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
  
  
  
  ###########################################################################################
  ################################ START: LOAD SESSION JOSE #################################
  
  ##### Start Modulo: Render session list in DT #####
  output$dtsession <- DT::renderDataTable({
    DT::datatable(
      sessionVals$aux, 
      #iris,
      #refreshDT(),
      selection = 'single',
      options = list(
        pageLength = 5#,
        #columnDefs = list(list(visible=FALSE, targets=c(1, 7)))
        #list(width = '30%', targets = c(1)),
        #list(className = 'dt-center', targets = c(7,8))
      )
    )
  })
  ##### End Modulo: Render session list in DT ######
  
  ##### Start Modulo: Load fieldbook #####

  
  my_files <- function() {
    lf <- list.files(sessionpath)
    lf

  }
  
  
  #Evento reactivo que captura id de la fila seleccionada 
  selectedRow <- eventReactive(input$load_inputNew1, {
    id <- input$dtsession_rows_selected
    sessionVals$aux[id, 1]
  })
  
  
  loadsession2 <- function() {
    

    if (length(selectedRow() != 0)) {
      if (file.exists(isolate(paste0(sessionpath, selectedRow(), ".csv")))){
        uploaded_inputs <- read.csv(paste0(sessionpath, selectedRow(), ".csv"))
        uploaded_inputs <- uploaded_inputs[order(uploaded_inputs$inputId),]
        
        View(uploaded_inputs)
        #soil_df <- as.data.frame(uploaded_inputs) %>% filter(inputId %like% "soil")
        #View(soil_df)
        
        #uploaded_inputs <- uploaded_inputs #%>% filter(!str_detect(inputId,"soil"))
          #%>% filter(!str_detect(id, "-selectized")) %>%
        
        #View(as.data.frame(uploaded_inputs))
        
        #Funding Agency
        nrowFundingAgency <- as.data.frame(uploaded_inputs) %>% filter(inputId == "nrowFundingAgency") %>% select_("value")
        nrowFundingAgency <- as.numeric(as.character(nrowFundingAgency[[1]]))
        
        if(length(nrowFundingAgency)>0 && nrowFundingAgency>=2){
          for(i in 2:nrowFundingAgency){
            insertBoxFundingAgency(i)
          }
        }
        
        #Project Managment Entities
        nrowProjectEntities <- as.data.frame(uploaded_inputs) %>% filter(inputId == "nrowProjectEntities") %>% select_("value")
        nrowProjectEntities <- as.numeric(as.character(nrowProjectEntities[[1]]))
        if(length(nrowProjectEntities)>0 && nrowProjectEntities>=2){
          for(i in 2:nrowProjectEntities){
            insertBoxManagEntity(i)
          }
        }
        
        #Experiment Leads
        nrowExperimentLeads <- as.data.frame(uploaded_inputs) %>% filter(inputId == "nrowExperimentLeads") %>% select_("value")
        nrowExperimentLeads <- as.numeric(as.character(nrowExperimentLeads[[1]]))
        if(length(nrowExperimentLeads)>0 && nrowExperimentLeads>=2){
          for(i in 2:nrowExperimentLeads){
            insertBoxExperimentLead(i)
          }
        }
        
        #Personnel
        nrowPersonnel <- as.data.frame(uploaded_inputs) %>% filter(inputId == "nrowPersonnel") %>% select_("value")
        nrowPersonnel <- as.numeric(as.character(nrowPersonnel[[1]]))

        if(length(nrowPersonnel)>0 && nrowPersonnel>=2){
          for(i in 2:nrowPersonnel){
            insertBoxPersonnel(i)
          }
        }
        
        #Crop
        croppingType <- as.data.frame(uploaded_inputs) %>% filter(inputId == "croppingType") %>% select_("value")
        croppingType <- as.character(croppingType[[1]])
        
        if(croppingType != "Monocrop" && length(croppingType)>0){
          updateSelectizeInput(session,
                               inputId = "croppingType",
                               selected = croppingType)

          if (croppingType == "Intercrop"){
            nrowCropIC <- as.data.frame(uploaded_inputs) %>% filter(inputId == "nrowCropIC") %>% select_("value")
            nrowCropIC <- as.numeric(as.character(nrowCropIC[[1]]))

            if(nrowCropIC>=3){
              for(i in 3:nrowCropIC){
                insertBoxcrop(i,"int")
              }
            }
          }else if (croppingType == "Relay crop"){
            nrowCropREL <- as.data.frame(uploaded_inputs) %>% filter(inputId == "nrowCropREL") %>% select_("value")
            nrowCropREL <- as.numeric(as.character(nrowCropREL[[1]]))
            
            if(nrowCropREL>=3){
              for(i in 3:nrowCropREL){
                insertBoxcrop(i,"rel")
              }
            }
           }else if (croppingType == "Rotation"){
              nrowCropROT <- as.data.frame(uploaded_inputs) %>% filter(inputId == "nrowCropROT") %>% select_("value")
              nrowCropROT <- as.numeric(as.character(nrowCropROT[[1]]))

  
              if(nrowCropROT>=3){
                for(i in 3:nrowCropROT){
                  insertBoxcrop(i,"rot")
                }
              }
          }
        }
        
        #Soil 
        nrowSoil <- as.data.frame(uploaded_inputs) %>% filter(inputId == "nrowSOIL") %>% select_("value")
        nrowSoil <- as.numeric(as.character(nrowSoil[[1]]))
        
        timing<- get_dcm_values(cmdt, "Timing","")
        

        if(length(nrowSoil)>0 && nrowSoil>=1){
          for(i in 1:nrowSoil){
            insertRow_SOIL(i,timing,"","")
          }
        }

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
          500,
          lapply(1:nrow(uploaded_inputs),function(i){
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
                  
              delay(i*60,updateSelectizeInput(session,
                                       inputId = uploaded_inputs$inputId[i],
                                       selected = getInputs(uploaded_inputs[i, 4], "")))
              
              delay(i*60,Sys.sleep(0.5))

              }
              
            
            if (type == "selectizeInput" && create == "y") {
              
              delay(i*60,updateSelectizeInput(session,
                                   inputId = uploaded_inputs$inputId[i],
                                   selected = getInputs(uploaded_inputs[i, 4], ""),
                                   choices = getInputs(uploaded_inputs[i, 4], ""),
                                   options = list('create' = TRUE)))
              
              delay(i*60,Sys.sleep(0.5))

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
        )
        
        delay(500,Sys.sleep(0.5))
        #output$text2 <- renderText({"Loaded successfully"})
        shinyalert("Loaded successfully", type = "success", timer = 1500, showConfirmButton = F)
      }
      else{
        #output$text <- renderText({"The session file does not exist"})
        shinyalert("Oops!", "The session file does not exist", type = "error", timer = 1500, showConfirmButton = F)
      }
    }
    
    
    
  }
  
  
  # Datatable perteniciente a la vista Manage Fieldbooks
  refreshDT <- function() {
    df <- data.frame()
    a <- b <- c <- d <- e <- f <- g <- c()
    
    if (length(my_files()) >= 1) {
      
      for (i in 1:length(my_files())) {
        # Unique ID
        mf <- my_files()[i]
        mf <- unlist(strsplit(mf, "[.]"))
        a[i] <- mf[1]
        
        # Experiment ID
        fl <- read.csv(paste0(sessionpath, my_files()[i]))
        b[i] <- as.character(fl[5, 4])
        
        # Experiment name
        fl <- read.csv(paste0(sessionpath, my_files()[i]))
        c[i] <- as.character(fl[6, 4])
        
        # Experiment project name
        d[i] <- as.character(fl[7, 4])
        
        # Date created 
        e[i] <- as.character(fl[2, 4])
        
        # Date modified
        #e[i] <- as.character(file.info(paste0(sessionpath, my_files()[i]))$mtime)
        f[i] <- as.character(fl[3, 4])
        
        # User
        g[i] <- as.character(fl[1, 4])
      }
      
      userM <- session$userData$userMail
      
      df <- data.frame(a, b, c, d, e, f, g, stringsAsFactors = F)
      
      
      
      df <- dplyr::filter(as.data.frame(df), g == userM)
      df <- df %>% dplyr::arrange(desc(f))
      #print(df)
      sessionVals$aux <- data.frame(df)
      colnames(sessionVals$aux) <- c("ID", "Experiment ID", "Experiment name", "Experiment project name", "Date created", "Date modified", "User")
      #colnames(df) <- c("ID", "Experiment ID", "Experiment name", "Experiment project name", "Date created", "Date modified", "User")
      #print(df)
    } else {
      
      sessionVals$aux <- data.frame()
      #df <- data.frame()
    }
  }
  
  
  observeEvent(input$refreshsession1, {
    #print("uno")
    refreshDT()
  })
  
  
  
  #Boton load session
  #Ejemplo
  observeEvent(input$load_inputNew1, {
    loadsession2()

    # for (i in 1:4) {
    #   insertBoxFundingAgency(i)
    # }
    
  })
  
  
  ################################ END: LOAD SESSION JOSE #################################
  ###########################################################################################
  

  
  
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
  
  
  # ##### Start Modulo: Render session list in DT #####
  # output$dtsession <- DT::renderDataTable({
  #   DT::datatable(
  #     sessionVals$aux, 
  #     #iris,
  #     #refreshDT(),
  #     selection = 'single',
  #     options = list(
  #       pageLength = 5#,
  #       #columnDefs = list(list(visible=FALSE, targets=c(1, 7)))
  #       #list(width = '30%', targets = c(1)),
  #       #list(className = 'dt-center', targets = c(7,8))
  #     )
  #   )
  # })
  # ##### End Modulo: Render session list in DT ######
  # 
  # ##### Start Modulo: Load fieldbook #####
  # # Obtiene el id del row del DT
  # selectedRow <- eventReactive(input$load_inputs, {
  #   id <- input$dtsession_rows_selected
  #   sessionVals$aux[id, 1]
  # })
  # 
  # 
  # my_files <- function() {
  #   lf <- list.files(sessionpath)
  #   lf
  #   print(lf)
  #   
  # }
  # 
  # 
  # #Evento reactivo que captura id de la fila seleccionada 
  # selectedRow <- eventReactive(input$load_inputNew1, {
  #   id <- input$dtsession_rows_selected
  #   sessionVals$aux[id, 1]
  # })
  # 
  # 
  # loadsession2 <- function() {
  #   
  #   print("Entro 1")
  #   if (length(selectedRow() != 0)) {
  #     print("Entro 2")
  #     if (file.exists(isolate(paste0(sessionpath, selectedRow(), ".csv")))){
  #       uploaded_inputs <- read.csv(paste0(sessionpath, selectedRow(), ".csv"))
  #       #print(uploaded_inputs)
  #       
  #       
  #       for(i in 1:nrow(uploaded_inputs)) {
  #         type <- as.character(uploaded_inputs[i, 2])
  #         create <- as.character(uploaded_inputs[i, 3])
  #         
  #         if (type == "textInput") {
  #           updateTextInput(session,
  #                           inputId = uploaded_inputs$inputId[i],
  #                           value = uploaded_inputs$value[i])
  #         }
  #         
  #         if (type == "dateRangeInput") {
  #           if (uploaded_inputs[i, 4] != "") {
  #             v <- getInputs(uploaded_inputs[i, 4], "")
  #             x <- as.Date(v[1]) + 1
  #             y <- as.Date(v[2]) + 1
  #             updateAirDateInput(session,
  #                                inputId = uploaded_inputs$inputId[i],
  #                                value = c(x, y),
  #                                clear = T)
  #           } else {
  #             updateAirDateInput(session,
  #                                inputId = uploaded_inputs$inputId[i],
  #                                clear = T)
  #           }
  #         }
  #         
  #         if (type == "selectizeInput" && create == "n") {
  #           updateSelectizeInput(session,
  #                                inputId = uploaded_inputs$inputId[i],
  #                                selected = getInputs(uploaded_inputs[i, 4], ""))
  #           
  #           ##### LLamar a metodo que dibuja Funding agency type
  #           
  #           
  #           
  #         }
  #         
  #         if (type == "selectizeInput" && create == "y") {
  #           updateSelectizeInput(session,
  #                                inputId = uploaded_inputs$inputId[i],
  #                                selected = getInputs(uploaded_inputs[i, 4], ""),
  #                                choices = getInputs(uploaded_inputs[i, 4], ""),
  #                                options = list('create' = TRUE))
  #         }
  #         
  #         if (type == "textAreaInput") {
  #           updateTextAreaInput(session,
  #                               inputId = uploaded_inputs$inputId[i],
  #                               value = uploaded_inputs$value[i])
  #         }
  #         
  #         if (type == "numericInput") {
  #           updateNumericInput(session,
  #                              inputId = uploaded_inputs$inputId[i],
  #                              value = uploaded_inputs$value[i])
  #         }
  #         
  #         if (type == "checkboxInput") {
  #           if (uploaded_inputs$value[i] == "FALSE") {
  #             x <- FALSE
  #           } else {
  #             x <- TRUE
  #           }
  #           
  #           updateCheckboxInput(session,
  #                               inputId = uploaded_inputs$inputId[i],
  #                               value = x)
  #         }
  #         
  #         if (type == "dateInput") {
  #           if (uploaded_inputs[i, 4] != "") {
  #             v <- getInputs(uploaded_inputs[i, 4], "")
  #             v <- as.Date(v) + 1
  #             updateAirDateInput(session,
  #                                inputId = uploaded_inputs$inputId[i],
  #                                value = v,
  #                                clear = T)
  #           } else {
  #             updateAirDateInput(session,
  #                                inputId = uploaded_inputs$inputId[i],
  #                                clear = T)
  #           }
  #         }
  #       }
  #       
  #       delay(
  #         1500,
  #         for(i in 1:nrow(uploaded_inputs)) {
  #           type <- as.character(uploaded_inputs[i, 2])
  #           create <- as.character(uploaded_inputs[i, 3])
  #           
  #           if (type == "textInput") {
  #             updateTextInput(session,
  #                             inputId = uploaded_inputs$inputId[i],
  #                             value = uploaded_inputs$value[i])
  #           }
  #           
  #           if (type == "dateRangeInput") {
  #             if (uploaded_inputs[i, 4] != "") {
  #               v <- getInputs(uploaded_inputs[i, 4], "")
  #               x <- as.Date(v[1]) + 1
  #               y <- as.Date(v[2]) + 1
  #               updateAirDateInput(session,
  #                                  inputId = uploaded_inputs$inputId[i],
  #                                  value = c(x, y),
  #                                  clear = T)
  #             } else {
  #               updateAirDateInput(session,
  #                                  inputId = uploaded_inputs$inputId[i],
  #                                  clear = T)
  #             }
  #           }
  #           
  #           if (type == "selectizeInput" && create == "n") {
  #             updateSelectizeInput(session,
  #                                  inputId = uploaded_inputs$inputId[i],
  #                                  selected = getInputs(uploaded_inputs[i, 4], ""))
  #           }
  #           
  #           if (type == "selectizeInput" && create == "y") {
  #             updateSelectizeInput(session,
  #                                  inputId = uploaded_inputs$inputId[i],
  #                                  selected = getInputs(uploaded_inputs[i, 4], ""),
  #                                  choices = getInputs(uploaded_inputs[i, 4], ""),
  #                                  options = list('create' = TRUE))
  #           }
  #           
  #           if (type == "textAreaInput") {
  #             updateTextAreaInput(session,
  #                                 inputId = uploaded_inputs$inputId[i],
  #                                 value = uploaded_inputs$value[i])
  #           }
  #           
  #           if (type == "numericInput") {
  #             updateNumericInput(session,
  #                                inputId = uploaded_inputs$inputId[i],
  #                                value = uploaded_inputs$value[i])
  #           }
  #           
  #           if (type == "checkboxInput") {
  #             if (uploaded_inputs$value[i] == "FALSE") {
  #               x <- FALSE
  #             } else {
  #               x <- TRUE
  #             }
  #             
  #             updateCheckboxInput(session,
  #                                 inputId = uploaded_inputs$inputId[i],
  #                                 value = x)
  #           }
  #           
  #           if (type == "dateInput") {
  #             if (uploaded_inputs[i, 4] != "") {
  #               v <- getInputs(uploaded_inputs[i, 4], "")
  #               v <- as.Date(v) + 1
  #               updateAirDateInput(session,
  #                                  inputId = uploaded_inputs$inputId[i],
  #                                  value = v,
  #                                  clear = T)
  #             } else {
  #               updateAirDateInput(session,
  #                                  inputId = uploaded_inputs$inputId[i],
  #                                  clear = T)
  #             }
  #           }
  #         }
  #       )
  #       
  #       #output$text2 <- renderText({"Loaded successfully"})
  #       shinyalert("Loaded successfully", type = "success", timer = 1500, showConfirmButton = F)
  #     }
  #     else{
  #       #output$text <- renderText({"The session file does not exist"})
  #       shinyalert("Oops!", "The session file does not exist", type = "error", timer = 1500, showConfirmButton = F)
  #     }
  #   }
  # }
  # 
  # 
  # # Datatable perteniciente a la vista Manage Fieldbooks
  # refreshDT <- function() {
  #   df <- data.frame()
  #   a <- b <- c <- d <- e <- f <- g <- c()
  #   
  #   if (length(my_files()) >= 1) {
  # 
  #     for (i in 1:length(my_files())) {
  #       # Unique ID
  #       mf <- my_files()[i]
  #       mf <- unlist(strsplit(mf, "[.]"))
  #       a[i] <- mf[1]
  #       
  #       # Experiment ID
  #       fl <- read.csv(paste0(sessionpath, my_files()[i]))
  #       b[i] <- as.character(fl[5, 4])
  #       
  #       # Experiment name
  #       fl <- read.csv(paste0(sessionpath, my_files()[i]))
  #       c[i] <- as.character(fl[6, 4])
  #       
  #       # Experiment project name
  #       d[i] <- as.character(fl[7, 4])
  #       
  #       # Date created 
  #       e[i] <- as.character(fl[2, 4])
  #       
  #       # Date modified
  #       #e[i] <- as.character(file.info(paste0(sessionpath, my_files()[i]))$mtime)
  #       f[i] <- as.character(fl[3, 4])
  #       
  #       # User
  #       g[i] <- as.character(fl[1, 4])
  #     }
  #     
  #     userM <- session$userData$userMail
  #     
  #     df <- data.frame(a, b, c, d, e, f, g, stringsAsFactors = F)
  #     
  # 
  #     
  #     df <- dplyr::filter(as.data.frame(df), g == userM)
  #     df <- df %>% dplyr::arrange(desc(f))
  #     #print(df)
  #     sessionVals$aux <- data.frame(df)
  #     colnames(sessionVals$aux) <- c("ID", "Experiment ID", "Experiment name", "Experiment project name", "Date created", "Date modified", "User")
  #     #colnames(df) <- c("ID", "Experiment ID", "Experiment name", "Experiment project name", "Date created", "Date modified", "User")
  #     #print(df)
  #   } else {
  #     
  #     sessionVals$aux <- data.frame()
  #     #df <- data.frame()
  #   }
  # }
  # 
  # 
  # observeEvent(input$refreshsession1, {
  #   #print("uno")
  #   print("refresh")
  #   refreshDT()
  # })
  # 
  # 
  # 
  # #Boton load session
  # #Ejemplo
  # observeEvent(input$load_inputNew1, {
  #   
  #   print("aca")
  #   loadsession2()
  #   
  #   
  #   
  #   
  #   # for (i in 1:4) {
  #   #   insertBoxFundingAgency(i)
  #   # }
  #   
  #   
  #   # if (session$userData$logged) {
  #   # 
  #   #   removeDin()
  #   # 
  #   #   showModal(modalDialog(
  #   #     title =
  #   #       fluidRow(
  #   #         column(
  #   #           6,
  #   #           style = ("margin-top: -20px;margin-bottom: -10px;"),
  #   #           h3("Load fieldbook")
  #   #         ),
  #   #         column(
  #   #           6,
  #   #           align = "right",
  #   #           style = "margin-top: 0px;"#,
  #   #           #actionLink("btncancel2", "X")
  #   #         )
  #   #       ),
  #   #     fluidRow(
  #   #       column(
  #   #         1,
  #   #         icon("exclamation-triangle", "fa-3x")
  #   #       ),
  #   #       column(
  #   #         11,
  #   #         "Save changes to fieldbook before closing?",
  #   #         br(),
  #   #         "Your changes will be lost if you don’t save them."
  #   #       )
  #   #     ),
  #   #     br(),
  #   #     fluidRow(
  #   #       column(
  #   #         12,
  #   #         align = "center",
  #   #         actionButton('load_inputNew1', 'Next', icon("download"), class = "btn-primary", style="color: #fff;", onclick = "openTab('newFieldbookAgrofims')", width = "100px")#,
  #   #         # actionButton("btnsave", "Save", class = "btn-success", style="color: #fff;", width = "95px"),
  #   #         # actionButton("btndontsave", "Don't save", width = "95px"),
  #   #         # actionButton("btncancel", "Cancel", width = "95px")
  #   #       )
  #   #     ),
  #   #     footer = NULL
  #   #   ))
  #   # } else {
  #   #   shinyalert("Sorry", "You must login to create new fieldbook", type = "info", timer = 1500, showConfirmButton = F)
  #   # }
  # })

  
  
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
    #resetExperiment()
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
  
  ############################### END SERVER: SAVE SESSION ###############################
  ########################################################################################
  
  ############################################################################################
  ############################### START SERVER: SEND FIELDBOOK ###############################
  
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
  
  ############################### END SERVER: SEND FIELDBOOK ###############################
  ##########################################################################################
  
  ########################################################################################
  ############################### START SERVER: EXPERIMENT ###############################
  
  ###################### START SERVER: PRINCIPAL ID ######################
  
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
    } else {
      a <- "NoId"
    }
    a
  }
  
  # Funcion que renderiza en imagen png el codigo qr para fieldbook
  output$myqr <- renderImage({
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
  }, deleteFile = TRUE)
  
  ###################### END SERVER: PRINCIPAL ID ######################
  
  ###################### START: EXPERIMENT DETAILS ######################
  
  # Funcion que genera Experiment ID
  expDetIdgenerator <- function() {
    x <- input$experimentName
    y <- input$experimentProjectName
    
    if (x=="")
      x <- "XX"
    if (y=="")
      y <- "XX"
    
    a <- substring(x, 1, 2)
    b <- substring(y, 1, 2)
    
    #t <- as.numeric(as.POSIXct("2019-02-12 09:31:06 -05"))
    t <- as.integer(as.POSIXct(Sys.time()))
    
    id <- paste(toupper(a), toupper(b), t, sep = "")
    id
  }
  
  # Input: "Experiment ID" Ej. EVLB1549379878 (autogenerado)
  output$experimentIdUI <- renderUI({
    disabled(textInput(inputId = "experimentId", label = "Experiment ID", value = expDetIdgenerator()))
  })
  
  # Funcion que renderiza: "Experiment end date"
  output$exp_end_date <- renderUI({
    if (!is.null(input$fbDesign_project_start_date)) {
      airDatepickerInput("fbDesign_project_end_date",
                         "Experiment end date",
                         clearButton = T,
                         autoClose = T,
                         value = as.Date(input$fbDesign_project_start_date) + 30,
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         placeholder = "yyyy-mm-dd",
                         addon = "none"
      )
    } else {
      airDatepickerInput("fbDesign_project_end_date",
                         "Experiment end date",
                         clearButton = T,
                         autoClose = T,
                         placeholder = "yyyy-mm-dd",
                         addon = "none"
      )
    }
  })
  
  # Input: "Type of experiment"
  output$typeExperiment <- renderUI({
    selectizeInput(
      "designFieldbook_typeExperiment", "Type of experiment", multiple = TRUE,
      options = list(maxItems = 8, placeholder = "Select..."),
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
  
  # Funcion que activa Other y depende de "Type of experiment"
  observeEvent(input$othertE, {
    choices <-  input[[input$othertEid]]
    updateOthertE(choices)
  })
  
  # Funcion que inserta el Other de "Type of experiment"
  updateOthertE <- function(choices) {
    
    if (any(choices == "Other") == T) {
      removeUI(selector = "#othertE", immediate = T)
      if (any(choices != "")) {
        # Other
        insertUI(
          selector = "#othertE_aux",
          where = "beforeBegin",
          ui = fluidRow(
            id = "othertE",
            column(
              12,
              selectizeInput(
                inputId = "designFieldbook_typeExperiment_other", label = "Other", 
                choices = c(), multiple = T,
                options = list('create' = TRUE)
              )
            )
          )
        )
      }
    } else {
      removeUI(selector = "#othertE", immediate = T)
    }
  }
  
  ###################### END: EXPERIMENT DETAILS ######################
  
  # Variable reactiva general
  experimentVars <- reactiveValues()
  
  ###################### START: FUNDING AGENCY ######################
  
  # Funding Agency
  experimentVars$num_FA <- 0
  experimentVars$DEFAULT_FA <- 1
  experimentVars$ids_FA <- c() # get actives fund. agency ids
  
  observeEvent(input$addFundingAgency, {
    defaultBoxes = experimentVars$DEFAULT_FA
    if (experimentVars$num_FA >= 1) {
      insertBoxFundingAgency(experimentVars$num_FA + 1)
    }
  })
  
  observe({
    if (experimentVars$num_FA == 0) {
      default <- experimentVars$DEFAULT_FA
      for (i in 1:default) {
        insertBoxFundingAgency(i)
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
  
  getUiFundingAgency <- function(index) {
    
    
    fluidRow(
      id = paste0("fl_box_fundingAgency_", index), 
      box(
        title = "", solidHeader = TRUE, status = "warning", width=12,
        column(
          12, offset = 0, style='padding:0px; text-align:right;', actionButton(paste0("exp_closeBox_FA_", index), "", shiny::icon("close"))
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
  
  ###################### END: FUNDING AGENCY ######################
  
  ###################### START: PROJECT MANAGEMENT ENTITIES ######################
  
  # Project Management Entities  
  experimentVars$num_PE <- 0
  experimentVars$DEFAULT_PE <- 1
  experimentVars$ids_PE <- c() # get actives fund. agency ids
  
  observeEvent(input$addManagEntity, {
    defaultBoxes = experimentVars$DEFAULT_PE
    if (experimentVars$num_PE  >= 1) {
      insertBoxManagEntity(experimentVars$num_PE + 1)
    }
  })
  
  observe({
    if (experimentVars$num_PE == 0) {
      default <- experimentVars$DEFAULT_PE
      for (i in 1:default) {
        insertBoxManagEntity(i)
      }
    }
  })
  
  insertBoxManagEntity <- function(index) {
    experimentVars$ids_PE <- c(experimentVars$ids_PE, paste0("PE_", index))
    
    insertUI(
      selector = "#fr_managementEntities_boxes",
      where = "beforeBegin",
      ui = getUiProjectEntity(index)
    )
    experimentVars$num_PE <- experimentVars$num_PE + 1
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
              choices = c("Academic institution",
                          "Agricultural experimental extension",
                          "CGIAR center",
                          "Extension organization",
                          "Farm",
                          "Farmer organization",
                          "Government research institution, designated laboratory or center",
                          "Governement research institution (NARS)",
                          "International NGO",
                          "National NGO",
                          "Private sector entity",
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
  
  ###################### END: PROJECT MANAGEMENT ENTITIES ######################
  
  ###################### START: EXPERIMENT LEADS ######################
  
  # Experiment Leads
  experimentVars$num_EL <- 0
  experimentVars$DEFAULT_EL <- 1
  experimentVars$ids_EL <- c() # get actives fund. agency ids
  
  observeEvent(input$addExperimentLeads, {
    defaultBoxes = experimentVars$DEFAULT_EL
    if (experimentVars$num_EL >= 1) {
      insertBoxExperimentLead(experimentVars$num_EL + 1)
    }
  })
  
  observe({
    if (experimentVars$num_EL == 0) {
      default <- experimentVars$DEFAULT_EL
      for (i in 1:default) {
        insertBoxExperimentLead(i)
      }
    }
  })
  
  insertBoxExperimentLead <- function(index) {
    experimentVars$ids_EL <- c(experimentVars$ids_EL, paste0("EL_", index))
    
    insertUI(
      selector = "#fr_experimentLeads_boxes",
      where = "beforeBegin",
      ui = getUiExperimentLead(index)
    )
    experimentVars$num_EL <- experimentVars$num_EL + 1
  }
  
  getUiExperimentLead <- function(index) {
    fluidRow(
      id = paste0("fl_box_exp_lead_", index),
      box(
        title = "", solidHeader = TRUE, status = "warning", width=12,     
        column(
          12, offset = 0, style='padding:0px; text-align:right;', actionButton(paste0("exp_closeBox_EL_", index), "", icon("close"))
        ),
        fluidRow(
          column(
            width = 4,
            selectizeInput(
              paste0("projLeadEnt_", index), 
              "Lead organization", multiple =T, options = list(maxItems =1, placeholder = "Select one.."), 
              choices = c("Academic institution",
                          "Agricultural experimental extension",
                          "CGIAR center",
                          "Extension organization",
                          "Farm",
                          "Farmer organization",
                          "Government research institution, designated laboratory or center",
                          "Governement research institution (NARS)",
                          "International NGO",
                          "National NGO",
                          "Private sector entity",
                          "Other")
            ),
            textInput(inputId = paste0("expLead_", index), label = "Lead person/Primary Investigator", value = "")
          ),
          conditionalPanel(
            paste0("input.projLeadEnt_", index, " == 'CGIAR center'"),
            column(
              width = 4,
              selectizeInput(
                paste0("tLeadCenter_", index), "Choose CGIAR center", multiple = TRUE, options = list(maxItems =1, placeholder = "Select one..."), 
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
                            "IWMI - International Water Management Institute",
                            "WorldFish")
              )
            ),
            column(
              width = 4,
              selectizeInput(
                paste0("tLeadContCRP_", index), "Contributor CRP", multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), 
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
            hidden(textInput(paste0("projLeadEnt_", index, "_other"), "", value = ""))
          )
        )
      )
    )
  }
  
  # getUiExperimentLead <- function(index){
  #   fluidRow(
  #     id = paste0("fl_box_exp_lead_", index),
  #     box(
  #       title = "", solidHeader = TRUE, status = "warning", width = 12,
  #       column(
  #         12, offset = 0, style='padding:0px; text-align:right;', actionButton(paste0("exp_closeBox_EL_", index), "", icon("close"))
  #       ),
  #       fluidRow(
  #         column(
  #           width = 6,
  #           selectizeInput(
  #             paste0("projLeadEnt_", index), 
  #             "Experiment, lead organization type", selected="CGIAR center", multiple = T, options = list(maxItems = 1, placeholder = "Select one..."), 
  #             choices = c("CGIAR center",
  #                         "Other")
  #           ),
  #           conditionalPanel(
  #             paste0("input.projLeadEnt_", index, " == 'CGIAR center'"),
  #             selectizeInput(
  #               paste0("tLeadCenter_", index), "Choose CGIAR center", multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), 
  #               choices = c("Africa Rice Center",
  #                           "Bioversity International",
  #                           "Center for International Forestry Research (CIFOR)",
  #                           "International Center for Agricultural Research (ICARDA)",
  #                           "International Center for Tropical Agriculture (CIAT)",
  #                           "International Crops Research Institute for the Semi-Arid (ICRISAT)",
  #                           "International Food Policy Research Institute (IFPRI)",
  #                           "International Institute of Tropical Agriculture (IITA)",
  #                           "International Livestock Research Institure (ILRI)",
  #                           "International Maize and Wheat Improvement Center (CIMMYT)",
  #                           "International Potato Center (CIP)",
  #                           "International Rice Research Institute (IRRI)",
  #                           "International Water Management Institute (IWMI)",
  #                           "World Agroforestry Centre (ICRAF)",
  #                           "WorldFish",
  #                           "None")
  #             )
  #           ),
  #           conditionalPanel(
  #             paste0("input.projLeadEnt_", index, " == 'Other'"),
  #             selectizeInput(
  #               paste0("lead_org_type_", index), "",multiple = TRUE,  options = list(maxItems = 1, placeholder = "Select one..."),
  #               choices = c("Agricultural experimental extension",
  #                           "CGIAR center",
  #                           "Extension organization",
  #                           "Farm",
  #                           "Farmer association or cooperative",
  #                           "Government research institution, designated laboratory or center",
  #                           "Government research institution (NARS)",
  #                           "Non-governmental organization",
  #                           "Private company",
  #                           "University",
  #                           "University, main campus",
  #                           "Other")
  #             ),
  #             hidden(textInput(paste0("lead_org_type_", index, "_other"), "")),
  #             textInput(paste0("leadNameOther_", index), "Experiment, lead organization name", value = "")
  #           ),
  #           textInput(inputId = paste0("expLead_", index), label = "Experiment lead person / Primary Investigator", value = "")
  #         )
  #       )
  #     )
  #   )
  # }
  
  ###################### END: EXPERIMENT LEADS ######################
  
  # Funcion general que elimina en Experiment
  observeEvent(input$closeBox_EXP, {
    vars <- unlist(strsplit(input$closeBox_EXPid,"_"))
    type <- vars[3]
    index <- vars[4]
    
    if (type == "FA") {
      if (length(experimentVars$ids_FA) > 1) {
        removeUI(selector = paste0("#fl_box_fundingAgency_", index), immediate = T)
        experimentVars$ids_FA <- experimentVars$ids_FA[! experimentVars$ids_FA %in% paste0("FA_",index)]
      }
    }
    
    if (type == "PE") {
      if (length(experimentVars$ids_PE) > 1) {
        removeUI(selector = paste0("#fl_box_exp_ent_", index), immediate = T)
        experimentVars$ids_PE <- experimentVars$ids_PE[! experimentVars$ids_PE %in% paste0("PE_",index)]
      }
    }
    
    if (type == "EL") {
      if (length(experimentVars$ids_EL) > 1) {
        removeUI(selector = paste0("#fl_box_exp_lead_", index), immediate = T)
        experimentVars$ids_EL <- experimentVars$ids_EL[! experimentVars$ids_EL %in% paste0("EL_",index)]
      }
    }
  })
  
  ############################### END SERVER: EXPERIMENT  ###############################
  #######################################################################################
  
  #######################################################################################
  ############################### START SERVER: PERSONNEL ###############################
  
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
            textInput(paste0("person_firstName_", index), "First name", value = ""),
            textInput(paste0("person_lastName_", index), "Last name", value = "")
          ),
          column(
            width=6,
            textInput(paste0("person_email_", index), "Email", value = "", placeholder = "example@domain.com"),
            selectizeInput(
              paste0("person_affiliation_", index), "Affiliation", multiple = T,
              options = list(maxItems = 1, placeholder = "Select one.."),
              choices = c("Academic institution",
                          "Agricultural experimental extension",
                          "CGIAR center",
                          "Extension organization",
                          "Farmer organization",
                          "Governement research institution (NARS)",
                          "Government research institution, designated laboratory or center",
                          "International NGO",
                          "National NGO",
                          "Private sector entity",
                          "Other")
            ),
            conditionalPanel(
              paste0("input.person_affiliation_", index," == 'CGIAR center'" ),
              selectizeInput(
                paste0("person_center_", index), "", multiple = TRUE,
                options = list(maxItems = 1, placeholder = "Select one..."),
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
                            "IWMI - International Water Management Institute",
                            "WorldFish")
              )
            ),
            conditionalPanel(
              paste0("input.person_affiliation_", index, " != undefined && 
                     input.person_affiliation_", index, ".length >= 1 &&
                     input.person_affiliation_", index," != 'CGIAR center' &&
                     input.person_affiliation_", index, " != 'Other'"),
              textInput(paste0("affiliation_name_", index), "")
              ),
            hidden(textInput(paste0("person_affiliation_", index, "_other"), "", value = "")),
            textInput(
              inputId = paste0("person_orcid_", index),
              label = HTML("ORCID if available (if not consider <a href='https://orcid.org/register' target='_blank'>registering!</a>)"),
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
    
    if (length(personnelVars$ids_PERS) > 1) {
      removeUI(selector = paste0("#fr_personnel_box_", index), immediate = T)
      personnelVars$ids_PERS <- personnelVars$ids_PERS[! personnelVars$ids_PERS %in% paste0("PERS_",index)]
    }
    
  })
  
  ############################### END SERVER: PERSONNEL ###############################
  #####################################################################################
  
  ##################################################################################
  ############################### START SERVER: SITE ###############################
  
  shiny::observe({
    path <- fbglobal::get_base_dir()
    geodb_file <- "table_sites_agrofims.rds"
    path <- file.path(path, geodb_file)
    x_sites_data <- readRDS(file = path)
    # values$sites_data <-  dplyr::filter(x_sites_data, userId==0)
    values$sites_data <-  x_sites_data
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
  
  
  observeEvent(input$othertVEG,{
    choices <-  input[[input$othertVEGid]]
    updateOthertVEG(choices)
  })
  
  # Funcion que inserta el Other de "Type of experiment"
  updateOthertVEG <- function(choices) {
    
    if (any(choices == "Other") == T) {
      removeUI(selector = "#otherVEG", immediate = T)
      if (any(choices != "")) {
        # Other
        insertUI(
          selector = "#othertVEG_aux",
          where = "beforeBegin",
          ui = fluidRow(
            id = "othertVEG",
            column(
              12,
              selectizeInput(
                inputId = "fbDesign_inSiteVegetation_other", label = "Other", 
                choices = c("a","b","c"), multiple = T,
                options = list('create' = TRUE)
              )
            )
          )
        )
      }
    } else {
      removeUI(selector = "#othertVEG", immediate = T)
    }
  }
  
  ############################### END SERVER: SITE ###############################
  ################################################################################
  
  ##################################################################################
  ############################### START SERVER: CROP ###############################
  
  ###################### START: FIELDBOOK DETAILS ######################
  
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
  
  ###################### END: FIELDBOOK DETAILS ######################
  
  cropsVar <- reactiveValues()
  cropsVar$cropValues <- c()
  
  ###################### START: INTERCROP ######################
  
  # Intercrop: Asigna variables reactivas
  NUM_BOX_INTERCROP_DEFAULT <- 2
  
  
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
    cropsVar$cropValues
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
    cropType <- input[["croppingType"]]
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
    
    getCropIdsValues(cropType)
    
  })
  
  # When 'other crop' name is filled 
  observeEvent(input$cropBoxMulticropVarOther,{
    
    vars <- unlist(strsplit(input$cropBoxMulticropVarOtherId, "_"))
    cropType <- vars[1]
    crop_order <- vars[3]
    cropCommonNameOther = input[[paste0(cropType,"_cropCommonName_", crop_order)]]
    mtext <- input[[input$cropBoxMulticropVarOtherId]]
    

    if (mtext == "")
      mtext <- "Other"
    output[[paste0("intercropName_row_crop_", crop_order)]] <- renderText(mtext)
      

    if (cropType == "int" || cropType == "rel" || cropType =="rot") {
      if (cropCommonNameOther == "Other" && length(cropCommonNameOther) > 0) {
        #Planting and Transplanting
        output[[paste0("title_panel_",cropType,"_pt_", crop_order)]] = renderText({
          mtext
        })
        #Harvest
        output[[paste0("title_panel_",cropType,"_harv_", crop_order)]] = renderText({
          mtext
        })
        #Measurement 
        output[[paste0("title_panel_",cropType,"_mea_", crop_order)]] = renderText({
          mtext
        })
        #Phenology
        output[[paste0("title_panel_",cropType,"_phe_", crop_order)]] = renderText({
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
        
        deleteTabsFromPlantingAndTransplanting(typeCrop,index)
        deleteTabsFromHarvest(typeCrop,index)
        
        deleteTabsFromMeasurementAndPhenology(typeCrop,index)
        
        intercropVars$ids <- intercropVars$ids[!intercropVars$ids %in% paste0("int_", index)]
      }
    } else if (typeCrop == "rel") {
      if (length(relaycropVars$ids) > 2) {
        removeUI(selector = paste0("#", typeCrop, "_fr_box_", index), immediate = T)
        
        deleteTabsFromPlantingAndTransplanting(typeCrop,index)
        deleteTabsFromHarvest(typeCrop,index)
        
        deleteTabsFromMeasurementAndPhenology(typeCrop,index)
        
        relaycropVars$ids <- relaycropVars$ids[!relaycropVars$ids %in% paste0("rel_", index)]
      }
    } else if (typeCrop == "rot") {
      if (length(rotationcropVars$ids) > 2) {
        removeUI(selector = paste0("#", typeCrop, "_fr_box_", index), immediate = T)
        
        deleteTabsFromPlantingAndTransplanting(typeCrop,index)
        deleteTabsFromHarvest(typeCrop,index)
        
        deleteTabsFromMeasurementAndPhenology(typeCrop,index)
        
        rotationcropVars$ids <- rotationcropVars$ids[!rotationcropVars$ids %in% paste0("rot_", index)]
      }
    }
  })
  
  # Funcion que obtiene los valores de los crop
  getCropIdsValues <- function(cropType){
    
    cropIds <- NULL
    cropsVar$cropValues <- NULL
    
    if(cropType == "Intercrop")
      cropIds <- intercropVars$ids
    else if (cropType == "Relay crop")
      cropIds <- relaycropVars$ids
    else if (cropType == "Rotation")
      cropIds <- rotationcropVars$ids
    
    for (i in cropIds)
    {
      vars <- unlist(str_split(i,"_"))
      val <- input[[paste0(vars[1],"_cropCommonName_",vars[2])]]
      
      if(length(val)>0){
        
        if(val == "Other")
          val <- input[[paste0(vars[1],"_cropCommonName_",vars[2],"_other")]]
        
        cropsVar$cropValues  <- append(val,cropsVar$cropValues)
      }
      
    }
    
    return(cropsVar$cropValues)
  }
  
  ###################### END: FUNCIONES GENERALES INTERCROP/RELAYCROP/ROTATION  ######################
  
  ############################### END SERVER: CROP ###############################
  ################################################################################
  
  ##################################################################################
  ############################### START SERVER: DESIGN #############################
  
  factores <- agdesign::dt_factordesign
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
  
  ###################### START: FUNCIONES GENERALES CRD/RCBD/FCRD/FRCBD/SPRCBD/SPSP ######################
  
  #Variable para validar disenno segun factor
  AINFO <- "yes"
  
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
                    paste0(design, "_sel_factor_", index), "",
                    multiple = TRUE,
                    options = list(placeholder = "Select...", maxItems = 1),
                    choices = c(dt$fchoices),
                    selected = value
                  ),
                  fluidRow(id = paste0(design, "_sel_factorOth_aux_", index)),
                  fluidRow(id = paste0(design, "_factor_crop_aux_", index))
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
                  ),
                  hidden(
                    selectInput(
                      paste0(design, "_numLevelsESP_", index),
                      "Number of evaluations",
                      choices = 1:10,
                      selected = 1
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
    
    # Genera Multicrop del FACTOR
    updateSelectCropGEN(index, choises, design)
    
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
            column(12, textInput(paste0(design, "_sel_factor_other_", index), "", value=" "))
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
  
  # Funcion GENERAL que activa "CROP" de "FACTOR" dependiendo del diseno
  updateSelectCropGEN <- function(index, choices, design) {
    
    
    
    if (!is.null(choices)) {
      
      cropType<- input[["croppingType"]]
      cropVals <- getCropIdsValues(cropType)
      
      muc <- get_dfa_values(dt=dt , choice = choices, attribute = "MULTICROP")
      
      removeUI(
        selector = paste0("#", design, "_factor_crop_", index),
        immediate = T
      )
      
      #Capturamos valor del Crop
      cropValue <- input[["croppingType"]]
      
      if(muc=="yes" && cropValue != "Monocrop"){
        insertUI(
          selector = paste0("#", design, "_factor_crop_aux_", index),
          where = "beforeBegin",
          ui = fluidRow(
            id = paste0(design, "_factor_crop_", index),
            column(
              12,
              selectizeInput(
                paste0(design, "factor_crop_input", index), "Crop",
                multiple = TRUE,
                options = list(placeholder = "Select...", maxItems = 1),
                # Arreglo insertamos
                choices = cropsVar$cropValues
              )
            )
          )
        )
      } 
      # else {
      #   removeUI(
      #     selector = paste0("#", design, "_factor_crop_", index),
      #     immediate = T
      #   )
      # }
      
    }
  }
  
  # Funcion GENERAL que responde a "LEVELS"
  observeEvent(input$levelsGEN, {
    vars <- unlist(strsplit(input$levelsGENid, "_"))
    design <- vars[1]
    index <- vars[3]
    
    num_levels <-  input[[input$levelsGENid]]
    
    # ocultar en desarrollo la siguiente linea "factores"
    factores <- agdesign::dt_factordesign
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
  
  # Funcion GENERAL que responde a CASOS ESPECIALES "TYPE (OTHER)"
  observeEvent(input$otherTypeESP,{
    
    vars <- unlist(strsplit(input$otherTypeESPid, "_"))
    design <- vars[1]
    index <- vars[4]
    boxIndex <- vars[5]
    value <- input[[input$otherTypeESPid]]
    
    if(length(value)>0 && value == "Other")
    {
      insertUI(
        selector = paste0("#", design, "_lvl_espType_aux_", index, "_", boxIndex),
        where = "beforeBegin" ,
        ui = fluidRow(
          id=paste0(design,"_lvl_espTypeOther_row",index,"_",boxIndex),
          column(
            12,
            textInput(paste0(design, "_lvl_espType_Other_", index,"_",boxIndex),
                      label = "Other",
                      placeholder = "Write...")
            # selectizeInput(
            #   paste0(design, "_lvl_espType_Other_", index,"_",boxIndex),
            #   label = "Other",
            #   multiple = T,
            #   choices = c(),
            #   options = list(
            #     maxItems = 20,
            #     placeholder = "Write..." ,
            #     'create' = TRUE,
            #     'persist' = FALSE
            #   )
            # )
          )
        )
      )
    }else{
      removeUI(
        selector = paste0("#",design,"_lvl_espTypeOther_row",index,"_",boxIndex),
        immediate = T
      )
    }
  })
  
  # Funcion GENERAL que responde a CASOS ESPECIALES "LEVELS"
  observeEvent(input$levelsESP, {
    vars <- unlist(strsplit(input$levelsESPid, "_"))
    design <- vars[1]
    index <- vars[3]
    
    num_levels <-  input[[input$levelsESPid]]
    
    #ocultar en desarrollo la siguiente linea "factores"
    factores <- agdesign::dt_factordesign
    dt <- factores %>% mutate(fchoices = FACTOR)
    
    drawSpecialCasesLevelGEN(
      order = index,
      dt = dt,
      design,
      input_choice = input[[paste0(design, "_sel_factor_", index)]],
      num_levels
    )
    shinyjs::show(id = paste0(design, "_numLevelsESP_", index))
  })
  
  # Funcion que responde a los CASOS ESPECIALES TIMING "LEVELS"
  observeEvent(input$levelsTimingESP, {
    vars <- unlist(strsplit(input$levelsTimingESPid, "_"))
    design <- vars[1]
    index <- vars[3]
    
    num_levels <-  input[[input$levelsTimingESPid]]
    timingValue = input[[paste0(design,"_lvltiming_",index )]]
    #ocultar en desarrollo la siguiente linea "factores"
    factores <- agdesign::dt_factordesign
    dt <- factores %>% mutate(fchoices = FACTOR)
    
    drawTimingSpecialCasesLevelGEN_BODY(design,timingValue, index,num_levels)
    
  })
  
  # Funcion GENERAL que activa "LEVELS" dependiendo del diseno
  updateLevelSelectionGEN <- function(index, value, design) {
    # ocultar en desarrollo la siguiente linea "factores"
    factores <- agdesign::dt_factordesign
    dt <- factores %>% mutate(fchoices = FACTOR)
    
    removeUI(selector = paste0("#", design, "_fl_title_factor_", index), immediate = T)
    removeUI(selector = paste0("#", design, "_note_", index), immediate = T)
    removeUI(selector = paste0("#", design, "_levelSelection_", index), immediate = T)
    removeUI(selector = paste0("#", design, "_levelSelOther_", index), immediate = T)
    
    
    shinyjs::hide(id = paste0(design, "_numLevels_", index))
    shinyjs::hide(id = paste0(design, "_numLevelsESP_", index))
    
    
    num_levels <- input[[paste0(design, "_numLevels_", index)]]
    num_levels_esp <- input[[paste0(design, "_numLevelsESP_", index)]]
    
    
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
      
      
      ### Agregado ###
      input_choice = input[[paste0(design, "_sel_factor_", index)]]
      ### Agregado ###
      
      if (!is.null(type_form) && type_form == "combo box") {
        
        if (input_choice == "Fertilizer type and amount" && AINFO == "yes"){
          
          drawSpecialCasesLevelGEN(
            order = index,
            dt = dt,
            design,
            input_choice = input[[paste0(design, "_sel_factor_", index)]],
            num_levels_esp
          )
          shinyjs::show(id = paste0(design, "_numLevelsESP_", index))
        } else{
          drawComboBoxLevelGEN(
            order = index,
            dt = dt,
            design,
            input_choice = input[[paste0(design, "_sel_factor_", index)]]
          )
          shinyjs::hide(id = paste0(design, "_numLevels_", index))
        }
      } else if (!is.null(type_form) && type_form == "text input") {
        
        if ((input_choice %in%  c("Mulch type and amount",
                                  "Crop residue amount",
                                  "Fertilizer type and amount",
                                  "Nutrient element type and amount",
                                  "Irrigation amount",
                                  "Biotic stress type and amount",
                                  "Biotic stress control product type and amount"))
            && AINFO == "yes"){
          
          drawSpecialCasesLevelGEN(
            order = index,
            dt = dt,
            design,
            input_choice = input[[paste0(design, "_sel_factor_", index)]],
            num_levels_esp
          )
          
          shinyjs::show(id = paste0(design, "_numLevelsESP_", index))
          
        } 
        else {
          drawTextInputLevelGEN(
            order = index,
            dt = dt,
            design,
            input_choice = input[[paste0(design, "_sel_factor_", index)]],
            "numeric + units"
          )
          shinyjs::hide(id = paste0(design, "_numLevels_", index))
        }
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
      } else if (!is.null(type_form) && type_form == "text box or date"){
        if ((input_choice %in%  c("Weeding timing", 
                                  "Irrigation timing", 
                                  "Fertilizer timing",
                                  "Abiotic stress timing",
                                  "Biotic stress control timing",
                                  "Biotic stress application timing"))
            && AINFO == "yes"){
          
          drawTimingSpecialCasesLevelGEN_HEA(
            order = index,
            dt = dt,
            design,
            input_choice = input[[paste0(design, "_sel_factor_", index)]],
            1
          )
          
          #shinyjs::show(id = paste0(design, "_numLevelsTimingESP_", index))
          
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
    lbl <- get_dfa_values(dt, choice = input_choice, attribute = "FACTOR")
    unit <- get_dfa_values(dt, choice = input_choice, attribute = "UNIT")
    
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
  
  # Funcion GENERAL que responde a CASOS ESPECIALES TIMING 
  observeEvent(input$timingESP,{
    id <- input$timingESPid
    vars <- unlist(str_split(id,"_"))
    timingValue <- input[[id]]
    design <- vars[1]
    index <- vars[3]
    
    if (timingValue == "Date")
    {
      removeUI(
        selector = paste0("#",design, "_numLevelsTimingESP_row_", index),
        immediate = T
      )
      
      insertUI(
        selector = paste0("#",design, "_numLevelsTimingESP_aux_", index),
        where = "afterEnd",
        ui = fluidRow(id=paste0(design,"_numLevelsTimingESP_row_", index),
                      column(12,
                             selectizeInput(
                               inputId  = paste0(design, "_numLevelsTimingESP_", index),
                               label    = "Number of inputs", 
                               multiple = TRUE,
                               options  = list(maxItems = 1,placeholder = "Select one..."),
                               choices  = c(1:10), selected = 1
                             )
                      )
        )
      )
    }else{
      removeUI(
        selector = paste0("#",design, "_numLevelsTimingESP_row_", index),
        immediate = T
      )
    }
    
    drawTimingSpecialCasesLevelGEN_BODY(design,timingValue, index,1)
    
  })
  
  # Funcion GENERAL que dibuja CASOS ESPECIALES dependiendo del diseno
  drawSpecialCasesLevelGEN <- function(order,dt, design, input_choice, num_levels) {
    
    flevel <- get_dfa_values(dt, choice = input_choice, attribute = "LEVEL")
    unit <- get_dfa_values(dt, choice = input_choice, attribute = "UNIT")
    
    #Levels
    choices_level <- strsplit(flevel, split = ";")[[1]] %>% stringi::stri_trim_both()
    #Units
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
        fluidRow(
          id= paste0(design, "_levelSelection_", order,"_1"),
          column(
            12,
            box(
              solidHeader = TRUE,
              status = "info",
              width = 12,
              column(
                12,
                fluidRow(
                  column(
                    12,
                    selectizeInput(
                      inputId = paste0(design, "_lvl_espType_", order,"_1"),
                      label = "Type",
                      multiple = F,
                      options = list(placeholder = "Select..."),
                      choices = c(choices_level)
                    )
                  )
                ),
                fluidRow(id=paste0(design,"_lvl_espType_aux_",order,"_1"))
              ),
              column(
                6,
                selectizeInput(
                  paste0(design, "_lvl_", order,"_1"),
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
                  inputId = paste0(design, "_lvl_unit_", order,"_1"),
                  label = "Unit",
                  choices = c(choices_unit),
                  selected = 2
                )
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
          selector = paste0("#", design, "_levelSelection_", order, "_", i - 1),
          where = "afterEnd",
          ui = fluidRow(
            id = paste0(design, "_levelSelection_", order, "_", i),
            column(
              12,
              box(
                solidHeader = TRUE,
                status = "info",
                width = 12,
                column(
                  12,
                  fluidRow(
                    column(
                      12,
                      selectizeInput(
                        inputId = paste0(design, "_lvl_espType_", order,"_",i),
                        label = "Type",
                        multiple = F,
                        options = list(placeholder = "Select..."),
                        choices = c(choices_level)
                      )
                    )
                  ),
                  fluidRow(id=paste0(design,"_lvl_espType_aux_",order,"_",i))
                ),
                column(
                  6,
                  selectizeInput(
                    paste0(design, "_lvl_", order,"_",i),
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
                    inputId = paste0(design, "_lvl_unit_", order,"_",i),
                    label = "Unit",
                    choices = c(choices_unit),
                    selected = 2
                  )
                )
                
              )
            )
          )
        )
      }
    }
  }
  
  # Funcion GENERAL que dibuja CASOS ESPECIALES TIMING HEADER dependiendo del diseno
  drawTimingSpecialCasesLevelGEN_HEA <- function(order,dt, design, input_choice, num_levels) {
    
    flevel <- get_dfa_values(dt, choice = input_choice, attribute = "LEVEL")
    #unit <- get_dfa_values(dt, choice = input_choice, attribute = "UNIT")
    
    choices_level <- strsplit(flevel, split = ";")[[1]] %>% stringi::stri_trim_both()
    #choices_unit <- strsplit(unit, ",")[[1]]
    
    
    # removeUI(
    #   selector = paste0("#", design, "_levelSelection_", order),
    #   immediate = T
    # )
    
    removeUI(
      selector = paste0("#", design, "_levelSelectionTiming_", order),
      immediate = T
    )
    
    insertUI(
      selector = paste0("#", design, "_levelSelection_aux_", order),
      where = "beforeBegin",
      ui = fluidRow(
        id = paste0(design, "_levelSelection_", order),
        column(
          12,
          fluidRow(
            column(
              8,
              selectizeInput(
                inputId = paste0(design, "_lvltiming_", order),
                label = "Timing",
                multiple = TRUE,
                options = list(maxItems = 1, placeholder = "Select..."),
                choices = c(choices_level),selected="Days after planting"
              )
            ),
            column(
              4,
              fluidRow(id=paste0(design, "_numLevelsTimingESP_aux_", order))
            )
          ),
          fluidRow(
            id=paste0(design, "_levelSelectionTiming_", order),
            column(12,
                   fluidRow(
                     id = paste0(design, "_levelSelectionTiming_", order,"_1"),
                     column(
                       8,
                       selectizeInput(inputId = paste0(design, "_lvltimingValue_", order, "_1"),
                                      label = "Days after planting",
                                      multiple = TRUE,
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
          ),
          fluidRow(id=paste0(design, "_levelSelectionTiming_aux_", order))
        )
      )
    )
  }
  
  # Funcion GENERAL que dibuja CASOS ESPECIALES TIMING BODY dependiendo del diseno
  drawTimingSpecialCasesLevelGEN_BODY <- function(design,timingValue, order,num_levels){
    
    removeUI(
      selector = paste0("#",design, "_levelSelectionTiming_", order),
      immediate = T
    )
    
    if(length(timingValue)>0){
      insertUI(
        selector = paste0("#", design, "_levelSelectionTiming_aux_", order),
        where = "beforeBegin",
        ui = fluidRow(id = paste0(design, "_levelSelectionTiming_", order))
      )
      
      for (i in 1:num_levels) {
        
        insertUI(
          selector = paste0("#", design, "_levelSelectionTiming_", order),
          where = "beforeEnd",
          ui = column(
            12,
            fluidRow(
              id = paste0(design, "_levelSelectionTiming_", order,"_",i),
              if(timingValue == "Date"){
                column(3,
                       airDatepickerInput(
                         inputId = paste0(design, "_lvltimingValue_", order, "_",i),
                         label = paste0("# ",i," ",timingValue),
                         dateFormat = "yyyy-mm-dd",
                         value = Sys.Date(),
                         placeholder = "yyyy-mm-dd",
                         addon = "none",
                         clearButton = TRUE
                       )
                )
                
              }else if(timingValue == "Frequency"){
                column(8,
                       textInput(paste0(design, "_lvltimingValue_", order, "_",i),
                                 label = timingValue)
                )
                
              }else if(timingValue == "Other"){
                column(8,
                       selectizeInput(inputId = paste0(design, "_lvltimingValue_", order, "_",i),
                                      label = timingValue,
                                      multiple = TRUE,
                                      choices = c(),
                                      options = list(
                                        maxItems = 20,
                                        placeholder = "Write..." ,
                                        'create' = TRUE,
                                        'persist' = FALSE
                                      )
                       )
                )
                
              }else{
                column(
                  8,
                  selectizeInput(inputId = paste0(design, "_lvltimingValue_", order, "_",i),
                                 label = paste0(timingValue),
                                 multiple = TRUE,
                                 choices = c(),
                                 options = list(
                                   maxItems = 20,
                                   placeholder = "Write..." ,
                                   'create' = TRUE,
                                   'persist' = FALSE
                                 )
                  )
                )
                
              }
            )
          )
        )
      }
      
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
                addon = "none",
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
                addon = "none",
                clearButton = TRUE
              )
            )
          )
        )
      }
    }
  }
  
  ###################### END: FUNCIONES GENERALES CRD/RCBD/FCRD/FRCBD/SPRCBD/SPSP ######################
  
  ###################### START: FUNCIONES COMPARTIDAS CRD/RCBD ######################
  
  # Funcion que responde a LVLS de la tabla CRD-RCBD 
  observeEvent(input$levelInput, {
    designFactor <- tolower(designVarsFactor$design)
    if (designFactor == "crd") {
      design <- tolower(input$designFieldbook_agrofims)
      IdDesignInputs <- getFactorIds(design)
      index <- get_index_design(IdDesignInputs, design)
      allinputs<-AllInputs()
      
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
      allinputs<-AllInputs()
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
  
  ###################### END: FUNCIONES COMPARTIDAS CRD/RCBD ######################
  
  ##################################################################################
  ############################### END SERVER: DESIGN ###############################
  
  ##################################################################################################
  
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
                         addon = "none",
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
                         addon = "none",
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
                         addon = "none",
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
                         addon = "none",
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
                         addon = "none",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("puddling_start_date",
                         "Start date",
                         clearButton = T,
                         autoClose = T,
                         addon = "none",
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
                         addon = "none",
                         minDate = as.Date(input$fbDesign_project_start_date) + 1,
                         maxDate = as.Date(input$fbDesign_project_end_date) + 1
                         
      )
    } else {
      airDatepickerInput("tillage_start_date",
                         "Start date",
                         clearButton = T,
                         autoClose = T,
                         addon = "none",
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
  observeEvent(input$PTBoxMulticropVar, {
    
    id <- input$PTBoxMulticropVarid
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
      if(val[i] != "" && !is.na(val[i])){
        val[i] <- paste0(cropType, "_pt_", id)
        
      }
      i<-i+1
    }
    
    val <- val[val != ""]
    val
    
  }
  
  expconPTmulticrop <- reactiveValues()
  expconPTmulticrop$var_int <- c()
  expconPTmulticrop$var_rel <- c()
  expconPTmulticrop$var_rot <- c()
  
  
  # Funcion que inserta los tabs dependiendo del tipo de cultivo
  insertTabsPT <- function(vals, cropType) {
    
    if (length(vals) != 0) {
      
      if(cropType == "int"){
        xx <- expconPTmulticrop$var_int[!expconPTmulticrop$var_int%in%vals]
        vals <- vals[!vals %in% unique(expconPTmulticrop$var_int)]
        expconPTmulticrop$var_int <- c(expconPTmulticrop$var_int, vals)
      }else if(cropType == "rel"){
        xx <- expconPTmulticrop$var_rel[!expconPTmulticrop$var_rel%in%vals]
        vals <- vals[!vals %in% unique(expconPTmulticrop$var_rel)]
        expconPTmulticrop$var_rel <- c(expconPTmulticrop$var_rel, vals)
      }else if(cropType == "rot"){
        xx <- expconPTmulticrop$var_rot[!expconPTmulticrop$var_rot%in%vals]
        vals <- vals[!vals %in% unique(expconPTmulticrop$var_rot)]
        expconPTmulticrop$var_rot <- c(expconPTmulticrop$var_rot, vals)
      }
      
      
      if (!is.null(xx) && identical(xx, character(0)) == FALSE) {
        for (i in 1:length(xx)) {
          removeTab(inputId = paste0("tabpanelPT", cropType), target = xx[i])
          if(cropType == "int"){
            expconPTmulticrop$var_int <- expconPTmulticrop$var_int[!expconPTmulticrop$var_int %in% xx]
          }else if(cropType == "rel"){
            expconPTmulticrop$var_rel <- expconPTmulticrop$var_rel[!expconPTmulticrop$var_rel %in% xx]
          }else if(cropType == "rot"){
            expconPTmulticrop$var_rot <- expconPTmulticrop$var_rot[!expconPTmulticrop$var_rot %in% xx]
          }
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
  
  deleteTabsFromPlantingAndTransplanting <- function(typeCrop,index){
    if (typeCrop == "int"){
      removeTab(inputId = paste0("tabpanelPT", typeCrop), target = paste0("int_pt_",index)) 
      expconPTmulticrop$var_int <- expconPTmulticrop$var_int[!expconPTmulticrop$var_int %in% paste0("int_pt_",index)] 
    }else if (typeCrop == "rel"){
      removeTab(inputId = paste0("tabpanelPT", typeCrop), target = paste0("rel_pt_",index)) 
      expconPTmulticrop$var_rel <- expconPTmulticrop$var_rel[!expconPTmulticrop$var_rel %in% paste0("rel_pt_",index)] 
    }else if (typeCrop == "rot"){
      removeTab(inputId = paste0("tabpanelPT", typeCrop), target = paste0("rot_pt_",index)) 
      expconPTmulticrop$var_rot <- expconPTmulticrop$var_rot[!expconPTmulticrop$var_rot %in% paste0("rot_pt_",index)] 
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
  # rotation PT crop 1
  expconRotPTcrop1 <- reactiveValues()
  expconRotPTcrop1$num <- 0
  expconRotPTcrop1$DEFAULT <- 1
  # rotation PT crop 2
  expconRotPTcrop2 <- reactiveValues()
  expconRotPTcrop2$num <- 0
  expconRotPTcrop2$DEFAULT <- 1
  # rotation PT crop 3
  expconRotPTcrop3 <- reactiveValues()
  expconRotPTcrop3$num <- 0
  expconRotPTcrop3$DEFAULT <- 1
  # rotation PT crop 4
  expconRotPTcrop4 <- reactiveValues()
  expconRotPTcrop4$num <- 0
  expconRotPTcrop4$DEFAULT <- 1
  # rotation PT crop 5
  expconRotPTcrop5 <- reactiveValues()
  expconRotPTcrop5$num <- 0
  expconRotPTcrop5$DEFAULT <- 1
  
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
    if (expconRotPTcrop1$num >= 1 && crop == "rot_pt_1") { insertRow_PT(crop = "rot_pt_1", expconRotPTcrop1$num + 1) }
    if (expconRotPTcrop2$num >= 1 && crop == "rot_pt_2") { insertRow_PT(crop = "rot_pt_2", expconRotPTcrop2$num + 1) }
    if (expconRotPTcrop3$num >= 1 && crop == "rot_pt_3") { insertRow_PT(crop = "rot_pt_3", expconRotPTcrop3$num + 1) }
    if (expconRotPTcrop4$num >= 1 && crop == "rot_pt_4") { insertRow_PT(crop = "rot_pt_4", expconRotPTcrop4$num + 1) }
    if (expconRotPTcrop5$num >= 1 && crop == "rot_pt_5") { insertRow_PT(crop = "rot_pt_5", expconRotPTcrop5$num + 1) }
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
    # rotation PT crop 1
    if (crop == "rot_pt_1") {
      insertUI(
        selector = "#rot_pt_1_fr_plantingTransplating",
        where = "beforeBegin",
        ui = getPTUI_GEN(crop, index)
      )
      expconRotPTcrop1$num <- expconRotPTcrop1$num + 1
    }
    # rotation PT crop 2
    if (crop == "rot_pt_2") {
      insertUI(
        selector = "#rot_pt_2_fr_plantingTransplating",
        where = "beforeBegin",
        ui = getPTUI_GEN(crop, index)
      )
      expconRotPTcrop2$num <- expconRotPTcrop2$num + 1
    }
    # rotation PT crop 3
    if (crop == "rot_pt_3") {
      insertUI(
        selector = "#rot_pt_3_fr_plantingTransplating",
        where = "beforeBegin",
        ui = getPTUI_GEN(crop, index)
      )
      expconRotPTcrop3$num <- expconRotPTcrop3$num + 1
    }
    # rotation PT crop 4
    if (crop == "rot_pt_4") {
      insertUI(
        selector = "#rot_pt_4_fr_plantingTransplating",
        where = "beforeBegin",
        ui = getPTUI_GEN(crop, index)
      )
      expconRotPTcrop4$num <- expconRotPTcrop4$num + 1
    }
    # rotation PT crop 5
    if (crop == "rot_pt_5") {
      insertUI(
        selector = "#rot_pt_5_fr_plantingTransplating",
        where = "beforeBegin",
        ui = getPTUI_GEN(crop, index)
      )
      expconRotPTcrop5$num <- expconRotPTcrop5$num + 1
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
                12,
                selectizeInput(
                  paste0(crop, "_directSeeding_to_collect_field_", index), label = "To collect in the field", multiple = TRUE, 
                  options = list(maxItems = 12, placeholder = "Select one..."), 
                  choices = magm_label$get("direct")
                  # choices = c("Start date",
                  #             "Seeding environment",
                  #             "Seeding technique",
                  #             "Seed treatment",
                  #             "Type",
                  #             "Traction",
                  #             "Distance between rows",
                  #             "Seeding rate",
                  #             "Distance between plants",
                  #             "Number of rows",
                  #             "Plant density",
                  #             "Distance between bunds")
                ),
                hr()
              )
            ),
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
                                         addon = "none",
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
                                         addon = "none",
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
                12,
                selectizeInput(
                  paste0(crop, "_transplanting_to_collect_field_", index), label = "To collect in the field", multiple = TRUE, 
                  options = list(maxItems = 12, placeholder = "Select one..."), 
                  choices = magm_label$get("transplanting")
                  # choices = c("Start date",
                  #             "End date",
                  #             "Age of seedling (days)",
                  #             "Seedling environment",
                  #             "Technique",
                  #             "Seed treatment",
                  #             "Traction",
                  #             "Distance between rows",
                  #             "Seedling density",
                  #             "Number of rows",
                  #             "Distance between plants",
                  #             "Distance between bunds")
                ),
                hr()
              )
            ),
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
                        addon = "none",
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
                        addon = "none",
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
                        addon = "none",
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
                        addon = "none",
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
                         addon = "none",
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
                         addon = "none",
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
                         addon = "none",
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
                         addon = "none",
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
                         addon = "none",
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
                         addon = "none",
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
                         addon = "none",
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
                         addon = "none",
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
                  addon = "none",
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
                  addon = "none",
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
                  addon = "none",
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
                  addon = "none",
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
                  addon = "none",
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
                  addon = "none",
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
  observeEvent(input$HARVBoxMulticropVar, {
    id <- input$HARVBoxMulticropVarid
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
      if(val[i] != "" && !is.na(val[i])){
        val[i] <- paste0(cropType, "_harv_", id)
        
      }
      i<-i+1
    }
    
    val <- val[val != ""]
    val
  }
  
  expconHARVmulticrop <- reactiveValues()
  expconHARVmulticrop$var_int <- c()
  expconHARVmulticrop$var_rel <- c()
  expconHARVmulticrop$var_rot <- c()
  
  # Funcion que inserta los tabs dependiendo del tipo de cultivo
  insertTabsHARV <- function(vals, cropType) {
    if (length(vals) != 0) {
      
      if (cropType == "int"){
        xx <- expconHARVmulticrop$var_int[!expconHARVmulticrop$var_int%in%vals]
        vals <- vals[!vals %in% unique(expconHARVmulticrop$var_int)]
        expconHARVmulticrop$var_int <- c(expconHARVmulticrop$var_int, vals)
      }else if (cropType == "rel"){
        xx <- expconHARVmulticrop$var_rel[!expconHARVmulticrop$var_rel%in%vals]
        vals <- vals[!vals %in% unique(expconHARVmulticrop$var_rel)]
        expconHARVmulticrop$var_rel <- c(expconHARVmulticrop$var_rel, vals)
      }else if (cropType == "rot"){
        xx <- expconHARVmulticrop$var_rot[!expconHARVmulticrop$var_rot%in%vals]
        vals <- vals[!vals %in% unique(expconHARVmulticrop$var_rot)]
        expconHARVmulticrop$var_rot <- c(expconHARVmulticrop$var_rot, vals)
      }
      
      
      if (!is.null(xx)) {
        for (i in 1:length(xx) && identical(xx,character(0))==FALSE) {
          removeTab(inputId = paste0("tabpanelHARV", cropType), target = xx[i])
          if (cropType == "int"){
            expconHARVmulticrop$var_int <- expconHARVmulticrop$var_int[!expconHARVmulticrop$var_int %in% xx]
          } else if (cropType == "rel"){
            expconHARVmulticrop$var_rel <- expconHARVmulticrop$var_rel[!expconHARVmulticrop$var_rel %in% xx]
          } else if (cropType == "rot"){
            expconHARVmulticrop$var_rot <- expconHARVmulticrop$var_rot[!expconHARVmulticrop$var_rot %in% xx]
          }
          
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
              fluidRow(
                column(
                  12,
                  selectizeInput(
                    paste0(vals[i], "_harvest_to_collect_field_", i ), label = "To collect in the field", multiple = TRUE, 
                    options = list(maxItems = 5, placeholder = "Select one..."), 
                    choices = magm_label$get("harvest")
                    # choices = c("Start date",
                    #             "End date",
                    #             "Harvest Method",
                    #             "Crop component harvested",
                    #             "Harvestable area",
                    #             "Amount harvested",
                    #             "Harvest cut height",
                    #             "Type",
                    #             "Traction")
                  )#,
                  #hr()
                )
              ),
              fluidRow(id = paste0(vals[i], "_fr_harvest")),
              actionButton(paste0(vals[i], "_harv_add"), "Add harvest"),
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
  
  deleteTabsFromHarvest <- function(typeCrop,index){
    if (typeCrop == "int"){
      removeTab(inputId = paste0("tabpanelHARV", typeCrop), target = paste0("int_harv_",index)) 
      expconHARVmulticrop$var_int <- expconHARVmulticrop$var_int[!expconHARVmulticrop$var_int %in% paste0("int_harv_",index)] 
    }else if (typeCrop == "rel"){
      removeTab(inputId = paste0("tabpanelHARV", typeCrop), target = paste0("rel_harv_",index)) 
      expconHARVmulticrop$var_rel <- expconHARVmulticrop$var_rel[!expconHARVmulticrop$var_rel %in% paste0("rel_harv_",index)]
    }else if (typeCrop == "rot"){
      removeTab(inputId = paste0("tabpanelHARV", typeCrop), target = paste0("rot_harv_",index)) 
      expconHARVmulticrop$var_rot <- expconHARVmulticrop$var_rot[!expconHARVmulticrop$var_rot %in% paste0("rot_harv_",index)]
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
  # rotation HARV crop 1
  expconRotHARVcrop1 <- reactiveValues()
  expconRotHARVcrop1$num <- 0
  expconRotHARVcrop1$DEFAULT <- 1
  expconRotHARVcrop1$ids <- c()
  # rotation HARV crop 2
  expconRotHARVcrop2 <- reactiveValues()
  expconRotHARVcrop2$num <- 0
  expconRotHARVcrop2$DEFAULT <- 1
  expconRotHARVcrop2$ids <- c()
  # rotation HARV crop 3
  expconRotHARVcrop3 <- reactiveValues()
  expconRotHARVcrop3$num <- 0
  expconRotHARVcrop3$DEFAULT <- 1
  expconRotHARVcrop3$ids <- c()
  # rotation HARV crop 4
  expconRotHARVcrop4 <- reactiveValues()
  expconRotHARVcrop4$num <- 0
  expconRotHARVcrop4$DEFAULT <- 1
  expconRotHARVcrop4$ids <- c()
  # rotation HARV crop 5
  expconRotHARVcrop5 <- reactiveValues()
  expconRotHARVcrop5$num <- 0
  expconRotHARVcrop5$DEFAULT <- 1
  expconRotHARVcrop5$ids <- c()
  
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
    if (expconRotHARVcrop1$num >= 1 && crop == "rot_harv_1") { insertRow_HARV(crop = "rot_harv_1", expconRotHARVcrop1$num + 1) }
    if (expconRotHARVcrop2$num >= 1 && crop == "rot_harv_2") { insertRow_HARV(crop = "rot_harv_2", expconRotHARVcrop2$num + 1) }
    if (expconRotHARVcrop3$num >= 1 && crop == "rot_harv_3") { insertRow_HARV(crop = "rot_harv_3", expconRotHARVcrop3$num + 1) }
    if (expconRotHARVcrop4$num >= 1 && crop == "rot_harv_4") { insertRow_HARV(crop = "rot_harv_4", expconRotHARVcrop4$num + 1) }
    if (expconRotHARVcrop5$num >= 1 && crop == "rot_harv_5") { insertRow_HARV(crop = "rot_harv_5", expconRotHARVcrop5$num + 1) }
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
    # rotation HARV crop 1
    if (crop == "rot_harv_1") {
      expconRotHARVcrop1$ids <- c(expconRotHARVcrop1$ids, paste0("rot_harv_1_", index))
      
      insertUI(
        selector = "#rot_harv_1_fr_harvest",
        where = "beforeBegin",
        ui = getHARVUI_GEN(crop, index)
      )
      expconRotHARVcrop1$num <- expconRotHARVcrop1$num + 1
    }
    # rotation HARV crop 2
    if (crop == "rot_harv_2") {
      expconRotHARVcrop2$ids <- c(expconRotHARVcrop2$ids, paste0("rot_harv_2_", index))
      
      insertUI(
        selector = "#rot_harv_2_fr_harvest",
        where = "beforeBegin",
        ui = getHARVUI_GEN(crop, index)
      )
      expconRotHARVcrop2$num <- expconRotHARVcrop2$num + 1
    }
    # rotation HARV crop 3
    if (crop == "rot_harv_3") {
      expconRotHARVcrop3$ids <- c(expconRotHARVcrop3$ids, paste0("rot_harv_3_", index))
      
      insertUI(
        selector = "#rot_harv_3_fr_harvest",
        where = "beforeBegin",
        ui = getHARVUI_GEN(crop, index)
      )
      expconRotHARVcrop3$num <- expconRotHARVcrop3$num + 1
    }
    # rotation HARV crop 4
    if (crop == "rot_harv_4") {
      expconRotHARVcrop4$ids <- c(expconRotHARVcrop4$ids, paste0("rot_harv_4_", index))
      
      insertUI(
        selector = "#rot_harv_4_fr_harvest",
        where = "beforeBegin",
        ui = getHARVUI_GEN(crop, index)
      )
      expconRotHARVcrop4$num <- expconRotHARVcrop4$num + 1
    }
    # rotation HARV crop 5
    if (crop == "rot_harv_5") {
      expconRotHARVcrop5$ids <- c(expconRotHARVcrop5$ids, paste0("rot_harv_5_", index))
      
      insertUI(
        selector = "#rot_harv_5_fr_harvest",
        where = "beforeBegin",
        ui = getHARVUI_GEN(crop, index)
      )
      expconRotHARVcrop5$num <- expconRotHARVcrop5$num + 1
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
                  addon = "none",
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
                  addon = "none",
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
                  addon = "none",
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
                  addon = "none",
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
            textInput(paste0(crop, "_hahd_crop_component_harvested_m2_",index), "Number of m2 units harvested")
          ),
          conditionalPanel(
            paste0("input.", crop, "_hahd_crop_harvestable_area_",index, " == 'Individual plants'"),
            textInput(paste0(crop, "_hahd_crop_component_harvested_ip_",index), "Number of plants harvested")
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
    if (crop == "rot_harv_1") {
      if (length(expconRotHARVcrop1$ids) > 1) {
        removeUI(selector = paste0("#", crop, "_fr_harvest_", index), immediate = T)
        expconRotHARVcrop1$ids <- expconRotHARVcrop1$ids[!expconRotHARVcrop1$ids %in% paste0("rot_harv_1_", index)]
      }
    }
    if (crop == "rot_harv_2") {
      if (length(expconRotHARVcrop2$ids) > 1) {
        removeUI(selector = paste0("#", crop, "_fr_harvest_", index), immediate = T)
        expconRotHARVcrop2$ids <- expconRotHARVcrop2$ids[!expconRotHARVcrop2$ids %in% paste0("rot_harv_2_", index)]
      }
    }
    if (crop == "rot_harv_3") {
      if (length(expconRotHARVcrop3$ids) > 1) {
        removeUI(selector = paste0("#", crop, "_fr_harvest_", index), immediate = T)
        expconRotHARVcrop3$ids <- expconRotHARVcrop3$ids[!expconRotHARVcrop3$ids %in% paste0("rot_harv_3_", index)]
      }
    }
    if (crop == "rot_harv_4") {
      if (length(expconRotHARVcrop4$ids) > 1) {
        removeUI(selector = paste0("#", crop, "_fr_harvest_", index), immediate = T)
        expconRotHARVcrop4$ids <- expconRotHARVcrop4$ids[!expconRotHARVcrop4$ids %in% paste0("rot_harv_4_", index)]
      }
    }
    if (crop == "rot_harv_5") {
      if (length(expconRotHARVcrop4$ids) > 1) {
        removeUI(selector = paste0("#", crop, "_fr_harvest_", index), immediate = T)
        expconRotHARVcrop5$ids <- expconRotHARVcrop5$ids[!expconRotHARVcrop5$ids %in% paste0("rot_harv_5_", index)]
      }
    }
    
  })
  
  ###################### END: HARVEST ######################
  
  ############################### END SERVER: MANAGEMENT PRACTICES ###############################
  ################################################################################################
  
  #######################################################################################################
  ############################### START SERVER: MEASUREMENT AND PHENOLOGY ###############################
  
  ###################### START: GENERAL ######################
  
  # Funcion que oculta tabs
  observe({
    shiny::hideTab(inputId = "tabpanelMEAint", target = "mea_int_default")
    shiny::hideTab(inputId = "tabpanelMEArel", target = "mea_rel_default")
    shiny::hideTab(inputId = "tabpanelMEArot", target = "mea_rot_default")
    
    shiny::hideTab(inputId = "tabpanelPHEint", target = "phe_int_default")
    shiny::hideTab(inputId = "tabpanelPHErel", target = "phe_rel_default")
    shiny::hideTab(inputId = "tabpanelPHErot", target = "phe_rot_default")
  })
  
  # Funcion que agrega los tabs para measurement y phenology por tipo de cultivo
  observeEvent(input$MEA_PHE_BoxMulticropVar,{
    id <- input$MEA_PHE_BoxMulticropVarid
    vars <- unlist(strsplit(id,"_"))
    cropType <- vars[1]
    index <- vars[3]
    
    vals <- getValuesCrop_MEA_PHE(cropType,"MEA")
    
    #Eliminar sesion
    # print("===========================")
    # print(id)
    # print(vals)
    # print("===========================")

    
    insertTabs_MEA_PHE(vals, cropType,"MEA")
    
    vals <- getValuesCrop_MEA_PHE(cropType,"PHE")
    insertTabs_MEA_PHE(vals, cropType,"PHE")
    
    
    #Actualiza el nombre de los tabs según crop
    output[[paste0("title_panel_",cropType,"_mea_",index)]] = renderText({
      input[[id]]
    })
    output[[paste0("title_panel_",cropType,"_phe_",index)]] = renderText({
      input[[id]]
    })
    
    #Elimina el ultimo tab en caso no se seleccione nada
    if(length(vals)==0){
      removeTab(inputId = paste0("tabpanelMEA",cropType), target = paste0(cropType,"_mea_",index))
      removeTab(inputId = paste0("tabpanelPHE",cropType), target = paste0(cropType,"_phe_",index))
      
      if(cropType == "int"){
        mea_phe_multicrop$var_MEA_int <- mea_phe_multicrop$var_MEA_int[!mea_phe_multicrop$var_MEA_int %in% paste0(cropType, "_mea_", index)]
        mea_phe_multicrop$var_PHE_int <- mea_phe_multicrop$var_PHE_int[!mea_phe_multicrop$var_PHE_int %in% paste0(cropType, "_phe_", index)]
      }else if(cropType == "rel"){
        mea_phe_multicrop$var_MEA_rel <- mea_phe_multicrop$var_MEA_rel[!mea_phe_multicrop$var_MEA_rel %in% paste0(cropType, "_mea_", index)]
        mea_phe_multicrop$var_PHE_rel <- mea_phe_multicrop$var_PHE_rel[!mea_phe_multicrop$var_PHE_rel %in% paste0(cropType, "_phe_", index)]
      }else if(cropType == "rot"){
        mea_phe_multicrop$var_MEA_rot <- mea_phe_multicrop$var_MEA_rot[!mea_phe_multicrop$var_MEA_rot %in% paste0(cropType, "_mea_", index)]
        mea_phe_multicrop$var_PHE_rot <- mea_phe_multicrop$var_PHE_rot[!mea_phe_multicrop$var_PHE_rot %in% paste0(cropType, "_phe_", index)]
      }
    }
    
  })
  
  # Variables Reactivas para measurement y phenology por tipo de cultivo
  mea_phe_multicrop <- reactiveValues()
  mea_phe_multicrop$var_MEA_int <- c()
  mea_phe_multicrop$var_MEA_rel <- c()
  mea_phe_multicrop$var_MEA_rot <- c()
  mea_phe_multicrop$var_PHE_int <- c()
  mea_phe_multicrop$var_PHE_rel <- c()
  mea_phe_multicrop$var_PHE_rot <- c()
  
  # Funcion que inserta tabs de Measurement and Phenology
  insertTabs_MEA_PHE <- function(vals, cropType, flag_MEA_PHE) {
    
    aux <- tolower(flag_MEA_PHE)
    if(flag_MEA_PHE == "MEA")
      auxDesc <- "measurement"
    else
      auxDesc <- "phenology"
    
    if (length(vals) != 0) {
      
      if(flag_MEA_PHE == "MEA"){
        
        if(cropType == "int"){
          xx <- mea_phe_multicrop$var_MEA_int[!mea_phe_multicrop$var_MEA_int%in%vals]
          vals <- vals[!vals %in% unique(mea_phe_multicrop$var_MEA_int)]
          mea_phe_multicrop$var_MEA_int <- c(mea_phe_multicrop$var_MEA_int, vals)
        }else if(cropType == "rel"){
          xx <- mea_phe_multicrop$var_MEA_rel[!mea_phe_multicrop$var_MEA_rel%in%vals]
          vals <- vals[!vals %in% unique(mea_phe_multicrop$var_MEA_rel)]
          mea_phe_multicrop$var_MEA_rel <- c(mea_phe_multicrop$var_MEA_rel, vals)
        }else if(cropType == "rot"){
          xx <- mea_phe_multicrop$var_MEA_rot[!mea_phe_multicrop$var_MEA_rot%in%vals]
          vals <- vals[!vals %in% unique(mea_phe_multicrop$var_MEA_rot)]
          mea_phe_multicrop$var_MEA_rot <- c(mea_phe_multicrop$var_MEA_rot, vals)
        }
        
        if (!is.null(xx) && identical(xx, character(0)) == FALSE) {
          for (i in 1:length(xx)) {
            removeTab(inputId = paste0("tabpanel", flag_MEA_PHE, cropType),target = xx[i])
            if (cropType == "int") {
              mea_phe_multicrop$var_MEA_int <- mea_phe_multicrop$var_MEA_int[!mea_phe_multicrop$var_MEA_int %in% xx]
            } else if (cropType == "rel") {
              mea_phe_multicrop$var_MEA_rel <- mea_phe_multicrop$var_MEA_rel[!mea_phe_multicrop$var_MEA_rel %in% xx]
            } else if (cropType == "rot") {
              mea_phe_multicrop$var_MEA_rot <- mea_phe_multicrop$var_MEA_rot[!mea_phe_multicrop$var_MEA_rot %in% xx]
            }
          }
        }
        
      }else if(flag_MEA_PHE == "PHE"){
        if(cropType == "int"){
          xx <- mea_phe_multicrop$var_PHE_int[!mea_phe_multicrop$var_PHE_int%in%vals]
          vals <- vals[!vals %in% unique(mea_phe_multicrop$var_PHE_int)]
          mea_phe_multicrop$var_PHE_int <- c(mea_phe_multicrop$var_PHE_int, vals)
        }else if(cropType == "rel"){
          xx <- mea_phe_multicrop$var_PHE_rel[!mea_phe_multicrop$var_PHE_rel%in%vals]
          vals <- vals[!vals %in% unique(mea_phe_multicrop$var_PHE_rel)]
          mea_phe_multicrop$var_PHE_rel <- c(mea_phe_multicrop$var_PHE_rel, vals)
        }else if(cropType == "rot"){
          xx <- mea_phe_multicrop$var_PHE_rot[!mea_phe_multicrop$var_PHE_rot%in%vals]
          vals <- vals[!vals %in% unique(mea_phe_multicrop$var_PHE_rot)]
          mea_phe_multicrop$var_PHE_rot <- c(mea_phe_multicrop$var_PHE_rot, vals)
        }
        
        if (!is.null(xx) && identical(xx,character(0))==FALSE) {
          for (i in 1:length(xx)) {
            removeTab(inputId = paste0("tabpanel", flag_MEA_PHE, cropType),target = xx[i])
            if (cropType == "int") {
              mea_phe_multicrop$var_PHE_int <- mea_phe_multicrop$var_PHE_int[!mea_phe_multicrop$var_PHE_int %in% xx]
            } else if (cropType == "rel") {
              mea_phe_multicrop$var_PHE_rel <- mea_phe_multicrop$var_PHE_rel[!mea_phe_multicrop$var_PHE_rel %in% xx]
            } else if (cropType == "rot") {
              mea_phe_multicrop$var_PHE_rot <- mea_phe_multicrop$var_PHE_rot[!mea_phe_multicrop$var_PHE_rot %in% xx]
            }
          }
        }
      }
      
      if (length(vals) >= 1) {
        for (i in 1:length(vals)) {
          vars <- unlist(str_split(vals[i],"_"))
          cropValue <- input[[paste0(vars[1],"_cropCommonName_",vars[3])]]
          msm <- get_dcm_values(cmdt, "Measurement", cropValue) 
          
          insertTab(
            inputId = paste0("tabpanel",flag_MEA_PHE,cropType),
            tabPanel(
              title = uiOutput(paste0("title_panel_",vals[i])),
              value = vals[i],
              br(),
              
              if(flag_MEA_PHE == "MEA")
                getDesignUI_HEADER_MEA(vals[i],msm),
              if(flag_MEA_PHE == "PHE")
                getDesignUI_BODY_PHE(vals[i]),
              
              fluidRow(id = paste0(vals[i], "_fr_",auxDesc))
              #insertRow_MEA(typeCrop = vals[i], 1)
            ),
            target = paste0(aux,"_",cropType,"_default"),
            position = "before",
            select = T
          )
        }
      }
    }
  }
  
  # Funcion que obtiene valores para Measurement and Phenology
  getValuesCrop_MEA_PHE <- function(cropType,flag_MEA_PHE) {
    flag_MEA_PHE <- tolower(flag_MEA_PHE)
    
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
      if(val[i] != ""  && !is.na(val[i])){
        val[i] <- paste0(cropType,"_",flag_MEA_PHE,"_", id)
        
      }
      i<-i+1
    }
    
    val <- val[val != ""]
    val
  }
  
  
  # Funcion que elimina tabs de Measurement and Phenology
  deleteTabsFromMeasurementAndPhenology <- function(typeCrop,index){
    if(typeCrop == "int")
    {
      # Measurement
      removeTab(inputId = paste0("tabpanelMEA", typeCrop), target = paste0(typeCrop,"_mea_",index))
      mea_phe_multicrop$var_MEA_int <- mea_phe_multicrop$var_MEA_int[!mea_phe_multicrop$var_MEA_int %in% paste0(typeCrop,"_mea_",index)]
      # Phenology
      removeTab(inputId = paste0("tabpanelPHE", typeCrop), target = paste0(typeCrop,"_phe_",index))
      mea_phe_multicrop$var_PHE_int <- mea_phe_multicrop$var_PHE_int[!mea_phe_multicrop$var_PHE_int %in% paste0(typeCrop,"_phe_",index)]
    }else if(typeCrop == "rel"){
      # Measurement
      removeTab(inputId = paste0("tabpanelMEA", typeCrop), target = paste0(typeCrop,"_mea_",index))
      mea_phe_multicrop$var_MEA_rel <- mea_phe_multicrop$var_MEA_rel[!mea_phe_multicrop$var_MEA_rel %in% paste0(typeCrop,"_mea_",index)]
      # Phenology
      removeTab(inputId = paste0("tabpanelPHE", typeCrop), target = paste0(typeCrop,"_phe_",index))
      mea_phe_multicrop$var_PHE_rel <- mea_phe_multicrop$var_PHE_rel[!mea_phe_multicrop$var_PHE_rel %in% paste0(typeCrop,"_phe_",index)]
    }else if(typeCrop == "rot"){
      # Measurement
      removeTab(inputId = paste0("tabpanelMEA", typeCrop), target = paste0(typeCrop,"_mea_",index))
      mea_phe_multicrop$var_MEA_rot <- mea_phe_multicrop$var_MEA_rot[!mea_phe_multicrop$var_MEA_rot %in% paste0(typeCrop,"_mea_",index)]
      # Phenology
      removeTab(inputId = paste0("tabpanelPHE", typeCrop), target = paste0(typeCrop,"_phe_",index))
      mea_phe_multicrop$var_PHE_rot <- mea_phe_multicrop$var_PHE_rot[!mea_phe_multicrop$var_PHE_rot %in% paste0(typeCrop,"_phe_",index)]
    }
  }
  
  ###################### END: GENERAL ######################
  
  ###################### START: MEASUREMENT ######################
  
  cmdt <- agdesign::dt_cmea
  
  # MEASUREMENT: Asigna variables reactivas
  
  #Monocrop
  meaMONO <- reactiveValues()
  meaMONO$num <- 0
  meaMONO$ids <- c()
  #Inter measurement 1
  meaINT1 <- reactiveValues()
  meaINT1$num <- 0
  meaINT1$ids <- c()
  #Inter measurement 2
  meaINT2 <- reactiveValues()
  meaINT2$num <- 0
  meaINT2$ids <- c()
  #Inter measurement 3
  meaINT3 <- reactiveValues()
  meaINT3$num <- 0
  meaINT3$ids <- c()
  #Inter measurement 4
  meaINT4 <- reactiveValues()
  meaINT4$num <- 0
  meaINT4$ids <- c()
  #Inter measurement 5
  meaINT5 <- reactiveValues()
  meaINT5$num <- 0
  meaINT5$ids <- c()
  #Relay measurement 1
  meaREL1 <- reactiveValues()
  meaREL1$num <- 0
  meaREL1$ids <- c()
  #Relay measurement 2
  meaREL2 <- reactiveValues()
  meaREL2$num <- 0
  meaREL2$ids <- c()
  #Relay measurement 3
  meaREL3 <- reactiveValues()
  meaREL3$num <- 0
  meaREL3$ids <- c()
  #Relay measurement 4
  meaREL4 <- reactiveValues()
  meaREL4$num <- 0
  meaREL4$ids <- c()
  #Relay measurement 5
  meaREL5 <- reactiveValues()
  meaREL5$num <- 0
  meaREL5$ids <- c()
  #Rotation measurement 1
  meaROT1 <- reactiveValues()
  meaROT1$num <- 0
  meaROT1$ids <- c()
  #Rotation measurement 2
  meaROT2 <- reactiveValues()
  meaROT2$num <- 0
  meaROT2$ids <- c()
  #Rotation measurement 3
  meaROT3 <- reactiveValues()
  meaROT3$num <- 0
  meaROT3$ids <- c()
  #Rotation measurement 4
  meaROT4 <- reactiveValues()
  meaROT4$num <- 0
  meaROT4$ids <- c()
  #Relay measurement 5
  meaROT5 <- reactiveValues()
  meaROT5$num <- 0
  meaROT5$ids <- c()
  
  # Funcion que crea el disenno de measurement --> solo Monocrop
  output$uiCropMeaMono <- renderUI({
    cropValue <- input[["cropCommonNameMono"]]
    msm <- get_dcm_values(cmdt, "Measurement",cropValue)
    
    fluidRow(
      column(
        12,
        h2("Crop measurement"),
        # p(class = "text-muted", style="text-align:justify",
        #   paste("Please, select measurement by click.")
        # ),
        fluidRow(
          column(
            6, 
            div(
              style="display: inline-block;vertical-align:top;width:40%",
              selectizeInput(
                "mono_mea_1_search", "", multiple = TRUE,
                options = list(maxItems = 1, placeholder = "Search..."),
                choices = c(msm)
              )
            ),
            div(
              style="display: inline-block;vertical-align:top;margin-top: 20px;",
              shiny::actionButton("mono_mea_1_add", "Add measurement", icon("plus-circle"), class = "btn-primary", style="color: #fff;")
            )
          ),
          column(
            6,
            ""
          )
        ),
        # fluidRow(
        #   column(3,  selectizeInput(
        #     "mono_mea_1_search", "", multiple = TRUE,
        #     options = list(maxItems = 1, placeholder = "Search..."),
        #     choices = c(msm)            
        #   )),
        #   column(9, br(), shiny::actionButton("mono_mea_1_add", "Add measurement", class = "btn-primary", style="color: #fff;"))
        # ),
        fluidRow(id = "mono_mea_fr_measurement")
      )
    )
  })
  
  # Funcion general que agrega filas de measurement
  observeEvent(input$addRow_button_MEA,{
    id <- input$addRow_button_MEAid
    vars <- unlist(str_split(id,"_"))
    index <- vars[3]
    typeCrop <- vars[1]
    cropValue <- NULL
    
    if(typeCrop=="mono"){
      cropValue = input[["cropCommonNameMono"]]
    }else{
      cropValue <- input[[paste0(typeCrop,"_cropCommonName_",index)]]
    }
    
    if (!is.null(input[[paste0(typeCrop,"_mea_",index,"_search")]])){
      insertRow_MEA(typeCrop = paste0(typeCrop,"_mea_", index ) , 1,cropValue)
    }
  })
  
  # Funcion que responde al cambio de Numlevels para las filas de measurement
  observeEvent(input$levelsTiming_MEA,{
    id <- input$levelsTiming_MEAid
    vars <- unlist(str_split(id,"_"))
    numlvls <- input[[input$levelsTiming_MEAid]]
    typeCrop <- vars[1]
    index <- vars[3]
    boxIndex <- vars[5]
    
    timingValue = input[[paste0(typeCrop,"_mea_",index, "_timing_", boxIndex )]]
    
    drawTimingLevelsRow_MEA(timingValue,typeCrop,index,boxIndex,numlvls)
    
  })
  
  # Funcion que responde al cambio de Timing
  observeEvent(input$timing_MEA,{
    id <- input$timing_MEAid
    vars <- unlist(str_split(id,"_"))
    typeCrop <- vars[1]
    index <- vars[3]
    boxIndex <- vars[5]
    timingValue <- input[[id]]
    
    if (length(timingValue)>0 && timingValue == "Date")
    {
      removeUI(
        selector = paste0("#",typeCrop,"_mea_",index, "_timingNumLevels_row_", boxIndex),
        immediate = T
      )
      
      insertUI(
        selector = paste0("#",typeCrop,"_mea_",index, "_timingNumLevels_aux_", boxIndex),
        where = "beforeBegin",
        ui = fluidRow(id=paste0(typeCrop,"_mea_",index, "_timingNumLevels_row_", boxIndex),
                      column(12,
                             selectizeInput(
                               paste0(typeCrop,"_mea_",index, "_timingNumLevels_", boxIndex), "Number of dates", multiple = TRUE,
                               options = list(maxItems = 1,placeholder = "Select one..."),
                               choices = c(1:10), selected = 1
                             )
                      )
        )
      )
    }else{
      removeUI(
        selector = paste0("#",typeCrop,"_mea_",index, "_timingNumLevels_row_", boxIndex),
        immediate = T
      )
    }
    
    drawTimingLevelsRow_MEA(timingValue,typeCrop,index,boxIndex,1)
    
  })
  
  # Funcion que dibuja input lvls para measurement
  drawTimingLevelsRow_MEA <- function(timingValue,typeCrop,index,boxIndex,numlvls){
    
    removeUI(
      selector = paste0("#",typeCrop,"_mea_",index,"_levelTiming_",boxIndex),
      immediate = T
    )
    
    if(length(timingValue)>0){
      insertUI(
        selector = paste0("#",typeCrop,"_mea_",index, "_timing_row_aux_",boxIndex),
        where = "beforeBegin",
        ui = fluidRow(id = paste0(typeCrop,"_mea_",index,"_levelTiming_",boxIndex))
      )
      
      
      for (i in 1:numlvls)
      {
        insertUI(
          selector = paste0("#",typeCrop,"_mea_",index,"_levelTiming_",boxIndex),
          where = "beforeEnd",
          ui = column(12,
                      fluidRow(
                        id = paste0(typeCrop, "_mea_", index, "_timing_row_", boxIndex, "_", i),
                        column(12,
                               
                               if(timingValue == "Date"){
                                 airDatepickerInput(
                                   inputId = paste0(typeCrop,"_mea_",index,"_timingValue_", boxIndex,"_",i),
                                   label = paste0("#",i, " Date"),
                                   dateFormat = "yyyy-mm-dd",
                                   value = Sys.Date(),
                                   placeholder = "yyyy-mm-dd",
                                   clearButton = TRUE,
                                   position = "bottom right", addon = "none",
                                   autoClose = T
                                 )
                               }else if(timingValue == "Frequency")
                               {
                                 textInput(paste0(typeCrop,"_mea_",index,"_timingValue_",boxIndex,"_",i),
                                           label = timingValue)
                               }else if(timingValue == "Other")
                               {
                                 selectizeInput(inputId = paste0(typeCrop,"_timingValue_",index,"_1"),
                                                label = timingValue,
                                                multiple = TRUE,
                                                choices = c(),
                                                options = list(
                                                  maxItems = 20,
                                                  placeholder = "Write..." ,
                                                  'create' = TRUE,
                                                  'persist' = FALSE
                                                )
                                 )
                               }
                               else{
                                 selectizeInput(inputId = paste0(typeCrop,"_timingValue_",index,"_1"),
                                                label = timingValue,
                                                multiple = TRUE,
                                                choices = c(),
                                                options = list(
                                                  maxItems = 20,
                                                  placeholder = "Write..." ,
                                                  'create' = TRUE,
                                                  'persist' = FALSE
                                                )
                                 )
                               }
                        )
                      )
          )
        )
      }
    }
  }
  
  # Funcion que dibuja las filas insertadas de measurement
  insertRow_MEA <- function(typeCrop, index,crop) {
    # monocrop Measurement
    if (typeCrop == "mono_mea_1") {
      meaMONO$num <- meaMONO$num + index
      meaMONO$ids <- c(meaMONO$ids, paste0(typeCrop, "_fluidRow_", meaMONO$num))
      insertUI(
        selector = "#mono_mea_fr_measurement",
        where = "beforeBegin",
        ui = getDesignUI_MEA(typeCrop, meaMONO$num,crop)
      )
    }
    # inter Measurement crop 1
    if (typeCrop == "int_mea_1") {
      meaINT1$num <- meaINT1$num + index
      meaINT1$ids <- c(meaINT1$ids, paste0(typeCrop, "_fluidRow_", meaINT1$num))
      
      insertUI(
        selector = "#int_mea_1_fr_measurement",
        where = "beforeBegin",
        ## Teting ##
        ui = getDesignUI_MEA(typeCrop, meaINT1$num,crop)
      )
    }
    # inter Measurement crop 2
    if (typeCrop == "int_mea_2") {
      meaINT2$num <- meaINT2$num + index
      meaINT2$ids <- c(meaINT2$ids, paste0(typeCrop, "_fluidRow_", meaINT2$num))
      insertUI(
        selector = "#int_mea_2_fr_measurement",
        where = "beforeBegin",
        ui = getDesignUI_MEA(typeCrop, meaINT2$num,crop)
      )
    }
    # inter Measurement crop 3
    if (typeCrop == "int_mea_3") {
      meaINT3$num <- meaINT3$num + index
      meaINT3$ids <- c(meaINT3$ids, paste0(typeCrop, "_fluidRow_", meaINT3$num))
      insertUI(
        selector = "#int_mea_3_fr_measurement",
        where = "beforeBegin",
        ui = getDesignUI_MEA(typeCrop, meaINT3$num,crop)
      )
    }
    # inter Measurement crop 4
    if (typeCrop == "int_mea_4") {
      meaINT4$num <- meaINT4$num + index
      meaINT4$ids <- c(meaINT4$ids, paste0(typeCrop, "_fluidRow_", meaINT4$num))
      insertUI(
        selector = "#int_mea_4_fr_measurement",
        where = "beforeBegin",
        ui = getDesignUI_MEA(typeCrop, meaINT4$num,crop)
      )
    }
    # inter Measurement crop 5
    if (typeCrop == "int_mea_5") {
      meaINT5$num <- meaINT5$num + index
      meaINT5$ids <- c(meaINT5$ids, paste0(typeCrop, "_fluidRow_", meaINT5$num))
      insertUI(
        selector = "#int_mea_5_fr_measurement",
        where = "beforeBegin",
        ui = getDesignUI_MEA(typeCrop, meaINT5$num,crop)
      )
    }
    # relay Measurement crop 1
    if (typeCrop == "rel_mea_1") {
      meaREL1$num <- meaREL1$num + index
      meaREL1$ids <- c(meaREL1$ids, paste0(typeCrop, "_fluidRow_", meaREL1$num))
      
      insertUI(
        selector = "#rel_mea_1_fr_measurement",
        where = "beforeBegin",
        ui = getDesignUI_MEA(typeCrop, meaREL1$num,crop)
      )
    }
    # relay Measurement crop 2
    if (typeCrop == "rel_mea_2") {
      meaREL2$num <- meaREL2$num + index
      meaREL2$ids <- c(meaREL2$ids, paste0(typeCrop, "_fluidRow_", meaREL2$num))
      insertUI(
        selector = "#rel_mea_2_fr_measurement",
        where = "beforeBegin",
        ui = getDesignUI_MEA(typeCrop, meaREL2$num,crop)
      )
      
    }
    # relay Measurement crop 3
    if (typeCrop == "rel_mea_3") {
      meaREL3$num <- meaREL3$num + index
      meaREL3$ids <- c(meaREL3$ids, paste0(typeCrop, "_fluidRow_", meaREL3$num))
      insertUI(
        selector = "#rel_mea_3_fr_measurement",
        where = "beforeBegin",
        ui = getDesignUI_MEA(typeCrop, meaREL3$num,crop)
      )
    }
    # relay Measurement crop 4
    if (typeCrop == "rel_mea_4") {
      meaREL4$num <- meaREL4$num + index
      meaREL4$ids <- c(meaREL4$ids, paste0(typeCrop, "_fluidRow_", meaREL4$num))
      insertUI(
        selector = "#rel_mea_4_fr_measurement",
        where = "beforeBegin",
        ui = getDesignUI_MEA(typeCrop, meaREL4$num,crop)
      )
    }
    # relay Measurement crop 5
    if (typeCrop == "rel_mea_5") {
      meaREL5$num <- meaREL5$num + index
      meaREL5$ids <- c(meaREL5$ids, paste0(typeCrop, "_fluidRow_", meaREL5$num))
      insertUI(
        selector = "#rel_mea_5_fr_measurement",
        where = "beforeBegin",
        ui = getDesignUI_MEA(typeCrop, meaREL5$num,crop)
      )
    }
    # rotation Measurement crop 1
    if (typeCrop == "rot_mea_1") {
      meaROT1$num <- meaROT1$num + index
      meaROT1$ids <- c(meaROT1$ids, paste0(typeCrop, "_fluidRow_", meaROT1$num))
      
      insertUI(
        selector = "#rot_mea_1_fr_measurement",
        where = "beforeBegin",
        ui = getDesignUI_MEA(typeCrop, meaROT1$num,crop)
      )
    }
    # rotation Measurement crop 2
    if (typeCrop == "rot_mea_2") {
      meaROT2$num <- meaROT2$num + index
      meaROT2$ids <- c(meaROT2$ids, paste0(typeCrop, "_fluidRow_", meaROT2$num))
      insertUI(
        selector = "#rot_mea_2_fr_measurement",
        where = "beforeBegin",
        ui = getDesignUI_MEA(typeCrop, meaROT2$num,crop)
      )
      
    }
    # rotation Measurement crop 3
    if (typeCrop == "rot_mea_3") {
      meaROT3$num <- meaROT3$num + index
      meaROT3$ids <- c(meaROT3$ids, paste0(typeCrop, "_fluidRow_", meaROT3$num))
      insertUI(
        selector = "#rot_mea_3_fr_measurement",
        where = "beforeBegin",
        ui = getDesignUI_MEA(typeCrop, meaROT3$num,crop)
      )
    }
    # rotation Measurement crop 4
    if (typeCrop == "rot_mea_4") {
      meaROT4$num <- meaROT4$num + index
      meaROT4$ids <- c(meaROT4$ids, paste0(typeCrop, "_fluidRow_", meaROT4$num))
      insertUI(
        selector = "#rot_mea_4_fr_measurement",
        where = "beforeBegin",
        ui = getDesignUI_MEA(typeCrop, meaROT4$num,crop)
      )
    }
    # rotation Measurement crop 5
    if (typeCrop == "rot_mea_5") {
      meaROT5$num <- meaROT5$num + index
      meaROT5$ids <- c(meaROT5$ids, paste0(typeCrop, "_fluidRow_", meaROT5$num))
      insertUI(
        selector = "#rot_mea_5_fr_measurement",
        where = "beforeBegin",
        ui = getDesignUI_MEA(typeCrop, meaROT5$num,crop)
      )
    }
  }
  
  # Funcion que tiene el disenno del cuerpo para measurement
  getDesignUI_MEA <- function(typeCrop, index, crop) {
    parmea <- get_dcm_values(cmdt, "Subgroup",crop)
    unit<- get_dcm_values(cmdt, "TraitUnit",crop)
    timing<- get_dcm_values(cmdt, "Timing",crop)

    fluidRow(id = paste0(typeCrop, "_fluidRow_", index),
             box(
               solidHeader = TRUE,
               status = "warning",
               width = 12,
               fluidRow(
                 #id = paste0(typeCrop, "_fluidRow_", index),
                 column(
                   2,
                   disabled(textInput(paste0(typeCrop, "_measurement_", index), "Measurement", value = input[[paste0(typeCrop,"_search")]]))
                 ),
                 column(
                   2,
                   selectizeInput(
                     paste0(typeCrop, "_parmea_", index), "Parameter measured", multiple = TRUE,
                     options = list(maxItems = 1, placeholder = "Select one..."),
                     choices = c(parmea[[1]]), selected = c(parmea[[1]])[1]            
                   )
                 ),
                 column(
                   2,
                   selectizeInput(
                     paste0(typeCrop, "_unit_", index), "Unit", multiple = TRUE,
                     options = list(maxItems = 1, placeholder = "Select one..."),selected = "g",
                     choices = c(unit)            
                   )
                 ),
                 column(
                   1,
                   textInput(paste0(typeCrop, "_per_season_", index), "Per season", value = "1" )
                 ),
                 column(
                   1,
                   textInput(paste0(typeCrop, "_per_plot_", index), "Per plot", value = "1")
                 ),
                 column(
                   2,
                   selectizeInput(
                     paste0(typeCrop, "_timing_", index), "Timing", multiple = TRUE,
                     options = list(maxItems = 1, placeholder = "Select one..."),
                     choices = c(timing), selected = "Days after planting"
                   ),
                   fluidRow(
                     column(
                       6
                     ),
                     column(
                       6,
                       # Auxiliar row to draw date pickers
                       fluidRow(id=paste0(typeCrop, "_timingNumLevels_aux_", index))
                     )
                   )
                 ),
                 column(
                   2,
                   fluidRow(
                     column(
                       8,
                       fluidRow(
                         id=paste0(typeCrop,"_levelTiming_",index),
                         column(12,
                                fluidRow(
                                  id=paste0(typeCrop,"_timing_row_",index,"_1"),
                                  column(12,
                                         selectizeInput(inputId = paste0(typeCrop,"_timingValue_",index,"_1"),
                                                        label = "Days after planting",
                                                        multiple = TRUE,
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
                       ),
                       fluidRow(id = paste0(typeCrop, "_timing_row_aux_",index)) #References function drawTimingLevelsRow_MEA()
                     ),
                     column(
                       4,
                       br(),
                       actionButton(paste0(typeCrop, "_closeBox_mea_", index), "", icon("close"))
                     )
                   )
                 )
               )
             )
    )
  }
  
  # Funcion que tiene el disenno de la cabecera para measurement
  getDesignUI_HEADER_MEA <- function(vals,msm){
    
    fluidRow(
      column(
        6, 
        div(
          style="display: inline-block;vertical-align:top;width:40%",
          selectizeInput(
            paste0(vals,"_search"), "", multiple = TRUE,
            options = list(maxItems = 1, placeholder = "Search measurement..."),
            choices = c(msm)
          )
        ),
        div(
          style="display: inline-block;vertical-align:top;margin-top: 20px;",
          shiny::actionButton(paste0(vals,"_add"), "Add measurement", icon("plus-circle"), class = "btn-primary", style="color: #fff;")
        )
      ),
      column(
        6,
        ""
      ),
      fluidRow(id = paste0(vals,"_row"))
    )
    # fluidRow(
    #   column(
    #     12,
    #     fluidRow(
    #       column(3,selectizeInput(
    #         paste0(vals,"_search"), "", multiple = TRUE,
    #         options = list(maxItems = 1, placeholder = "Search measurement ..."),
    #         choices = c(msm)            
    #       )
    #       ),
    #       column(9, br(), shiny::actionButton(paste0(vals,"_add"), "Add measurement", class = "btn-primary", style="color: #fff;"))
    #     ),
    #     #br(),
    #     # fluidRow(
    #     #   column(2, h4("Measurement", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;")),
    #     #   column(2, h4("Parameter measured", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;")),
    #     #   column(2, h4("Trait unit", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;")),
    #     #   column(1, h4("Per season", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;")),
    #     #   column(1, h4("Per plot", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;")),
    #     #   column(2, h4("Timing", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;")),
    #     #   column(1, h4("Timing value", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;")),
    #     #   column(1, "")
    #     # ),
    #     fluidRow(id = paste0(vals,"_row"))
    #   )
    # )
  }
  
  # Funcion GENERAL que activa "Close"
  observeEvent(input$closeBox_button_MEA, {
    vars <- unlist(strsplit(input$closeBox_button_MEAid, "_"))
    typeCrop <- paste0(vars[1], "_", vars[2],"_",vars[3])
    index <- vars[6]
    
    # MEA-MONO
    if(typeCrop == "mono_mea_1"){
      removeUI(selector = paste0("#", typeCrop, "_fluidRow_", index), immediate = T)
      meaMONO$ids <- meaMONO$ids[!meaMONO$ids %in% paste0(typeCrop, "_fluidRow_", index)]
    }
    
    # MEA-INT
    if (typeCrop == "int_mea_1") {
      removeUI(selector = paste0("#", typeCrop, "_fluidRow_", index), immediate = T)
      meaINT1$ids <- meaINT1$ids[!meaINT1$ids %in% paste0(typeCrop, "_fluidRow_", index)]
    }
    if (typeCrop == "int_mea_2") {
      removeUI(selector = paste0("#", typeCrop, "_fluidRow_", index), immediate = T)
      meaINT2$ids <- meaINT2$ids[!meaINT2$ids %in% paste0(typeCrop, "_fluidRow_", index)]
    }
    if (typeCrop == "int_mea_3") {
      removeUI(selector = paste0("#", typeCrop, "_fluidRow_", index), immediate = T)
      meaINT3$ids <- meaINT3$ids[!meaINT3$ids %in% paste0(typeCrop, "_fluidRow_", index)]
    }
    if (typeCrop == "int_mea_4") {
      removeUI(selector = paste0("#", typeCrop, "_fluidRow_", index), immediate = T)
      meaINT4$ids <- meaINT4$ids[!meaINT4$ids %in% paste0(typeCrop, "_fluidRow_", index)]
    }
    if (typeCrop == "int_mea_5") {
      removeUI(selector = paste0("#", typeCrop, "_fluidRow_", index), immediate = T)
      meaINT5$ids <- meaINT5$ids[!meaINT5$ids %in% paste0(typeCrop, "_fluidRow_", index)]
    }
    
    # MEA-REL
    if (typeCrop == "rel_mea_1") {
      removeUI(selector = paste0("#", typeCrop, "_fluidRow_", index), immediate = T)
      meaREL1$ids <- meaREL1$ids[!meaREL1$ids %in% paste0(typeCrop, "_fluidRow_", index)]
    }
    if (typeCrop == "rel_mea_2") {
      removeUI(selector = paste0("#", typeCrop, "_fluidRow_", index), immediate = T)
      meaREL2$ids <- meaREL2$ids[!meaREL2$ids %in% paste0(typeCrop, "_fluidRow_", index)]
    }
    if (typeCrop == "rel_mea_3") {
      removeUI(selector = paste0("#", typeCrop, "_fluidRow_", index), immediate = T)
      meaREL3$ids <- meaREL3$ids[!meaREL3$ids %in% paste0(typeCrop, "_fluidRow_", index)]
    }
    if (typeCrop == "rel_mea_4") {
      removeUI(selector = paste0("#", typeCrop, "_fluidRow_", index), immediate = T)
      meaREL4$ids <- meaREL4$ids[!meaREL4$ids %in% paste0(typeCrop, "_fluidRow_", index)]
    }
    if (typeCrop == "rel_mea_5") {
      removeUI(selector = paste0("#", typeCrop, "_fluidRow_", index), immediate = T)
      meaREL5$ids <- meaREL5$ids[!meaREL5$ids %in% paste0(typeCrop, "_fluidRow_", index)]
    }
    
    # MEA-ROT
    if (typeCrop == "rot_mea_1") {
      removeUI(selector = paste0("#", typeCrop, "_fluidRow_", index), immediate = T)
      meaROT1$ids <- meaROT1$ids[!meaROT1$ids %in% paste0(typeCrop, "_fluidRow_", index)]
    }
    if (typeCrop == "rot_mea_2") {
      removeUI(selector = paste0("#", typeCrop, "_fluidRow_", index), immediate = T)
      meaROT2$ids <- meaROT2$ids[!meaROT2$ids %in% paste0(typeCrop, "_fluidRow_", index)]
    }
    if (typeCrop == "rot_mea_3") {
      removeUI(selector = paste0("#", typeCrop, "_fluidRow_", index), immediate = T)
      meaROT3$ids <- meaROT3$ids[!meaROT3$ids %in% paste0(typeCrop, "_fluidRow_", index)]
    }
    if (typeCrop == "rot_mea_4") {
      removeUI(selector = paste0("#", typeCrop, "_fluidRow_", index), immediate = T)
      meaROT4$ids <- meaROT4$ids[!meaROT4$ids %in% paste0(typeCrop, "_fluidRow_", index)]
    }
    if (typeCrop == "rot_mea_5") {
      removeUI(selector = paste0("#", typeCrop, "_fluidRow_", index), immediate = T)
      meaROT5$ids <- meaROT5$ids[!meaROT5$ids %in% paste0(typeCrop, "_fluidRow_", index)]
    }
    
  })
  
  ###################### END: MEASUREMENT ######################
  
  ###################### START: PHENOLOGY ######################
  
  # Funcion que crea el disenno de measurement --> solo Monocrop
  pheno_vars <- agdesign::dt_cphe
  
  output$uiCropPheMono <- renderUI({
    fluidRow(
      column(
        width = 12,
        h2("Crop Phenology"),
        p(
          class = "text-muted",
          style = "text-align:justify",
          paste("Please, select phenology by click.")
        ),
        DTOutput("tblPhe_mono_mea_1")
      )
    )
  })
  
  output$tblPhe_mono_mea_1 <- renderDT({
    cropMono <- input[["cropCommonNameMono"]]
    if(length(cropMono)>0){
      datatable(
        #dtInterPhe <<- finterphe("Cassava"),
        pheno_vars,
        selection = 'multiple',
        #editable = TRUE,
        options = list(
          pageLength = 25,
          columnDefs = list(list(visible=F, targets=c(1,2,3,5,6,8,9,10,11,12,13,14,15,16,17)))
        )
      )
    }
  })
  
  # Funcion que crea el disenno del body para Phenology
  getDesignUI_BODY_PHE <- function(vals){
    fluidRow(
      column(
        12,
        DTOutput(paste0("tblPhe_",vals)), ##Duplicated ID generated
        delay(100,createDataTableTabsPhenology(vals))
        
      )
    )
    
  }
  
  # Funcion que crea el datatable para phenology
  createDataTableTabsPhenology <- function(vals){
    output[[paste0("tblPhe_",vals)]] <- renderDT(
      #dtInterPhe <<- finterphe("Cassava"),
      pheno_vars,
      selection = 'multiple',
      #editable = TRUE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible=F, targets=c(1,2,3,5,6,8,9,10,11,12,13,14,15,16,17)))
      )
    )
  }
  
  ###################### END: PHENOLOGY ######################
  
  ############################### END SERVER: MEASUREMENT AND PHENOLOGY ###############################
  #####################################################################################################
  
  #####################################################################################
  ############################### START SERVER: WEATHER ###############################
  
  wea_cmdt <- agdesign::dt_cmea
  weatherdt <- agdesign::dt_weather
  
  weatherVars <- reactiveValues()
  weatherVars$num <- 1
  weatherVars$ids <- c()
  
  # Funcion que crea el disenno de Weather
  output$uiWeather <- renderUI({
    #cropValue <- input[["cropCommonNameMono"]]
    msm <- get_dweather_values(weatherdt, "Measurement") 
    
    fluidRow(
      column(
        width = 12,
        h2("Weather details"),
        fluidRow(
          column(6,  
                 div(
                   style="display: inline-block;vertical-align:top;width:40%",
                   selectizeInput(
                     "weather_search", "", multiple = TRUE,
                     options = list(maxItems = 1, placeholder = "Search..."),
                     choices = c(msm)            
                   )
                 ),
                 div(
                   style="display: inline-block;vertical-align:top;margin-top: 20px;",
                   shiny::actionButton("btn_weather_add", "Add measurement",  icon("plus-circle"), class = "btn-primary", style="color: #fff;")
                 )
          ),
          column(6,"")
        ),
        # fluidRow(
        #   column(3, h4("Measurement", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;")),
        #   column(2, h4("Unit", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;")),
        #   column(2, h4("Samples Per season", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;")),
        #   column(2, h4("Timing", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;")),
        #   column(2, h4("", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;"))
        # ),
        fluidRow(id = "weather_fr")
      )
    )
    
  })
  
  # Funcion general que agrega filas de weather
  observeEvent(input$addRow_button_WEA,{
    id <- input$addRow_button_WEAid
    weatherValue <- input[["weather_search"]]
    index <- weatherVars$num 
    timing<- get_dcm_values(wea_cmdt, "Timing","")
    
    if (!is.null(weatherValue)){
      insertRow_WEA(index, timing)
    } 
  })
  
  # Funcion que activa close de weather
  observeEvent(input$closeBox_WEA,{
    id <- input$closeBox_WEAid
    vars <- unlist(str_split(id,"_"))
    index <- vars[3]
    removeUI(selector = paste0("#", "weather_fluidRow_", index), immediate = T)
    
    weatherVars$ids <- weatherVars$ids[!weatherVars$ids %in% paste0("weather_",index)]
    
  })
  
  # Funcion que se activa al cambiar num levels de timing
  observeEvent(input$timingNumLevels_WEA,{
    id <- input$timingNumLevels_WEAid
    vars <- unlist(str_split(id,"_"))
    numlvls <- input[[input$timingNumLevels_WEAid]]
    index <- vars[3]
    
    timingValue = input[[paste0("weather_timing_", index )]]
    
    drawTimingLevelsRow_WEA(timingValue,index,numlvls)
    
  })
  
  # Funcion que se activa al cambiar tipo de Timing
  observeEvent(input$timing_WEA,{
    id <- input$timing_WEAid
    
    #Eliminar
    print(id)
    
    vars <- unlist(str_split(id,"_"))
    index <- vars[3]
    timingValue <- input[[id]]
    
    if (length(timingValue)>0 && timingValue == "Date")
    {
      removeUI(
        selector = paste0("#","weather_timingNumLevels_row_", index),
        immediate = T
      )
      
      insertUI(
        selector = paste0("#","weather_timingNumLevels_aux_", index),
        where = "beforeBegin",
        ui = fluidRow(id=paste0("weather_timingNumLevels_row_", index),
                      column(12,
                             selectizeInput(
                               paste0("weather_timingNumLevels_", index), "Number of dates", multiple = TRUE,
                               options = list(maxItems = 1,placeholder = "Select one..."),
                               choices = c(1:10), selected = 1
                             )
                      )
        )
      )
    }else{
      removeUI(
        selector = paste0("#","weather_timingNumLevels_row_", index),
        immediate = T
      )
    }
    
    drawTimingLevelsRow_WEA(timingValue,index,1)
  })
  
  # Funcion que dibuja input lvls para measurement
  drawTimingLevelsRow_WEA <- function(timingValue,index,numlvls){
    
    removeUI(
      selector = paste0("#","weather_levelTiming_",index),
      immediate = T
    )
    
    
    if(length(timingValue)>0){
      insertUI(
        selector = paste0("#","weather_timing_row_aux_",index),
        where = "beforeBegin",
        ui = fluidRow(id = paste0("weather_levelTiming_",index))
      )
      
      
      for (i in 1:numlvls)
      {
        insertUI(
          selector = paste0("#","weather_levelTiming_",index),
          where = "beforeEnd",
          ui = column(12,
                      fluidRow(
                        id = paste0("weather_timing_row_", index, "_", i),
                        column(12,
                               
                               if(timingValue == "Date"){
                                 airDatepickerInput(
                                   inputId = paste0("weather_timingValue_",index,"_",i),
                                   label = paste0("#",i, " Date"),
                                   dateFormat = "yyyy-mm-dd",
                                   value = Sys.Date(),
                                   placeholder = "yyyy-mm-dd",
                                   clearButton = TRUE,
                                   position = "bottom right", addon = "none",
                                   autoClose = T
                                 )
                               }else if(timingValue == "Frequency")
                               {
                                 textInput(paste0("weather_timingValue_",index,"_",i),
                                           label = timingValue)
                               }else if(timingValue == "Other")
                               {
                                 selectizeInput(inputId = paste0("weather_timingValue_",index,"_1"),
                                                label = timingValue,
                                                multiple = TRUE,
                                                choices = c(),
                                                options = list(
                                                  maxItems = 20,
                                                  placeholder = "Write..." ,
                                                  'create' = TRUE,
                                                  'persist' = FALSE
                                                )
                                 )
                               }
                               else{
                                 selectizeInput(inputId = paste0("weather_timingValue_",index,"_1"),
                                                label = timingValue,
                                                multiple = TRUE,
                                                choices = c(),
                                                options = list(
                                                  maxItems = 20,
                                                  placeholder = "Write..." ,
                                                  'create' = TRUE,
                                                  'persist' = FALSE
                                                )
                                 )
                               }
                        )
                      )
          )
        )
      }
    }
  }
  
  # Funcion que dibuja las filas insertadas de weather
  insertRow_WEA <- function(index,timing) {
    weatherVars$ids <- c(weatherVars$ids,paste0("weather_",index))
    insertUI(
      selector = "#weather_fr",
      where = "beforeBegin",
      ui = getDesignUI_WEA(index, timing)
    )
    weatherVars$num <- weatherVars$num + 1
  }
  
  # Funcion que tiene el disenno para las filas de weather
  getDesignUI_WEA <- function(index,timing) {
    weather_measurement <- input[["weather_search"]]
    unit<- get_dweather_values(weatherdt, "TraitUnit",weather_measurement)
    
    # parmea <- get_dweather_values(cmdt, "Subgroup",crop)
    # timing<- get_dweather_values(cmdt, "Timing",crop)
    
    fluidRow(id = paste0("weather_fluidRow_", index),
             box(
               solidHeader = TRUE,
               status = "warning",
               width = 12,
               column(
                 3,
                 disabled(textInput(paste0("weather_mea_", index), "Measurement", value = input[["weather_search"]]))
               ),
               column(
                 2,
                 selectizeInput(
                   paste0("weather_unit_", index), "Unit", multiple = TRUE,
                   options = list(maxItems = 1, placeholder = "Select one..."),
                   choices = c(unit), selected = unit[1]           
                 )
               ),
               column(
                 2,
                 textInput(paste0("weather_per_season_", index), "Samples Per Season", value = "1")
               ),
               column(
                 2,
                 selectizeInput(
                   paste0("weather_timing_", index), "Timing", multiple = TRUE,
                   options = list(maxItems = 1, placeholder = "Select one..."),
                   choices = c(timing), 
                   selected = "Days after planting"
                 ),
                 fluidRow(
                   column(
                     6
                   ),
                   column(
                     6,
                     # Auxiliar row to draw date pickers
                     fluidRow(id=paste0("weather_timingNumLevels_aux_", index))
                   )
                 )
               ),
               column(
                 2,
                 fluidRow(
                   id=paste0("weather_levelTiming_",index),
                   column(12,
                          fluidRow(
                            id=paste0("weather_timing_row_",index,"_1"),
                            column(12,
                                   selectizeInput(inputId = paste0("weather_timingValue_",index,"_1"),
                                                  label = "Days after planting",
                                                  multiple = TRUE,
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
                 ),
                 fluidRow(id = paste0("weather_timing_row_aux_",index))
               ),
               column(
                 1,
                 br(),
                 actionButton(paste0("weather_closeBox_", index), "", icon("close"))
               )
             )
    )
  }
  
  # Funcion que trae valores para weather desde el RDS
  get_dweather_values <- function(data_dictionary=NULL, attribute = "Measurement", value=NULL){
    
    if(!is.null(data_dictionary)){
      if(attribute == "Measurement"){
        out <- data_dictionary %>% select_(attribute)
        out <- sort(unique(na.omit(out[[1]])),decreasing = F)
      }else if(attribute == "TraitUnit"){
        out <- data_dictionary %>% filter(Measurement==value) %>% select_(attribute)
        out <- unique(na.omit(out[[1]]))
        
        out <- sort(unlist(strsplit(out,"\\|")),decreasing = F) #Regular expression we use \\
      }
      
    }else
    {
      out <- ""
    }
    
    out
  }
  
  ############################### END SERVER: WEATHER ###############################
  ###################################################################################
  
  #####################################################################################
  ############################### START SERVER: SOIL ###############################
  
  soil_cmdt <- agdesign::dt_cmea
  soildt<- agdesign::dt_soil
  soilVars <- reactiveValues()
  soilVars$num <- 1
  soilVars$ids <- c()
  
  output$uisoil <- renderUI({})
  outputOptions(output, "uisoil", suspendWhenHidden = FALSE)
  
  # Funcion que crea el disenno de soil
  output$uisoil <- renderUI({
    #cropValue <- input[["cropCommonNameMono"]]
    msm <- get_dsoil_values(soildt, "Measurement") 
    
    fluidRow(
      column(
        width = 12,
        h2("Soil details"),
        fluidRow(
          column(6,  
                 div(
                   style="display: inline-block;vertical-align:top;width:40%",
                   selectizeInput(
                     "soil_search", "", multiple = TRUE,
                     options = list(maxItems = 1, placeholder = "Search..."),
                     choices = c(msm)            
                   )
                 ),
                 div(
                   style="display: inline-block;vertical-align:top;margin-top: 20px;",
                   shiny::actionButton("btn_soil_add", "Add measurement",  icon("plus-circle"), class = "btn-primary", style="color: #fff;")
                 )
          ),
          column(6,"")
        ),
        # fluidRow(
        #   column(3, h4("Measurement", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;")),
        #   column(2, h4("Unit", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;")),
        #   column(2, h4("Samples Per season", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;")),
        #   column(2, h4("Timing", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;")),
        #   column(2, h4("", style="font-weight: 600;color: #555;font-size: 16px;text-align: center;"))
        # ),
        fluidRow(id = "soil_fr")
      )
    )
    
  })
  
  
  measurementSoilId <- eventReactive( input$measurement_SOIL, {
    input$measurement_SOILid
  })
  
  
  # Funcion que actualiza el combobox segun measurement
  observeEvent(input$measurement_SOIL,{
    id <- input$measurement_SOILid
    
    
    #Eliminar sesion
    
    print(id)
    
    id2 <- measurementSoilId()
    
    print(id2)
    
    
    measurement <- input[[input$measurement_SOILid]]
    vars <- unlist(str_split(id,"_"))
    updateSelectizeInput(session,
                         inputId = paste0("soil_unit_",vars[3]),
                         choices = get_dsoil_values(soildt, "TraitUnit",measurement))
  })
  
  
  # Funcion general que agrega filas de soil
  observeEvent(input$addRow_button_SOIL,{
    id <- input$addRow_button_SOILid
    soil_measurement <- input[["soil_search"]]
    index <- soilVars$num 
    timing<- get_dcm_values(soil_cmdt, "Timing","")
    unit<- get_dsoil_values(soildt, "TraitUnit",soil_measurement)
    
    if(!is.null(soil_measurement)){
      insertRow_SOIL(index,timing,soil_measurement,unit)
    }
    
  })
  
  # Funcion que elimina los boxes
  observeEvent(input$closeBox_SOIL,{
    id <- input$closeBox_SOILid
    vars <- unlist(str_split(id,"_"))
    index <- vars[3]
    
    removeUI(selector = paste0("#", "soil_fluidRow_", index), immediate = T)
    
    soilVars$ids <- soilVars$ids[!soilVars$ids %in% paste0("soil_",index)]
  })
  
  # Funcion que se activa al cambiar num levels de timing
  observeEvent(input$timingNumLevels_SOIL,{
    id <- input$timingNumLevels_SOILid
    vars <- unlist(str_split(id,"_"))
    numlvls <- input[[input$timingNumLevels_SOILid]]
    index <- vars[3]
    
    timingValue = input[[paste0("soil_timing_", index )]]
    
    drawTimingLevelsRow_SOIL(timingValue,index,numlvls)
    
  })
  
  # Funcion que se activa al cambiar tipo de Timing
  observeEvent(input$timing_SOIL,{
    id <- input$timing_SOILid
    
    vars <- unlist(str_split(id,"_"))
    index <- vars[3]
    timingValue <- input[[id]]
    
    if (length(timingValue)>0 && timingValue == "Date")
    {
      removeUI(
        selector = paste0("#","soil_timingNumLevels_row_", index),
        immediate = T
      )
      
      insertUI(
        selector = paste0("#","soil_timingNumLevels_aux_", index),
        where = "beforeBegin",
        ui = fluidRow(id=paste0("soil_timingNumLevels_row_", index),
                      column(12,
                             selectizeInput(
                               paste0("soil_timingNumLevels_", index), "Number of dates", multiple = TRUE,
                               options = list(maxItems = 1,placeholder = "Select one..."),
                               choices = c(1:10), selected = 1
                             )
                      )
        )
      )
    }else{
      removeUI(
        selector = paste0("#","soil_timingNumLevels_row_", index),
        immediate = T
      )
    }
    
    drawTimingLevelsRow_SOIL(timingValue,index,1)
  })
  
  # Funcion que dibuja input lvls para measurement
  drawTimingLevelsRow_SOIL <- function(timingValue,index,numlvls){
    
    removeUI(
      selector = paste0("#","soil_levelTiming_",index),
      immediate = T
    )
    
    
    if(length(timingValue)>0){
      insertUI(
        selector = paste0("#","soil_timing_row_aux_",index),
        where = "beforeBegin",
        ui = fluidRow(id = paste0("soil_levelTiming_",index))
      )
      
      
      for (i in 1:numlvls)
      {
        insertUI(
          selector = paste0("#","soil_levelTiming_",index),
          where = "beforeEnd",
          ui = column(12,
                      fluidRow(
                        id = paste0("soil_timing_row_", index, "_", i),
                        column(12,
                               
                               if(timingValue == "Date"){
                                 airDatepickerInput(
                                   inputId = paste0("soil_timingValue_",index,"_",i),
                                   label = paste0("#",i, " Date"),
                                   dateFormat = "yyyy-mm-dd",
                                   value = Sys.Date(),
                                   placeholder = "yyyy-mm-dd",
                                   clearButton = TRUE,
                                   position = "bottom right", addon = "none",
                                   autoClose = T
                                 )
                               }else if(timingValue == "Frequency")
                               {
                                 textInput(paste0("soil_timingValue_",index,"_",i),
                                           label = timingValue)
                               }else if(timingValue == "Other")
                               {
                                 selectizeInput(inputId = paste0("soil_timingValue_",index,"_1"),
                                                label = timingValue,
                                                multiple = TRUE,
                                                choices = c(),
                                                options = list(
                                                  maxItems = 20,
                                                  placeholder = "Write..." ,
                                                  'create' = TRUE,
                                                  'persist' = FALSE
                                                )
                                 )
                               }
                               else{
                                 selectizeInput(inputId = paste0("soil_timingValue_",index,"_1"),
                                                label = timingValue,
                                                multiple = TRUE,
                                                choices = c(),
                                                options = list(
                                                  maxItems = 20,
                                                  placeholder = "Write..." ,
                                                  'create' = TRUE,
                                                  'persist' = FALSE
                                                )
                                 )
                               }
                        )
                      )
          )
        )
      }
    }
  }
  
  # Funcion que dibuja las filas insertadas de soil
  insertRow_SOIL <- function(index, timing, soil_measurement, unit) {
    soilVars$ids <- c(soilVars$ids,paste0("soil_",index))
    
    insertUI(
      selector = "#soil_fr",
      where = "beforeBegin",
      ui = getDesignUI_SOIL(index,timing, soil_measurement, unit)
    )
    soilVars$num <- soilVars$num + 1
  }
  
  # Funcion que tiene el disenno para las filas de soil
  getDesignUI_SOIL <- function(index, timing, soil_measurement, unit) {
    
    # parmea <- get_dsoil_values(cmdt, "Subgroup",crop)
    # timing<- get_dsoil_values(cmdt, "Timing",crop)
    
    fluidRow(id = paste0("soil_fluidRow_", index),
             box(
               solidHeader = TRUE,
               status = "warning",
               width = 12,
               column(
                 2,
                 disabled(textInput(paste0("soil_mea_", index), "Measurement", value = soil_measurement))
               ),
               column(
                 1,
                 selectizeInput(
                   paste0("soil_unit_", index), "Unit", multiple = TRUE,
                   options = list(maxItems = 1, placeholder = "Select one..."),
                   choices = get_dsoil_values(soildt, "TraitUnit",soil_measurement), selected = get_dsoil_values(soildt, "TraitUnit",soil_measurement)[1]           
                 )
               ),
               column(
                 2,
                 selectizeInput(
                   paste0("soil_depth_", index), "Depth", multiple = TRUE,
                   options = list(maxItems = 20, placeholder = "Write...",'create' = TRUE,'persist' = FALSE),
                   choices = c()         
                 )
               ),
               column(
                 1,
                 selectizeInput(
                   paste0("soil_depthunit_", index), "Depth Unit", multiple = TRUE,
                   options = list(maxItems = 1, placeholder = "Select one..."),
                   choices = c("mm","cm","m"), selected = "mm"       
                 )
               ),
               column(
                 1,
                 textInput(paste0("soil_per_season_", index), "Samples Per Season", value = "1")
               ),
               column(
                 1,
                 textInput(paste0("soil_per_plot_", index), "Samples Per Plot", value = "1")
               ),
               column(
                 1,
                 selectizeInput(
                   paste0("soil_timing_", index), "Timing", multiple = TRUE,
                   options = list(maxItems = 1, placeholder = "Select one..."),
                   choices = c(timing), 
                   selected = "Days after planting"
                 ),
                 fluidRow(
                   column(
                     6
                   ),
                   column(
                     6,
                     # Auxiliar row to draw date pickers
                     fluidRow(id=paste0("soil_timingNumLevels_aux_", index))
                   )
                 )
               ),
               column(
                 2,
                 fluidRow(
                   id=paste0("soil_levelTiming_",index),
                   column(12,
                          fluidRow(
                            id=paste0("soil_timing_row_",index,"_1"),
                            column(12,
                                   selectizeInput(inputId = paste0("soil_timingValue_",index,"_1"),
                                                  label = "Days after planting",
                                                  multiple = TRUE,
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
                 ),
                 fluidRow(id = paste0("soil_timing_row_aux_",index))
               ),
               column(
                 1,
                 br(),
                 actionButton(paste0("soil_closeBox_", index), "", icon("close"))
               )
             )
    )
  }
  
  # Funcion que trae valores para soil desde el RDS
  get_dsoil_values <- function(data_dictionary=NULL, attribute = "Measurement",value=NULL){
    if(!is.null(data_dictionary)){
      
      if(attribute == "Measurement"){
        out <- data_dictionary %>% select_(attribute)
        out <- sort(unique(na.omit(out[[1]])),decreasing = F)
      }else if(attribute == "TraitUnit")
      {
        out <- data_dictionary %>% filter(Measurement==value) %>% select_(attribute)
        out <- unique(na.omit(out[[1]]))
        out <- sort(unlist(strsplit(out,"\\|")),decreasing = F) #Regular expression we use \\
      }
      
    }
    else{
      out <- ""
    }
    
    out
  }
  
  ############################### END SERVER: SOIL ###############################
  ###################################################################################
  
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
  
  
  
  
  ###########
  ###########
  ########### START: CODIGO DE OMAR PARA GENERAR LIBRO DE CAMPO ###########
  ###########
  ###########
  
  
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
  
  ############################### START: EXPERIMENT CONDITIONS #######################################
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
  
  #' Residue management ###############################################################
  dt_residual<- reactive({
    
    #ai<- readRDS("/home/obenites/AGROFIMS/agdesign/inst/table_ids.rds")
    #input<-readRDS("/home/obenites/AGROFIMS/agdesign/inst/inputs.rds")
    
    if(isTRUE(input$residueDesc_checkbox)){
      dt1 <- get_ec_resdesc(input=input)$dt         
    }
    else {
      dt1 <- data.frame()
    }
    
    if(isTRUE(input$residueManag_checkbox)){
      dt2 <- get_ec_resmgt(input=input)$dt 
    } 
    else{
      dt2 <- data.frame()
    }
    dt <- smart_colbind(dt1,dt2) #column bind of two sub tabs (description and management)
    if(nrow(fbdesign())==0 &&  length(dt)>0){
      dt <- dt
    } 
    else if(nrow(fbdesign())>0 &&  length(dt)>0 ) {
      dt <- cbind(fbdesign(), dt)
    } 
    else{
      dt<- data.frame()
    }
    dt
  })
  dt_prot_residual<-reactive({
    
    #read data
    kds_resmgt<- magmtprac$resmgt
    
    if(isTRUE(input$residueDesc_checkbox)){
      dt1 <- get_protocol_resdesc(input=input)
      #print(dt1)
    } 
    else {
      dt1 <- data.frame()
      print(dt1)
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
    print(dt)
    if(nrow(dt)!=0){
      dt<- dplyr::left_join(kds_resmgt, dt) %>% filter(Value!="")
    } else {
      dt<- data.frame()
    }
    
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
  
  #' Seedbed preparation  #############################################################
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
    } 
    else if( nrow(fbdesign())>0 && length(dt)>0 ) {
      dt <- cbind(fbdesign(), dt)
    } else {
      dt <- data.frame()
    }
    dt
  })
  dt_protocol_seedbed <- reactive({
    
    #read data
    kds_seedbed <- magmtprac$seedbed  #  readxl::read_excel(paste0(globalpath, ecname),sheet = "Seedbed preparation")
    
    
    if(isTRUE(input$landLevelling_checkbox)){
      land <- get_protocol_sblavl(input=input)
    } 
    else{
      land <- data.frame()  
    }
    
    if(isTRUE(input$puddling_checkbox)){
      pud<- get_protocol_sbpud(input= input)
    } 
    else{
      pud<- data.frame()
    }
    
    if(isTRUE(input$tillage_checkbox)){
      till<- get_protocol_sbtill(input=input)
    } 
    else {
      till<- data.frame()
    }
    
    dt <- rbind(land,pud,till) #column bind of two sub tabs (description and management)
    #print("dt")
    if(nrow(dt)!=0){
      dt<- dplyr::left_join(kds_seedbed, dt) %>% filter(Value!="")  
    } else{
      dt<- data.frame()
    }
    #print(dt)
    #dt<- ec_clean_header(dt)
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
  get_collectable_seedbed <- reactive({
    
    
    if(isTRUE(input$landLevelling_checkbox)){
      s1<- get_collectable_sblavl(allinputs = AllInputs())
    } else{
      s1 <- NULL  
    }
    
    if(isTRUE(input$puddling_checkbox)){
      s2 <- get_collectable_sbpud(allinputs = AllInputs())
    } else{
      s2 <- NULL 
    }
    
    if(isTRUE(input$tillage_checkbox)){
      s3 <- get_collectable_sbtill(allinputs = AllInputs())
    } else {
      s3 <- NULL 
    }
    out <- c(s1,s2,s3)
  })
  
  #' Soil Fertility     #############################################################
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
  
  #' Planting & Transplanting   #####################################################################
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
      }
      
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
        } else if( nrow(ptdt_list[[i]])==0){ # when one platn-trans data.frame is empty
          dt[[i]] <- smart_colbind(fbdesign(), ptdt_list[[i]] )
        } else if(nrow(ptdt_list[[i]])!=0){ #when one plan-trans data.frame is full
          dt[[i]] <- cbind(fbdesign(), ptdt_list[[i]] )
        }
      }
      names(dt)<- crecm
    } 
    
    
    dt  
  })
  dt_protocol_plantrans <- reactive({
    
    kds_plant<- magmtprac$platrans  
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector",default = "Monocrop") 
    
    if(ct=="Monocrop"){
      dt<- get_protocol_plantrans(allinputs=AllInputs(), input, ctype="monocrop", cropId= "1", addId="1")
      if( length(dt)>0){
        dt <-  dplyr::left_join(kds_plant , dt) %>% filter(Value!="")  
      } else {
        dt <- data.frame()
      }
    } 
    else if(ct=="Intercrop"){
      
      id_rand_inter <- getAddInputId(intercropVars$ids, "int_", "") 
      circm <- map_values(input, id_chr="int_cropCommonName_",id_rand_inter, format = "vector", lbl= "Select crop")
      dtf <-NULL
      for(i in 1:length(id_rand_inter)){
        dtf[[i]] <- get_protocol_plantrans(allinputs=AllInputs(), input=input, ctype="intercrop", cropId=id_rand_inter[i], addId="1")
        if(nrow(dtf[[i]])!=0){
          dtf[[i]] <- dplyr::left_join(kds_plant , dtf[[i]]) %>% filter(Value!="")  
          print("transplanting")
          print(dtf[[i]])
        }
        else {
          dtf[[i]] <- data.frame()
        }
      }
      dt <- rbindlist(dtf,fill = TRUE)
    }
    else if(ct=="Relay crop"){
      id_rand_rel <- getAddInputId(relaycropVars$ids, "rel_", "") 
      circm <- map_values(input, id_chr="rel_cropCommonName_",id_rand_rel, format = "vector", lbl= "Select crop")
      dtf <-NULL
      for(i in 1:length(id_rand_rel)){
        dtf[[i]] <- get_protocol_plantrans(allinputs=AllInputs(), input=input, ctype="relay crop", cropId=id_rand_rel[i], addId="1")
        
        if(nrow(dtf[[i]])!=0){
          dtf[[i]] <- dplyr::left_join(kds_plant , dtf[[i]]) %>% filter(Value!="")  
        }
        else {
          dtf[[i]] <- data.frame()
        }
      }
      dt <- rbindlist(dtf,fill = TRUE)
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
      }
      lbl<- NULL
      for(i in 1:length(ptlbl_list)){
        if(length(ptlbl_list[[i]])!=0){
          lbl[[i]] <- ptlbl_list[[i]] #str_replace_all(string = names(ptdt_list[[i]]), pattern = "__[:digit:]+$",replacement = "")
          names(lbl[[i]])<- circm[i]
        }
      }
      if(!is.null(lbl)){
        #names(lbl)<- circm
        lbl <- purrr::compact(lbl)  
      }
      
    } 
    else if (ct=="Relay crop"){
      
      id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "") 
      crecm <- map_values(input, id_chr="rel_cropCommonName_",id_re_rand, format = "vector", lbl= "Select crop")
      ptlbl_list<- NULL
      for(i in 1:length(id_re_rand)){
        ptlbl_list[[i]] <- get_ec_plantrans(allinputs=AllInputs(), input=input, ctype="relay crop", cropId= id_re_rand[i], addId="1")$lbl
        print("labels plant")
        print(ptlbl_list[[i]])
      }
      lbl<- NULL
      for(i in 1:length(ptlbl_list)){
        if(length(ptlbl_list[[i]])!=0){
          lbl[[i]] <- ptlbl_list[[i]] #str_replace_all(string = names(ptdt_list[[i]]), pattern = "__[:digit:]+$",replacement = "")
          names(lbl[[i]])<- crecm[i]
          print("labels plant 2")
          print(lbl[[i]]) 
        }
      }
      if(!is.null(lbl)){
        #names(lbl)<- crecm
        lbl <- purrr::compact(lbl)  
      }
      
    }
    lbl
  }) 
  
  #' Mulching and residue ############################################################
  dt_mulching <- reactive({
    
    dt <- get_ec_mulching(allinputs= AllInputs())$dt
    if(nrow(fbdesign())==0){
      dt <- dt
    }else {
      dt <-cbind(fbdesign() ,dt)
    }
    dt
  })
  dt_protocol_mulching <- reactive({
    kds_mulch <- magmtprac$mulch
    dt <- get_protocol_mulching(allinputs = AllInputs()) 
    if(nrow(dt)!=0){
      dt<- dplyr::left_join(kds_mulch , dt) %>% filter(Value!="")  
    } else{
      dt<- data.frame()
    }
    
    dt 
  })
  lbl_mulching <- reactive({
    
    lbl <- get_ec_mulching(allinputs= AllInputs())$lbl
    if(length(lbl)==0){lbl <- "no-label"}
    lbl
  })
  
  #' Irrigation  #####################################################################
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
  dt_protocol_irrigation <- reactive({
    kds_irri <- magmtprac$irri
    addId <- getAddInputId(addId = expconIRRImonocrop$ids, "mono_irri_", "")
    dt<- get_protocol_irri(allinputs= AllInputs(),addId=addId) 
    if(nrow(dt)!=0){
      dt<- dplyr::left_join(kds_irri , dt) %>% filter(Value!="")
      dt$NumberofMeasurementsPerSeason <- ns_irrigation()
    } else{
      dt<- data.frame()
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
    ns <- length(addId)
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
  dt_protocol_weeding <- reactive({
    
    kds_weed <- magmtprac$weed
    addId <- getAddInputId(addId = expconWEEmonocrop$ids, "mono_wee_", "")
    dt<- get_protocol_weed(allinputs=AllInputs(), addId=addId, ctype="monocrop")
    if(nrow(dt)!=0){
      dt<- dplyr::left_join(kds_weed , dt) %>% filter(Value!="")
      dt$NumberofMeasurementsPerSeason <- ns_weeding()
    } else{
      dt<- data.frame()
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
    ns <- length(addId)
  })
  
  
  ### Harvest  ######################################################################################################
  dt_harvest <- reactive({
    
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector",default = "Monocrop") 
    if(ct=="Monocrop"){
      addId <- getAddInputId(addId = expconHARVmonocrop$ids, "mono_harv_", "")
      dt <- get_ec_harv(allinputs=AllInputs(), input, ctype="monocrop", addId=addId)$dt
      #allinputs, input, ctype="monocrop", cropId="1", addId="1"
      if(nrow(fbdesign())==0){
        dt <- dt
      }else {
        dt <-cbind(fbdesign() ,dt)
      }
    }
    else if(ct=="Intercrop"){
      id_rand <- getAddInputId(intercropVars$ids, "int_", "")
      cropId<- id_rand
      cropnames <- map_values(input = input, id_chr="int_cropCommonName_",id_rand, format = "vector", lbl= "Select crop")
      addId <- lapply(cropId , function(x) get_addId_multiharvest(x,ctype= "intercrop"))
      
      #Get tables outputs
      dt <- NULL
      for(i in 1:length(cropId)){
        dt[[i]] <- get_ec_harv(allinputs=AllInputs(), input=input, ctype="intercrop", cropId= cropId[i] , addId= addId[[i]] )$dt  
      }
      names(dt)<- cropnames

      #Join fbdesign with harvest header of each crop for intercrop trials
      for(j in 1:length(dt)){
        if(nrow(fbdesign())==0){
          dt[[ cropnames[j] ]] <- dt[[ cropnames[j] ]]
        }else {
          dt[[ cropnames[j] ]] <-cbind(fbdesign() ,dt[[ cropnames[j] ]] )
        }
      }
    } 
    else if(ct=="Relay crop"){
      id_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
      cropId<- id_rand
      cropnames <- map_values(input = input, id_chr="rel_cropCommonName_",id_rand, format = "vector", lbl= "Select crop")
      addId <- lapply(cropId , function(x) get_addId_multiharvest(x, ctype= "relay crop"))

      #Get tables outputs
      dt <- NULL
      for(i in 1:length(cropId)){
        dt[[i]] <- get_ec_harv(allinputs=AllInputs(), input=input, ctype= "relay crop", cropId= cropId[i] , addId= addId[[i]] )$dt  
        print("harv dt list")
        print(dt)
      }
      names(dt)<- cropnames
      
      
      
      # 
      #Join fbdesign with harvest header of each crop for intercrop trials
      for(j in 1:length(dt)){
        if(nrow(fbdesign())==0){
          dt[[ cropnames[j] ]] <- dt[[ cropnames[j] ]]
        }else {
          dt[[ cropnames[j] ]] <-cbind(fbdesign() ,dt[[ cropnames[j] ]] )
        }
      }
      
    }
    
    
    ##Output
    dt
  })
  dt_protocol_harvest <- reactive({
    
    kds_harv <- magmtprac$harv
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector",default = "Monocrop") 
    
    if(ct=="Monocrop"){
      addId <- getAddInputId(addId = expconHARVmonocrop$ids, "mono_harv_", "")
      dt <-get_protocol_harv(allinputs=AllInputs(), input, ctype="monocrop", addId=addId)
      #allinputs, input, ctype="monocrop", cropId="1", addId="1"
      if(nrow(dt)!=0){
        dt<- dplyr::left_join(kds_harv , dt) %>% filter(Value!="")
        dt$NumberofMeasurementsPerSeason <- length(ns_harvest())
      }else {
        dt <- data.frame()
      }
    } else {
      
      if(ct=="Intercrop"){
        id_rand <- getAddInputId(intercropVars$ids, "int_", "")
        cropId<- id_rand
        cropnames <- map_values(input = input, id_chr="int_cropCommonName_",id_rand, format = "vector", lbl= "Select crop")
        addId <- lapply(cropId , function(x) get_addId_multiharvest(x,"intercrop"))
      }
      else if(ct=="Relay crop"){
        id_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
        cropId<- id_rand
        cropnames <- map_values(input = input, id_chr="rel_cropCommonName_",id_rand, format = "vector", lbl= "Select crop")
        addId <- lapply(cropId , function(x) get_addId_multiharvest(x,"relay crop"))
      }  
        
      dtf <-NULL
        for(i in 1:length(cropId)){
          dtf[[i]] <- get_protocol_harv(allinputs=AllInputs(), input=input, ctype= tolower(ct), cropId= cropId[i],addId= addId[[i]]) 
          if(nrow(dtf[[i]])!=0){
            dtf[[i]] <- dplyr::left_join(kds_harv , dtf[[i]]) %>% dplyr::filter(Value!="")
            dtf[[i]]$NumberofMeasurementsPerSeason <- length(ns_harvest()[[i]])
          }
          else {
            dtf[[i]] <- data.frame()
          }
        }
        dt <- rbindlist(dtf,fill = TRUE)
        
      } 
    
    dt 
  })
  lbl_harvest <- reactive({
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector",default = "Monocrop") 
    
    if(ct=="Monocrop"){
      addId <- getAddInputId(addId = expconHARVmonocrop$ids, "mono_harv_", "")
      lbl <- get_ec_harv(allinputs=AllInputs(), input, ctype="monocrop", addId=addId)$lbl
      if(length(lbl)==0){lbl <- "no-label"}
    } 
    else {
      
      if(ct=="Intercrop"){
        id_rand <- getAddInputId(intercropVars$ids, "int_", "")
        cropId<- id_rand
        cropnames <- map_values(input = input, id_chr="int_cropCommonName_",id_rand, format = "vector", lbl= "Select crop")
        addId <- lapply(cropId , function(x) get_addId_multiharvest(x, ctype="intercrop"))
      }
      else if(ct=="Relay crop"){
        id_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
        cropId<- id_rand
        cropnames <- map_values(input = input, id_chr="rel_cropCommonName_",id_rand, format = "vector", lbl= "Select crop")
        addId <- lapply(cropId , function(x) get_addId_multiharvest(x, ctype="relay crop"))
      }
      
      ##Iterate and get list of labels for different crops
      lbl_list <- NULL
      for(i in 1:length(cropId)){
        lbl_list[[i]] <- get_ec_harv(allinputs=AllInputs(), input, ctype=tolower(ct), cropId= cropId[i] , addId= addId[[i]] )$lbl  
      }
      names(lbl_list)<- cropnames
      
      #Get labbels
      lbl<- NULL
      for(i in 1:length(lbl_list)){
        lbl[[i]] <- str_replace_all(string = lbl_list[[i]], pattern = "__[:digit:]+$",replacement = "") 
        names(lbl[[i]])<-cropnames[i]
      }
      
    }
    lbl
  })
  ns_harvest <- reactive({
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector",default = "Monocrop") 
    if(ct=="Monocrop"){
      addId <- getAddInputId(addId = expconHARVmonocrop$ids, "mono_harv_", "")
      ns<- length(addId)
    }else{
      
      if(ct=="Intercrop"){
        cropId<- getAddInputId(intercropVars$ids, "int_", "") 
        cropnames <- map_values(input = input, id_chr="int_cropCommonName_", cropId, format = "vector", lbl= "Select crop")
        ns <- lapply(cropId , function(x) get_addId_multiharvest(x,ctype= "intercrop"))
        #ns <- length(ns)
      }
      else if(ct=="Relay crop"){
        cropId<- getAddInputId(relaycropVars$ids, "rel_", "") 
        cropnames <- map_values(input = input, id_chr="rel_cropCommonName_", cropId, format = "vector", lbl= "Select crop")
        ns <- lapply(cropId , function(x) get_addId_multiharvest(x,ctype= "relay crop"))
        #ns <- length(ns)
      }
      #else if (ct=="Rotation){
        # TODO ROTATION
      #}
     
    }
    ns
  }) 
  
  ############################## END: EXPERIMENT CONDITIONS ##########################################################
  
  
  ##################### START: Phenolgy Tables ########################################################################
  
  #Reactive phenology for multicrop trial##########################################################################
  pheno_multicrop_vars <- reactive({
    
    ct <- map_singleform_values(input$croppingType,  type = "combo box", format = "vector",default = "Monocrop") 
    dt<-list()
    ## MULTICROP TRIALS
    if(ct!="Monocrop"){
      
      if(ct=="Intercrop"){
        id_crop <- getAddInputId(intercropVars$ids, "int_", "") 
        crop <- map_values(input = input, id_chr="int_cropCommonName_",id_crop, format = "vector", lbl= "Select crop")
        id_phe_dt <- get_dtphe_multicrop_ids(ctype="intercrop")
        prefix <- "int"
      }
      
      if(ct=="Relay crop"){
        id_crop <- getAddInputId(relaycropVars$ids, "rel_", "") 
        crop <- map_values(input = input, id_chr="rel_cropCommonName_", id_crop, format = "vector", lbl= "Select crop")
        id_phe_dt <- get_dtphe_multicrop_ids(ctype="relay crop")
        prefix <- "rel"
      }
      
      if(ct=="Rotation"){
        id_crop <- getAddInputId(relaycropVars$ids, "rot_", "") 
        crop <- map_values(input = input, id_chr="rot_cropCommonName_",id_crop, format = "vector", lbl= "Select crop")
        id_phe_dt <- get_dtphe_multicrop_ids(ctype="rotation")
        prefix <- "rot"
      }
      
      for(i in 1:length(crop)){
        
        phe_row_selected<- input[[paste0("tblPhe_",prefix,"_phe_", id_phe_dt[i],"_rows_selected")]]  #input$tblInterPheCassava_rows_selected
        dtPhenoMulticrop <- pheno_vars #%>% dplyr::filter(Crop==crop[i])  #dtInterPheCassava #filtrar phenota por tabla principal
        dtPhenoMulticrop <- ec_clean_header(dtPhenoMulticrop)
        
        if(!is.null(phe_row_selected)){  
          dt[[i]] <- multicrop_phe_vars(dtPhenoMulticrop, phe_row_selected) 
          dt[[i]]$Crop <- crop[i]
          colnames(dt[[i]]) <- c("Crop","Group","Subgroup","Measurement",
                                 "TraitName", "TraitUnit",
                                 "CropMeasurementPerSeason",
                                 "CropMeasurementPerPlot",
                                 #"NumberOfMeasurementsPerSeason",
                                 #"NumberOfMeasurementsPerPlot",
                                  "TraitAlias",
                                 "TraitDataType","TraitValidation","VariableId")
          print("phenology multicrop data")
          print(dt[[i]])
          
          dt[[i]]$CropMeasurementPerSeason<-1
          dt[[i]]$CropMeasurementPerPlot<-1
          
          dt[[i]] <- data.frame(dt[[i]],stringsAsFactors=FALSE)
          dt[[i]]<- ec_clean_header(dt[[i]]) #TODO: REMOVE NA. column and other headers
        } 
        else {
          dt[[i]] <- data.frame(Status="",Crop="", Group="", Subgroup="", Measurement="",
                                TraitName = "",TraitUnit="",
                                CropMeasurementPerSeason="",  CropMeasurementPerPlot="",
                                TraitAlias="", TraitDataType="",TraitValidation="", 
                                VariableId="")
        }
      }
      names(dt) <- crop
    }
    dt
  })
  #Phenologic var for Multicrop trials ##############################################################################
  get_dtphe_multicrop_ids <- function(ctype= "intercrop"){
    
    if(ctype=="intercrop"){
      v <- getAddInputId( mea_phe_multicrop$var_PHE_int, "int_phe_","")
    } else if(ctype=="relay crop"){
      v <- getAddInputId(mea_phe_multicrop$var_PHE_rel , "rel_phe_","")
    } else if(ctype=="rotation"){
      v <- getAddInputId(mea_phe_multicrop$var_PHE_rot, "rot_phe_","")
    }
    print(v)
    out<- v 
  }
  
  ##################### END: Phenolgy Tables #########################################################################
  
  
  ######################################### START: Reactive phenology Fieldbook ##################################################
  pheno_dt <- reactive({
    ct <- map_singleform_values(input$croppingType,  type = "combo box", format = "vector",default = "Monocrop") 
    ## BEGIN MONORCROP 
    if(ct=="Monocrop"){
      #row_select <- input$tblMonoPhe_rows_selected
      row_select <- input$tblPhe_mono_mea_1_rows_selected
      #dt <- dtMonocropphe[row_select, ]
      dt<- pheno_vars[row_select, ]
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
    } 
    
    dt
  })
  pheno_mult_dt <- reactive({
    
    ct <- map_singleform_values(input$croppingType,  type = "combo box", format = "vector",default = "Monocrop") 
    
    if(ct=="Intercrop"){
      id_crop <- getAddInputId(intercropVars$ids, "int_", "") 
      cropnames <- map_values(input = input, id_chr="int_cropCommonName_",id_crop, format = "vector", lbl= "Select crop")
    }
    if(ct=="Relay crop"){
      id_crop <- getAddInputId(relaycropVars$ids, "rel_", "") 
      cropnames <- map_values(input = input, id_chr="rel_cropCommonName_", id_crop, format = "vector", lbl= "Select crop")
    }
    if(ct=="Rotation"){
      id_crop <- getAddInputId(relaycropVars$ids, "rot_", "") 
      cropnames <- map_values(input = input, id_chr="rot_cropCommonName_",id_crop, format = "vector", lbl= "Select crop")
    }  
    
    pheno_dt <- vector("list",length = length(cropnames))
    for(i in 1:length(cropnames)){
      
      dt <- pheno_multicrop_vars()[[cropnames[i]]]
      lbl <- dt$TraitName
      print("label multicrop")
      print(lbl)
      
      if(length(lbl)==0 && nrow(dt)==0){
        dt <- data.frame()
      } 
      else if(nrow(fbdesign())==0 && length(lbl)>=1){
        dt<- t(rep("", length(lbl)))%>% as.data.frame(stringAsFactors=FALSE)
        names(dt) <- lbl
      } 
      else if(nrow(fbdesign())>0 && length(lbl)>=1) {
        dt<- t(rep("", length(lbl)))%>% as.data.frame(stringAsFactors=FALSE)
        names(dt) <- lbl
        dt <-cbind(fbdesign() ,dt)
      }
      pheno_dt[[i]] <- dt
      
    }
    names(pheno_dt) <- cropnames
    pheno_dt
    
  })
  
 
  ################### END: Phenology Fieldbook#### ########################################################################
  

  
  ################################ START: Reactive Weather DT   #####################################################
  weather_dt <- reactive({
    
    #addId <- as.character(weatherVars$num)
    addId <- getAddInputId(weatherVars$ids, "weather_", "") 
    print(addId)
    wdt_vars <- get_weather_variables(AllInputs(),addId= addId)
    
    
    if(nrow(wdt_vars)>0){
     wdt_vars <-get_dt_weather(weather_variables = wdt_vars, dt_weather= dt_weather)
      cs<- add_season_numplot_prefix(dt=wdt_vars)
      wdt_vars<- cs
      m <- data.frame(matrix("", ncol = length(wdt_vars), nrow = 1),stringsAsFactors = FALSE)
      names(m)<-wdt_vars
      wdt_vars<-m
    } 
    else {
      # a2 <- data.frame(Measurement = "", TraitUnit = "", TraitAlias = "",
      #                  TraitDataType = "", TraitValidation ="", VariableId= "")
      wdt_vars <-  data.frame() #data.frame()
    }
    #print("entro 21")
    wdt_vars
    
  })
  
  weather_list <- reactive({
    addId <- getAddInputId(weatherVars$ids, "weather_", "") 
    wdt_vars <- get_weather_variables(AllInputs(),addId= addId)
    wdt_vars <-get_dt_weather(weather_variables = wdt_vars, dt_weather= dt_weather)
  })
  
  ################################ END: Reactive Weather DT   #######################################################
  
  
  
  ################################ START Reactive Soil DT ##########################################################
  soil_dt<- reactive({
    
    #addId <-  soilVars$ids #as.character(soilVars$num)
    addId <- getAddInputId(soilVars$ids, "soil_", "") 
    soil_vars <- get_soil_variables(AllInputs(),addId= addId)
    
     if(nrow(soil_vars)>0){
       soil_vars <-get_dt_soil(soil_variables = soil_vars, dt_soil=dt_soil)
       
      #colnames(soildt) <- c("Group", "Measurement","TraitUnit","NumberofMeasurementsPerSeason", "NumberofMeasurementsPerPlot")
      # colnames(soildt) <- c("Crop","Group","Subgroup","Measurement",
      #                   "TraitUnit","CropMeasurementPerSeason",
      #                   "CropMeasurementPerPlot","TraitName", "TraitAlias",
      #                   "TraitDataType","TraitValidation","VariableId")
      cs<- add_season_numplot_prefix(dt=soil_vars)
      lbl<- cs
      
    } else {
      lbl <- NULL
    }
    
    if(length(lbl)==0){
      soil_vars <- data.frame()
    } else if(nrow(fbdesign())==0 && length(lbl)>=1){
      soil_vars <- t(rep("", length(lbl)))%>% as.data.frame(stringAsFactors=FALSE)
      names(soil_vars) <- lbl
    } else if(nrow(fbdesign())>0 && length(lbl)>=1) {
      print("case 3")
      soil_vars <- t(rep("", length(lbl)))%>% as.data.frame(stringAsFactors=FALSE)
      names(soil_vars) <- lbl
      soil_vars <-cbind(fbdesign() ,soil_vars)
    }
    soil_vars 
  })
  
  soil_list <- reactive({
    
    addId <- getAddInputId(soilVars$ids, "soil_", "") 
    soil_vars <- get_soil_variables(AllInputs(),addId= addId)
    soil_vars <-get_dt_soil(soil_variables = soil_vars, dt_soil=dt_soil)
    
  })
  ################################ END Soil DT #####################################################################
  
  
  ########################################  START: EXPERIMENT + PERSONEL+CROP #########################################
  #experiment
  exp_dt<- reactive({
    id<- map_singleform_values(input = input$experimentId, type = "text input",format = "data.frame", label="Experiment ID")
    exname<- map_singleform_values(input = input$experimentName, type = "text input",format = "data.frame", label="Experiment name")
    prname<- map_singleform_values(input = input$experimentProjectName, type = "text input",format = "data.frame", label="Experiment project name")
    sdate<- map_singleform_values(input = input$fbDesign_project_start_date,type = "date",format = "data.frame", label="Experiment start date")
    edate<- map_singleform_values(input = input$fbDesign_project_end_date,type = "date",format = "data.frame", label="Experiment end date")
    type<- map_singleform_values(input = input$designFieldbook_typeExperiment, 
                                 input_other = input$designFieldbook_typeExperiment_other,
                                 type = "combo box",format = "data.frame", label="Type of experiment")
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
    pl <- map_values(input, id_chr="projLeadEnt_", id_rand_el,format = "data.frame",lbl= "Experiment, lead organization name")
    #pl <- map_values(input, id_chr="projLeadEnt_", id_rand_el,format = "data.frame",lbl= "Experiment, lead organization type")
    plc <- map_values(input, id_chr="tLeadCenter_", id_rand_el, format = "data.frame", lbl= "Experiment, lead organization name")
    plc <- map_values(input, id_chr="tLeadContCRP_", id_rand_el, format = "data.frame", lbl= "Experiment, lead contributor crp")
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
  ## Build experimental design table metadata
  get_faclevdt <- function(design, allinputs){
    
    output <- try({  
      design <- tolower(design)
      dsg <- experimental_design_label(design)
      dsg_abbr <- design %>% toupper()
      
      #Get IDS from design inputs
      IdDesignInputs <- getFactorIds(design)
      #Get index from Design's IDs
      index <- get_index_design(IdDesignInputs, design)
      
      
      flbl<- get_factors_design(allinputs = allinputs, index, design = design,duplicate = FALSE)
      #Get list of labels
      flvl <- get_levels_design(allinputs = allinputs, data_dictionary= dtfactordesign,
                                index, factors = flbl, design=design, format="list")
      #out <- setDT(transpose(flvl))[]
      flvl <-  lapply(flvl, function(x)paste(x,collapse=", "))
      # Number of factors
      nf <- length(flvl)
      
      ## Labels
      flab<- paste("Factor", 1:length(flbl))
      levlab <- paste("Factor", 1:length(flbl), "- Levels")
      paramlab <- c(rbind(flab, levlab)) 
      #Ensemble as a data frame of factors and levels
      out<- data.frame()
      for( i in 1:length(flvl)){
        out <- rbind(out, rbind(flbl[i], flvl[[i]]) )
      }
      #Put as a table
      dsg_dt<- data.frame(Factor= c("Experimental design", "Experimental design abbreviation",
                                    "Number of factors"), 
                          Value = c(dsg,dsg_abbr, nf),stringsAsFactors = FALSE)
      out<- data.frame(Factor= paramlab, Value= out$V1)
      out<-rbind(dsg_dt, out) 
      
      out 
    })
    
    if(class(output)=="try-error"){
      out<- data.frame(Factor=NULL, Value= NULL)
    }else{
      out<- output
    }
    out
  }
  #The entire metadata of the experiment
  globalMetadata<- function(){
    fl_dt <- get_faclevdt(design=input$designFieldbook_agrofims, allinputs=AllInputs() )
    vers_dt <- data.frame(Factor = "Version", Value= "test version 23")
    gtable <- rbind( exp_dt(), 
                     fa_dt(),
                     pe(), 
                     epl(), 
                     pers_dt(),
                     crop_dt(), infounit(),
                     #TODO:: MEJORAR
                     fl_dt, site_dt(),vers_dt )
    
    # gtable <- rbind(pers_dt(),crop_dt(), infounit(),
    #                  #TODO:: MEJORAR
    #                  fl_dt,
    #                  site_dt(),
    #                  vers_dt
    # )
    
    names(gtable)[1]<- "Parameter"
    gtable
  }
  ########################################  END: METADATA ###########################################################
  
  
  ################################### Fieldbook design (statistical design) ########################################
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
  ######################## END  Fieldbook design (statistical design)
  
  
  
  ###############################################START: Site data ####################################################
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
      
      xpath <- fbglobal::get_base_dir() #get main route
      xfp <- file.path(xpath, "table_sites_agrofims.rds")
      
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
  ###############################################END: Site data ####################################################
  
  ################################### START  TRAIT TABLE #############################################################
  
  #Get ADD Ids for multi crop trials in CROP MEASUREMENT
  get_cmea_multicrop_addId <- function(cropId, ctype= "intercrop"){
    
    if(ctype=="intercrop"){
      
      
      if( cropId=="1" ){
        print(meaINT1$ids)
        v <- getAddInputId(meaINT1$ids, "int_mea_[:digit:]+_fluidRow_","")
        #print(v)
      } else if (cropId=="2"){
        print(meaINT2$ids)
        v <- getAddInputId(meaINT2$ids, "int_mea_[:digit:]+_fluidRow_","")
        #print(v)
      } else if (cropId=="3"){
        v <- getAddInputId(meaINT3$ids ,"int_mea_[:digit:]+_fluidRow_","")
      } else if (cropId=="4"){
        v <- getAddInputId(meaINT4$ids ,"int_mea_[:digit:]+_fluidRow_","")
      } else if (cropId=="5"){
        v <- getAddInputId(meaINT5$ids ,"int_mea_[:digit:]+_fluidRow_","")
      } else{ 
        v <-NULL
      }
    } 
    else if(ctype=="relay crop"){
      
      if(cropId=="1"){#
        v <- getAddInputId(meaREL1$ids , "rel_mea_[:digit:]+_fluidRow_","")
      } else if (cropId=="2"){
        v <- getAddInputId(meaREL2$ids , "rel_mea_[:digit:]+_fluidRow_","")
      } else if (cropId=="3"){
        v <- getAddInputId(meaREL3$ids ,"rel_mea_[:digit:]+_fluidRow_","")
      } else if (cropId=="4"){
        v <- getAddInputId(meaREL4$ids ,"rel_mea_[:digit:]+_fluidRow_","")
      } else if (cropId=="5"){
        v <- getAddInputId(meaREL5$ids , "rel_mea_[:digit:]+_fluidRow_","")
      } else{ 
        v <-NULL
      }
      
    } 
    else if(ctype=="rotation"){
      
      if( cropId=="1" ){
        v <- getAddInputId(meaROT1$ids, "rot_mea_[:digit:]+_fluidRow_","")
      } else if (cropId=="2"){
        v <- getAddInputId(meaROT2$ids,"rot_mea_[:digit:]+_fluidRow_","")
      } else if (cropId=="3"){
        v <- getAddInputId(meaROT3$ids,"rot_mea_[:digit:]+_fluidRow_","")
      } else if (cropId=="4"){
        v <- getAddInputId(meaROT4$ids,"rot_mea_[:digit:]+_fluidRow_","")
      } else if (cropId=="5"){
        v <- getAddInputId(meaROT5$ids,"rot_mea_[:digit:]+_fluidRow_","")
      } else{ 
        v <-NULL
      }
      
    }
    print(v)
    out<- v 
  }
  
  #Reactive for in CROP MEASUREMENT to get actives ADD Ids according to cropping type inputs
  cmea_multicrop_add <- reactive({
    
    if(input$croppingType=="Intercrop"){
      cropId <- getAddInputId(intercropVars$ids, "int_", "")
      addId<- vector(mode="list", length = length(cropId))
      addId <- lapply(cropId , function(x) get_cmea_multicrop_addId(x, ctype= "intercrop") )
    } else if (input$croppingType=="Relay crop"){
      cropId <- getAddInputId(relaycropVars$ids, "rel_", "")
      addId<-vector(mode="list", length = length(cropId))
      addId <- lapply(cropId , function(x) get_cmea_multicrop_addId(x, ctype= "relay crop") )
    } else if (input$croppingType=="Rotation"){
      cropId <- getAddInputId(rotationcropVars$ids, "rot_", "")
      addId<-vector(mode="list", length = length(cropId))
      addId <- lapply(cropId , function(x) get_cmea_multicrop_addId(x, ctype= "rotation") )
    }
    # print(addId)
     addId 
  })
  
  # CROP MEASUREMENT TABLE (TRAIT+UNIT)
  trait_dt <- reactive({
    
    if(input$croppingType=="Monocrop"){
      crop <- map_singleform_values(input$cropCommonNameMono, input_other = input$cropCommonNameMono_other, type= "combo box", format = "vector",label = "Crop")
      addId <- getAddInputId(meaMONO$ids, "mono_mea_1_fluidRow_", "")
      dt_measurements <- get_dtcmea_variables(allinputs=AllInputs(), ctype="monocrop", 
                                              addId=addId, crop=crop, cropId= "1")
      list_dt_cmeasurements <- get_dt_trait(dtcmea_variables=dt_measurements, dt_cmea=dt_cmea)    #dplyr::left_join(dt_measurements, dt_cmea)
      #class(list_dt_cmeasurements) <- "monocrop"
      
    } 
    else if(input$croppingType=="Intercrop"){
      
      addId <- cmea_multicrop_add()
      cropId <- getAddInputId(intercropVars$ids, "int_", "")
      crop <- map_values(input = input, id_chr="int_cropCommonName_",cropId, 
                         format = "vector", lbl= "Select crop")
      
      list_dt_cmeasurements <-vector(mode = "list",length = length(crop))
      for(i in seq.int(crop)){
        list_dt_cmeasurements[[i]] <- get_dtcmea_variables(allinputs=AllInputs(), ctype="intercrop",  
                                                           addId=addId[[i]], crop=crop[i], cropId= cropId[i])
        list_dt_cmeasurements[[i]] <- get_dt_trait(dtcmea_variables=list_dt_cmeasurements[[i]], dt_cmea=dt_cmea)
      }
      names(list_dt_cmeasurements) <- crop
    } 
    else if(input$croppingType=="Relay crop"){
      addId <- cmea_multicrop_add()
      cropId <- getAddInputId(relaycropVars$ids, "rel_", "")
      crop <- map_values(input = input, id_chr="rel_cropCommonName_",cropId, 
                         format = "vector", lbl= "Select crop")
      list_dt_cmeasurements <-vector(mode = "list",length = length(crop))
      for(i in seq.int(crop)){
        list_dt_cmeasurements[[i]] <- get_dtcmea_variables(allinputs=AllInputs(), ctype="relay crop",  
                                                           addId=addId[[i]], crop=crop[i], cropId= cropId[i])
        list_dt_cmeasurements[[i]] <- get_dt_trait(dtcmea_variables=list_dt_cmeasurements[[i]], dt_cmea=dt_cmea)
        
      }
      names(list_dt_cmeasurements) <- crop
    } 
    else if(input$croppingType=="Rotation"){
      addId <- cmea_multicrop_add()
      cropId <- getAddInputId(rotationcropVars$ids, "rot_", "")
      crop <- map_values(input = input, id_chr="rot_cropCommonName_",cropId, 
                         format = "vector", lbl= "Select crop")
      
      list_dt_cmeasurements <-vector(mode = "list",length = length(crop))
      for(i in seq.int(crop)){
        list_dt_cmeasurements[[i]] <- get_dtcmea_variables(allinputs=AllInputs(), ctype="rotation",  
                                                           addId=addId[[i]], crop=crop[i], cropId= cropId[i])
        list_dt_cmeasurements[[i]] <- get_dt_trait(dtcmea_variables=list_dt_cmeasurements[[i]], dt_cmea=dt_cmea)
        
      }
      names(list_dt_cmeasurements) <- crop
    }
    print("salida 2")
    list_dt_cmeasurements
  })
  
  ################################### END TRAIT TABLE############################################################### 
  
  #################### START PROTOCOL TABLE #########################################################################
  
  protocol_dt <- reactive({
    
    if(!is.null(input$selectAgroFeature)){
      
      if(is.element(el = "Residue management", set = input$selectAgroFeature)){
        out1 <- dt_prot_residual()
      }else {
        out1 <- data.frame()
      }
      print("protocol 1")
      if(is.element(el = "Seedbed preparation", set = input$selectAgroFeature)){
        out2 <- dt_protocol_seedbed()
      }else {
        out2 <- data.frame()
      }
      print("protocol 2")
      if(is.element(el = "Planting and transplanting",set = input$selectAgroFeature)){
        out3 <- dt_protocol_plantrans()
      }else {
        out3 <- data.frame()
      }
      print("protocol 3")
      if(is.element(el = "Mulch management", set = input$selectAgroFeature)){
        out4 <- dt_protocol_mulching()
      }else {
        out4 <- data.frame()
      }
      print("protocol 4")
      if(is.element(el = "Irrigation",input$selectAgroFeature)){
        out5 <- dt_protocol_irrigation()
      }else {
        out5 <- data.frame()
      }
      print("protocol 5")
      if(is.element(el =  "Weeding",input$selectAgroFeature)){
        out6 <- dt_protocol_weeding()
      }else {
        out6 <- data.frame()
      }
      print("protocol 6")
      if(is.element(el = "Harvest", input$selectAgroFeature)){
        out7 <- dt_protocol_harvest()
      }else {
        out7 <- data.frame()
      }
      print("protocol 7")
      protocol <- list(out1, out2, out3, out4, out5, out6, out7)  
      
      valid  <-lapply(protocol, function(x){length(x)!=0} ) %>% unlist()
      
      protocol <-data.table::rbindlist(protocol[valid],fill = TRUE)
      protocol <- ec_clean_header(protocol) 
    } 
    else {
      protocol <- data.frame()
    }
    
  })
  
  ################## END PROTOCOL TABLE ######################################################################
  
  
  ### Fieldbook + Traits for Monocrop ###############################################################################
  fbdesign_traits <- reactive({
    
    fb <- fbdesign()
    trait_dt <- trait_dt()
    ##NEW CODE
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector",default = "Monocrop") 
    
    if(ct=="Monocrop"){
      cs<- add_season_numplot_prefix(dt=trait_dt) #trait is a table
    } 
    else{
      cs<-NULL
    }
    trait_selected <- cs
    
    if(!is.null(trait_selected) || length(trait_selected)==0 ){
      mm  <-  matrix(nrow = nrow(fb), ncol = length(trait_selected) )
      nm  <-  c(names(fb), trait_selected)
      fb  <-  cbind(fb, mm)
      names(fb)  <-  nm
    }
    
    fb
    
  })
  
  ### Fieldbook + Traits for multicrop trials ######################################################################
  fbdesign_mult_traits <- reactive({
    
    ct <- map_singleform_values(input$croppingType, type = "combo box", format = "vector",default = "Intercrop") 
    
    if(ct=="Intercrop"){
      id_ic_rand <- getAddInputId(intercropVars$ids, "int_", "")
      crop <- map_values(input = input, id_chr="int_cropCommonName_",id_ic_rand, format = "vector", lbl= "Select crop")
    }
    if(ct=="Relay crop"){
      id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
      crop <- map_values(input = input, id_chr="rel_cropCommonName_",id_re_rand, format = "vector", lbl= "Select crop")
    }
    
    if(ct!="Monocrop"){
      fb <- fbdesign()
      trait <- trait_dt()
      fb_list <- list()
      for(i in 1:length(trait)){
        trait_selected  <- add_season_numplot_prefix(dt=trait[[i]])
        #trait_selected <- cs
        if(!is.null(trait_selected) || length(trait_selected)==0 ){
          mm  <-  matrix(nrow = nrow(fb), ncol = length(trait_selected) )
          nm  <-  c(names(fb), trait_selected)
          fb_list[[i]]  <-  cbind(fb, mm)
          names(fb_list[[i]])  <-  nm
        }
      }
      names(fb_list) <- crop
      fb <-fb_list
    }
    
  })
  
  
  ## Get addId for multi crop trials
  get_addId_multiharvest <- function(cropId, ctype= "intercrop" ){
    
    if(ctype=="intercrop"){
      if( cropId=="1" ){
        v <- getAddInputId(expconIntHARVcrop1$ids,pattern = "int_harv_[:digit:]+_","")
      } else if (cropId=="2"){
        v <- getAddInputId(expconIntHARVcrop2$ids,pattern = "int_harv_[:digit:]+_","")
      } else if (cropId=="3"){
        v <- getAddInputId(expconIntHARVcrop3$ids,pattern = "int_harv_[:digit:]+_","")
      } else if (cropId=="4"){
        v <- getAddInputId(expconIntHARVcrop4$ids,pattern = "int_harv_[:digit:]+_","")
      } else if (cropId=="5"){
        v <- getAddInputId(expconIntHARVcrop5$ids,pattern = "int_harv_[:digit:]+_","")
      } else{ 
        v <-NULL
      }
    } 
    else if(ctype=="relay crop"){
      
      if(cropId=="1"){
        v <- getAddInputId(expconRelHARVcrop1$ids, pattern = "rel_harv_[:digit:]+_","")
      } else if (cropId=="2"){
        v <- getAddInputId(expconRelHARVcrop2$ids, pattern = "rel_harv_[:digit:]+_","")
      } else if (cropId=="3"){
        v <- getAddInputId(expconRelHARVcrop3$ids, pattern = "rel_harv_[:digit:]+_","")
      } else if (cropId=="4"){
        v <- getAddInputId(expconRelHARVcrop3$ids, pattern = "rel_harv_[:digit:]+_","")
      } else if (cropId=="5"){
        v <- getAddInputId(expconRelHARVcrop4$ids, pattern = "rel_harv_[:digit:]+_","")
      } else{ 
        v <-NULL
      }
      
    } 
    else if(ctype=="rotation"){
      
      if( cropId=="1" ){
        v <- getAddInputId(expconIntHARVcrop1$ids,pattern = "int_harv_[:digit:]+_","")
      } else if (cropId=="2"){
        v <- getAddInputId(expconIntHARVcrop2$ids,pattern = "int_harv_[:digit:]+_","")
      } else if (cropId=="3"){
        v <- getAddInputId(expconIntHARVcrop3$ids,pattern = "int_harv_[:digit:]+_","")
      } else if (cropId=="4"){
        v <- getAddInputId(expconIntHARVcrop4$ids,pattern = "int_harv_[:digit:]+_","")
      } else if (cropId=="5"){
        v <- getAddInputId(expconIntHARVcrop5$ids,pattern = "int_harv_[:digit:]+_","")
      } else{ 
        v <-NULL
      }
      
    }
    
    out<- v 
  }
  
  
  ### Book preview ##################################################################
  shiny::observeEvent(input$fbDesign_draft_agrofims, {
    
    #print("trait")
    #print(trait_dt())
    
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
      }
      else if(crp=="" && ct=="Monocrop"){
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
  
  
  
  ######################### Donwload Fieldbook #################################################################
  output$downloadData <- downloadHandler(
    filename = "fileNameBook.xlsx",
    content = function(file) {
      
      withProgress(message = 'Downloading fieldbook', value = 0, {
        
        # ai <- AllInputs()
        # saveRDS(ai, "/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/table_ids.rds")
        # x <- reactiveValuesToList(input)
        # saveRDS(x, "/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/inputs.rds")
        # 
        #design <- tolower(input$designFieldbook_agrofims) #lowercase
        #Get IDS from design inputs
        #IdDesignInputs <- getFactorIds(design)
        #id_rand <<- getAddInputId(intercropVars$ids, "int_", "")
        #cropId<<- id_rand
        #cropnames <<- map_values(input = input, id_chr="int_cropCommonName_",id_rand, format = "vector", lbl= "Select crop")
        #addId <<- lapply(cropId , function(x) get_addId_multiharvest(x))
        
        gmetadata <- globalMetadata()
        fname <- paste(file,"xlsx",sep=".")
        
        #Cropping type
        ct <- map_singleform_values(input$croppingType,  type = "combo box", format = "vector",default = "Monocrop") 
        
        if(ct=="Monocrop"){
          fb  <- fbdesign_traits()
        } 
        else if(ct=="Intercrop" || ct=="Relay crop"){
          fb<- fbdesign_mult_traits()
        }
        
        print("inicio")
        ######################### INITIALIZE THE EXCEL WORKBOOK ###############################
        wb <- createWorkbook()
        
        print("inicio2")
        incProgress(2/20,message = "Downloading data...")
        
        print("inicio3")
        ######################## METADATA ######################################################################
        incProgress(6/20,message = "Metadata metadata sheet...")
        openxlsx::addWorksheet(wb, "Metadata", gridLines = TRUE)
        openxlsx::writeDataTable(wb, "Metadata", x = gmetadata,
                                 colNames = TRUE, withFilter = FALSE)
        ########################################################################################################
        print("inicio 3 -1 ")
        ########################################## Protocol data  ##############################################
 
        #a1<<- protocol_dt()
        
        if(nrow(protocol_dt())!=0){
          openxlsx::addWorksheet(wb, "Protocol", gridLines = TRUE)
          openxlsx::writeDataTable(wb, "Protocol", x =  protocol_dt(),colNames = TRUE, withFilter = FALSE)
        }
        #####################################################################################################
        
        ######################## Notes_deviations ###########################################################
        openxlsx::addWorksheet(wb, "Notes_Deviations", gridLines = TRUE)
        openxlsx::writeDataTable(wb, "Notes_Deviations", x = fbdesign(),colNames = TRUE, withFilter = FALSE)
        ####################################################################################################
        
        print("inicio4")
        
        #############################  FIELDBOOK SHEET  ###########################################################
        if(ct=="Monocrop"){
          incProgress(7/20,message = "Adding Crop measurements data...")
          openxlsx::addWorksheet(wb, "Crop_measurements", gridLines = TRUE)
          openxlsx::writeDataTable(wb, "Crop_measurements", x = fb,
                                   colNames = TRUE, withFilter = FALSE)
          
        } 
        else if(ct=="Intercrop"){
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
        else if(ct=="Relay crop"){
          
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
        ############################# END FIELDBOOK SHEET  ########################################################
        
        ###################### START MANAGEMENT PRACTICES SHEET #############################################################
        ######################### Residue management ###############################################################
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
        ######################### Seedbed preparation ##############################################################
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
        
        ######################### Planting and transplanting #####################################################
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
              
              # if(nrow(dt_plantrans()[[i]])!=0  && ncol(fbdesign())!= ncol(dt_plantrans()[[i]])){
              # dt_pltr <- dt_plantrans()
              
              if(nrow(dt_plantrans()[[i]])!=0  && ncol(fbdesign())!= ncol(dt_plantrans()[[i]])){
                dt_pltr <- dt_plantrans()[[i]]
                
                #TODO Avoid LONG names in sheetNames (error) max 32 characters
                openxlsx::addWorksheet(wb,  paste("Planting-",crecm[i]), gridLines = TRUE)
                openxlsx::writeDataTable(wb, paste("Planting-",crecm[i]), x = dt_pltr,
                                         colNames = TRUE, withFilter = FALSE)
              }
              
            }
          }
        }
        
        ######################### Mulch management#### ###########################################################
        if(is.element("Mulch management",input$selectAgroFeature)){
          print("Adding Mulching") 
          incProgress(7/20,message = "Adding mulching sheet")
          openxlsx::addWorksheet(wb, "Mulch_management", gridLines = TRUE)
          
          openxlsx::writeDataTable(wb, "Mulch_management", x = dt_mulching(),
                                   colNames = TRUE, withFilter = FALSE)
          
        }
        
        ######################### Irrigation########## ###########################################################
        if(is.element("Irrigation",input$selectAgroFeature)){
          incProgress(7/20,message = "Adding irrigation sheet")
          openxlsx::addWorksheet(wb, "Irrigation", gridLines = TRUE)
          openxlsx::writeDataTable(wb, "Irrigation", x = dt_irrigation(),
                                   colNames = TRUE, withFilter = FALSE)
          
        }
        
        ######################### Weeding############# ###########################################################
        if(is.element("Weeding",input$selectAgroFeature)){
          print("weeding")
          incProgress(7/20,message = "Adding weeding sheet")
          openxlsx::addWorksheet(wb, "Weeding", gridLines = TRUE)
          openxlsx::writeDataTable(wb, "Weeding", x = dt_weeding(),
                                   colNames = TRUE, withFilter = FALSE)
        }
        
        ######################### Harvest ############ ###########################################################
        if(is.element("Harvest",input$selectAgroFeature)){
          print("harvest")
          if(ct=="Monocrop"){
            incProgress(7/20,message = "Adding harvest sheet")
            openxlsx::addWorksheet(wb, "Harvest", gridLines = TRUE)
            openxlsx::writeDataTable(wb, "Harvest", x = dt_harvest(),
                                     colNames = TRUE, withFilter = FALSE)
          } 
          else{
            
            
            if(ct=="Intercrop"){
              id_rand <- getAddInputId(intercropVars$ids, "int_", "")
              circm <- map_values(input = input, id_chr="int_cropCommonName_",id_rand, format = "vector", lbl= "Select crop")
              #addId <- lapply(cropId , function(x) get_addId_multiharvest(x))
              
            } else if(ct=="Relay crop"){
              id_rand <- getAddInputId(relaycropVars$ids, "rel_", "")
              circm <- map_values(input = input, id_chr="rel_cropCommonName_",id_rand, format = "vector", lbl= "Select crop")
            }
            
            #hrv<<- dt_harvest()
            
            for(i in 1:length(circm)){
              incProgress(7/20,message = "Adding harvest" )##paste("Adding", circm[i] , "harvest sheet",sep=""))
              dt_harv <- dt_harvest()
              #print(dt_harvest)
              print("paso")
              openxlsx::addWorksheet(wb,  paste0("Harvest-",circm[i]), gridLines = TRUE)
              openxlsx::writeDataTable(wb, paste0("Harvest-",circm[i]), x = dt_harv[[circm[i]]],
                                       colNames = TRUE, withFilter = FALSE)
            }
            
          }
        }
        
        ###################### END MANAGEMENT PRACTICES SHEET ########################################################
        
        print("inicio6")
        
        incProgress(9/20,message = "Adding crop measurement sheet...")
        
        ############# START: PHENOLOGY FIELDBOOK SHEET ######################################################################
        if(ct=="Monocrop"){
          print("inicio8 1")
          if(nrow(pheno_dt())!=0){
            openxlsx::addWorksheet(wb, "Phenology", gridLines = TRUE)
            openxlsx::writeDataTable(wb, "Phenology", x = pheno_dt(),
                                     colNames = TRUE, withFilter = FALSE)
          }
        } 
        else if(ct=="Intercrop"){
          id_crop <- getAddInputId(intercropVars$ids, "int_", "")
          cropnames <- map_values(input = input, id_chr="int_cropCommonName_",id_crop, format = "vector", lbl= "Select crop")
          
          print("ENTRO A INTERROP")
          for(i in 1:length(cropnames)){
            print(i)
            if(nrow(pheno_mult_dt()[[ cropnames[i] ]])!=0 && !is.element("Measurement_3", names(pheno_mult_dt()[[ cropnames[i] ]]) )){
              incProgress(7/20,message = "Adding Phenology data...")
              openxlsx::addWorksheet(wb, paste0("Phenology-",cropnames[i]), gridLines = TRUE)
              openxlsx::writeDataTable(wb, paste0("Phenology-",cropnames[i]),
                                       x = pheno_mult_dt()[[ cropnames[i] ]],
                                       colNames = TRUE, withFilter = FALSE)
            }
          }
          
        } 
        else if(ct=="Relay crop"){
          
          print("ENTRO A RELAY CROPl")
          
          id_crop <- getAddInputId(relaycropVars$ids, "rel_", "")
          cropnames <- map_values(input = input, id_chr="rel_cropCommonName_",id_crop, format = "vector", lbl= "Select crop")
          #crop <- map_values(input = input, id_chr="int_cropCommonName_",id_crop, format = "vector", lbl= "Select crop")
          
          print("ENTRO A Relay crop")
          for(i in 1:length(cropnames)){
            print(i)
            if(nrow(pheno_mult_dt()[[ cropnames[i] ]])!=0 && !is.element("Measurement_3", names(pheno_mult_dt()[[ cropnames[i] ]]) )){
              incProgress(7/20,message = "Adding Phenology data...")
              openxlsx::addWorksheet(wb, paste0("Phenology-",cropnames[i]), gridLines = TRUE)
              openxlsx::writeDataTable(wb, paste0("Phenology-",cropnames[i]),
                                       x = pheno_mult_dt()[[ cropnames[i] ]],
                                       colNames = TRUE, withFilter = FALSE)
            }
          }
          
          
        }
        
        
        ############# END: PHENOLOGY FIELDBOOK SHEET ########################################################################
        
        print("inicio9")
        
        ############ WEATHER SHEET ############################################################################
        if(nrow(weather_dt())!=0){
          openxlsx::addWorksheet(wb, "Weather", gridLines = TRUE)
          openxlsx::writeDataTable(wb, "Weather", x = weather_dt(),
                                   colNames = TRUE, withFilter = FALSE)
        }
        ############# END WEATHER SHEET ##########################################################################
        
        print("inicio10")
        print(soil_dt())
        ############# SOIL SHEET ################################################################################### 
        if(nrow(soil_dt())!=0){
          openxlsx::addWorksheet(wb, "Soil", gridLines = TRUE)
          openxlsx::writeDataTable(wb, "Soil", x = soil_dt(),
                                   colNames = TRUE, withFilter = FALSE)
        }
        ############# END SOIL SHEET ##########################################################################
        
        print("inicio 11")
        
        ############# CROP MEASUREMENT TRAIT SHEET ###################################################################
        if(ct=="Monocrop"){
          #row_sel<- input$tblMono_rows_selected
          
          if(length(trait_dt)>0){
            cm_tl  <- trait_dt()
            
          } else{
            cm_tl<- data.frame()
          }
        } 
        else { #intecrop, relay crop and rotation
          cm_tl <-  data.table::rbindlist(trait_dt(),fill = TRUE)
          #cm_tl <- ec_clean_header(cm_tl)
        }
        ############# END CROP MEASUREMENT TRAIT SHEET ###############################################################
        
        print("inicio 12")
        
        ############# START SOIL MEASUREMENT FOR TRAIT LIST  ###############################################################
        if(nrow(soil_dt())!=0){
          
          print(soil_list())
          
          soil_tl<- soil_list()
        }
        else{
          soil_tl <- data.frame()
        }
        ############## END SOIL TRAIT LIST SHEET ##################################################################
        
        print("inicio 13")
        
        ############# WEATHER TRAIT LIST SHEET #######################################################################
        print("Weather data frame")
        
        if(nrow(weather_dt())!=0){
          
          print(weather_list())
          
          wdt_tl<- weather_list()
        }
        else{
          wdt_tl<- data.frame()
        }
        ############## END WEATHER TRAIT LIST SHEET ##################################################################
        
        
        ############## PHENOLOGY TRAIT LIST ##########################################################################
        print("pheno trait list")
        if(ct=="Monocrop"){
          
          if(nrow(pheno_dt())!=0){
            row_select <- input$tblPhe_mono_mea_1_rows_selected
            row_select<- sort(row_select)
            dt <- pheno_vars[row_select, ]
            dt <- ec_clean_header(dt)
            ph_tl <- dt
            #ph_tl$CropMeasurementPerSeason <- ph_tl$CropMeasurementPerPlot <- 1
            print("---pheno mono trial list")
            print(names(ph_tl))
            # ph_tl <- data.table::setnames(x = ph_tl, old = c("CropMeasurementPerSeason","CropMeasurementPerPlot"),
            #                               new = c("NumberofMeasurementsPerSeason","NumberofMeasurementsPerPlot"))
            ph_tl$NumberofMeasurementsPerSeason <- ph_tl$NumberofMeasurementsPerPlot <- 1
          } else  {
            ph_tl <- data.frame()
          }
          
        }
        else {
          ph_tl <- rbindlist(pheno_multicrop_vars(),fill = TRUE)
          print("---pheno MULTICROP trial list")
          print(names(ph_tl))
          ph_tl$CropMeasurementPerSeason <- pheno_vars$CropMeasurementPerPlot <- 1
          ph_tl <- data.table::setnames(ph_tl, c("CropMeasurementPerSeason","CropMeasurementPerPlot"),
                                               c("NumberofMeasurementsPerSeason","NumberofMeasurementsPerPlot"))
          ph_tl <- ec_clean_header(ph_tl)
        }
        ############## END PHENOLOGY TRAIT LIST SHEET################################################################
        
        
        ################### START TRAIT LIST SHEET FOR CROP MEASUREMENTS AND MANAGEMENT PRACTICES ######################### 
        
        ##Consolidation of crop measurement, soil, weather amd phenology data
        print("inicio14")
        l_lt <- list(cm_tl, ph_tl, wdt_tl, soil_tl)
        dt_kds<- rbindlist(l_lt, fill = TRUE)
        print("inicio 14.1")
        # #Remove foo columns
        print("inicio15")
        dt_kds<- ec_clean_header(dt_kds)
        print("residue pt")
        if(is.element("Residue management",input$selectAgroFeature)){
          kds_resmgt<- magmtprac$resmgt
          #kds_resmgt <- kds_resmgt %>% dplyr::filter(Fieldbook_download %in% lbl_residual())#deprecated
          #Filter labels
          kds_resmgt <- kds_resmgt %>% dplyr::filter(TraitName %in% lbl_residual())
          #Collectable inputs
          if(length(get_collectable_resmgt(allinputs=AllInputs()))!=0){
            collect_resmgt <- get_collectable_resmgt(allinputs=AllInputs())
            kds_resmgt <-  kds_resmgt %>% dplyr::mutate(temp = paste0(Subgroup,"_",Measurement))
            kds_resmgt <- kds_resmgt %>% dplyr::filter(temp %in% collect_resmgt)
          }
          kds_resmgt <- data.table(kds_resmgt)
          dt_kds<-rbindlist(list(dt_kds,kds_resmgt),fill = TRUE)
          dt_kds<- ec_clean_header(dt_kds)
        }
        if(is.element("Seedbed preparation",input$selectAgroFeature)){
          
          kds_sedbed <- magmtprac$seedbed
          kds_sedbed <- ec_filter_data(kds_sedbed)
          #kds_sedbed <- kds_sedbed %>% dplyr::filter(Fieldbook_download %in% lbl_seedbed()) #deprecated
          kds_sedbed <- kds_sedbed %>% dplyr::filter(TraitName %in% lbl_seedbed())
          #Collectable inputs------------------------------
          if(length(get_collectable_seedbed()!=0)){
            collect_seedbed <- get_collectable_seedbed()
            kds_sedbed <-  kds_sedbed %>% dplyr::mutate(temp = paste0(Subgroup,"_",Measurement))
            kds_sedbed <- kds_sedbed %>%  dplyr::filter(temp %in% collect_seedbed)
          }
          #end collectable inputs
          
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
        if(is.element("Planting and transplanting",input$selectAgroFeature)){
          
          kds_platra <- magmtprac$platrans
          kds_platra <- ec_filter_data(kds_platra)
          
          #TODO :generalizar para intercrop
          if(ct=="Monocrop"){
            kds_platra <- kds_platra %>% dplyr::filter(TraitName %in% lbl_plantrans())
            #Collectable inputs
            if(length(get_collectable_plantrans(AllInputs(), ctype="monocrop"))!=0){
              collect_platra <- get_collectable_plantrans(AllInputs(),ctype="monocrop")
              kds_platra <- kds_platra %>% dplyr::mutate(temp=paste0(Group,"_",Measurement))
              kds_platra <- kds_platra %>% filter(temp %in% collect_platra)
            }
            #End collectablbe inputs
            
          }
          else if(ct=="Intercrop") {
            
            temp_platra <- list()
            for(i in 1:length(lbl_plantrans()) ) {
              
              if( length(lbl_plantrans())!=0) {
                temp_platra[[i]] <- kds_platra %>% dplyr::filter(TraitName %in% lbl_plantrans()[[i]])
                temp_platra[[i]]$Crop <- circm[i]
              }
            }
            kds_platra <- rbindlist(temp_platra,fill = TRUE)
            #Collectable inputs ---------------------------
            #lbbb <<- get_collectable_plantrans(AllInputs(), ctype="intercrop",crop=circm, cropId=id_ic_rand)
            
            if(length(get_collectable_plantrans(AllInputs(), ctype="intercrop",crop=circm, cropId=id_ic_rand)  )!=0){
              collect_platra <- get_collectable_plantrans(AllInputs(),ctype="intercrop", crop=circm,cropId= id_ic_rand)
              kds_platra <- kds_platra %>% dplyr::mutate(temp=paste0(Group,"_",Crop,"_",Measurement))
              kds_platra <- kds_platra %>% filter(temp %in% collect_platra)
            }
            #end collectable inputs-------------------------
            
          }
          else if(ct=="Relay crop"){
            
            id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "") 
            crecm <- map_values(input, id_chr="rel_cropCommonName_",id_re_rand, format = "vector", lbl= "Select crop")
            
            
            temp_platra <- list()
            for(i in 1:length(lbl_plantrans()) ) {
              
              if( length(lbl_plantrans())!=0) {
                temp_platra[[i]] <- kds_platra %>% dplyr::filter(TraitName %in% lbl_plantrans()[[i]])
                temp_platra[[i]]$Crop <- crecm[i]
              }
            }
            kds_platra <- rbindlist(temp_platra,fill = TRUE)
            #Collectable inputs -------------------------------
            if(length(get_collectable_plantrans(AllInputs(), ctype="relay crop",crop=crecm, cropId=id_re_rand))!=0){
              collect_platra <- get_collectable_plantrans(AllInputs(),ctype="relay crop", crop=crecm, cropId= id_re_rand)
              kds_platra <- kds_platra %>% dplyr::mutate(temp=paste0(Group,"_",Crop,"_",Measurement))
              kds_platra <- kds_platra %>% filter(temp %in% collect_platra)
            }
            #End collectable inputs-------------------------
          }
          
          kds_platra <- data.table(kds_platra)
          dt_kds<-rbindlist(list(dt_kds,kds_platra),fill = TRUE)
          dt_kds<- ec_clean_header(dt_kds)
        }
        print("paso pt")
        if(is.element("Mulch management",input$selectAgroFeature)){
          
          kds_mulch <- magmtprac$mulch
          kds_mulch <- ec_filter_data(kds_mulch)
          kds_mulch <- kds_mulch %>% dplyr::filter(TraitName %in% lbl_mulching())
          #Collectable ids -------------------------------------------------------------
          if(length(get_collectable_mulching(AllInputs()))!=0){
            collect_mulch <- get_collectable_mulching(AllInputs())
            kds_mulch <-  kds_mulch %>% dplyr::mutate(temp = paste0(Group,"_",Measurement))
            kds_mulch <- kds_mulch %>% dplyr::filter(temp %in% collect_mulch)
          }
          #end collectalbe ids
          
          kds_mulch <- data.table(kds_mulch)
          dt_kds<-rbindlist(list(dt_kds,kds_mulch),fill = TRUE)
          dt_kds<- ec_clean_header(dt_kds)
        }
        print("paso mul")
        if(is.element("Irrigation",input$selectAgroFeature)){
          
          kds_irri <- magmtprac$irri
          kds_irri <- ec_filter_data(kds_irri)
          print("--irri1")
          kds_irri <-  kds_irri %>% dplyr::filter(TraitName %in% lbl_irrigation())
          print("--irri2")
          #Collectable ids -------------------------------------------------------------
          if(length(get_collectable_irri(AllInputs()))!=0){
            collect_irri <- get_collectable_irri(AllInputs())
            kds_irri <-  kds_irri %>% dplyr::mutate(temp = paste0(Group,"_",Measurement))
            kds_irri <- kds_irri %>%  dplyr::filter(temp %in% collect_irri)
            print("--irri3")
          }#end collectalbe ids
          print("irri names")
          names(kds_irri)
          
          kds_irri$NumberofMeasurementsPerSeason <- ns_irrigation()
          print("--irri4")
          kds_irri <- data.table(kds_irri)
          dt_kds<- data.table::rbindlist(list(dt_kds,kds_irri),fill = TRUE)
          dt_kds<- ec_clean_header(dt_kds)
        }
        print("paso irri")
        if(is.element("Weeding",input$selectAgroFeature)){
          
          kds_weed <- magmtprac$weed
          kds_weed <- ec_filter_data(kds_weed)
          kds_weed <-  kds_weed %>% dplyr::filter(TraitName %in% lbl_weeding())
          print("--wee1")
          if(length(get_collectable_irri(AllInputs()))!=0){
            collect_weed <- get_collectable_weed(AllInputs())
            kds_weed <-  kds_weed %>% dplyr::mutate(temp = paste0(Group,"_",Measurement))
            kds_weed <- kds_weed %>%  dplyr::filter(temp %in% collect_weed)
            print("--wee2")
          }
          #Add number of measurement per season          
          kds_weed$NumberofMeasurementsPerSeason <- ns_weeding()
          kds_weed <- data.table(kds_weed)
          dt_kds<-data.table::rbindlist(list(dt_kds,kds_weed),fill = TRUE)
          dt_kds<- ec_clean_header(dt_kds)
        }
        print("paso wwed")
        if(is.element("Harvest",input$selectAgroFeature)){
          
          kds_harv <- magmtprac$harv
          kds_harv <- ec_filter_data(kds_harv)
          if(ct=="Monocrop" ){
            kds_harv <-  kds_harv %>% dplyr::filter(TraitName %in% lbl_harvest())
            if(length(get_collectable_harvest(AllInputs(), ctype= "monocrop" ))!=0){
              collect_harv <- get_collectable_harvest(AllInputs(),ctype="monocrop")
              print(collect_harv)
              kds_harv <- kds_harv  %>% dplyr::mutate(temp=paste0(Group,"_",Measurement))
              kds_harv <- kds_harv  %>% dplyr::filter(temp %in% collect_harv)
            }
            #Add number of evaluation per seasons
            kds_harv$NumberofMeasurementsPerSeason <- ns_harvest()
          }
          else{
            
            if(ct=="Intercrop"){
              id_re_rand <- getAddInputId(intercropVars$ids, "int_", "") 
              crmult <- map_values(input, id_chr="int_cropCommonName_", id_re_rand, format = "vector", lbl= "Select crop")
            }
            if(ct=="Relay crop"){
              id_re_rand <- getAddInputId(relaycropVars$ids, "rel_", "") 
              crmult <- map_values(input, id_chr="rel_cropCommonName_", id_re_rand, format = "vector", lbl= "Select crop")
            }
            # else if(ct=="Rotation"){
            #  ****** TODO ROTATION  *******
            # }
            
            temp_harv <- list()
            # Generate table for multicrop trials and Add number of evaluation per season per each crop
            for(i in seq.int(lbl_harvest())){
              temp_harv[[i]] <- kds_harv %>% dplyr::filter(TraitName %in% lbl_harvest()[[i]])
              temp_harv[[i]]$Crop <- crmult[i]
              temp_harv[[i]]$NumberofMeasurementsPerSeason <- length(ns_harvest()[[i]])
            }
            
            #Bind all lists in one data.table
            kds_harv <- data.table::rbindlist(temp_harv,fill = TRUE)
            #Collect Ids
            if(length(get_collectable_harvest(AllInputs(),ctype= tolower(ct), crop=crmult, cropId= id_re_rand))!=0){
              collect_harv <- get_collectable_harvest(AllInputs(),ctype= tolower(ct), crop=crmult, cropId= id_re_rand)
              kds_harv <- kds_harv %>% dplyr::mutate(temp=paste0(Group,"_",Crop,"_",Measurement))
              kds_harv <- kds_harv %>% dplyr::filter(temp %in% collect_harv)
            }
          }
          #Transform, bind and clean harvest trait data
          kds_harv <- data.table(kds_harv)
          #kds_harv$CropMeasurementPerSeason <- ns_harvest()
          dt_kds<- data.table::rbindlist(list(dt_kds,kds_harv),fill = TRUE)
          dt_kds<- ec_clean_header(dt_kds)
        }
        
        ################### END TRAIT LIST SHEET FOR CROP MEASUREMENTS AND MANAGEMENT PRACTICES ######################### 
        
        
        ###################### START ADDING  EXTRA VARIABLES ###########################################################
        dt_extra_vars <- ec_clean_header(extra_variables)
        dt_kds<-rbindlist(list(dt_kds,dt_extra_vars),fill = TRUE)
        ###################### END ADDING EXTRA VARIABLES ########################################################
        
        
        ############### START TRAIT LIST SHEET FOR CROP MEASUREMENTS AND MANAGEMENT PRACTICES ############################
        
        
        lbl_traitlist_dt <- c("Crop","Group","Subgroup","Measurement","TraitName",
                              "TraitUnit",
                              "NumberofMeasurementsPerSeason","NumberofMeasurementsPerPlot",
                              "TraitAlias",
                              "TraitDataType","TraitValidation","VariableId")
        dt_kds <- dt_kds[,lbl_traitlist_dt]
        
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
        
        ################# END TRAIT LIST SHEET FOR CROP MEASUREMENTS AND MANAGEMENT PRACTICES ############################
        
        print("inicio18")  
        incProgress(19/20,message = "Downloading file...")
        saveWorkbook(wb, file = fname , overwrite = TRUE)
        file.rename(fname, file)
        
        #}################# END ######################
        
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