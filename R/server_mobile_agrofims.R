#' shiny UI element for HIDAP-KDSMART
#'
#' returns a re-usable user interface element HIDAP-KDSMART
#'
#' @author Ivan Perez, Omar Benites
#' @param type type of ui element; default is a tab in a shiny dashboard
#' @param title display title
#' @param name a reference name
#' @export

##################################################################################################################
############################################# START: KDSMART JOSE ##################################################
##################################################################################################################

server_mobile_agrofims <- function(input, output, session, values){
  sessionVals <- reactiveValues()
  sessionVals$dtkdsmartaux <- data.frame()
  
  sessionpath <- "/home/obenites/AGROFIMS/kdsmart"
  
  
  conexionDB <- function(){
    
    return(dbConnect(MySQL(), user='agrofims', password='cnReOdGjS851TTR140318', dbname='agrofims', host='176.34.248.121'))
  }
  
  selectfbDB <- function() {
    #statusfb <- "subido"
    
    mydb = conexionDB()
    
    query <- paste0("SELECT uniqueId, experimentId, fieldbookId, fieldbookName, registered, modified, status FROM `kdsmart` order by modified desc")

    res <- dbSendQuery(mydb, query)
    res <- fetch(res, n = -1)
    
    #colnames(res) <- c("ID", "Experiment ID", "Fieldbook ID", "User" ,"Date created", "Date modified", "Status")
    colnames(res) <- c("ID", "Experiment ID", "Fieldbook ID", "Fieldbook Name" ,"Date created", "Date modified", "Status")
    
    sessionVals$dtkdsmartaux <- data.frame(res)
    
    return(sessionVals$dtkdsmartaux)
  }
  
  
  ##### Start Modulo: Render KDSmart list in DT #####
  output$dtkdsmart <- DT::renderDataTable({
    DT::datatable(
      #sessionVals$dtkdsmartaux,
      selectfbDB(),
      selection = 'single',
      options = list(
        pageLength = 5
      )
    )
  })
  ##### End Modulo: Render KDSmart list in DT ######
  
  
  # Metodo que duplica csv e inserta en la base de datos
  observeEvent(input$duplicate_file2,{
    
    id <- selectedRow()
    
    newid <- idgenerator()
    
    listOfFiles <- list.files(sessionpath, paste0(id,".csv"),full.names=T)
    file.copy(listOfFiles, paste0(sessionpath,"/",newid,".csv"))
    
    mydb = conexionDB()
    
    #Agregamos el registro del archivo duplicado a la BD
    query <- paste0("INSERT INTO kdsmart SELECT '",newid,
                    "' , experimentId, fieldbookId, user, registered, modified, status, '", id ,"' from kdsmart", 
                    " WHERE uniqueId = '",  id,"'" )

    
    ## Start: Modificamos experiment ID ##
    
    print(paste0(sessionpath,"/",id,'.xlsx'))
    wb <- loadWorkbook(paste0(sessionpath,"/",id,'.xlsx'))
    print("Cargo el libro correctamente.")
    
    c1 <- "A1B21562997519"

    writeData(wb = wb, sheet = "Metadata",  startCol ="B", startRow = 2,  x = c1 )
    
    saveWorkbook(wb, file = paste0(sessionpath,"/",newid,".xlsx"), overwrite = TRUE)
    
    
    # Testing to save directory files in array to pass then as parameter to system lines writing below.
    # files <- c(paste0("/home/obenites/AGROFIMS/kdsmart/",id,".csv"),"/home/obenites/AGROFIMS/kdsmart/Y38UDGXH.xlsx")
    # filesToCompress <- paste(files, collapse = " ")
    
    ## End: Modificamos experiment ID ##    
    
    
    #Convierte a formato kdx
    system("java -jar /home/ubuntu/agrofims2kdx-0.9.2.jar -outdir /home/obenites/AGROFIMS/kdsmart /home/ubuntu/fileNameBook1.xlsx -nogui", FALSE)

    
    #Compress more than one file.
    system(paste0("tar -zcvf /var/www/html/kdsmart/",newid,".tar.gz /home/obenites/AGROFIMS/kdsmart/",id,".csv /home/obenites/AGROFIMS/kdsmart/ZH6ORTVG.xlsx" ), TRUE)
    
    dbSendQuery(mydb, query)
    
    #Compress one file.
    #system(paste0("tar -zcvf /var/www/html/kdsmart/",newid,".tar.gz /home/obenites/AGROFIMS/kdsmart/",id,".csv" ), TRUE)

  })
  
  output$downloadKDX <- downloadHandler(
    filename  = function() {
      # number 4 is fieldbook name.
      paste(sessionVals$dtkdsmartaux[input$dtkdsmart_rows_selected,4],".kdx",sep="")
    },
    content = function(file) {
      
      id = input$dtkdsmart_rows_selected

      # number 4 is fieldbook name.
      newid = sessionVals$dtkdsmartaux[id, 4]

      listOfFiles <- list.files(sessionpath, paste0(newid,".xlsx"),full.names=T)
      
      # START: Save in targz format
      system(paste0("tar -zcvf /var/www/html/kdsmart/",newid,".tar.gz /home/obenites/AGROFIMS/kdsmart/",newid,".xlsx" ), TRUE)
      # END: Save in targz format
      
      # START: Export in kdx format
      wb = loadWorkbook(paste0(sessionpath,"/",newid,'.xlsx'))
      experimentId = readxl::read_xlsx(path = paste0(sessionpath,"/",newid,'.xlsx'),sheet = 1,col_names=FALSE, range="B2")[[1]]
      
      system(paste0("java -jar /home/ubuntu/agrofims2kdx-0.9.2.jar -outdir /home/obenites/AGROFIMS/kdsmart ", listOfFiles ," -nogui"), FALSE)
      file.copy(paste0("/home/obenites/AGROFIMS/kdsmart/",experimentId,".kdx"), file)
      # END: Export in kdx format
      
    }
  )
  
  
  output$downloadExcel <- downloadHandler(
    filename  = function() {
      # number 4 is fieldbook name.
      paste(sessionVals$dtkdsmartaux[input$dtkdsmart_rows_selected,4],".xlsx",sep="")
    },
    content = function(file) {
      
      id = input$dtkdsmart_rows_selected
      
      # number 4 is fieldbook name.
      newid = sessionVals$dtkdsmartaux[id, 4]
      
      listOfFiles <- list.files(sessionpath, paste0(newid,".xlsx"),full.names=T)
      
      # START: Save in targz format
      system(paste0("tar -zcvf /var/www/html/kdsmart/",newid,".tar.gz /home/obenites/AGROFIMS/kdsmart/",newid,".xlsx" ), TRUE)
      # END: Save in targz format
      
      # START: Export in kdx format
      wb = loadWorkbook(paste0(sessionpath,"/",newid,'.xlsx'))
      experimentId = readxl::read_xlsx(path = paste0(sessionpath,"/",newid,'.xlsx'),sheet = 1,col_names=FALSE, range="B2")[[1]]
      
      system(paste0("java -jar /home/ubuntu/agrofims2kdx-0.9.2.jar -outdir /home/obenites/AGROFIMS/kdsmart ", listOfFiles ," -nogui"), FALSE)
      file.copy(paste0("/home/obenites/AGROFIMS/kdsmart/",newid,".xlsx"), file)
      # END: Export in kdx format
      
    }
  )
  
  
  observeEvent(input$refreshsession2,{
    output$dtkdsmart <- DT::renderDataTable({
      DT::datatable(
        #sessionVals$dtkdsmartaux,
        selectfbDB(),
        selection = 'single',
        options = list(
          pageLength = 5
        )
      )
    })
  })
  

  # output$downloadKDX <- downloadHandler(
  #   filename = function() {
  #     paste("data-", Sys.Date(), ".csv", sep="")
  #   },
  #   content = function(file) {
  #     write.csv(iris, file)
  #   }
  # )

  #Evento reactivo que captura id de la fila seleccionada 
  selectedRow <- eventReactive(input$duplicate_file2, {
    id <- input$dtkdsmart_rows_selected
    sessionVals$dtkdsmartaux[id, 1]
  })

  idgenerator <- function() {
    id <- stri_rand_strings(1, 8,  '[A-Z0-9]')
    id
  }
  
  convertKDX <- function(fileString){
    
    system(paste0("java -jar /home/ubuntu/agrofims2kdx-0.9.2.jar -outdir /home/obenites/AGROFIMS/kdsmart ", filestring, " -nogui"), FALSE)
    
  }
  
  ##################################################################################################################
  ############################################# END: KDSMART JOSE ##################################################
  ##################################################################################################################
  
  # # path global para lectura de RDS's
  # globalpath <- "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/www/internal_files/"
  # 
  # # path global donde se aloja las sessiones y backups
  # sessionpath <- "/home/obenites/AGROFIMS/savesession/"
  # sessionpathbk <- "/home/obenites/AGROFIMS/savesession_bk/"
  # 
  # ##### Start Modulo: Refresh list #####
  # my_files <- function() {
  #   lf <- list.files(sessionpath)
  #   lf
  # }
  # 
  # sessionVals <- reactiveValues()
  # sessionVals$aux <- data.frame()
  # 
  # refreshDT <- function() {
  #   df <- data.frame()
  #   a <- b <- c <- d <- e <- f <- g <- c()
  #   
  #   if (length(my_files()) >= 1) {
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
  #     df <- dplyr::filter(as.data.frame(df), g == userM)
  #     df <- df %>% dplyr::arrange(desc(f))
  #     
  #     sessionVals$aux <- data.frame(df)
  #     colnames(sessionVals$aux) <- c("ID", "Experiment ID", "Experiment name", "Experiment project name", "Date created", "Date modified", "User")
  #   } else {
  #     
  #     sessionVals$aux <- data.frame()
  #   }
  # }
  # 
  # observeEvent(input$refreshsession, {
  #   refreshDT()
  # })
  # ##### End Modulo: Refresh list ######
  # 
  # ##### Start Modulo: Render session list in DT #####
  # output$dtsession <- DT::renderDataTable({
  #   DT::datatable(
  #     sessionVals$aux, 
  #     #iris,
  #     selection = 'single',
  #     options = list(
  #       pageLength = 5,
  #       columnDefs = list(list(visible=FALSE, targets=c(1, 7)))
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
  # # Load session
  # loadsession <- function() {
  #   if (length(selectedRow() != 0)) {
  #     if (file.exists(isolate(paste0(sessionpath, selectedRow(), ".csv")))){
  #       uploaded_inputs <- read.csv(paste0(sessionpath, selectedRow(), ".csv"))
  #       #print(uploaded_inputs)
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
  # # Funcion que devuelve valores de un array para la funcion Load session
  # getInputs<- function(valor, q){
  #   valor <- sapply(valor, as.character)
  #   valor[is.na(valor)] <- " "
  #   valor
  #   
  #   if (stringr::str_detect(valor, "&")) {
  #     if (q == "start") {
  #       valor <- unlist(strsplit(valor, "&"))
  #       valor <- valor[[1]]
  #     }
  #     
  #     if (q == "end") {
  #       valor <- unlist(strsplit(valor, "&"))
  #       valor <- valor[[2]]
  #     }
  #   }
  #   
  #   if(stringr::str_detect(valor,"&")){
  #     valor<-unlist(strsplit(valor, "&"))
  #   } else {
  #     valor<-valor
  #   }
  #   
  #   valor
  # }
  # 
  # #Load session
  # observeEvent(input$load_inputs, {
  #   loadsession()
  # })
  # ##### End Modulo: Load fieldbook ######
  # 
  # ##### Start Modulo: Delete fieldbook ######
  # observeEvent(input$delete_file, {
  #   id <- input$dtsession_rows_selected
  #   i <- sessionVals$aux[id,1]
  #   r <- paste0(sessionpath, i, ".csv")
  #   
  #   if (file.exists(r)) {
  #     unlink(r)
  #     #onclick("refreshsession")
  #     shinyalert("Delete successfully", type = "success", timer = 1500, showConfirmButton = F)
  #     refreshDT()
  #   }
  # })
  # ##### End Modulo: Delete fieldbook ######
  # 
  # ##### Start Modulo: Duplicate fieldbook ######
  # # funcion que genera el ID del fieldbook
  # idgenerator <- function() {
  #   id <- stri_rand_strings(1, 8,  '[A-Z0-9]')
  #   id
  # }
  # 
  # # Duplica el fieldbook
  # observeEvent(input$duplicate_file, {
  #   id <- input$dtsession_rows_selected
  #   i <- sessionVals$aux[id,1]
  #   r <- paste0(sessionpath, i, ".csv")
  #   
  #   if (file.exists(r)) {
  #     newId <- idgenerator()
  #     newFile <- file.copy(r, paste0(sessionpath, newId, ".csv"))
  #     
  #     df <- as.data.frame(read.csv(paste0(sessionpath, newId, ".csv"), stringsAsFactors = F))
  #     df[2,4] <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
  #     df[3,4] <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
  #     df[4,4] <- newId
  #     value <- df[6,4]
  #     df[6,4] <- paste0(value, " - copy")
  #     
  #     write.csv(df, paste0(sessionpath, newId, ".csv"), row.names = FALSE)
  #     write.csv(df, paste0(sessionpathbk, newId, ".csv"), row.names = FALSE)
  #     
  #     shinyalert("Duplicate successfully", type = "success", timer = 1500, showConfirmButton = F)
  #     refreshDT()
  #   }
  # })
  # ##### End Modulo: Duplicate fieldbook ######
  # 
  # ##### Start Modulo: Download fieldbook ######
  # # funcion que genera el ID del fieldbook
  # # idgenerator <- function() {
  # #   id <- stri_rand_strings(1, 8,  '[A-Z0-9]')
  # #   id
  # # }
  # 
  # # Download fieldbook
  # observeEvent(input$download_file, {
  #   # id <- input$dtsession_rows_selected
  #   # i <- sessionVals$aux[id,1]
  #   # r <- paste0(sessionpath, i, ".csv")
  #   # 
  #   # if (file.exists(r)) {
  #   #   download.file(r, "filedescarga.csv")
  #   #   # newId <- idgenerator()
  #   #   # newFile <- file.copy(r, paste0(sessionpath, newId, ".csv"))
  #   #   # 
  #   #   # df <- as.data.frame(read.csv(paste0(sessionpath, newId, ".csv"), stringsAsFactors = F))
  #   #   # df[2,4] <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
  #   #   # df[3,4] <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
  #   #   # df[4,4] <- newId
  #   #   # value <- df[5,4]
  #   #   # df[5,4] <- paste0(value, " - copy")
  #   #   # 
  #   #   # write.csv(df, paste0(sessionpath, newId, ".csv"), row.names = FALSE)
  #   #   # write.csv(df, paste0(sessionpathbk, newId, ".csv"), row.names = FALSE)
  #   #   # 
  #   #   # shinyalert("Duplicate successfully", type = "success", timer = 1500, showConfirmButton = F)
  #   #   # refreshDT()
  #   # }
  # })
  # ##### End Modulo: Download fieldbook ######
}