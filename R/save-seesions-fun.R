# # #Funcion que crea lista de inputs a guardar: Experiment
# inputsExperiment <- function(input, output) {
#   a1 <- a2 <- a3 <- c()
#   b1 <- b2 <- b3 <- b4 <- b5 <- b6 <- b7 <- b8 <- b9 <- b10 <- b11 <- b12 <- c()
#   c1 <- c2 <- c3 <- c4 <- c5 <- c6 <- c7 <- c8 <- c9 <- c10 <- c11 <- c12 <- c13 <- c14 <- c15 <- c16 <- c17 <- c18 <- c()
# 
#   inputRds <- readRDS(paste0(globalpath, "inputId1_v3.rds"))
#   inputRds <- dplyr::filter(as.data.frame(inputRds), tabPanel == "Experiment")
#   df1 <- inputRds[c(4, 5, 6)]
# 
#   # inputs para: Funding agency type
#   if (!is.null(input$designFieldbook_fundAgencyType) && !is.na(input$designFieldbook_fundAgencyType) && length(input$designFieldbook_fundAgencyType) >= 1) {
#     for (i in 1:length(input$designFieldbook_fundAgencyType)) {
#       a1[i] <- paste0("fundName_", i)
#       a2[i] <- "textInput"
#       a3[i] <- "n"
#     }
#     df2 <- data.frame(inputId = a1, type = a2, create = a3, stringsAsFactors = F)
#   } else {
#     df2 <- NULL
#   }
# 
#   # inputs para: Number of project management entities
#   if (!is.null(input$numProjEntity) && !is.na(input$numProjEntity) && input$numProjEntity >= 1) {
#     for (i in 1:input$numProjEntity) {
#       b1[i] <- paste0("projEntity_", i)
#       b2[i] <- paste0("projEntity_", i, "_other")
#       b3[i] <- paste0("contCenter_", i)
#       b4[i] <- paste0("contCRP_", i)
# 
#       b5[i] <- "selectizeInput"
#       b6[i] <- "textInput"
#       b7[i] <- "selectizeInput"
#       b8[i] <- "selectizeInput"
# 
#       b9[i] <- "n"
#       b10[i] <- "n"
#       b11[i] <- "n"
#       b12[i] <- "n"
#     }
#     df3 <- data.frame(inputId = c(b1, b2, b3, b4), type = c(b5, b6, b7, b8), create = c(b9, b10, b11, b12), stringsAsFactors = F)
#   } else {
#     df3 <- NULL
#   }
# 
#   # inputs para: Number of experiment leads
#   if (!is.null(input$numLeads) && !is.na(input$numLeads) && input$numLeads >= 1) {
#     for (i in 1:input$numLeads) {
#       c1[i] <- paste0("projLeadEnt_", i)
#       c2[i] <- paste0("tLeadCenter_", i)
#       c3[i] <- paste0("lead_org_type_1_", i)
#       c4[i] <- paste0("lead_org_type_1_", i, "_other")
#       c5[i] <- paste0("leadNameOther_", i)
#       c6[i] <- paste0("expLead_", i)
# 
#       c7[i] <- "selectizeInput"
#       c8[i] <- "selectizeInput"
#       c9[i] <- "selectizeInput"
#       c10[i] <- "textInput"
#       c11[i] <- "textInput"
#       c12[i] <- "textInput"
# 
#       c13[i] <- "n"
#       c14[i] <- "n"
#       c15[i] <- "n"
#       c16[i] <- "n"
#       c17[i] <- "n"
#       c18[i] <- "n"
#     }
#     df4 <- data.frame(inputId = c(c1, c2, c3, c4, c5, c6), type = c(c7, c8, c9, c10, c11, c12), create = c(c13, c14, c15, c16, c17, c18), stringsAsFactors = F)
#   } else {
#     df4 <- NULL
#   }
# 
#   res <- rbind(df1, df2, df3, df4)
#   res
# }