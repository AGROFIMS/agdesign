#' Get inputs for experimental design tab

#' @title Get levels of factors (inputs) inserted by users in AGROFIMS    
#' @param gr1 group 1 for selected factor in UI 
#' @param gr2 group 2 for selected factor in UI 
#' @param gr3 group 3 for selected factor in UI
#  @param input list input from Shiny UI
#  @param output list output parameter from Shiny Server
#' @param napp numeric Number of application
#' @param rtf numeric Rate total producto
#' @param ref numeric Nutrient rate element
#' @param dfr data frame Selected factors builded on a data frame
#' @author Omar Benites
#'  
#fct_inputs_soil <- function(input, output, napp, rtf, ref, dfr) {
fctinputs <- function(gr1= NULL, gr2= NULL, gr3= NULL, napp= 3, rtf, ref, dfr) {
  
    #gr1 <- gr1 #group 1 of factor
    #gr2  <- gr2 #group 2 of factor
    
    if(is.null(gr1) || is.null(gr2) || is.null(gr3)){
      fct_inputs <- list(level = "foo", label= "foo" ) 
    } else {
    fct_inputs <- getTrtInputs(group= gr1, subgroup = gr2, fct = gr3, dfr = dfr) #extract levels and factor for specific factor.
    #Special case for soil factor #####
    gr3 <- gr3 #third group of factor
    if(gr3 =="Number of fertilizer applications"){fct_inputs <- list(level = napp, label= gr3 ) }
    if(gr3 =="Fertilizer product application rate" ||
       gr3 =="Biofertilizer" || gr3 =="Inorganic"||
       gr3 =="Green manure"  || gr3 =="Lime"||
       gr3 =="Organic"){
      fct_inputs <- list(level = rtf, label= gr3)
    }
    if(gr3 =="Nutrient element application rate") {fct_inputs <- list(level = ref, label= gr3 ) }
    #end special case for soil factor  
    fct_inputs
    }
}

#' Check experimental design inputs in AGROFIMS
#' 
#' @author Omar Benites
#' @description Provides messages and warnings related to inputs that would crash the creations of the field trial.
#' @param isFullFct character. Yes: it is full factorial. No: not-full factorial
#' @param nfactors numeric number of factors from Shiny.
#' @param ntrt numeric Number of treatments from Shiny.
#' @param nr numeric Number of replication or blocks
#' @param group1 character vector Group 1 select button from Shiny
#' @param group2 character vector Group 2 select button from Shiny
#' @param group3 character vector Group 3 select button from Shiny
#' @param nlevel numeric vector Number of levels of taginput boxes from Shiny
#' @param level character vector. Values comes from group-taginput boxes from Shiny
#' @param dfr data frame Experimental design
#' @export
# 
# checkUI_design_inputs <- function(isFullFct="Yes", nfactors= NULL, ntrt =NULL, 
#                                   nr=NULL, group1=NULL, group2=NULL, group3=NULL, nfactor, nlevel = NULL, level=NULL, dfr= NULL){
#   
#   lgl <- FALSE #if pass the value
#   
#   if( isFullFct=="Yes" ){ #FULL FACTORIAL
#     
#     if(nfactors==1){ #number of factor in full factorial equal to 1
#       msg <- "Full factorial experiments need at least 2 factors"
#       lgl <- FALSE
#     } else  if(is.null(group1)){ #empty fist combo (group 1)
#       msg <- "Complete factor information in the first selection"
#       lgl <- FALSE
#     } else if(is.null(group2)){ #empty 2 combo (group 2)
#       msg <- "Complete factor information in the second selection"
#       lgl <- FALSE
#     } else if(is.null(group3)){ #empty 3 combo (group 3)
#       msg <- "Complete factor information in the third selection"
#       lgl <- FALSE
#     } else if(is.null(level)){ #empty levels combo 
#       msg <- "Complete at least 2 levels for each factor"
#       lgl <- FALSE
#     }
#     
#     out <- list(msg = msg, lgl = lgl)
#     
#   } else { #NON-FULL FACTORIAL
#     out <- NULL
#   }
#   out
#   
# }


#' @param design character statistical design
#' @param nrep numeric number of replications or blocks
#' @param isFullFct character Is full factorial experiment? Yes or No.
#' @param fnames character vector factor names
#' @param flevels character vector factor levels
#' @param nonfulldfr data frame If isFullFct = "No", user can load a non-full factorial design data.
#' 

#' #' Get groups or factor labels from experimental design tab
#' #' 
#' #' @title get table of groups or factor labels
#' #' @description Get groups or factor labels according user navigate in the experimental design tab
#' #' @param nf numeric number of factors
#' #' @param input shinyObject input argument from shiny server
#' #' @return data frame Table grouped by factor
#' #' @author Omar Benites
#' # usage (get_f())
#' 
#' get_fgrlbl <- function(nf =1, input){
#'   
#'   #i factor, j = group number
#'   grdt<- data.frame(gr1= NA, gr2= NA, gr3= NA)
#'   for(i in 1:nf){
#'     for (j in 1:3){
#'       if(is.null(input[[paste0("sel",i,"_",j)]])){
#'         grdt[i,j] <-"foo"
#'       } else {
#'         grdt[i,j] <- input[[paste0("sel",i,"_",j)]]
#'       }
#'     }
#'   }
#'   
#'   grdt
#'   #grdt$nfactor <- 1:nf #add a oorrelative factor id number to each row 
#'   
#' }  


# if(isFullFct=="No"){
#   nf <- as.numeric(input$nfactors_hdafims_n) # n factors non factorial
#   nr <- as.numeric(input$designFieldbook_agrofims_r_n) # replications factors non factorial
#   
# } else { #full factorial
#   nr <- as.numeric(input$designFieldbook_agrofims_r_y) # n rep yes
#   nf <- as.numeric(input$nfactors_hdafims_y) # n factors yes
# }
# 
# nonf <- as.numeric(input$nfactors_hdafims_n) # n factors non factorial
# nonr <- as.numeric(input$designFieldbook_agrofims_r_n) # replications factors non factorial
# dt <- nonfulldfr #treatmentValues$data #treatments table
# trt <- dt$TREATMENT #treatments
# 

#' design experiments
# design_agfieldbook <- function(design= "RCBD", isFullFct, nrep, fnames, flevels, nonfulldfr){
#   
#  
#   #Is Full or Non-Full Factorial? ###############################################
#   isFullFct <- tolower(isFullFct)
#   #isfullFct <- tollower(input$fullFactorialRB) #Is  factorrial?
#   
#   
#   #Statistical Design ###########################################################    
#   design <- input$designFieldbook_agrofims #experimental design #################
#   #design <- design
#   
#   # Non-Full Factorial ##########################################################  
#  
#   if(isFullFct=="No"){
# 
#     dt <-  nonfulldfr$data #treatments table
#     trt <- dt$TREATMENT #treatments
#     nrep <-
#     
#     #ToDo:
#     if(design=="CRD"){
#       fb <- st4gi::cr.crd(geno = trt, nrep = nonr, nc = 5)$book #fieldbook
#       #names(fb) <-  c("PLOT",  "TREATMENT")
#       #fb <- fb[,-c(2,3)] #remove row and column headers
#     }
#     
#     if(design=="RCBD"){
#       fb <- st4gi::cr.rcbd(geno = trt, nb = nonr, nc = 5)$book #fieldbook
#       #names(fb) <-  c("PLOT", "BLOCK", "TREATMENT")
#       #fb <- fb[,-c(3,4)] #remove row and column headers 
#     }
#     
#     fb
#     # end non full factorial ####################################################
#   } else {
#   # Full Factorial ##############################################################
#   
#     #nf <- as.numeric(input$nfactors_hdafims_y) # n factors yes
#     #nrep <- as.numeric(input$designFieldbook_agrofims_r_y) # n rep yes
#     
#     if(nf==2){ #number of factors :2 
#       fb <- try(st4gi::cr.f(fnames = c(dffinputs$f1inputs$label, dffinputs$f2inputs$label),
#                             flevels = list(dffinputs$f1inputs$level,dffinputs$f2inputs$level), nrep = nr,
#                             design = tolower(design), nc = 5)$book)
#     } else if(nf==3){  #number of factors :3  
#       fb <- try(st4gi::cr.f(fnames = c(dffinputs$f1inputs$label, dffinputs$f2inputs$label, dffinputs$f3inputs$label),
#                             flevels = list(dffinputs$f1inputs$level,dffinputs$f2inputs$level, dffinputs$f3inputs$level),
#                             nrep = nr, design = tolower(design), nc = 5)$book)
#     } else if(nf==4 ){  #number of factors :4 
#       fb <- try(st4gi::cr.f(fnames = c(dffinputs$f1inputs$label, dffinputs$f2inputs$label, dffinputs$f3inputs$label, dffinputs$f4inputs$label),
#                             flevels = list(dffinputs$f1inputs$level,dffinputs$f2inputs$level, dffinputs$f3inputs$level, dffinputs$f4inputs$level),
#                             nrep = nr, design = tolower(design), nc = 5)$book)
#     } else if(nf==5){  #number of factors :5
#       fb <- try(st4gi::cr.f(fnames = c(dffinputs$f1inputs$label, dffinputs$f2inputs$label, dffinputs$f3inputs$label, dffinputs$f4inputs$label, dffinputs$f5inputs$label ),
#                             flevels = list(f1inputs$level,f2inputs$level, f3inputs$level, f4inputs$level, f5inputs$level),
#                             nrep = nr, design = tolower(design), nc = 5)$book)
#   }
#   
#   }
#   #Renaming fieldbook column's header ###############################################   
#   
#   if(is.element("plot", names(fb))){ colnames(fb)[grep("^plot$", colnames(fb))]<-"PLOT" }
#   if(is.element("row", names(fb))){ colnames(fb)[grep("^row$", colnames(fb))]<-"ROW" }
#   if(is.element("col", names(fb))){ colnames(fb)[grep("^col$", colnames(fb))]<-"COL" }
#   if(is.element("geno", names(fb))) { colnames(fb)[grep("^geno$", colnames(fb))]<-"TREATMENT"}
#   if(is.element("treat", names(fb))) { colnames(fb)[grep("^treat$", colnames(fb))]<-"TREATMENT"}
#   if(is.element("block", names(fb))){ colnames(fb)[grep("^block$", colnames(fb))]<-"BLOCK"}
#   
#   if(is.element("ROW", names(fb))) {    fb$ROW <- NULL }
#   if(is.element("COL", names(fb))) {    fb$COL <- NULL }
#   
#   fb
# }
# 










# # Fieldbook design ############################
# 
#    #fb_agrofims <- shiny::reactive({
# 
#    fb_agrofims <- function(){
#      #Design inputs
# 
#      isfullFctl <- input$fullFactorialRB #Is  factorrial?
# 
#      design <- input$designFieldbook_agrofims #experimental design
#      if(design == "CRD")  { design<- "crd"}
#      if(design == "RCBD") { design<- "rcbd"}
#      design <- design
# 
#      # Non Factorial -------------------------------------------------------------------
#      if(isfullFctl == "No"){
# 
#        nonf <- as.numeric(input$nfactors_hdafims_n) # n factors non factorial
#        nonr <- as.numeric(input$designFieldbook_agrofims_r_n) # replications factors non factorial
# 
#        dt <- treatmentValues$data #treatments table
#        trt <- dt$TREATMENT #treatments
# 
#        if(design=="crd"){
#          fb <- st4gi::cr.crd(geno = trt, nrep = nonr, nc = 5)$book #fieldbook
#          #names(fb) <-  c("PLOT",  "TREATMENT")
#          #fb <- fb[,-c(2,3)] #remove row and column headers
#        }
# 
#        if(design=="rcbd"){
#          fb <- st4gi::cr.rcbd(geno = trt, nb = nonr, nc = 5)$book #fieldbook
#          #names(fb) <-  c("PLOT", "BLOCK", "TREATMENT")
#          #fb <- fb[,-c(3,4)] #remove row and column headers
#        }
# 
# 
#        fb
# 
#        # end non full factorial -----------------------------------------------------------
# 
#      }
# 
#      # Full Factorial -----------------------------------------------------------
#      #else if(isfullFctl == "Yes" ){
#      else {
#        nf <- as.numeric(input$nfactors_hdafims_y) # n factors yes
#        nr <- as.numeric(input$designFieldbook_agrofims_r_y) # n rep yes
# 
#        #Factor 1
#        gr1 <- input$sel_1_1 ; sgr1<- input$sel_1_2; sf1<-input$sel_1_3  #gr: group, sgr: subgroup, sf: factor of group-subgroup
#        numfct1 <- input$numLevels_1 # number of levels
#        fct1 <- paste0(gr1, sf1) #factor1 label in the spreadshet
#        lvl1 <- input$levelSelection_1 #Factor-levels 1
#        f1Inputs <- getTrtInputs(group= input$sel1_1, subgroup = input$sel1_2, fct = input$sel1_3, dfr = f1())
#        sf1 <- input$sel1_3
#        if(is.null(input$sel1_3)){
#          sf1 <- "foo"
#        } else{
#          sf1 <- input$sel1_3
#        }
#        if(sf1=="Number of fertilizer applications"){f1Inputs <- list(level = nappf1(), label= sf1 ) }
#        if(sf1 =="Fertilizer product application rate" ||
#           sf1 =="Biofertilizer" ||  sf1=="Inorganic"||
#           sf1 =="Green manure"|| sf1 =="Lime"||
#           sf1 =="Organic"){
#          f1Inputs <- list(level = rtf1(), label= sf1)
#        }
#        if(sf1 =="Nutrient element application rate") {f1Inputs <- list(level = ref1(), label= sf1 ) }
# 
# 
#        #Factor 2
#        gr2 <- input$sel_2_1;  sgr2 <- input$sel_2_2 ; sf2<-input$sel_2_3 #gr: group, sgr: subgroup, sf: factor of group-subgroup
#        numfct2 <- input$numLevels_2 # number of levels
#        fct2 <- paste0(gr1, sf1) #factor2 label in spreadshet
#        lvl2 <- input$levelSelection_2 #Factor-levels 2
#        f2Inputs <- getTrtInputs(group= input$sel2_1, subgroup = input$sel2_2, fct = input$sel2_3, dfr = f2())
#        if(is.null(input$sel2_3)){
#          sf2 <- "foo"
#        } else{
#          sf2 <- input$sel2_3
#        }
# 
#        if(sf2=="Number of fertilizer applications"){f2Inputs <- list(level = nappf2(), label= sf2 ) }
#        if(sf2 =="Fertilizer product application rate" ||
#           sf2 =="Biofertilizer" ||  sf2=="Inorganic"||
#           sf2 =="Green manure"|| sf2 =="Lime"||
#           sf2 =="Organic"){
#          f2Inputs <- list(level = rtf2(), label= sf2)
#        }
#        if(sf2 =="Nutrient element application rate") {f2Inputs <- list(level = ref2(), label= sf2) }
# 
# 
#        #Factor 3
#        gr3 <-input$sel_3_1 ; sgr3 <- input$sel_3_2;  sf3 <- input$sel_3_3 #gr: group, sgr: subgroup, sf: factor of group-subgroup
#        numfct3 <- input$numLevels_3 # number of levels
#        fct3 <- paste0(gr1, sf1) #factor3 label in spreadshet
#        lvl3 <- input$levelSelection_3 #Factor-levels 3
#        f3Inputs <- getTrtInputs(group= input$sel3_1, subgroup = input$sel3_2, fct = input$sel3_3, dfr = f3())
#        if(is.null(input$sel3_3)){
#          sf3 <- "foo"
#        } else{
#          sf3 <- input$sel3_3
#        }
#        if(sf3=="Number of fertilizer applications"){f3Inputs <- list(level = nappf3(), label= sf3  ) }
#        if(sf3 =="Fertilizer product application rate" ||
#           sf3 =="Biofertilizer" ||  sf3=="Inorganic"||
#           sf3 =="Green manure"|| sf3 =="Lime"||
#           sf3 =="Organic"){
#          f3Inputs <- list(level = rtf3(), label= sf3 )
#        }
#        if(sf3 =="Nutrient element application rate") {f3Inputs <- list(level = ref3(), label= sf3 ) }
# 
# 
# 
#        #print("error 2")
#        #Factor 4
#        gr4 <-input$sel_4_1 ; sgr4 <- input$sel_4_2;  sf4 <- input$sel_4_3 #gr: group, sgr: subgroup, sf: factor of group-subgroup
#        numfct4 <- input$numLevels_4 # number of levels
#        lblfct4 <- paste0(gr4, sf4) #factor4 label in spreadshet
#        lvl4 <- input$levelSelection_4#Factor-levels 3
#        f4Inputs <- getTrtInputs(group= input$sel4_1, subgroup = input$sel4_2, fct = input$sel4_3, dfr = f4())
#        #sf4 <- input$sel4_3
#        if(is.null(input$sel4_3)){
#          sf4 <- "foo"
#        } else{
#          sf4 <- input$sel4_3
#        }
#        if(sf4=="Number of fertilizer applications"){f4Inputs <- list(level = nappf4(), label= sf4 ) }
#        if(sf4 =="Fertilizer product application rate" ||
#           sf4 =="Biofertilizer" ||  sf4=="Inorganic"||
#           sf4 =="Green manure"|| sf4 =="Lime"||
#           sf4 =="Organic"){
#          f4Inputs <- list(level = rtf4(), label= sf4)
#        }
#        if(sf4 =="Nutrient element application rate") {f4Inputs <- list(level = ref4(), label= sf4) }
# 
# 
#        #Factor 5
#        gr5 <-input$sel_5_1;  sgr5 <- input$sel_5_2;  sf5 <- input$sel_5_3 #gr: group, sgr: subgroup, sf: factor of group-subgroup
#        numfct5 <- input$numLevels_5 # number of levels
#        lblfct5 <- paste0(gr5, sf5) #factor5 label in spreadshet
#        lvl5 <- input$levelSelection_5#Factor-levels 3
#        f5Inputs <- getTrtInputs(group= input$sel5_1, subgroup = input$sel5_2, fct = input$sel5_3, dfr = f5())
#        #sf5 <- input$sel5_3
#        if(is.null(input$sel5_3)){
#          sf5 <- "foo"
#        } else{
#          sf5 <- input$sel5_3
#        }
#        if(sf5=="Number of fertilizer applications"){f5Inputs <- list(level = nappf5(),label= sf5 ) }
#        if(sf5 =="Fertilizer product application rate" ||
#           sf5 =="Biofertilizer" ||  sf5=="Inorganic"||
#           sf5 =="Green manure"|| sf5 =="Lime"||
#           sf5 =="Organic"){
#          f5Inputs <- list(level = rtf5(), label= sf5)
#        }
#        if(sf5 =="Nutrient element application rate") {f5Inputs <- list(level = ref5(), label= sf5) }
# 
# 
# 
#        #print("error 3")
#        if(nf==2){
#          fb <- try(st4gi::cr.f(fnames = c(f1Inputs$label, f2Inputs$label),
#                                flevels = list(f1Inputs$level,f2Inputs$level), nrep = nr,
#                                design = design, nc = 5)$book)
# 
#        }
#        else if(nf==3){
#          fb <- try(st4gi::cr.f(fnames = c(f1Inputs$label, f2Inputs$label, f3Inputs$label),
#                                flevels = list(f1Inputs$level,f2Inputs$level, f3Inputs$level),
#                                nrep = nr, design = design, nc = 5)$book)
#        }
#        else if(nf==4 ){
#          fb <- try(st4gi::cr.f(fnames = c(f1Inputs$label, f2Inputs$label),
#                                flevels = list(f1Inputs$level,f2Inputs$level, f3Inputs$level, f4Inputs$level),
#                                nrep = nr, design = design, nc = 5)$book)
#        }
#        else if(nf==5){
#          fb <- try(st4gi::cr.f(fnames = c(f1Inputs$label, f2Inputs$label),
#                                flevels = list(f1Inputs$level,f2Inputs$level, f3Inputs$level, f4Inputs$level, f5Inputs$level),
#                                nrep = nr, design = design, nc = 5)$book)
#        }
#     }
# 
#      # print("plot row cols")
#      # print(names(fb))
#      # print(fb)
# 
#      if(is.element("plot", names(fb))){ colnames(fb)[grep("^plot$", colnames(fb))]<-"PLOT" }
#      if(is.element("row", names(fb))){ colnames(fb)[grep("^row$", colnames(fb))]<-"ROW" }
#      if(is.element("col", names(fb))){ colnames(fb)[grep("^col$", colnames(fb))]<-"COL" }
#      if(is.element("geno", names(fb))) { colnames(fb)[grep("^geno$", colnames(fb))]<-"TREATMENT"}
#      if(is.element("treat", names(fb))) { colnames(fb)[grep("^treat$", colnames(fb))]<-"TREATMENT"}
#      if(is.element("block", names(fb))){ colnames(fb)[grep("^block$", colnames(fb))]<-"BLOCK"}
# 
#      if(is.element("ROW", names(fb))) {    fb$ROW <- NULL }
#      if(is.element("COL", names(fb))) {    fb$COL <- NULL }
# 
#      # print("plot row cols 2")
#      # print(names(fb))
#      # print(fb)
#      #
#      fb
#    }
#   #})
# 
# 
# # Fieldbook with traits #######################
# #  fb_agrofims_traits <- reactive({
# 
# fb_agrofims_traits <- function(){
# 
#   fb <- fb_agrofims()
#   #print(fb)
# 
#   trait <- traits_dt()
#   #print(trait)
#   cr<- trait$Crop
#   cm <- trait$`Crop measurement`
#   sb<- trait$Subgroup
#   sc <- trait$Scale
#   sc[is.na(sc)] <- "unitless"
#   #co <- trait$VariableId
#   cs <- paste(cr,sb, cm, sc, sep="-")
# 
#   #trait_selected <- trait_agrofims() %>% as.data.frame(stringsAsFactors =FALSE) #unlist(shinyTree::get_selected(input$designFieldbook_traits_agrofims))
#   trait_selected <- cs
#   #print("Trait selected")
#   #print(trait_selected)
# 
#   if(!is.null(trait_selected) || length(trait_selected)==0 ){
#     mm  <-  matrix(nrow = nrow(fb), ncol = length(trait_selected) )
#     nm  <-  c(names(fb), trait_selected)
#     fb  <-  cbind(fb, mm)
#     names(fb)  <-  nm
#   }
# 
#   fb
# }
# 
# #})
# 
# 
# 
# 
# 
# ################# Design ######################################################
# 
# path <- fbglobal::get_base_dir()
# # field operations as list of factors
# fp <- file.path(path, "listFactors_v6.rds")
# 
# # para guardar lista de comboboxes para la tabla en treatment description
# lvl <- reactiveValues()
# factors <- as.data.frame(readRDS(fp))
# lvl$lv_1_1 <- unique(factors$GROUP)
# lvl$lv_1_2 <- NULL
# lvl$lv_1_3 <- NULL
# 
# lvl$lv_2_1 <- unique(factors$GROUP)
# lvl$lv_2_2 <- NULL
# lvl$lv_2_3 <- NULL
# 
# lvl$lv_3_1 <- unique(factors$GROUP)
# lvl$lv_3_2 <- NULL
# lvl$lv_3_3 <- NULL
# 
# lvl$lv_4_1 <- unique(factors$GROUP)
# lvl$lv_4_2 <- NULL
# lvl$lv_4_3 <- NULL
# 
# lvl$lv_5_1 <- unique(factors$GROUP)
# lvl$lv_5_2 <- NULL
# lvl$lv_5_3 <- NULL
# 
# 
# ### para la tabla del treatment description cuando
# ### no es full factorial
# treatmentValues <- reactiveValues()
# 
# ## cambia de titulo de bloques a repliacaciones y viceversa cuando se escoge CRD o RCBD
# observeEvent(input$designFieldbook_agrofims, {
#   if(input$designFieldbook_agrofims =="CRD"){
#     updateSelectInput(session,"designFieldbook_agrofims_r_y", label ="Replications")
#     updateSelectInput(session,"designFieldbook_agrofims_r_n", label ="Replications")
#   }
#   else if(input$designFieldbook_agrofims =="RCBD"){
#     updateSelectInput(session,"designFieldbook_agrofims_r_y", label ="Blocks")
#     updateSelectInput(session,"designFieldbook_agrofims_r_n", label ="Blocks")
#   }
# 
# })
# 
# ### reactivo cuando se selecciona si es full factorial o no
# observeEvent(input$fullFactorialRB, {
# 
# 
#   ## titulo de blocks segun el diseno estadistico
#   rep_title <- ""
#   if(input$designFieldbook_agrofims =="CRD"){
#     rep_title <- "Replications"
#   }
#   else if(input$designFieldbook_agrofims =="RCBD"){
#     rep_title <- "Blocks"
#   }
# 
#   ## verificando si es o no full factorial
#   if(input$fullFactorialRB == "Yes"){
# 
#     end <- numFactors$numNotFull
# 
#     for(num in 1:end){
#       removeUI(
#         selector = paste0("#not_full_factor_box_", num),
#         immediate = T
#       )
#     }
#     numFactors$numNotFull <- 0
# 
#     removeUI(
#       selector="#not_fluid_full_factor",
#       immediate = T
#     )
# 
#     insertUI(
#       selector = "#fluid_treatment_description",
#       where = "afterBegin",
#       ui = fluidRow( id= "fluid_full_factor",
#                      column(width = 12,
#                             column(width = 6,
#                                    selectInput(inputId = "nfactors_hdafims_y", label = "Number of factors", choices = 1:5, 1)
#                             ),
#                             column(width = 6,
#                                    shiny::selectInput("designFieldbook_agrofims_r_y", rep_title , 2:1000, 2 ) #issues16: https://github.com/AGROFIMS/hagrofims/issues/16
#                             ),
# 
#                             fluidRow(id="full_factor_input")
#                      )
#       )
# 
#     )
# 
#     treatmentValues$data <- data.table(c("", ""), # treatment
#                                        c("", ""), # factor 1
#                                        c("", ""), # factor 2
#                                        c("", ""), # factor 3
#                                        c("", ""), # factor 4
#                                        c("", ""), # factor 5
#                                        c("", ""), # factor 1 seleted/wirtten value
#                                        c("", ""), # factor 2 seleted/wirtten value
#                                        c("", ""), # factor 3 seleted/wirtten value
#                                        c("", ""), # factor 4 seleted/wirtten value
#                                        c("", "")  # factor 5 seleted/wirtten value
#     )
# 
#     treatmentValues$baseRow <- c("-", "", "", "", "", "", "", "", "", "", "") ## base row when adding one to treatment table
# 
#     colnames(treatmentValues$data) <-  c('TREATMENT', 'FACTOR 1', 'FACTOR 2', 'FACTOR 3', 'FACTOR 4','FACTOR 5', "val1", "val2", "val3", "val4","val5")
# 
#   }
#   else if(input$fullFactorialRB == "No"){
# 
#     aux <- numFactors$numFull +1
# 
#     updateSelectInput(session, "nfactors_hdafims_y", selected = aux)
# 
# 
#     end <- numFactors$numFull
# 
#     end <- end+1
# 
#     for(num in 1:end){
#       removeUI(
#         selector = paste0("#full_factor_box_", num),
#         immediate = T
#       )
#     }
# 
#     numFactors$numFull <- 0
# 
# 
#     removeUI(
#       selector="#fluid_full_factor",
#       immediate = T
#     )
# 
# 
#     insertUI(
#       selector = "#fluid_treatment_description",
#       where = "afterBegin",
#       ui = fluidRow( id= "not_fluid_full_factor",
#                      column(width = 12,
#                             column(width = 4,
#                                    selectizeInput(inputId = "nfactors_hdafims_n", label = "Number of factors",  choices = 1:5, 1)
#                             ),
#                             column(width = 4,
#                                    shiny::selectInput("designFieldbook_agrofims_t_n", "Number of treatments", 2:100, 2 )
#                             ),
#                             column(width = 4,
#                                    shiny::selectInput("designFieldbook_agrofims_r_n", rep_title, 2:1000, 2 ) #issue 16: https://github.com/AGROFIMS/hagrofims/issues/16
#                                    # selectInput(inputId = "nfactors_hdafims_n", label = "Number of factors", choices = 1:5)
#                             ),
#                             fluidRow(id="not_full_factor_input"),
#                             br(),
# 
#                             column(12,h2("Level Selection"),
#                                    dataTableOutput("Table_treatments"),
#                                    tags$head(
#                                      tags$script("$(document).on('change', '.select_treatment', function () {
#                                                  Shiny.onInputChange('treatmentValueClickId',this.id);
#                                                  Shiny.onInputChange('treatmentValueSelected',this.value);
#                                                  Shiny.onInputChange('treatmentValueClick', Math.random())
#   });"
#                                        ),
# 
#                                      tags$script("$(document).on('change', '.input_treatment', function() {
#                                                  Shiny.onInputChange('treatmentValueButttonClickId',this.id);
#                                                  Shiny.onInputChange('treatmentValueButttonEntered',this.value);
#                                                  Shiny.onInputChange('treatmentValueButttonClick', Math.random())
#                                                  this.blur();
# });"
#                                        )
#                                    )
#                                    )
# 
# 
#                                      )
#                             )
#                             )
# 
#   }
# 
#   deleteAllTabsSoilFertility() ## cleaning all soil fertility tabs
# 
#   })
# 
# 
# ### variables para manejo de el numero de factores seleccionados
# numFactors <- reactiveValues()
# numFactors$numFull <- 0
# numFactors$numNotFull <- 0
# 
# ## variables para numero de treatments seleccionados en NOT FULL FACTORIAL
# num <- reactiveValues()
# num$currNumReplications <- 2 ## valor por defecto
# 
# 
# ### cuando se cambia el numero de factores a YES FULL FACTORIAL
# observeEvent(input$nfactors_hdafims_y, {
# 
#   iter <- as.numeric(input$nfactors_hdafims_y)
# 
#   if(numFactors$numFull < iter){
#     start <- numFactors$numFull + 1
#     for(i in start:iter){
#       drawFullFactorialFactor(i)
#     }
#   }
#   else if(numFactors$numFull > iter){
#     start <- iter+1
#     end <- numFactors$numFull
#     for(num in start:end){
#       removeUI(
#         selector = paste0("#full_factor_box_", num),
#         immediate = T
#       )
#       removeTabSoilFertility(num) ## deleting soil fertility tab if exists
#     }
# 
#   }
#   numFactors$numFull <- iter
# })
# 
# ### function para dibujar  box con los select cuando es YES FULL FACTORIAL
# drawFullFactorialFactor <- function(order){
#   insertUI(
#     selector = "#full_factor_input",
#     where = "beforeBegin",
#     ui =fluidRow(id = paste0("full_factor_box_", order),
#                  column(width = 12,
#                         box(title = paste0("#", order, " Factor"),
#                             width = 12,
#                             solidHeader = TRUE, status = "warning",
#                             column(width = 12,
#                                    fluidRow(
#                                      column( width = 6,
#                                              fluidRow(
#                                                fluidRow(
#                                                  column(width = 4,
#                                                         selectizeInput(paste0("sel", order, "_1"), "", choices = lvl[[paste0("lv_", order, "_1")]], multiple =T, options = list(maxItems =1, placeholder ="Select..."))
#                                                  ),
#                                                  column(width = 4,
#                                                         selectizeInput(paste0("sel", order, "_2"), "", choices = NULL, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
#                                                  ),
#                                                  column(width = 4,
#                                                         selectizeInput(paste0("sel", order, "_3"), "", choices =  NULL, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
#                                                  )
#                                                )
#                                              )
# 
#                                      ),
#                                      column(width = 6,
#                                             fluidRow(
#                                               column(width = 6,
#                                                      fluidRow(id=paste0("fl_title_factor_aux_", order))
# 
#                                               ),
#                                               column(width = 6,
#                                                      numericInput(paste0("numLevels_", order), HTML("Number of levels"), max = 5, min = 2, value = 2)
#                                               )
#                                             ),
#                                             fluidRow(id= paste0("levelSelection_", order))
#                                      )
#                                    )
#                             )
#                         )
#                  ))
#   )
# 
# }
# 
# 
# ### cuando se cambia el numero de factores a NO FULL FACTORIAL
# observeEvent(input$nfactors_hdafims_n, {
#   iter <- as.integer(input$nfactors_hdafims_n)
# 
#   if(is.na(iter)  || iter < 1 ) return()
# 
#   if(numFactors$numNotFull < iter ){
#     start <- numFactors$numNotFull + 1
#     for(i in start:iter){
#       drawNotFullFactorialFactor(i)
#     }
#   }
#   else if(numFactors$numNotFull > iter){
#     start <- iter+1
#     end <- numFactors$numNotFull
#     for(i in start:end){
#       removeUI(
#         selector = paste0("#not_full_factor_box_", i),
#         immediate = T
#       )
# 
#       convertListToHTMLSelect(i)
#       removeTabSoilFertility(i) ## deleting soil fertility tab if exists
#     }
#   }
#   generateTreatmentStringColumn()
#   numFactors$numNotFull <- iter
# })
# 
# 
# ### function para dibujar  box con los select cuando es NO FULL FACTORIAL
# drawNotFullFactorialFactor <- function(order){
#   insertUI(
#     selector = "#not_full_factor_input",
#     where = "beforeBegin",
#     ui =
#       fluidRow(id = paste0("not_full_factor_box_", order),
#                column(width = 12,
# 
#                       box(
#                         title = paste0("#", order, " Factor"),
#                         width = 12,
#                         solidHeader = TRUE, status = "warning",
# 
#                         column(width = 12,
# 
#                                fluidRow(
#                                  fluidRow(
#                                    column(width = 4,
#                                           selectizeInput(paste0("sel", order, "_1"), "", choices = lvl[[paste0("lv_", order, "_1")]], multiple =T, options = list(maxItems =1, placeholder ="Select..."))
#                                    ),
#                                    column(width = 4,
#                                           selectizeInput(paste0("sel", order, "_2"), "", choices = NULL, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
#                                    ),
#                                    column(width = 4,
#                                           selectizeInput(paste0("sel", order, "_3"), "", choices =  NULL, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
#                                    )
#                                  )
#                                )
#                         )
#                       )
#                )
#       )
#   )
# 
# }
# 
# ### dibujando tabla de treatments cuando es NO FULL FACTORIAL
# output$Table_treatments <-renderDataTable({
#   DT=treatmentValues$data
#   datatable(DT,
#             escape=F,
#             selection = list(mode = 'none'),
#             options = list(
#               searching = F,
#               ordering=F,
#               scrollX = TRUE,
#               pageLength = 10,
#               columnDefs = list(list(className = 'dt-center', width = '15%', targets = 1:6),list(visible=FALSE, targets=7:11) )
#             )
#   )}
# )
# 
# 
# ### event when a option is selected in list inside treatment table in NO FULL FACTORIAL
# observeEvent(input$treatmentValueClick, {
# 
#   var <- input$treatmentValueSelected
# 
#   coords <- gsub("select_factor_treatment_","",input$treatmentValueClickId)
#   coords <- strsplit(coords, "_")[[1]]
# 
#   ## deselecting whichever was selected first
#   sel <- gsub(' selected', "", treatmentValues$data[[as.numeric(coords[1])+1]][as.numeric(coords[2])])
#   ## the update with the selected value
#   sel <- gsub(paste0('<option value="', var,'"'), paste0('<option value="', var,'" selected'), sel)
#   treatmentValues$data[[as.numeric(coords[1])+6]][as.numeric(coords[2])] <- var
#   treatmentValues$data[[as.numeric(coords[1])+1]][as.numeric(coords[2])] <- sel
#   treatmentValues$data[[1]][as.numeric(coords[2])] <-  generateTreatmentString(coords[2])
# })
# 
# ### event when a textbox is written and enter is pressed inside treatment table in NO FULL FACTORIAL
# observeEvent(input$treatmentValueButttonClick, {
#   var <- input$treatmentValueButttonEntered
#   coords <- gsub("input_factor_treatment_","",input$treatmentValueButttonClickId)
#   coords <- strsplit(coords, "_")[[1]]
# 
#   var2 <-paste0('<input id="input_factor_treatment_', coords[1], '_', coords[2], '" class ="input_treatment"  value = "', var, '" style="width:150px;"/>')
#   treatmentValues$data[[as.numeric(coords[1])+1]][as.numeric(coords[2])] <- var2
#   treatmentValues$data[[as.numeric(coords[1])+6]][as.numeric(coords[2])] <- var
#   treatmentValues$data[[1]][as.numeric(coords[2])] <-  generateTreatmentString(coords[2])
# })
# 
# 
# ### funcion que concantena los seleccionados de los factores en la tabla treatment en NO FULL FACTORIAL
# generateTreatmentString <- function(row_index){
#   nfactors <- as.numeric(input$nfactors_hdafims_n)
#   index <- as.numeric(row_index)
#   str <- c()
# 
#   for(i in 1:nfactors){
#     if(treatmentValues$data[[i+6]][index] == ""){
#       str <- c(str, "-")
#     }
#     else{
#       str <- c(str,  treatmentValues$data[[i+6]][index])
#     }
#   }
# 
#   return(paste(str, collapse = "/"))
# }
# 
# 
# ### funcion que genera la columna 'treatment' en la tabla de treatments en NO FULL FACTORIAL
# generateTreatmentStringColumn <- function(){
#   numTreatments <- as.numeric(input$designFieldbook_agrofims_t_n)
# 
#   vals <- c()
#   for( i in 1:numTreatments){
#     vals <- c(vals, generateTreatmentString(i))
#   }
#   treatmentValues$data[1] <- vals
# 
# }
# 
# ### evento cuando se cambia el numero de tratamientos en NO FULL FACTORIAL
# observeEvent(input$designFieldbook_agrofims_t_n, {
#   rep <- as.numeric(input$designFieldbook_agrofims_t_n)
#   if(num$currNumReplications > rep  && !is.na(rep)){
#     start<- rep +1
#     for(i in num$currNumReplications:start){
#       treatmentValues$data <- treatmentValues$data[-i,]
#     }
#     num$currNumReplications <- rep
#   }
#   else if(num$currNumReplications < rep && !is.na(rep)){
#     start  <- num$currNumReplications +1
#     for(i in start:rep){
#       treatmentValues$data <-  rbind(treatmentValues$data, as.list(lapply(treatmentValues$baseRow, function(x) gsub("_NUM", paste0("_", i), x))))
#     }
#     num$currNumReplications <- rep
#   }
# 
# })
# 
# 
# ### genera lista desplegable que se usara en el treatment table en NO FULL FACTORIAL
# convertListToHTMLSelect <- function(index, myList="", form="", colname = ""){
# 
#   if(is.null(input[["fullFactorialRB"]]) || input[["fullFactorialRB"]] == "Yes" ) return()
# 
#   numTreatments <- isolate(input$designFieldbook_agrofims_t_n)
#   numTreatments <-  as.integer(numTreatments)
# 
#   factor_sel_1 <- input[[paste0("sel", index, "_1")]]
# 
#   if(is.null(factor_sel_1) || !is.integer(numTreatments) || numTreatments  < 1 ) return()
#   if(!is.null(input[[paste0("sel", index, "_3")]])) colname <- input[[paste0("sel", index, "_3")]]
# 
#   ans <- c() ## list for the factor column in table
#   ans2 <- c() ## for selected values - hidden column
#   opt <- NULL
# 
#   str <- ""
#   base <- ""
#   base_2 <-""
# 
#   for(i in 1:numTreatments){
#     ans2 <- c(ans2, "")
#   }
# 
#   if(factor_sel_1 == "Soil fertility"){
#     ans2 <- c()
#     nLevels <- input[[paste0("numLevels_tabSoil_", index)]]
#     base <- paste0('<select id="select_factor_treatment_', index, '_NUM" class ="select_treatment" style="width:150px;">')
# 
#     if(is.null(nLevels)) nLevels <- 1
#     options_str <- ""
# 
#     for(i in 1:nLevels){
#       options_str <- paste0(options_str, '<option value="Level ', i,'">Level ', i, '</option>')
#     }
# 
#     for(i in 1:numTreatments){
#       str <- paste0('<select id="select_factor_treatment_', index, '_', i,  '" class ="select_treatment" style="width:150px;">')
#       str <- paste0(str, options_str,"</select>" )
#       ans <- c(ans, str)
#       ans2 <- c(ans2, "Level 1")
#     }
# 
#   }
#   else{
#     if(form == "combo box"){ ## is a list separated by semicolons
#       opts <- strsplit(myList, ";")[[1]]
#       ans2 <- c()
#       base <- paste0('<select id="select_factor_treatment_', index, '_NUM" class ="select_treatment" style="width:150px;">')
#       base_2 <- opts[1]
# 
#       options_str <- ""
#       for(opt in opts){
#         options_str <- paste0(options_str, '<option value="', opt,'">', opt, '</option>')
#       }
# 
#       base <- paste0(base, options_str,"</select>" )
# 
#       for(i in 1:numTreatments){
#         str <- paste0('<select id="select_factor_treatment_', index, '_', i ,  '" class ="select_treatment" style="width:150px;">')
#         str <- paste0(str, options_str,"</select>" )
#         ans <- c(ans, str)
#         ans2 <- c(ans2, opts[1])
#       }
#     }
#     else if( form=="text input"){
#       base <- paste0('<input id="input_factor_treatment_', index, '_NUM" class ="input_treatment"  value = "" style="width:150px;"/>')
#       for(i in 1:numTreatments){
#         str <- paste0('<input id="input_factor_treatment_', index, '_', i, '" class ="input_treatment"  value = "" style="width:150px;"/>')
#         ans <- c(ans, str)
#       }
#     }
#     else{ ## is a single value
#       str <- myList
#       for(i in 1:numTreatments){
#         ans <- c(ans, str)
#       }
#       ans2 <- ans
#       base <- str
#       base_2 <- str
#     }
#   }
# 
#   if(colname == "") colname <- paste0("FACTOR ", index)
# 
#   treatmentValues$data[index+1] <- ans
#   treatmentValues$data[index+6] <- ans2  ##  reseting hidden values selected
#   colnames(treatmentValues$data)[index+1] <- colname
# 
#   treatmentValues$baseRow[index+1] <- base
#   treatmentValues$baseRow[index+6] <- base_2
# 
#   ## changing base
#   numFactors <- as.numeric(input$nfactors_hdafims_n)
#   end <- numFactors + 6
#   aux <- treatmentValues$baseRow[7:end]
#   treatmentValues$baseRow[1] <- paste(replace(aux, aux == "", "-"), collapse = "/")
# 
#   generateTreatmentStringColumn()
# }
# 
# 
# 
# ### variable to keep track of soils tabs
# numSoilPanels <- reactiveValues()
# numSoilPanels$current <- c()
# numSoilPanels$levels <- c() ## to control how many levels each tab has
# numSoilPanels$appList <-list() ## to control the list inside comboboxes for applications in soil fertility tabs
# 
# ### function to add tabs for soil fertility
# addTabSoilFertility <- function(index){
# 
#   ind <- match(index, numSoilPanels$current)
#   if(!is.na(ind)) return()
# 
#   len <- length(numSoilPanels$current)
#   mtarget <- "tabTreatmentFactors" ## default if list is empty or the tab goes first
# 
# 
#   numSoilPanels$current <- c(numSoilPanels$current, index)
#   numSoilPanels$levels <- c(numSoilPanels$levels, 0)
#   numSoilPanels$appList[[paste0("f", index)]] <- list("void", c()) ## user has not chosen factor yet
# 
#   aux_sort <- sort(numSoilPanels$current)
# 
#   ind <- match(index, aux_sort)
# 
# 
#   if(is.numeric(ind) && ind != 1){
#     aux <- aux_sort[ind-1]
#     mtarget <- paste0("panelTreatment_soilFertility_",  aux)
#   }
# 
#   insertTab(inputId = "treatmentSetPanel",
#             tabPanel(paste0("Soil fertility details - factor ", index),  value = paste0("panelTreatment_soilFertility_", index),
#                      column(12, br(),
#                             fluidRow(
#                               column(6,
#                                      uiOutput(paste0("uiFactorName_tabSoil_", index))
#                               ),
#                               column(6,
#                                      column(4,
#                                             numericInput(paste0("numLevels_tabSoil_", index), "Levels", min =1, max=100, value=1)
#                                      )
#                               )
#                             ),
#                             fluidRow(id=paste0("fluidRow_levelsTabSoil_", index))
#                      )
#             ),
#             position = "after",
#             target = mtarget
#   )
# 
# }
# 
# ## observe when changing levels at tab soil fertility for factor 1
# observeEvent(input$numLevels_tabSoil_1, {
#   if(!is.null(input$numLevels_tabSoil_1)){
#     print(input$numLevels_tabSoil_1)
#     isolate(
#       drawLevelsSoilTab(1,input$numLevels_tabSoil_1))
#     convertListToHTMLSelect(1)
#   }
# })
# 
# ## observe when changing levels at tab soil fertility for factor 2
# observeEvent(input$numLevels_tabSoil_2, {
#   if(!is.null(input$numLevels_tabSoil_2)){
#     isolate(
#       drawLevelsSoilTab(2,input$numLevels_tabSoil_2))
#     convertListToHTMLSelect(2)
#   }
# 
# })
# 
# ## observe when changing levels at tab soil fertility for factor 3
# observeEvent(input$numLevels_tabSoil_3, {
#   if(!is.null(input$numLevels_tabSoil_3)){
#     isolate(
#       drawLevelsSoilTab(3,input$numLevels_tabSoil_3))
#     convertListToHTMLSelect(3)
#   }
# })
# 
# ## observe when changing levels at tab soil fertility for factor 4
# observeEvent(input$numLevels_tabSoil_4, {
#   if(!is.null(input$numLevels_tabSoil_4)){
#     isolate(
#       drawLevelsSoilTab(4,input$numLevels_tabSoil_4))
#     convertListToHTMLSelect(4)
#   }
# })
# 
# ## observe when changing levels at tab soil fertility for factor 5
# observeEvent(input$numLevels_tabSoil_5, {
#   if(!is.null(input$numLevels_tabSoil_5)){
#     isolate(
#       drawLevelsSoilTab(5,input$numLevels_tabSoil_5))
#     convertListToHTMLSelect(5)
#   }
# })
# 
# ## function to draw and remove box levels in soil fertility tabs
# drawLevelsSoilTab <- function(index, levels){
#   if(!is.numeric(levels) || levels < 1) return()
# 
#   ind <- match(index, numSoilPanels$current)
#   mlevels <- numSoilPanels$levels[ind]
#   if(!is.numeric(mlevels)) return()
# 
#   if(mlevels < levels){
#     start <- mlevels + 1
#     for(i in start:levels){
#       drawBoxLevelTabSoil(index, i)
#     }
#   }
#   else if(mlevels > levels){
#     removeBoxeLevelTabSoil(index, levels + 1, mlevels)
#   }
#   numSoilPanels$levels[ind] <- levels
# }
# 
# ## ui of box levels for soil fertility tabs
# drawBoxLevelTabSoil <- function(index, level){
#   box_id <- paste0("box_level_soilTab_", index, "_", level)
#   insertUI(selector =paste0("#fluidRow_levelsTabSoil_", index),
#            where = "beforeBegin",
#            ui =
#              fluidRow(id= box_id,
#                       box( title = paste0("Level ", level),
#                            width = 12,
#                            solidHeader = TRUE, status = "warning",
#                            fluidRow(
#                              column(2),
#                              column(10,
#                                     column(4),
#                                     column(4),
#                                     column(4,
#                                            selectInput(paste0("numApps_tabSoil_factor_", index, "_box_", level), "# of applications", choices = 1:6, selected = 3)
#                                     )
#                              )
#                            ),
#                            fluidRow(
#                              column(2, HTML("<center>"), h4(" "), HTML("</center>")),
#                              column(10,
#                                     column(3, HTML("<center>"),
#                                            fluidRow(
#                                              column(12,h4("Fertilizer product"), style = "padding: 0px; text-align:center;")
#                                              # column(6, style="padding-left:5px; text-align:left;" ,checkboxInput(paste0("checkb_product_", index, "_level_", level), ""))
#                                            ),
#                                            HTML("</center>")
#                                     ),
#                                     column(3, HTML("<center>"), h4("Fertilizer product rate (kg/ha)"), HTML("</center>")),
#                                     column(3, HTML("<center>"),
#                                            fluidRow(
#                                              column(12,h4("Nutrient element"), style = "padding: 0px; text-align:center;")
#                                              # column(6, style="padding-left:5px; text-align:left;" ,checkboxInput(paste0("checkb_element_", index, "_level_", level), ""))
#                                            ),
#                                            HTML("</center>")
#                                     ),
#                                     column(3, HTML("<center>"), h4("Nutrient element rate (kg/ha)"), HTML("</center>"))
#                              )
#                            ),
#                            conditionalPanel(paste0("parseInt(input.numApps_tabSoil_factor_", index, "_box_", level, ")  >=1 ") ,
#                                             drawApplicationRowSoilTab(1, index, level)
#                            ),
#                            conditionalPanel(paste0("parseInt(input.numApps_tabSoil_factor_", index, "_box_", level, ") >= 2 ") ,
#                                             drawApplicationRowSoilTab(2, index, level)
#                            ),
#                            conditionalPanel(paste0("parseInt(input.numApps_tabSoil_factor_", index, "_box_", level, ") >= 3 "),
#                                             drawApplicationRowSoilTab(3, index, level)
#                            ),
#                            conditionalPanel(paste0("parseInt(input.numApps_tabSoil_factor_", index, "_box_", level, ") >= 4 ") ,
#                                             drawApplicationRowSoilTab(4, index, level)
#                            ),
#                            conditionalPanel(paste0("parseInt(input.numApps_tabSoil_factor_", index, "_box_", level, ") >= 5"),
#                                             drawApplicationRowSoilTab(5, index, level)
#                            ),
#                            conditionalPanel(paste0("parseInt(input.numApps_tabSoil_factor_", index, "_box_", level, ") >= 6"),
#                                             drawApplicationRowSoilTab(6, index, level)
#                            ),
# 
# 
#                            fluidRow(id= paste0("fluidRow_soilTab_factor_", index, "_level_", level)),
#                            fluidRow(
#                              column(2),
#                              column(10,
# 
#                                     column(3,br(),HTML("<div style='text-align:center;'>"), h5("Total calculated application:"), HTML("</div>")),
#                                     column(3, disabled(textInput(paste0("input_product_RateTotal_factor_", index, "_level_", level), ""))),
#                                     # column(3,
#                                     #        fluidRow(
#                                     #          column(9,
#                                     #                 textInput(paste0("input_product_RateTotal_factor_", index, "_level_", level), "")
#                                     #          ),
#                                     #          column(3,style=" text-align:left; padding-left:2px;",br(),
#                                     #                 actionButton(paste0("buttonSoilTab_product_RateTotal_factor_", index, "_level_", level), "", icon =icon("calculator"))
#                                     #                 )
#                                     #        )
#                                     # ),
#                                     column(3),
#                                     column(3,disabled(textInput(paste0("input_element_RateTotal_factor_", index, "_level_", level), "")))
# 
#                                     # column(3,
#                                     #        fluidRow(
#                                     #          column(9,
#                                     #                 textInput(paste0("input_element_RateTotal_factor_", index, "_level_", level), "")
#                                     #          ),
#                                     #          column(3,style=" text-align:left; padding-left:2px;",br(),
#                                     #                 actionButton(paste0("buttonSoilTab_element_RateTotal_factor_", index, "_level_", level), "", icon =icon("calculator"))
#                                     #          )
#                                     #        )
#                                     # )
#                              )
#                            )
#                       )
#              )
#   )
# }
# 
# drawApplicationRowSoilTab <- function(napp,index, level){
#   fluidRow(
#     column(2, br(),HTML("<center>"), h5(paste0("Application ", napp)), HTML("</center>")),
#     column(10,
#            column(3,
#                   fluidRow(id = paste0("fr_selectProductRef_factor_", index, "_level_", level, "_app_", napp)),
#                   fluidRow( id = paste0("fr_selectProduct_factor_", index, "_level_", level, "_app_", napp), column(12,
#                                                                                                                     selectizeInput(paste0("select_product_factor_", index, "_level_", level, "_app_", napp), "",
#                                                                                                                                    getList(numSoilPanels$appList[[paste0("f", index)]][[2]]),  multiple = T, options = list(placeholder ="Select..."))
#                   ))
#            ),
#            column(3,
#                   textInput(paste0("input_tabSoil_rate_product_", index, "_level_", level, "_app_", napp), "")
#            ),
#            column(3,
#                   selectizeInput(paste0("input_element_factor_", index, "_level_", level, "_app_", napp), "",multiple = T, options = list(placeholder ="Select..."),
#                                  choices = c("Nitrogen",
#                                              "Phosphorus",
#                                              "Potassium",
#                                              "Boron",
#                                              "Calcium",
#                                              "Copper",
#                                              "Iron",
#                                              "Manganese",
#                                              "Molybdenum",
#                                              "Sulfur",
#                                              "Zinc",
#                                              "Other"
#                                  )
#                   )
#            ),
#            column(3,
#                   textInput(paste0("input_tabSoil_rate_element_", index, "_level_", level, "_app_", napp), "")
#            )
#     )
#   )
# }
# 
# # design
# observeEvent(input$calculateTabSoil,{
#   aux_vals <- strsplit(input$calculateTabSoilButtonId ,"_")[[1]]
#   index  <- aux_vals[5]
#   level <- aux_vals[7]
#   type <- aux_vals[4]
# 
#   napps = as.numeric(input[[paste0("numApps_tabSoil_factor_" , index,"_box_" , level)]])
#   values = list()
#   lens = list()
#   results = list()
#   max_len = 0
#   sum <- ""
# 
#   #print(aux_vals)
#   #print(index)
# 
#   if(napps >0){
#     for(i  in 1:napps){
#       in_id = paste0("input_tabSoil_rate_", type ,"_" , index , "_level_" , level , "_app_" , i)
#       #print(in_id)
#       inp <- input[[in_id]]
#       if(inp != ""){
#         values[[paste0("v", i)]] <- strsplit(inp, ":")
#         lens[[paste0("v", i)]] <- length(values[[paste0("v", i)]][[1]] )
#       }
#       else{
#         values[[paste0("v", i)]] = ""
#         lens[[paste0("v", i)]] = 0
#       }
#       if(max_len < lens[[paste0("v", i)]]) max_len = lens[[paste0("v", i)]]
#     }
# 
#     if(max_len != 0 ){
# 
#       for(i in 1:max_len){
# 
#         results[[paste0("v", i)]] <- 0
#         for(j in 1:napps){
#           if(lens[[j]] >= i){
#             num <- as.integer(values[[paste0("v",j)]][[1]][i])
#             if(!is.na(num))  results[[paste0("v", i)]] <- num + results[[paste0("v", i)]]
#           }
#         }
#       }
#       sum <- paste(results, collapse = ":")
#     }
# 
#     updateTextInput(session, paste0("input_", type, "_RateTotal_factor_", index, "_level_", level), value= sum)
#   }
# 
# 
# })
# 
# # exp conditions 1
# observeEvent(input$calculateTabSoil2,{
#   aux_vals <- strsplit(input$calculateTabSoil2ButtonId ,"_")[[1]]
#   index  <- aux_vals[5]
#   level <- aux_vals[7]
#   type <- aux_vals[4]
# 
#   #napps = as.numeric(input[[paste0("numApps_tabSoil_factor_" , index,"_box_" , level)]])
#   napps = as.numeric(input[[paste0("soil_fertilizer_num_apps")]])
#   values = list()
#   lens = list()
#   results = list()
#   max_len = 0
#   sum <- ""
# 
#   #print(aux_vals)
#   #print(index)
# 
#   if(napps >0){
#     for(i  in 1:napps){
#       #in_id = paste0("input_tabSoil_rate_", type ,"_" , index , "_level_" , level , "_app_" , i)
#       in_id = paste0("input_productRate_soil_table_row", i)
#       inp <- input[[in_id]]
# 
#       # add
#       # inp <- as.character(inp)
#       # if (is.na(inp)) {
#       #   inp <- ""
#       # }
# 
#       #print(inp)
#       if(inp != ""){
#         values[[paste0("v", i)]] <- strsplit(inp, ":")
#         lens[[paste0("v", i)]] <- length(values[[paste0("v", i)]][[1]] )
#       }
#       else{
#         values[[paste0("v", i)]] = ""
#         lens[[paste0("v", i)]] = 0
#       }
#       if(max_len < lens[[paste0("v", i)]]) max_len = lens[[paste0("v", i)]]
#     }
# 
#     if(max_len != 0 ){
# 
#       for(i in 1:max_len){
# 
#         results[[paste0("v", i)]] <- 0
#         for(j in 1:napps){
#           if(lens[[j]] >= i){
#             num <- as.integer(values[[paste0("v",j)]][[1]][i])
#             if(!is.na(num))  results[[paste0("v", i)]] <- num + results[[paste0("v", i)]]
#           }
#         }
#       }
#       sum <- paste(results, collapse = ":")
#     }
# 
#     #updateTextInput(session, paste0("input_", type, "_RateTotal_factor_", index, "_level_", level), value= sum)
#     updateTextInput(session, "soil_fertilizer_totalAppRate1", value= sum)
# 
#   }
# 
# 
# })
# 
# # exp conditions 2
# observeEvent(input$calculateTabSoil3,{
#   aux_vals <- strsplit(input$calculateTabSoil3ButtonId ,"_")[[1]]
#   index  <- aux_vals[5]
#   level <- aux_vals[7]
#   type <- aux_vals[4]
# 
#   #napps = as.numeric(input[[paste0("numApps_tabSoil_factor_" , index,"_box_" , level)]])
#   napps = as.numeric(input[[paste0("soil_fertilizer_num_apps")]])
#   values = list()
#   lens = list()
#   results = list()
#   max_len = 0
#   sum <- ""
# 
#   #print(aux_vals)
#   #print(index)
# 
#   if(napps >0){
#     for(i  in 1:napps){
#       #in_id = paste0("input_tabSoil_rate_", type ,"_" , index , "_level_" , level , "_app_" , i)
#       in_id = paste0("input_elementRate_soil_table_row_", i)
#       inp <- input[[in_id]]
# 
#       # add
#       # inp <- as.character(inp)
#       # if (is.na(inp)) {
#       #   inp <- ""
#       # }
# 
#       #print(inp)
#       if(inp != ""){
#         values[[paste0("v", i)]] <- strsplit(inp, ":")
#         lens[[paste0("v", i)]] <- length(values[[paste0("v", i)]][[1]] )
#       }
#       else{
#         values[[paste0("v", i)]] = ""
#         lens[[paste0("v", i)]] = 0
#       }
#       if(max_len < lens[[paste0("v", i)]]) max_len = lens[[paste0("v", i)]]
#     }
# 
#     if(max_len != 0 ){
# 
#       for(i in 1:max_len){
# 
#         results[[paste0("v", i)]] <- 0
#         for(j in 1:napps){
#           if(lens[[j]] >= i){
#             num <- as.integer(values[[paste0("v",j)]][[1]][i])
#             if(!is.na(num))  results[[paste0("v", i)]] <- num + results[[paste0("v", i)]]
#           }
#         }
#       }
#       sum <- paste(results, collapse = ":")
#     }
# 
#     #updateTextInput(session, paste0("input_", type, "_RateTotal_factor_", index, "_level_", level), value= sum)
#     updateTextInput(session, "soil_fertilizer_totalAppRate2", value= sum)
# 
#   }
# 
# 
# })
# 
# 
# 
# ### function to remove box levels for soil fertility tab
# removeBoxeLevelTabSoil <- function(index, start, end){
#   for(i in start:end){
#     box_id <- paste0("#box_level_soilTab_", index, "_", i)
#     removeUI(
#       selector = box_id,
#       immediate = T,
#       session = getDefaultReactiveDomain()
#     )
#   }
# }
# 
# ### function to remove tabs for soil fertility
# removeTabSoilFertility <- function(index){
# 
#   ind <- match(index, numSoilPanels$current)
# 
#   if(is.na(ind) || ind < 0){ return() }
# 
#   removeTab(inputId = "treatmentSetPanel",
#             target= paste0("panelTreatment_soilFertility_", index)
#   )
# 
#   numSoilPanels$current <- numSoilPanels$current[-ind]
#   numSoilPanels$levels <- numSoilPanels$levels[-ind]
#   numSoilPanels$appList[[paste0("f", index)]] <- NULL
# }
# 
# ### function to delete all soil fertility detail tabs
# deleteAllTabsSoilFertility <- function(){
#   mlist <- numSoilPanels$current
#   for(val in mlist){
#     removeTabSoilFertility(val)
#   }
# 
# }
# 
# generateListLevelsSoilTab <- function(index, form ="void", values = NULL, factorName = ""){
# 
#   numSoilPanels$appList[[paste0("f", index)]] <- list(form, values)
#   output[[paste0("uiFactorName_tabSoil_", index)]] <- renderUI(h3(paste0("Factor: ", factorName)))
# 
#   numLevels <- input[[paste0("numLevels_tabSoil_",index)]]
# 
#   if(is.numeric(numLevels) && numLevels > 0){
#     for(i in 1:numLevels){
#       ## number of applications static for now
#       numApps <- 6
#       for(j in 1:numApps){
#         select_id <-  paste0("fr_selectProduct_factor_", index, "_level_", i, "_app_", j)
# 
#         ## removing and inserting ui bc  updateSelectizeInput when updating choices to null is not working
#         ## must look for better options
#         updateSelectizeInput(session,paste0("select_product_factor_", index, "_level_", i, "_app_", j), choices = getList(values))
#         # removeUI(
#         #   selector =  paste0("#", select_id),
#         #   immediate = T,
#         #   session = getDefaultReactiveDomain()
#         # )
#         #
#         # insertUI(
#         #   selector = paste0("#fr_selectProductRef_factor_", index, "_level_", i , "_app_", j),
#         #   where = "afterEnd",
#         #   ui = fluidRow( id = select_id,
#         #                  selectizeInput(paste0("select_product_factor_", index, "_level_", i, "_app_", j), "",
#         #                                 getList(values), multiple = T, options = list(placeholder ="Select..."))
#         #   )
#         # )
# 
#       }
# 
#     }
#   }
# 
# }
# getList <- function(str){
#   if(is.character(str)) return(unlist(strsplit( str, ";")))
#   else{ return(c(""))}
# }
# 
# 
# ################# fin design ######################################################
# 
# 
# ###########################################################
# 
# # featNames <- names(Agronomic_features$`Agronomic features`)
# 
# 
# 
# 
# # }) End agronomic trait shinyTree  ####################################
# 
# 
# #### factors ####################################################################################
# 
# 
# ## observe when comboboxes of factors are changed
# observe({
#   updateSelectInput(session, "sel1_3", choices = lvl$lv_1_3)
# })
# observe({
#   updateSelectInput(session, "sel1_2", choices = lvl$lv_1_2)
# })
# observe({
#   updateSelectInput(session, "sel2_3", choices = lvl$lv_2_3)
# })
# observe({
#   updateSelectInput(session, "sel2_2", choices = lvl$lv_2_2)
# })
# observe({
#   updateSelectInput(session, "sel3_3", choices = lvl$lv_3_3)
# })
# observe({
#   updateSelectInput(session, "sel3_2", choices = lvl$lv_3_2)
# })
# observe({
#   updateSelectInput(session, "sel4_3", choices = lvl$lv_4_3)
# })
# observe({
#   updateSelectInput(session, "sel4_2", choices = lvl$lv_4_2)
# })
# observe({
#   updateSelectInput(session, "sel5_3", choices = lvl$lv_5_3)
# })
# observe({
#   updateSelectInput(session, "sel5_2", choices = lvl$lv_5_2)
# })
# 
# ## when number of levels are changed for a factor
# observe({
#   if(is.numeric(input$numLevels_1) && input$numLevels_1 > 0){
#     isolate(updateLevelSelection(1))
#   }
# })
# observe({
#   if(is.numeric(input$numLevels_2) && input$numLevels_2 > 0){
#     isolate(updateLevelSelection(2))
#   }
# })
# observe({
#   if(is.numeric(input$numLevels_3) && input$numLevels_3 > 0){
#     isolate(updateLevelSelection(3))
#   }
# })
# observe({
#   if(is.numeric(input$numLevels_4) && input$numLevels_4 > 0){
#     isolate(updateLevelSelection(4))
#   }
# })
# observe({
#   if(is.numeric(input$numLevels_5) && input$numLevels_5 > 0){
#     isolate(updateLevelSelection(5))
#   }
# })
# 
# 
# ## function to draw level selection when is full factorial
# updateLevelSelection <- function(index){
# 
#   sel_1 <- input[[paste0("sel", index, "_1")]]
#   sel_2 <- input[[paste0("sel", index, "_2")]]
#   sel_3 <- input[[paste0("sel", index, "_3")]]
# 
#   if(is.null(sel_1) || is.null(sel_2) || is.null(sel_3)) return()
# 
#   aux <- dplyr::filter(factors,GROUP==sel_1 & SUBGROUP==sel_2 & FACTOR==sel_3)
# 
#   removeUI(selector = paste0("#fl_title_factor_", index), immediate = T)
# 
#   if(nrow(aux) > 0){
# 
# 
#     if(isolate(input$fullFactorialRB == "No" )){
#       if(aux$FORM == "combo box"){
#         convertListToHTMLSelect(index, aux$LEVEL, aux$FORM, sel_3)
#       }
#       else{
#         convertListToHTMLSelect(index, sel_3, aux$FORM, sel_3)
#       }
# 
#     }
# 
#     isolate(
#       if(sel_1 == "Soil fertility"){
#         generateListLevelsSoilTab(index, aux$FORM, aux$LEVEL, sel_3)
# 
#         insertUI(
#           selector = paste0("#fl_title_factor_aux_", index),
#           where = "beforeBegin",
#           ui = fluidRow(id=paste0("fl_title_factor_", index),
#                         column(width = 12, br(),
#                                h4(HTML(paste0("<b>", sel_3, "</b>")))
#                         ),
#                         column(width = 12,
#                                h4(HTML(paste0("<font color='red'><b>", "Add design details using tab at top" , "</b></font>")))
#                         )
#           )
#         )
# 
#       }
#       else{
#         insertUI(
#           selector = paste0("#fl_title_factor_aux_", index),
#           where = "beforeBegin",
#           ui = fluidRow(id=paste0("fl_title_factor_", index), column(width = 12, br(), h4(HTML(paste0("<b>", sel_3, "</b>")))))
#         )
# 
#         if(isolate(is.numeric(input[[paste0("numLevels_", index)]]))){
#           if(aux$FORM == "combo box"){
#             drawComboboxLevel(index, input[[paste0("numLevels_", index)]], aux$LEVEL)
#           }
#           else if(aux$FORM == "text input"){
#             drawTextInputLevel(index, input[[paste0("numLevels_", index)]], aux$UNIT)
#           }
#           else if(aux$FORM == "numeric input"){
#             drawNumericInputLevel(index, input[[paste0("numLevels_", index)]])
#           }
# 
#           else if(aux$FORM == "date"){
#             drawDateLevel(index, input[[paste0("numLevels_", index)]])
#           }
# 
#         }
# 
# 
#       }
#     )
# 
# 
#   }
#   else{
#     removeUI(selector = paste0("#fluid_levels_", index), immediate = T)
#     isolate(if(sel_1 == "Soil fertility") {generateListLevelsSoilTab(index)})
#   }
# 
# }
# 
# auxfunction <- function(index){
# 
# }
# 
# ## cuando se cambia el primer select del primer factor
# observe({
#   if(!is.null(input$sel1_1)){
#     aux <- dplyr::filter(factors,GROUP==input$sel1_1)
#     lvl$lv_1_2 <- unique(aux$SUBGROUP)
#     isolate(
#       if(input$sel1_1 == "Soil fertility"){
#         addTabSoilFertility(1)
#         shinyjs::hide(id="numLevels_1")
#       }
#       else{
#         removeTabSoilFertility(1)
#         shinyjs::show(id="numLevels_1")
#       }
#     )
#   }
#   else{
#     removeTabSoilFertility(1)
#     lvl$lv_1_2 <- NULL
#     shinyjs::show(id="numLevels_1")
#   }
# 
#   lvl$lv_1_3 <- NULL
#   removeUI(selector = "#fluid_levels_1", immediate = T)
#   isolate(convertListToHTMLSelect(1))
#   removeUI( selector ="#fl_title_factor_1", immediate = T )
# 
# })
# ## cuando se cambia el segundo select del primer factor
# observe( {
#   if(!is.null(input$sel1_2)){
#     aux <- dplyr::filter(factors,GROUP==input$sel1_1 & SUBGROUP==input$sel1_2)
#     lvl$lv_1_3 <- unique(aux$FACTOR)
#   }
#   else{
#     lvl$lv1_3 <- NULL
#   }
# 
#   isolate(convertListToHTMLSelect(1))
#   isolate(if(!is.null(input$sel1_1) && input$sel1_1 == "Soil fertility") generateListLevelsSoilTab(1))
#   removeUI(selector = "#fluid_levels_1", immediate = T)
#   removeUI( selector ="#fl_title_factor_1", immediate = T )
# })
# ## cuando se cambia el tercer select del primer factor
# observeEvent(input$sel1_3, {
#   removeUI( selector ="#fl_title_factor_1", immediate = T )
#   if(!is.null(input$sel1_3)){
#     updateLevelSelection(1)
#   }
#   else{
#     isolate(convertListToHTMLSelect(1))
#     removeUI(selector = "#fluid_levels_1", immediate = T)
#     isolate(if(input$sel1_1 == "Soil fertility") {generateListLevelsSoilTab(1)})
#   }
# })
# 
# 
# ## cuando se cambia el primer select del segundo factor
# observe({
#   if(!is.null(input$sel2_1)){
#     aux <- dplyr::filter(factors,GROUP==input$sel2_1)
#     lvl$lv_2_2 <- unique(aux$SUBGROUP)
# 
#     isolate(
#       if(input$sel2_1 == "Soil fertility"){
#         shinyjs::hide(id="numLevels_2")
#         addTabSoilFertility(2)
#       }
#       else{
#         shinyjs::show(id="numLevels_2")
#         removeTabSoilFertility(2)
#       }
#     )
#   }
#   else{
#     lvl$lv_2_2 <- NULL
#     shinyjs::show(id="numLevels_2")
#     removeTabSoilFertility(2)
#     updateSelectInput(session, "sel2_2", choices = NULL)
#   }
#   removeUI(selector = "#fluid_levels_2", immediate = T)
#   lvl$lv_2_3 <- NULL
#   isolate(convertListToHTMLSelect(2))
#   updateSelectInput(session, "sel2_3", choices = NULL)
#   removeUI( selector ="#fl_title_factor_2", immediate = T )
# 
# })
# 
# ## cuando se cambia el segudo select del segundo factor
# observe( {
#   if(!is.null(input$sel2_2)){
#     aux <- dplyr::filter(factors,GROUP==input$sel2_1 & SUBGROUP==input$sel2_2)
#     lvl$lv_2_3 <- unique(aux$FACTOR)
#   }
#   else{
#     lvl$lv_2_3 <- NULL
#   }
#   isolate(convertListToHTMLSelect(2))
#   isolate(if(!is.null(input$sel2_1) && input$sel2_1 == "Soil fertility") {generateListLevelsSoilTab(2)})
#   removeUI(selector = "#fluid_levels_2", immediate = T)
#   removeUI( selector ="#fl_title_factor_2", immediate = T )
# })
# 
# ## cuando se cambia el tercer select del segundo factor
# observeEvent(input$sel2_3,{
#   removeUI( selector ="#fl_title_factor_2", immediate = T )
#   if(!is.null(input$sel2_3)){
#     updateLevelSelection(2)
#   }
#   else{
#     isolate(convertListToHTMLSelect(2))
#     isolate(if(input$sel2_1 == "Soil fertility"){generateListLevelsSoilTab(2)})
#     removeUI(selector = "#fluid_levels_2", immediate = T)
#   }
# })
# 
# ## cuando se cambia el primer select del tercer factor
# observe({
#   if(!is.null(input$sel3_1)){
#     aux <- dplyr::filter(factors,GROUP==input$sel3_1)
#     lvl$lv_3_2 <- unique(aux$SUBGROUP)
#     isolate(
#       if(input$sel3_1 == "Soil fertility"){
#         shinyjs::hide(id="numLevels_3")
#         addTabSoilFertility(3)
#       }
#       else{
#         shinyjs::show(id="numLevels_3")
#         removeTabSoilFertility(3)
#       }
#     )
#   }
#   else{
#     lvl$lv_3_2 <- NULL
#     shinyjs::show(id="numLevels_3")
#     removeTabSoilFertility(3)
#     updateSelectInput(session, "sel3_2", choices = NULL)
#   }
#   removeUI(selector = "#fluid_levels_3", immediate = T)
#   lvl$lv_3_3 <- NULL
#   isolate(convertListToHTMLSelect(3))
#   removeUI( selector ="#fl_title_factor_3", immediate = T )
# 
# })
# 
# ## cuando se cambia el segundo select del tercer factor
# observe( {
#   lvl$lv_3_3 <- NULL
#   if(!is.null(input$sel3_2)){
#     aux <- dplyr::filter(factors,GROUP==input$sel3_1 & SUBGROUP==input$sel3_2)
#     lvl$lv_3_3 <- unique(aux$FACTOR)
#   }
#   # else{
#   #
#   #   updateSelectInput(session, "sel3_3", choices = NULL)
#   # }
#   isolate(convertListToHTMLSelect(3))
#   isolate(if(!is.null(input$sel3_1) && input$sel3_1 == "Soil fertility") {generateListLevelsSoilTab(3)})
#   removeUI(selector = "#fluid_levels_3", immediate = T)
#   removeUI( selector ="#fl_title_factor_3", immediate = T )
# })
# 
# ## cuando se cambia el tercer select del tercer factor
# observeEvent(input$sel3_3,{
#   removeUI( selector ="#fl_title_factor_3", immediate = T )
#   if(!is.null(input$sel3_3)){
#     updateLevelSelection(3)
#   }
#   else{
#     isolate(convertListToHTMLSelect(3))
#     isolate(if(input$sel3_1 == "Soil fertility"){generateListLevelsSoilTab(3)})
#     removeUI(selector = "#fluid_levels_3", immediate = T)
#   }
# })
# 
# ## cuando se cambia el primer select del cuarto factor
# observe({
#   if(!is.null(input$sel4_1)){
#     aux <- dplyr::filter(factors,GROUP==input$sel4_1)
#     lvl$lv_4_2 <- unique(aux$SUBGROUP)
# 
#     isolate(
#       if(input$sel4_1 == "Soil fertility"){
#         shinyjs::hide(id="numLevels_4")
#         addTabSoilFertility(4)
#       }
#       else{
#         shinyjs::show(id="numLevels_4")
#         removeTabSoilFertility(4)
#       }
#     )
#   }
#   else{
#     lvl$lv_4_2 <- NULL
#     shinyjs::show(id="numLevels_4")
#     removeTabSoilFertility(4)
#   }
#   isolate(convertListToHTMLSelect(4))
#   removeUI(selector = "#fluid_levels_4", immediate = T)
#   lvl$lv_4_3 <- NULL
#   removeUI( selector ="#fl_title_factor_4", immediate = T )
# 
# })
# 
# ## cuando se cambia el segundo select del cuarto factor
# observe({
#   lvl$lv_4_3 <- NULL
#   if(!is.null(input$sel4_2)){
#     aux <- dplyr::filter(factors,GROUP==input$sel4_1 & SUBGROUP==input$sel4_2)
#     lvl$lv_4_3 <- unique(aux$FACTOR)
#   }
#   # else{
#   #
#   #   updateSelectInput(session, "sel4_3", choices = NULL)
#   # }
#   isolate(convertListToHTMLSelect(4))
#   isolate(if(!is.null(input$sel4_1) && input$sel4_1 == "Soil fertility") {generateListLevelsSoilTab(4)})
#   removeUI(selector = "#fluid_levels_4", immediate = T)
#   removeUI( selector ="#fl_title_factor_4", immediate = T )
# })
# 
# ## cuando se cambia el tercer select del cuarto factor
# observeEvent(input$sel4_3,{
#   removeUI( selector ="#fl_title_factor_4", immediate = T )
#   if(!is.null(input$sel4_3)){
#     updateLevelSelection(4)
#   }
#   else{
#     isolate(convertListToHTMLSelect(4))
#     isolate(if(input$sel4_1 == "Soil fertility"){generateListLevelsSoilTab(4)})
#     removeUI(selector = "#fluid_levels_4", immediate = T)
#   }
# })
# 
# ## cuando se cambia el primer select del quinto factor
# observe({
#   if(!is.null(input$sel5_1)){
#     aux <- dplyr::filter(factors,GROUP==input$sel5_1)
#     lvl$lv_5_2 <- unique(aux$SUBGROUP)
#     isolate(
#       if(input$sel5_1 == "Soil fertility"){
#         shinyjs::show(id="numLevels_5")
#         addTabSoilFertility(5)
#       }
#       else{
#         shinyjs::show(id="numLevels_5")
#         removeTabSoilFertility(5)
#       }
#     )
#   }
#   else{
#     lvl$lv_5_2 <- NULL
#     shinyjs::show(id="numLevels_5")
#     removeTabSoilFertility(5)
#   }
#   removeUI(selector = "#fluid_levels_5", immediate = T)
#   lvl$lv_5_3 <- NULL
#   isolate(convertListToHTMLSelect(5))
#   removeUI( selector ="#fl_title_factor_5", immediate = T )
# })
# ## cuando se cambia el segudo select del quinto factor
# observe({
#   lvl$lv_5_3 <- NULL
#   if(!is.null(input$sel5_2)){
#     aux <- dplyr::filter(factors, GROUP==input$sel5_1 & SUBGROUP==input$sel5_2)
#     lvl$lv_5_3 <- unique(aux$FACTOR)
#   }
#   # else{
#   #
#   #   updateSelectInput(session, "sel5_3", choices = NULL)
#   # }
#   isolate(convertListToHTMLSelect(5))
#   isolate(if(!is.null(input$sel5_1) && input$sel5_1 == "Soil fertility") {generateListLevelsSoilTab(5)})
#   removeUI(selector = "#fluid_levels_5", immediate = T)
#   removeUI( selector ="#fl_title_factor_5", immediate = T )
# })
# ## cuando se cambia el terccer select del quinto factor
# observeEvent(input$sel5_3,{
#   removeUI( selector ="#fl_title_factor_5", immediate = T )
#   if(!is.null(input$sel5_3)){
#     updateLevelSelection(5)
#   }
#   else{
#     isolate(convertListToHTMLSelect(5))
#     isolate(if(input$sel5_1 == "Soil fertility"){generateListLevelsSoilTab(5)})
#     removeUI(selector = "#fluid_levels_5", immediate = T)
#   }
# })
# 
# ## dibuja selectizeInput en los factores cuando tercer select es del tipo lista
# drawComboboxLevel <- function(order, num, lev){
#   opt <- strsplit(lev, ";")
#   removeUI(selector = paste0("#fluid_levels_", order), immediate = T)
#   insertUI(selector = paste0("#levelSelection_", order),
#            where = "afterEnd",
#            ui = fluidRow( id= paste0("fluid_levels_", order),
#                           column(width = 12,
#                                  selectizeInput(paste0("levels_", order), HTML("Select levels"),
#                                                 multiple =T,
#                                                 options = list(maxItems = num, placeholder = "Select ..." ),
#                                                 choices = opt[[1]]
#                                  )
#                           )
#            )
#   )
# }
# 
# ## dibuja selectizeInput para escribir en los factores cuando tercer select es del tipo text input
# drawTextInputLevel <- function(order, num, units){
#   removeUI(selector = paste0("#fluid_levels_", order), immediate = T)
#   if(is.na(units)){
#     insertUI(selector = paste0("#levelSelection_", order),
#              where = "afterEnd",
#              ui = fluidRow( id= paste0("fluid_levels_", order),
#                             column(width = 12,
#                                    selectizeInput(paste0("levels_", order), HTML("Enter levels"),
#                                                   multiple =T, choices = c(),
#                                                   options = list(maxItems = num, placeholder = "Write..." ,
#                                                                  'create' = TRUE,
#                                                                  'persist' = FALSE)
#                                    )
#                             )
#              )
#     )
#   }
#   else{
#     vunits <- strsplit(units, ",")
#     insertUI(selector = paste0("#levelSelection_", order),
#              where = "afterEnd",
#              ui = fluidRow( id= paste0("fluid_levels_", order),
#                             column(width = 6,
#                                    selectizeInput(paste0("levels_",order), HTML("Enter levels"),
#                                                   multiple =T, choices = c(),
#                                                   options = list(maxItems = num, placeholder = "Write..." ,
#                                                                  'create' = TRUE,
#                                                                  'persist' = FALSE)
#                                    )
#                             ),
#                             column(width = 6,
#                                    selectizeInput(paste0("funits_", order), HTML("Unit"),
#                                                   multiple =T, choices = vunits[[1]] ,
#                                                   options = list(maxItems = 1, placeholder = "Select unit...")
#                                    )
#                             )
#              )
#     )
#   }
# }
# 
# ## dibuja numericInput en los factores cuando tercer select es del tipo numeric input
# drawNumericInputLevel <- function(order, num){
#   removeUI(selector = paste0("#fluid_levels_", order), immediate = T)
#   insertUI(selector = paste0("#levelSelection_", order),
#            where = "afterEnd",
#            ui = fluidRow( id= paste0("fluid_levels_", order),
#                           column(width = 12,
#                                  numericInput(paste0("levels_", order), HTML("Levels"), min=1, max = num, value = 1)
#                           )
#            )
#   )
# }
# 
# ## dibuja dateInput en los factores cuando tercer select es del tipo date
# drawDateLevel <- function(order, num){
#   removeUI(selector = paste0("#fluid_levels_", order), immediate = T)
#   insertUI(selector = paste0("#levelSelection_", order),
#            where = "afterEnd",
#            ui = fluidRow( id= paste0("fluid_levels_", order),
#                           column(width = 12,
#                                  fluidRow( id = paste0("factor_dates_", order , "_1"),
#                                            column(width = 6,
#                                                   dateInput(paste0("factor_start_date_", order, "_1"), HTML("#1 Start date"),format = "yyyy-mm-dd")
#                                            ),
#                                            column(width = 6,
#                                                   dateInput(paste0("factor_end_date_", order, "_1"), HTML("#1 End date"),format = "yyyy-mm-dd")
#                                            )
#                                  )
#                           )
#            )
#   )
#   if(num > 1){
#     for (i in 2:num) {
#       insertUI(selector = paste0("#factor_dates_", order,"_", i-1),
#                where = "afterEnd",
#                ui = fluidRow(id = paste0("factor_dates_", order , "_", i) ,
#                              column(width = 6,
#                                     dateInput(paste0("factor_start_date_", order, "_", i), HTML(paste0("#",i, " Start date")),format = "yyyy-mm-dd")
#                              ),
#                              column(width = 6,
#                                     dateInput(paste0("factor_end_date_", order, "_", i), HTML(paste0("#", i, " End date")),format = "yyyy-mm-dd")
#                              )
#                )
#                # ui =  dateRangeInput(paste0("dates_",order ,"_", i), paste0("#" ,i, " Select dates"), startview = "year",format = "yyyy/mm/dd")
#       )
#     }}
# }
# 
# 
# #### end factors ####################################################################################