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

  np <- input$npersons
  vperType <- c()
  vperfname <- c()
  vperlname <- c()
  vperemail <- c()
  vperAff <- c()
  vperOrcid <- c()
  vpercountry <- c()

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

    vperemail <- c(vperemail, input[[paste0("person",i,"Email")]])
    vperOrcid <- c(vperOrcid, input[[paste0("person",i,"ORCID")]])

    if(is.null(input[[paste0("person",i,"Country")]])) vpercountry <- c(vpercountry, "")
    else vpercountry <- c(vpercountry, input[[paste0("person",i,"Country")]])
  }

  vperType <- paste(vperType, collapse = ",")
  vperfname <-paste(vperfname, collapse = ",")
  vperlname <- paste(vperlname, collapse = ",")
  vperemail <- paste(vperemail, collapse = ",")
  vperAff <- paste(vperAff, collapse = ",")
  vperOrcid <- paste(vperOrcid, collapse = ",")
  vpercountry <- paste(vpercountry, collapse = ",")

  c19 <- c('Person type',vperType )
  c20 <- c('Person, first name',vperfname )
  c21 <- c('Person, last name', vperlname)
  c22 <- c('Person, email', vperemail)
  c23 <- c('Person, affiliation', vperAff)
  c24 <- c('Person, ORCID',vperOrcid )
  c25 <- c('Country in which active', vpercountry)


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
