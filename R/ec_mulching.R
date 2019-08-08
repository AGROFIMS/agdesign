# Get management practices for mulching experiments  ########################
get_ec_mulching <- function(allinputs){
  
  #allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/tests/testthat/userInput/table_ids.rds")
  # input<- readRDS("/home/obenites/AGROFIMS/agdesign/inst/inputs.rds")
  
  mu <- allinputs %>% filter(!str_detect(id, "button")) %>%
                      filter(!str_detect(id, "-selectized")) %>%
                      filter(str_detect(id, "^mumd_"))
  
  #stard date
  startD <- mu %>% filter(str_detect(id, "^mumd_mulch_start_date"))
  #Obs.: In case of start date combo do not load in the interface on time
  if(nrow(startD)==0) startD <- data.frame(id = "mumd_mulch_start_date", values = NA)
  
  # print("start date")
  # print(startD)
  
  #type 1
  type1 <- mu %>% filter(str_detect(id, "^mumd_mulch_type$"))
  type1_other <- mu %>% filter(str_detect(id, "^mumd_mulch_type_other$"))
  type1 <- dt_inputs(type1, type1_other)
  
  #Mulch thickness
  mthick <- mu %>% filter(str_detect(id, "^mumd_mulch_thickness$"))
  mthick_unit <- mu %>% filter(str_detect(id, "^mumd_mulch_thickness_unit$"))
  
  #mulch amount
  mamount <- mu %>% filter(str_detect(id, "^mumd_mulch_amountPerSq$"))
  mamount_unit <- mu %>% filter(str_detect(id, "^mumd_mulch_amountPerSq_unit$"))
  
  #mulch color
  mcolor <- mu %>% filter(str_detect(id, "^mumd_mulch_color$"))
  
  #percentage
  mper <- mu %>% filter(str_detect(id, "^mumd_mulch_percCoverage$"))
  mper_unit <- mu %>% filter(str_detect(id, "^mumd_mulch_percCoverage_unit$"))
  
  #Mulch removal start date
  mrstarD <- mu %>% filter(str_detect(id, "^mumd_mulch_remove_start_date$")) 
  if(nrow(mrstarD)==0) mrstarD <- data.frame(id = "mumd_mulch_remove_start_date", values = NA)
  
  
  #Mulch removal end date
  mrendD <- mu %>% filter(str_detect(id, "^mumd_mulch_remove_end_date$")) 
  if(nrow(mrendD)==0) mrendD <- data.frame(id = "mumd_mulch_remove_end_date", values = NA)
  
  
  
  #notes
  notes <- mu %>% filter(str_detect(id, "^mumd_mulching_management_notes$")) 
  
  ### Implement -----
  #type of implementation
  type2<- mu %>% filter(str_detect(id, "^mumd_mulch_implement_type$")) 
  
  #type of traction
  traction <- mu %>% filter(str_detect(id, "^mumd_mulch_traction$"))
  traction_other <- mu %>% filter(str_detect(id, "^mumd_mulch_traction_other$"))
  traction <- dt_inputs(traction, traction_other)
  # -----
  
  dt<- rbind(startD,type1,mthick, mamount, mcolor, mper, mrstarD,mrendD, notes, type2, traction)
  dt<- t(dt$values) %>% as.data.frame(stringAsFactors=FALSE)
  dt<- dt %>%  dplyr::mutate_all(as.character)
  
  #BASE LABELS
  lbl <- c("Mulching_start_date", "Mulching_type", 
           paste0("Mulch_thickness_", mthick_unit$values),
           paste0("Mulch_amount_",mamount_unit$values),
           "Mulch_color", 
           paste0("Mulch_percent_coverage","_%"),
           "Mulch_removal_start_date", 
           "Mulch_removal_end_date",
           "Mulching_notes",
           "Mulching_implement_type",
           "Mulching_traction_type"
  )
  
  
  #LABELS FOR SPREADSHEETS and KDSMART
  lbl_dt<- paste(lbl, rep("1", length(lbl)) ,sep="__") 
  
  #TODO : AGREGAR END DATE "Mulch_end_date"
  names(dt) <- lbl_dt #changes names
  
  out<- list(dt = dt, lbl=lbl)
  
}

# Get protocol values for mulching experiments  #############################
get_protocol_mulching <- function(allinputs){
    out<- get_ec_mulching(allinputs)$dt 
    names(out) <- stringr::str_replace_all(names(out),"__1","")
    out <- t(out) %>% as.data.frame(stringsAsFactors=FALSE) %>% tibble::rownames_to_column()
    out <- out %>% dplyr::filter(V1!="") %>% dplyr::filter(!stringr::str_detect(V1, "^NA$"))
    names(out) <- c("TraitName","Value")
    out
}

# Get collectable inputs from Mulching #############################################################################
# allinputs: data frame of all inputs derived from ReactiveValuesToList
get_collectable_mulching <- function(allinputs, ver="default"){
  
  mu <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^","mulch_management_to_collect_field","$") )) %>% dplyr::nth(2)
  out <- stringi::stri_split_regex(mu,",")[[1]] %>% stringr::str_trim(side = "both") %>% setdiff("")
  if(length(out)!=0){
    
    if(ver=="default"){
      out <- paste0("Mulch management" ,"_", out)
    }
    else if(ver=="export"){
      out <- ifelse(str_detect(string = out,pattern = "Mulch" ), out, paste0("Mulching_",out))
    }
  }
  out
}

