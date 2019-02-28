
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
  
  #Labels
  lbl <- c("Mulch_start_date", "Mulch_type", 
           paste0("Mulch_thickness_", mthick_unit$values),
           paste0("Mulch_amount_",mamount_unit$values),
           "Mulch_color", 
           paste0("Mulch_percentage_of_coverage_",mper_unit$values),
           "Mulch_removal_start_date", 
           "Mulch_removal_end_date",
           "Mulch_notes",
           "Mulch_implement_type",
           "Mulch_implement_traction"
           )
  
  #TODO : AGREGAR END DATE "Mulch_end_date"
  names(dt) <- lbl
  dt
  
}