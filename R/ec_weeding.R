# 
#  addId <- getAddInputId(addId = expCondsVars$ids_weed, "ECWE_", "")
#  get_ec_weed(allinputs=allinputs, addId=addId)
#  dt_weeding <- reactive({
#       addId <- getAddInputId(addId = expCondsVars$ids_weed, "ECWE_", "")
#       dt <- get_ec_weed(allinputs=allinputs(), addId=addId)
#      
#   }) 
get_ec_weed<- function(allinputs, addId){
    
    #allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/inst/table_ids.rds")
    #input<- readRDS("/home/obenites/AGROFIMS/agdesign/inst/inputs.rds")
    #id_rand<- c("AJRFGXWN","LDTEALNS")
   
    w  <- allinputs %>% filter(!str_detect(id, "button")) %>%
                        filter(!str_detect(id, "-selectized")) %>%
                        filter(str_detect(id,"wewd"))
    
    lbl <- c("Weeding_start_date", "Weeding_technique", 
             "Weeding_notes", "Weeding_implement_type", "Weeding_implement_traction")
    lbl_weed <- vector(mode = "character",length = 0L)
    for(i in 1:length(addId)){
      lbl_weed <- append(lbl_weed, paste(lbl, i,sep="__"))
    }
    
    #order by id_rand
    w<- arrange_by_pattern(w, pattern = addId) 
    
    #start date
    startD <- w %>% filter(str_detect(id, "^wewd_weeding_start_date_"))
     
    #technique
    tech <- w %>% filter(str_detect(id, "^wewd_weeding_technique_"))
    
    #notes
    notes <- w %>% filter(str_detect(id, "^wewd_weeding_notes_"))
    
    #type
    type <- w %>% filter(str_detect(id, "^wewd_weeding_type_[:alpha:]+$"))
    type_other <- w %>% filter(str_detect(id, "^wewd_weeding_type_[:alpha:]+_other$"))
    type<- dt_inputs(type, type_other)
    
    #traction
    traction <- w %>% filter(str_detect(id, "^wewd_weeding_traction_[:alpha:]+$"))
    traction_other <- w %>% filter(str_detect(id, "^wewd_weeding_type_[:alpha:]+_other$"))
    traction<- dt_inputs(traction, traction_other)

    dt<- rbind(startD, tech, notes, type, traction)
    dt<- arrange_by_pattern(dt, pattern = addId)
    #extract and tranpose column with valus
    dt<- t(dt$values) %>% as.data.frame(stringAsFactors=FALSE)
    
    # TODO : AGREGAR ESTAS COLUMNAS
    # Weeding_end_date
    # Weeding_biomass_fresh_weight
    # Weeding_biomass_subsample_fresh_weight
    # Weeding_biomass_dry_weight
    # Weeding_biomass_subsample_dry_weight
    
    names(dt) <- lbl_weed
    dt
}

