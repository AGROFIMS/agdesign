#Get management practicse for weeding experiments ####################################################
get_ec_weed<- function(allinputs, addId, ctype="monocrop" ){
    
    #allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/inst/table_ids.rds")
    #input<- readRDS("/home/obenites/AGROFIMS/agdesign/inst/inputs.rds")
    #id_rand<- c("AJRFGXWN","LDTEALNS")
   
    w  <- allinputs %>% filter(!str_detect(id, "button")) %>%
                        filter(!str_detect(id, "-selectized")) %>%
                        filter(str_detect(id, paste0(ctype,"_wewd")))
    
    # lbl <- c("Weeding_start_date", "Weeding_technique", 
    #          "Weeding_notes", "Weeding_implement_type", "Weeding_implement_traction")
    
    lbl <- c("Weeding_start_date", "Weeding_technique", 
             "Weeding_notes", "Weeding_implement_type", "Weeding_traction_type",
             "Weed_biomass_fresh_weight_kg/ha",
             "Weed_biomass_subsample_fresh_weight_kg/ha",
             "Weed_biomass_dry_weight_kg/ha",
             "Weed_biomass_subsample_dry_weight_kg/ha"
             )
    
    lbl_weed <- vector(mode = "character",length = 0L)
    for(i in 1:length(addId)){
      lbl_weed <- append(lbl_weed, paste(lbl, i,sep="__"))
    }
    
    #order by id_rand
    w<- arrange_by_pattern(w, pattern = addId) 
    
    #start date
    startD <- w %>% filter(str_detect(id, paste0("^",ctype,"_wewd_weeding_start_date_")))
     
    #technique
    tech <- w %>% filter(str_detect(id, paste0("^",ctype,"_wewd_weeding_technique_")))
    
    #notes
    notes <- w %>% filter(str_detect(id, paste0("^",ctype,"_wewd_weeding_notes_")))
    
    #type
    type <- w %>% filter(str_detect(id,paste0("^",ctype,"_wewd_weeding_type_[:digit:]+$")))
    type_other <- w %>% filter(str_detect(id,paste0("^",ctype,"_wewd_weeding_type_[:digit:]+_other$")))
    type<- dt_inputs(type, type_other)
    
    #traction
    traction <- w %>% filter(str_detect(id, paste0("^",ctype,"_wewd_weeding_traction_[:digit:]+$")))
    traction_other <- w %>% filter(str_detect(id, paste0("^",ctype,"_wewd_weeding_traction_[:digit:]+_other$")))
    traction<- dt_inputs(traction, traction_other)
    
    wbiofw <- data.frame(id= paste0(ctype,"_wewd_weeding_wbiofw_",seq.int(addId)) , values= rep("",length(addId)), stringsAsFactors = FALSE) #"Weed_biomass_fresh_weight_kg/ha",
    #wbiosubfw <-  data.frame(id="non_indexed" , values= "") #Weed_biomass_subsample_fresh_weight_kg/ha",
    wbiosubfw <- data.frame(id= paste0(ctype,"_wewd_weeding_wbiosubfw_",seq.int(addId)) , values= rep("",length(addId)), stringsAsFactors = FALSE)
    #wbdw  <-  data.frame(id="non_indexed" , values= "") #"Weed_biomass_dry_weight_kg/ha",
    wbdw  <- data.frame(id= paste0(ctype,"_wewd_weeding_wbdw_",seq.int(addId)) , values= rep("",length(addId)), stringsAsFactors = FALSE)
    wbsubdw <-  data.frame(id= paste0(ctype,"_wewd_weeding_wbsubdw_",seq.int(addId)) , values= rep("",length(addId)), stringsAsFactors = FALSE) #"Weed_biomass_subsample_dry_weight_kg/ha"
     

    dt<- rbind(startD, tech, notes, type, traction, wbiofw, wbiosubfw, wbdw ,wbsubdw )
    dt<- arrange_by_pattern(dt, pattern = addId)
    #extract and tranpose column with valus
    dt <- t(dt$values) %>% as.data.frame(stringAsFactors=FALSE)
    dt<- dt %>%  dplyr::mutate_all(as.character)
    
    # TODO : AGREGAR ESTAS COLUMNAS
    # Weeding_end_date
    # Weeding_biomass_fresh_weight
    # Weeding_biomass_subsample_fresh_weight
    # Weeding_biomass_dry_weight
    # Weeding_biomass_subsample_dry_weight
    
    names(dt) <- lbl_weed #changes names
    #dta
    
    #LABEL FOR TRAITLIST
    lbl <- str_replace_all(string = names(dt), pattern = "__[:digit:]+$",replacement = "") %>% unique()
    
    #OUTPUT
    out<- list(dt=dt, lbl = lbl)
    
    
}

#Get protocol table for weeding experiments  #########################################################
get_protocol_weed <- function(allinputs, addId, ctype="monocrop"){
  
  out <- get_ec_weed(allinputs, addId, ctype="monocrop")$dt
  names(out) <- stringr::str_replace_all(names(out),"__1","")
  out <- t(out) %>% as.data.frame(stringsAsFactors=FALSE) %>% tibble::rownames_to_column()
  out <- out %>% dplyr::filter(V1!="") %>% dplyr::filter(!stringr::str_detect(V1, "^NA$"))
  names(out) <- c("TraitName","Value")
  out
}


#Get Collectable inputs for Weeding ###################################################################
#allinputs: data frame with all the user's inputs 
get_collectable_weed <- function(allinputs, ver ="default"){
  
  weed <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^","weeding_to_collect_field","$") )) %>% dplyr::nth(2)
  out <- stringi::stri_split_regex(weed,",")[[1]] %>% stringr::str_trim(side = "both")  %>% setdiff("")
  
  
  if(length(out)!=0){
    if(ver=="default"){
      out <- paste0("Weeding" ,"_", out)
    }
    else if(ver=="export"){
      out <- ifelse(str_detect(string = out,pattern = "technique"), paste0("weeding technique"), out)
    }
  }
  out
}