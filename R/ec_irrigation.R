
# Get management practices for irrigation experiments ###############################################
#ctype: cropppyng type ("monocrop","intercrop")
get_ec_irri <- function(allinputs, ctype="monocrop", addId ){

    #allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/inst/table_ids.rds")
    #input<- readRDS("/home/obenites/AGROFIMS/agdesign/inst/inputs.rds")

    
    #Irrigation inputs table
    irri <- allinputs %>%  dplyr::filter(!stringr::str_detect(id, "button")) %>%
                            dplyr::filter(!stringr::str_detect(id, "-selectized")) %>%
                            dplyr::filter(stringr::str_detect(id,  paste0(ctype, "_irid")))

    #addId <- str_extract_all(irri$id, "[:uppercase:]{8}") %>% unlist() %>% unique()
    
    irri <- arrange_by_pattern(irri, addId)

    #startD
    startD<- irri %>% dplyr::filter(str_detect(id, paste0(ctype,"_irid_irrigationevent_start_date_[:digit:]+$")))

    #irrigationevent_end_date_UCCIZOLN
    endD <- irri %>% dplyr::filter(str_detect(id, paste0(ctype,"_irid_irrigationevent_end_date_[:digit:]+$")))

    #Irrigation Technique
    technique <- irri %>% dplyr::filter(str_detect(id, paste0(ctype,"_irid_irrigation_technique_[:digit:]+$")))
    #technique_other <- irri %>% filter(str_detect(id, paste0("irid_irrigation_technique_", addId[i],"_other","$")))
    #technique <- dt_inputs(technique, technique_other)
    
    #Special case: Irrigation system (which depends on Technique) ------------------
    irrigation_system <- data.frame(stringsAsFactors = FALSE)
    #lbl <-NULL
    for(i in 1:length(addId)){

        if(technique[i,2]=="Sprinkler irrigation"){
           tech_splin <- irri %>% filter(str_detect(id, paste0(ctype,"_irid_irrigation_using_sprinkler_systems_", addId[i],"$")))
           if(!is.null(tech_splin[1,2])|| !is.na(tech_splin)){
            if(tech_splin[1,2]=="Other"){
               splin_other<- irri %>% filter(str_detect(id, paste0(ctype,"_irid_irrigation_using_sprinkler_systems_",addId[i],"_other","$")))
               tech_splin  <- dt_inputs(tech_splin,splin_other)
            }
           }
           #Label
           #lbl<- paste("Irrigation_splinkler_irrigation_system", addId[i],sep="__")
           lbl<- paste("Irrigation_sprinkler_technique", addId[i],sep="__")
           
           #Table
           dt_irri_system <- tech_splin
           
      }
        else if(technique[i,2]=="Localized"){
           tech_local<- irri %>% filter(str_detect(id, paste0(ctype, "_irid_localized_irrigation_technique_",addId[i],"$")))
           
           #Label  
           #lbl<-  paste("Irrigation_localized_irrigation_system", addId[i],sep="__")
           lbl<-  paste("Irrigation_localized_technique", addId[i],sep="__")
           
           if(tech_local[1,2]=="Other"){
             local_other<- irri %>% filter(str_detect(id, paste0(ctype,"_irid_localized_irrigation_technique_",addId[i],"_other", "$")))
             tech_local <- dt_inputs(tech_local,local_other)

           }
           #Table
           dt_irri_system<-tech_local
       }
        else if(technique[i,2]=="Surface"){
          
          #Label
          #lbl<- paste("Irrigation_surface_irrigation_system" , addId[i],sep="__") #deprecated
          lbl<- paste("Irrigation_surface_technique" , addId[i],sep="__")
          
          tech_surface<-  irri %>% filter(str_detect(id, paste0(ctype,"_irid_surface_irrigation_technique_",addId[i],"$")))
          if(tech_surface[1,2]=="Other"){
            surface_other<- irri %>% filter(str_detect(id, paste0(ctype,"_irid_surface_irrigation_technique_",addId[i],"_other","$")))
            tech_surface<- dt_inputs(tech_surface, surface_other)

          }
          #Table
          dt_irri_system<-tech_surface
            # irrigation_system[i,1]<- "Surface irrigation system"
            # irrigation_system[i,2]<- tech_surface[i,2]
      }
        else if(technique[i,2]=="Other"){
          #Label
          #lbl<- paste("Irrigation_other_irrigation technique" , addId[i],sep="__")
          lbl<- paste("Other_irrigation_technique" , addId[i],sep="__")
          
          tech_other <- irri %>% filter(str_detect(id, paste0(ctype,"_irid_irrigation_technique_", addId[i],"_other","$")))
          # irrigation_system[i,1]<- "Other irrigation system"
          # irrigation_system[i,2]<- tech_other[i,2]
          dt_irri_system<- tech_other
        }
        else {
          lbl <- paste("NoLabel",addId[i], sep="__")
          dt_irri_system <- data.frame(id="", values = "NoValue")
        }
      irrigation_system[i,1] <- lbl
      irrigation_system[i,2] <- dt_irri_system[1,2]

      # irrigation_system[i,1]<- "Other irrigation system"
      # irrigation_system[i,2]<- tech_other[i,2]
   }
    names(irrigation_system)<-c("id", "values")
    #TODO: filtrar los valores de "NoLabel" en la col. id y poner la numeración de cada
    # evaluacion de irrigación
    lbl_irri_system <- stringr::str_replace_all(string = irrigation_system$id ,"[:digit:]+$", as.character(1:length(addId)) )
    #lbl_irri_system <- lbl_irri_system[!str_detect(lbl_irri_system, pattern = "NoLabel")]
    #irrigation_system <- irrigation_system %>% filter(!str_detect(id, "NoLabel"))
    #-------------------------------------------------------------------------------

    #Irrigation source
    source <- irri %>% dplyr::filter(stringr::str_detect(id,  paste0("^",ctype,"_irid_irrigation_source_[:digit:]+$" )))
    source_other <- irri %>% dplyr::filter(stringr::str_detect(id,  paste0("^",ctype,"_irid_irrigation_source_[:digit:]+_other$")))
    source <- dt_inputs(source, source_other)

    #irrigation source distance
    source_distance<- irri %>% dplyr::filter(str_detect(id, paste0("^",ctype,"_irid_irrigation_source_distance_[:digit:]+$"))) #%>%
                                 # filter(!str_detect(id, "unit"))
    #unit
    source_distance_unit <- irri %>% dplyr::filter(str_detect(id,  paste0("^",ctype,"_irid_irrigation_source_distance_[:digit:]+_unit")))

    #irrgation amount
    amount <- irri %>% dplyr::filter(str_detect(id,  paste0(ctype,"_irid_irrigation_amount_[:digit:]+$")))
    #unit
    amount_unit <- irri %>% dplyr::filter(str_detect(id,  paste0(ctype,"_irid_irrigation_amount_[:digit:]+_unit")))

    #irrigation notes
    notes<- irri %>% dplyr::filter(str_detect(id,  paste0(ctype,"_irid_irrigation_notes_[:digit:]+$")))

    dt<- rbind(startD, endD, technique, irrigation_system, source, source_distance, amount, notes)
    
   
    lbl_start <- paste("Irrigation_start_date",1:length(addId),sep = "__")
    lbl_end <- paste("Irrigation_end_date",1:length(addId),sep = "__")
    lbl_tech <- paste("Irrigation_technique", 1:length(addId),sep = "__")
    lbl_source<- paste("Irrigation_source_type",1:length(addId),sep = "__")
    #lbl_source_dis<- paste( paste("Irrigation_source_distance", source_distance_unit$values,sep="_"),   1:length(addId), sep = "__") #deprecated
    lbl_source_dis<- paste( paste("Source_distance", source_distance_unit$values,sep="_"),   1:length(addId), sep = "__")
    
    lbl_amount <- paste(paste("Irrigation_amount", amount_unit$values, sep="_"),  1:length(addId),sep = "__")
    lbl_notes<- paste("Irrigation_notes", 1:length(addId),sep = "__")

    #Ensemble all irrigation labels
    lbl_irri <- c(lbl_start, lbl_end, lbl_tech, lbl_irri_system, 
                  lbl_source, lbl_source_dis, lbl_amount,lbl_notes)
    
    #Special case:
    #Swichtching id values by irrigation labels (lbl_irr)
    dt$id <- lbl_irri
    
    #Remove NoLabel or NonData rows
    dt <- arrange_by_pattern(dt, as.character(1:length(addId))) %>% 
          dplyr::filter(!stringr::str_detect(id,"NoLabel__"))  
    
    #transpose data as rows   
    dt_irri<- t(dt$values) %>% as.data.frame(stringAsFactors=FALSE)
    names(dt_irri) <- dt$id
   
    #LABEL FOR TRAITLIST
    lbl <- stringr::str_replace_all(string = names(dt_irri), pattern = "__[:digit:]+$",replacement = "") %>% unique()
    
    #OUTPUT
    out<- list(dt=dt_irri, lbl = lbl)
    
}

# Get protocol for irrigation experiments  ##########################################################
get_protocol_irri <- function(allinputs, ctype="monocrop", addId ){
  
  out <- get_ec_irri(allinputs, ctype, addId)$dt
  names(out) <- stringr::str_replace_all(names(out),"__1","")
  out <- t(out) %>% as.data.frame(stringsAsFactors=FALSE) %>% tibble::rownames_to_column()
  out <- out %>% dplyr::filter(V1!="") %>% dplyr::filter(!stringr::str_detect(V1, "^NA$"))
  names(out) <- c("TraitName","Value")
  out
}


# Get Collectable Irrigation inputs #################################################################
get_collectable_irri <- function(allinputs, ver="default"){
  
  irri <- allinputs %>% dplyr::filter(str_detect(id,  paste0("^","irrigation_to_collect_field","$") )) %>% dplyr::nth(2)
  out <- stringi::stri_split_regex(irri,",")[[1]] %>% stringr::str_trim(side = "both") %>% setdiff("")
  if(length(out)!=0){
    if(ver=="default"){
      out <- paste0("Irrigation" ,"_", out)
    }
    else if(ver=="export"){
      out <- ifelse(str_detect(string = out,pattern = "Source|Irrigation|Localized|Surface"), out, paste0("Irrigation_",out))
      #Special cases 1: Localized trait is collected
      out <- ifelse(str_detect(string = out,pattern = "Localized"), paste0("localized technique"), out)
      #Special cases 2: 
      out <- ifelse(str_detect(string = out,pattern = "Surface"), paste0("surface technique"), out)
      #Special cases 3: Localized trait is collected
      out <- ifelse(str_detect(string = out,pattern = "sprinkler"), paste0("sprinkler technique"), out)
    }
    
  }
  out
}