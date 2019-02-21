
# addId= expCondsVars$ids_irri

get_ec_irri <- function(allinputs, addId){

    #allinputs <- readRDS("/home/obenites/AGROFIMS/agdesign/inst/table_ids.rds")
    #input<- readRDS("/home/obenites/AGROFIMS/agdesign/inst/inputs.rds")

    
    #Irrigation inputs table
    irri <- allinputs %>%  filter(!str_detect(id, "button")) %>%
                           filter(!str_detect(id, "-selectized")) %>%
                           filter(str_detect(id,"irid"))

    #addId <- str_extract_all(irri$id, "[:uppercase:]{8}") %>% unlist() %>% unique()
    
    irri <- arrange_by_pattern(irri, addId)

    #startD
    startD<- irri %>% filter(str_detect(id, "irid_irrigationevent_start_date_[:alpha:]+$"))

    #irrigationevent_end_date_UCCIZOLN
    endD <- irri %>% filter(str_detect(id, "irid_irrigationevent_end_date_[:alpha:]+$"))

    #Irrigation Technique
    technique <- irri %>% filter(str_detect(id, "^irid_irrigation_technique_[:alpha:]+$"))

    #Special case: Irrigation system (which depends on Technique) ------------------
    irrigation_system <- data.frame(stringsAsFactors = FALSE)
    #lbl <-NULL
    for(i in 1:length(addId)){

        if(technique[i,2]=="Sprinkler irrigation"){
           tech_splin <- irri %>% filter(str_detect(id, paste0("irid_irrigation_using_sprinkler_systems_", addId[i],"$")))
           if(!is.null(tech_splin[1,2])|| !is.na(tech_splin)){
            if(tech_splin[1,2]=="Other"){
               splin_other<- irri %>% filter(str_detect(id, paste0("irid_irrigation_using_sprinkler_systems_",addId[i],"_other","$")))
               tech_splin  <- dt_inputs(tech_splin,splin_other)
            }
           }
           lbl<- paste("Splinkler_irrigation_system", addId[i],sep="_")
           dt_irri_system <- tech_splin
           # irrigation_system[i,1]<- "Splinkler irrigation system"
           # irrigation_system[i,2]<- tech_splin[i,2]
      }
        else if(technique[i,2]=="Localized"){
           tech_local<- irri %>% filter(str_detect(id, paste0("irid_localized_irrigation_technique",addId[i],"$")))

           lbl<-  paste("Localized_irrigation_system", addId[i],sep="_")
           if(tech_local[1,2]=="Other"){
             local_other<- irri %>% filter(str_detect(id, paste0("irid_localized_irrigation_technique",addId[i],"_other", "$")))
             tech_local <- dt_inputs(tech_local,local_other)

           }
           dt_irri_system<-tech_local
       }
        else if(technique[i,2]=="Surface"){
          lbl<- paste("Surface_irrigation_system" , addId[i],sep="_")
          tech_surface<-  irri %>% filter(str_detect(id, paste0("irid_surface_irrigation_technique_",addId[i],"$")))
          if(tech_surface[1,2]=="Other"){
            surface_other<- irri %>% filter(str_detect(id, paste0("irid_surface_irrigation_technique_",addId[i],"_other","$")))
            tech_surface<- dt_inputs(tech_surface, surface_other)

          }
          dt_irri_system<-tech_surface
            # irrigation_system[i,1]<- "Surface irrigation system"
            # irrigation_system[i,2]<- tech_surface[i,2]
      }
        else if(technique[i,2]=="Other"){
          lbl<- paste("Other_irrigation_system" , addId[i],sep="_")
          tech_other <- irri %>% filter(str_detect(id, paste0("irid_irrigation_technique_", addId[i],"_other","$")))
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
    lbl_irri_system <- str_replace_all(string = irrigation_system$id ,"[:uppercase:]{8}", as.character(1:length(addId)) )
    #lbl_irri_system <- lbl_irri_system[!str_detect(lbl_irri_system, pattern = "NoLabel")]
    #irrigation_system <- irrigation_system %>% filter(!str_detect(id, "NoLabel"))
    #-------------------------------------------------------------------------------

    #Irrigation source
    source <- irri %>% filter(str_detect(id, "^irid_irrigation_source_[:alpha:]+$"))
    source_other <- irri %>% filter(str_detect(id, "irid_irrigation_source_[:alpha:]+_other$"))
    source <- dt_inputs(source, source_other)

    #irrigation source distance
    source_distance<- irri %>% filter(str_detect(id, "^irid_irrigation_source_distance_[:alpha:]{1,8}$")) #%>%
                                 # filter(!str_detect(id, "unit"))
    #unit
    source_distance_unit <- irri %>% filter(str_detect(id, "^irid_irrigation_source_distance_[:alpha:]+unit"))

    #irrgation amount
    amount <- irri %>% filter(str_detect(id, "irid_irrigation_amount_[:alpha:]{1,8}$"))
    #unit
    amount_unit <- irri %>% filter(str_detect(id, "irid_irrigation_amount_[:alpha:]+unit"))

    #irrigation notes
    notes<- irri %>% filter(str_detect(id, "irid_irrigation_notes_[:alpha:]+$"))

    dt<- rbind(startD, endD, technique, irrigation_system, source, source_distance, amount, notes)
    
   
    lbl_start <- paste("Irrigation_start_date",1:length(addId),sep = "__")
    lbl_end <- paste("Irrigation_end_date",1:length(addId),sep = "__")
    lbl_tech <- paste("Irrigation_technique", 1:length(addId),sep = "__")
    lbl_source<- paste("Irrigation_source",1:length(addId),sep = "__")
    lbl_source_dis<- paste( paste("Irrigation_source_distance", source_distance_unit$values,sep="_"),   1:length(addId), sep = "__")
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
          filter(!str_detect(id,"NoLabel__"))  
    
    #transpose data as rows   
    dt_irri<- t(dt$values) %>% as.data.frame(stringAsFactors=FALSE)
    names(dt_irri) <- dt$id
    dt_irri

}
