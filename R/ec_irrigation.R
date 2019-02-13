#trans_distance_rows_1

# ai<- ai %>%  filter(!str_detect(id, "button")) %>%
#              filter(!str_detect(id, "-selectized")) %>% 
#              filter(str_detect(id,"irri"))  
# 
# 
# 
# 
# #irrigationevent_start_date_UCCIZOLN
# irri_startD<- ai %>% filter(str_detect(id, "irrigationevent_start_date_"))
# 
# #irrigationevent_end_date_UCCIZOLN
# irri_endD <- ai %>% filter(str_detect(id, "irrigationevent_end_date_"))
# 
# #Irrigation Technique
# irri_technique <- ai %>% filter(str_detect(id, "^irrigation_technique_[:alpha:]+$"))
# #---SPECIAL CASES ------
# if(irri_technique[i,2]=="Sprinkler irrigation"){
#     irri_tech_splin<- ai %>% filter(str_detect(id, "^irrigation_using_sprinkler_systems_[:alpha:]+$"))
# 
#     if(irri_tech_splin[i]=="Other"){
#       irri_splin_other<- ai %>% filter(str_detect(id, "^irrigation_using_sprinkler_systems_[:alpha:]+_other$"))
#     }
#     irri_tech_splin<- dt_inputs(irri_tech_splin,irri_splin_other)
# }
# 
# if(irri_technique[i]=="Localized"){
# 
#     #localized_irrigation_techniqueUCCIZOLN
#    irri_tech_local<- ai %>% filter(str_detect(id, "^localized_irrigation_technique[:alpha:]+$"))
#    if(irri_tech_local=="Other"){
#      irri_local_other<- ai %>% filter(str_detect(id, "^localized_irrigation_technique[:alpha:]+_other$"))
#    }
#    irri_tech_local <- dt_inputs(irri_tech_local,irri_local_other)
# }
# 
# if(irri_technique=="Surface"){
#   irri_tech_surface<-  ai %>% filter(str_detect(id, "^surface_irrigation_technique_[:alpha:]+$"))
#   if(irri_tech_surface[i]=="Other"){
#     #surface_irrigation_technique_UCCIZOLN_other
#     irri_surface_other<- ai %>% filter(str_detect(id, "^surface_irrigation_technique_[:alpha:]+_other$"))
#   }
#   irri_tech_surface<- dt_inputs(irri_tech_surface, irri_surface_other)
# }
# 
# if(irri_technique[i]=="Other"){
#   irri_tech_other <- ai %>% filter(str_detect(id, "^irrigation_technique_[:alpha:]+_other$"))
#   #irrigation_technique_UCCIZOLN_other
# }
# 
# 
# 
# -----
# 
# #Irrigation source
# 
# #irrigation_source_UCCIZOLN
# irri_source_other <- ai %>% filter(str_detect(id, "^irrigation_source_[:alpha:]+$"))
# 
# # irrigation source: other case
#   #irrigation_source_UCCIZOLN_other
# irri_source_other <- ai %>% filter(str_detect(id, "irrigation_source_[:alpha:]+_other$"))
# irri_source <-dt_inputs (irri_source, irri_source_other)
# 
# #irrigation source distance
# #irrigation_source_distance_UCCIZOLN
# #irrigation_source_distance_UCCIZOLNunit
# irri_source_distance<- ai %>% filter(str_detect(id, "^irrigation_source_distance_")) %>%
#                               filter(!str_detect(id, "unit"))
# 
# #irrigation source distance unit
# ai %>% filter(str_detect(id, "^irrigation_source_distance_[:alpha:]+unit"))
# 
# 
# #irrgation amount
# #irrigation_amount_UCCIZOLN
# ai %>% filter(str_detect(id, "irrigation_amount_"))
# 
# #irrigation amount unit
# #irrigation_amount_UCCIZOLNunit
# ai %>% filter(str_detect(id, "irrigation_amount_[:alpha:]+unit"))
# 
# #irrigation notes
# ai %>% filter(str_detect(id, "irrigation_notes_"))
# #irrigation_notes_UCCIZOLN


