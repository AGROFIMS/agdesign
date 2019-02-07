# df<- readRDS("/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/allinputs.rds")
 #ai<- readRDS("inst/app_hdagrofims/ai.rds")

get_ec_harvest <- function(allinputs, lbl){
  
  dt1 <- ai %>% filter(str_detect(id, "^residue_"))
  dt2 <- ai %>% filter(str_detect(id, "^crop_residue_"))
  
}