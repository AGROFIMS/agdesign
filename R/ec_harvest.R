# df<- readRDS("/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/allinputs.rds")
 #ai<- readRDS("inst/app_hdagrofims/ai.rds")

get_ec_harvest <- function(allinputs, lbl){
  
  dt1 <- allinputs %>% filter(str_detect(id, "^hahd_"))
  dt1<- t(dt1) %>% as.data.frame(stringsAsFactors=FALSE)
  
  #dt <- data.frame(matrix(ncol = length(dt1), nrow = 1))
  #names(dt) <- lbl_soil
  # dt[1,]<- dt_soil[,"values"]
  # dt
  
}