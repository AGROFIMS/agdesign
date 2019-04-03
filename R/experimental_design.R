# Create experimental design in AGROFIMS
# 
# @description create experimental design in agrofims through user interface inputs
# @param design experimental design abbreviation.
# @param rep replication
# @param block blocks
# @param trt treatments
# @param ntrt number of treatments
# @param fnames list of factors names
# @param flevels list of levels according to previous factors
# @param mplot main plot
# @param splot sub plot
# @param ssplot sub-sub plot
# @param rowf row factor
# @param colf col factor
# @export

fbdesign_agrofims <- function(design, rep=2, block=2, trt=2, ntrt=NULL, 
                               fnames=NULL,flevels=NULL,
                               mplot=NULL, splot=NULL, ssplot=NULL,
                               rowf=NULL, colf=NULL){
            
            if (design == "crd") {
              fb<- try(st4gi::cr.crd(geno = trt,nrep = rep))
              names(fb)<- c("PLOT", "ROW","COL","TREATMENT")
            }
            
            if (design == "rcbd") {
              fb<- try(st4gi::cr.rcbd(geno = trt,nb = block))
              names(fb) <- c("PLOT","ROW","COL","BLOCK","TREATMENT")
            }
            
            if (design == "fcrd") { ##factorial crd
              fb <- try(st4gi::cr.f(fnames = fnames, flevels = flevels, design = "crd", nrep = rep)$book)
              names(fb)[1:4] <- c("PLOT","ROW","COL","TREATMENT") #rename first 4 cols
            }
            
            if (design == "frcbd") { ##factorial rcbd
              fb <- try(st4gi::cr.f(fnames = fnames, flevels = flevels, design = "rcbd", nrep = rep)$book)
              names(fb)[1:5] <- c("PLOT","BLOCK" ,"ROW","COL","TREATMENT")  #rename first 5 cols
            }
            
            if (design == "sprcbd") { #split plot rcbd
              fb <- try(st4gi::cr.spld(fnames = fnames ,flevels = flevels ,nb = block)$book )
              names(fb)[1:6] <- c("BLOCK" ,"PLOT","SUBPLOT","ROW","COL","TREATMENT")  #rename first 5 cols
              
            } #R.Eyzaguirre recommends just one Split Design
            
            if (design == "spsp") { #split-split plot
              fb <- try(st4gi::cr.spld(fnames = fnames ,flevels = flevels ,nb = block)$book )
              names(fb)[1:7]<- c("BLOCK" ,"PLOT","SUBPLOT","SUB-SUB-PLOT","ROW","COL","TREATMENT")
            }
            
            if (design == "strip") { #strip plot
              fb <- try(st4gi::cr.strd(A = ,B = ,nb = )$book )
              names(fb)[1:4] <- c("PLOT", "BLOCK", "ROW","COL")
            }
            
            fb
          
}

## Get factors from design tab ############################################################
get_factors_design <- function(allinputs, design="fcrd",duplicate= TRUE){
  
  
  lookup <- paste0(design,"_sel_factor_")
  
  dt <- allinputs %>%  filter(!str_detect(id, "button")) %>%
    filter(!str_detect(id, "-selectized")) %>%
    filter(str_detect(id, lookup))
  
  out<- dt$values
  if(duplicate){
    out<- paste0(out,"_f", 1:length(out))
    out <- stringr::str_replace_all(out,pattern = "[[:space:]]",replacement = "_")
  }
  
  out
}

## Get levels from design tab #############################################################
get_levels_design <- function(allinputs, design="fcrd", format=c("list","data.frame")){
  
  format<- match.arg(format)
  
  lookup<- paste0(design,"_lvl_")
  dt <- allinputs %>%  filter(!str_detect(id, "add")) %>%
    filter(!str_detect(id, "button")) %>%
    filter(!str_detect(id, "unit")) %>% 
    filter(!str_detect(id, "_sel_factor_")) %>%
    filter(!str_detect(id, "-selectized")) %>%  
    filter(str_detect(id, lookup))
  
  print(dt)
  
  if(format=="list"){
    flvl<- dt$values
    out<-NULL
    for(i in 1:length(flvl)){
      out[[i]] <- strsplit(flvl[i],split = ", ")[[1]] %>% str_trim(side = "both")
    }
  }
  if(format=="data.frame"){
    out<- dt
  }
  out
}

# Get units from design tab ###############################################################
# get_levels_units_design<- function(flevels, design="frcbd"){
#   
#   lookup<- paste0(design, "_lvl_unit")
#   dtunits <- allinputs %>%   
#         filter(!str_detect(id, "-selectized")) %>% 
#         filter(str_detect(id, lookup))
#   
#   stringdist_left_join(lvl, units, by ="id", max_dist = 5)
#   
#   
#   
# }


