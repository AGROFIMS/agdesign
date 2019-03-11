# get crop measurement tables for intercrop trials

intercrop_cmtables <- function(dtInterCrop, inter_row_selected ){
  
  a<- dtInterCrop  #fg()
  colnames(a) <- c("Crop","Group","Subgroup","Measurement",
                   "TraitUnit","CropMeasurementPerSeason",
                   "CropMeasurementPerPlot","TraitAlias",
                   "TraitDataType","TraitValidation","VariableId")
  
  if(nrow(a) >0){
    #row_select <- inter_row_selected #input$tblMono_rows_selected
    row_select <- sort(inter_row_selected)
    aux_dt<- a[row_select,]
    #Remove Status column
    aux_dt$Status <- NULL
    
    ##OLD CODE
    #Place TraitName in traits_dt()
    cr<- aux_dt$Crop
    sb<- aux_dt$Subgroup
    cm <- aux_dt$Measurement
    sc <- aux_dt$TraitUnit
    sc[is.na(sc)] <- "unitless"
    cs <- paste(cr,sb, cm, sc, sep="_")
    aux_dt$TraitName <- cs
    #--- Asign final trait_dt to a
    a<- aux_dt
    #END OLD CODE
  }
  
}  

intercrop_phetables <- function(dtInterCrop, fbdesign, inter_row_selected ){

  row_select <- sort(inter_row_selected)
  dt <- dtInterCrop[row_select, ]
  lbl <- dt$TraitName
  
  if(length(lbl)==0 && nrow(dt)==0){
    dt <- data.frame()
  } else if(nrow(fbdesign)==0 && length(lbl)>=1){
    dt<- t(rep("", length(lbl)))%>% as.data.frame(stringAsFactors=FALSE)
    names(dt) <- lbl
  } else if(nrow(fbdesign)>0 && length(lbl)>=1) {
    dt<- t(rep("", length(lbl)))%>% as.data.frame(stringAsFactors=FALSE)
    names(dt) <- lbl
    dt <-cbind(fbdesign ,dt)
  }
  dt
  
}
 
intercrop_phe_vars <- function(dtInterCrop,  inter_row_selected ){
  
  row_select <- sort(inter_row_selected)
  dt <- dtInterCrop[row_select, ]
  #lbl <- dt$Measurement
  
}




