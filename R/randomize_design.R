#
# #' Design a fieldbook.
# #'
# #' @param design statistical design
# #' @param matl list of germplasm
# #' @param reps number of plot repetitions
# #' @param msite logical, is this a mother site in a mother baby trial
# #' @param lbls labels for germplasm
# #' @param checkl list of check germplasm
# #' @param bsize block size
# #' @param adfn name of additional factor
# #' @param adfl levels of additional factor
# #' @param startn start number
# #' @param seed random seed
# #' @param randM randomization method
# #' @author Raul Eyzaguirre, Reinhard Simon
# #' @return data.frame
# #' @export
# randomize.design = function(design="(CRD)",
# 		matl, # Vector of material list
# 		reps, # number of repetitions
# 		msite=FALSE, #is mother site in a M & B design
# 		lbls, # short names of labels of genotypes
# 		checkl = NULL, # check genotypes for ABD
# 		bsize=2,# block size only BIB/A01D
# 		adfn=NULL,   # name for additional factor
# 		adfl=NULL,   # vector of additional factor levels
# 		startn = 1,
# 		seed = 0,
# 		randM="Super-duper"
# ) {
#   #library(stringr)
# abb=lbls
# diseno = NULL
# if (str_detect(design,"(UDNR)")){
# 	diseno = as.data.frame(matrix(NA, nrow=length(matl), ncol=3), stringsAsFactors=F)
# 	diseno[,1:3] <- cbind(Plot=seq(startn, startn+length(matl)-1),Rep=rep(1,length(matl)),X=matl)
# 	colnames(diseno)[1:3] <- c("PLOT", "REP", lbls)
# #	labs = as.data.frame(matrix(c("Plot", "Block or repetition", lbls), nrow=1), strings.as.factors=F)
# }
#
# if (str_detect(design,"(CRD)")){
# 	diseno = as.data.frame(matrix(NA, nrow=reps*length(matl), ncol=3), stringsAsFactors=F)
# 	diseno[,1:3] <- design.crd(matl, reps, number=startn)
# 	colnames(diseno) <- c("PLOT", "REP", lbls)
# #	labs = as.data.frame(matrix(c("Plot", "Block or repetition", lbls), nrow=1), strings.as.factors=F)
# }
# if (str_detect(design,"(RCBD)")){
# 	diseno = as.data.frame(matrix(NA, nrow=reps*length(matl), ncol=3), stringsAsFactors=F)
# 	diseno[,1:3]<- design.rcbd(matl, reps, serie=startn)$book
# 	colnames(diseno) <- c("PLOT", "REP", lbls)
# 	#colnames(diseno) <- c("Plot", "Block", abbreviate(inst[3]), abb)
# #	labs = as.data.frame(matrix(c("Plot", "Block or repetition", lbls), nrow=1), strings.as.factors=F)
# }
# # if (str_detect(design,"(BIBD)")){
# # 	bib <- design.bib(matl, bsize, number=startn)
# # 	diseno = as.data.frame(matrix(NA, nrow=dim(bib)[1], ncol=3), stringsAsFactors=F)
# # 	diseno[,1:3] <- bib
# # 	colnames(diseno) <- c("PLOT", "REP", lbls)
# # #	labs = as.data.frame(matrix(c("Plot", "Block or repetition", lbls), nrow=1), strings.as.factors=F)
# # }
# if (str_detect(design,"(LSD)")){
# 	diseno = as.data.frame(matrix(NA, nrow=length(matl)^2, ncol=4), stringsAsFactors=F)
# 	diseno[,1:4]<- design.lsd(matl, number=startn)
# 	colnames(diseno) <- c("PLOT", "REP", "CBLOCK", lbls)
# #	labs = as.data.frame(matrix(c("Plot", "Row Block", "Column Block", lbls), nrow=1), strings.as.factors=F)
# }
# if (str_detect(design,"(F2CRD)")){
# 	nt <- length(matl)*length(adfl)
# 	tr <- 1:nt
# 	est <- cbind(1:nt, rep(adfl, length(matl)), rep(matl, each=length(adfl)))
# 	diseno = as.data.frame(matrix(NA, nrow=reps*nt, ncol=4), stringsAsFactors=F)
# 	fdcrd <- design.crd(tr, reps)$book
# 	diseno[,1:2] <- fdcrd[,1:2]
# 	ord <- fdcrd[,3]
# 	for (i in 1:(nt*reps)){
# 		diseno[i,3] <- est[est[,1]==ord[i],2]
# 		diseno[i,4] <- est[est[,1]==ord[i],3]
# 	}
# 	colnames(diseno) <- c("PLOT", "REP", adfn, lbls)
# 	colnames(diseno) <- c("PLOT", "REP", "FACTOR", lbls)  #cambiamos la etiqueta adfn por FACTOR para evitar erores
# #	labs = as.data.frame(matrix(c("Plot", "Block or repetition", "Factor A", "Factor B", lab), nrow=1), strings.as.factors=F)
# }
#
# if (str_detect(design,"(F2RCBD)")){
# 	nt <- length(matl)*length(adfl)
# 	tr <- 1:nt
# 	est <- cbind(1:nt, rep(adfl, length(matl)), rep(matl, each=length(adfl)))
# 	diseno = as.data.frame(matrix(NA, nrow=reps*nt, ncol=4), stringsAsFactors=F)
# 	fdrcbd <- design.rcbd(tr, reps, number=startn)
# 	diseno[,1:2] <- fdrcbd[,1:2]
# 	ord <- fdrcbd[,3]
# 	for (i in 1:(nt*reps)){
# 		diseno[i,3] <- est[est[,1]==ord[i],2]
# 		diseno[i,4] <- est[est[,1]==ord[i],3]
# 	}
# 	colnames(diseno) <- c("PLOT", "REP", adfn, lbls)
# 	colnames(diseno) <- c("PLOT", "REP", "FACTOR", lbls)
# #	labs = as.data.frame(matrix(c("Plot", "Block or repetition", "Factor A", "Factor B", lab), nrow=1), strings.as.factors=F)
# }
# if (str_detect(design,"(SPCRD)")){
# 	diseno = as.data.frame(matrix(NA, nrow=reps*length(matl)*length(adfl), ncol=4), stringsAsFactors=F)
# 	diseno[,1:4] <- design.split(adfl, matl, reps, "crd", number=startn)
# 	colnames(diseno) <- c("PLOT", "REP", adfn, lbls)
# 	colnames(diseno) <- c("PLOT", "REP", "FACTOR", lbls)
# #	labs = as.data.frame(matrix(c("Plot", "Block or repetition", "Factor in Plots", "Factor in SubPlots", lab), nrow=1), strings.as.factors=F)
# }
# if (str_detect(design,"(SPRCBD)")){
# 	diseno = as.data.frame(matrix(NA, nrow=reps*length(matl)*length(adfl), ncol=4), stringsAsFactors=F)
# 	diseno[,1:4] <- design.split(adfl,matl,reps, "rcbd", number=startn)
#    colnames(diseno) <- c("PLOT", "REP", adfn, lbls)
#    colnames(diseno) <- c("PLOT", "REP","FACTOR",lbls)
# #	labs = as.data.frame(matrix(c("Plot", "Block or repetition", "Factor in Plots", "Factor in SubPlots", lab), nrow=1), strings.as.factors=F)
# }
# if (str_detect(design,"(ABD)")){
# 	diseno = as.data.frame(matrix(NA, nrow=reps*length(checkl)+length(matl), ncol=3), stringsAsFactors=F)
# 	diseno[,1:3] <- design.dau(checkl, matl, reps, number=startn)
# 	colnames(diseno) <- c("PLOT", "REP", lbls)
# }
# if (str_detect(design,"(A01D)")){
# 	# GTDM-396
# 	diseno = as.data.frame(matrix(NA, nrow=reps*length(matl), ncol=4), stringsAsFactors=F)
# 	diseno[,1:4] <- design.alpha(matl, bsize, reps, number=startn)$book[,c(1,5,3,4)]
# 	colnames(diseno) <- c("PLOT", "REP", "BLOCK", lbls)
# }
# # if (str_detect(design,"(MBCRD)")){
# # 	if(msite){ # use CRD design
# # 		diseno = as.data.frame(matrix(NA, nrow=reps*length(matl), ncol=3), stringsAsFactors=F)
# # 		diseno[,1:3] <- design.crd(matl, reps, number=startn)
# # 		colnames(diseno) <- c("PLOT", "REP", lbls)
# # 	}
# # 	if(!msite){# use unreplicated non-randomized design
# # 		diseno = as.data.frame(matrix(NA, nrow=length(matl), ncol=3), stringsAsFactors=F)
# # 		diseno[,1:3] <- cbind(Plot=seq(startn, startn+length(matl)-1),Rep=rep(1,length(matl)),X=matl)
# # 		colnames(diseno)[1:3] <- c("PLOT", "REP", lbls)
# # 	}
# # }
# if (str_detect(design,"(STRIP)")){
#   diseno = as.data.frame(matrix(NA, nrow=reps*length(matl)*length(adfl), ncol=4), stringsAsFactors=F)
#   diseno[,1:4] <- design.strip(adfl, matl, reps, number=startn)
#   colnames(diseno) <- c("PLOT", "REP", adfn, lbls)
#   colnames(diseno) <- c("PLOT", "REP", "FACTOR", lbls)
#   #	labs = as.data.frame(matrix(c("Plot", "Block or repetition", "Factor in Plots", "Factor in SubPlots", lab), nrow=1), strings.as.factors=F)
# }
#
#
#
# diseno
#
# }




#' Unreplicated design with no-randomization
#'
#' @param trt Treatments
#' @param r A Pseudo replications. It's just preserve the order. By default r=1.
#' @author Omar Benites
#' @description Creates an unreplicated design with no-randomization.
#' @export

design.undr <- function(trt,r=1){
  diseno = as.data.frame(matrix(NA, nrow=length(trt), ncol=3), stringsAsFactors=F)
  diseno[,1:3] <- cbind(Plot=seq(r, r+length(trt)-1), Rep=rep(1,length(trt)),X=trt)
  diseno <- as.data.frame(diseno)
  outdesign <- list(treatments = trt, book = diseno)
  return(outdesign)
  #colnames(diseno)[1:3] <- c("PLOT", "r", lbls)
}


#' Factorial Two-Way Design under Complete Randomization
#'
#' @param trt1 Treatments
#' @param trt2 The factorial level
#' @param r Replications or blocks
#' @param series The plot start number.  1: 11,12; 2: 101,102; 3: 1001,1002
#' @param random TRUE or FALSE - randomize
#' @author Omar Benites
#' @description Creates an Factorial Two-Way Design under Complete Randomization
#' @export

design.f2crd <- function(trt1, trt2, r, series=1, random=TRUE){

  nt <- length(trt1)*length(trt2)
  tr <- 1:nt
  est <- cbind(1:nt, rep(trt2, length(trt1)), rep(trt1, each=length(trt2)))
  diseno = as.data.frame(matrix(NA, nrow=r*nt, ncol=4), stringsAsFactors=F)
  #fdcrd <- design.crd(tr, r, number=startn,first = TRUE)
  fdcrd <- agricolae::design.crd(tr, r, series, randomization = random, seed = 1234)
  fdcrd <- fdcrd$book
  diseno[,1:2] <- fdcrd[,1:2]
  ord <- fdcrd[,3]
  for (i in 1:(nt*r)){
    diseno[i,3] <- est[est[,1]==ord[i],2]
    diseno[i,4] <- est[est[,1]==ord[i],3]
  }
 #diseno[,1] <- 1:nrow(diseno)
 #colnames(diseno) <- c("PLOT", "REP", trt2, lbls)
 outdesign <- list(treatment=trt1, repetition=r, factor=trt2, book=diseno)
 return(outdesign)
 #colnames(diseno) <- c("PLOT", "REP", trt2)
 # colnames(diseno) <- c("PLOT", "REP", "FACTOR", lbls)
}


#' Factorial Two-Way Design under Complete Block Randomization
#'
#' @param trt1 Treatments
#' @param trt2 The factorial level
#' @param r Replications or blocks
#' @param series The plot start number.  1: 11,12; 2: 101,102; 3: 1001,1002
#' @param random TRUE or FALSE - randomize
#' @author Omar Benites
#' @description Creates an factorial two-Way design under Complete Block Randomization
#' @export
#
design.f2rcbd <- function(trt1, trt2, r, series=1, random=TRUE){

  nt <- length(trt1)*length(trt2)
  tr <- 1:nt
  est <- cbind(1:nt, rep(trt2, length(trt1)), rep(trt1, each=length(trt2)))
  diseno = as.data.frame(matrix(NA, nrow=r*nt, ncol=4), stringsAsFactors=F)
  #fdcrd <- design.crd(tr, r, number=startn,first = TRUE)
  fdrcbd <- agricolae::design.rcbd(tr, r, series, randomization = random,seed = 1234)
  fdrcbd <- fdrcbd$book
  diseno[,1:2] <- fdrcbd[,1:2]
  ord <- fdrcbd[,3]
  for (i in 1:(nt*r)){
    diseno[i,3] <- est[est[,1]==ord[i],2]
    diseno[i,4] <- est[est[,1]==ord[i],3]
  }
  #diseno[,1] <- 1:nrow(diseno)
  #colnames(diseno) <- c("PLOT", "REP", trt2, lbls)
  outdesign <- list(treatment=trt1, repetition=r, factor=trt2, book=diseno)
  return(outdesign)
}


#' Alpha Design Condition Checker
#'
#' @param trt Treatments
#' @param k Block size
#' @param r Replication
#' @description Function to check the necessary conditions
#' @export
#'
design.alpha.check <- function(trt, k, r){
  res=F
  n = length(trt)
  s = n/k
  n=length(trt)
  vn=seq_len(n) #secuencia de 1 hasta n
  p=vn[n%%vn==0] #divisores del numero de genotipos

  if(length(trt)==12 && k==2 && r==2){
    res="This combination is unable"
    alfares=FALSE
  }

  if(n%%k!=0)
  {
    res="The number of treatments must be multiple of K (size block)"
    alfares=FALSE
  }else if(n<12)
  {res="At least 12 genotypes for Alpha Design (A01D)";
  alfares=FALSE #At least 12 genotyopes
  }else if(r==2)    # Series I
  {
    if(k<=s)
    {
      res=TRUE
      alfares=TRUE
    }else
    {
      knew=p[p>2 & p!=k & p<k]
      snew=n/knew
      ksug=knew[which(snew>knew)]
      res=paste("For this number of replication r = ",r," , we suggest this size block (K): ",paste(ksug,collapse=" , "),sep="")
      alfares=FALSE
    }
  }else if(r==3)
  {
    if((s-1)%%2==0 && k<=s)
    {
      res=TRUE
      #res=paste("For this number of replication r = ",r," , we suggest this size block (K): ",paste(ksug,collapse=" , "),sep="")
      alfares=TRUE
    }else if(s%%2==0 && k<=(s-1))
    {
      res=TRUE
      alfares=TRUE
    }else
    {
      knew=p[p>2 & p!=k & p<k] #suggested number
      snew=n/knew
      ksug=knew[which(snew>knew)]
      #res=ksug
      res=paste("For this number of replication r = ",r," , we suggest this size block (K): ",paste(ksug,collapse=" , "),sep="")
      alfares=FALSE
    }
  }else if(r==4)
  {
    if((s-1)%%2==0 && k<=(s) && (s%%3!=0))
    {
      #res=TRUE
      #res=paste("We suggest this size block (K): ",paste(ksug,collapse=" , "),sep="")
      res=paste("For this number of replication r = ",r," , we suggest this size block (K): ",paste(ksug,collapse=" , "),sep="")
      alfares=TRUE
    }else if(s%%2==0)
    {
      res="The number of blocks must be odd [(S)] = [ Number of genotypes (N) ] / [ Block size (K) ]" #debe ser un numero impar para r=4
    }else
    {
      knew=p[p%%3!=0 & p<s & p>2 & p<=sqrt(n) & p!=k]  #suggested number
      snew=n/knew
      ksug=knew[which(snew>knew)]
      #res=ksug
      res=paste("For this number of replication r = ",r," , we suggest this size block (K): ",paste(ksug,collapse=" , "),sep="")
      alfares=FALSE
    }
  }else
  {
    res=FALSE
  }
  if(length(res)==0){res=FALSE}
  res=list(res=res, alfares=alfares)
  return(res)
}

#trt1 <- material_list()$institutional
#input$designFieldbook_r
#input$designFieldbook_k






