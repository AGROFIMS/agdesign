factor_dt <- function(){


  vinfExp <-""
  if(!is.null(input$info_experiment_unit)) vinfExp <- input$info_experiment_unit


  c1 <- c('Information on experimental unit',vinfExp )

  vfarea <-""
  vfexpmaxwidth <- ""
  vfexpmaxlength <- ""
  vpdiam <- ""
  vpdpth <- ""
  if(vinfExp == "plot"  ){
    vfarea <- vinfExp
    wunit <- ""
    lunit <- ""
    if(!is.null(input$expt_plot_width_unit))  wunit <- input$expt_plot_width_unit
    if(!is.null(input$expt_plot_length_unit))  lunit <- input$expt_plot_length_unit
    vfexpmaxwidth <- paste0(input$expt_plot_width, " " , wunit)
    vfexpmaxlength <- paste0(input$expt_plot_length, " " , lunit)
  }
  else if(vinfExp == "field"){
    vfarea <- vinfExp
    wunit <- ""
    lunit <- ""
    if(!is.null(input$expt_field_width_unit))  wunit <- input$expt_field_width_unit
    if(!is.null(input$expt_field_length_unit))  lunit <- input$expt_field_length_unit
    vfexpmaxwidth <- paste0(input$expt_field_width, " " , wunit)
    vfexpmaxlength <- paste0(input$expt_field_length, " " , lunit)
  }
  else if(vinfExp == "pot"){
    wunit <- ""
    lunit <- ""
    if(!is.null(input$pot_diameter_unit))  wunit <- input$pot_diameter_unit
    if(!is.null(input$pot_depth_unit))  lunit <- input$pot_depth_unit
    vpdiam <- paste0(input$pot_diameter, " " , wunit)
    vpdpth <- paste0(input$pot_depth, " " , lunit)

  }

  c2 <- c('Field area',vfarea )
  c3 <- c('Experimental field maximum width', vfexpmaxwidth)
  c4 <- c('Experimental field maximum length', vfexpmaxlength)
  c5 <- c('Pot diameter',vpdiam )
  c6 <- c('Pot depth',vpdpth )
  c7 <- c('Experimental design', input$designFieldbook_agrofims)
  c8 <- c('Experimental design abbreviation', "")
  c9 <- c('Number of replications', input$designFieldbook_agrofims_r)
  c40 <- c('Number of factors', input$nfactors_hdafims)

  levels1 <- c("NA", "NA", "NA", "NA","NA")
  levels2 <- c("NA", "NA", "NA", "NA","NA")
  levels3 <- c("NA", "NA", "NA", "NA","NA")
  levels4 <- c("NA", "NA", "NA", "NA","NA")
  levels5 <- c("NA", "NA", "NA", "NA","NA")
  levelsDt <- data.table(levels1,levels2,levels3,levels4,levels5)


  nf <- input$nfactors_hdafims

  factors <- c("NA", "NA", "NA", "NA","NA")
  for(i in 1:nf){
    g1 <- input[[paste0("sel" , i, "_2" )]]
    g2 <- input[[paste0("sel" , i, "_3" )]]
    if(!is.null(g1) && !is.null(g2)){
      factors[i] <- paste0(g1, " ", g2)

      g3 <- input[[paste0("sel" , i, "_3" )]]
      ls1 <- input[[paste0("numLevels_", i)]]
      if(is_numeric(ls1) && !is.null(g3)){
        if (ls1>5) ls1 <- 5 #max5
        if(g3 %like% "date"){
          for(j in 1:ls1){
            sdate <- input[[paste0("factor_start_date_",i, "_", j)]]
            edate <- input[[paste0("factor_end_date_",i, "_", j)]]
            levelsDt[i,j] <- paste0(sdate, " - ", edate)
          }
        }
        else{
          nl <- input[[paste0("levels_",i)]]
          count <- 1
          for(lv in nl){
            if(count <= 5){
              if(is.null(input[[paste0("funits_", i)]])){
                levelsDt[i,count] <- lv
              }
              else{
                levelsDt[i,count]<- paste0(lv, " ", input[[paste0("funits_", i)]])
              }
            }
            count <- count + 1
          }
        }
      }

    }

  }


  c10 <- c('Factor 1',factors[1])
  c11 <- c('Factor 1 - level 1',levelsDt[1,1])
  c12 <- c('Factor 1 - level 2',levelsDt[1,2] )
  c13 <- c('Factor 1 - level 3',levelsDt[1,3])
  c14 <- c('Factor 1 - level 4',levelsDt[1,4] )
  c15 <- c('Factor 1 - level 5',levelsDt[1,5] )

  c16 <- c('Factor 2', factors[2])
  c17 <- c('Factor 2 - level 1',levelsDt[2,1])
  c18 <- c('Factor 2 - level 2',levelsDt[2,2])
  c19 <- c('Factor 2 - level 3',levelsDt[2,3])
  c20 <- c('Factor 2 - level 4',levelsDt[2,4] )
  c21 <- c('Factor 2 - level 5',levelsDt[2,5] )

  c22 <- c('Factor 3', factors[3])
  c23 <- c('Factor 3 - level 1',levelsDt[3,1] )
  c24 <- c('Factor 3 - level 2',levelsDt[3,2] )
  c25 <- c('Factor 3 - level 3',levelsDt[3,3] )
  c26 <- c('Factor 3 - level 4',levelsDt[3,4] )
  c27 <- c('Factor 3 - level 5',levelsDt[3,5] )

  c28 <- c('Factor 4', factors[4] )
  c29 <- c('Factor 4 - level 1',levelsDt[4,1])
  c30 <- c('Factor 4 - level 2',levelsDt[4,2] )
  c31 <- c('Factor 4 - level 3',levelsDt[4,3])
  c32 <- c('Factor 4 - level 4',levelsDt[4,4] )
  c33 <- c('Factor 4 - level 5',levelsDt[4,5])

  c34 <- c('Factor 5', factors[5])
  c35 <- c('Factor 5 - level 1',levelsDt[5,1] )
  c36 <- c('Factor 5 - level 2',levelsDt[5,2])
  c37 <- c('Factor 5 - level 3',levelsDt[5,3])
  c38 <- c('Factor 5 - level 4',levelsDt[5,4] )
  c39 <- c('Factor 5 - level 5',levelsDt[5,5])


  df_metadata <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c40,c10,
                            c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,
                            c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,
                            c31,c32,c33,c34,c35,c36,c37,c38,c39)
  var_metadata <<-  t(df_metadata)
  # print(df_metadata)

}



