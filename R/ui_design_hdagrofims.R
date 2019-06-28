shiny::HTML("<a name ='top'></a>")
# choices for statistical design input for HIDAP-AGROFIMS
listCountries <- c('Aruba','Afghanistan','Angola','Anguilla','Albania','Andorra','United Arab Emirates','Argentina','Armenia','American Samoa','Antarctica','French Southern Territories','Antigua and Barbuda','Australia','Austria','Azerbaijan','Burundi','Belgium','Benin','Bonaire','Burkina Faso','Bangladesh','Bulgaria','Bahrain','Bahamas','Bosnia and Herzegowina','Belarus','Belize','Bermuda','Bolivia','Brazil','Barbados','Brunei','Bhutan','Burma','Bouvet Island','Botswana','Byelorusian SSR (Former)','Central African Republic','Canada','Cocos (Keeling) Islands','Switzerland','Chile','China','CIPHQ','Cote dIvoire','Cameroon','Congo','Congo','Cook Islands','Colombia','Comoros','Cape Verde','Costa Rica','Czechoslovakia (Former)','Cuba','Curacao','Christmas Island (Australia)','Cayman Islands','Cyprus','Czech Republic','German Democratic Republic','Germany','Djibouti','Dominica','Denmark','Dominican Republic','Algeria','Ecuador','Egypt','Eritrea','Western Sahara','Spain','Estonia','Ethiopia','Finland','Fiji','Falkland Islands (Malvinas)','France','Faroe Islands','Micronesia','Gabon','United Kingdom','Georgia','Ghana','Gibraltar','Guinea','Guadeloupe','Gambia','Guinea-Bissau','Equatorial Guinea','Greece','Grenada','Greenland','Guatemala','French Guiana','Guam','Guyana','Hong Kong','Heard and Mc Donald Islands','Honduras','Croatia','Haiti','Hungary','Indonesia','India','British Indian Ocean Territory','Ireland','Iran','Iraq','Iceland','Israel','Italy','Jamaica','Jordan','Japan','Kazakhstan','Kenya','Kyrgyzstan','Cambodia','Kiribati','Saint Kitts and Nevis','Korea','Kuwait','Lao People s Democratic Republic','Lebanon','Liberia','Libyan Arab Jamahiriya','Saint Lucia','Liechtenstein','Sri Lanka','Lesotho','Lithuania','Luxemburg','Latvia','Macau','Saint Martin (French part)','Macedonia','Morocco','Monaco','Moldova','Madagascar','Maldives','Mexico','Marshall Islands','Mali','Malta','Myanmar','Mongolia','Northern Mariana Islands','Mozambique','Mauritania','Montserrat','Martinique','Mauritius','Malawi','Malaysia','Mayotte','Namibia','New Caledonia','Niger','Norfolk Island','Nigeria','Nicaragua','Niue','Netherlands','Norway','Nepal','Nauru','Neutral Zone (Former)','New Zealand','Oman','Pakistan','Palestine','Panama','Pitcairn Islands','Peru','Philippines','Palau','Papua New Guinea','Poland','Puerto Rico','Korea','Portugal','Paraguay','French Polynesia','Qatar','Reunion','Romania','Russian Federation','Rwanda','Saudi Arabia','Serbia and Montenegro','Scotland','Sudan','Senegal','Singapore','Saint Helena','Svalbard and Jan Mayen Islands','Solomon Islands','Sierra Leone','El Salvador','San Marino','Somalia','Saint Pierre and Miquelon','Serbia','Sao Tome e Principe','Union of Soviet Socialist Republics (Former)','Surinam','Slovakia','Slovenia','Sweden','Swaziland','Seychelles','Syrian Arab Republic','Turks and Caicos Islands','Chad','Togo','Thailand','Tajikistan','Tokelau','Turkmenistan','East Timor','Tonga','Trinidad and Tobago','Tunisia','Turkey','Tuvalu','Taiwan','Tanzania','Uganda','Ukraine','United States Misc. Pacific Islands','unknown','Uruguay','United States of America','Uzbekistan','Vatican City State','Saint Vincent and the Grenadines','Venezuela','British Virgin Islands','Virgin Islands (US)','Viet Nam','Vanuatu','Wallis and Fortuna Islands_','Samoa','Yemen','Yugoslavia (Former)','South Africa','Zaire','Zambia','Zimbabwe')
design_choices_agrofims <- c(
  #"Unreplicated Design (UNDR)" = "UNDR",
  #"Westcott Design (WD)" = "WD",#
  "Completely Randomized Design (CRD)" = "CRD",
  "Randomized Complete Block Design (RCBD)" = "RCBD",
  "Factorial with CRD"="FCRD",
  "Factorial with RCBD"="FRCBD",
  #"Augmented Block Design (ABD)" = "ABD",
  #"Alpha Design (AD)" = "AD",
  #"Latin Square Design (LSD)" = "LSD",
  #"Factorial Two-Way Design in CRD (F2CRD)" = "F2CRD",
  #"Factorial Two-Way Design in RCBD (F2RCBD)" = "F2RCBD",
  #"Split Plot with Plots in CRD (SPCRD)" = "SPCRD",
  #"Strip-Split-Plot Design" = "STRIP",
  "Split Plot Design" = "SPRCBD", #R.Eyzaguirre recommends just one Split Design
  "Split-Split Plot Design"="SPSP"
  #"Strip Plot Design"= "STRIP"
  #"Split Plot with Plots Design" = "SPRCBD", #
  #"Split Plot with Plots in LSD (SPLSD)" = "SPLSD",
  #"Strip Plot Design (STRIP)" = "STRIP"
  # "Balanced Incomplete Block Design (BIBD)" = "BIBD",
  # "Graeco-Latin Design (GLD)" = "GLD",
  # "Youden Design (YD)" = "YD",
  # "Cyclic Design (CD)" = "CD",
  # "Lattice Design (LD)" = "LD" ,
  # #"Augmented Partially Replicated Design (APRD)" = "APRD",
  # #"Factorial Design (F2SPPD)" = "F2SPPD",
  # "North Carolina Design I" = "NCI"#,
  # #"North Carolina Design II" = "NCII",
  # #"North Carolina Design III" = "NCIII",
)

# Conditional Panels or features according to each statistical design
design_conditional_panels_agrofims <- function(){
  list(
    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'RCBD'  |
      input.designFieldbook_agrofims == 'CRD' |
      input.designFieldbook_agrofims == 'F2CRD'  |
      input.designFieldbook_agrofims == 'F2RCBD' |
      input.designFieldbook_agrofims == 'ABD' |
      input.designFieldbook_agrofims == 'DAU' |
      input.designFieldbook_agrofims == 'AD'  |
      input.designFieldbook_agrofims == 'SPCRD'|
      input.designFieldbook_agrofims == 'SPRCBD'|
      input.designFieldbook_agrofims == 'SPLSD'|
      input.designFieldbook_agrofims == 'STRIP'",
      # TODO: ab - factorial, split
      shiny::selectInput("designFieldbook_agrofims_r", "Replications", 2:100, 2 )
    ),
    
    # shiny::conditionalPanel(
    #      "input.designFieldbook_agrofims == 'UNDR'  |
    #       input.designFieldbook_agrofims == 'CRD'   |
    #       input.designFieldbook_agrofims == 'RCBD'  |
    #       input.designFieldbook_agrofims == 'F2CRD' |
    #       input.designFieldbook_agrofims == 'F2RCBD'|
    #       input.designFieldbook_agrofims == 'SPRCBD'",
    #
    #   shiny::checkboxInput("designFieldbook_agrofims_cbrwcol", "Add row and column", value = FALSE)#,
    
    # shiny::conditionalPanel(
    #   "input.designFieldbook_agrofims_cbrwcol == true",
    #   shiny::selectInput("designFieldbook_agrofims_expdis_colb", "Number of columns", 2:100, 10)
    # )
    # ),
    
    
    
    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'SPCRD' |
      input.designFieldbook_agrofims == 'SPRCBD'|
      input.designFieldbook_agrofims == 'F2CRD' |
      input.designFieldbook_agrofims == 'F2RCBD'|
      input.designFieldbook_agrofims == 'SPLSD' |
      input.designFieldbook_agrofims == 'STRIP'
      ",
      
      #shiny::selectInput("designFieldbook_agrofims_r", "Replications:", 1:5, 2 ),
      textInput(inputId = "factor_name", label = "Enter Additional Factor Name", ""),
      br(),
      textInput(inputId = "factor_lvl", label = "Type levels of factors (separated by commas ',')", value = "")#,
      # textInput(inputId = "factor_lvl1", label = "First Level for Additional Factor",value = ""),
      # textInput(inputId = "factor_lvl2", label = "Second Level for Additional Factor",value = ""),
      # textInput(inputId = "factor_lvl3", label = "Third Level for Additional Factor",value = "")
      
    ),
    
    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'SPCRD' |
      input.designFieldbook_agrofims == 'SPRCBD'",
      
      shiny::uiOutput("fbdesign_split_cb")
    ),
    
    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'WD'",
      shiny::selectInput("designFieldbook_agrofims_wd_col",  "Number of columns", 50:5000, 100 ),
      shiny::selectInput("designFieldbook_agrofims_wd_colb", "Number of columns between two check columns", 2:100, 10)
      
    ),
    
    
    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'ABD'", # TODO: ab - factorial, split
      infoBox(title = "IMPORTANT NOTE", subtitle = "Augmented design needs at least 2 checks. Verify if your material have these checks and put an 'x' mark in 'Is_control' column for each check.
              Otherwise, Augmented Design does not work.",
              
              icon = icon("bullhorn"), color = "blue", fill = TRUE, width = NULL)
    ),
    
    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'WD'", # TODO: ab - factorial, split
      infoBox(title = "IMPORTANT NOTE", subtitle = "Westcott design needs 2 checks and at least 10 genotypes. Verify if your material list has these checks and put an 'x' mark in 'Is_control' column for each check.
              Otherwise, Westcott Design does not work.",
              
              icon = icon("bullhorn"), color = "blue", fill = TRUE, width = NULL)
    ),
    
    
    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'LD'", # TODO: ab - factorial, split
      shiny::selectInput("designFieldbook_agrofims_r", "Replications", 2:100, 2 )
    ),
    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'BIBD'",
      shiny::selectInput("designFieldbook_agrofims_maxR", "Repetition maximum (k)", 3:100, 20 )
    ) ,
    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'AD' |
      input.designFieldbook_agrofims == 'BIBD'",
      # TODO do server side checking of permitted combinations (based on number of treatments)
      # trt = k * s
      # s = number of blocks
      # k = size of block (max is trt)
      #
      # r = 2: k <= s
      # r = 3: s odd; k <= s
      # r = 3: s even; k <= (s - 1)
      # r = 4: s odd but not multiple of 3; k <= s
      #shiny::selectInput("designFieldbook_agrofims_r", "Replications:", 2:6000, 2 ),
      shiny::selectInput("designFieldbook_agrofims_k", "Block size (k)", 2:100, 4 )
    ),
    
    shiny::conditionalPanel(
      
      "input.designFieldbook_agrofims == 'AD'", # TODO: ab - factorial, split
      fluidRow(
        shiny::wellPanel(
          shiny::HTML("<b>ALPHA CONDITION:</b>"),
          textOutput("alphaMessage")
        )
      )
    ),
    
    #     shiny::conditionalPanel(
    #       "input.designFieldbook_agrofims == 'CD'",
    #       # TODO do server side checking of permitted combinations (based on number of treatments)
    #       # number of treatments 6:30
    #       shiny::selectInput("designFieldbook_agrofims_r", "Replications:", 2:10, 2 ),
    #       shiny::selectInput("designFieldbook_agrofims_k", "Block size (k):", 2:10,3 )
    #     ),
    
    ### Combine factors in statistical designs ########################
    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'UNDR' |
      input.designFieldbook_agrofims == 'CRD'   |
      input.designFieldbook_agrofims == 'RCBD'  |
      input.designFieldbook_agrofims == 'WD'    |
      input.designFieldbook_agrofims == 'AD'    |
      input.designFieldbook_agrofims == 'LSD'   |
      input.designFieldbook_agrofims == 'ABD'",
      shiny::checkboxInput("designFieldbook_agrofims_combfactor", "Add Factor",value = FALSE  ),
      
      shiny::conditionalPanel(
        "input.designFieldbook_agrofims_combfactor == true",
        
        textInput(inputId = "combfactor_name", label = "Combine Factor Name", ""),
        br(),
        textInput(inputId = "combfactor_lvl", label = "Type levels of factors (separated by commas ',')", value = "")#,
        
      )
      
    ),
    #########################################
    
    ###############
    
    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'UNDR'  |
      input.designFieldbook_agrofims == 'CRD'   |
      input.designFieldbook_agrofims == 'RCBD'  |
      input.designFieldbook_agrofims == 'WD'    |
      input.designFieldbook_agrofims == 'AD'    |
      input.designFieldbook_agrofims == 'LSD'   |
      input.designFieldbook_agrofims == 'ABD'",
      shiny::checkboxInput("designFieldbook_agrofims_cbssample", "Add sub samples", value = FALSE  ),
      
      shiny::conditionalPanel(
        "input.designFieldbook_agrofims_cbssample == true",
        shiny::numericInput(inputId = "designFieldbook_agrofims_nsample", label = "Enter samples", value = 10, min = 1, max = 1000)
      )
      
    )#,
    
    
    
  )
}

#' shiny UI element for HIDAP-AGROFIMS
#'
#' returns a re-usable user interface element
#'
#' @author Ivan Perez, Omar Benites
#' @param type type of ui element; default is a tab in a shiny dashboard
#' @param title display title
#' @param name a reference name
#' @export

ui_fieldbook_agrofims <- function(type="tab",title="Design Fieldbook",name="phenotype_fieldbook_design" ) {
  
  shinydashboard::tabItem(
    tabName = name,
    
    shinyjs::useShinyjs(),
    useShinyalert(),
    
    #############################################################################
    ############################### START UI: CSS ###############################
    
    # tags$style("*[id^='v2_'] {
    #            transform: scale(1.8);
    #            }"),
    # 
    # tags$style("#dt_sel {
    #            transform: scale(1.5);
    #            }"),
    
    tags$style("#lastsaved {
               font-style: italic;
               font-size: 13px;
               font-weight: bold;
               margin-top: 4px;
}"),
    
    tags$style("#lastsaved_i {
               font-style: italic;
               font-size: 13px;
               font-weight: bold;
               margin-top: 4px;
               }"),

    tags$style(HTML("
                    .box.box-solid.box-warning>.box-header {
                    color:#000;
                    background:#f5f5f5;
                    /*padding-top:0px*/
                    display: none;
                    }
                    
                    /**[id^='fl_box_fundingAgency_'] .box.box-solid.box-warning>.box-header {
                    display: none;
                    }*/ 
                    
                    .box.box-solid.box-warning>.box-body {
                    color:#000;
                    background:#f5f5f5
                    }
                    
                    .box.box-solid.box-warning{
                    border-bottom-color:#f5f5f5;
                    border-left-color:#f5f5f5;
                    border-right-color:#f5f5f5;
                    border-top-color:#f5f5f5;
                    }
                    
                    .box.box-solid.box-info>.box-header {
                    color:#000;
                    background:#f2dede;
                    /*padding-top:0px*/
                    }
                    
                    .box.box-solid.box-info>.box-body {
                    color:#000;
                    background:#f2dede
                    }
                    
                    .box.box-solid.box-info{
                    border-bottom-color:#f2dede;
                    border-left-color:#f2dede;
                    border-right-color:#f2dede;
                    border-top-color:#f2dede;
                    }
                    
                    #gh {
                    font-size: 24px;
                    }
                    
                    ")),
    
    
    tags$head(
      tags$style(HTML("
                      .shiny-output-error-validation {
                      color: green;
                      font-size: 120%;
                      font-family: Arial, Helvetica, sans-serif;
                      }
                      "))
      ),
    
    # Asociado al boton Next
    tags$head(tags$style(
      HTML('
           #sidebar {
           padding: 19px 20px 20px;
           margin-top: 20px;
           margin-bottom: 20px;
           background-color: #f5f5f5;
           border-top: 1px solid #e5e5e5;
           }
           .well {
           border-radius: 0px;
           
           border: 0px solid #e3e3e3;
           }'
                            )
      )),
    
    tags$style(HTML("
                    .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
                    color: #fff;
                    cursor: default;
                    background-color: #0081c2;
                    border: 1px solid #ddd;
                    border-bottom-color: transparent;}
                    ")),
    
    tags$style("
               #myqr {
               width: 100px !important;
               height: 100px !important;
               }
               
               #uniqueId {
               text-align:center !important;
               }
               "),
    
    tags$style("
               #myqr2 {
               width: 100px !important;
               height: 100px !important;
               }
               
               #uniqueId {
               text-align:center !important;
               }
               "),             
    
    tags$style("
               /*.form-group {
               margin-bottom: 0px;
               }*/
               "),
    
    #Asociado a Management Practices
    tags$style(HTML("
                    .box.box-solid.box-warning>.box-header {
                    color:#000;
                    background:#f5f5f5;
                    /*padding-top:0px*/
                    display: none;
                    }
                    
                    /**[id^='fl_box_fundingAgency_'] .box.box-solid.box-warning>.box-header {
                    display: none;
                    }*/ 
                    
                    .box.box-solid.box-warning>.box-body {
                    color:#000;
                    background:#f5f5f5
                    }
                    
                    .box.box-solid.box-warning{
                    border-bottom-color:#f5f5f5;
                    border-left-color:#f5f5f5;
                    border-right-color:#f5f5f5;
                    border-top-color:#f5f5f5;
                    }
                    
                    #gh {
                    font-size: 24px;
                    }
                    
                    .box.box-solid.box-primary>.box-header {
                    color:#555;
                    font-weight: 800;
                    /* background:#aac309; */
                    background: -webkit-linear-gradient(top, #FAFAFA 0%, #E9E9E9 100%);
                    border: 1px solid #D5D5D5;
                    }
                    
                    .box.box-solid.box-primary{
                    /* border-bottom-color:#aac309;
                    border-left-color:#aac309;
                    border-right-color:#aac309;
                    border-top-color:#aac309; */
                    border: 1px solid #D5D5D5;
                    }
                    
                    .box.box-solid.box-primary>.box-header .btn, .box.box-solid.box-primary>.box-header a {
                    color: #000;
                    }
                    
                    .checkbox, .radio {
                    margin-top: 0px;
                    margin-bottom: -20px;
                    }
                    
                    /*Add lines*/
                    [id*='_to_collect_field']+ div>.selectize-input {
                    background: #d9edf7; color: #ffffff;
                    }
                    ")),
    
    ############################### END UI: CSS ###############################
    ###########################################################################
    
    #####################################################################################
    ############################### START UI: JAVA SCRIPT ###############################
    
    ###################### START: GENERALES ######################
    
    # Code for showing  when other is selected in a combobox
    tags$script('$(document).on("change", "select",  function(){
                var a = this.id + "_other";
                var b = "#" + a ;
                var val = this.value
                
                if(val == "Other") {
                shinyjs.show(a)
                $(b).focus();
                }
                else{shinyjs.hide(a)}
                })
                '
    ),
    
    # ?????
    tags$script('Shiny.addCustomMessageHandler("focus",
                function(a) {
                document.getElementById(a).focus();
                });
                '
    ),
    
    # ?????
    tags$head(
      tags$script(
        HTML("
             var openTab = function(tabName){
             $('a', $('.sidebar')).each(function(){
             if(this.getAttribute('data-value') == tabName){
             this.click()
             };
             });
             }
             ")
        )
        ),
    
    # Abre los boxes
    tags$script('$(document).on("click", "a[id*=\'_titleId\']",  function(){
                Shiny.onInputChange("boxTitleClicked", Math.random());
                Shiny.onInputChange("boxTitleClickedId", this.id);
                })
                '
      ),
    
    ###################### END: GENERALES ######################
    
    ###################### START: EXPERIMENT, PERSONNEL & SITE ######################
    
    # Other de Type of experiment
    tags$script(
      '$(document).on("change", "select[id^=\'designFieldbook_typeExperiment\']:not([id*=\'other\'])",  function(){
        Shiny.onInputChange("othertE", Math.random());
        Shiny.onInputChange("othertEid", this.id);
        })'
    ),
    
    # Other de Vegetation surrounding
    tags$script(
      '$(document).on("change", "select[id^=\'fbDesign_inSiteVegetation\']:not([id*=\'other\'])",  function(){
        Shiny.onInputChange("othertVEG", Math.random());
        Shiny.onInputChange("othertVEGid", this.id);
        })'
    ),
    
    
    # Deleted GENERAL
    tags$script('$(document).on("click", "button[id^=\'exp_closeBox_\']",  function(){
                   Shiny.onInputChange("closeBox_EXP", Math.random());
                   Shiny.onInputChange("closeBox_EXPid", this.id);
                   })
                  '),
    
    ## Personnel
    # Deleted GENERAL
    tags$script('$(document).on("click", "button[id^=\'per_closeBox_\']",  function(){
                   Shiny.onInputChange("closeBox_PER", Math.random());
                   Shiny.onInputChange("closeBox_PERid", this.id);
                   })
                  '),
    
    ###################### END: EXPERIMENT, PERSONNEL & SITE ######################
    
    ###################### START: CROP ######################
    
    # When intercrop is selected
    tags$script('$(document).on("change", "select[id*=\'_cropCommonName_\']",  function(){
                Shiny.onInputChange("cropBoxInterVar", Math.random());
                Shiny.onInputChange("cropBoxInterVarId", this.id);
    })
                '),
    
    # When 'other crop' name is filled multicrop
    tags$script('$(document).on("change", "input[id*=\'_cropCommonName_\']",  function(){
                Shiny.onInputChange("cropBoxMulticropVarOther", Math.random());
                Shiny.onInputChange("cropBoxMulticropVarOtherId", this.id);
    })
                '),
    
    
    # Deleted GENERAL
    tags$script('$(document).on("click", "button[id*=\'_closeCrop_\']",  function(){
                Shiny.onInputChange("closeBox_CROP", Math.random());
                Shiny.onInputChange("closeBox_CROPid", this.id);
    })
                '),
    
    # Code for showing  when other is selected in a combobox
    tags$script('$(document).on("change", "select",  function(){
                var a = this.id + "_other";
                var b = "#" + a ;
                var val = this.value
                
                if(val == "Other") {
                shinyjs.show(a)
                $(b).focus();
                }
                else{shinyjs.hide(a)}
    })
                '
    ),
    
    ###################### END: CROP ######################
    
    ###################### START: DESIGN ######################
    
    # Factor GENERAL
    tags$script(
      '$(document).on("change", "select[id*=\'_sel_factor_\']",  function(){
      Shiny.onInputChange("selectGEN", Math.random());
      Shiny.onInputChange("selectGENid", this.id);
})'
    ),
    # Levels GENERAL
    tags$script(
      '$(document).on("change", "select[id*=\'_numLevels_\']",  function(){
      Shiny.onInputChange("levelsGEN", Math.random());
      Shiny.onInputChange("levelsGENid", this.id);
      })'
    ),
    
    # Timing Levels CASOS ESPECIALES TYPE OTHER
    tags$script(
      '$(document).on("change", "select[id*=\'_lvl_espType_\']",  function(){
      Shiny.onInputChange("otherTypeESP", Math.random());
      Shiny.onInputChange("otherTypeESPid", this.id);
      })'
    ),
    
    # Timing Levels CASOS ESPECIALES
    tags$script(
      '$(document).on("change", "select[id*=\'_lvltiming_\']",  function(){
      Shiny.onInputChange("timingESP", Math.random());
      Shiny.onInputChange("timingESPid", this.id);
      })'
    ),
    
    # Levels CASOS ESPECIALES
    tags$script(
      '$(document).on("change", "select[id*=\'_numLevelsESP_\']",  function(){
      Shiny.onInputChange("levelsESP", Math.random());
      Shiny.onInputChange("levelsESPid", this.id);
      })'
    ),
    
    # Levels CASOS ESPECIALES TIMING
    tags$script(
      '$(document).on("change", "select[id*=\'_numLevelsTimingESP_\']",  function(){
      Shiny.onInputChange("levelsTimingESP", Math.random());
      Shiny.onInputChange("levelsTimingESPid", this.id);
      })'
    ),
    
    
    # Other de Factor GENERAL
    tags$script(
      '$(document).on("change", "select[id*=\'_lvl_\']",  function(){
      Shiny.onInputChange("otherGEN", Math.random());
      Shiny.onInputChange("otherGENid", this.id);
})'
    ),
    # Duplicate GENERAL
    tags$script(
      '$(document).on("click", "button[id*=\'_btDuplicate_\']",  function(){
      Shiny.onInputChange("duplicateGEN", Math.random());
      Shiny.onInputChange("duplicateGENid", this.id);
      })'
    ),
    # Delete GENERAL
    tags$script(
      '$(document).on("click", "button[id*=\'_closeBox_\']",  function(){
      Shiny.onInputChange("closeBox_button_GEN", Math.random());
      Shiny.onInputChange("closeBox_button_GENid", this.id);
      })'
    ),
    # Other/Other GENERAL
    tags$script(
      '$(document).on("change", "select[id*=\'_typeInput_\']",  function(){
      Shiny.onInputChange("otherOthGEN", Math.random());
      Shiny.onInputChange("otherOthGENid", this.id);
      })'
    ),
    # Levels Inputs CRD
    tags$script(
      '$(document).on("change", "select[id*=\'crd_lvl_\']:not([id$=\'_dateinput\'])",  function(){
      Shiny.onInputChange("levelInput", Math.random());
      Shiny.onInputChange("levelInputid", this.id);
      })'
    ),
    # Levels Inputs RCBD
    tags$script(
      '$(document).on("change", "select[id*=\'rcbd_lvl_\']:not([id$=\'_dateinput\'])",  function(){
      Shiny.onInputChange("levelInput", Math.random());
      Shiny.onInputChange("levelInputid", this.id);
      })'
    ),
    # Levels Inputs Dates
    tags$script(
      '$(document).on("change", "input[id$=\'_dateinput\']",  function(){
      Shiny.onInputChange("levelInput", Math.random());
      Shiny.onInputChange("levelInputid", this.id);
      })'
    ),
    # Inputs Factor Columns
    tags$script(
      '$(document).on("change", "select[id*=\'input_factor_treatment_\']",  function(){
      Shiny.onInputChange("input_factor_treatment", Math.random());
      Shiny.onInputChange("input_factor_treatmentid", this.id);
      })'
    ),
    # Inputs Other Factor CRD
    tags$script(
      '$(document).on("change", "input[id*=\'crd_sel_factor_other_\']",  function(){
      Shiny.onInputChange("crd_rcbd_SelFactorOther", Math.random());
      Shiny.onInputChange("crd_rcbd_SelFactorOtherid", this.id);
      })'
    ),
    # Inputs Other Factor RCBD
    tags$script(
      '$(document).on("change", "[id*=\'rcbd_sel_factor_other_\']",  function(){
      Shiny.onInputChange("crd_rcbd_SelFactorOther", Math.random());
      Shiny.onInputChange("crd_rcbd_SelFactorOtherid", this.id);
      })'
    ),
    
    ###################### END: DESIGN ######################
    
    ###################### START: MANAGEMENT PRACTICES ######################
    # Abre los boxes
    tags$script('$(document).on("click", "a[id*=\'_titleId\']",  function(){
                Shiny.onInputChange("boxTitleClicked", Math.random());
                Shiny.onInputChange("boxTitleClickedId", this.id);
})
                '
    ),
    
    # ?????
    tags$head(
      tags$script(
        HTML("
             var openTab = function(tabName){
             $('a', $('.sidebar')).each(function(){
             if(this.getAttribute('data-value') == tabName){
             this.click()
             };
             });
             }
             
             shinyjs.collapse = function(boxid) {
             $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
             }
             //
             ")
        )
        ),
    
    # ?????
    tags$script('Shiny.addCustomMessageHandler("focus",
                function(a) {
                document.getElementById(a).focus();
                });
                '
      ),
    
    # actionButton("cassava_pt_add", "Add Planting & Transplanting")
    # Add planting mono and multicrop
    tags$script('$(document).on("click", "button[id*=\'_pt_add\']",  function(){
                Shiny.onInputChange("PTBoxVar", Math.random());
                Shiny.onInputChange("PTBoxVarid", this.id);
                })
                '),
    # When intercrop is selected planting
    tags$script('$(document).on("change", "select[id*=\'_cropCommonName_\']",  function(){
                Shiny.onInputChange("PTBoxMulticropVar", Math.random());
                Shiny.onInputChange("PTBoxMulticropVarid", this.id);
                })
                '),
    # Add irrigation monocrop
    tags$script('$(document).on("click", "button[id*=\'_irri_add\']",  function(){
                Shiny.onInputChange("IRRIBoxVar", Math.random());
                Shiny.onInputChange("IRRIBoxVarid", this.id);
                })
                '),
    # Delete irrigation GENERAL
    tags$script('$(document).on("click", "button[id*=\'_closeBox_ECIRRI_\']",  function(){
                  Shiny.onInputChange("closeBox_ECIRRI_GEN", Math.random());
                  Shiny.onInputChange("closeBox_ECIRRI_GENid", this.id);
                  })
                  '),
    # Add weeding monocrop
    tags$script('$(document).on("click", "button[id*=\'_wee_add\']",  function(){
                  Shiny.onInputChange("WEEBoxVar", Math.random());
                  Shiny.onInputChange("WEEBoxVarid", this.id);
      })
                  '),
    # Delete weeding GENERAL
    tags$script('$(document).on("click", "button[id*=\'_closeBox_ECWEE_\']",  function(){
                  Shiny.onInputChange("closeBox_ECWEE_GEN", Math.random());
                  Shiny.onInputChange("closeBox_ECWEE_GENid", this.id);
      })
                  '),
    # Add harvest mono and multicrop
    tags$script('$(document).on("click", "button[id*=\'_harv_add\']",  function(){
                  Shiny.onInputChange("HARVBoxVar", Math.random());
                  Shiny.onInputChange("HARVBoxVarid", this.id);
      })
                  '),
    # When intercrop is selected harvest
    tags$script('$(document).on("change", "select[id*=\'_cropCommonName_\']",  function(){
                  Shiny.onInputChange("HARVBoxMulticropVar", Math.random());
                  Shiny.onInputChange("HARVBoxMulticropVarid", this.id);
      })
                  '),
    # Delete harvest GENERAL
    tags$script('$(document).on("click", "button[id*=\'_closeBox_ECHARV_\']",  function(){
                  Shiny.onInputChange("closeBox_ECHARV_GEN", Math.random());
                  Shiny.onInputChange("closeBox_ECHARV_GENid", this.id);
      })
                  '),
    
    ###################### END: MANAGEMENT PRACTICES ######################
    
    ###################### START: MEASUREMENT AND PHENOLOGY ######################
    
    # When crop is selected
    tags$script('$(document).on("change", "select[id*=\'_cropCommonName_\']",  function(){
                Shiny.onInputChange("MEA_PHE_BoxMulticropVar", Math.random());
                Shiny.onInputChange("MEA_PHE_BoxMulticropVarid", this.id);
})
                '),
    
    # Delete GENERAL para MEA
    tags$script(
      '$(document).on("click", "button[id*=\'_closeBox_mea_\']",  function(){
      Shiny.onInputChange("closeBox_button_MEA", Math.random());
      Shiny.onInputChange("closeBox_button_MEAid", this.id);
      })'
    ),
    
    # Add row measurement 
    tags$script(
      '$(document).on("click", "button[id*=\'_mea_\'][id$=\'add\']",  function(){
      Shiny.onInputChange("addRow_button_MEA", Math.random());
      Shiny.onInputChange("addRow_button_MEAid", this.id);
      })'
    ),
    
    # When timing change
    tags$script(
      '$(document).on("change", "select[id*=\'_timing_\']",  function(){
      Shiny.onInputChange("timing_MEA", Math.random());
      Shiny.onInputChange("timing_MEAid", this.id);
      })'
    ),
    
    # When lvls change for rows of measurement
    tags$script(
      '$(document).on("change", "select[id*=\'_timingNumLevels_\']",  function(){
      Shiny.onInputChange("levelsTiming_MEA", Math.random());
      Shiny.onInputChange("levelsTiming_MEAid", this.id);
      })'
    ),
    
    ###################### END: MEASUREMENT AND PHENOLOGY ######################
    
    ###################### START: WEATHER ######################
    
    # Add row weather 
    tags$script(
      '$(document).on("click", "button[id*=\'btn_weather_add\']",  function(){
      Shiny.onInputChange("addRow_button_WEA", Math.random());
      Shiny.onInputChange("addRow_button_WEAid", this.id);
      })'
    ),
    
    # Close button
    tags$script(
      '$(document).on("click", "button[id*=\'weather_closeBox\']",  function(){
      Shiny.onInputChange("closeBox_WEA", Math.random());
      Shiny.onInputChange("closeBox_WEAid", this.id);
      })'
    ),
    
    # When timing change
    tags$script(
      '$(document).on("change", "select[id*=\'weather_timing_\']",  function(){
      Shiny.onInputChange("timing_WEA", Math.random());
      Shiny.onInputChange("timing_WEAid", this.id);
      })'
    ),
    
    # When timing num levels change
    tags$script(
      '$(document).on("change", "select[id*=\'weather_timingNumLevels_\']",  function(){
      Shiny.onInputChange("timingNumLevels_WEA", Math.random());
      Shiny.onInputChange("timingNumLevels_WEAid", this.id);
      })'
    ),
    
    
    ###################### END: WEATHER ######################
    
    
    ###################### START: SOIL ######################
    
    # Add row soil 
    tags$script(
      '$(document).on("click", "button[id*=\'btn_soil_add\']",  function(){
      Shiny.onInputChange("addRow_button_SOIL", Math.random());
      Shiny.onInputChange("addRow_button_SOILid", this.id);
      })'
    ),
    
    # Close button
    tags$script(
      '$(document).on("click", "button[id*=\'soil_closeBox\']",  function(){
      Shiny.onInputChange("closeBox_SOIL", Math.random());
      Shiny.onInputChange("closeBox_SOILid", this.id);
      })'
    ),
    
    # When timing change
    tags$script(
      '$(document).on("change", "select[id*=\'soil_timing_\']",  function(){
      Shiny.onInputChange("timing_SOIL", Math.random());
      Shiny.onInputChange("timing_SOILid", this.id);
      })'
    ),
    
    # When timing num levels change
    tags$script(
      '$(document).on("change", "select[id*=\'soil_timingNumLevels_\']",  function(){
      Shiny.onInputChange("timingNumLevels_SOIL", Math.random());
      Shiny.onInputChange("timingNumLevels_SOILid", this.id);
      })'
    ),
    
    # When measurement change
    tags$script(
      '$(document).on("change", "[id*=\'soil_mea_\']",  function(){
      Shiny.onInputChange("measurement_SOIL", Math.random());
      Shiny.onInputChange("measurement_SOILid", this.id);
      })'
    ),

    
    
    
    ###################### END: SOIL ######################

    
    ############################### END UI: JAVA SCRIPT ###############################
    ###################################################################################
    
    ###################################################################################
    ############################### START UI: INTERFACE ###############################
    
    # HEADER TITLE & SAVE MODULE BUTTONS
    fluidRow(
      column(6, style = "margin-top: -16px; margin-bottom: 16px;", h1("Experiment description")),
      column(
        6, align = "right", style = "margin-top: 11px;"#,
        # actionButton("xtest", "Test"),
        # actionButton('newfieldbook', 'New', icon("file"), class = "btn-primary", style="color: #fff;", width = "75px"),
        # actionButton('openfieldbook', 'Open', icon("folder-open"), width = "75px", onclick = "openTab('uisessionagrofims')"),
        # actionButton('savefieldbook', 'Save', icon("save"), class = "btn-success", style="color: #fff;", width = "75px"),
        # actionButton("testsession", "test"),
        # htmlOutput("lastsaved"),
        # uiOutput("saveUI"),
        # bookmarkButton()
      )
    ),
    
    fluidRow(
      column(12, verbatimTextOutput("text1"))
    ),
    
    # PRINCIPAL BOX: CREATE FIELDBOOK
    fluidRow(
      box(
        title = tagList(shiny::icon("plus-circle"), "Create fieldbook"), status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
        tabsetPanel(
          id= "fbDesignNav",
          
          ##### START: TAB EXPERIMENT #####
          
          shiny::tabPanel(
            title = tagList(shiny::icon("info"), "Experiment"), value = "experiment",
            
            column(
              12,
              fluidRow(
                column(
                  6,
                  h2("Experiment details"),
                  uiOutput("experimentIdUI"),
                  textInput(inputId = "experimentName", label = "Experiment name", value = ""),
                  textInput(inputId = "experimentProjectName", label = "Experiment project name", value = ""),
                  
                  fluidRow(
                    column(
                      6,
                      airDatepickerInput("fbDesign_project_start_date",
                                         "Experiment start date",
                                         clearButton = T,
                                         autoClose = T,
                                         value = Sys.Date() + 1,
                                         placeholder = "yyyy-mm-dd",
                                         addon = "none"
                      )
                    ),
                    column(
                      6,
                      uiOutput("exp_end_date")
                    )
                  ),
                  
                  uiOutput("typeExperiment"),
                  fluidRow(id = "othertE_aux"),
                  textAreaInput(inputId = "experimentObj", label = "Experiment objective", value = "")
                ),
                column(
                  6, 
                  align = "right",
                  br(),
                  imageOutput("myqr"),
                  uiOutput("IdUI")
                )
              )
            ),
            column(
              12,
              br(),
              h2("Funding Agency"),
              fluidRow(id = "fr_fundingAgency_boxes"),
              actionButton("addFundingAgency", "Add a funding agency", icon("plus-circle"), class = "btn-primary", style="color: #fff;" ),
              br(), br()
            ),
            
            column(
              6,
              br(),
              h2("Grant details"),
              textInput("experiment_grantNumber", "Grant number"),
              textInput("experiment_grantId", "Grant Id"),
              br()
            ),
            
            column(
              12,
              h2("Project Management Entities"),
              fluidRow(id = "fr_managementEntities_boxes"),
              actionButton("addManagEntity", "Add a project management entity", icon("plus-circle"), class = "btn-primary", style="color: #fff;"),
              br(), br(), br(), br()
            ),
            
            column(
              12,
              HTML("<div style='margin-bottom: 10px;'><h2 style='display:inline;'>Experiment Leads</h2> <h4 style='display:inline;'>(if different from project management entity)</h4></div>"),
              fluidRow(id = "fr_experimentLeads_boxes"),
              actionButton("addExperimentLeads", "Add an experiment lead", icon("plus-circle"), class = "btn-primary", style="color: #fff;"),
              br(), br(), br()
            ),
            
            sidebarPanel(
              id = "sidebar", width = 12,
              actionButton("btnNextPersonnelInfo", "Next", class = "btn-primary", style="color: #fff;", href = "#top")
            )
          ),
          
          ##### END: TAB EXPERIMENT #####
          
          ##### START: TAB PERSONNEL #####
          
          tabPanel(
            title = tagList(shiny::icon("user"), "Personnel"), value = "tabPersonnel",
            column(
              width = 12,
              h2("Personnel associated with the experiment"),
              fluidRow(
                column(6, style = "margin-top: 25px;", actionButton("btLoadMyInfoPersonnel", "Load my info", icon("user")))
              ),
              br(),
              fluidRow(id = "fr_personnel_boxes"),
              actionButton("addPersonnel", "Add personnel", icon("plus-circle"), class = "btn-primary", style="color: #fff;"),
              br(), br(), br()
            ),
            
            sidebarPanel(
              id="sidebar", width = 12,
              actionButton("btnNextSite", "Next", class = "btn-primary",style="color: #fff;", href="#top")
            )
          ),
          
          ##### END: TAB PERSONNEL #####
          
          ##### START: TAB SITE #####
          
          tabPanel(
            title = tagList(shiny::icon("location-arrow"), "Site"), value = "tabSite",
            column(
              12,
              fluidRow(
                column(
                  6,
                  h2("Site information"),
                  fluidRow(
                    column(6, style = "margin-top: 25px;", actionButton("refreshSiteList", "Refresh all sites", icon("sync")))
                  ),
                  shiny::uiOutput("fbDesign_country", inline = TRUE, width = 500),
                  shiny::uiOutput("fbDesign_countrySite", inline = TRUE, width = 500),
                  br(),
                  h2("Site description"),
                  selectizeInput(
                    "fbDesign_inHighLevel", label = "Higher-level landform", multiple = TRUE,
                    choices = c("Basin",
                                "Hill",
                                "Mountain",
                                "Plain",
                                "Plateau",
                                "Upland",
                                "Valley",
                                "Other"),
                    options = list(maxItems = 1, placeholder = 'Select  one...')
                  ),
                  hidden(textInput(paste0("fbDesign_inHighLevel_other"), "", value = "")),
                  selectizeInput(
                    "fbDesign_inSiteVegetation", label = "Vegetation surrounding the experiment site", multiple = TRUE,
                    choices = c("Crops",
                                "Forest",
                                "Grassland",
                                "Savanna",
                                "Shrubs",
                                "Woodland",
                                "Other"),
                    options = list(maxItems = 5, placeholder = 'Select...')
                  ),
                  fluidRow(id = "othertVEG_aux"),
                  textAreaInput("inSiteDescNotes", label="Site description notes", value=""),
                  br()
                )
              )
            ),
            column(
              12,
              fluidRow(
                column(
                  6,
                  h2("Soil classification"),
                  selectizeInput(
                    "soil_class_system", label = "Soil classification system", multiple = TRUE,
                    choices = c("FAO",
                                "USDA",
                                "Other"),
                    options = list(maxItems = 1, placeholder = 'Select  one...')
                  )
                ),
                column(
                  6, style = "margin-top: 68px;",
                  hidden(textInput(paste0("soil_class_system_other"), "", value = "")),
                  conditionalPanel(
                    "input.soil_class_system == 'FAO'",
                    selectizeInput(
                      "soil_class_fao", label = "", multiple = TRUE,
                      choices = c("Acrisols",
                                  "Andosol",
                                  "Arenosols",
                                  "Cambiosols",
                                  "Chernozems",
                                  "Ferralsols",
                                  "Fluvisols",
                                  "Gleysols",
                                  "Greyzems",
                                  "Gypsisols",
                                  "Histosols",
                                  "Kastanozems",
                                  "Lithosols",
                                  "Luvisols",
                                  "Nitisols",
                                  "Phaeozems"),
                      options = list(maxItems = 1, placeholder = 'Select  one...')
                    )
                  ),
                  conditionalPanel(
                    "input.soil_class_system == 'USDA'",
                    selectizeInput(
                      "soil_class_usda", label = "", multiple = TRUE,
                      choices = c("Alfisols",
                                  "Andisols",
                                  "Aridisols",
                                  "Entisols",
                                  "Gelisols",
                                  "Histosols",
                                  "Inceptisols",
                                  "Mollisols",
                                  "Oxisols",
                                  "Spodosols",
                                  "Ultisols",
                                  "Vertisols"),
                      options = list(maxItems = 1, placeholder = 'Select  one...')
                    )
                  )
                )
              )
            ),
            sidebarPanel(
              id="sidebar", width = 12,
              actionButton("btnNextCropInfo", "Next", class = "btn-primary",style="color: #fff;", href="#top")
            )
          ),
          
          ##### END: TAB SITE #####
          
          ##### START: TAB CROP #####
          
          shiny::tabPanel(
            title = tagList(shiny::icon("pagelines"), "Crop"), value = "tabCrop",
            
            column(
              12,
              fluidRow(
                column(
                  6,
                  h2("Fieldbook details"),
                  uiOutput("fieldbookIdUI")
                ),
                
                column(
                  6, 
                  align = "right",
                  br(),
                  imageOutput("myqr2")
                )
              )
            ),
            column(
              6,
              h2("Description of crops sown"),
              shiny::selectInput(
                "croppingType", "Cropping type",
                choices = c("Monocrop",
                            "Intercrop",
                            "Relay crop",
                            "Rotation"), 
                selected = "Monocrop"
              )
            ),
            column(
              12,
              conditionalPanel(
                "input.croppingType == 'Monocrop'",
                fluidRow(
                  column(
                    12,
                    br(),
                    h2("Crop information"),
                    fluidRow(
                      column(
                        6,
                        selectizeInput(
                          "cropCommonNameMono", "Crop common name", multiple = TRUE,
                          options = list(maxItems = 1, placeholder = "Select one..."),
                          choices = c("Cassava",
                                      "Common bean",
                                      "Maize",
                                      "Potato",
                                      "Rice",
                                      "Sweetpotato",
                                      "Wheat",
                                      "Other")            
                        ),
                        hidden(textInput("cropCommonNameMono_other", ""))
                      ),
                      column(
                        6,
                        selectizeInput(
                          "cultivarNameMono", label = "Variety name(s)", 
                          choices = c(), multiple = T,
                          options = list('create' = TRUE)
                        )
                      )
                    )
                  )
                )               
              ),
              conditionalPanel(
                "input.croppingType == 'Intercrop'",
                h2("Crop information"),
                fluidRow(id="fr_intercrop_boxes"),
                actionButton("addIntercrop", "Add crop"),
                br(),br(),
                h2("Intercrop arrangement"),
                fluidRow(
                  column(
                    6,
                    selectizeInput(
                      "fr_intercrop_arrangement", "", multiple = TRUE,
                      options = list(maxItems = 1, placeholder = "Select one..."),
                      choices = c("Mixed intercropping",
                                  "Row intercropping")             
                    )
                  )
                ),
                fluidRow(
                  column(
                    12,
                    h2("Intercrop row geometry"),
                    fluidRow(id="fr_intercrop_geometry_boxes")
                  )
                )               
              ),
              conditionalPanel(
                "input.croppingType == 'Relay crop'",
                h2("Crop information"),
                fluidRow(id="fr_relaycrop_boxes"),
                actionButton("addRelaycrop", "Add crop"),
                br(),br()
              ),
              
              conditionalPanel(
                "input.croppingType == 'Rotation'",
                h2("Crop information"),
                fluidRow(id="fr_rotationcrop_boxes"),
                actionButton("addRotationcrop", "Add crop"),
                br(),br()
              ),
              br(),
              h2("Previous crop or fallow"),
              fluidRow(
                column(
                  6,
                  selectizeInput(
                    "prevCropName", "", multiple = TRUE,
                    options = list(maxItems = 1, placeholder = "Select one..."),
                    choices = c("Cassava",
                                "Common bean",
                                "Maize",
                                "Potato",
                                "Rice",
                                "Sweetpotato",
                                "Wheat",
                                "Fallow",
                                "Pasture",
                                "Other")
                  ),
                  hidden(textInput("prevCropName_other", ""))
                )
              )
            ),
            
            sidebarPanel(
              id="sidebar", width = 12,
              actionButton("btnDesign", "Next", class = "btn-primary",style="color: #fff;", href="#top")
            )
          ),
          
          ##### END: TAB CROP #####
          
          ##### START: TAB DESIGN #####
          
          shiny::tabPanel(
            title = tagList(shiny::icon("th-list"), "Design"), value = "tabDesign",
            
            column(
              6,
              h2("Experimental design"),
              shiny::selectInput(
                "designFieldbook_agrofims", "", 
                selected = 'FRCBD', multiple = FALSE,
                c("Select one..." = "", design_choices_agrofims)
              ),
              
              #actionButton("boton","action"),
              
              conditionalPanel(
                "input.designFieldbook_agrofims == 'CRD' || 
                input.designFieldbook_agrofims == 'RCBD' || 
                input.designFieldbook_agrofims == 'FCRD' || 
                input.designFieldbook_agrofims == 'FRCBD'",
                h2("Experimental unit"),
                selectizeInput(
                  "info_experiment_unit", "Information on experimental unit", multiple = T,
                  options = list(maxItems =1, placeholder="Select one..."),
                  choices = c("plot",
                              "field",
                              "pot")
                ),
                conditionalPanel(
                  "input.info_experiment_unit == 'plot'",
                  fluidRow(
                    column(3, textInput("expt_plot_length", label="Length", value="")),
                    column(
                      3,
                      selectizeInput(
                        "expt_plot_length_unit", label="Unit", multiple = TRUE,
                        options = list(maxItems =1, placeholder ="Select one..."),
                        choices = c("m", "ft")
                      )
                    ),
                    column(3, textInput("expt_plot_width", label="Width", value="")),
                    column(
                      3,
                      selectizeInput(
                        "expt_plot_width_unit", label="Unit", multiple = TRUE,
                        options = list(maxItems =1, placeholder ="Select one..."),
                        choices = c("m", "ft")
                      )
                    )
                  )
                ),
                conditionalPanel(
                  "input.info_experiment_unit == 'field'",
                  fluidRow(
                    column(3, textInput("expt_field_length", label="Length", value="")),
                    column(
                      3,
                      selectizeInput(
                        "expt_field_length_unit", label="Unit", multiple = TRUE,
                        options = list(maxItems =1, placeholder ="Select one..."),
                        choices = c("m",
                                    "km",
                                    "ft",
                                    "mi")
                      )
                    ),
                    column(3, textInput("expt_field_width", label="Width", value="")),
                    column(
                      3,
                      selectizeInput(
                        "expt_field_width_unit", label="Unit", multiple = TRUE,
                        options = list(maxItems =1, placeholder ="Select one..."),
                        choices = c("m",
                                    "km",
                                    "ft",
                                    "mi")
                      )
                    )
                  )
                ),
                conditionalPanel(
                  "input.info_experiment_unit == 'pot'",
                  fluidRow(
                    column(3, textInput("pot_diameter", label="Diameter", value="")),
                    column(
                      3,
                      selectizeInput(
                        "pot_diameter_unit", label="Unit", multiple = TRUE,
                        options = list(maxItems =1, placeholder ="Select one..."),
                        choices = c("cm", "in")
                      )
                    ),
                    column(3, textInput("pot_depth", label="Depth", value="")),
                    column(
                      3,
                      selectizeInput(
                        "pot_depth_unit", label="Unit", multiple = TRUE,
                        options = list(maxItems =1, placeholder ="Select one..."),
                        choices = c("cm", "in")
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                "input.designFieldbook_agrofims == 'SPRCBD'",
                h2("Information on experimental unit"),
                fluidRow(
                  column(
                    12, 
                    h4("Information on main plot")
                  )
                ),
                fluidRow(
                  column(3, textInput("sprcbd_main_expt_plot_length", label="Length", value="")),
                  column(
                    3,
                    selectizeInput(
                      "sprcbd_main_expt_plot_length_unit", label="Unit", multiple = TRUE,
                      options = list(maxItems =1, placeholder ="Select one..."),
                      choices = c("m", "ft")
                    )
                  ),
                  column(3, textInput("sprcbd_main_expt_plot_width", label="Width", value="")),
                  column(
                    3,
                    selectizeInput(
                      "sprcbd_main_expt_plot_width_unit", label="Unit", multiple = TRUE,
                      options = list(maxItems =1, placeholder ="Select one..."),
                      choices = c("m", "ft")
                    )
                  )
                ),
                fluidRow(
                  column(
                    12, 
                    h4("Information on sub plot")
                  )
                ),
                fluidRow(
                  column(3, textInput("sprcbd_sub_expt_plot_length", label="Length", value="")),
                  column(
                    3,
                    selectizeInput(
                      "sprcbd_sub_expt_plot_length_unit", label="Unit", multiple = TRUE,
                      options = list(maxItems =1, placeholder ="Select one..."),
                      choices = c("m", "ft")
                    )
                  ),
                  column(3, textInput("sprcbd_sub_expt_plot_width", label="Width", value="")),
                  column(
                    3,
                    selectizeInput(
                      "sprcbd_sub_expt_plot_width_unit", label="Unit", multiple = TRUE,
                      options = list(maxItems =1, placeholder ="Select one..."),
                      choices = c("m", "ft")
                    )
                  )
                )
              ),
              conditionalPanel(
                "input.designFieldbook_agrofims == 'SPSP'",
                h2("Information on experimental unit"),
                fluidRow(
                  column(
                    12, 
                    h4("Information on main plot")
                  )
                ),
                fluidRow(
                  column(3, textInput("spsp_main_expt_plot_length", label="Length", value="")),
                  column(
                    3,
                    selectizeInput(
                      "spsp_main_expt_plot_length_unit", label="Unit", multiple = TRUE,
                      options = list(maxItems =1, placeholder ="Select one..."),
                      choices = c("m", "ft")
                    )
                  ),
                  column(3, textInput("spsp_main_expt_plot_width", label="Width", value="")),
                  column(
                    3,
                    selectizeInput(
                      "spsp_main_expt_plot_width_unit", label="Unit", multiple = TRUE,
                      options = list(maxItems =1, placeholder ="Select one..."),
                      choices = c("m", "ft")
                    )
                  )
                ),
                fluidRow(
                  column(
                    12, 
                    h4("Information on sub plot")
                  )
                ),
                fluidRow(
                  column(3, textInput("spsp_sub_expt_plot_length", label="Length", value="")),
                  column(
                    3,
                    selectizeInput(
                      "spsp_sub_expt_plot_length_unit", label="Unit", multiple = TRUE,
                      options = list(maxItems =1, placeholder ="Select one..."),
                      choices = c("m", "ft")
                    )
                  ),
                  column(3, textInput("spsp_sub_expt_plot_width", label="Width", value="")),
                  column(
                    3,
                    selectizeInput(
                      "spsp_sub_expt_plot_width_unit", label="Unit", multiple = TRUE,
                      options = list(maxItems =1, placeholder ="Select one..."),
                      choices = c("m", "ft")
                    )
                  )
                ),
                fluidRow(
                  column(
                    12, 
                    h4("Information on sub-sub plot")
                  )
                ),
                fluidRow(
                  column(3, textInput("spsp_subsub_expt_plot_length", label="Length", value="")),
                  column(
                    3,
                    selectizeInput(
                      "spsp_subsub_expt_plot_length_unit", label="Unit", multiple = TRUE,
                      options = list(maxItems =1, placeholder ="Select one..."),
                      choices = c("m", "ft")
                    )
                  ),
                  column(3, textInput("spsp_subsub_expt_plot_width", label="Width", value="")),
                  column(
                    3,
                    selectizeInput(
                      "spsp_subsub_expt_plot_width_unit", label="Unit", multiple = TRUE,
                      options = list(maxItems =1, placeholder ="Select one..."),
                      choices = c("m", "ft")
                    )
                  )
                )
              )
            ),
            column(
              12,
              br(),
              h2("Treatment description"),
              conditionalPanel(
                "input.designFieldbook_agrofims == 'CRD'",
                #h1("CRD"),
                fluidRow(column(
                  6,
                  selectInput(
                    "crd_ntrt",
                    "Number of treatments",
                    choices = 2:100,
                    selected = 2
                  )
                ),
                column(
                  6,
                  selectInput(
                    "crd_rep",
                    "Repetitions",
                    choices = 2:100,
                    selected = 2
                  )
                )),
                fluidRow(id = "crd_boxes"),
                actionButton("crd_add", "Add factor",icon("plus-circle"), class = "btn-primary", style="color: #fff;"),
                br(),
                br(),
                column(
                  id = "col_NFF_consolid_crd",
                  width = 4,
                  HTML("<center>"),
                  "Treatment",
                  HTML("</center>"),
                  fluidRow(id = "fr_col_NFF_cons_crd")
                ),
                fluidRow(id = "not_full_factor_table_crd")
              ),
              conditionalPanel(
                "input.designFieldbook_agrofims == 'RCBD'",
                #h1("RCBD"),
                fluidRow(column(
                  6,
                  selectInput(
                    "rcbd_ntrt",
                    "Number of treatments",
                    choices = 2:100,
                    selected = 2
                  )
                ),
                column(
                  6,
                  selectInput(
                    "rcbd_rep",
                    "Block ",
                    choices = 2:100,
                    selected = 2
                  )
                )),
                fluidRow(id = "rcbd_boxes"),
                actionButton("rcbd_add", "Add factor", icon("plus-circle"), class = "btn-primary", style="color: #fff;"),
                br(),
                br(),
                column(
                  id = "col_NFF_consolid_rcbd",
                  width = 4,
                  HTML("<center>"),
                  "Treatment",
                  HTML("</center>"),
                  fluidRow(id = "fr_col_NFF_cons_rcbd")
                ),
                fluidRow(id = "not_full_factor_table_rcbd")
              ),
              conditionalPanel(
                "input.designFieldbook_agrofims == 'FCRD'",
                #h1("FCRD"),
                fluidRow(column(
                  6,
                  selectInput(
                    "fcrd_rep",
                    "Repetitions",
                    choices = 2:100,
                    selected = 2
                  )
                )),
                fluidRow(id = "fcrd_boxes"),
                actionButton("fcrd_add", "Add factor", icon("plus-circle"), class = "btn-primary", style="color: #fff;")
              ),
              conditionalPanel(
                "input.designFieldbook_agrofims == 'FRCBD'",
                #h1("FRCBD"),
                fluidRow(column(
                  6,
                  selectInput(
                    "frcbd_block",
                    "Block",
                    choices = 2:100,
                    selected = 2
                  )
                )),
                fluidRow(id = "frcbd_boxes"),
                actionButton("frcbd_add", "Add factor", icon("plus-circle"), class = "btn-primary", style="color: #fff;")
              ),
              conditionalPanel(
                "input.designFieldbook_agrofims == 'SPRCBD'",
                #h1("SPRCBD"),
                fluidRow(column(
                  6,
                  selectInput(
                    "sp1_block",
                    "Block",
                    choices = 2:100,
                    selected = 2
                  )
                )),
                fluidRow(id = "sprcbd_boxes"),
                fluidRow(id = "sprcbd_boxes")#,
                #actionButton("sprcbd_add", "Add factor")
              ),
              conditionalPanel(
                "input.designFieldbook_agrofims == 'SPSP'",
                #h1("SPSP"),
                fluidRow(column(
                  6,
                  selectInput(
                    "spsp2_block",
                    "Block",
                    choices = 2:100,
                    selected = 2
                  )
                )),
                fluidRow(id = "spsp_boxes")#,
                #actionButton("spsp_add", "Add factor")
              ),
              conditionalPanel(
                "input.designFieldbook_agrofims == 'STRIP'",
                #selectInput("strip_block","Block", choices = 2:10000,selected = 2),
                h1("STRIP"),
                h2("Under construction")
              )
            ),
            sidebarPanel(
              id="sidebar", width = 12,
              actionButton("btnDesign", "Next", class = "btn-primary",style="color: #fff;", href="#top")
            )
          ),
          
          ##### END: TAB DESIGN #####
          
          ##### START: TAB MANAGEMENT PRACTICES #####
          
          shiny::tabPanel(
            title = tagList(shiny::icon("truck"), "Management practices"), value = "tabAgroFeat",
            
            fluidRow(
              column(
                12,
                h2("Management practices"),
                selectizeInput(
                  "selectAgroFeature", "", multiple = TRUE, 
                  choices = c("Residue management",
                              "Seedbed preparation",
                              "Soil fertility",
                              "Planting and transplanting",
                              "Mulch management",
                              "Irrigation",
                              "Weeding",
                              "Harvest"),
                  options = list(maxItems = 8, placeholder = "Select some...")
                )
              )
            ),
            fluidRow(
              column(
                12,
                tabsetPanel(
                  id = "nutrienTabPanels",
                  tabPanel(
                    div(id = "gh", "Residue management"), value = "tabResidue",
                    column(
                      12,
                      br(),
                      fluidRow(
                        box(
                          id = "residue_description_boxid",
                          title = checkboxInput("residueDesc_checkbox", actionLink("residue_description_titleId", "Residue description"), T),
                          status = "primary", solidHeader = TRUE,
                          width = 12, collapsible = TRUE, collapsed = TRUE,
                          fluidRow(
                            column(
                              12,
                              selectizeInput(
                                "residue_description_to_collect_field", label = "To collect in the field", multiple = TRUE, 
                                options = list(maxItems = 10, placeholder = "Select one..."), 
                                choices = magm_label$get("resdes")
                                # choices = c("Plant part",
                                #             "Crop residue moisture",
                                #             "Crop residue thickness",
                                #             "Crop residue amount",
                                #             "Crop residue percent of coverage")
                              ),
                              hr()
                            )
                          ),
                          fluidRow(
                            column(
                              6,
                              selectizeInput(
                                "rmgt_residue_plantPart", label = "Plant part", multiple = TRUE, 
                                options = list(maxItems = 1, placeholder = "Select one..."), 
                                choices = c("Husk",
                                            "Leaf",
                                            "Root",
                                            "Seed Pod/Cob/Fruit",
                                            "Stem",
                                            "Stubble",
                                            "Other")
                              ),
                              hidden(textInput("rmgt_residue_plantPart_other", "", value = "")),
                              selectizeInput(
                                "rmgt_crop_residue_moisture", "Crop residue moisture", multiple = T, 
                                options = list(maxItems=1, placeholder = "Select one..."),
                                choices = c("Dry", "Moist", "Wet")
                              ),
                              fluidRow(
                                column(6, numericInput("rmgt_crop_residue_thick", value = "", label = "Crop residue thickness", min = 0, max = 100, step = 0.1)),
                                column(
                                  6,
                                  selectizeInput(
                                    "rmgt_crop_residue_thick_unit", "Unit", multiple = T, 
                                    options=list(maxItems=1, placeholder="Select one..."),
                                    choices = c("cm", "ft", "in", "m"), selected = "cm"
                                  )
                                )
                              ),
                              fluidRow(
                                column(6, numericInput("rmgt_crop_residue_amount_sqm", value = "", label = "Crop residue amount", min = 0, max = 100, step = 0.1)),
                                column(
                                  6,
                                  selectizeInput(
                                    "rmgt_crop_residue_amount_sqm_unit", "Unit", multiple = T, 
                                    options=list(maxItems = 1, placeholder = "Select one..."),
                                    choices = c("g/ft2", "g/m2", "kg/ha", "kg/m2", "lb/ac"), selected = "kg/ha"
                                  )
                                )
                              ),
                              fluidRow(
                                column(6, numericInput("rmgt_crop_residue_perc_cov", "Crop residue percent of coverage", value = "", min = 0, max = 100)),
                                column(6, selectInput("rmgt_crop_residue_perc_cov_unit", "Unit", c("%"), selected = "%"))
                              ),
                              textAreaInput("rmgt_residue_description_notes", label = "Notes", value = "")
                            )
                          )
                        ), 
                        box(
                          id="residue_management_boxid",
                          title = checkboxInput("residueManag_checkbox", actionLink("residue_management_titleId", "Residue management"), T),
                          status = "primary", solidHeader = TRUE,
                          width = 12, collapsible = TRUE,  collapsed = TRUE,
                          fluidRow(
                            column(
                              12,
                              selectizeInput(
                                "residue_management_to_collect_field", label = "To collect in the field", multiple = TRUE, 
                                options = list(maxItems = 10, placeholder = "Select one..."), 
                                choices = magm_label$get("resmgtm")
                                # choices = c("Start date",
                                #             "Technique",
                                #             "Traction")
                              ),
                              hr()
                            )
                          ),
                          fluidRow(
                            column(
                              6,
                              fluidRow(
                                column(
                                  6,
                                  uiOutput("res_start_date")
                                )
                              ),
                              selectizeInput(
                                "rmgt_residue_technique", label = "Technique", multiple = TRUE, 
                                options = list(maxItems =1, placeholder ="Select one..."), 
                                choices = c("Burning",
                                            "Incorporation",
                                            "Spreading",
                                            "Other")
                              ),
                              hidden(textInput("rmgt_residue_technique_other", "", value = "")),
                              conditionalPanel(
                                "input.rmgt_residue_technique == 'Incorporation'", 
                                fluidRow(
                                  column(6, textInput("rmgt_residue_inc_depth", "Residue incorporation depth", value = "")),
                                  column(
                                    6, 
                                    selectizeInput(
                                      "rmgt_residue_inc_depth_unit", "Unit", multiple = T, 
                                      options=list(maxItems=1, placeholder="Select one..."),
                                      choices = c("cm", "ft", "in", "m"), selected = "cm"
                                    )
                                  )
                                )
                              ),
                              selectizeInput(
                                "rmgt_residue_traction", label = "Traction", multiple = TRUE, 
                                options = list(maxItems =1, placeholder ="Select one..."), 
                                choices = c("Animal",
                                            "Manual",
                                            "2 wheel tractor",
                                            "4 wheel tractor",
                                            "Other")
                              ),
                              hidden(textInput("rmgt_residue_traction_other", "", value = "")),
                              textAreaInput("rmgt_residue_management_notes", label = "Notes", value = "")
                            )
                          )
                        )
                      )
                    )
                  ),
                  tabPanel(
                    div(id = "gh", "Seedbed preparation"), value = "tabSeedbed",
                    column(
                      12,
                      br(),
                      fluidRow(
                        box(
                          id = "land_levelling_boxid",
                          title = checkboxInput("landLevelling_checkbox" , actionLink("land_levelling_titleId", "Land Levelling"), F),
                          status = "primary", solidHeader = TRUE,
                          width = 12, collapsible = TRUE,  collapsed = TRUE,
                          fluidRow(
                            column(
                              12,
                              selectizeInput(
                                "land_levelling_to_collect_field", label = "To collect in the field", multiple = TRUE, 
                                options = list(maxItems = 10, placeholder = "Select one..."), 
                                choices = magm_label$get("landlev")
                                # choices = c("Start date",
                                #             "Total number of levelling passes",
                                #             "Type",
                                #             "Traction")
                              ),
                              hr()
                            )
                          ),
                          fluidRow(
                            column(
                              6,
                              fluidRow(
                                column(
                                  6,
                                  uiOutput("landLev_start_date")
                                )
                              ),
                              numericInput("numPasses", label = "Total number of levelling passes", value="", min = 1, max = 20, step = 1),
                              textAreaInput("landLeveling_notes", label="Notes", value="")
                            ),
                            column(
                              6,
                              br(),
                              fluidRow(
                                box(
                                  title = "Implement", solidHeader = TRUE, status = "warning", width = 12,
                                  fluidRow(
                                    column(12, h4("Implement", style="font-weight: 800;color: #555;"))
                                  ),
                                  selectizeInput(
                                    "land_impl_type", label = "Type", multiple = TRUE, 
                                    options = list(maxItems =1, placeholder = "Select one..."),
                                    choices = c("Chain harrow",
                                                "Disc harrow",
                                                "Drag bucket",
                                                "Laser-controlled",
                                                "Levelling board",
                                                "Levelling bucket",
                                                "Roller", 
                                                "Tine harrow",
                                                "Other")
                                  ),
                                  hidden(textInput("land_impl_type_other", "")),
                                  selectizeInput(
                                    "land_traction", multiple = TRUE, options = list(maxItems = 1, placeholder ="Select one..."), 
                                    label = "Traction", choices = c("Animal",
                                                                    "Manual",
                                                                    "2 wheel tractor",
                                                                    "4 wheel tractor",
                                                                    "Other")
                                  ),
                                  hidden(textInput("land_traction_other", "", value = ""))
                                )
                              )
                            )
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          id = "puddling_boxid",
                          title = checkboxInput("puddling_checkbox", actionLink("puddling_titleId", "Puddling"), F),
                          solidHeader = TRUE, status = "primary",
                          width = 12, collapsible = TRUE,  collapsed = TRUE,
                          fluidRow(
                            column(
                              12,
                              selectizeInput(
                                "puddling_to_collect_field", label = "To collect in the field", multiple = TRUE, 
                                options = list(maxItems = 10, placeholder = "Select one..."), 
                                choices = magm_label$get("pud")
                                # choices = c("Start date",
                                #             "Puddling depth",
                                #             "Total number of puddling passes",
                                #             "Type",
                                #             "Traction")
                              ),
                              hr()
                            )
                          ),
                          fluidRow(
                            column(
                              6,
                              fluidRow(
                                column(
                                  6,
                                  uiOutput("pud_start_date")
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              6,
                              fluidRow(
                                column(6, numericInput("puddling_depth_val", label = "Puddling depth", value="", min = 1, step = 0.1)),
                                column(
                                  6,
                                  selectizeInput(
                                    "puddling_depth_unit", label="Unit", multiple = TRUE, 
                                    options = list(maxItems = 1, placeholder ="Select one..."), 
                                    choices = c("cm", "ft", "in", "m"), selected = "cm"
                                  )
                                )
                              ),
                              numericInput("puddling_total_number_puddling_passes", "Total number of puddling passes", value = "", min = 0, step = 1),
                              textAreaInput("puddling_notes", label="Notes", value="")
                            ),
                            column(
                              6,
                              fluidRow(
                                box(
                                  title = "Implement", solidHeader = TRUE, status = "warning", width = 12,
                                  fluidRow(
                                    column(12, h4("Implement", style="font-weight: 800;color: #555;"))                  
                                  ),
                                  selectizeInput(
                                    "pud_impl_type", label = "Type", multiple = TRUE, 
                                    options = list(maxItems =1, placeholder ="Select one..."),
                                    choices = c("Chisel plough",
                                                "Cultivator",
                                                "Disc plough",
                                                "Hand-held hoe",
                                                "Mouldboard / Ridging plough",
                                                "Paraplough",
                                                "Spade plough",
                                                "Subsoiler",
                                                "Other")
                                  ),
                                  hidden(textInput("pud_impl_type_other", "")),
                                  selectizeInput(
                                    "pud_traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                                    label = "Traction", choices = c("Animal",
                                                                    "Manual",
                                                                    "2 wheel tractor",
                                                                    "4 wheel tractor",
                                                                    "Other")
                                  ),
                                  hidden(textInput("pud_traction_other", "", value = ""))
                                )
                              )
                            )
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          id="tillage_boxid",
                          title = checkboxInput("tillage_checkbox", actionLink("tillage_titleId", "Tillage"), F),
                          status = "primary", solidHeader = TRUE,
                          width = 12, collapsible = TRUE,  collapsed = TRUE,
                          fluidRow(
                            column(
                              12,
                              selectizeInput(
                                "tillage_to_collect_field", label = "To collect in the field", multiple = TRUE, 
                                options = list(maxItems = 10, placeholder = "Select one..."), 
                                choices = magm_label$get("till")
                                # choices = c("Start date",
                                #             "Technique",
                                #             "Tillage depth",
                                #             "Total number of tillage passes",
                                #             "Type",
                                #             "Traction")
                              ),
                              hr()
                            )
                          ),
                          fluidRow(
                            column(
                              width = 6,
                              fluidRow(
                                column(6, uiOutput("till_start_date"))
                              ),
                              selectizeInput(
                                "till_technique",  label = "Technique", multiple = TRUE, 
                                options = list(maxItems =1, placeholder ="Select one..."), 
                                choices = c("Conventional till",
                                            "Deep till",
                                            "No-till",
                                            "Reduced till",
                                            "Ridge-till",
                                            "Strip-till",
                                            "Other")
                              ),
                              hidden(textInput("till_technique_other", "")),
                              fluidRow(
                                column(6, numericInput("tillage_depth", "Tillage depth", value = "", min = 0, step = 0.1)),
                                column(
                                  6,
                                  selectizeInput(
                                    "tillage_depth_unit", label="Unit", multiple = TRUE, 
                                    options = list(maxItems =1, placeholder ="Select one..."), 
                                    choices=c("cm", "ft", "in", "m"), selected = "cm"
                                  )
                                )
                              ),
                              numericInput("total_number_tillage_passes", "Total number of tillage passes", value = "", min = 0, step = 1),
                              textAreaInput("tillage_notes", label="Notes", value="")
                            ),
                            column(
                              6,
                              br(),
                              fluidRow(
                                box(
                                  title = "Implement", solidHeader = TRUE, status = "warning", width = 12,
                                  fluidRow(
                                    column(12, h4("Implement", style="font-weight: 800;color: #555;"))
                                  ),
                                  selectizeInput(
                                    "till_impl_type", label = "Type", multiple = TRUE, 
                                    options = list(maxItems = 1, placeholder ="Select one..."), 
                                    choices = c("Chisel plough",
                                                "Cultivator",
                                                "Disc plough",
                                                "Hand-held hoe",
                                                "Mouldboard / Ridging plough",
                                                "Paraplough",
                                                "Spade plough",
                                                "Subsoiler",
                                                "Other")
                                  ),
                                  hidden(textInput("till_impl_type_other", "", value = "")),
                                  selectizeInput(
                                    "till_traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), 
                                    label = "Traction", choices = c("Animal",
                                                                    "Manual",
                                                                    "2 wheel tractor",
                                                                    "4 wheel tractor",
                                                                    "Other")
                                  ),
                                  hidden(textInput("till_traction_other", "", value = ""))
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  tabPanel(
                    div(id = "gh", "Soil fertility"), value = "tabSoil",
                    column(
                      12,
                      h1("Under construction")
                    )
                  ),
                  tabPanel(
                    div(id = "gh", "Planting and transplanting"), value="tabPlanting",
                    column(
                      12,
                      br(),
                      conditionalPanel(
                        "input.croppingType == 'Monocrop'",
                        fluidRow(id = "monocrop_fr_plantingTransplating")#,
                        #actionButton("monocrop_pt_add", "Add Planting & Transplanting")
                      ),
                      conditionalPanel(
                        "input.croppingType == 'Intercrop'",
                        tabsetPanel(
                          id = "tabpanelPTint",
                          shiny::tabPanel(title = "default int", value = "pt_int_default")
                        )
                      ),
                      conditionalPanel(
                        "input.croppingType == 'Relay crop'",
                        tabsetPanel(
                          id = "tabpanelPTrel",
                          shiny::tabPanel(title = "default rel", value = "pt_rel_default")
                        )
                      ),
                      conditionalPanel(
                        "input.croppingType == 'Rotation'",
                        tabsetPanel(
                          id = "tabpanelPTrot",
                          shiny::tabPanel(title = "default rot", value = "pt_rot_default")
                        )
                      )
                    )
                  ),
                  tabPanel(
                    div(id = "gh", "Mulch management"), value="tabMulching",
                    column(
                      12,
                      br(),
                      fluidRow(
                        box(
                          id = "mulch_management_boxid",
                          title= "Mulching details",
                          status = "primary", solidHeader = TRUE,
                          width = 12, collapsible = TRUE, collapsed = FALSE,
                          fluidRow(
                            column(
                              12,
                              selectizeInput(
                                "mulch_management_to_collect_field", label = "To collect in the field", multiple = TRUE, 
                                options = list(maxItems = 20, placeholder = "Select one..."), 
                                choices = magm_label$get("mulching")
                                # choices = c("Mulching start date",
                                #             "Type",
                                #             "Mulch thickness",
                                #             "Mulch amount",
                                #             "Mulch color",
                                #             "Percentage of coverage",
                                #             "Mulch removal start date",
                                #             "Mulch removal end date",
                                #             "Type",
                                #             "Traction")
                              ),
                              hr()
                            )
                          ),
                          fluidRow(
                            column(
                              6,
                              fluidRow(
                                column(
                                  6,
                                  uiOutput("mul_start_date")
                                )
                              ),
                              selectizeInput(
                                "mumd_mulch_type", label = "Type", multiple = TRUE, 
                                options = list(maxItems =1, placeholder ="Select one..."),
                                choices = c("Bark / Wood chips",
                                            "Compost",
                                            "Foil (landscape fabric)",
                                            "Grass / Straw",
                                            "Gravel",
                                            "Hush / Chaff",
                                            "Leaves",
                                            "Plastic",
                                            "Saw dust",
                                            "Other")
                                
                              ),
                              hidden(textInput("mumd_mulch_type_other", "", value = "")),
                              fluidRow(
                                column(6, numericInput("mumd_mulch_thickness", value="", label = "Mulch thickness", min=0, max=100, step=0.1)),
                                column(
                                  6,
                                  selectizeInput(
                                    "mumd_mulch_thickness_unit","Unit", multiple = T, 
                                    options=list(maxItems=1, placeholder="Select one..."),
                                    choices = c("cm","ft", "in", "m"), 
                                    selected = "cm"
                                  )
                                )
                              ),
                              fluidRow(
                                column(6, numericInput("mumd_mulch_amountPerSq", value="", label = "Mulch amount", min=0, max=100, step = 0.1)),
                                column(
                                  6,
                                  selectizeInput(
                                    "mumd_mulch_amountPerSq_unit", "Unit", multiple = T, 
                                    options=list(maxItems=1, placeholder="Select one..."),
                                    choices = c("g/ft2", "g/m2","kg/ha", "kg/m2", "lb/ac"), 
                                    selected = "kg/ha"
                                  )
                                )
                              ),
                              textInput("mumd_mulch_color","Mulch color"),
                              fluidRow(
                                column(6, textInput("mumd_mulch_percCoverage", value="", label = "Percentage of coverage")),
                                column(
                                  6,
                                  selectInput(
                                    "mumd_mulch_percCoverage_unit", "Unit", c("%"), 
                                    selected= "%"
                                  )
                                )
                              ),
                              fluidRow(
                                column(6, uiOutput("mulre_start_date")),
                                column(6, uiOutput("mulre_end_date"))
                              ),
                              textAreaInput("mumd_mulching_management_notes", label="Notes", value="")
                            ),
                            column(
                              6,
                              br(),
                              fluidRow(
                                box(
                                  title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                  fluidRow(
                                    column(12, h4("Implement", style="font-weight: 800;color: #555;"))
                                  ),
                                  selectizeInput(
                                    "mumd_mulch_implement_type", label = "Type", multiple = TRUE, 
                                    options = list(maxItems =1, placeholder ="Select one..."), 
                                    choices = c("Manual",
                                                "Mechanized")
                                  ),
                                  selectizeInput(
                                    "mumd_mulch_traction", label = "Traction", multiple = TRUE, 
                                    options = list(maxItems =1, placeholder ="Select one..."), 
                                    choices = c("Animal",
                                                "Manual",
                                                "2 wheel tractor",
                                                "4 wheel tractor",
                                                "Other")
                                  ),
                                  hidden(textInput("mumd_mulch_traction_other", "", value = ""))
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  tabPanel(
                    div(id = "gh", "Irrigation"), value = "tabIrrigation",
                    column(
                      12,
                      br(),
                      fluidRow(
                        column(
                          12,
                          selectizeInput(
                            "irrigation_to_collect_field", label = "To collect in the field", multiple = TRUE, 
                            options = list(maxItems = 20, placeholder = "Select one..."), 
                            choices = magm_label$get("irrigation")
                            # choices = c("Start date",
                            #             "End date",
                            #             "Irrigation technique",
                            #             "Irrigation source",
                            #             "Irrigation source distance",
                            #             "Irrigation amount")
                          )#,
                          #hr()
                        )
                      ),
                      fluidRow(id = "monocrop_fr_irrigation"),
                      actionButton("monocrop_irri_add", "Add irrigation")
                    )
                  ),
                  tabPanel(
                    div(id = "gh", "Weeding"), value = "tabWeeding",
                    column(
                      12,
                      br(),
                      fluidRow(
                        column(
                          12,
                          selectizeInput(
                            "weeding_to_collect_field", label = "To collect in the field", multiple = TRUE, 
                            options = list(maxItems = 20, placeholder = "Select one..."), 
                            choices = magm_label$get("weeding")
                            # choices = c("Start date",
                            #             "Technique",
                            #             "Type",
                            #             "Traction")
                          )#,
                          #hr()
                        )
                      ),
                      fluidRow(id = "monocrop_fr_weeding"),
                      actionButton("monocrop_wee_add", "Add weeding")
                    )
                  ),
                  tabPanel(
                    div(id = "gh", "Harvest"), value = "tabHarvest",
                    column(
                      12,
                      br(),
                      conditionalPanel(
                        "input.croppingType == 'Monocrop'",
                        fluidRow(
                          column(
                            12,
                            selectizeInput(
                              paste0("mono_harvest_to_collect_field"), label = "To collect in the field", multiple = TRUE, 
                              options = list(maxItems = 20, placeholder = "Select one..."), 
                              choices = c("Start date",
                                          "End date",
                                          "Harvest Method",
                                          "Crop component harvested",
                                          "Harvestable area",
                                          "Amount harvested",
                                          "Harvest cut height",
                                          "Type",
                                          "Traction")
                            )#,
                            #hr()
                          )
                        ),
                        fluidRow(id = "monocrop_fr_harvest"),
                        actionButton("monocrop_harv_add", "Add harvest")
                      ),
                      conditionalPanel(
                        "input.croppingType == 'Intercrop'",
                        tabsetPanel(
                          id = "tabpanelHARVint",
                          shiny::tabPanel(title = "default int", value = "harv_int_default")
                        )
                      ),
                      conditionalPanel(
                        "input.croppingType == 'Relay crop'",
                        tabsetPanel(
                          id = "tabpanelHARVrel",
                          shiny::tabPanel(title = "default rel", value = "harv_rel_default")
                        )
                      ),
                      conditionalPanel(
                        "input.croppingType == 'Rotation'",
                        tabsetPanel(
                          id = "tabpanelHARVrot",
                          shiny::tabPanel(title = "default rot", value = "harv_rot_default")
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          
          ##### END: TAB MANAGEMENT PRACTICES #####
          
          ##### START: TAB MEASUREMENT #####
          
          shiny::tabPanel(
            "Crop Measurement", value = "tabCropMeasurement", icon = shiny::icon("leaf"),
            conditionalPanel(
              "input.croppingType == 'Monocrop'",
              uiOutput("uiCropMeaMono")
            ),
            conditionalPanel(
              "input.croppingType == 'Intercrop'",
              column(
                width = 12,
                h2("Crop measurement"),
                p(class = "text-muted", style="text-align:justify",
                  paste("Please, select measurement by click.")
                ),
                tabsetPanel(
                  id = "tabpanelMEAint",
                  tabPanel(title = "default int", value = "mea_int_default")
                )
              )
            ),
            conditionalPanel(
              "input.croppingType == 'Relay crop'",
              column(
                width = 12,
                h2("Crop measurement"),
                p(
                  class = "text-muted",
                  style = "text-align:justify",
                  paste("Please, select measurement by click.")
                ),
                tabsetPanel(
                  id = "tabpanelMEArel",
                  tabPanel(title = "default rel", value = "mea_rel_default")
                )
              )
            ),
            conditionalPanel(
              "input.croppingType == 'Rotation'",
              column(
                width = 12,
                h2("Crop measurement"),
                p(
                  class = "text-muted",
                  style = "text-align:justify",
                  paste("Please, select measurement by click.")
                ),
                tabsetPanel(
                  id = "tabpanelMEArot",
                  tabPanel(title = "default rot", value = "mea_rot_default")
                )
              )
            )
          ),
          
          ##### END: TAB MEASUREMENT #####
          
          ##### START: TAB PHENOLOGY #####
          
          shiny::tabPanel(
            "Crop Phenology", value = "tabCropPhenology", icon = shiny::icon("leaf"),
            conditionalPanel(
              "input.croppingType == 'Monocrop'",
              uiOutput("uiCropPheMono")
            ),
            conditionalPanel(
              "input.croppingType == 'Intercrop'",
              column(
                width = 12,
                h2("Crop phenology"),
                p(
                  class = "text-muted",
                  style = "text-align:justify",
                  paste("Please, select phenology by click.")
                ),
                tabsetPanel(
                  id = "tabpanelPHEint",
                  tabPanel(title = "default int", value = "phe_int_default")
                )
              )
            ),
            conditionalPanel(
              "input.croppingType == 'Relay crop'",
              column(
                width = 12,
                h2("Crop phenology"),
                p(
                  class = "text-muted",
                  style = "text-align:justify",
                  paste("Please, select phenology by click.")
                ),
                tabsetPanel(
                  id = "tabpanelPHErel",
                  tabPanel(title = "default rel", value = "phe_rel_default")
                )
              )
            ),
            conditionalPanel(
              "input.croppingType == 'Rotation'",
              column(
                width = 12,
                h2("Crop phenology"),
                p(
                  class = "text-muted",
                  style = "text-align:justify",
                  paste("Please, select phenology by click.")
                ),
                tabsetPanel(
                  id = "tabpanelPHErot",
                  tabPanel(title = "default rot", value = "phe_rot_default")
                )
              )
            )
            
          ),
          
          ##### END: TAB PHENOLOGY #####
          
          ##### START: TAB WEATHER #####
          
          shiny::tabPanel(
            title = tagList(shiny::icon("bolt"), "Weather"), value = "tabWeather",
            uiOutput("uiWeather")
          ),
          
          ##### END: TAB WEATHER #####
          
          ##### START: TAB SOIL #####
          
          shiny::tabPanel(
            title = tagList(shiny::icon("dice-four"), "Soil"), value = "tabSoil",
            uiOutput("uisoil")
          )
          
          ##### END: TAB SOIL #####
          
          # shiny::tabPanel("Crop Measurement", value = 'crop_measurement_mono', icon = shiny::icon("leaf"),
          # shiny::tabPanel("Crop Phenology", value = 'crop_phenology_mono', icon = shiny::icon("leaf"),
          # shiny::tabPanel("Weather", value = 'tabWeather2', icon = shiny::icon("bolt"),
          # shiny::tabPanel("Soil", value = 'tabSoil2', icon = shiny::icon("dice-four"),
          # shiny::tabPanel("Soil", value = 'tabSoil', icon = shiny::icon("bolt"),
          
          )
        )
      ),
    
    fluidRow(
      HTML('<div style="float: right; margin: 0 15px 18px 0px;">'),
      #shiny::actionButton(inputId = "refresh", label = "Refresh", icon = icon("fa fa-refresh")),
      #shinyBS::bsButton( "fbDesign_draft", "BookView" ),
      shiny::actionButton("fbDesign_draft_agrofims", "Book Preview", icon("table"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      #downloadButton("sendKDSmart", "Send to KDSmart", icon("send"), style="color: #000; background-color: #c5c5c5; border-color: #000"),
      #actionButton("sendKDSmart", "Send to KDSmart"),
      downloadButton("downloadData", "Download", class = "color: #fff; background-color: #51a351; border-color: #51a351"),
      
      #shinysky::actionButton2("fbDesign_create_agrofims", label = "Download", icon ="file-excel-o", icon.library = "bootstrap", styleclass= "color: #fff; background-color: #51a351; border-color: #51a351"),
      #shiny::actionButton("fbDesign_create", "Download", icon("file-excel-o"), style="color: #fff; background-color: #51a351; border-color: #51a351"),
      #shinyBS::bsAlert(anchorId = "alert_fb_done"),
      shinysky::shinyalert("alert_fb_done", FALSE, auto.close.after = 4),
      HTML('</div>')
    ),
    
    conditionalPanel(
      condition = "output.show_agrotable",
      shiny::fluidRow(
        shinydashboard::box(title = "Fieldbook Preview",
                            status = "primary",
                            #height = 500,
                            #width = NULL,
                            solidHeader = TRUE,
                            width = 12, collapsible = TRUE,
                            #shiny::actionButton("butNewFieldbook", "New fieldbook", inline = TRUE)#,
                            rhandsontable::rHandsontableOutput("fbDesign_table_agrofims", height = 400)
                            # )#,
                            #)
        ),
        br(),
        br(),
        br()
      )
    ),
    br(),
    br(),
    br()
    
    ############################### END UI: INTERFACE ###############################
    #################################################################################
    )
  }
