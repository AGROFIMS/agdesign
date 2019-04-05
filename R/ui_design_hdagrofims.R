shiny::HTML("<a name ='top'></a>")
# choices for statistical design input for HIDAP-AGROFIMS
listCountries <- c('Aruba','Afghanistan','Angola','Anguilla','Albania','Andorra','United Arab Emirates','Argentina','Armenia','American Samoa','Antarctica','French Southern Territories','Antigua and Barbuda','Australia','Austria','Azerbaijan','Burundi','Belgium','Benin','Bonaire','Burkina Faso','Bangladesh','Bulgaria','Bahrain','Bahamas','Bosnia and Herzegowina','Belarus','Belize','Bermuda','Bolivia','Brazil','Barbados','Brunei','Bhutan','Burma','Bouvet Island','Botswana','Byelorusian SSR (Former)','Central African Republic','Canada','Cocos (Keeling) Islands','Switzerland','Chile','China','CIPHQ','Cote dIvoire','Cameroon','Congo','Congo','Cook Islands','Colombia','Comoros','Cape Verde','Costa Rica','Czechoslovakia (Former)','Cuba','Curacao','Christmas Island (Australia)','Cayman Islands','Cyprus','Czech Republic','German Democratic Republic','Germany','Djibouti','Dominica','Denmark','Dominican Republic','Algeria','Ecuador','Egypt','Eritrea','Western Sahara','Spain','Estonia','Ethiopia','Finland','Fiji','Falkland Islands (Malvinas)','France','Faroe Islands','Micronesia','Gabon','United Kingdom','Georgia','Ghana','Gibraltar','Guinea','Guadeloupe','Gambia','Guinea-Bissau','Equatorial Guinea','Greece','Grenada','Greenland','Guatemala','French Guiana','Guam','Guyana','Hong Kong','Heard and Mc Donald Islands','Honduras','Croatia','Haiti','Hungary','Indonesia','India','British Indian Ocean Territory','Ireland','Iran','Iraq','Iceland','Israel','Italy','Jamaica','Jordan','Japan','Kazakhstan','Kenya','Kyrgyzstan','Cambodia','Kiribati','Saint Kitts and Nevis','Korea','Kuwait','Lao People s Democratic Republic','Lebanon','Liberia','Libyan Arab Jamahiriya','Saint Lucia','Liechtenstein','Sri Lanka','Lesotho','Lithuania','Luxemburg','Latvia','Macau','Saint Martin (French part)','Macedonia','Morocco','Monaco','Moldova','Madagascar','Maldives','Mexico','Marshall Islands','Mali','Malta','Myanmar','Mongolia','Northern Mariana Islands','Mozambique','Mauritania','Montserrat','Martinique','Mauritius','Malawi','Malaysia','Mayotte','Namibia','New Caledonia','Niger','Norfolk Island','Nigeria','Nicaragua','Niue','Netherlands','Norway','Nepal','Nauru','Neutral Zone (Former)','New Zealand','Oman','Pakistan','Palestine','Panama','Pitcairn Islands','Peru','Philippines','Palau','Papua New Guinea','Poland','Puerto Rico','Korea','Portugal','Paraguay','French Polynesia','Qatar','Reunion','Romania','Russian Federation','Rwanda','Saudi Arabia','Serbia and Montenegro','Scotland','Sudan','Senegal','Singapore','Saint Helena','Svalbard and Jan Mayen Islands','Solomon Islands','Sierra Leone','El Salvador','San Marino','Somalia','Saint Pierre and Miquelon','Serbia','Sao Tome e Principe','Union of Soviet Socialist Republics (Former)','Surinam','Slovakia','Slovenia','Sweden','Swaziland','Seychelles','Syrian Arab Republic','Turks and Caicos Islands','Chad','Togo','Thailand','Tajikistan','Tokelau','Turkmenistan','East Timor','Tonga','Trinidad and Tobago','Tunisia','Turkey','Tuvalu','Taiwan','Tanzania','Uganda','Ukraine','United States Misc. Pacific Islands','unknown','Uruguay','United States of America','Uzbekistan','Vatican City State','Saint Vincent and the Grenadines','Venezuela','British Virgin Islands','Virgin Islands (US)','Viet Nam','Vanuatu','Wallis and Fortuna Islands_','Samoa','Yemen','Yugoslavia (Former)','South Africa','Zaire','Zambia','Zimbabwe'
)
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
#' @author Raul Arias, Omar Benites, Ivan Perez
#' @param type type of ui element; default is a tab in a shiny dashboard
#' @param title display title
#' @param name a reference name
#' @export


################## Begin Simple Modules

ui_fieldbook_agrofims <- function(type="tab",title="Design Fieldbook",name="phenotype_fieldbook_design" ){
  
  ##### Tab item #####
  shinydashboard::tabItem(
    tabName = name,

    fluidRow(
      column(6, style = "margin-top: -16px; margin-bottom: 16px;", h1("Experiment description")),
      column(6, align = "right", style = "margin-top: 11px;",
             useShinyalert()#,
             #actionButton("xtest", "Test"),
             #actionButton('newfieldbook', 'New', icon("file"), class = "btn-primary", style="color: #fff;", width = "75px"),
             #actionButton('openfieldbook', 'Open', icon("folder-open"), width = "75px", onclick = "openTab('uisessionagrofims')"),
             #actionButton('savefieldbook', 'Save', icon("save"), class = "btn-success", style="color: #fff;", width = "75px"),
             #actionButton("testsession", "test"),
             #htmlOutput("lastsaved")
             #uiOutput("saveUI")
      )
    ),

    fluidRow(
      column(12, verbatimTextOutput("text1"))
    ),

    #h1("Experiment description"),

    # To reset panels and UI
    shinyjs::useShinyjs(),
    #useShinyFeedback(),
    
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
    
    
    #### nuevo codigo ivannnnnn ###############################################################
    # FCRD 
    tags$script('$(document).on("change", "select[id^=\'fcrd_sel_factor_\']",  function(){
                Shiny.onInputChange("selectFCRD", Math.random());
                Shiny.onInputChange("selectFCRDid", this.id);
})
                '),
    
    tags$script('$(document).on("change", "select[id^=\'fcrd_numLevels_\']",  function(){
                Shiny.onInputChange("levelsFCRD", Math.random());
                Shiny.onInputChange("levelsFCRDid", this.id);
                })
                '),
    
    tags$script('$(document).on("change", "select[id^=\'fcrd_lvl_\']",  function(){
                Shiny.onInputChange("otherFCRD", Math.random());
                Shiny.onInputChange("otherFCRDid", this.id);
                })
                '),
    
    # FRCBD
    tags$script('$(document).on("change", "select[id^=\'frcbd_sel_factor_\']",  function(){
                Shiny.onInputChange("selectFRCBD", Math.random());
                Shiny.onInputChange("selectFRCBDid", this.id);
                })
                '),
    
    tags$script('$(document).on("change", "select[id^=\'frcbd_numLevels_\']",  function(){
                Shiny.onInputChange("levelsFRCBD", Math.random());
                Shiny.onInputChange("levelsFRCBDid", this.id);
})
                '),
    
    tags$script('$(document).on("change", "select[id^=\'frcbd_lvl_\']",  function(){
                Shiny.onInputChange("otherFRCBD", Math.random());
                Shiny.onInputChange("otherFRCBDid", this.id);
})
                '),
    
    # FCRD: script listening when a box is to be deleted
    tags$script('$(document).on("click", "button[id^=\'fcrd_closeBox_\']",  function(){
                Shiny.onInputChange("closeBox_button_FCRD", Math.random());
                Shiny.onInputChange("closeBox_button_FCRDid", this.id);
    })
                '),
    
    # FRCBD: script listening when a box is to be deleted
    tags$script('$(document).on("click", "button[id^=\'frcbd_closeBox_\']",  function(){
                Shiny.onInputChange("closeBox_button_FRCBD", Math.random());
                Shiny.onInputChange("closeBox_button_FRCBDid", this.id);
    })
                '),
    
    ####
    
    # SPRCBD 
    tags$script('$(document).on("change", "select[id^=\'sprcbd_sel_factor_\']",  function(){
                Shiny.onInputChange("selectSPRCBD", Math.random());
                Shiny.onInputChange("selectSPRCBDid", this.id);
    })
                '),
    
    tags$script('$(document).on("change", "select[id^=\'sprcbd_numLevels_\']",  function(){
                Shiny.onInputChange("levelsSPRCBD", Math.random());
                Shiny.onInputChange("levelsSPRCBDid", this.id);
                })
                '),
    
    tags$script('$(document).on("change", "select[id^=\'sprcbd_lvl_\']",  function(){
                Shiny.onInputChange("otherSPRCBD", Math.random());
                Shiny.onInputChange("otherSPRCBDid", this.id);
                })
                '),
    
    # SPSP
    tags$script('$(document).on("change", "select[id^=\'spsp_sel_factor_\']",  function(){
                Shiny.onInputChange("selectSPSP", Math.random());
                Shiny.onInputChange("selectSPSPid", this.id);
    })
                '),
    
    tags$script('$(document).on("change", "select[id^=\'spsp_numLevels_\']",  function(){
                Shiny.onInputChange("levelsSPSP", Math.random());
                Shiny.onInputChange("levelsSPSPid", this.id);
    })
                '),
    
    tags$script('$(document).on("change", "select[id^=\'spsp_lvl_\']",  function(){
                Shiny.onInputChange("otherSPSP", Math.random());
                Shiny.onInputChange("otherSPSPid", this.id);
    })
                '),
    
    # SPRCBD: script listening when a box is to be deleted
    tags$script('$(document).on("click", "button[id^=\'sprcbd_closeBox_\']",  function(){
                Shiny.onInputChange("closeBox_button_SPRCBD", Math.random());
                Shiny.onInputChange("closeBox_button_SPRCBDid", this.id);
    })
                '),
    
    # SPSP: script listening when a box is to be deleted
    tags$script('$(document).on("click", "button[id^=\'spsp_closeBox_\']",  function(){
                Shiny.onInputChange("closeBox_button_SPSP", Math.random());
                Shiny.onInputChange("closeBox_button_SPSPid", this.id);
    })
                '),
    
    
    
    
    
    
    ############################

    ### code for buttons in tab soil to calculate sum
    # tags$script('$(document).on("click", "button[id^=\'buttonSoilTab_\']",  function(){
    tags$script('$(document).on("change", "input[id^=\'input_tabSoil_rate_\']",  function(){
                 Shiny.onInputChange("calculateTabSoil", Math.random());
                 Shiny.onInputChange("calculateTabSoilButtonId", this.id);
                 })
                '
    ),
    
    tags$script('$(document).on("change", "select[id^=\'cropCommonNameInter\']",  function(){
                 Shiny.onInputChange("cropBoxInterVar", Math.random());
                 Shiny.onInputChange("cropBoxInterVarId", this.id);
                 })
                '
    ),
    tags$script('$(document).on("change", "input[id^=\'cropCommonNameInter\']",  function(){
                 Shiny.onInputChange("cropBoxInterVarOther", Math.random());
                 Shiny.onInputChange("cropBoxInterVarOtherId", this.id);
                 })
                '
    ),

    # suma product en exp cond
    tags$script('$(document).on("change", "input[id^=\'input_productRate_soil_table_row\']",  function(){
                 Shiny.onInputChange("calculateTabSoil2", Math.random());
                 Shiny.onInputChange("calculateTabSoil2ButtonId", this.id);
                 })
                '
    ),
    
    tags$script('$(document).on("click", "a[id*=\'_titleId\']",  function(){
                 Shiny.onInputChange("boxTitleClicked", Math.random());
                 Shiny.onInputChange("boxTitleClickedId", this.id);
                 })
                '
    ),

    # suma elemt en exp cond
    tags$script('$(document).on("change", "input[id^=\'input_elementRate_soil_table_row_\']",  function(){
                 Shiny.onInputChange("calculateTabSoil3", Math.random());
                 Shiny.onInputChange("calculateTabSoil3ButtonId", this.id);
                 })
                '
    ),

    ### code for buttons in tab soil to calculate sum in Experiment conditions
    # tags$script('$(document).on("click", "button[id^=\'buttonSoilTab_\']",  function(){
    # tags$script('$(document).on("change", "input[id^=\'input_tabSoil_rate_\']",  function(){
    #                   Shiny.onInputChange("calculateTabSoil", Math.random());
    #                   Shiny.onInputChange("calculateTabSoilButtonId", this.id);
    #
    #               })
    #             '
    # ),

    ## script listening when fertilizer type is changed in soil fertility in experiment conditions
    tags$script('$(document).on("change", "select[id^=\'select_fertilizerType_soil_table_row\']",  function(){
                 Shiny.onInputChange("soilFertility_typeFertilizer", Math.random());
                 Shiny.onInputChange("soilFertility_typeFertilizer_id", this.id);
                 Shiny.onInputChange("soilFertility_typeFertilizer_value", this.value);
                 })
                '
    ),
    
    ## script listening when a box is to be deleted
    tags$script('$(document).on("click", "button[id^=\'closeBox_\']",  function(){
                 Shiny.onInputChange("closeBox_button", Math.random());
                 Shiny.onInputChange("closeBox_button_id", this.id);
                 })
                '
    ),

    ## script listening when product is changed in soil fertility in experiment conditions
    tags$script('$(document).on("change", "select[id^=\'select_product_soil_table_row_\']",  function(){
                 Shiny.onInputChange("soilFertility_product", Math.random());
                 Shiny.onInputChange("soilFertility_product_id", this.id);
                 })
                '
    ),

    tags$script('Shiny.addCustomMessageHandler("focus",
                 function(a) {
                 document.getElementById(a).focus();
                 });
                '
    ),
    
    
    ## select factor in list
    tags$script('$(document).on("change", "select[id^=\'sel_factor_\']",  function(){
                 Shiny.onInputChange("selectFactor", Math.random());
                 Shiny.onInputChange("selectFactorID", this.id);
                 })
                '
    ),
    
    ## duplicate factor
    tags$script('$(document).on("click", "button[id^=\'btDuplicate_\']",  function(){
                 Shiny.onInputChange("duplicateFactor", Math.random());
                 Shiny.onInputChange("duplicateFactorID", this.id);
                 })
                '
    ),
    
    ## duplicate factor
    tags$script('$(document).on("change", "input[id^=\'numLevels_tabSoil_\']",  function(){
                Shiny.onInputChange("levelsSoilFertility", Math.random());
                Shiny.onInputChange("levelsSoilFertilityID", this.id);
                })'
  ),
  
  ## num level update
  tags$script('$(document).on("change", "input[id^=\'numLevels_\']",  function(){
                Shiny.onInputChange("levelsFF", Math.random());
                Shiny.onInputChange("levelsFFID", this.id);
                })'
  ),
  tags$script('$(document).on("change", "input[id^=\'input_factor_treatment_\']",  function(){
                Shiny.onInputChange("changeInputSummary", Math.random());
                Shiny.onInputChange("changeInputSummaryId", this.id);
                })'
  ),
  tags$script('$(document).on("change", "select[id^=\'input_factor_treatment_\']",  function(){
                Shiny.onInputChange("changeInputSummary", Math.random());
                Shiny.onInputChange("changeInputSummaryId", this.id);
                })'
  ),
  
  
  
  

    # select_product_soil_table_row_
    #        tags$style(HTML("
    #          #landLeveling_start_date .form-control {
    #          background-color: #cce5ff;
    #    }
    #
    # ")),

    tags$style("*[id^='v2_'] {
                    transform: scale(1.8);
               }"),

    tags$style("#dt_sel {
                    transform: scale(1.5);
               }"),
    
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
    
                          tags$style("#myqr {
                                          
                                              width: 100px !important;
                                              height: 100px !important;
                                          
                                                }
                                      
                                      #uniqueId {
                                          
                                              text-align:center !important;
                                          
                                      }
                                     
                                     
                                     "),
  
                           tags$style("#myqr2 {
                                          
                                              width: 100px !important;
                                              height: 100px !important;
                                          
                                                }
                                      
                                      #uniqueId {
                                          
                                              text-align:center !important;
                                          
                                      }
                                     
                                     
                                     "),
  
  tags$style("
      .form-group {
        margin-bottom: 0px;
      }
    "),

                          # fluidRow(
                          #   box()
                          # ),
                          # tabsetPanel(id = "inExpInfo", #Begin Master tabSetPanel

                           # tabPanel(" ", #primer tab principal
                                    # column(width = 12,

                            # tabPanel("Create fieldbook",icon = icon("tag", lib = "glyphicon"),
                                     # br(),
                                     # shiny::wellPanel(
                                     #   shiny::HTML("<b>Fieldbook ID </b>"),
                                     #   shiny::textOutput("fbDesign_id")
                                     # ),

    # Box: Create fieldbook
    fluidRow(
      box(
        title = tagList(shiny::icon("plus-circle"), "Create fieldbook"), status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
        tabsetPanel(
          id= "fbDesignNav",

          ##### Tab: Start Experiment #####
          shiny::tabPanel(
            title = tagList(shiny::icon("info"), "Experiment"), value = "experiment",
            # column(
            #   12,
            #   fluidRow(
            #     column(6, h1("Experiment details")),
            #     column(6, align = "right", style = "margin-top: 26px;",
            #            actionButton("load_exampleM", "Monocrop"),
            #            actionButton("load_exampleI", "Intercrop")
            #     )
            #   )
            # ),
            
            column(
              12,
              fluidRow(
                column(
                  6,
                  h2("Experiment details"),
                  uiOutput("experimentIdUI"),
                  textInput(inputId = "experimentName", label = "Experiment name", value = ""),
                  textInput(inputId = "experimentProjectName", label = "Experiment project name", value = ""),
                  
                  #shiny::dateRangeInput("fbDesign_project_time_line", "Experiment date", start = Sys.Date() - 2, end = Sys.Date() + 20, startview = "year", format = "yyyy-mm-dd"),
                  # airDatepickerInput("fbDesign_project_time_line",
                  #                    "Experiment date",
                  #                    multiple = 2, clearButton = TRUE,
                  #                    value = c(Sys.Date() + 1, Sys.Date() + 30),
                  #                    range = T,
                  #                    autoClose = T,
                  #                    placeholder = "yyyy-mm-dd     to     yyyy-mm-dd",
                  #                    separator = "     to     "),
                  
                  fluidRow(
                    column(
                      6,
                      airDatepickerInput("fbDesign_project_start_date",
                                         "Experiment start date",
                                         clearButton = T,
                                         autoClose = T,
                                         value = Sys.Date() + 1,
                                         placeholder = "yyyy-mm-dd"
                      )
                    ),
                    column(
                      6,
                      uiOutput("exp_end_date")
                    )
                  ),
                  
                  selectizeInput("designFieldbook_typeExperiment", "Type of experiment", multiple = TRUE,
                                 options = list(maxItems =1, placeholder = "Select one..."),
                                 choices = c("Controlled treatment trial",
                                             "Varietal trial",
                                             #"Demonstration trial",
                                             "Germplasm screening trial")
                  ),
                  
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
            useShinyjs(),
            column(
              12,
              br(),
              h2("Funding Agency"),
              #actionButton("idtester", "test"),
              #actionButton("btn2", "action2"),
              #uiOutput("txt1"),
              fluidRow(id = "fr_fundingAgency_boxes"),
              actionButton("addFundingAgency", "Add a funding agency"),
              br(), br(), br()
            ),

            column(
              width = 6,
              textInput("experiment_grantNumber", "Grant number"),
              textInput("experiment_grantTitle", "Grant title")
            ),

            column(
              width = 12,
              h2("Project Management Entities"),
              fluidRow(id = "fr_managementEntities_boxes"),
              actionButton("addManagEntity", "Add a project management entity"),
              br(), br(), br()
            ),
            
            column(
              12,
              br(),
              #h2("Experiment Leads (if different from project management entity)"),
              HTML("<h2 style='display:inline;'>Experiment Leads</h2> <h4 style='display:inline;'>(if different from project management entity)</h4>"),
              fluidRow(id = "fr_experimentLeads_boxes"),
              actionButton("addExperimentLeads", "Add an experiment lead"),
              br(), br(), br()
            ),

            sidebarPanel(
              id = "sidebar", width = 12,
              actionButton("btnNextPersonnelInfo", "Next", class = "btn-primary", style="color: #fff;", href = "#top")
            )
          ),
          ##### Tab: End Experiment #####

          ##### Tab: Start Personnel #####
          tabPanel(
            title = tagList(shiny::icon("user"), "Personnel"), value = "tabPersonnel",
            column(
              width = 12,
              h2("Personnel associated with the experiment"),

              fluidRow(
                # column(6, selectInput("npersons", "Number of personnel", choices = 1:5)),
                column(6, style = "margin-top: 25px;", actionButton("btLoadMyInfoPersonnel", "Load my info", icon("user")))
                
              ),
              br(),
              fluidRow(id = "fr_personnel_boxes"),
              actionButton("addPersonnel", "Add personnel")
            ),
            
            sidebarPanel(
              id="sidebar", width = 12,
              actionButton("btnNextSite", "Next", class = "btn-primary",style="color: #fff;", href="#top")
            )
          ),
          ##### Tab: End Personnel #####

          ##### Tab: Start Site #####
          tabPanel(
            title = tagList(shiny::icon("location-arrow"), "Site"), value="tabSite",
            column(
              width = 6,
              h2("Site information"),
              uiOutput("uiTest"),
              #p(actionButton("refreshSiteList", "Look for sites", icon("location-arrow"))),
              p(actionButton("refreshSiteList", "Refresh all sites", icon("sync"))),
              shiny::uiOutput("fbDesign_country", inline = TRUE, width = 500),
              shiny::uiOutput("fbDesign_countrySite", inline = TRUE, width = 500), #,#locality
              br(),
              h2("Site surrounded by"),
              selectizeInput("fbDesign_inHighLevel", label="Higher-level landform", multiple = TRUE,
                             choices = sort(c("Plain",
                                         "Basin",
                                         "Valley",
                                         "Plateau",
                                         "Upland",
                                         "Hill",
                                         "Mountain")),
                             options = list(maxItems = 1, placeholder = 'Select  one...')
              ),

              selectizeInput("fbDesign_inSiteVegetation", label="Vegetation surrounding the experiment site", multiple = TRUE,
                             choices = c("Crops",
                                         "Forest",
                                         "Grassland",
                                         "Savanna",
                                         "Shrubs",
                                         "Woodland",
                                         "Other"),
                             options = list(maxItems = 5, placeholder = 'Select one... ')
              ),
              textAreaInput("inSiteDescNotes", label="Site description notes", value="")#,
              
              # br(),
              # h2("Site soil classification"),
              # h3("Under construction")
            ),

            sidebarPanel(
              id="sidebar", width = 12,
              actionButton("btnNextCropInfo", "Next", class = "btn-primary",style="color: #fff;", href="#top")
            )
          ),
          ##### Tab: End Site #####

          ##### Tab: Start Design #####
          shiny::tabPanel(
            title = tagList(shiny::icon("th-list"), "Design"), value = "tabDesign",
            tags$style(HTML("
                            #lvl_hdafims1 + div> div>.item {
                            background:   #337ab7 !important;
                            color: white;
                            }
                            
                            #lvl_hdafims1 + div> div>.selectize-dropdown-content .active {
                            background:   #337ab7 !important;
                            }
                            
                            #lvl_hdafims2 + div> div>.item {
                            background:   #f3217a !important;
                            color: white;
                            }
                            
                            #lvl_hdafims2 + div> div>.selectize-dropdown-content .active {
                            background:   #f3217a !important;
                            }
                            
                            #lvl_hdafims3 + div> div>.item {
                            background:   #777 !important;
                            color: white;
                            }
                            
                            #lvl_hdafims3 + div> div>.selectize-dropdown-content .active {
                            background:   #777 !important;
                            }
                            
                            #lvl_hdafims4 + div> div>.item {
                            background:   #5cb85c !important;
                            color: white;
                            }
                            
                            #lvl_hdafims4 + div> div>.selectize-dropdown-content .active {
                            background:   #5cb85c !important;
                            }
                            
                            #lvl_hdafims5 + div> div>.item {
                            background:   #FBBF09 !important;
                            color: white;
                            }
                            
                            #lvl_hdafims5 + div> div>.selectize-dropdown-content .active {
                            background:   #FBBF09 !important;
                            }"

            )),
            
            column(
              width = 6,
              #br(),
              h2("Design information"),
              # shiny::selectInput("designFieldbook_agrofims", "Select experimental design", selected = 'RCBD', multiple = FALSE,
              #                    c("Choose one" = "", design_choices_agrofims)
              # ),
              br()
              # conditionalPanel("input.designFieldbook_agrofims == 'CRD' || input.designFieldbook_agrofims == 'RCBD'",
              #                  h2("Information on experimental unit"),
              #                  selectizeInput("info_experiment_unit", "Information on experimental unit", multiple = T,
              #                                 options = list(maxItems =1, placeholder="Select one..."),
              #                                 choices = c("plot",
              #                                             "field",
              #                                             "pot")
              #                  ),
              #                  
              #                  conditionalPanel("input.info_experiment_unit == 'plot'",
              #                                   fluidRow(
              #                                     column(width = 3, textInput("expt_plot_length", label="Length", value="")),
              #                                     column(
              #                                       width = 3,
              #                                       selectizeInput("expt_plot_length_unit", label="Unit", multiple = TRUE,
              #                                                      options = list(maxItems =1, placeholder ="Select one..."),
              #                                                      choices = c("m",
              #                                                                  "ft")
              #                                       )
              #                                     ),
              #                                     column(width = 3, textInput("expt_plot_width", label="Width", value="")),
              #                                     column(
              #                                       width = 3,
              #                                       selectizeInput("expt_plot_width_unit", label="Unit", multiple = TRUE,
              #                                                      options = list(maxItems =1, placeholder ="Select one..."),
              #                                                      choices = c("m",
              #                                                                  "ft")
              #                                       )
              #                                     )
              #                                   )
              #                  ),
              #                  
              #                  conditionalPanel("input.info_experiment_unit == 'field'",
              #                                   fluidRow(
              #                                     column(width = 3, textInput("expt_field_length", label="Length", value="")),
              #                                     column(
              #                                       width = 3,
              #                                       selectizeInput("expt_field_length_unit", label="Unit", multiple = TRUE,
              #                                                      options = list(maxItems =1, placeholder ="Select one..."),
              #                                                      choices = c("m",
              #                                                                  "km",
              #                                                                  "ft",
              #                                                                  "mi")
              #                                       )
              #                                     ),
              #                                     column(width = 3, textInput("expt_field_width", label="Width", value="")),
              #                                     column(
              #                                       width = 3,
              #                                       selectizeInput("expt_field_width_unit", label="Unit", multiple = TRUE,
              #                                                      options = list(maxItems =1, placeholder ="Select one..."),
              #                                                      choices = c("m",
              #                                                                  "km",
              #                                                                  "ft",
              #                                                                  "mi")
              #                                       )
              #                                     )
              #                                   )
              #                  ),
              #                  
              #                  conditionalPanel("input.info_experiment_unit == 'pot'",
              #                                   fluidRow(
              #                                     column(width = 3, textInput("pot_diameter", label="Diameter", value="")),
              #                                     column(
              #                                       width = 3,
              #                                       selectizeInput("pot_diameter_unit", label="Unit", multiple = TRUE,
              #                                                      options = list(maxItems =1, placeholder ="Select one..."),
              #                                                      choices = c("cm",
              #                                                                  "in")
              #                                       )
              #                                     ),
              #                                     
              #                                     column(width = 3, textInput("pot_depth", label="Depth", value="")),
              #                                     column(
              #                                       width = 3,
              #                                       selectizeInput("pot_depth_unit", label="Unit", multiple = TRUE,
              #                                                      options = list(maxItems =1, placeholder ="Select one..."),
              #                                                      choices = c("cm",
              #                                                                  "in")
              #                                       )
              #                                     )
              #                                   )
              #                  )
              # ),
              # 
              # #br(),
              # conditionalPanel("input.designFieldbook_agrofims == 'SPRCBD'",
              #                  h2("Information on experimental unit"),
              #                  # selectizeInput("info_experiment_unit", "Information on experimental unit", multiple = T,
              #                  #                options = list(maxItems =1, placeholder="Select one..."),
              #                  #                choices = c("plot")#,
              #                  #                            # "field",
              #                  #                            # "pot")
              #                  # ),
              #                  fluidRow(
              #                    column(
              #                      width = 12, 
              #                      h4("Information on main plot")
              #                    )
              #                  ),
              #                  fluidRow(
              #                    column(width = 3, textInput("expt_plot_length", label="Length", value="")),
              #                    column(
              #                      width = 3,
              #                      selectizeInput("expt_plot_length_unit", label="Unit", multiple = TRUE,
              #                                     options = list(maxItems =1, placeholder ="Select one..."),
              #                                     choices = c("m",
              #                                                 "ft")
              #                      )
              #                    ),
              #                    column(width = 3, textInput("expt_plot_width", label="Width", value="")),
              #                    column(
              #                      width = 3,
              #                      selectizeInput("expt_plot_width_unit", label="Unit", multiple = TRUE,
              #                                     options = list(maxItems =1, placeholder ="Select one..."),
              #                                     choices = c("m",
              #                                                 "ft")
              #                      )
              #                    )
              #                  ),
              #                  
              #                  fluidRow(
              #                    column(
              #                      width = 12, 
              #                      h4("Information on sub plot")
              #                    )
              #                  ),
              #                  fluidRow(
              #                    column(width = 3, textInput("expt_plot_length", label="Length", value="")),
              #                    column(
              #                      width = 3,
              #                      selectizeInput("expt_plot_length_unit", label="Unit", multiple = TRUE,
              #                                     options = list(maxItems =1, placeholder ="Select one..."),
              #                                     choices = c("m",
              #                                                 "ft")
              #                      )
              #                    ),
              #                    column(width = 3, textInput("expt_plot_width", label="Width", value="")),
              #                    column(
              #                      width = 3,
              #                      selectizeInput("expt_plot_width_unit", label="Unit", multiple = TRUE,
              #                                     options = list(maxItems =1, placeholder ="Select one..."),
              #                                     choices = c("m",
              #                                                 "ft")
              #                      )
              #                    )
              #                  )
              #                  
              # )
              
              
              #,
              
              # br(),
              # h2("Design information"),
              # shiny::selectInput("designFieldbook_agrofims", "Select experimental design", selected = 'CRD', multiple = FALSE,
              #                    c("Choose one" = "", design_choices_agrofims)
              # )
            ),
            
            column(
              12,
              br(),
              h2("Treatment description"),
              # HTML("<center>"),
              h3( shinyWidgets::radioGroupButtons(inputId = "fullFactorialRB", label = "Is this a full factorial design?", choices=c("Yes", "No"), status= "primary", size= "lg", checkIcon = list(yes = icon("ok", lib = "glyphicon"))))
              # HTML("</center>")
            ),
            
            column(
              12,
              tabsetPanel(
                id= "treatmentSetPanel",
                shiny::tabPanel(
                  "Factors",
                  value = "tabTreatmentFactors",
                  br(),
                  br(),
                  column(
                    width = 12,
                    conditionalPanel("input.designFieldbook_agrofims == 'SPRCBD'",
                                     h3("Main and sub plot factors (1st factor applies to main plot; 2nd factor to sub plot)")
                    ),
                    fluidRow(id = "fluid_treatment_description"),
                    
                    fluidRow(id="fluid_factor_input")
                  )
                )
              )
            ),
            
            sidebarPanel(
              id="sidebar", width = 12,
              actionButton("btnNextAgro", "Next", class = "btn-primary",style="color: #fff;" , href="#top")
            )
            ),
          ##### Tab: End Design #####
          
          ##### Tab: Start Design New #####
          
          shiny::tabPanel(
            title = tagList(shiny::icon("th-list"), "Design New"), value = "tabDesign",
            
            column(
              width = 6,
              #br(),
              h2("Design information"),
              shiny::selectInput("designFieldbook_agrofims", "Select experimental design", selected = 'FRCBD', multiple = FALSE,
                                 c("Choose one" = "", design_choices_agrofims)
              ),
              
              conditionalPanel("input.designFieldbook_agrofims == 'CRD' || input.designFieldbook_agrofims == 'RCBD' || input.designFieldbook_agrofims == 'FCRD' || input.designFieldbook_agrofims == 'FRCBD'",
                               h2("Information on experimental unit"),
                               selectizeInput("info_experiment_unit", "Information on experimental unit", multiple = T,
                                              options = list(maxItems =1, placeholder="Select one..."),
                                              choices = c("plot",
                                                          "field",
                                                          "pot")
                               ),
                               
                               conditionalPanel("input.info_experiment_unit == 'plot'",
                                                fluidRow(
                                                  column(width = 3, textInput("expt_plot_length", label="Length", value="")),
                                                  column(
                                                    width = 3,
                                                    selectizeInput("expt_plot_length_unit", label="Unit", multiple = TRUE,
                                                                   options = list(maxItems =1, placeholder ="Select one..."),
                                                                   choices = c("m",
                                                                               "ft")
                                                    )
                                                  ),
                                                  column(width = 3, textInput("expt_plot_width", label="Width", value="")),
                                                  column(
                                                    width = 3,
                                                    selectizeInput("expt_plot_width_unit", label="Unit", multiple = TRUE,
                                                                   options = list(maxItems =1, placeholder ="Select one..."),
                                                                   choices = c("m",
                                                                               "ft")
                                                    )
                                                  )
                                                )
                               ),
                               
                               conditionalPanel("input.info_experiment_unit == 'field'",
                                                fluidRow(
                                                  column(width = 3, textInput("expt_field_length", label="Length", value="")),
                                                  column(
                                                    width = 3,
                                                    selectizeInput("expt_field_length_unit", label="Unit", multiple = TRUE,
                                                                   options = list(maxItems =1, placeholder ="Select one..."),
                                                                   choices = c("m",
                                                                               "km",
                                                                               "ft",
                                                                               "mi")
                                                    )
                                                  ),
                                                  column(width = 3, textInput("expt_field_width", label="Width", value="")),
                                                  column(
                                                    width = 3,
                                                    selectizeInput("expt_field_width_unit", label="Unit", multiple = TRUE,
                                                                   options = list(maxItems =1, placeholder ="Select one..."),
                                                                   choices = c("m",
                                                                               "km",
                                                                               "ft",
                                                                               "mi")
                                                    )
                                                  )
                                                )
                               ),
                               
                               conditionalPanel("input.info_experiment_unit == 'pot'",
                                                fluidRow(
                                                  column(width = 3, textInput("pot_diameter", label="Diameter", value="")),
                                                  column(
                                                    width = 3,
                                                    selectizeInput("pot_diameter_unit", label="Unit", multiple = TRUE,
                                                                   options = list(maxItems =1, placeholder ="Select one..."),
                                                                   choices = c("cm",
                                                                               "in")
                                                    )
                                                  ),
                                                  
                                                  column(width = 3, textInput("pot_depth", label="Depth", value="")),
                                                  column(
                                                    width = 3,
                                                    selectizeInput("pot_depth_unit", label="Unit", multiple = TRUE,
                                                                   options = list(maxItems =1, placeholder ="Select one..."),
                                                                   choices = c("cm",
                                                                               "in")
                                                    )
                                                  )
                                                )
                               )
              ),
              
              #br(),
              conditionalPanel("input.designFieldbook_agrofims == 'SPRCBD'",
                               h2("Information on experimental unit"),
                               # selectizeInput("info_experiment_unit", "Information on experimental unit", multiple = T,
                               #                options = list(maxItems =1, placeholder="Select one..."),
                               #                choices = c("plot")#,
                               #                            # "field",
                               #                            # "pot")
                               # ),
                               fluidRow(
                                 column(
                                   width = 12, 
                                   h4("Information on main plot")
                                 )
                               ),
                               fluidRow(
                                 column(width = 3, textInput("sprcbd_main_expt_plot_length", label="Length", value="")),
                                 column(
                                   width = 3,
                                   selectizeInput("sprcbd_main_expt_plot_length_unit", label="Unit", multiple = TRUE,
                                                  options = list(maxItems =1, placeholder ="Select one..."),
                                                  choices = c("m",
                                                              "ft")
                                   )
                                 ),
                                 column(width = 3, textInput("sprcbd_main_expt_plot_width", label="Width", value="")),
                                 column(
                                   width = 3,
                                   selectizeInput("sprcbd_main_expt_plot_width_unit", label="Unit", multiple = TRUE,
                                                  options = list(maxItems =1, placeholder ="Select one..."),
                                                  choices = c("m",
                                                              "ft")
                                   )
                                 )
                               ),
                               
                               fluidRow(
                                 column(
                                   width = 12, 
                                   h4("Information on sub plot")
                                 )
                               ),
                               fluidRow(
                                 column(width = 3, textInput("sprcbd_sub_expt_plot_length", label="Length", value="")),
                                 column(
                                   width = 3,
                                   selectizeInput("sprcbd_sub_expt_plot_length_unit", label="Unit", multiple = TRUE,
                                                  options = list(maxItems =1, placeholder ="Select one..."),
                                                  choices = c("m",
                                                              "ft")
                                   )
                                 ),
                                 column(width = 3, textInput("sprcbd_sub_expt_plot_width", label="Width", value="")),
                                 column(
                                   width = 3,
                                   selectizeInput("sprcbd_sub_expt_plot_width_unit", label="Unit", multiple = TRUE,
                                                  options = list(maxItems =1, placeholder ="Select one..."),
                                                  choices = c("m",
                                                              "ft")
                                   )
                                 )
                               )
                               
              ),
              conditionalPanel("input.designFieldbook_agrofims == 'SPSP'",
                               h2("Information on experimental unit"),
                               # selectizeInput("info_experiment_unit", "Information on experimental unit", multiple = T,
                               #                options = list(maxItems =1, placeholder="Select one..."),
                               #                choices = c("plot")#,
                               #                            # "field",
                               #                            # "pot")
                               # ),
                               fluidRow(
                                 column(
                                   width = 12, 
                                   h4("Information on main plot")
                                 )
                               ),
                               fluidRow(
                                 column(width = 3, textInput("spsp_main_expt_plot_length", label="Length", value="")),
                                 column(
                                   width = 3,
                                   selectizeInput("spsp_main_expt_plot_length_unit", label="Unit", multiple = TRUE,
                                                  options = list(maxItems =1, placeholder ="Select one..."),
                                                  choices = c("m",
                                                              "ft")
                                   )
                                 ),
                                 column(width = 3, textInput("spsp_main_expt_plot_width", label="Width", value="")),
                                 column(
                                   width = 3,
                                   selectizeInput("spsp_main_expt_plot_width_unit", label="Unit", multiple = TRUE,
                                                  options = list(maxItems =1, placeholder ="Select one..."),
                                                  choices = c("m",
                                                              "ft")
                                   )
                                 )
                               ),
                               
                               fluidRow(
                                 column(
                                   width = 12, 
                                   h4("Information on sub plot")
                                 )
                               ),
                               fluidRow(
                                 column(width = 3, textInput("spsp_sub_expt_plot_length", label="Length", value="")),
                                 column(
                                   width = 3,
                                   selectizeInput("spsp_sub_expt_plot_length_unit", label="Unit", multiple = TRUE,
                                                  options = list(maxItems =1, placeholder ="Select one..."),
                                                  choices = c("m",
                                                              "ft")
                                   )
                                 ),
                                 column(width = 3, textInput("spsp_sub_expt_plot_width", label="Width", value="")),
                                 column(
                                   width = 3,
                                   selectizeInput("spsp_sub_expt_plot_width_unit", label="Unit", multiple = TRUE,
                                                  options = list(maxItems =1, placeholder ="Select one..."),
                                                  choices = c("m",
                                                              "ft")
                                   )
                                 )
                               ),
                               
                               fluidRow(
                                 column(
                                   width = 12, 
                                   h4("Information on sub-sub plot")
                                 )
                               ),
                               fluidRow(
                                 column(width = 3, textInput("spsp_subsub_expt_plot_length", label="Length", value="")),
                                 column(
                                   width = 3,
                                   selectizeInput("spsp_subsub_expt_plot_length_unit", label="Unit", multiple = TRUE,
                                                  options = list(maxItems =1, placeholder ="Select one..."),
                                                  choices = c("m",
                                                              "ft")
                                   )
                                 ),
                                 column(width = 3, textInput("spsp_subsub_expt_plot_width", label="Width", value="")),
                                 column(
                                   width = 3,
                                   selectizeInput("spsp_subsub_expt_plot_width_unit", label="Unit", multiple = TRUE,
                                                  options = list(maxItems =1, placeholder ="Select one..."),
                                                  choices = c("m",
                                                              "ft")
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
                h1("CRD"),
                fluidRow(
                  column(
                    6,
                    selectInput("crd_rep", "Repetitions", choices = 2:100, selected = 2)
                  ),
                  column(
                    6,
                    selectInput("crd_ntrt", "Number of treatments", choices = 2:100, selected = 2)
                  )
                ),
                fluidRow(id = "crd_boxes"),
                actionButton("crd_add", "Add factor")
              ),
              
              conditionalPanel(
                "input.designFieldbook_agrofims == 'RCBD'",
                #selectInput("crd_rep", "Block ", choices = 2:10000,selected = 2),
                #selectInput("rcbd_ntrt", "Number of treatments", choices = 2:10000,selected = 2),
                "RCBD"
              ),
              
              conditionalPanel(
                "input.designFieldbook_agrofims == 'FCRD'",
                h1("FCRD"),
                fluidRow(
                  column(
                    6,
                    selectInput("fcrd_rep", "Repetitions", choices = 2:100, selected = 2)
                  )
                ),
                fluidRow(id = "fcrd_boxes"),
                actionButton("fcrd_add", "Add factor")
              ),
              
              conditionalPanel(
                "input.designFieldbook_agrofims == 'FRCBD'",
                h1("FRCBD"),
                fluidRow(
                  column(
                    6,
                    selectInput("frcbd_block", "Block", choices = 2:100, selected = 2)
                  )
                ),
                fluidRow(id = "frcbd_boxes"),
                actionButton("frcbd_add", "Add factor")
              ),
              
              conditionalPanel(
                "input.designFieldbook_agrofims == 'SPRCBD'",
                h1("Split Plot"),
                fluidRow(
                  column(
                    6,
                    selectInput("sp1_block", "Block", choices = 2:100, selected = 2)
                  )
                ),
                fluidRow(id = "sprcbd_boxes"),
                fluidRow(id = "sprcbd_boxes")#,
                #actionButton("sprcbd_add", "Add factor")
              ),
              
              conditionalPanel(
                "input.designFieldbook_agrofims == 'SPSP'",
                h1("Split-Split Plot"),
                fluidRow(
                  column(
                    6,
                    selectInput("spsp2_block", "Block", choices = 2:100, selected = 2)
                  )
                ),
                fluidRow(id = "spsp_boxes")#,
                #actionButton("spsp_add", "Add factor")
              ),
              
              conditionalPanel(
                "input.designFieldbook_agrofims == 'STRIP'",
                #selectInput("strip_block","Block", choices = 2:10000,selected = 2),
                "STRIP"
              )
            )
          ),
          
          ##### Tab: End Design New #####
          
          
          ##### Tab: Start Crop #####
          tabPanel(
            title = tagList(shiny::icon("pagelines"), "Crop"), value="tabCrop",
            
            column(
              12,
              fluidRow(
                column(
                  6,
                  h2("Fieldbook details"),
                  uiOutput("fieldbookIdUI")
                  #textInput(inputId = "experimentName", label = "Experiment name", value = ""),
                  #textInput(inputId = "experimentProjectName", label = "Experiment project name", value = "")
                ),
                
                column(
                  6, 
                  align = "right",
                  br(),
                  imageOutput("myqr2")#,
                  #uiOutput("IdUI")
                )
              )
            ),
            
            column(
              width = 6,
              h2("Description of crops sown"),
              shiny::selectInput("croppingType", "Cropping type",
                                 choices = c("Monocrop",
                                             "Intercrop",
                                             "Relay crop",
                                             "Rotation")#,
                                 #selected = "Intercrop"
              )
            ),

            column(
              width = 12,
              conditionalPanel("input.croppingType == 'Monocrop'",
                               fluidRow(
                                 column(
                                   width = 12,
                                   br(),
                                   h2("Crop information"),
                                   fluidRow(
                                     column(
                                       width = 6,
                                       selectizeInput("cropCommonNameMono", "Crop common name", multiple = TRUE,
                                                      options = list(maxItems =1, placeholder="Select one..."),
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
                                       width = 6,
                                       selectizeInput(inputId = "cultivarNameMono", label = "Variety name(s)", choices = c(), multiple = T,
                                                      options = list('create' = TRUE)
                                       )
                                     )
                                   )
                                 )
                               )
              ),

              conditionalPanel("input.croppingType == 'Intercrop'",
                     # fluidRow(
                     #   
                     # ),
                     h2("Crop information"),
                     fluidRow(id="fr_intercrop_boxes"),
                     actionButton("addIntercrop", "Add crop"),
                     br(),br(),
                     h2("Intercrop arrangement"),
                     fluidRow(
                       column(
                         width = 6,
                         selectizeInput("fr_intercrop_arrangement", "", multiple = TRUE,
                                        options = list(maxItems =1, placeholder="Select one..."),
                                        choices = c("Mixed intercropping",
                                                    "Row intercropping")
                         )
                       )
                     ),
                     fluidRow(
                       column(12,
                              h2("Intercrop row geometry"),
                              fluidRow(id="fr_intercrop_geometry_boxes")
                       )
                     )
              ),
              
              conditionalPanel("input.croppingType == 'Relay crop'",
                               # fluidRow(
                               #   
                               # ),
                               h2("Crop information"),
                               #h3("Under construction"),
                               
                               fluidRow(id="fr_relaycrop_boxes"),
                               actionButton("addRelaycrop", "Add crop"),
                               br(),br()
                               #h2("relaycrop arrangement"),
                               #fluidRow(
                              #   column(
                              #     width = 6,
                              #     selectizeInput("fr_relaycrop_arrangement", "", multiple = TRUE,
                              #                    options = list(maxItems =1, placeholder="Select one..."),
                              #                    choices = c("Mixed relaycropping",
                              #                                "Row relaycropping")
                              #     )
                              #   )
                              # ),
                              # fluidRow(
                              #   column(12,
                              #          h2("relaycrop row geometry"),
                              #          fluidRow(id="fr_relaycrop_geometry_boxes")
                              #   )
                              # )
              ),
              
              conditionalPanel("input.croppingType == 'Rotation'",
                               # fluidRow(
                               #   
                               # ),
                               h2("Crop information"),
                               #h3("Under construction"),
                               fluidRow(id="fr_rotationcrop_boxes"),
                               actionButton("addRotationcrop", "Add crop"),
                               br(),br()#,
                               # h2("Intercrop arrangement"),
                               # fluidRow(
                               #   column(
                               #     width = 6,
                               #     selectizeInput("fr_intercrop_arrangement", "", multiple = TRUE,
                               #                    options = list(maxItems =1, placeholder="Select one..."),
                               #                    choices = c("Mixed intercropping",
                               #                                "Row intercropping")
                               #     )
                               #   )
                               # ),
                               # fluidRow(
                               #   column(12,
                               #          h2("Intercrop row geometry"),
                               #          fluidRow(id="fr_intercrop_geometry_boxes")
                               #   )
                               # )
              ),

              br(),
              h2("Previous crop or fallow"),
              fluidRow(
                column(
                  width = 6,
                  selectizeInput("prevCropName", "", multiple = TRUE,
                                 options = list(maxItems =1, placeholder="Select one..."),
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
          ##### Tab: End Crop #####

 

          ##### Tab: Start Experiment conditions #####
          shiny::tabPanel("Experiment conditions", value = "tabAgroFeat", icon = shiny::icon("truck"),
                                                        column(width = 6,
                                                               h2("Experiment conditions"),
                                                               selectizeInput("selectAgroFeature", "", c(), multiple = TRUE, choices=c(
                                                                  "Residue management",
                                                                  "Seedbed preparation",
                                                                  "Soil fertility",
                                                                  "Planting and transplanting",
                                                                  "Mulch management",
                                                                  "Irrigation",
                                                                  "Weeding",
                                                                  "Harvest"
                                                                  ),
                                                                  options = list(maxItems = 8, placeholder = "Select some...")
                                                                )
                                                        ),

                                                      br(),
                                                      column(width = 12,

                                                      br(),

                                                        tabsetPanel(id= "nutrienTabPanels",
                                                              tabPanel(div(id = "gh", "Residue management"), value="tabResidue",
                                                                       column(width = 12,
                                                                              br(),
                                                                              fluidRow(
                                                                                box(id="residue_description_boxid",
                                                                                    title = checkboxInput("residueDesc_checkbox", actionLink("residue_description_titleId", "Residue description"), T),
                                                                                    status = "primary",
                                                                                    solidHeader = TRUE,
                                                                                    width = 12, collapsible = TRUE,  collapsed = TRUE,
                                                                                    fluidRow(
                                                                                      column(width = 6,
                                                                                             selectizeInput("rmgt_residue_plantPart", label = "Plant part", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                              c("Husk",
                                                                                                                "Leaf",
                                                                                                                "Root",
                                                                                                                "Seed Pod/Cob/Fruit",
                                                                                                                "Stem",
                                                                                                                "Stubble",
                                                                                                                "Other")
                                                                                             ),
                                                                                             hidden(textInput("rmgt_residue_plantPart_other", "", value = "")),
                                                                                             selectizeInput("rmgt_crop_residue_moisture", "Crop residue moisture", multiple=T, options = list(maxItems=1, placeholder="Select one..."),
                                                                                                            choices = c("Dry", "Moist", "Wet")),
                                                                                             fluidRow(
                                                                                               column(width = 6,
                                                                                                      numericInput("rmgt_crop_residue_thick", value="", label = "Crop residue thickness", min=0, max=100, step = 0.1)
                                                                                               ),
                                                                                               column(width = 6,
                                                                                                      selectizeInput("rmgt_crop_residue_thick_unit", "Unit",multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                                                                                     choices = c("cm", "ft", "in", "m"), selected = "cm")
                                                                                               )
                                                                                             ),
                                                                                             fluidRow(
                                                                                               column(width = 6,
                                                                                                      numericInput("rmgt_crop_residue_amount_sqm", value="", label = "Crop residue amount", min=0, max=100, step = 0.1)
                                                                                               ),
                                                                                               column(width = 6,
                                                                                                      selectizeInput("rmgt_crop_residue_amount_sqm_unit", "Unit",multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                                                                                     choices = c("g/ft2", "g/m2", "kg/ha", "kg/m2", "lb/ac"), selected = "kg/ha")
                                                                                               )
                                                                                             ),
                                                                                             fluidRow(
                                                                                               column(6, numericInput("rmgt_crop_residue_perc_cov","Crop residue percent of coverage", value="", min=0, max=100)),
                                                                                               column(6, selectInput("rmgt_crop_residue_perc_cov_unit", "Unit", c("%"), selected = "%"))
                                                                                             ),
                                                                                             # fluidRow(
                                                                                             #   column(6,textInput("residue_inc_depth", "Residue incorporation depth", value="")),
                                                                                             #   column(6, selectizeInput("residue_inc_depth_unit", "Unit",multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                                                             #                            choices = c("cm", "ft", "in", "m"))
                                                                                             #   )
                                                                                             # ),
                                                                                             
                                                                                             
                                                                                             # fluidRow(
                                                                                             #   column(width = 6,
                                                                                             #
                                                                                             #          textInput("above_ground_res_amount", value="", label = "Above ground residue (amount)")
                                                                                             #   ),
                                                                                             #   column(width = 6,
                                                                                             #          selectizeInput("above_ground_res_amount_unit", "Unit",multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                                                             #                         choices = c("g", "kg"))
                                                                                             #   )
                                                                                             # )
                                                                                             
                                                                                             textAreaInput("rmgt_residue_description_notes", label="Notes", value="")
                                                                                             
                                                                                      )
                                                                                    )
                                                                                ), 
                                                                                
                                                                                box(
                                                                                  id="residue_management_boxid",
                                                                                  title = checkboxInput("residueManag_checkbox", actionLink("residue_management_titleId", "Residue management"), T),
                                                                                  status = "primary",
                                                                                  solidHeader = TRUE,
                                                                                  width = 12, collapsible = TRUE,  collapsed = TRUE,
                                                                                  fluidRow(
                                                                                    column(width = 6,
                                                                                           fluidRow( ##IMPLEMENTAR EN EL EXCEL
                                                                                             column(width = 6,
                                                                                                    #dateInput("residue_start_date", label ="Start date", format = "yyyy-mm-dd")
                                                                                                    #airDatepickerInput("residue_start_date", "Start date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                                                                                    uiOutput("res_start_date")
                                                                                             )
                                                                                           ),
                                                                                           
                                                                                           selectizeInput("rmgt_residue_technique", label = "Technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                            c("Burning",
                                                                                                              "Incorporation",
                                                                                                              "Spreading",
                                                                                                              "Other"
                                                                                                            )
                                                                                           ),
                                                                                           hidden(textInput("rmgt_residue_technique_other", "", value = "")),
                                                                                           conditionalPanel("input.rmgt_residue_technique == 'Incorporation'", 
                                                                                                            fluidRow(
                                                                                                              column(6,textInput("rmgt_residue_inc_depth", "Residue incorporation depth", value="")),
                                                                                                              column(6, selectizeInput("rmgt_residue_inc_depth_unit", "Unit",multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                                                                                                       choices = c("cm", "ft", "in", "m"), selected = "cm")
                                                                                                              )
                                                                                                            )
                                                                                                            
                                                                                           ),
                                                                                           selectizeInput("rmgt_residue_traction", label = "Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                            c("Animal",
                                                                                                              "Manual",
                                                                                                              "2 wheel tractor",
                                                                                                              "4 wheel tractor",
                                                                                                              "Other"
                                                                                                            )
                                                                                           ),
                                                                                           hidden(textInput("rmgt_residue_traction_other", "", value = "")),
                                                                                           textAreaInput("rmgt_residue_management_notes", label="Notes", value="")
                                                                                    )
                                                                                  )
                                                                                  
                                                                                )
                                                                              )
                                                                       )),

                                                        tabPanel(div(id = "gh", "Seedbed preparation"), value="tabSeedbed",
                                                                 #fluidRow(
                                                                   column(width = 12,
                                                                 br(),
                                                                 #h2("Land preparation"),
                                                                 fluidRow(
                                                                   column(width = 12,
                                                                   p(tags$b(""))
                                                                   ),
                                                                 box(id="land_levelling_boxid",
                                                                     title = checkboxInput("landLevelling_checkbox" , actionLink("land_levelling_titleId", "Land Levelling"), F),
                                                                     #title = "Land Levelling",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,  collapsed = TRUE,


                                                                    fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     #dateInput("landLeveling_start_date", label ="Start date", format = "yyyy-mm-dd")
                                                                                     #airDatepickerInput("landLeveling_start_date", "Start date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                                                                     uiOutput("landLev_start_date")
                                                                              )
                                                                              # column(width = 6,
                                                                              #        #dateInput("landLeveling_end_date", label ="End date", format = "yyyy-dd-mm")
                                                                              #        #airDatepickerInput("landLeveling_end_date", "End date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                                                              #        uiOutput("landLev_end_date")
                                                                              # )
                                                                            ),
                                                                            #textInput("numPasses", value="", label = "Total number of levelling passes")#,
                                                                            numericInput("numPasses", label = "Total number of levelling passes", value="", min = 1, max = 20, step = 1),
                                                                            textAreaInput("landLeveling_notes", label="Notes", value="")
                                                                            #textInput("operationsOrder", value="", label = "Operations order")
                                                                          ),
                                                                     column(width = 6,
                                                                            br(),
                                                                        fluidRow(
                                                                         box(
                                                                           title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                           
                                                                           fluidRow(
                                                                             column(12,
                                                                                    h4("Implement", style="font-weight: 800;color: #555;")
                                                                             )
                                                                           ),

                                                                             selectizeInput("land_impl_type", label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."),
                                                                                                                        choices = c(
                                                                                                                                   "Chain harrow",
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
                                                                             selectizeInput("land_traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), label = "Traction", choices =
                                                                                                                                c("Animal",
                                                                                                                                  "Manual",
                                                                                                                                  "2 wheel tractor",
                                                                                                                                  "4 wheel tractor",
                                                                                                                                  "Other"
                                                                                                                                )
                                                                             ),
                                                                             hidden(textInput("land_traction_other", "", value = ""))
                                                                        ))
                                                                    ))
                                                                )),
                                                                fluidRow(
                                                                 box(id="puddling_boxid",
                                                                     title = checkboxInput("puddling_checkbox", actionLink("puddling_titleId", "Puddling"), F),
                                                                     solidHeader = TRUE,
                                                                     status = "primary",
                                                                     width = 12, collapsible = TRUE,  collapsed = TRUE,
                                                                     fluidRow(
                                                                        column(width = 6,
                                                                               fluidRow(
                                                                                  column(width = 6,
                                                                                         #dateInput("puddling_start_date", label ="Start date", format = "yyyy-mm-dd")
                                                                                         #airDatepickerInput("puddling_start_date", "Start date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                                                                         uiOutput("pud_start_date")
                                                                                  )
                                                                                  # column(width = 6,
                                                                                  #        #dateInput("puddling_end_date", label ="End date", format = "yyyy-mm-dd")
                                                                                  #        #airDatepickerInput("puddling_end_date", "End date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                                                                  #        uiOutput("pud_end_date")
                                                                                  # )
                                                                               )
                                                                         )

                                                                      ),

                                                                     fluidRow(
                                                                         column(width = 6,
                                                                            fluidRow(
                                                                             # box(
                                                                             #   title = "Method", solidHeader = TRUE, status = "warning", width=12,
                                                                             #          textInput("Penetrometer_in_field", value="", label = "Penetrometer in field"),
                                                                             #          fluidRow(
                                                                             #            column(width = 6,
                                                                             #                   #textInput("puddling_depth_val", label="Puddling depth", value="")
                                                                             #                   numericInput("puddling_depth_val", label = "Puddling depth", value="", min = 1, max = NA, step = 0.1)
                                                                             #            ),
                                                                             #            column(width = 6,##IMPLEMENTAR EN EL EXCEL
                                                                             #                  selectizeInput("puddling_depth_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("cm", "mm", "ft", "in"))
                                                                             #            )
                                                                             #          )
                                                                             # )

                                                                             ),
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     #textInput("puddling_depth_val", label="Puddling depth", value="")
                                                                                     numericInput("puddling_depth_val", label = "Puddling depth", value="", min = 1, step = 0.1)
                                                                              ),
                                                                              column(width = 6,##IMPLEMENTAR EN EL EXCEL
                                                                                     selectizeInput("puddling_depth_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("cm", "ft", "in", "m"), selected = "cm")
                                                                              )
                                                                            ),
                                                                            numericInput("puddling_total_number_puddling_passes", "Total number of puddling passes", value = "", min = 0, step = 1),
                                                                            textAreaInput("puddling_notes", label="Notes", value="")
                                                                         ),

                                                                         column(width = 6,
                                                                            fluidRow(
                                                                             box(
                                                                               title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                               fluidRow(
                                                                                 column(12,
                                                                                        h4("Implement", style="font-weight: 800;color: #555;")
                                                                                 )
                                                                               ),
                                                                               selectizeInput("pud_impl_type", label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."),
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
                                                                               selectizeInput("pud_traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), label = "Traction", choices =
                                                                                                c("Animal",
                                                                                                  "Manual",
                                                                                                  "2 wheel tractor",
                                                                                                  "4 wheel tractor",
                                                                                                  "Other"
                                                                                                )
                                                                               ),
                                                                               hidden(textInput("pud_traction_other", "", value = ""))
                                                                             )
                                                                          ))
                                                                     )


                                                                 )),
                                                                fluidRow(
                                                                 box(id="tillage_boxid",
                                                                     title = checkboxInput("tillage_checkbox", actionLink("tillage_titleId", "Tillage"), F),
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,  collapsed = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     #dateInput("tillage_start_date", label ="Start date", format = "yyyy-mm-dd")
                                                                                     #airDatepickerInput("tillage_start_date", "Start date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                                                                     uiOutput("till_start_date")
                                                                              )
                                                                              # column(width = 6,
                                                                              #        #dateInput("tillage_end_date", label ="End date", format = "yyyy-mm-dd")
                                                                              #        #airDatepickerInput("tillage_end_date", "End date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                                                              #        uiOutput("till_end_date")
                                                                              # )
                                                                            ),
                                                                            selectizeInput("till_technique",  label = "Technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                                        c("Conventional till",
                                                                                                                          "Deep till",
                                                                                                                          "No-till",
                                                                                                                          "Reduced till",
                                                                                                                          "Ridge-till",
                                                                                                                          "Strip-till",
                                                                                                                          "Other"
                                                                                                                        )
                                                                                          ),
                                                                            hidden(textInput("till_technique_other", "")),
                                                                            # textInput("till_depth_method", value="", label = "Depth of tillage - measurement method"),
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     #textInput("tillage_depth", value = "", label="Tillage depth")
                                                                                     numericInput("tillage_depth", "Tillage depth", value = "", min = 0, step = 0.1)
                                                                              ),
                                                                              column(width = 6,
                                                                                     selectizeInput("tillage_depth_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("cm", "ft", "in", "m"), selected = "cm")
                                                                              )

                                                                            ),
                                                                            #textInput("till_depth", value="", label = "Depth"),
                                                                            #textInput("total_number_tillage_passes", value="", label = "Total number of tillage passes")
                                                                            numericInput("total_number_tillage_passes", "Total number of tillage passes", value = "", min = 0, step = 1),
                                                                            textAreaInput("tillage_notes", label="Notes", value="")

                                                                     ),
                                                                     column(width = 6,
                                                                            br(),
                                                                            fluidRow(
                                                                         box(
                                                                           title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                           fluidRow(
                                                                             column(12,
                                                                                    h4("Implement", style="font-weight: 800;color: #555;")
                                                                             )
                                                                           ),
                                                                           selectizeInput("till_impl_type", label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("Chisel plough",
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
                                                                           selectizeInput("till_traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), label = "Traction", choices =
                                                                                            c("Animal",
                                                                                              "Manual",
                                                                                              "2 wheel tractor",
                                                                                              "4 wheel tractor",
                                                                                              "Other"
                                                                                            )
                                                                           ),
                                                                           hidden(textInput("till_traction_other", "", value = ""))
                                                                         ))
                                                                    )

                                                                  )
                                                                 ))#,
                                                                # fluidRow(
                                                                #  box(id="liming_boxid",
                                                                #      title = actionLink("liming_titleId", "Liming"),
                                                                #      status = "primary",
                                                                #      solidHeader = TRUE,
                                                                #      width = 12, collapsible = TRUE, collapsed = TRUE,
                                                                #      fluidRow(
                                                                #      column(width = 6,
                                                                #             fluidRow(
                                                                #               column(width = 6,
                                                                #                      dateInput("liming_start_date", label ="Start date", format = "yyyy/mm/dd")
                                                                #               ),
                                                                #               column(width = 6,
                                                                #                      dateInput("liming_end_date", label ="End date", format = "yyyy/mm/dd")
                                                                #               )
                                                                #             ),
                                                                #             textInput("lim_material", label="Material", value=""),
                                                                #             fluidRow(
                                                                #               column(width = 6,
                                                                #                      textInput("lim_quantity", value = "", label="Quantity")
                                                                #               ),
                                                                #               column(width = 6,
                                                                #                      selectizeInput("lim_quantity_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("kg/m2", "kg/ha", "t/ha"))
                                                                #               )
                                                                #
                                                                #             ),
                                                                #             textAreaInput("lim_description", value="", label = "Description")
                                                                #     ),
                                                                #     column(width = 6,
                                                                #            br(),
                                                                #            fluidRow(
                                                                #              box(
                                                                #                title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                #                selectizeInput("liming_impl_type", label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("Chisel plough",
                                                                #                                                                                                                                                         "Cultivator",
                                                                #                                                                                                                                                         "Disc plough",
                                                                #                                                                                                                                                         "Hand-held hoe",
                                                                #                                                                                                                                                         "Mouldboard/ridging plough",
                                                                #                                                                                                                                                         "Paraplow",
                                                                #                                                                                                                                                         "Spade plough",
                                                                #                                                                                                                                                         "Subsoiler",
                                                                #                                                                                                                                                         "Other")
                                                                #                ),
                                                                #                hidden(textInput("liming_impl_type_other", "", value = ""))
                                                                #
                                                                #              ))
                                                                #     )
                                                                #     )#,
                                                                #  ))


                                                          )),#),
                                                        tabPanel(div(id = "gh", "Soil fertility"), value="tabNutrient",
                                                                 br(),
                                                                 
                                                                 box(id="fertilizer_application_details_boxid",
                                                                     title = actionLink("fertilizer_application_details_titleId", "Fertilizer application details"),
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,  collapsed = FALSE,
                                                                     fluidRow(
                                                                       column(3,
                                                                              numericInput("soil_fertilizer_num_apps", "Number of applications", min=1, max = 10, value=3)
                                                                       )
                                                                     ),
                                                                     fluidRow(
                                                                       column(12,
                                                                              column(1, style="padding:5px; text-align:center; word-wrap: break-word;  width: 5.33333333%;", h4("# app")),
                                                                              column(10, style="padding:0px;  width: 93.33333333%;",
                                                                                     column(6,style="padding:0px;",
                                                                                            column(3, style="padding:5px; text-align:center; word-wrap: break-word;", h4("Fertilizer type") ),
                                                                                            column(3, style="padding:5px; text-align:center; word-wrap: break-word;", h4("Product")),
                                                                                            column(2, style="padding:5px; text-align:center; word-wrap: break-word;", h4("Product rate (kg/ha)")),
                                                                                            column(4, style="padding:5px; text-align:center; word-wrap: break-word;", h4("Element"))
                                                                                     ),
                                                                                     column(6, style="padding:0px;",
                                                                                            column(2, style="padding:5px; text-align:center; word-wrap: break-word;", h4("Element rate (kg/ha)") ),
                                                                                            column(3, style="padding:5px; text-align:center; word-wrap: break-word;",h4("Start date")),
                                                                                            column(3, style="padding:5px; text-align:center; word-wrap: break-word;",h4("End date")),
                                                                                            # column(3, style="padding:5px; text-align:center; word-wrap: break-word;",h4("Implement")),
                                                                                            # column(3, style="padding:5px; text-align:center; word-wrap: break-word;",h4("Traction")),
                                                                                            column(4, style="padding:5px; text-align:center; word-wrap: break-word;",h4("Technique"))
                                                                                     )
                                                                              ),
                                                                              column(1,  style="padding:5px; text-align:center; word-wrap: break-word; display: none;",
                                                                                     h4("Notes")
                                                                              )
                                                                       )
                                                                     ),
                                                                     fluidRow(id="fr_fertilizer_application"),
                                                                     fluidRow(
                                                                       column(12,
                                                                              column(1, style="padding:3px; text-align:center;"),
                                                                              column(10, style="padding:0px; display: none;",
                                                                                     column(6, style="padding:0px;",
                                                                                            column(3, style="padding:5px;"
                                                                                            ),
                                                                                            column(5, style="padding:5px;",
                                                                                                   br(),HTML("<div style='text-align:center;'>"), h5("Total calculated application:"), HTML("</div>")
                                                                                            ),
                                                                                            column(2, style="padding:5px;",
                                                                                                   textInput("soil_fertilizer_totalAppRate1", "")
                                                                                            ),
                                                                                            column(2, style="padding:5px;"
                                                                                            )
                                                                                     ),
                                                                                     column(6,style="padding:0px;",
                                                                                            column(2, style="padding:5px;",
                                                                                                   textInput("soil_fertilizer_totalAppRate2", "")
                                                                                            ),
                                                                                            column(3, style="padding:5px;"
                                                                                            ),
                                                                                            column(3, style="padding:5px"
                                                                                            ),
                                                                                            column(4, style="padding:5px;"
                                                                                            )
                                                                                     )
                                                                              ),
                                                                              column(1,  style="padding:3px;"
                                                                              )
                                                                       )
                                                                       
                                                                     )#,
                                                                     # fluidRow(
                                                                     #   column(4,br(),
                                                                     #          #textInput("soil_fertilizer_totalAppRate", "Total application rate"),
                                                                     #          textInput("soil_fertilizer_fractionTotalAppRate", "Fraction of total application rate")
                                                                     #   )
                                                                     # ),
                                                                     
                                                                     #### Ocultado por ivan
                                                                     # fluidRow(
                                                                     #
                                                                     #     column(12,
                                                                     #            column(6,
                                                                     #              h2("Nutrient content")
                                                                     #            ),
                                                                     #            column(6,
                                                                     #                   br(),
                                                                     #                actionButton("addproducts_soil", "", icon= icon("plus")),
                                                                     #                actionButton("delproducts_soil", "", icon= icon("minus"))
                                                                     #            )
                                                                     #     )
                                                                     #   ),
                                                                     #   fluidRow(
                                                                     #     column(12,
                                                                     #         column(3, style = "padding:10px;",
                                                                     #                HTML("<center>"), h3("Fertilizer product"), HTML("</center>")
                                                                     #         ),
                                                                     #         column(1, style = "padding:10px;",
                                                                     #                HTML("<center>"),h3("N (%)"),HTML("</center>")
                                                                     #         ),
                                                                     #         column(1, style = "padding:10px;",
                                                                     #                HTML("<center>"),h3("P (%)"),HTML("</center>")
                                                                     #         ),
                                                                     #         column(1, style = "padding:10px;",
                                                                     #                HTML("<center>"),h3("K (%)"),HTML("</center>")
                                                                     #         ),
                                                                     #         fluidRow(id="fr_fertilizer_application_nutrient_title")
                                                                     #     )
                                                                     #   ),
                                                                     # fluidRow(
                                                                     #   column(12,
                                                                     #          column(3, style = "padding:10px;",
                                                                     #                 fluidRow(id = "fr_aux_soil_fertProduct")
                                                                     #          ),
                                                                     #          column(1, style = "padding:10px;",
                                                                     #                 fluidRow(id = "fr_aux_soil_N")
                                                                     #          ),
                                                                     #          column(1, style = "padding:10px;",
                                                                     #                 fluidRow(id = "fr_aux_soil_P")
                                                                     #          ),
                                                                     #          column(1, style = "padding:10px;",
                                                                     #                 fluidRow(id = "fr_aux_soil_K")
                                                                     #          ),
                                                                     #          fluidRow(id="fr_fertilizer_application_nutrient")
                                                                     #   )
                                                                     # )
                                                                     
                                                                     #### Fin Ocultado por ivan
                                                                     
                                                                     
                                                                 )
                                                        ),#)#end tab nutrient management event
                                                        tabPanel(div(id = "gh", "Planting and transplanting"), value="tabPlanting",
                                                                 #br(),
                                                                 #fluidRow(
                                                                 column(width = 12,
                                                                        br(),
                                                                        column(width = 12,
                                                                               p(tags$b(""))
                                                                        ),
                                                                        fluidRow(id = "fr_plantingTransplating_reference_point")
                                                                        #h2("Planting, seeding and transplanting"),
                                                                        
                                                                 )
                                                        ),#),#end tab planting
                                                        tabPanel(div(id = "gh", "Mulch management"), value="tabMulching",
                                                                 #fluidRow(
                                                                   column(width = 12,
                                                                          br(),
                                                                          #h2("Mulching and residue management"),
                                                                 #br(),
                                                                 fluidRow(
                                                                   column(width = 12,
                                                                          p(tags$b(""))
                                                                   ),
                                                                 box(id="mulch_management_boxid",
                                                                     #title = checkboxInput("mulchManag_checkbox", actionLink("mulch_management_titleId", "Mulching details"), T),
                                                                     title= "Mulching details",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE, collapsed = FALSE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     #dateInput("mulch_start_date", label ="Mulching start date", format = "yyyy-mm-dd")
                                                                                     #airDatepickerInput("mulch_start_date", "Mulching start date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                                                                     uiOutput("mul_start_date")
                                                                              )
                                                                              # column(width = 6,
                                                                              #        #dateInput("mulch_end_date", label ="Mulching end date", format = "yyyy-mm-dd")
                                                                              #        #airDatepickerInput("mulch_end_date", "Mulching end date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                                                              #        uiOutput("mul_end_date")
                                                                              # )
                                                                            ),
                                                                            selectizeInput("mumd_mulch_type", label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."),
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
                                                                              column(width = 6,
                                                                                numericInput("mumd_mulch_thickness", value="", label = "Mulch thickness", min=0, max=100, step=0.1)
                                                                              ),
                                                                              column(width = 6,
                                                                                selectizeInput("mumd_mulch_thickness_unit","Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                                                               choices = c("cm","ft", "in", "m"), selected = "cm")
                                                                              )
                                                                            ),
                                                                            fluidRow(
                                                                              column(width = 6,

                                                                                  numericInput("mumd_mulch_amountPerSq", value="", label = "Mulch amount", min=0, max=100, step = 0.1)
                                                                              ),
                                                                              column(width = 6,
                                                                                selectizeInput("mumd_mulch_amountPerSq_unit", "Unit",multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                                                               choices = c("g/ft2", "g/m2","kg/ha", "kg/m2", "lb/ac"), selected = "kg/ha")
                                                                              )
                                                                            ),
                                                                            # selectizeInput("mulch_color", label = "Mulch color", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("Black",
                                                                            #                                                               "Brown",
                                                                            #                                                               "Gray",
                                                                            #                                                               "Transparent",
                                                                            #                                                               "White",
                                                                            #                                                               "Yellow")
                                                                            # ),
                                                                            textInput("mumd_mulch_color","Mulch color"),
                                                                            fluidRow(
                                                                              column(6,
                                                                                     textInput("mumd_mulch_percCoverage", value="", label = "Percentage of coverage")
                                                                                     ),
                                                                              column(6,
                                                                                     selectInput("mumd_mulch_percCoverage_unit", "Unit", c("%"), selected= "%")
                                                                                     )
                                                                            ),
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     #dateInput("mulch_remove_start_date", label ="Mulch removal start date", format = "yyyy-mm-dd")
                                                                                     #airDatepickerInput("mulch_remove_start_date", "Mulch removal start date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                                                                     uiOutput("mulre_start_date")
                                                                              ),
                                                                              column(width = 6,
                                                                                     #dateInput("mulch_remove_end_date", label ="Mulch removal end date", format = "yyyy-mm-dd")
                                                                                     #airDatepickerInput("mulch_remove_end_date", "Mulch removal end date", placeholder = "yyyy-mm-dd", clearButton = T, autoClose = T)
                                                                                     uiOutput("mulre_end_date")
                                                                              )
                                                                            ),
                                                                            textAreaInput("mumd_mulching_management_notes", label="Notes", value="")
                                                                          ),
                                                                     column(width = 6,
                                                                            br(),
                                                                      fluidRow(
                                                                       box(
                                                                         title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                         fluidRow(
                                                                           column(12,
                                                                                  h4("Implement", style="font-weight: 800;color: #555;")
                                                                           )
                                                                         ),
                                                                         # textInput("mulch_make", value="", label = "Implement make"),
                                                                         # textInput("mulch_model", value="", label = "Implement model"),
                                                                               selectizeInput("mumd_mulch_implement_type", label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                c("Manual",
                                                                                                  "Mechanized"
                                                                                                )
                                                                               ),
                                                                                selectizeInput("mumd_mulch_traction", label = "Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                              c("Animal",
                                                                                                "Manual",
                                                                                                "2 wheel tractor",
                                                                                                "4 wheel tractor",
                                                                                                "Other"
                                                                                              )
                                                                                ),
                                                                                 hidden(textInput("mumd_mulch_traction_other", "", value = ""))
                                                                       ))
                                                                   ))


                                                                 ))
                                                        )),#),#end tab mulching
                                                        
                                                        tabPanel(div(id = "gh", "Irrigation"), value="tabIrrigation",
                                                                 #br(),
                                                                 #fluidRow(
                                                                 column(width = 12,
                                                                        br(),
                                                                        #h2("Irrigation"),
                                                                        fluidRow(
                                                                          box(id="irrigation_desc_boxid",
                                                                              title = actionLink("irrigation_desc_titleId", "Irrigation details"),
                                                                              status = "primary",
                                                                              solidHeader = TRUE,
                                                                              width = 12, collapsible = TRUE, collapsed = FALSE,
                                                                              fluidPage(id="fr_irrigation_boxes"), 
                                                                              #column(12,actionButton("addIrrigation", "Add irrigation"))
                                                                              actionButton("addIrrigation", "Add irrigation")
                                                                          ))#end box description irrigation
                                                                 )),#),#end tab irrigation


                                                        tabPanel(div(id = "gh", "Weeding"), value="tabWeeding", br(),
                                                           box(id="weeding_boxid",
                                                               title = actionLink("weeding_titleId", "Weeding details"),
                                                               status = "primary",
                                                               solidHeader = TRUE,
                                                               width = 12, collapsible = TRUE,  collapsed = FALSE,

                                                               fluidPage(id="fr_weeding_boxes"),
                                                               #column(12,actionButton("addWeeding", "Add weeding"))
                                                               actionButton("addWeeding", "Add weeding")
                                                          )
                                                        ),#),#end tab weeding
                                                        tabPanel(div(id = "gh", "Harvest"), value="tabHarvest",
                                                                 #br(),
                                                                 #fluidRow(
                                                                 column(width = 12,
                                                                        br(),
                                                                        #h2("Harvest"),
                                                                        fluidRow(id="fr_harvest_reference_point"),
                                                                        actionButton("addHarvest", "Add Harvest")
                                                                        
                                                                 ))#),#end tab harvest




                                                        # tabPanel("Biofertilizer", value="tabBiofertilizer",
                                                        #          #br(),
                                                        #          #fluidRow(
                                                        #            column(width = 12,
                                                        #                   #br(),
                                                        #                   h2("Biofertilizer"),
                                                        #                   fluidRow(
                                                        #          box(id="desc_biofertilizer_boxid",
                                                        #              title = actionLink("desc_biofertilizer_titleId", "Description Biofertilizer"),
                                                        #              status = "primary",
                                                        #              solidHeader = TRUE,
                                                        #              width = 12, collapsible = TRUE, collapsed = TRUE,
                                                        #              fluidRow(id = "bio_description",
                                                        #                column(width = 6,
                                                        #                       numericInput("numApplicationsBiofert", label  = "Number of applications", value = 1, min = 1, max = 5)
                                                        #
                                                        #                )
                                                        #              )
                                                        #          ))#end box description biofertilizer
                                                        # )),#),#end tab biofertilizer
                                                      #   tabPanel("Pest observation and control", value="tabPestNDisease",
                                                      #
                                                      #       column(width = 12,
                                                      #           h2("Pest observation and control"),
                                                      #            box(id="pest_control_boxid",
                                                      #                title = actionLink("pest_control_titleId", "Pest control"),
                                                      #                status = "primary",
                                                      #                solidHeader = TRUE,
                                                      #                width = 12, collapsible = TRUE, collapsed = TRUE,
                                                      #
                                                      #                fluidRow(id ="pestNDisease_fluid",
                                                      #                  column(width = 6,
                                                      #                         numericInput("numApplicationsPestDisease", label  = "Number of applications", value = 1, min = 1, max = 5)
                                                      #                 )
                                                      #
                                                      #               )
                                                      #             )#end box pest control
                                                      #   )
                                                      # ),#end tab pest&disease


                                                      )#end tabbox
                                                      #)
                                                    ),#asd

                                                    #),
                                                    #fluidRow(
                                                      sidebarPanel(id="sidebar", width = 12,
                                                                   actionButton("btnNextCropPheno", "Next", class = "btn-primary",style="color: #fff;", href="#top")
                                                      )
                                                    #)



                                             ),

                                         # shiny::tabPanel("Crop phenology", value = "tabCropPheno", icon = shiny::icon("envira"),
                                         #                 column(
                                         #                   12,
                                         #                   h2("Crop phenology"),
                                         #                   br()
                                         #                 ),
                                         # 
                                         # 
                                         #                 box( title ="Planting, transplanting", solidHeader = TRUE, status = "warning", width=12,
                                         #                      fluidRow(
                                         #                        column(8,
                                         #                               fluidRow(
                                         #                                 column(4,
                                         #                                        dateInput("cropPheno_planting_date", label ="Planting date", format = "yyyy-mm-dd", value = NA)
                                         #                                 ),
                                         #                                 column(4,
                                         #                                        dateInput("cropPheno_transpanting_date", label ="Transplanting date", format = "yyyy-mm-dd", value = NA)
                                         #                                 ),
                                         #                                 column(4,
                                         #                                        dateInput("cropPhen_plantingEmergence_date", label ="Emergence date", format = "yyyy-mm-dd", value = NA)
                                         #                                 )
                                         #                               )
                                         # 
                                         # 
                                         #                        ),
                                         # 
                                         #                        column(4, textAreaInput("cropPheno_sowEmerg_notes", "Notes", width = '100%'))
                                         #                      )
                                         # 
                                         #                 ),
                                         # 
                                         #                 box( title ="Sowing and emergence", solidHeader = TRUE, status = "warning", width=12,
                                         #                      fluidRow(
                                         #                        column(8,
                                         #                            fluidRow(
                                         #                               column(6,
                                         #                                      dateInput("cropPheno_sowing_date", label ="Sowing date", format = "yyyy-mm-dd", value = NA)
                                         #                               ),
                                         #                               column(6,
                                         #                                      dateInput("cropPheno_emergence_date", label ="Emergence date", format = "yyyy-mm-dd", value = NA)
                                         #                               )
                                         #                            )
                                         # 
                                         #                        ),
                                         # 
                                         #                        column(4, textAreaInput("cropPheno_sowEmerg_notes", "Notes", width = '100%'))
                                         #                      )
                                         # 
                                         #                  ),
                                         # 
                                         #                 box( title ="Flowering", solidHeader = TRUE, status = "warning", width=12,
                                         #                      fluidRow(
                                         #                        column(8,
                                         #                               fluidRow(
                                         #                                 column(4,
                                         #                                        dateInput("cropPheno_flowering_sdate", label ="Start date", format = "yyyy-mm-dd", value = NA)
                                         #                                 ),
                                         #                                 column(4,
                                         #                                        dateInput("cropPheno_flowering_50date", label ="50% flowering date", format = "yyyy-mm-dd", value = NA)
                                         #                                 ),
                                         #                                 column(4,
                                         #                                        dateInput("cropPheno_flowering_edate", label ="End date", format = "yyyy-mm-dd", value = NA)
                                         #                                 )
                                         #                               )
                                         # 
                                         #                        ),
                                         # 
                                         #                        column(4, textAreaInput("cropPheno_flowering_notes", "Notes", width = '100%'))
                                         #                      )
                                         # 
                                         #                 ),
                                         # 
                                         #                 box( title ="Grain filling", solidHeader = TRUE, status = "warning", width=12,
                                         #                      fluidRow(
                                         #                        column(8,
                                         #                               fluidRow(
                                         #                                 column(6,
                                         #                                        dateInput("cropPheno_grainFilling_sdate", label ="Start date", format = "yyyy-mm-dd", value = NA)
                                         #                                 ),
                                         #                                 column(6,
                                         #                                        dateInput("cropPheno_grainFilling_edate", label ="End date", format = "yyyy-mm-dd", value = NA)
                                         #                                 )
                                         #                               )
                                         # 
                                         #                        ),
                                         # 
                                         #                        column(4, textAreaInput("cropPheno_grainFilling_notes", "Notes"))
                                         #                      )
                                         # 
                                         #                 ),
                                         # 
                                         #                 box( title ="Fruit development", solidHeader = TRUE, status = "warning", width=12,
                                         #                      fluidRow(
                                         #                        column(8,
                                         #                               fluidRow(
                                         #                                 column(3,
                                         #                                        dateInput("cropPheno_fruitDev_date", label ="Fruit development date", format = "yyyy-mm-dd", value = NA)
                                         #                                 ),
                                         #                                 column(3,
                                         #                                        dateInput("cropPheno_fruit50dev_date", label ="50% fruit development date", format = "yyyy-mm-dd", value = NA)
                                         #                                 ),
                                         #                                 column(3,
                                         #                                        dateInput("cropPheno_fruitDevEnd_date", label ="Fruit development end date", format = "yyyy-mm-dd", value = NA)
                                         #                                 ),
                                         #                                 column(3,
                                         #                                        dateInput("cropPheno_fruitRip_date", label ="Fruit ripening date", format = "yyyy-mm-dd", value = NA)
                                         #                                 )
                                         #                               )
                                         # 
                                         #                        ),
                                         # 
                                         #                        column(4, textAreaInput("cropPheno_fruitDev_notes", "Notes"))
                                         #                      )
                                         # 
                                         #                 ),
                                         # 
                                         #                 box( title ="Maturity and senescence", solidHeader = TRUE, status = "warning", width=12,
                                         #                      fluidRow(
                                         #                        column(8,
                                         #                               fluidRow(
                                         #                                 column(4,
                                         #                                        dateInput("cropPheno_maturity_start_date", label ="Maturity start date", format = "yyyy-mm-dd", value = NA)
                                         #                                 ),
                                         #                                 column(4,
                                         #                                        dateInput("cropPheno_maturity_50_date", label ="50% maturity date", format = "yyyy-mm-dd", value = NA)
                                         #                                 ),
                                         #                                 column(4,
                                         #                                        dateInput("cropPheno_maturity_end_date", label ="Maturity end date", format = "yyyy-mm-dd", value = NA)
                                         #                                 ),
                                         #                                 column(4,
                                         #                                        dateInput("cropPheno_senescence_start_date", label ="Senescence start date", format = "yyyy-mm-dd", value = NA)
                                         #                                 ),
                                         #                                 column(4,
                                         #                                        dateInput("cropPheno_senescence_50_date", label ="50% senescence date", format = "yyyy-mm-dd", value = NA)
                                         #                                 ),
                                         #                                 column(4,
                                         #                                        dateInput("cropPheno_senescence_end_date", label ="Senescence end date", format = "yyyy-mm-dd", value = NA)
                                         #                                 )
                                         #                               )
                                         # 
                                         #                        ),
                                         # 
                                         #                        column(4, textAreaInput("cropPheno_maturity_notes", "Notes"))
                                         #                      )
                                         # 
                                         #                 ),
                                         #                 box( title ="Other phenological stage", solidHeader = TRUE, status = "warning", width=12,
                                         #                      fluidRow(
                                         #                        column(8,
                                         #                               fluidRow(
                                         #                                 column(4,
                                         #                                        textInput("cropPheno_otherPheno_name", "Name")
                                         #                                 ),
                                         #                                 column(4,
                                         #                                        dateInput("cropPheno_otherPheno_start_date", label ="Start date", format = "yyyy-mm-dd", value = NA)
                                         #                                 ),
                                         #                                 column(4,
                                         #                                        dateInput("cropPheno_otherPheno_end_date", label ="End date", format = "yyyy-mm-dd", value = NA)
                                         #                                 )
                                         # 
                                         #                               )
                                         # 
                                         #                        ),
                                         # 
                                         #                        column(4, textAreaInput("cropPheno_otherPheno_notes", "Notes"))
                                         #                      )
                                         # 
                                         #                 ),
                                         # 
                                         #                 sidebarPanel(id="sidebar", width = 12,
                                         #                              actionButton("btnNextTraits", "Next", class = "btn-primary",style="color: #fff;", href="#top")
                                         #                 )
                                         # 
                                         #      ),

                                              # shiny::tabPanel("Crop measurement", value = "tabTraits", icon = shiny::icon("leaf"),
                                              #
                                              #       column(width = 12,
                                              #              h2("Crop measurement"),
                                              #              br()
                                              #
                                              #     ),
                                              #     uiOutput("uiTraitsList"),
                                              #
                                              #       sidebarPanel(id="sidebar", width = 12,
                                              #                    actionButton("btnNextEnv", "Next", class = "btn-primary",style="color: #fff;", href="#top")
                                              #       )
                                              #     #)
                                              # ),
                                              # shiny::tabPanel("Crop measurement", value = "tabTraits", icon = shiny::icon("leaf"),
                                              #
                                              #                 column(width = 12,
                                              #                        h2("Crop measurement")
                                              #                        # actionButton("btGetCheckedValues", "Get cheked")
                                              #
                                              #                 ),
                                              #                 uiOutput("uiTraitsList"),
                                              #
                                              #                 sidebarPanel(id="sidebar", width = 12,
                                              #                              actionButton("btnNextEnv", "Next", class = "btn-primary",style="color: #fff;", href="#top")
                                              #                 )
                                              #                 #)
                                              # ),
                                              # shiny::tabPanel("Crop measurement2", value = "tabTraits2", icon = shiny::icon("leaf"),
                                              #
                                              #                 column(width = 12,
                                              #                        h2("Crop measurement2"),
                                              #                        #br()
                                              #                        p(class = "text-muted", style="text-align:justify",
                                              #                          paste("Please, select measurement by click.")
                                              #                        )
                                              #
                                              #                 ),
                                              #                 uiOutput("uiTraitsList2"),
                                              #
                                              #                 sidebarPanel(id="sidebar", width = 12,
                                              #                              actionButton("btnNextEnv", "Next", class = "btn-primary",style="color: #fff;", href="#top")
                                              #                 )
                                              #                 #)
                                              # ),
          
                                              # shiny::tabPanel("Crop measurement", value = "tabTraits", icon = shiny::icon("leaf"),
                                              # 
                                              #                 column(width = 12,
                                              #                        h2("Crop measurement"),
                                              #                        #br()
                                              #                        p(class = "text-muted", style="text-align:justify",
                                              #                          paste("Please, select measurement by click.")
                                              #                        ),
                                              #                        column(12, align = "center", checkboxInput("dt_sel", "Select all"))
                                              #                        # br(),
                                              #                        # h4("selected_rows:"),
                                              #                        # verbatimTextOutput("selected_rows", TRUE)
                                              # 
                                              #                 ),
                                              # 
                                              #                 uiOutput("uiTraitsList3"),
                                              # 
                                              #                 #DT::DTOutput("dt"),
                                              # 
                                              #                 sidebarPanel(id="sidebar", width = 12,
                                              #                              actionButton("btnNextEnv", "Next", class = "btn-primary",style="color: #fff;", href="#top")
                                              #                 )
                                              #                 #)
                                              # )


                                              # shiny::tabPanel("Crop Phenology", value = 'tabPhenology', icon = shiny::icon("bolt"),
                                              #     #br(),
                                              #     # h2("Weather & Soil"),
                                              #     #fluidRow(
                                              #       column(width = 12,
                                              #     #         h2("Weather & Soil"),
                                              #     #shinyTree::shinyTree("designFieldbook_weatherVar_agrofims",search = TRUE,checkbox = TRUE),
                                              #     #shinyTree::shinyTree("designFieldbook_soilVar_agrofims",search = TRUE,checkbox = TRUE)
                                              #     
                                              #     DTOutput('phenoDT')
                                              #     )
                                              # ),
          
                                              # nueva version:
                                              shiny::tabPanel("Crop Measurement", value = 'crop_measurement_mono', icon = shiny::icon("leaf"),
                                                              column(width = 12,
                                                                     h2("Crop measurement"),
                                                                     p(class = "text-muted", style="text-align:justify",
                                                                       paste("Please, select measurement by click.")
                                                                     ),
                                                                     uiOutput("uiCropMeaMono")
                                                              )
                                              ),

                                              shiny::tabPanel("Crop Measurement", value = 'crop_measurement_inter', icon = shiny::icon("leaf"),
                                                              column(width = 12,
                                                                     h2("Crop measurement"),
                                                                     #actionButton("qwe", "dale"),
                                                                     p(class = "text-muted", style="text-align:justify",
                                                                       paste("Please, select measurement by click.")
                                                                     ),
                                                                     #uiOutput("uiCropMeaInter"),
                                                                     tabsetPanel(
                                                                       id = "tabpanelinter",
                                                                       #selected = 1,
                                                                       shiny::tabPanel("Cassava Measurement", value = "crop_measurement_Cassava",
                                                                                       br(),
                                                                                       DTOutput("tblInterCassava")
                                                                       ),
                                                                       shiny::tabPanel("Common bean Measurement", value = "crop_measurement_Commonbean",
                                                                                       br(),
                                                                                       DTOutput("tblInterCommon")
                                                                       ),
                                                                       shiny::tabPanel("Maize Measurement", value = "crop_measurement_Maize",
                                                                                       br(),
                                                                                       DTOutput("tblInterMaize")
                                                                       ),
                                                                       shiny::tabPanel("Potato Measurement", value = "crop_measurement_Potato",
                                                                                       br(),
                                                                                       DTOutput("tblInterPotato")
                                                                       ),
                                                                       shiny::tabPanel("Rice Measurement", value = "crop_measurement_Rice",
                                                                                       br(),
                                                                                       DTOutput("tblInterRice")
                                                                       ),
                                                                       shiny::tabPanel("Sweetpotato Measurement", value = "crop_measurement_Sweetpotato",
                                                                                       br(),
                                                                                       DTOutput("tblInterSweetpotato")
                                                                       ),
                                                                       shiny::tabPanel("Wheat Measurement", value = "crop_measurement_Wheat",
                                                                                       br(),
                                                                                       DTOutput("tblInterWheat")
                                                                       ),
                                                                       shiny::tabPanel("Other 1 Measurement", value = "crop_measurement_Other_1",
                                                                                       br(),
                                                                                       actionButton("do", "do"),
                                                                                       DTOutput("tblInterOther1")
                                                                       ),
                                                                       shiny::tabPanel("Other 2 Measurement", value = "crop_measurement_Other_2",
                                                                                       br(),
                                                                                       DTOutput("tblInterOther2")
                                                                       ),
                                                                       shiny::tabPanel("Other 3 Measurement", value = "crop_measurement_Other_3",
                                                                                       br(),
                                                                                       DTOutput("tblInterOther3")
                                                                       ),
                                                                       shiny::tabPanel("Other 4 Measurement", value = "crop_measurement_Other_4",
                                                                                       br(),
                                                                                       DTOutput("tblInterOther4")
                                                                       ),
                                                                       shiny::tabPanel("Other 5 Measurement", value = "crop_measurement_Other_5",
                                                                                       br(),
                                                                                       DTOutput("tblInterOther5")
                                                                       ),
                                                                       shiny::tabPanel("Other 6 Measurement", value = "crop_measurement_Other_6",
                                                                                       br(),
                                                                                       DTOutput("tblInterOther6")
                                                                       ),
                                                                       shiny::tabPanel("Other 7 Measurement", value = "crop_measurement_Other_7",
                                                                                       br(),
                                                                                       DTOutput("tblInterOther7")
                                                                       ),
                                                                       shiny::tabPanel("Other 8 Measurement", value = "crop_measurement_Other_8",
                                                                                       br(),
                                                                                       DTOutput("tblInterOther8")
                                                                       ),
                                                                       shiny::tabPanel("Other 9 Measurement", value = "crop_measurement_Other_9",
                                                                                       br(),
                                                                                       DTOutput("tblInterOther9")
                                                                       ),
                                                                       shiny::tabPanel("Other 10 Measurement", value = "crop_measurement_Other_10",
                                                                                       br(),
                                                                                       DTOutput("tblInterOther10")
                                                                       )
                                                                     )
                                                              )
                                              ),
          
                                              shiny::tabPanel("Crop Measurement", value = 'crop_measurement_relay', icon = shiny::icon("leaf"),
                                                              column(width = 12,
                                                                     h2("Crop measurement"),
                                                                     #actionButton("qwe", "dale"),
                                                                     p(class = "text-muted", style="text-align:justify",
                                                                       paste("Please, select measurement by click.")
                                                                     ),
                                                                     #uiOutput("uiCropMeaInter"),
                                                                     tabsetPanel(
                                                                       id = "tabpanelrelay",
                                                                       #selected = 1,
                                                                       shiny::tabPanel("Cassava Measurement", value = "crop_measurement_relay_Cassava",
                                                                                       br(),
                                                                                       DTOutput("tblRelayCassava")
                                                                       ),
                                                                       shiny::tabPanel("Common bean Measurement", value = "crop_measurement_relay_Commonbean",
                                                                                       br(),
                                                                                       DTOutput("tblRelayCommon")
                                                                       ),
                                                                       shiny::tabPanel("Maize Measurement", value = "crop_measurement_relay_Maize",
                                                                                       br(),
                                                                                       DTOutput("tblRelayMaize")
                                                                       ),
                                                                       shiny::tabPanel("Potato Measurement", value = "crop_measurement_relay_Potato",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPotato")
                                                                       ),
                                                                       shiny::tabPanel("Rice Measurement", value = "crop_measurement_relay_Rice",
                                                                                       br(),
                                                                                       DTOutput("tblRelayRice")
                                                                       ),
                                                                       shiny::tabPanel("Sweetpotato Measurement", value = "crop_measurement_relay_Sweetpotato",
                                                                                       br(),
                                                                                       DTOutput("tblRelaySweetpotato")
                                                                       ),
                                                                       shiny::tabPanel("Wheat Measurement", value = "crop_measurement_relay_Wheat",
                                                                                       br(),
                                                                                       DTOutput("tblRelayWheat")
                                                                       ),
                                                                       shiny::tabPanel("Other 1 Measurement", value = "crop_measurement_relay_Other_1",
                                                                                       br(),
                                                                                       actionButton("doRelay", "do"),
                                                                                       DTOutput("tblRelayOther1")
                                                                       ),
                                                                       shiny::tabPanel("Other 2 Measurement", value = "crop_measurement_relay_Other_2",
                                                                                       br(),
                                                                                       DTOutput("tblRelayOther2")
                                                                       ),
                                                                       shiny::tabPanel("Other 3 Measurement", value = "crop_measurement_relay_Other_3",
                                                                                       br(),
                                                                                       DTOutput("tblRelayOther3")
                                                                       ),
                                                                       shiny::tabPanel("Other 4 Measurement", value = "crop_measurement_relay_Other_4",
                                                                                       br(),
                                                                                       DTOutput("tblRelayOther4")
                                                                       ),
                                                                       shiny::tabPanel("Other 5 Measurement", value = "crop_measurement_relay_Other_5",
                                                                                       br(),
                                                                                       DTOutput("tblRelayOther5")
                                                                       ),
                                                                       shiny::tabPanel("Other 6 Measurement", value = "crop_measurement_relay_Other_6",
                                                                                       br(),
                                                                                       DTOutput("tblRelayOther6")
                                                                       ),
                                                                       shiny::tabPanel("Other 7 Measurement", value = "crop_measurement_relay_Other_7",
                                                                                       br(),
                                                                                       DTOutput("tblRelayOther7")
                                                                       ),
                                                                       shiny::tabPanel("Other 8 Measurement", value = "crop_measurement_relay_Other_8",
                                                                                       br(),
                                                                                       DTOutput("tblRelayOther8")
                                                                       ),
                                                                       shiny::tabPanel("Other 9 Measurement", value = "crop_measurement_relay_Other_9",
                                                                                       br(),
                                                                                       DTOutput("tblRelayOther9")
                                                                       ),
                                                                       shiny::tabPanel("Other 10 Measurement", value = "crop_measurement_relay_Other_10",
                                                                                       br(),
                                                                                       DTOutput("tblRelayOther10")
                                                                       )
                                                                     )
                                                              )
                                              ),

                                              shiny::tabPanel("Crop Phenology", value = 'crop_phenology_mono', icon = shiny::icon("leaf"),
                                                              column(width = 12,
                                                                     h2("Crop phenology"),
                                                                     p(class = "text-muted", style="text-align:justify",
                                                                       paste("Please, select phenology by click.")
                                                                     ),
                                                                     uiOutput("uiCropPheMono")
                                                              )
                                              ),

                                              shiny::tabPanel("Crop Phenology", value = 'crop_phenology_inter', icon = shiny::icon("leaf"),
                                                              column(width = 12,
                                                                     h2("Crop phenology"),
                                                                     #actionButton("qwe", "dale"),
                                                                     p(class = "text-muted", style="text-align:justify",
                                                                       paste("Please, select phenology by click.")
                                                                     ),
                                                                     #uiOutput("uiCropMeaInter"),
                                                                     tabsetPanel(
                                                                       id = "tabpanelinterphe",
                                                                       #selected = 1,
                                                                       shiny::tabPanel("Cassava Phenology", value = "crop_phenology_Cassava",
                                                                                       br(),
                                                                                       DTOutput("tblInterPheCassava")
                                                                       ),
                                                                       shiny::tabPanel("Common bean Phenology", value = "crop_phenology_Commonbean",
                                                                                       br(),
                                                                                       DTOutput("tblInterPheCommon")
                                                                       ),
                                                                       shiny::tabPanel("Maize Phenology", value = "crop_phenology_Maize",
                                                                                       br(),
                                                                                       DTOutput("tblInterPheMaize")
                                                                       ),
                                                                       shiny::tabPanel("Potato Phenology", value = "crop_phenology_Potato",
                                                                                       br(),
                                                                                       DTOutput("tblInterPhePotato")
                                                                       ),
                                                                       shiny::tabPanel("Rice Phenology", value = "crop_phenology_Rice",
                                                                                       br(),
                                                                                       DTOutput("tblInterPheRice")
                                                                       ),
                                                                       shiny::tabPanel("Sweetpotato Phenology", value = "crop_phenology_Sweetpotato",
                                                                                       br(),
                                                                                       DTOutput("tblInterPheSweetpotato")
                                                                       ),
                                                                       shiny::tabPanel("Wheat Phenology", value = "crop_phenology_Wheat",
                                                                                       br(),
                                                                                       DTOutput("tblInterPheWheat")
                                                                       ),
                                                                       shiny::tabPanel("Other 1 Phenology", value = "crop_phenology_Other_1",
                                                                                       br(),
                                                                                       #actionButton("dop", "dop"),
                                                                                       DTOutput("tblInterPheOther1")
                                                                       ),
                                                                       shiny::tabPanel("Other 2 Phenology", value = "crop_phenology_Other_2",
                                                                                       br(),
                                                                                       DTOutput("tblInterPheOther2")
                                                                       ),
                                                                       shiny::tabPanel("Other 3 Phenology", value = "crop_phenology_Other_3",
                                                                                       br(),
                                                                                       DTOutput("tblInterPheOther3")
                                                                       ),
                                                                       shiny::tabPanel("Other 4 Phenology", value = "crop_phenology_Other_4",
                                                                                       br(),
                                                                                       DTOutput("tblInterPheOther4")
                                                                       ),
                                                                       shiny::tabPanel("Other 5 Phenology", value = "crop_phenology_Other_5",
                                                                                       br(),
                                                                                       DTOutput("tblInterPheOther5")
                                                                       ),
                                                                       shiny::tabPanel("Other 6 Phenology", value = "crop_phenology_Other_6",
                                                                                       br(),
                                                                                       DTOutput("tblInterPheOther6")
                                                                       ),
                                                                       shiny::tabPanel("Other 7 Phenology", value = "crop_phenology_Other_7",
                                                                                       br(),
                                                                                       DTOutput("tblInterPheOther7")
                                                                       ),
                                                                       shiny::tabPanel("Other 8 Phenology", value = "crop_phenology_Other_8",
                                                                                       br(),
                                                                                       DTOutput("tblInterPheOther8")
                                                                       ),
                                                                       shiny::tabPanel("Other 9 Phenology", value = "crop_phenology_Other_9",
                                                                                       br(),
                                                                                       DTOutput("tblInterPheOther9")
                                                                       ),
                                                                       shiny::tabPanel("Other 10 Phenology", value = "crop_phenology_Other_10",
                                                                                       br(),
                                                                                       DTOutput("tblInterPheOther10")
                                                                       )
                                                                     )
                                                              )
                                              ),
          
                                              shiny::tabPanel("Crop Phenology", value = 'crop_phenology_relay', icon = shiny::icon("leaf"),
                                                              column(width = 12,
                                                                     h2("Crop phenology"),
                                                                     #actionButton("qwe", "dale"),
                                                                     p(class = "text-muted", style="text-align:justify",
                                                                       paste("Please, select phenology by click.")
                                                                     ),
                                                                     #uiOutput("uiCropMeaInter"),
                                                                     tabsetPanel(
                                                                       id = "tabpanelrelayphe",
                                                                       #selected = 1,
                                                                       shiny::tabPanel("Cassava Phenology", value = "crop_phenology_relay_Cassava",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPheCassava")
                                                                       ),
                                                                       shiny::tabPanel("Common bean Phenology", value = "crop_phenology_relay_Commonbean",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPheCommon")
                                                                       ),
                                                                       shiny::tabPanel("Maize Phenology", value = "crop_phenology_relay_Maize",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPheMaize")
                                                                       ),
                                                                       shiny::tabPanel("Potato Phenology", value = "crop_phenology_relay_Potato",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPhePotato")
                                                                       ),
                                                                       shiny::tabPanel("Rice Phenology", value = "crop_phenology_relay_Rice",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPheRice")
                                                                       ),
                                                                       shiny::tabPanel("Sweetpotato Phenology", value = "crop_phenology_relay_Sweetpotato",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPheSweetpotato")
                                                                       ),
                                                                       shiny::tabPanel("Wheat Phenology", value = "crop_phenology_relay_Wheat",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPheWheat")
                                                                       ),
                                                                       shiny::tabPanel("Other 1 Phenology", value = "crop_phenology_relay_Other_1",
                                                                                       br(),
                                                                                       #actionButton("dop", "dop"),
                                                                                       DTOutput("tblRelayPheOther1")
                                                                       ),
                                                                       shiny::tabPanel("Other 2 Phenology", value = "crop_phenology_relay_Other_2",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPheOther2")
                                                                       ),
                                                                       shiny::tabPanel("Other 3 Phenology", value = "crop_phenology_relay_Other_3",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPheOther3")
                                                                       ),
                                                                       shiny::tabPanel("Other 4 Phenology", value = "crop_phenology_relay_Other_4",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPheOther4")
                                                                       ),
                                                                       shiny::tabPanel("Other 5 Phenology", value = "crop_phenology_relay_Other_5",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPheOther5")
                                                                       ),
                                                                       shiny::tabPanel("Other 6 Phenology", value = "crop_phenology_relay_Other_6",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPheOther6")
                                                                       ),
                                                                       shiny::tabPanel("Other 7 Phenology", value = "crop_phenology_relay_Other_7",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPheOther7")
                                                                       ),
                                                                       shiny::tabPanel("Other 8 Phenology", value = "crop_phenology_relay_Other_8",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPheOther8")
                                                                       ),
                                                                       shiny::tabPanel("Other 9 Phenology", value = "crop_phenology_relay_Other_9",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPheOther9")
                                                                       ),
                                                                       shiny::tabPanel("Other 10 Phenology", value = "crop_phenology_relay_Other_10",
                                                                                       br(),
                                                                                       DTOutput("tblRelayPheOther10")
                                                                       )
                                                                     )
                                                              )
                                              ),
          
                                              shiny::tabPanel("Weather", value = 'tabWeather2', icon = shiny::icon("bolt"),
                                                              column(width = 12,
                                                                     h2("Weather details"),
                                                                     p(class = "text-muted", style="text-align:justify",
                                                                       paste("Please, select row by click.")
                                                                     ),
                                                                     uiOutput("uiWeatherTab2")
                                                              )
                                                              
                                              ),
                                              
                                              shiny::tabPanel("Soil", value = 'tabSoil2', icon = shiny::icon("dice-four"),
                                                              column(width = 12,
                                                                     h2("Soil details"),
                                                                     p(class = "text-muted", style="text-align:justify",
                                                                       paste("Please, select row by click.")
                                                                     ),
                                                                     uiOutput("uiSoilTab2")
                                                              )
                                              )
          
                                              # shiny::tabPanel("Weather", value = 'tabWeather', icon = shiny::icon("bolt"),
                                              #                 column(width = 12,
                                              #                 h2("Weather details"),
                                              #                 p(class = "text-muted", style="text-align:justify",
                                              #                   paste("Please, select row by click.")
                                              #                 )),
                                              #                 #br(),
                                              #                 uiOutput("uiWeatherTab")
                                              # 
                                              # ),
                                              # 
                                              # shiny::tabPanel("Soil", value = 'tabSoil', icon = shiny::icon("bolt"),
                                              #                 #br(),
                                              #                 # h2("Weather & Soil"),
                                              #                 #fluidRow(
                                              #                 column(width = 12,
                                              #                        #         h2("Weather & Soil"),
                                              #                        #shinyTree::shinyTree("designFieldbook_weatherVar_agrofims",search = TRUE,checkbox = TRUE),
                                              #                        #shinyTree::shinyTree("designFieldbook_soilVar_agrofims",search = TRUE,checkbox = TRUE)
                                              #                        #br(),
                                              #                        h2("Soil details"),
                                              #                        p(class = "text-muted", style="text-align:justify",
                                              #                          paste("Please, select row by click.")
                                              #                        ),
                                              #                        br(),
                                              #                        uiOutput("uiSoilTab")
                                              # 
                                              #                        # DTOutput('soilDT')
                                              #                 )
                                              # )

                                              # shiny::tabPanel("Phenology dev", value = 'tabPhenoTest', icon = shiny::icon("bolt"),
                                              #                 column(width = 12,
                                              #                        h2("Crop Phenology"),
                                              #                        shinyTree::shinyTree("designFieldbook_cropPhenoTree_agrofims",search = TRUE,checkbox = TRUE)
                                              #                 )
                                              # )#,
                                         #) #end master

                                      )#) end tabbox,
                                    ) #end box
                                  ), #end fluid row


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
                          #rHandsontableOutput("hot", width = 1000),
          conditionalPanel( condition = "output.show_agrotable",
                                     shiny::fluidRow(
                                         #box(
                                         #column(width = 12,height=6,
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
  ),#end conditional panel

                                    br(),
                                    br(),
                                    br()


                           # ) #fin primer tab

                                       # ) #end

                            ######
                            ##################  End Simple Modules

                                       # )# End of Master tabSetPanel
                                     )
  }
