
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
pkg <- c("shiny", "shinydashboard", "rhandsontable", "data.table", "flexmix", 
         "ggplot2", "DT", "shinyBS", "Rcpp", "shinyAce", "shinyjs", "shinyFiles", "rmarkdown")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)
}
library(shinydashboard)
library(rhandsontable)
library(shinyBS)
library(DT)
library(shinyAce)
library(shinyFiles)
source("translations.R",local=T, encoding = 'UTF-8')
listChoicesSDTypeStr = paste0('listChoicesSDType = list("', uiCumuled,'" = 1, "',uiBreakDownByClass, '" = 2)')
eval(parse(text = listChoicesSDTypeStr))
listChoicesDiamRangeTypeStr = paste0('listChoicesDiamRangeType = list("', uiCm,'" = 1, "',uiClass, '" = 2)')
eval(parse(text = listChoicesDiamRangeTypeStr))
sidebar <- dashboardSidebar(
  hr(),
  shinyjs::useShinyjs(),
  sidebarMenu(id="tabs",
              menuItem(strong(uiMitHome), tabName="accueil", icon=icon("home"), selected=TRUE),
              menuItem(strong(uiMitInference), tabName = "inference", icon=icon("calculator")),
              menuItem(strong(uiMitSimulation), tabName = "simulation", icon=icon("line-chart")),
              menuItem(strong(uiMitVizualisation),  icon = icon("television"),
                       menuSubItem(strong(uiMitDataSimulation), tabName = "simulating_data", icon = icon("download")),
                       menuSubItem(strong(uiMitIndicators), tabName = "indicateurs", icon = icon("dashboard")),
                       menuSubItem(strong(uiMitDiametricStructure), tabName = "structurediametrique", icon = icon("circle-o")),
                       menuSubItem(strong(uiMitManageIndicators), tabName = "gestionindicateurs", icon=icon("cog"))
              ),
              menuItem(strong(uiMitHelp), tabName = "aide", icon = icon("question"))
  ),
  hr()
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "accueil",
            box(
              width = "100%", height = "600px",
              div(id="image_dynaffor",
                  imageOutput("imageDynaffor")
              ),
              div(id="objectif_logiciel",
                  h3(strong(uiSoftwareDescription))
              ),
              tags$style(type='text/css', "#image_dynaffor { text-align: center} #objectif_logiciel { text-align: center}")
            )
    ),
    tabItem(tabName = "inference",
            fluidRow(
              div(id = "param_inference",
                tabBox( width = "100%",
                  tabPanel(h5(strong(uiForestDynamicData)),
                     fluidRow(
                       column(width= 12,
                              fileInput('file_data_campagnes', uiImportCampagneCSVFile, multiple = FALSE, width = NULL,
                                        buttonLabel = paste0(uiImportFile," ..."), placeholder = uiAnyFilechoosed, accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv"))
                       ),
                       column(width = 12,
                          shinyjs::hidden(div(id="bloc_table_data_campagnes",
                           box(
                             h5(strong("Données des campagnes collectées")),width = "100%",
                               fluidRow(
                                 column(width = 12, id= "table_data_campagnes_col",
                                        rHandsontableOutput("table_data_campagnes", height = "50px")
                                 )
                              )
                           )
                         )),
                         shinyjs::hidden(div(id ='boxloader_FileDataCampagnes', width ="100px",
                                             div(id="img_loader_FileDataCampagnes", img(src= "trait_loader.gif", width= "100px")),
                                             tags$style(type='text/css', "#img_loader_FileDataCampagnes { text-align: center;}"),
                                             div(id="load_FileDataCampagnes_encours", strong(paste0(uiLoadingFileInprogress," ..."))),
                                             tags$style(type='text/css', "#load_FileDataCampagnes_encours {text-align: center;}")
                          )
                         ),
                         shinyjs::hidden(
                           div(id="bloc_table_parcelles_campagnes_selected",
                               box(
                                 h5(strong("Table de sélection des parcelles et des compagnes à utiliser")),width = "100%",
                                 fluidRow(
                                   column(width = 12, id= "table_parcelles_campagnes_selected_col",
                                        rHandsontableOutput("table_parcelles_campagnes_selected", height = "50px")
                                   )
                                 )
                               )
                           )
                         ),
                         
                         shinyjs::hidden(div(id="bloc_table_species_traits",
                             box(
                               h5(strong("Liste des espèces expoitées")),width = "100%",
                               fluidRow(
                                 column(width= 6,
                                        fileInput('file_species_traits', uiImportSpeciesTraitsCSVFile, multiple = FALSE, width = NULL,
                                                  buttonLabel = paste0(uiImportFile," ..."), placeholder = uiAnyFilechoosed, accept = c(
                                                    "text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv"))
                                 ),
                                 column(width = 12, id= "table_species_traits_col",
                                        rHandsontableOutput("table_species_traits", height = "50px")
                                 )
                               )
                             )
                         )),
                         shinyjs::hidden(div(id ='boxloader_FileSpeciesTraits', width ="100px",
                                             div(id="img_loader_FileSpeciesTraits", img(src= "trait_loader.gif", width= "100px")),
                                             tags$style(type='text/css', "#img_loader_FileSpeciesTraits { text-align: center;}"),
                                             div(id="load_FileSpeciesTraits_encours", strong(paste0(uiLoadingFileInprogress," ..."))),
                                             tags$style(type='text/css', "#load_FileSpeciesTraits_encours {text-align: center;}")
                         )
                         )
                         
                       )
                     )
                  ),
                  tabPanel(h5(strong(uiInferenceParameters)),
                     fluidRow(
                       column(width = 4,
                          box(
                            h5(strong("Liste des classes de diamètre")),width = "100%",
                            fluidRow(
                              column(width = 12, id= "table_classes_diam_col",
                                     rHandsontableOutput("table_classes_diam", height = "50px")
                              )
                            ),
                            fluidRow(
                              column(width= 12, 
                                     actionButton("save_classesDiam_btn", "Mettre à jour les données", style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                                     tags$style(type='text/css', "#save_classesDiam_btn{color: #fff; background-color: #337ab7; border-color: #2e6da4;font-weight: bold; margin-right: 4px;} #save_classesDiam_btn:hover { color:#000 !important;border-color:#979494 !important;}")
                              )
                            )
                          )
                       ),
                       column(width = 8,
                          box(
                            h5(strong("Selection des variables pour le modèle de recrutement")),width = "100%",
                            fluidRow(
                              column(width = 6, id= "table_Nb_Arbres_col",
                                 box(
                                   h5(strong("Effectifs des espèces par classe de diamètre")),width = "100%",
                                   fluidRow(
                                     column(width = 12,
                                            rHandsontableOutput("table_Nb_Arbres", height = "50px")
                                     )
                                   )
                                 )
                              ),
                              column(width = 6, id= "table_EffC_col",
                                 box(
                                   h5(strong("Effectifs dans les parcelles par classe de diamètre")),width = "100%",
                                   fluidRow(
                                     column(width = 12,
                                            rHandsontableOutput("table_EffC", height = "50px")
                                     )
                                   )
                                 )
                              )
                            )
                          )
                       )
                     ),
                     fluidRow(
                       column(width= 6, 
                              textInput("dyn_param_out_filename", "Fichier des paramètres de la dynamique",  placeholder = "Nom du fichier de sortie des paramètres de dynamique", width = "100%")
                       ),
                       column(width= 6, 
                              textInput("sim_data_out_filename", "Fichier des données de simulation",  placeholder = "Nom du fichier de sortie des données de simulation", width = "100%")
                       )
                     )
                  )
                )        
              ),
              # shinyjs::hidden(
              div(id="calculate_param_dyn_bloc",
                  box( 
                    width = "100%",
                    fluidRow(
                      column(id ='calculate_param_dyn_col', width= 12, 
                             actionButton("calculate_param_dyn_btn", "Calculer les paramètres", style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                             tags$style(type='text/css', "#calculate_param_dyn_btn{color: #fff; background-color: #337ab7; border-color: #2e6da4;font-weight: bold; margin-right: 4px;} #calculate_param_dyn_btn:hover { color:#000 !important;border-color:#979494 !important;}")
                      ),
                      shinyjs::hidden(column(id ='boxloader_CalculateParamDyn', width =12,
                                             div(id="img_loader_CalculateParamDyn", img(src= "trait_loader.gif", width= "100px")),
                                             tags$style(type='text/css', "#img_loader_CalculateParamDyn { text-align: center;}"),
                                             div(id="load_CalculateParamDyn_encours", strong(paste0(uiOngoingTreatment, " ..."))),
                                             tags$style(type='text/css', "#load_CalculateParamDyn_encours {text-align: center;}")
                      )
                      #tags$style(type='text/css', "#boxloader_SCD { position: fixed; left: 0%; top: 20%; z-index: 2;}")
                      )
                    )
                  )
              )
              # )
            )
    ),
    tabItem(tabName = "simulation",
            fluidRow(
              shinyjs::hidden(
                div(id ='boxloader',
                    box(title = "",
                        width = "100%",
                        fluidRow(
                          column(width= 8, 
                                 div(id="loader_sim", img(src= "sim_loader.gif", width= "100px")),
                                 shinyjs::hidden(div(id="image_success", img(src= "success1.png", width= "24px", height= "24px"))),
                                 shinyjs::hidden(div(id="image_failure", img(src= "error1.png", width= "24px", height= "24px"))),
                                 tags$style(type='text/css', "#loader_sim { padding-left: 50%; text-align: center;} #image_success { padding-left: 50%; text-align: center;} #image_failure { padding-left: 50%; text-align: center;}")
                          )
                        ),
                        fluidRow(
                          column(width= 8,
                                 div(id="sim_encours", strong(paste0(uiSimulatingInProgress, " ..."))),
                                 shinyjs::hidden(div(id="sim_finish", strong(uiSimulationTerminateSuccesfully))),
                                 shinyjs::hidden(div(id="sim_failure", strong(uiSimulationFaillure))),
                                 tags$style(type='text/css', "#sim_encours { padding-left: 50%; text-align: center;} #sim_finish { padding-left: 50%; text-align: center; color:green} #sim_failure { padding-left: 50%; text-align: center; color:red}")
                          )
                        ),
                        br(),
                        br()
                    )  
                )
              ),
              div(id = "param_sim_logging",
                  tabBox( width = NULL,
                          tabPanel(h5(strong(uiDynamicParameters)),
                                   
                                   shinyjs::hidden(
                                     div(id="description_site",
                                         uiOutput("file_dyn_informations"),
                                         br()
                                     )
                                   ),
                                  shinyFilesButton(id="load_dyn",label = uiLoadDynamicParameters, title = uiSelectDynamicFile, multiple = FALSE),
                                  shinyjs::hidden(div(id ='boxloader_FileDyn', width ="100px",
                                                      div(id="img_loader_FileDyn", img(src= "trait_loader.gif", width= "100px")),
                                                      tags$style(type='text/css', "#img_loader_FileDyn { text-align: center;}"),
                                                      div(id="load_FileDyn_encours", strong(paste0(uiLoadingFileInprogress," ..."))),
                                                      tags$style(type='text/css', "#load_FileDyn_encours {text-align: center;}")
                                  )
                                  #tags$style(type='text/css', "#boxloader_SCD { position: fixed; left: 0%; top: 20%; z-index: 2;}")
                                  )
                                   ),
                          tabPanel(h5(strong(uiSimulationParameters)),
                                   shinyjs::hidden(
                                     div(id="bloc_parameters_simulation",
                                         fluidRow(
                                           column(width= 6,
                                                  selectInput(
                                                    'siteParcelle', label = uiSimulateFromDataOfSite, multiple= FALSE, choices = NULL,
                                                    width = NULL, selectize = TRUE
                                                  ),
                                                  fileInput('file_data_start', uiImportFromCSVFile, multiple = FALSE, width = NULL,
                                                            buttonLabel = paste0(uiImportFile," ..."), placeholder = uiAnyFilechoosed, accept = c(
                                                              "text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv")),
                                                  shinyjs::hidden(
                                                    div(id="bloc_list_parcelles_sim",
                                                        fluidRow(
                                                          
                                                          column(width = 8, id= "parcelle_col",
                                                                 selectizeInput(
                                                                   'parcelle', label = uiPlotsToSimulate, multiple= TRUE, choices = NULL,
                                                                   options = list(create = FALSE)
                                                                 )
                                                                 #tags$style(type='text/css', "#parcelle_col {padding-right:0px}")
                                                          ),
                                                          column(width =  4, id= "Allparcelle_col",
                                                                 checkboxInput("Allparcelle", strong(uiAllPlots), FALSE)
                                                          ),
                                                          tags$style(type='text/css', "#Allparcelle_col {padding-top:3.5%}")
                                                        )
                                                    )
                                                  ),
                                                  shinyjs::hidden(
                                                    div(id="aggregate_parcels_bloc",
                                                        checkboxInput("aggregate_parcels", uiAggregateSelectedPlots, FALSE)
                                                    )
                                                  ),
                                                  shinyjs::hidden(
                                                    div(id="bloc_espece_sentier",
                                                        box(
                                                          h5(strong(uiChoiceOfSpecies)), width = "100%",
                                                          
                                                          fluidRow(
                                                            column(width= 8,
                                                                   selectizeInput(
                                                                     'espece', label = uiSpeciesToSimulate, multiple= FALSE, choices = NULL,
                                                                     options = list(create = FALSE)
                                                                   )
                                                            ),
                                                            column(width= 12,
                                                                   h5(strong(uiVectorOfInitialNumbersOfSpeciesByDiameterClass)),
                                                                   rHandsontableOutput("vector_number_per_diameter_class")
                                                            )
                                                          )
                                                        )
                                                    )
                                                  ),
                                                  numericInput("surfaceparcelle", uiSurfaceOfPlots, value=1, min = NA, max = NA, step = NA),
                                                  numericInput("anneedebutSim", uiStartYearOfSimulation, value=1984, min = NA, max = NA, step = NA),
                                                  shinyjs::hidden(
                                                    selectInput(
                                                      'anneedebutSimAutreSite', label = uiStartYearOfSimulation, multiple= FALSE, choices = NULL,
                                                      width = NULL, selectize = TRUE
                                                    )
                                                  ),
                                                  numericInput("anneefirstlogging", uiYearOfFirstLogging, value=1984, min = NA, max = NA, step = NA),   
                                                  
                                                  shinyjs::hidden(
                                                    div(id="bloc_recrutement_sentier",
                                                        box(
                                                          h5(strong("Recrutement")), width = "100%",
                                                          fluidRow(
                                                            column(width= 8,
                                                                   numericInput("tauxrecrutement", uiReproductiveFertilityRate, value=0, min = 0, max = Inf, step = NA)
                                                            ),
                                                            column(width= 12,
                                                                   h5(strong(uiPercentageOfBreedingTrees)),
                                                                   rHandsontableOutput("vector_tauxrecrutement_per_diameter_class")
                                                            )
                                                          )
                                                        )
                                                    )
                                                  )
                                                  
                                           ),
                                           column(width= 6,
                                                  numericInput("nombrerotation", uiNumberOfRotation, value=2, min = 0, max = Inf, step = NA),
                                                  numericInput("dureerotation", uiDurationOfRotation, value=25, min = 0, max = Inf, step = NA),
                                                  numericInput("nbchain", uiNumberOfSimulation, value=100, min = 0, max = Inf, step = NA),
                                                  numericInput("dureesimulation", uiDurationOfSimulation, value=50, min = 0, max = Inf, step = NA),
                                                  numericInput("delayAfterLogging", uiDurationOfDisturbanceAfterLogging, value=2, min = 0, max = Inf, step = NA),
                                                  shinyjs::hidden(numericInput("delayDynamicsPostExploitation", uiDurationOfStimulationOfDynamicAfterLogging, value=8, min = 0, max = Inf, step = NA))
                                                  
                                                  
                                           )
                                         ),
                                         fluidRow(
                                           shinyjs::hidden(
                                             div(id="bloc_start_species_number",
                                               box(
                                                 h5(strong(uiInitialNumberOfSpeciesByDiameterClass)),width = "100%",
                                                 tags$style(".shiny-file-input-progress {display: none}"),
                                                 fluidRow(
                                                   column(width= 12,
                                                      fluidRow(
                                                        column(width= 7,
                                                               rHandsontableOutput("data_start_species", height = "50px")
                                                        ),
                                                        column(width= 5,
                                                               rHandsontableOutput("data_start_missing_species", height = "50px")
                                                        )
                                                      )
                                                   ),
                                                  shinyjs::hidden( 
                                                     column(width= 4,  id="update_corresponding_data_perso_col",
                                                        
                                                        div(id="div_UpdateCDPerso", style="margin-top: 1em;",
                                                            actionButton("update_corresponding_data_perso", uiUpdateCorrespondenceData, style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                                                            tags$style(type='text/css', "#update_corresponding_data_perso{color: #fff; background-color: #337ab7; border-color: #2e6da4;font-weight: bold; margin-right: 4px;} #update_corresponding_data_perso:hover { color:#000 !important;border-color:#979494 !important;}")
                                                        ),
                                                        shinyjs::hidden(div(id ='boxloader_UpdateCDPerso', width ="100px",
                                                                            div(id="img_loader_UpdateCDPerso", img(src= "trait_loader.gif", width= "100px")),
                                                                            tags$style(type='text/css', "#img_loader_UpdateCDPerso { text-align: center;}"),
                                                                            div(id="load_UpdateCDPerso_encours", strong(paste0(uiUpdatingDataInProgress, " ...") )),
                                                                            tags$style(type='text/css', "#load_UpdateCDPerso_encours {text-align: center;}")
                                                          )
                                                        )
                                                     )
                                                  )
                                                 )
                                               )
                                              )
                                            )
                                         ),
                                         fluidRow(
                                           shinyjs::hidden(
                                             div(id="bloc_corresponding_species",
                                                 box(
                                                   h5(strong(uiCorrespondenceMSAndSUToEstimateDynamicParameters)),width = "100%",
                                                   fluidRow(
                                                     column(width= 12,
                                                            rHandsontableOutput("data_corresponding_species", height = "50px")
                                                     ),
                                                     column(width= 4,  id="update_corresponding_data_col",
                                                          div(id="div_UpdateCD", style="margin-top: 1em;",
                                                              actionButton("update_corresponding_data", uiUpdateCorrespondenceData, style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                                                              tags$style(type='text/css', "#update_corresponding_data{color: #fff; background-color: #337ab7; border-color: #2e6da4;font-weight: bold; margin-right: 4px;} #update_corresponding_data:hover { color:#000 !important;border-color:#979494 !important;}")
                                                          ),
                                                          shinyjs::hidden(div(id ='boxloader_UpdateCD', width ="100px",
                                                                              div(id="img_loader_UpdateCD", img(src= "trait_loader.gif", width= "100px")),
                                                                              tags$style(type='text/css', "#img_loader_UpdateCD { text-align: center;}"),
                                                                              div(id="load_UpdateCD_encours", strong(paste0(uiUpdatingDataInProgress," ..."))),
                                                                              tags$style(type='text/css', "#load_UpdateCD_encours {text-align: center;}")
                                                          )
                                                       )
                                                     )
                                                   )
                                                 )
                                             )
                                           )
                                         ),
                                         fluidRow(
                                           column(width = 12,
                                                  shinyjs::hidden(div(id ='boxloader_FileDataStart', width ="100px",
                                                                      div(id="img_loader_FileDataStart", img(src= "trait_loader.gif", width= "100px")),
                                                                      tags$style(type='text/css', "#img_loader_FileDataStart { text-align: center;}"),
                                                                      div(id="load_FileDataStart_encours", strong(paste0(uiLoadingFileInprogress," ..."))),
                                                                      tags$style(type='text/css', "#load_FileDataStart_encours {text-align: center;}")
                                                )
                                             )
                                             
                                           )
                                        )
                                     )
                                   )
                          ),
                          tabPanel(h5(strong(uiLoggingParameters)),
                                   shinyjs::hidden(
                                     div(id="bloc_parameters_logging",
                                         fluidRow(
                                           column(width=12, 
                                              checkboxInput("simulate_logging", strong(uiRealizationOfLogging), TRUE)
                                           )
                                           
                                         ),
                                         div(id="simulate_logging_bloc",
                                           fluidRow(
                                             column(width= 8,
                                                box(id = "data_logging_box",
                                                    h5(strong(uiTableOfSpeciesToLogg)),width = "900px",
                                                    rHandsontableOutput("data_logging", height = "50px")
                                                )
                                             ),
                                             column(width= 4, id="tarifgenerique_col",
                                                    textInput("tarifgenerique", uiGenericCubicRate, value="", placeholder = "Exemple : 10^(-2.96+1.93*log10(d))"),
                                                    tags$style(type='text/css', "#tarifgenerique { display: inline-block !important;}")
                                                    
                                             )
                                           ),
                                           shinyjs::hidden(
                                             br(),br(),
                                             fluidRow(
                                               column(width= 6,
                                                      numericInput("anneereprisedyn", "Nombre d'année avant la reprise de la dynamique", value=1, min = NA, max = NA, step = NA)
                                               )
                                               
                                             )
                                           ),
                                           
                                           br(),br(),
                                           
                                           fluidRow(
                                             column(width= 12,
                                               box(
                                                 h5(strong(uiVectorOfPostLoggingDamageByDiameterClass)), width = "100%",
                                                 rHandsontableOutput("vector_damage")
                                               )
                                             )
                                             
                                           ),
                                           br(),br(),
                                           
                                           fluidRow(
                                             shinyjs::hidden(
                                               column(width= 12, id="bloc_estimation_tcs_dimako",
                                                      box(
                                                        h5(strong(uiEstimateOfStockReconstitutionRateDimako)), width = "100%",
                                                        
                                                        fluidRow(
                                                          column(width= 3,
                                                                 numericInput("tauxaccroissement", uiGrowthRate, value=0.5, min = 0, max = Inf, step = NA)
                                                          ),
                                                          column(width= 3,
                                                                 numericInput("tauxmortalite", uiMortalityRate, value=1, min = 0, max = Inf, step = NA)
                                                          ),
                                                          shinyjs::hidden(
                                                            column(id="result_estimation_tcs", width = 12,
                                                                   uiOutput("estimation_tcs_value")
                                                            )
                                                          ),
                                                          column(width= 12,
                                                                 div(id="div_estimate_tcs", style="margin-top: 1em;",
                                                                     actionButton("estimate_tcs_btn", uiCalculateValue, style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                                                                     tags$style(type='text/css', "#estimate_tcs_btn{color: #fff; background-color: #337ab7; border-color: #2e6da4;font-weight: bold; margin-right: 4px;} #estimate_tcs_btn:hover { color:#000 !important;border-color:#979494 !important;}")
                                                                 ),
                                                                 shinyjs::hidden(div(id ='boxloader_estimate_tcs', width ="100px",
                                                                                     div(id="img_loader_estimate_tcs", img(src= "trait_loader.gif", width= "100px")),
                                                                                     tags$style(type='text/css', "#img_loader_estimate_tcs { text-align: center;}"),
                                                                                     div(id="load_estimate_tcs_encours", strong(paste0(uiEstimateTCSInprogress, " ..."))),
                                                                                     tags$style(type='text/css', "#load_estimate_tcs_encours {text-align: center;}")
                                                                 )
                                                                 )
                                                          )
                                                        )
                                                      )
                                               )
                                             )
                                             
                                           )
                                        )
                                     )
                                   )
                                   
                          )
                  )
              ),
              shinyjs::hidden(
                div(id ='box_alert_message',
                    fluidRow(
                      column(width= 8,
                             div(id="alert_bad_success", bsAlert("alert")),
                             tags$style(type='text/css', "#alert_bad_success { padding-left: 45%}")
                      )
                    )
                ),
                tags$style(type='text/css', "#alert_bad_success { position: fixed; left: 0%; top: 20%; z-index: 2;}")
              ),
              
              shinyjs::hidden(
                div(id="action_save_sim_file",
                    box( 
                      width = NULL,
                      fluidRow(
                        column(width= 12, 
                               checkboxInput("saveFileSimOrNot", strong(uiSaveSimulatedDataInSpecificFile), FALSE),
                               tags$style(type='text/css', "#saveFileSimOrNot{font-weight: bold;} ")
                        )
                      )
                    )
                )
              ),
              shinyjs::hidden(
                div(id="actions_sim",
                    box( 
                      width = NULL,
                      fluidRow(
                        column( id= "lancer_sim_col" , width= 12, 
                                div(style="display:inline-block",
                                    conditionalPanel(
                                      condition  = "input.saveFileSimOrNot == false",
                                      actionButton("lancer_sim_without_save_file", uiStartSimulation, style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                                      tags$style(type='text/css', "#lancer_sim_without_save_file{color: #fff; background-color: #337ab7; border-color: #2e6da4;font-weight: bold; margin-right: 4px;} #lancer_sim_without_save_file:hover { color:#000 !important;border-color:#979494 !important;}")
                                    ),
                                    conditionalPanel(
                                      condition  = "input.saveFileSimOrNot == true",
                                      shinySaveButton("lancer_sim", uiStartSimulation, paste0(uiSaveAs, " ..."), filetype = list(RData = "RData"), buttonType = "blue"),
                                      tags$style(type='text/css', "#lancer_sim{color: #fff; background-color: #337ab7; border-color: #2e6da4;font-weight: bold; margin-right: 4px;} #lancer_sim:hover { color:#000 !important;border-color:#979494 !important;}")
                                    )
                                ),
                                
                        #),
                        #column( id= "save_config_col" , width= 2, 
                                div(style="display:inline-block",
                                    shinySaveButton("save_config", uiSaveScenario, paste0(uiSaveAs, " ..."), filetype = list(RData = "RData"), buttonType = "default"),
                                  #actionButton("save_config", strong("Sauvegarder le scénario"), icon = icon('save'), style="color:#000 !important;border-color:#979494;")
                                  tags$style(type='text/css', "#save_config { color:#000 !important;border-color:#979494 !important; font-weight: bold;margin-right: 4px;}")
                                ),
                                
                        #),
                        #column( id= "load_config_col" , width= 2, 
                                div(style="display:inline-block",
                                  #actionButton("load_config", strong("Charger un scénario"), icon = icon('hourglass-half'), style="color:#000 !important;border-color:#979494;")
                                  shinyFilesButton('load_config', uiLoadScenario, uiSelectScenarioFile, multiple = FALSE),
                                  tags$style(type='text/css', "#load_config { color:#000 !important;border-color:#979494 !important; font-weight: bold;}")
                                )
                                
                        ),
                        shinyjs::hidden(column(id ='boxloader_FileConfig', width =12,
                                            div(id="img_loader_FileConfig", img(src= "trait_loader.gif", width= "100px")),
                                            tags$style(type='text/css', "#img_loader_FileConfig { text-align: center;}"),
                                            div(id="load_FileConfig_encours", strong(paste0(uiLoadingFileInprogress, " ..."))),
                                            tags$style(type='text/css', "#load_FileConfig_encours {text-align: center;}")
                        )
                        #tags$style(type='text/css', "#boxloader_SCD { position: fixed; left: 0%; top: 20%; z-index: 2;}")
                        ),
                        shinyjs::hidden(column(id= "gotoparameters_col", width= 2,
                                               div(
                                                 actionButton("gotoparameters", uiReturnToSettings, icon = icon("cog"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                                                 tags$style(type='text/css', "#annuler { padding-left: 5%} #gotoparameters:hover { color:#000 !important;border-color:#979494 !important;}")
                                               )
                        ))
                        
                      )
                    )
                )
            )
            )
    ),
    tabItem(tabName = "simulating_data",
            box(width = NULL,
              shinyjs::hidden(
                div(id="description_file_sim",
                    uiOutput("file_sim_informations"),
                    br()
                )
              ),
              shinyFilesButton(id="load_sim_data",label=uiLoadSimulatedDataFile, title=uiSelectAFile, multiple = FALSE),
              # fileInput("file1", "Choose CSV File",
              #          accept = c(
              #            "text/csv",
              #            "text/comma-separated-values,text/plain",
              #            ".csv")
              # ),
              shinyjs::hidden(div(id ='boxloader_SimData', width ="100px",
                                  div(id="img_loader_SimData", img(src= "trait_loader.gif", width= "100px")),
                                  tags$style(type='text/css', "#img_loader_SimData { text-align: center;}"),
                                  div(id="load_SimData_encours", strong(paste0(uiLoadingFileInprogress, " ..."))),
                                  tags$style(type='text/css', "#load_SimData_encours {text-align: center;}")
              )
              #tags$style(type='text/css', "#boxloader_SCD { position: fixed; left: 0%; top: 20%; z-index: 2;}")
              )
            )
    ),
    tabItem(tabName = "indicateurs",
            #fluidRow(
            box(width = NULL, status = "primary",
                fluidRow(
                  
                  column(width =  4,
                         selectInput(
                           'indicateur', label = uiIndicator, choices = c("Stock exploitable", "Volume exploitable", "Nombre d’arbres", "Volume d'arbres", "Surface terrière", "Biomasse", "Taux de reconstitution du stock"), width = NULL
                         )
                  )
                ),
                fluidRow(id="block_groupe_espece",
                         column(id="groupe_espece_indicateur_col", width =  4,
                                selectizeInput(
                                  'groupe_espece_indicateur', label = uiSpeciesGroup, multiple= TRUE, choices = c(221, 222, 223, 224, 225),
                                  options = list(create = FALSE), width = NULL
                                )
                         ),
                         
                         column(width =  4, id = "selectGEIAll_col",
                                div(
                                  checkboxInput("selectGEIAll", strong(uiAllSpecies), value = FALSE, width = NULL)
                                )
                         ),
                         tags$style(type='text/css', "#selectGEIAll_col {padding-top:2%}")
                ),
                #strong("Plage de diametre"),
                fluidRow(id="block_class_diam",
                         #useShinyjs(),
                         column(width =  2,
                                radioButtons("plageClass", label = uiDiameterRange,
                                             choices = listChoicesDiamRangeType, inline = TRUE)),
                         column(id="diamMinCol",  width =  2,
                                numericInput("diamMin", uiMinimalDiameter, value=0, min = NA, max = NA, step = NA)
                         ),
                         column(id="diamMaxCol", width =  2,
                                numericInput("diamMax", uiMaximalDiameter, value=0, min = NA, max = NA, step = NA)
                         ),
                         shinyjs::hidden(column(id="classMinCol",  width =  2,
                                                numericInput("classMin", uiMinimalClass, value=0, min = NA, max = NA, step = NA)
                         )),
                         shinyjs::hidden(column(id="classMaxCol", width =  2,
                                                numericInput("classMax", uiMaximalClass, value=0, min = NA, max = NA, step = NA)
                         ))
                ),
                fluidRow(
                  column(width = 12,
                         sliderInput("yearrange_indicateur", label = uiYearsRange, min = 2010, 
                                     max = 2030, value = c(2015, 2025), step=1, sep="", dragRange = FALSE),
                         checkboxInput("verif_indicator", strong(uiValidationWithRealData), FALSE)
                  )
                ),
                fluidRow(
                  column(width =  2,
                         actionButton("afficher_indicateur", strong(uiVisualize), icon = icon("eye"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4;")),
                  tags$style(type='text/css', "#afficher_indicateur:hover { color:#000 !important;border-color:#979494 !important;}")    
                ),
                shinyjs::hidden(div(id ='boxloader_IND', width ="100px",
                                    div(id="img_loader_IND", img(src= "trait_loader.gif", width= "100px")),
                                    tags$style(type='text/css', "#img_loader_IND { text-align: center;}"),
                                    div(id="load_IND_encours", strong(paste0(uiOngoingTreatment, " ..."))),
                                    tags$style(type='text/css', "#load_IND_encours {text-align: center;}")
                )
                #tags$style(type='text/css', "#boxloader_SCD { position: fixed; left: 0%; top: 20%; z-index: 2;}")
                )
            ),
            tabBox( width = NULL,
                    tabPanel(h5(strong(uiGraphicRepresentation)),
                             shinyjs::hidden(fluidRow(id = "bloc_graphique_Indicateur",
                               box(width = 12, 
                                   shinyjs::hidden(fluidRow(id = "bloc_export_indicateur_rg",
                                     column(width =2,
                                            selectInput(
                                              'extension',label = NULL, choices = c("png", "jpeg", "pdf"), width = NULL
                                            )
                                     ),
                                     column(width =2,
                                            downloadButton('download_indic_rg', strong(uiExport)),
                                            tags$style(type='text/css', "#download_indic_rg { color:#000 !important;border-color:#979494 !important;}")
                                     )
                                   )),
                                   
                                   plotOutput("plot_indicateur",height="500px"),
                                   status = "primary")
                             ))
                    ),
                    tabPanel(h5(strong(uiData)),
                             shinyjs::hidden(
                               div(id = "bloc_donnees_Indicateur",
                                   shinyjs::hidden(fluidRow(id = "bloc_export_indicateur_data",
                                                            column(width =2,
                                                                   selectInput(
                                                                     'extensionData',label = NULL, choices = c("csv", "pdf"), width = NULL
                                                                   )
                                                            ),
                                                            column(width =2,
                                                                   downloadButton('download_indic_data', strong(uiExport)),
                                                                   tags$style(type='text/css', "#download_indic_data { color:#000 !important;border-color:#979494 !important;}")
                                                            )
                                   )),
                                   br(),
                                   br(),
                                   fluidRow(
                                     column(width = 12, 
                                            DT::dataTableOutput("data_indicateur")
                                     )
                                   )
                                )
                             )
                             
                    )
            )
            #)
    ),
    tabItem(tabName = "structurediametrique",
            box(
              width = NULL, status = "primary",
              fluidRow(
                column(id="groupe_espece_strDia_col", width = 4, 
                       selectizeInput(
                         'groupe_espece_strDia', label = uiSpeciesGroup, multiple= TRUE, choices = c(221, 222, 223, 224, 225),
                         options = list(create = FALSE), width = NULL
                       )
                ),
                column(width =  4, id= "selectGESDAll_col",
                       checkboxInput("selectGESDAll", strong(uiAllSpecies), value = FALSE, width = NULL)
                ),
                tags$style(type='text/css', "#selectGESDAll_col {padding-top:2%}")
              ),
              fluidRow(
                column(width =  6,
                       radioButtons("whatSD", label = uiDiametricStructureType,
                                    choices = listChoicesSDType, inline = TRUE)
                )
              ),
              shinyjs::hidden(
                fluidRow(id="slider_SD",
                                       column(width =12  ,
                                              sliderInput("yearslider_strDiam", label = uiYear, min = 2010, 
                                                          max = 2030, value =  2025, step=1, sep="", dragRange = FALSE)
                                       )
              )), 
              fluidRow(
                # column(width =12  ,
                #        checkboxInput("verif_SD", strong("Validation avec les vraies données"), FALSE)
                # ),
                column(width = 4, 
                       actionButton("plot_SCD", strong(uiVisualize), icon = icon('eye'), style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                       tags$style(type='text/css', "#plot_SCD:hover { color:#000 !important;border-color:#979494 !important;}")
                )
                
              ),
              shinyjs::hidden(div(id ='boxloader_SCD', width ="100px",
                                  div(id="img_loader_SCD", img(src= "trait_loader.gif", width= "100px")),
                                  tags$style(type='text/css', "#img_loader_SCD { text-align: center;}"),
                                  div(id="load_SCD_encours", strong(paste0(uiOngoingTreatment, " ..."))),
                                  tags$style(type='text/css', "#load_SCD_encours {text-align: center;}")
              )
              )
            ),
            tabBox( width = NULL,
                    tabPanel(h5(strong(uiGraphicRepresentation)), 
                             shinyjs::hidden(fluidRow(id = "bloc_graphique_SCD",
                               box(width = 12, 
                                   shinyjs::hidden(fluidRow(id= "bloc_export_sd_rg",
                                     column(width =2,
                                            selectInput(
                                              'extensionSD',label = NULL, choices = c("png", "jpeg", "pdf"), width = NULL
                                            )
                                     ),
                                     column(width =2,
                                            downloadButton('download_strucDiam_rg', strong(uiExport)),
                                            tags$style(type='text/css', "#download_strucDiam_rg { color:#000 !important;border-color:#979494 !important;}")
                                     )
                                   )),
                                   plotOutput("plot_strDia",height="500px"),
                                   status = "primary")
                             ))
                    ),
                    tabPanel(h5(strong(uiData)),
                             shinyjs::hidden(
                               div(id = "bloc_donnees_SCD",
                                   shinyjs::hidden(fluidRow(id = "bloc_export_sd_data",
                                                            column(width =2,
                                                                   selectInput(
                                                                     'extensionSDData',label = NULL, choices = c("csv", "pdf"), width = NULL
                                                                   )
                                                            ),
                                                            column(width =2,
                                                                   downloadButton('download_strucDiam_data', strong(uiExport)),
                                                                   tags$style(type='text/css', "#download_strucDiam_data { color:#000 !important;border-color:#979494 !important;}")
                                                            )
                                   )),
                                   br(),
                                   br(),
                                   fluidRow(
                                     column(width = 12, 
                                            DT::dataTableOutput("data_strDia")
                                     )
                                   )
                               )
                             )
                    )
            )
            
    ),
    tabItem(tabName = "gestionindicateurs",
            fluidRow(
              tabBox( width = NULL,
                    tabPanel(h5(strong(uiAddIndicator)),
                             div(id="new_indicateur",
                                 fluidRow(
                                   column(width= 4,
                                          textInput("nom_indicateur_new", uiNameOfIndicator, value="", placeholder = uiNameOfIndicator)
                                   )
                                 ),
                                 fluidRow(
                                   column(width= 8,
                                          helpText(strong(uiMathematicalExpression)),
                                          aceEditor("fonction_indicateur_new", mode="r", theme="chrome", vimKeyBinding = FALSE,
                                                    readOnly = FALSE, height = "200px", fontSize = 12, debounce = 1000,
                                                    wordWrap = FALSE, showLineNumbers = TRUE, highlightActiveLine = TRUE,
                                                    selectionId = NULL, cursorId = NULL, hotkeys = NULL,
                                                    autoComplete = c("disabled", "enabled", "live"), autoCompleteList = NULL)
                                   )
                                 ),
                                 br(),
                                 fluidRow(id="action_add_IND",
                                          column(width= 2, 
                                                 div(
                                                   actionButton("ajout_indicateur_btn", strong(uiAdd), icon = icon('plus'), style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                                                   tags$style(type='text/css', "#ajout_indicateur_btn:hover { color:#000 !important;border-color:#979494 !important;}")
                                                 )
                                                 
                                          )
                                 ),
                                 shinyjs::hidden(div(id ='boxloader_add_IND', width ="100px",
                                                     div(id="img_loader_add_IND", img(src= "trait_loader.gif", width= "100px")),
                                                     tags$style(type='text/css', "#img_loader_add_IND { text-align: center;}"),
                                                     div(id="load_add_IND_encours", strong(paste0(uiSavingInProgress, " ..."))),
                                                     tags$style(type='text/css', "#load_add_IND_encours {text-align: center;}")
                                 )
                                 
                                 )    
                             )
                    ),
                    tabPanel(h5(strong(uiUpdateAndDeleteIndicator)),
                             div(id="update_indicateur",
                                 fluidRow(
                                   column(width= 4,
                                          selectInput(
                                            'nom_indicateur_update', label = uiIndicator, choices = c(), width = NULL
                                          )
                                          
                                   )), 
                                 fluidRow(
                                   column(width= 8,
                                          helpText(strong(uiMathematicalExpression)),
                                          aceEditor("fonction_indicateur_update", mode="r", theme="chrome", vimKeyBinding = FALSE,
                                                    readOnly = FALSE, height = "200px", fontSize = 12, debounce = 1000,
                                                    wordWrap = FALSE, showLineNumbers = TRUE, highlightActiveLine = TRUE,
                                                    selectionId = NULL, cursorId = NULL, hotkeys = NULL,
                                                    autoComplete = c("disabled", "enabled", "live"), autoCompleteList = NULL)
                                   )
                                 ),
                                 br(),
                                 fluidRow(id ="action_update_delete_IND",
                                          column(width= 4, 
                                                 div(style="display:inline-block",
                                                     actionButton("update_indicateur_btn", strong(uiUpdate), icon = icon('edit'), style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                                                     tags$style(type='text/css', "#update_indicateur_btn:hover { color:#000 !important;border-color:#979494 !important;}")
                                                 ),
                                                 div(style="display:inline-block",
                                                     actionButton("delete_indicateur_btn", strong(uiDelete), icon = icon('trash'), style="color: #fff; background-color: red; border-color: red;"),
                                                     tags$style(type='text/css', "#delete_indicateur_btn:hover { color:#000 !important;border-color:#979494 !important;}")
                                                 )
                                                 
                                          )
                                 ),
                                 shinyjs::hidden(div(id ='boxloader_update_IND', width ="100px",
                                                     div(id="img_loader_update_IND", img(src= "trait_loader.gif", width= "100px")),
                                                     tags$style(type='text/css', "#img_loader_update_IND { text-align: center;}"),
                                                     div(id="load_update_IND_encours", strong(paste0(uiUpdatingInProgress," ..."))),
                                                     tags$style(type='text/css', "#load_update_IND_encours {text-align: center;}")
                                 )
                                 )
                             )
                             
                    )
            )
      )
    ),
    tabItem(tabName = "aide",
        includeMarkdown("./about/about.Rmd")
    )
  )
)

dashboardPage(
  dashboardHeader(title = "DafSim"),
  sidebar,
  body
)