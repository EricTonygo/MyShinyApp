
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shinydashboard)
library(rhandsontable)
library(shinyBS)
library(DT)
library(shinyAce)
library(shinyFiles)
sidebar <- dashboardSidebar(
  hr(),
  shinyjs::useShinyjs(),
  sidebarMenu(id="tabs",
              menuItem(strong("Accueil"), tabName="accueil", icon=icon("home"), selected=TRUE),
              menuItem(strong("Simulation"), tabName = "simulation", icon=icon("line-chart")),
              menuItem(strong("Gérer les indicateurs"), tabName = "parametres", icon=icon("cog")),
              menuItem(strong("Visualisation"),  icon = icon("television"),
                       menuSubItem(strong("Données de simulation"), tabName = "simulating_data", icon = icon("download")),
                       menuSubItem(strong("Indicateurs"), tabName = "indicateurs", icon = icon("dashboard")),
                       menuSubItem(strong("Structure Diamétrique"), tabName = "structurediametrique", icon = icon("circle-o"))
              ),
              menuItem(strong("Aide"), tabName = "aide", icon = icon("question"))
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
                  #h3(strong("OUTIL D'AIDE A LA GESTION DURABLE DES EXPLOITATIONS FORESTIERES"))
                  h3(strong("OUTIL DE SIMULATION DE LA DYNAMIQUE FORESTIERE"))
              ),
              tags$style(type='text/css', "#image_dynaffor { text-align: center} #objectif_logiciel { text-align: center}")
              
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
                                 div(id="sim_encours", strong("Simulation en cours ...")),
                                 shinyjs::hidden(div(id="sim_finish", strong("Simulation terminée avec succès"))),
                                 shinyjs::hidden(div(id="sim_failure", strong("Echec de la simulation"))),
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
                          tabPanel(h5(strong("Paramètres de la dynamique")),
                                   
                                   shinyjs::hidden(
                                     div(id="description_site",
                                         uiOutput("file_dyn_informations"),
                                         br()
                                     )
                                   ),
                                  shinyFilesButton(id="load_dyn",label="Charger les paramètres de la dynamique",title='Selectionner le fichier de dynamique', multiple = FALSE),
                                  shinyjs::hidden(div(id ='boxloader_FileDyn', width ="100px",
                                                      div(id="img_loader_FileDyn", img(src= "trait_loader.gif", width= "100px")),
                                                      tags$style(type='text/css', "#img_loader_FileDyn { text-align: center;}"),
                                                      div(id="load_FileDyn_encours", strong("Chargement du fichier en cours ...")),
                                                      tags$style(type='text/css', "#load_FileDyn_encours {text-align: center;}")
                                  )
                                  #tags$style(type='text/css', "#boxloader_SCD { position: fixed; left: 0%; top: 20%; z-index: 2;}")
                                  )
                                   ),
                          tabPanel(h5(strong("Paramètres de la simulation")),
                                   shinyjs::hidden(
                                     div(id="bloc_parameters_simulation",
                                         fluidRow(
                                           column(width= 6,
                                                  numericInput("anneedebutSim", "Année de debut de la simulation", value=1984, min = NA, max = NA, step = NA),
                                                  numericInput("nbchain", "Nombre de simulation", value=50, min = 0, max = Inf, step = NA),
                                                  numericInput("dureesimulation", "Durée de la simulation", value=20, min = 0, max = Inf, step = NA),
                                                  shinyjs::hidden(
                                                    div(id="bloc_list_parcelles_sim",
                                                        fluidRow(
                                                                 column(width =  4,
                                                                        selectInput(
                                                                          'siteParcelle', label = "Site", multiple= FALSE, choices = NULL,
                                                                          width = NULL, selectize = TRUE
                                                                        )
                                                                  ),
                                                                 column(width = 4, id= "parcelle_col",
                                                                        #conditionalPanel(
                                                                          #condition  = "input.Allparcelle == false",
                                                                          selectizeInput(
                                                                            'parcelle', label = "Parcelle à simuler", multiple= TRUE, choices = NULL,
                                                                            options = list(create = FALSE)
                                                                          )
                                                                        #)
                                                                 ),
                                                                 column(width =  4, id= "Allparcelle_col",
                                                                        checkboxInput("Allparcelle", strong("Toutes les parcelles"), FALSE)
                                                                 ),
                                                                 tags$style(type='text/css', "#Allparcelle_col {padding-top:3.5%}")
                                                        )
                                                    )
                                                  ),
                                                  shinyjs::hidden(
                                                    div(id="aggregate_parcels_bloc",
                                                        checkboxInput("aggregate_parcels", "Agréger les parcelles sélectionnées en une", FALSE)
                                                    )
                                                  ),
                                                  shinyjs::hidden(
                                                    div(id="bloc_espece_sentier",
                                                        box(
                                                          h5(strong("Choix de l'espèce")), width = "100%",
                                                          
                                                          fluidRow(
                                                            column(width= 8,
                                                                   selectizeInput(
                                                                     'espece', label = "Espèce à simuler", multiple= FALSE, choices = NULL,
                                                                     options = list(create = FALSE)
                                                                   )
                                                            ),
                                                            column(width= 12,
                                                                   h5(strong("Vecteur des effectifs de départ de l'espèce par classe de diamètre")),
                                                                   rHandsontableOutput("vector_number_per_diameter_class")
                                                            )
                                                          )
                                                        )
                                                    )
                                                  )
                                           ),
                                           column(width= 6,
                                                  numericInput("anneefirstlogging", "Année de la première d'exploitation", value=1984, min = NA, max = NA, step = NA),
                                                  numericInput("nombrerotation", "Nombre de rotation", value=2, min = 0, max = Inf, step = NA),
                                                  numericInput("dureerotation", "Durée d\'une rotation", value=5, min = 0, max = Inf, step = NA),
                                                  checkboxInput("check", "Validation avec les vraies données", FALSE),
                                                  conditionalPanel(
                                                    condition  = "input.check == true",
                                                    numericInput("firstyearcompare", "Première année de comparaison", value=1985, min = NA, max = NA, step = NA)
                                                    #fileInput('file_true_data', 'Importer le fichier des vraies données',
                                                    #           accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                                                  ),
                                                  shinyjs::hidden(
                                                    div(id="bloc_recrutement_sentier",
                                                        box(
                                                          h5(strong("Recrutement")), width = "100%",
                                                          fluidRow(
                                                            column(width= 8,
                                                                   numericInput("tauxrecrutement", "Taux de recrutement", value=0, min = 0, max = Inf, step = NA)
                                                            ),
                                                            column(width= 12,
                                                                   h5(strong("Vecteur des contributions au taux recrutement")),
                                                                   rHandsontableOutput("vector_tauxrecrutement_per_diameter_class")
                                                            )
                                                          )
                                                        )
                                                    )
                                                  )
                                                  
                                           )
                                         ),
                                         fluidRow(
                                           shinyjs::hidden(
                                             div(id="bloc_start_species_number",
                                               box(
                                                 h5(strong("Effectifs de départ des espèces par classe de diamètre")),width = "100%",
                                                 tags$style(".shiny-file-input-progress {display: none}"),
                                                 fluidRow(
                                                   column(width= 4,
                                                      fileInput('file_data_start', "Importer à partir d'un fichier CSV", multiple = FALSE, accept = NULL, width = NULL,
                                                                buttonLabel = "Browse...", placeholder = "No file selected")
                                                   ),
                                                   
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
                                                   
                                                   column(width= 4,
                                                      div(style="margin-top: 1em;",
                                                          shinySaveButton("save_start_data", "Sauvegarder effectifs départ", "Save as ..", filetype = list(csv = "CSV", xlsx ="xlsx"), buttonType = "default"),
                                                          tags$style(type='text/css', "#save_start_data { color:#000 !important;border-color:#979494 !important; font-weight: bold;}")
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
                          tabPanel(h5(strong("Paramètres de l'exploitation")),
                                   #rhandsontable pour les données d'exploitation
                                   
                                   
                                   shinyjs::hidden(
                                     div(id="bloc_parameters_logging",
                                         fluidRow(
                                           column(width= 12,
                                                  textInput("tarifgenerique", "Tarif de cubage générique (en fonction du diamètre d)", value="", placeholder = "Exemple : 10^(-2.96+1.93*log10(d))"),
                                                  tags$style(type='text/css', "#tarifgenerique { display: inline-block !important;}")
                                                  
                                           )),
                                         fluidRow(
                                           box(
                                             h5(strong("Tableau des espèces à exploiter")),width = "100%",
                                             rHandsontableOutput("data_logging", height = "50px")
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
                                           box(
                                             h5(strong("Vecteur des dégâts post-exploitation par classe de diamètre (%)")), width = "100%",
                                             rHandsontableOutput("vector_damage")
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
                               checkboxInput("saveFileSimOrNot", strong("Sauvegarder les données simulées dans un fichier spécifique"), FALSE),
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
                                      actionButton("lancer_sim_without_save_file", "Lancer la simulation", style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                                      tags$style(type='text/css', "#lancer_sim_without_save_file{color: #fff; background-color: #337ab7; border-color: #2e6da4;font-weight: bold; margin-right: 4px;} #lancer_sim_without_save_file:hover { color:#000 !important;border-color:#979494 !important;}")
                                    ),
                                    conditionalPanel(
                                      condition  = "input.saveFileSimOrNot == true",
                                      shinySaveButton("lancer_sim", "Lancer la simulation", "Save simulating data file as ..", filetype = list(RData = "RData"), buttonType = "blue"),
                                      tags$style(type='text/css', "#lancer_sim{color: #fff; background-color: #337ab7; border-color: #2e6da4;font-weight: bold; margin-right: 4px;} #lancer_sim:hover { color:#000 !important;border-color:#979494 !important;}")
                                    )
                                ),
                                
                        #),
                        #column( id= "save_config_col" , width= 2, 
                                div(style="display:inline-block",
                                    shinySaveButton("save_config", "Sauvegarder le scénario", "Save as ..", filetype = list(RData = "RData"), buttonType = "default"),
                                  #actionButton("save_config", strong("Sauvegarder le scénario"), icon = icon('save'), style="color:#000 !important;border-color:#979494;")
                                  tags$style(type='text/css', "#save_config { color:#000 !important;border-color:#979494 !important; font-weight: bold;margin-right: 4px;}")
                                ),
                                
                        #),
                        #column( id= "load_config_col" , width= 2, 
                                div(style="display:inline-block",
                                  #actionButton("load_config", strong("Charger un scénario"), icon = icon('hourglass-half'), style="color:#000 !important;border-color:#979494;")
                                  shinyFilesButton('load_config', "Charger un scénario", 'Selectionner le fichier de scénario', multiple = FALSE),
                                  tags$style(type='text/css', "#load_config { color:#000 !important;border-color:#979494 !important; font-weight: bold;}")
                                )
                                
                        ),
                        shinyjs::hidden(column(id ='boxloader_FileConfig', width =12,
                                            div(id="img_loader_FileConfig", img(src= "trait_loader.gif", width= "100px")),
                                            tags$style(type='text/css', "#img_loader_FileConfig { text-align: center;}"),
                                            div(id="load_FileConfig_encours", strong("Chargement du fichier en cours ...")),
                                            tags$style(type='text/css', "#load_FileConfig_encours {text-align: center;}")
                        )
                        #tags$style(type='text/css', "#boxloader_SCD { position: fixed; left: 0%; top: 20%; z-index: 2;}")
                        ),
                        shinyjs::hidden(column(id= "gotoparameters_col", width= 2,
                                               div(
                                                 actionButton("gotoparameters", "Revenir aux paramètres", icon = icon("cog"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
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
              shinyFilesButton(id="load_sim_data",label="Charger le fichier des données simulées",title='Selectionner le fichier des données simulées', multiple = FALSE),
              #fileInput("file1", "Choose CSV File",
              #          accept = c(
              #            "text/csv",
              #            "text/comma-separated-values,text/plain",
              #            ".csv")
              #),
              shinyjs::hidden(div(id ='boxloader_SimData', width ="100px",
                                  div(id="img_loader_SimData", img(src= "trait_loader.gif", width= "100px")),
                                  tags$style(type='text/css', "#img_loader_SimData { text-align: center;}"),
                                  div(id="load_SimData_encours", strong("Chargement du fichier en cours ...")),
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
                           'indicateur', label = "Indicateur", choices = c("Stock exploitable", "Volume exploitable", "Nombre d’arbres", "Volume d'arbres", "Surface terrière", "Biomasse", "Taux de reconstitution du stock"), width = NULL
                         )
                  )
                ),
                fluidRow(id="block_groupe_espece",
                         column(id="groupe_espece_indicateur_col", width =  4,
                                selectizeInput(
                                  'groupe_espece_indicateur', label = "Groupe d'espèces", multiple= TRUE, choices = c(221, 222, 223, 224, 225),
                                  options = list(create = FALSE), width = NULL
                                )
                         ),
                         
                         column(width =  4, id = "selectGEIAll_col",
                                #fileInput('file_groupe_espece', 'sauvegarder une selection',
                                #         accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                                div(
                                  checkboxInput("selectGEIAll", strong("Toutes les espèces"), value = FALSE, width = NULL)
                                )
                         ),
                         tags$style(type='text/css', "#selectGEIAll_col {padding-top:2%}")
                ),
                #strong("Plage de diametre"),
                fluidRow(id="block_class_diam",
                         #useShinyjs(),
                         column(width =  2,
                                radioButtons("plageClass", label = "Plage de diametre",
                                             choices = list("cm" = 1, "classe" = 2), inline = TRUE)),
                         column(id="diamMinCol",  width =  2,
                                numericInput("diamMin", "Diam Min", value=0, min = NA, max = NA, step = NA)
                         ),
                         column(id="diamMaxCol", width =  2,
                                numericInput("diamMax", "Diam Max", value=0, min = NA, max = NA, step = NA)
                         ),
                         shinyjs::hidden(column(id="classMinCol",  width =  2,
                                                numericInput("classMin", "Classe Min", value=0, min = NA, max = NA, step = NA)
                         )),
                         shinyjs::hidden(column(id="classMaxCol", width =  2,
                                                numericInput("classMax", "Classe Max", value=0, min = NA, max = NA, step = NA)
                         ))
                ),
                fluidRow(
                  column(width = 12,
                         sliderInput("yearrange_indicateur", label = "Plage d'années", min = 2010, 
                                     max = 2030, value = c(2015, 2025), step=1, sep="")
                  )
                ),
                fluidRow(
                  column(width =  2,
                         actionButton("afficher_indicateur", strong("Visualiser"), icon = icon("eye"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4;")),
                  tags$style(type='text/css', "#afficher_indicateur:hover { color:#000 !important;border-color:#979494 !important;}")    
                ),
                shinyjs::hidden(div(id ='boxloader_IND', width ="100px",
                                    div(id="img_loader_IND", img(src= "trait_loader.gif", width= "100px")),
                                    tags$style(type='text/css', "#img_loader_IND { text-align: center;}"),
                                    div(id="load_IND_encours", strong("Traitement en cours ...")),
                                    tags$style(type='text/css', "#load_IND_encours {text-align: center;}")
                )
                #tags$style(type='text/css', "#boxloader_SCD { position: fixed; left: 0%; top: 20%; z-index: 2;}")
                )
            ),
            tabBox( width = NULL,
                    tabPanel(h5(strong("Représentation graphique")),
                             shinyjs::hidden(fluidRow(id = "bloc_graphique_Indicateur",
                               box(width = 12, 
                                   shinyjs::hidden(fluidRow(id = "bloc_export_indicateur_rg",
                                     column(width =2,
                                            selectInput(
                                              'extension',label = NULL, choices = c("png", "jpeg", "pdf"), width = NULL
                                            )
                                     ),
                                     column(width =2,
                                            downloadButton('download_indic_rg', strong('Exporter')),
                                            tags$style(type='text/css', "#download_indic_rg { color:#000 !important;border-color:#979494 !important;}")
                                     )
                                   )),
                                   
                                   plotOutput("plot_indicateur",height="500px"),
                                   status = "primary")
                             ))
                    ),
                    tabPanel(h5(strong("Données")),
                             shinyjs::hidden(
                               div(id = "bloc_donnees_Indicateur",
                                   shinyjs::hidden(fluidRow(id = "bloc_export_indicateur_data",
                                                            column(width =2,
                                                                   selectInput(
                                                                     'extensionData',label = NULL, choices = c("csv", "pdf"), width = NULL
                                                                   )
                                                            ),
                                                            column(width =2,
                                                                   downloadButton('download_indic_data', strong("Exporter")),
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
                         'groupe_espece_strDia', label = "Groupe d'espèces", multiple= TRUE, choices = c(221, 222, 223, 224, 225),
                         options = list(create = FALSE), width = NULL
                       )
                ),
                column(width =  4, id= "selectGESDAll_col",
                       checkboxInput("selectGESDAll", strong("Toutes les espèces"), value = FALSE, width = NULL)
                ),
                tags$style(type='text/css', "#selectGESDAll_col {padding-top:2%}")
              ),
              fluidRow(
                column(width =  6,
                       radioButtons("whatSD", label = "Type de structure diamétrique",
                                    choices = list("Cumulé" = 1, "Repartition par classe" = 2), inline = TRUE)
                )
              ),
              shinyjs::hidden(fluidRow(id="slider_SD",
                                       column(width =12  ,
                                              sliderInput("yearslider_strDiam", label = "Année", min = 2010, 
                                                          max = 2030, value =  2025, step=1, sep="")
                                       )
              )), 
              fluidRow(
                column(width = 4, 
                       actionButton("plot_SCD", strong("Visualiser"), icon = icon('eye'), style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                       tags$style(type='text/css', "#plot_SCD:hover { color:#000 !important;border-color:#979494 !important;}")
                )
                
              ),
              shinyjs::hidden(div(id ='boxloader_SCD', width ="100px",
                                  div(id="img_loader_SCD", img(src= "trait_loader.gif", width= "100px")),
                                  tags$style(type='text/css', "#img_loader_SCD { text-align: center;}"),
                                  div(id="load_SCD_encours", strong("Traitement en cours ...")),
                                  tags$style(type='text/css', "#load_SCD_encours {text-align: center;}")
              )
              )
            ),
            tabBox( width = NULL,
                    tabPanel(h5(strong("Représentation graphique")), 
                             shinyjs::hidden(fluidRow(id = "bloc_graphique_SCD",
                               box(width = 12, 
                                   shinyjs::hidden(fluidRow(id= "bloc_export_sd_rg",
                                     column(width =2,
                                            selectInput(
                                              'extensionSD',label = NULL, choices = c("png", "jpeg", "pdf"), width = NULL
                                            )
                                     ),
                                     column(width =2,
                                            downloadButton('download_strucDiam_rg', strong("Exporter")),
                                            tags$style(type='text/css', "#download_strucDiam_rg { color:#000 !important;border-color:#979494 !important;}")
                                     )
                                   )),
                                   plotOutput("plot_strDia",height="500px"),
                                   status = "primary")
                             ))
                    ),
                    tabPanel(h5(strong("Données")),
                             shinyjs::hidden(
                               div(id = "bloc_donnees_SCD",
                                   shinyjs::hidden(fluidRow(id = "bloc_export_sd_data",
                                                            column(width =2,
                                                                   selectInput(
                                                                     'extensionSDData',label = NULL, choices = c("csv", "pdf"), width = NULL
                                                                   )
                                                            ),
                                                            column(width =2,
                                                                   downloadButton('download_strucDiam_data', strong("Exporter")),
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
    tabItem(tabName = "parametres",
            box(width = "100%",
                fluidRow(
                  column(width =  6,
                         radioButtons("action_indicateur", label = NULL,
                                      choices = list("Nouvel indicateur" = 1, "MAJ indicateur existant" = 2), inline = TRUE)
                  )
                ),
                shinyjs::hidden(
                  div(id="update_indicateur",
                      fluidRow(
                        column(width= 4,
                               selectInput(
                                 'nom_indicateur_update', label = "Nom de l'indicateur", choices = c(), width = NULL
                               )
                               
                        )), 
                      fluidRow(
                        column(width= 8,
                               helpText(strong("Expression mathématique")),
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
                                 actionButton("update_indicateur_btn", strong("Modifier"), icon = icon('edit'), style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                                 tags$style(type='text/css', "#update_indicateur_btn:hover { color:#000 !important;border-color:#979494 !important;}")
                               ),
                               div(style="display:inline-block",
                                 actionButton("delete_indicateur_btn", strong("Supprimer"), icon = icon('trash'), style="color: #fff; background-color: red; border-color: red;"),
                                 tags$style(type='text/css', "#delete_indicateur_btn:hover { color:#000 !important;border-color:#979494 !important;}")
                               )
                               
                        )
                      ),
                      shinyjs::hidden(div(id ='boxloader_update_IND', width ="100px",
                                          div(id="img_loader_update_IND", img(src= "trait_loader.gif", width= "100px")),
                                          tags$style(type='text/css', "#img_loader_update_IND { text-align: center;}"),
                                          div(id="load_update_IND_encours", strong("Mise à jour en cours ...")),
                                          tags$style(type='text/css', "#load_update_IND_encours {text-align: center;}")
                      )
                      #tags$style(type='text/css', "#boxloader_SCD { position: fixed; left: 0%; top: 20%; z-index: 2;}")
                      )
                  )
                ),
                div(id="new_indicateur",
                    fluidRow(
                      column(width= 4,
                             textInput("nom_indicateur_new", "Nom de l'indicateur", value="", placeholder = "Nom de l'indicateur")
                             #textInput("nom_fonction_new", "Nom de la fonction", value="", placeholder = "Nom de la fonction")
                             #textInput("variable_indicateur_new", "Nom de la variable", value="", placeholder = "Nom de la variable")
                      )
                    ),
                    fluidRow(
                      column(width= 8,
                             helpText(strong("Expression mathématique")),
                             aceEditor("fonction_indicateur_new", mode="r", theme="chrome", vimKeyBinding = FALSE,
                                       readOnly = FALSE, height = "200px", fontSize = 12, debounce = 1000,
                                       wordWrap = FALSE, showLineNumbers = TRUE, highlightActiveLine = TRUE,
                                       selectionId = NULL, cursorId = NULL, hotkeys = NULL,
                                       autoComplete = c("disabled", "enabled", "live"), autoCompleteList = NULL)
                             #tags$textarea(id="fonction_indicateur_new", rows=6, cols=80)
                             #textInput("fonction_indicateur_new", "Expression mathématique", value="", placeholder = "Nom de la fonction")
                      )
                    ),
                    br(),
                    fluidRow(id="action_add_IND",
                      column(width= 2, 
                             div(
                               actionButton("ajout_indicateur_btn", strong("Ajouter"), icon = icon('plus'), style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                               tags$style(type='text/css', "#ajout_indicateur_btn:hover { color:#000 !important;border-color:#979494 !important;}")
                             )
                             
                      )
                    ),
                    shinyjs::hidden(div(id ='boxloader_add_IND', width ="100px",
                            div(id="img_loader_add_IND", img(src= "trait_loader.gif", width= "100px")),
                            tags$style(type='text/css', "#img_loader_add_IND { text-align: center;}"),
                            div(id="load_add_IND_encours", strong("Enregistrement en cours ...")),
                            tags$style(type='text/css', "#load_add_IND_encours {text-align: center;}")
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