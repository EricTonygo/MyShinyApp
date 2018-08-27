# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
rm(list = ls())
pkg <- c("shiny", "shinydashboard", "rhandsontable", "data.table", "flexmix", 
         "ggplot2", "DT", "shinyBS", "Rcpp", "shinyAce", "shinyjs", "shinyFiles", "rmarkdown")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)
}

library(shinydashboard)
library(rhandsontable)
library(data.table)
library(flexmix)
library(ggplot2)
library(DT)
library(Rcpp)
assign("DataFolder",  paste0(normalizePath(path="~"), "\\DafSim"), envir = .GlobalEnv)
assign("ParamsDynFile",  "", envir = .GlobalEnv)
assign("CurrentResultSimDataFile", "", envir = .GlobalEnv)
assign("DataStartInit", NULL, envir = .GlobalEnv)
assign("DataStartColHeaders", NULL, envir = .GlobalEnv)
assign("DataStartHeight", NULL, envir = .GlobalEnv)
assign("SpeciesTraitsSim", NULL, envir = .GlobalEnv)
assign("IndicatorsFolder", NULL, envir = .GlobalEnv)
assign("SimSiteNameFile", NULL, envir = .GlobalEnv)
assign("SimSiteName", NULL, envir = .GlobalEnv)
source("translations.R",local=T, encoding = 'UTF-8')
source('R/FormattingMBaiki2017.R')
source("R/ClasseDiamSTAGB.R")
source("R/selectData.R")
source("R/InferFCM.R")
source("MyAppInterfaceWithSimulator.R")
source("algoExtractIndicator.R")
source("buildListOfIndicator.R",local=T, encoding = 'UTF-8')
# sourceCpp("simulation.cpp")
options(shiny.maxRequestSize=10000*1024^2)
dir.exists <- function(d) {
  de <- file.info(d)$isdir
  ifelse(is.na(de), FALSE, de)
}


shinyServer(function(input, output, session){ 
  session$onSessionEnded(function() {
    stopApp()
  })
  ############################## Creation du dossier de sauvegarde des données de la simulation ##############################
  DataSimFolder = path.expand(path="~/DafSim/DonneesSimulation")
  if(!dir.exists(DataSimFolder)) {
    dir.create(DataSimFolder, recursive = TRUE)
  }
  Noms1=paste("DonneesSimulation",list.files("DonneesSimulation"),sep='/')
  Noms2=paste(DataSimFolder,list.files("DonneesSimulation"),sep='/')
  file.copy(Noms1,Noms2, overwrite = TRUE) 
  
  ############################## Creation du dossier de sauvegarde des données de la dynamique ##############################
  DataDynFolder = path.expand(path="~/DafSim/DonneesDynamique")
  if(!dir.exists(DataDynFolder)) {
    dir.create(DataDynFolder, recursive = TRUE)
  }
  Noms1=paste("DonneesDynamique",list.files("DonneesDynamique"),sep='/')
  Noms2=paste(DataDynFolder,list.files("DonneesDynamique"),sep='/')
  file.copy(Noms1,Noms2, overwrite = TRUE) 
  
  ############################## Creation du dossier des données de départ csv ##############################
  DataStartCSVFolder = path.expand(path="~/DafSim/DonneesDepart")
  if(!dir.exists(DataStartCSVFolder)) {
    dir.create(DataStartCSVFolder, recursive = TRUE)
  }
  Noms1=paste("DonneesDepart",list.files("DonneesDepart"),sep='/')
  Noms2=paste(DataStartCSVFolder,list.files("DonneesDepart"),sep='/')
  file.copy(Noms1,Noms2, overwrite = TRUE)
  
  ############################## Creation du dossier de sauvegarde des données simulées ##############################
  ResultsSimDataFolder = path.expand(path="~/DafSim/ResultatsSimulation")
  if(!dir.exists(ResultsSimDataFolder)) {
    dir.create(ResultsSimDataFolder, recursive = TRUE)
  }
  
  ############################## Création du fichier de sauvegarde des scénarios d'exploitation ##############################
  ScenariosLoggingFolder = path.expand(path="~/DafSim/ScenariosExploitation")
  if(!dir.exists(ScenariosLoggingFolder)) {
    dir.create(ScenariosLoggingFolder, recursive = TRUE)
  }
  
  ############################## Création du fichier de sauvegarde des données pour l'export ##############################
  DataExportsFolder = path.expand(path="~/DafSim/DonneesExports")
  if(!dir.exists(DataExportsFolder)) {
    dir.create(DataExportsFolder, recursive = TRUE)
  }
  
  ############################## Base de données de fichiers RDS de Simulation des sites pour les parcelles ##############################
  IndicatorsFolder <<- path.expand(path="~/DafSim/Indicateurs")
  if(!dir.exists(IndicatorsFolder)) {
    dir.create(IndicatorsFolder, recursive = TRUE)
  }
  Noms1=paste("Indicateurs",list.files("Indicateurs"),sep='/')
  Noms2=paste(IndicatorsFolder,list.files("Indicateurs"),sep='/')
  file.copy(Noms1,Noms2, overwrite = TRUE) 
  #volumes <- getVolumes()
  ##################################################################################################################################
  #initialisation du dossier par defaut dans lequel se feront la sauvegarde et le chargement des paramètrages des simulations
  #contenant des scéarios d'exploitation forestière et les autres parmètres de la simulation
  #Puis initialisation des fonctions de sauvegarde et chargement des fichiers au format .RData
  ##################################################################################################################################
  strEval = paste0('roots = c("',uiLoggingScenario,'"=ScenariosLoggingFolder)')
  eval(parse(text = strEval))
  shinyFileSave(input, 'save_config', session=session, roots=roots, restrictions = system.file(package = "base"))
  #shinyFileSave(input, 'save_start_data', session=session, roots=roots, restrictions = system.file(package = "base"))
  shinyFileChoose(input, 'load_config', session=session,roots=roots, filetypes=c('', 'RData'))

  strEval = paste0('roots2 = c("',uiDynamicData,'"=DataDynFolder)')
  eval(parse(text = strEval))
  shinyFileChoose(input, 'load_dyn', session=session,roots=roots2, filetypes=c('', 'rds'))
  
  strEval = paste0('roots3 = c("',uiSimulatingResults,'"=ResultsSimDataFolder)')
  eval(parse(text = strEval))
  shinyFileSave(input, 'lancer_sim', session=session, roots=roots3, restrictions = system.file(package = "base"))
  shinyFileChoose(input, 'load_sim_data', session=session,roots=roots3, filetypes=c('', 'RData'))
  ##################################################################################################################################
  #Initialisation des variables globales dont nous aurons besoin au cours de l'excécution du programme
  ##################################################################################################################################
  assign("DataToExport", data.table(),envir = .GlobalEnv)
  assign("PlotToExport", ggplot(), envir = .GlobalEnv)
  assign("PlotSTToExport", ggplot(), envir = .GlobalEnv)
  assign("ListOfIndicators", buildListIndicator(), envir = .GlobalEnv)
  assign("alphaInd", NULL, envir = .GlobalEnv)
  assign("choicesParcelles", NULL, envir = .GlobalEnv)
  assign("hidable", FALSE, envir = .GlobalEnv)
  
  getSitesListForParcelle <- function(){
    Sites = list.files(DataSimFolder)
    for(i in 1:length(Sites)){
      Sites[i] = unlist(strsplit(Sites[i], ".", fixed = TRUE))[1]
    }
    Sites = c(getCSVSiteName(), Sites)
    return(Sites)
  }
  
  ################ Function renvoyant le nom à utiliser pour le site prevenant d'un fichier CSV ################################
  getCSVSiteName <- function(){
    return (uiFromCSVFile)
  }
  
  ################ Formatting simulation and dynamique data correctly ################################
  formattingSimData <- function(ParamsSim = NULL){
    if(!is.null(ParamsSim)){
      if("Nom.sp"%in%names(ParamsSim)){
        ParamsSim$Nom.sp = as.character(ParamsSim$Nom.sp)
        ParamsSim$Nom.sp = as.factor(ParamsSim$Nom.sp)
      }
      if("Id.sp"%in%names(ParamsSim)){
        ParamsSim$Id.sp = as.character(ParamsSim$Id.sp)
        ParamsSim$Id.sp = as.factor(ParamsSim$Id.sp)
      }
      if("Id.zone"%in%names(ParamsSim)){
        ParamsSim$Id.zone = as.character(ParamsSim$Id.zone)
        ParamsSim$Id.zone = as.factor(ParamsSim$Id.zone)
      }
    }
    return(ParamsSim)
  }
  formattingDynData <- function(ParamsDyn = NULL){
    if(!is.null(ParamsDyn)){
      if("Nom.sp"%in%names(ParamsDyn$SpeciesTraits)){
        ParamsDyn$SpeciesTraits$Nom.sp = as.character(ParamsDyn$SpeciesTraits$Nom.sp)
        ParamsDyn$SpeciesTraits$Nom.sp = as.factor(ParamsDyn$SpeciesTraits$Nom.sp)
      }
      if("Id.sp"%in%names(ParamsDyn$SpeciesTraits)){
        ParamsDyn$SpeciesTraits$Id.sp = as.character(ParamsDyn$SpeciesTraits$Id.sp)
        ParamsDyn$SpeciesTraits$Id.sp = as.factor(ParamsDyn$SpeciesTraits$Id.sp)
      }
    }
    return(ParamsDyn)
  }
  
  ##################################################################################################################################
  #Function d'initialisation des inputs saisis par l'utilisateur.
  ##################################################################################################################################
  reset_user_data_input <- function (){
    
  }
  
  ##################################################################################################################################
  #Function de mise à jour du champs de saisie de la durée de la simulation
  ##################################################################################################################################
  update_input_simulation_duration <- function (){
    if(!is.null(SimSiteName) && SimSiteName != getCSVSiteName()){
      if(!is.null(input$anneefirstlogging) && !is.na(input$anneefirstlogging)  && !is.null(input$anneedebutSimAutreSite) && input$anneedebutSimAutreSite != "" && !is.null(input$nombrerotation) && !is.na(input$nombrerotation) && !is.null(input$dureerotation) && !is.na(input$dureerotation)){
        if(as.numeric(input$anneefirstlogging) > as.numeric(input$anneedebutSimAutreSite)){
          DurationOfSimulation = (as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSimAutreSite))+ as.numeric(input$nombrerotation)*as.numeric(input$dureerotation)
          updateNumericInput(session, "dureesimulation", value = DurationOfSimulation, min = DurationOfSimulation)
        }
      }
    }else{
      if(!is.null(input$anneefirstlogging) && !is.na(input$anneefirstlogging) && !is.null(input$anneedebutSim) && !is.na(input$anneedebutSim) && !is.null(input$nombrerotation) && !is.na(input$nombrerotation) && !is.null(input$dureerotation) && !is.na(input$dureerotation)){
        if(as.numeric(input$anneefirstlogging) > as.numeric(input$anneedebutSim)){
          DurationOfSimulation = (as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim))+ as.numeric(input$nombrerotation)*as.numeric(input$dureerotation)
          updateNumericInput(session, "dureesimulation", value = DurationOfSimulation, min = DurationOfSimulation)
        }
      }
    }
  }
  
  observeEvent(input$anneefirstlogging, {
    update_input_simulation_duration()
  })
  observeEvent(input$anneedebutSimAutreSite, {
    update_input_simulation_duration()
  })
  observeEvent(input$anneedebutSim, {
    update_input_simulation_duration()
  })
  observeEvent(input$nombrerotation, {
    update_input_simulation_duration()
  })
  observeEvent(input$dureerotation, {
    update_input_simulation_duration()
  })
  
  ##################################################################################################################################
  #Function de validation des inputs saisis par l'utilisateur.
  ##################################################################################################################################
  validate_user_data_input <- function (input, DataType){
    error_sim = FALSE
    error_sim_msg = NULL
    if(!is.null(DataType) && DataType == "parcelle"){
      if(!is.null(SimSiteName) && SimSiteName != getCSVSiteName()){
        if(!error_sim && input$Allparcelle == FALSE){
          if( is.null(input$parcelle) || input$parcelle ==""){
            error_sim = TRUE
            error_sim_msg = uiSelectSimulatingPlots
          } 
        } 
        if(!error_sim && !is.null(input$parcelle) || input$Allparcelle == TRUE){
          if(!error_sim && input$anneedebutSimAutreSite == ""){
            error_sim = TRUE
            error_sim_msg = uiSelectSimulatingStartYear
          }
          if(!error_sim && as.numeric(input$anneedebutSimAutreSite) > as.numeric(input$anneefirstlogging)){
            error_sim = TRUE
            error_sim_msg = uiFirstYearLoggingSupToSimulatingStartYear
          }
          if(!error_sim && ((as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSimAutreSite))+ as.numeric(input$nombrerotation)*as.numeric(input$dureerotation)) > as.numeric(input$dureesimulation)){
            error_sim = TRUE
            error_sim_msg = paste0(uiSimulatingDurationMustBeSupTo, " ",((as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSimAutreSite))+ as.numeric(input$nombrerotation)*as.numeric(input$dureerotation)))
          }
        }
      }else{
        if(!error_sim && is.null(input$file_data_start)){
          error_sim = TRUE
          error_sim_msg = uiLoadFileDataStart
        }
        if(!error_sim && input$surfaceparcelle == ""){
          error_sim = TRUE
          error_sim_msg = uiEnterSizeOfSurfaceOfParcel
        }
        if(!error_sim && input$anneedebutSim == ""){
          error_sim = TRUE
          error_sim_msg = uiEnterSimulatingStartYear
        }
        if(!error_sim && as.numeric(input$anneedebutSim) > as.numeric(input$anneefirstlogging)){
          error_sim = TRUE
          error_sim_msg =  uiFirstYearLoggingSupToSimulatingStartYear
        }
        if(!error_sim && ((as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim))+ as.numeric(input$nombrerotation)*as.numeric(input$dureerotation)) > as.numeric(input$dureesimulation)){
          error_sim = TRUE
          error_sim_msg = paste0(uiSimulatingDurationMustBeSupTo, " ",((as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim))+ as.numeric(input$nombrerotation)*as.numeric(input$dureerotation)))
        }
      }
    }else if(!error_sim && !is.null(DataType) && DataType == "sentier"){
      if(!error_sim && input$anneedebutSim == ""){
        error_sim = TRUE
        error_sim_msg = uiEnterSimulatingStartYear
      }
      if(!error_sim && input$espece == ""){
        error_sim = TRUE
        error_sim_msg = uiSelectSimulatingSpecie
      }
      if(!error_sim && as.numeric(input$anneedebutSim) > as.numeric(input$anneefirstlogging)){
        error_sim = TRUE
        error_sim_msg = uiFirstYearLoggingSupToSimulatingStartYear
      }
      if(!error_sim && ((as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim))+ as.numeric(input$nombrerotation)*as.numeric(input$dureerotation)) > as.numeric(input$dureesimulation)){
        error_sim = TRUE
        error_sim_msg = paste0(uiSimulatingDurationMustBeSupTo, " ",((as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim))+ as.numeric(input$nombrerotation)*as.numeric(input$dureerotation)))
      }
    }
    
    if(!error_sim && input$anneefirstlogging ==""){
      error_sim = TRUE
      error_sim_msg = uiEnterLoggingFirstYear
    }
    if(!error_sim && input$nombrerotation == ""){
      error_sim = TRUE
      error_sim_msg = uiEnterNumberOfRotation
    }
    if(!error_sim &&  input$dureerotation ==""){
      error_sim = TRUE
      error_sim_msg = uiEnterRotationDuration
    }
    if(!error_sim && input$dureesimulation ==""){
      error_sim = TRUE
      error_sim_msg = uiEnterSimulatingDuration
    }
    if(!error_sim && input$nbchain == ""){
      error_sim = TRUE
      error_sim_msg = uiEnterNumberOfSimulation
    }
    
    error = list(error_sim = error_sim, error_sim_msg = error_sim_msg)
    return (error)
  }
  
  
  
  ##################################################################################################################################
  #Function de validation des inputs saisis par l'utilisateur pour l'estimation 
  ##################################################################################################################################
  validate_user_data_input_for_tsc_dimako <- function (input){
    error_estim = FALSE
    error_estim_msg = NULL
    
    if(!error_estim && input$espece == ""){
      error_estim = TRUE
      error_estim_msg = uiSelectSimulatingSpecie
    }
    if(!error_estim && is.null(hot_to_r(input$data_logging))){
      error_estim = TRUE
      error_estim_msg = uiFillLoggingSpecieTable
    }
    
    if(!error_estim && is.null(hot_to_r(input$vector_number_per_diameter_class))){
      error_estim = TRUE
      error_estim_msg = uiFillSpecieNumberTable
    }
    
    if(!error_estim && input$tauxaccroissement == ""){
      error_estim = TRUE
      error_estim_msg = uiEnterGrowthRate
    }
    
    if(!error_estim && input$tauxmortalite == ""){
      error_estim = TRUE
      error_estim_msg = uiEnterMortalityRate
    }
    
    if(!error_estim &&  input$dureerotation ==""){
      error_estim = TRUE
      error_estim_msg = uiEnterRotationDuration
    }
    
    if(!error_estim && is.null(input$data_logging)){
      error_estim = TRUE
      error_estim_msg = uiFillLogginParametersOfSpecie
    }
    
    if(!error_estim && is.null(input$vector_damage)){
      error_estim = TRUE
      error_estim_msg = uiFillPostLoggingDamageByDiameterClass
    }
    
    if(!error_estim && is.null(input$vector_number_per_diameter_class)){
      error_estim = TRUE
      error_estim_msg = uiFillStartNumberOfSpecieByDiameterClass
    }
    error = list(error_estim = error_estim, error_estim_msg = error_estim_msg)
    return (error)
  }
  
  ##################################################################################################################################
  #updateSelectIndicator c'est la fonction permettant la mise à jour de la liste de selection des indicateurs
  #sur l'interface graphique lorsque qu'on ajoute, modifie, supprime un indicateur.
  #
  #loadUpdatedIndicator la fonction permettant de charger les indicateurs au lancement de la l'application
  ##################################################################################################################################
  updateSelectIndicator <-function(){
    vectorIndicator = c()
    for(i in 1:length(ListOfIndicators)){
      indicator= ListOfIndicators[[i]]
      vectorIndicator = c(vectorIndicator, indicator$NomInd)
    }
    updateSelectInput(session, "indicateur", choices = vectorIndicator,
                      selected = NULL)
  }
  
  loadUpdatedIndicator<-function(){
    
    tryCatch({
      IndicatorFile = paste0(IndicatorsFolder, "\\FileIndicateur.RData")
      if(!is.null(IndicatorFile) && file.exists(IndicatorFile) && file.access(names = IndicatorFile, mode = 4)==0){
        load(IndicatorFile)
      }else{
        listeIndicateur= list()
      }
      vectorIndicator = c()
      if(length(listeIndicateur) > 0){
        for(i in 1:length(listeIndicateur)){
          indicator= listeIndicateur[[i]]
          vectorIndicator = c(vectorIndicator, indicator$NomInd)
        }
      }
      
      updateSelectInput(session, "nom_indicateur_update", choices = vectorIndicator,
                        selected = NULL)
      rm(listeIndicateur)
    },error=function(e){
      showModal(modalDialog(
          title=uiError,
          size = "m",
          footer = modalButton(uiClose),
          e$message
        ) 
      )
    })
  }
  
  ################################Fonction associé à l'évènement du choix de la simulation de l'exploitation ou non ################
  observeEvent(input$simulate_logging,{
    tryCatch({
      if(input$simulate_logging == TRUE){
        shinyjs::show("simulate_logging_bloc")
      }else{
        shinyjs::hide("simulate_logging_bloc")
      }
    }, error=function(e){
      shinyjs::hide("simulate_logging_bloc")
    })
  })
  
  
  ################################Fonction associé à l'évènement du choix de l'indicteur à visualiser ################
  observeEvent(input$indicateur,{
    tryCatch({
      if(input$indicateur == uiUsableVolume || input$indicateur == uiUsableStock || input$indicateur == uiIventoryReplishmentRate){
        shinyjs::hide("block_class_diam")
      }else{
        shinyjs::show("block_class_diam")
      }
    }, error=function(e){
      #shinyjs::show("block_class_diam")
    })
  })
  
  
  ##################################################################################################################################
  #Fonction associée à l'évènement de sélection du nom d'un indicateur dans la liste des indicateurs modifiable
  ##################################################################################################################################
  observeEvent(input$nom_indicateur_update, {
    tryCatch({
      IndicatorFile = paste0(IndicatorsFolder, "\\FileIndicateur.RData")
      if(!is.null(IndicatorFile) && file.exists(IndicatorFile) && file.access(names = IndicatorFile, mode = 4)==0){
        load(IndicatorFile)
      }else{
        listeIndicateur= list()
      }
      trouve = FALSE
      i=1
      indicateur= list()
      while(!trouve && i<=length(listeIndicateur)){
        indicateur= listeIndicateur[[i]]
        if(indicateur$NomInd== input$nom_indicateur_update ){
          trouve=TRUE
        }
        i=i+1
      }
      #Mise à jour du textarea contenant la formule mathématique de l'indicateur necessaire pour son extraction
      if(trouve) updateAceEditor(session, "fonction_indicateur_update", indicateur$Func,
                      mode="r", theme="chrome",autoComplete = c("disabled", "enabled", "live"), autoCompleteList = NULL)
    },error=function(e){
      showModal(modalDialog(
          title=uiError,
          size = "m",
          footer = modalButton(uiClose),
          e$message
        ) 
      )
    })
    
  })
  
  ##################################################################################################################################
  #Fonction associée à l'évènement permettant de rentrer vers l'interface de definition des paramètres de la simulation après une 
  #simulation réussie ou un échec survenu aucours de la simulation.
  ##################################################################################################################################
  observeEvent(input$gotoparameters, {
    #createAlert(session, "alert", "exampleAlert", style = "success",content = "Echec de la simulation.", append = FALSE)
    shinyjs::hide('boxloader')
    shinyjs::hide('image_failure')
    shinyjs::hide('sim_failure')
    shinyjs::show('param_sim_logging')
    shinyjs::show('action_save_sim_file')
    shinyjs::show('lancer_sim_col')
    shinyjs::hide('gotoparameters_col')
    
  })
  
  ################################ Initialize table of diameter class list #########################
  initDiameterClassList <- function(){
    classDiamList = data.frame(classesDiametre = rep("", 8), stringsAsFactors = F)
    colHeaders = c("Bornes inférieures")
    output$table_classes_diam <-rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(classDiamList, colHeaders = colHeaders, selectCallback = TRUE, useTypes = TRUE, width = "100%", height = 240) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE) %>%
        #hot_cols(columnSorting = TRUE, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
        hot_col(col = 1, halign = "htCenter")%>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
  }
  ################################ Event of saving classe diam #########################
  observeEvent(input$save_classesDiam_btn, {
     tryCatch({
      ClassesDiam = hot_to_r(input$table_classes_diam)$classesDiametre;
      ClassesDiam = sort(as.numeric(ClassesDiam[!ClassesDiam%in%c("")]))
      if(length(ClassesDiam) >= 2){
        FormattedClassesDiam = c()
        for(iter in 1:(length(ClassesDiam)-1)){
          MyClasseDiam = paste0('[', ClassesDiam[iter],', ', ClassesDiam[iter+1], '[')
          FormattedClassesDiam = c(FormattedClassesDiam, MyClasseDiam)
        }
        MyClasseDiam = paste0('[', ClassesDiam[length(ClassesDiam)],', Inf[')
        FormattedClassesDiam = c(FormattedClassesDiam, MyClasseDiam)
        VarNbArbres = data.frame(classes= FormattedClassesDiam, include = rep(TRUE, length(ClassesDiam)), stringsAsFactors = F)
        VarEffC = data.frame(classes=FormattedClassesDiam, include = rep(TRUE, length(ClassesDiam)), stringsAsFactors = F)
        output$table_Nb_Arbres <-rhandsontable::renderRHandsontable({
          rhandsontable::rhandsontable(VarNbArbres, colHeaders = c("Classe", "Inclure"), selectCallback = TRUE, useTypes = TRUE, width = "100%", height = 220) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE) %>%
            hot_cols(columnSorting = TRUE, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
            hot_col(col = 1:2, halign = "htCenter")
        })
        output$table_EffC <-rhandsontable::renderRHandsontable({
          rhandsontable::rhandsontable(VarEffC, colHeaders = c("Classe", "Inclure"), selectCallback = TRUE, useTypes = TRUE, width = "100%", height = 220) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE) %>%
            hot_cols(columnSorting = TRUE, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
            hot_col(col = 1:2, halign = "htCenter")
        })
      }
      
    }, error = function (e){

    })
  })
  
  
  ################################ Uploading file data campagnes ##############################
  uploadFileDataCampagnes <- reactive({
    inFile <- input$file_data_campagnes
    if (!is.null(inFile)){
      read.csv2(inFile$datapath)
    }else{
      return(NULL)
    }
  })
  
  observeEvent(input$file_data_campagnes, {
    tryCatch({
      shinyjs::hide('bloc_table_data_campagnes')
      shinyjs::hide('bloc_table_parcelles_campagnes_selected')
      shinyjs::hide('bloc_table_species_traits')
      shinyjs::show('boxloader_FileDataCampagnes')
      dataCampagnes = uploadFileDataCampagnes()
      colHeaders = names(dataCampagnes)
      output$table_data_campagnes <-rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(dataCampagnes, colHeaders = colHeaders, selectCallback = TRUE, useTypes = TRUE, width = "1000px", height = 250) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE) %>%
          hot_cols(columnSorting = TRUE, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
          hot_col(col = 2:length(colHeaders), halign = "htCenter")
      })
      ListeCampagnes=sort(as.numeric(unique(as.character(dataCampagnes$Id.campagne0))))
      ListeParcelles=sort(as.numeric(unique(as.character(dataCampagnes$Id.zone))))
      TableParcellesCampagnesSelected = matrix(data = rep(TRUE, length(ListeParcelles)*length(ListeCampagnes)), nrow = length(ListeParcelles), ncol = length(ListeCampagnes), dimnames = list(ListeParcelles, ListeCampagnes))
      output$table_parcelles_campagnes_selected <-rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(TableParcellesCampagnesSelected, selectCallback = TRUE, useTypes = TRUE, width = "1000px", height = 250) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE) %>%
          hot_cols(columnSorting = TRUE, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
           hot_col(col = 1:ncol(TableParcellesCampagnesSelected), halign = "htCenter")
      })
      SpeciesTraitsInferTmp = dataCampagnes[c("Id.sp", "Nom.sp")]
      SpeciesTraitsInfer= SpeciesTraitsInferTmp[!duplicated(SpeciesTraitsInferTmp[, 1]),]
      SpeciesTraitsInfer = data.frame(SpeciesTraitsInfer[order(SpeciesTraitsInfer$Id.sp),], DME=80, WSG="", stringsAsFactors = F)
      SpeciesTraitsInfer$Nom.sp = as.character(SpeciesTraitsInfer$Nom.sp)
      row.names(SpeciesTraitsInfer) <- c(1:length(SpeciesTraitsInfer$Id.sp))
      colHeaders2 = names(SpeciesTraitsInfer)
      output$table_species_traits <-rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(SpeciesTraitsInfer, colHeaders = colHeaders2, selectCallback = TRUE, useTypes = TRUE, width = "1000px", height = 250) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE) %>%
          hot_cols(columnSorting = TRUE, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
          hot_col(col = 1:length(colHeaders2), halign = "htCenter")
      })
      shinyjs::hide('boxloader_FileDataCampagnes')
      shinyjs::show('bloc_table_data_campagnes')
      shinyjs::show('bloc_table_parcelles_campagnes_selected')
      shinyjs::show('bloc_table_species_traits')
    }, error= function(e){
      shinyjs::hide('boxloader_FileDataCampagnes')
      shinyjs::show('bloc_table_data_campagnes')
      showModal(modalDialog(
        title=uiError,
        size = "m",
        footer = modalButton(uiClose),
        e$message
      )
      )
    })
  })
  ################################ Uploading file data campagnes ##############################
  uploadFileSpeciesTraits <- reactive({
    inFile <- input$file_species_traits
    if (!is.null(inFile)){
      read.csv2(inFile$datapath)
    }else{
      return(NULL)
    }
  })
  
  
  observeEvent(input$file_species_traits, {
    tryCatch({
      shinyjs::hide('bloc_table_species_traits')
      SpeciesTraitsInfer = uploadFileSpeciesTraits()
      colHeaders = names(SpeciesTraitsInfer)
      output$table_species_traits <-rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(SpeciesTraitsInfer, colHeaders = colHeaders, selectCallback = TRUE, useTypes = TRUE, width = "1000px", height = 250) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE) %>%
          hot_cols(columnSorting = TRUE, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
          hot_col(col = 1:length(colHeaders), halign = "htCenter")
      })
      shinyjs::show('bloc_table_species_traits')
    }, error= function(e){
      shinyjs::show('bloc_table_species_traits')
      showModal(modalDialog(
        title=uiError,
        size = "m",
        footer = modalButton(uiClose),
        e$message
      )
      )
    })
  })
  
  
  
  
  
  ################################ Calculate dynamic parameters ##############################
  observeEvent(input$calculate_param_dyn_btn, {
    # tryCatch({
      shinyjs::hide('calculate_param_dyn_col')
      shinyjs::show('boxloader_CalculateParamDyn')
      # DataCampagnesFormatted = hot_to_r(input$table_data_campagnes)
      # DataCampagnesFormatted = subset(DataCampagnesFormatted, !is.null(Id.sp)&!is.na(Id.sp)&Id.sp!="")
      browser()
      DataCampagnesFormatted = read.csv2("dataCampagnes2.csv")
      names(DataCampagnesFormatted) <- c("Id.zone", "Id.sp", "Nom.sp", "Diam0", "Diam1", "Id.campagne0", "Id.campagne1", "Surface.zone")
      DataCampagnesFormatted$Id.zone = as.factor(as.character(DataCampagnesFormatted$Id.zone))
      DataCampagnesFormatted$Id.sp = as.factor(as.character(DataCampagnesFormatted$Id.sp))
      DataCampagnesFormatted$Nom.sp = as.factor(as.character(DataCampagnesFormatted$Nom.sp))
      DataCampagnesFormatted$Diam0 = as.numeric(DataCampagnesFormatted$Diam0)
      DataCampagnesFormatted$Diam1 = as.numeric(DataCampagnesFormatted$Diam1)
      DataCampagnesFormatted$Id.campagne0 = as.numeric(DataCampagnesFormatted$Id.campagne0)
      DataCampagnesFormatted$Id.campagne1 = as.numeric(DataCampagnesFormatted$Id.campagne1)
      DataCampagnesFormatted$Surface.zone = as.numeric(DataCampagnesFormatted$Surface.zone)
    
      
      ListeParcellesCampangesMB=Table.Parcelles.Campagnes.selected.MBaiki(DataCampagnesFormatted)
      # SelectedParcelleCampagnes <-  hot_to_r(input$table_parcelles_campagnes_selected)
      # SelectedParcelleCampagnes = subset(SelectedParcelleCampagnes, SelectedParcelleCampagnes[[1]]%in%c(TRUE, FALSE))
      # In = as.numeric(as.vector(t(SelectedParcelleCampagnes)))
      # ListeCampagnes=sort(as.numeric(unique(as.character(DataCampagnesFormatted$Id.campagne0))))
      # ListeParcelles=as.factor(sort(unique(as.character(DataCampagnesFormatted$Id.zone))))
      # ListeParcellesCampangesMB=data.frame(expand.grid(Id.campagne=ListeCampagnes, Id.zone=ListeParcelles),In=In)
      SpeciesTraitsInfer = hot_to_r(input$table_species_traits)
      SpeciesTraitsInfer = subset(SpeciesTraitsInfer, !is.null(Id.sp)&!is.na(Id.sp)&Id.sp!="")
      browser()
      names(SpeciesTraitsInfer) <- c("Id.sp", "Nom.sp", "DME", "WSG")
      SpeciesTraitsInfer$Id.sp = as.factor(as.character(SpeciesTraitsInfer$Id.sp))
      SpeciesTraitsInfer$Nom.sp = as.factor(as.character(SpeciesTraitsInfer$Nom.sp))
      SpeciesTraitsInfer$DME = as.numeric(SpeciesTraitsInfer$DME)
      SpeciesTraitsInfer$WSG = as.numeric(SpeciesTraitsInfer$WSG)
      DataSelected=SelectData(DataCampagnesFormatted,ListeParcellesCampangesMB,SpeciesTraitsInfer)
      
      
      
      # Diameter classes in cm 
      ClassesDiam = hot_to_r(input$table_classes_diam)$classesDiametre
      ClassesDiam = sort(as.numeric(ClassesDiam[!ClassesDiam%in%c("")]))
      
      
      # name of the period used for plot
      Lab.period="years"
      
      # Selection des modÃ¨les des 3 processus
      Models=list(Recruitment="RecruitmentFlexmix",Growth="GrowthFlexmix",Mortality="MortalityFlexmix")
      
      # Selection des variables pour le modÃ¨le du processus de recrutement
      VarRecrutNbArbres = c(paste("NbArbres",ClassesDiam,sep=""))[hot_to_r(input$table_Nb_Arbres)$include]
      VarRecrutEffC = c(paste("EffC",ClassesDiam,sep=""))[hot_to_r(input$table_EffC)$include]
      
      VarRecrut=c(VarRecrutNbArbres,VarRecrutEffC)
      
      # Esp?ces ? ne pas regrouper automatiquement
      UserListApartSpecies = NULL#list(c("50"))
      
      NomFileSimData=input$sim_data_out_filename
      NomFileSimData =  ifelse(is.null(NomFileSimData) | is.na(NomFileSimData) | NomFileSimData=="", "DefaultSimDataFile", NomFileSimData)
      PathFilesSimData = paste0(DataSimFolder, '/', NomFileSimData)
      browser()
      DataInfered=InferFCM(DataSelected, ClassesDiam, Models, VarRecrut, UserListApartSpecies,  PathFilesSimData)
      
      NomFileDynParams=input$dyn_param_out_filename
      NomFileDynParams =  ifelse(is.null(NomFileDynParams) | is.na(NomFileDynParams) | NomFileDynParams=="", "DefaultDynParamsFile", NomFileDynParams)
      saveRDS(DataInfered,file=paste0(DataDynFolder, '/', NomFileDynParams,'.RDS'))
      
      showNotification("Inference successfully ended!", duration = 10, type = "message")
      shinyjs::hide('boxloader_CalculateParamDyn')
      shinyjs::show('calculate_param_dyn_col')
    # },error=function(e){
    #   
    # })
  })
  
  
  updateInputForVisualization <- function(ResultFCM = NULL){
    if(!is.null(ResultFCM)){
      StoreTime = ResultFCM$StoreTime
      DataType = ResultFCM$DataType
      ClassesDiam = ResultFCM$ClassesDiam
      ParamSim = ResultFCM$ParamSim
      
      NbClasseDiam = length(ClassesDiam)
      updateNumericInput(session, "diamMin", value = ClassesDiam[1],
                         min = 0, max = ClassesDiam[NbClasseDiam], step = 1)
      updateNumericInput(session, "diamMax", value = ClassesDiam[NbClasseDiam],
                         min = 0, max = ClassesDiam[NbClasseDiam], step = 1)
      updateNumericInput(session, "classMin", value = 1,
                         min = 1, max = NbClasseDiam, step = 1)
      updateNumericInput(session, "classMax", value = NbClasseDiam,
                         min = 1, max = NbClasseDiam, step = 1)
      updateSliderInput(session, "yearslider_strDiam", value = StoreTime[1], min = StoreTime[1], max = StoreTime[length(StoreTime)], step = 1)
      if(!is.null(DataType) &&  DataType == "sentier"){
        updateSelectizeInput(session, "groupe_espece_indicateur", choices = ParamSim$Essence,
                             selected = ParamSim$Essence,  server = TRUE)
        updateSelectizeInput(session, "groupe_espece_strDia", choices = ParamSim$Essence,
                             selected = ParamSim$Essence,  server = TRUE)
      }else if(!is.null(DataType)){
        updateSelectizeInput(session, "groupe_espece_indicateur", choices = ResultFCM$SpeciesTraits$Nom.sp,
                             selected = NULL,  server = TRUE)
        updateSelectizeInput(session, "groupe_espece_strDia", choices = ResultFCM$SpeciesTraits$Nom.sp,
                             selected = NULL,  server = TRUE)
      }
      updateSliderInput(session, "yearrange_indicateur", value = c(StoreTime[1], StoreTime[length(StoreTime)]),
                        min = StoreTime[1], max = StoreTime[length(StoreTime)], step = 1)
     
    }
    
  }

  ################################ Uploading file data start ##############################
  uploadFileData <- reactive({
    inFile <- input$file_data_start
    if (!is.null(inFile)){
      read.csv2(inFile$datapath)
    }else{
      return(DataStartInit)
    }
  })
  
  
  observeEvent(input$file_data_start, {
    tryCatch({
      if(!is.null(ParamsDynFile) && file.exists(ParamsDynFile) && file.access(names = ParamsDynFile, mode = 4)==0){
        ParamsDyn = formattingDynData(readRDS(ParamsDynFile))
        ClassesDiam = ParamsDyn$ClassesDiam
        shinyjs::hide('bloc_start_species_number')
        shinyjs::show('boxloader_FileDataStart')
        dataStart = uploadFileData()
        ClassesDiamDataStart = dataStart[[names(dataStart)[1]]]
        if(length(ClassesDiam) == length(ClassesDiamDataStart)){
          colHeaders = c("CL Diam", names(dataStart)[-1])
          ClassesDiamList = c()
          for(iter in 1:(length(ClassesDiam)-1)){
            CurrentClass = paste0("[", ClassesDiam[iter], ", ",ClassesDiam[iter+1], "[")
            ClassesDiamList = c(ClassesDiamList, CurrentClass)
          }
          CurrentClass = paste0("[", ClassesDiam[length(ClassesDiam)], ", Inf[")
          ClassesDiamList = c(ClassesDiamList, CurrentClass)
          dataStart[,1] <- ClassesDiamList
          dataStart[,-1] <- as.character(unlist(dataStart[,-1]))
          dataStart[,-1][is.na(dataStart[,-1])] <- ""
          output$data_start_species <-rhandsontable::renderRHandsontable({
            rhandsontable::rhandsontable(dataStart, colHeaders = colHeaders, selectCallback = TRUE, useTypes = TRUE, width = "1000px", height = 250) %>%
              hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE) %>%
              hot_cols(columnSorting = TRUE, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
              hot_col(col = "CL Diam", readOnly = TRUE) %>%
              hot_col(col = 2:length(colHeaders), halign = "htCenter")
          })
          
          dataStartSpeciesName = as.factor(names(dataStart)[-1])
          AbsentSpecies = as.character(dataStartSpeciesName[!dataStartSpeciesName%in%ParamsDyn$SpeciesTraits$Nom.sp])
          if(length(AbsentSpecies) > 0){
            replacedSpecies = data.frame(Name.sp = AbsentSpecies, SubstitutedSpecies = rep("", length(AbsentSpecies)), stringsAsFactors = FALSE)
            output$data_start_missing_species <-rhandsontable::renderRHandsontable({
              rhandsontable::rhandsontable(replacedSpecies, colHeaders = c(uiMissingSpecie, uiReplacedby), selectCallback = TRUE, useTypes = TRUE, width = "100%", height = 250) %>%
                hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE) %>%
                hot_cols(columnSorting = TRUE, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
                hot_col(col = 2, type = "dropdown", source = sort(ParamsDyn$SpeciesTraits$Nom.sp)) %>%
                hot_col(col = 1, readOnly = TRUE)
            })
            shinyjs::show('update_corresponding_data_perso_col')
          }
          if(!is.null(ParamsDyn) && !is.null(dataStartSpeciesName)){
            SpeciesTraitsSite = data.frame(Nom.sp = dataStartSpeciesName)
            loadTableToUpdateDataLogging(SpeciesTraitsSite, ParamsDyn$SpeciesTraits, CorrespondingSpecies= NULL)
          }
          shinyjs::hide('anneedebutSimAutreSite')
          shinyjs::show('anneedebutSim')
          showNotification(uiDataLoadedSuccessfully, duration = 10, type = "message")
        }else{
          showModal(modalDialog(
            title=uiError,
            size = "m",
            footer = modalButton(uiClose),
            uiSimClassDiamDoNotCorrespondThoseOfDynamic
          ) 
          )
        }
        
      }
      shinyjs::hide('boxloader_FileDataStart')
      shinyjs::show('bloc_start_species_number')
    },error=function(e){
      shinyjs::hide('boxloader_FileDataStart')
      shinyjs::show('bloc_start_species_number')
      showModal(modalDialog(
        title=uiError,
        size = "m",
        footer = modalButton(uiClose),
        e$message
      ) 
      )
    })
  })
  
  ################################ Load table corresponding species ###################################
  loadTableCorrespondingSpecies<-function(SpeciesTraitsRef, SpeciesTraitsSite){
      SpeciesTraitsNameRef = as.character(SpeciesTraitsRef$Nom.sp)
      SpeciesTraitsNameSite = as.character(SpeciesTraitsSite$Nom.sp[!SpeciesTraitsSite$Nom.sp%in%SpeciesTraitsRef$Nom.sp])
      if(length(SpeciesTraitsNameSite) > 0){
        CorrespondingSpecies = data.frame(Name.sp = SpeciesTraitsNameSite, SubstitutedSpecies = rep("", length(SpeciesTraitsNameSite)), stringsAsFactors = FALSE)
        NumberSpecies = length(CorrespondingSpecies$Name.sp)
        height = NULL
        if(NumberSpecies>= 1 && NumberSpecies<= 7){
          height =185
        }else if(NumberSpecies> 7 && NumberSpecies<= 10){
          height = 220
        }else{
          height = 300
        }
        output$data_corresponding_species <-rhandsontable::renderRHandsontable({
          rhandsontable::rhandsontable(CorrespondingSpecies, colHeaders = c(uiSimulatingSpecie, uiCorrespondingSpecie), selectCallback = TRUE, useTypes = TRUE, width = "100%", height = height) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE) %>%
            hot_cols(columnSorting = TRUE, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
            hot_col(col = 2, type = "dropdown", source = sort(SpeciesTraitsNameRef)) %>%
            hot_col(col = 1, readOnly = TRUE) 
        })
        CorrespondingSpecies$Name.sp = as.factor(CorrespondingSpecies$Name.sp)
        CorrespondingSpecies$SubstitutedSpecies = as.factor(CorrespondingSpecies$SubstitutedSpecies)
        return(CorrespondingSpecies)
      }else{
        shinyjs::hide('bloc_corresponding_species')
        return(NULL)
      }
  }
  
  
  ################################ Evénement lié à la mise à jour de la table data_logging ######################################################
  observeEvent(input$update_corresponding_data, {
    if(!is.null(ParamsDynFile) && file.exists(ParamsDynFile) && file.access(names = ParamsDynFile, mode = 4)==0){
      tryCatch({
        shinyjs::hide('div_UpdateCD')
        shinyjs::show('boxloader_UpdateCD')
        ParamsDyn = formattingDynData(readRDS(ParamsDynFile))
        ParamsSimSite = NULL
        if(!is.null(SimSiteName) && SimSiteName != getCSVSiteName()){
          if(!is.null(SimSiteNameFile) && file.exists(SimSiteNameFile) && file.access(names = SimSiteNameFile, mode = 4)==0){
            ParamsSimSite = readRDS(SimSiteNameFile)
            ParamsSimSite = formattingSimData(ParamsSimSite$SimData)
          }
        }
        if(!is.null(ParamsDyn) && !is.null(ParamsSimSite)){
          if (!is.null(input$data_corresponding_species)) {
            CorrespondingSpecies <-  hot_to_r(input$data_corresponding_species)
            if(!is.null(CorrespondingSpecies)){
              error_sim = FALSE
              error_sim_msg = NULL
              if(!error_sim && length(CorrespondingSpecies$SubstitutedSpecies[CorrespondingSpecies$SubstitutedSpecies == ""]) > 0){
                error_sim= TRUE
                error_sim_msg= uiSomeSpeciesInCorrespondenceTableDoNotHaveCorrespondingSpecie
              }
              if(!error_sim){
                CorrespondingSpecies$Name.sp = as.factor(CorrespondingSpecies$Name.sp)
                CorrespondingSpecies$SubstitutedSpecies = as.factor(CorrespondingSpecies$SubstitutedSpecies)
                SpeciesTraitsSite = data.frame(Nom.sp= as.factor(levels(ParamsSimSite$Nom.sp)))
                loadTableToUpdateDataLogging(SpeciesTraitsSite, ParamsDyn$SpeciesTraits, CorrespondingSpecies = CorrespondingSpecies)
                showNotification(uiSimulatingDataUpdatedSuccessfully, duration = 10, type = "message")
              }else{
                showModal(modalDialog(
                      title=uiError,
                      size = "m",
                      footer = modalButton(uiClose),
                      error_sim_msg
                    )
                )
              }
            }
          }
        }
        shinyjs::hide('boxloader_UpdateCD')
        shinyjs::show('div_UpdateCD')
        
      }, error= function(e){
        shinyjs::hide('boxloader_UpdateCD')
        shinyjs::show('div_UpdateCD')
        showModal(modalDialog(
          title=uiErrorWhileLoading,
          size = "m",
          footer = modalButton(uiClose),
          e$message
        )
        )
      })
    }
  })
  
  
  ################################ Evénement lié à la mise à jour de la table data_logging cas des données personnalisées provenant du fichier csv ######################################################
  observeEvent(input$update_corresponding_data_perso, {
    if(!is.null(ParamsDynFile) && file.exists(ParamsDynFile) && file.access(names = ParamsDynFile, mode = 4)==0){
      tryCatch({
        shinyjs::hide('div_UpdateCDPerso')
        shinyjs::show('boxloader_UpdateCDPerso')
        ParamsDyn = formattingDynData(readRDS(ParamsDynFile))
        if (!is.null(ParamsDyn) && !is.null(input$data_start_species)) {
          DataStart <-  hot_to_r(input$data_start_species)
          DataStart <- DataStart[,-1]
          if (!is.null(input$data_start_missing_species)) {
            CorrespondingSpecies <-  hot_to_r(input$data_start_missing_species)
            if(!is.null(CorrespondingSpecies)){
              error_sim = FALSE
              error_sim_msg = NULL
              if(!error_sim && length(CorrespondingSpecies$SubstitutedSpecies[CorrespondingSpecies$SubstitutedSpecies == ""]) > 0){
                error_sim= TRUE
                error_sim_msg= uiSomeSpeciesInMissingTableDoNotHaveCorrespondingSpecie
              }
              if(!error_sim){
                CorrespondingSpecies$Name.sp = as.factor(CorrespondingSpecies$Name.sp)
                CorrespondingSpecies$SubstitutedSpecies = as.factor(CorrespondingSpecies$SubstitutedSpecies)
                SpeciesTraitsSite = data.frame(Nom.sp = as.factor(names(DataStart)))
                loadTableToUpdateDataLogging(SpeciesTraitsSite, ParamsDyn$SpeciesTraits, CorrespondingSpecies = CorrespondingSpecies)
                showNotification(uiSimulatingDataUpdatedSuccessfully, duration = 10, type = "message")
              }else{
                showModal(modalDialog(
                  title=uiError,
                  size = "m",
                  footer = modalButton(uiClose),
                  error_sim_msg
                )
                )
              }
            }
          }
        }
        shinyjs::hide('boxloader_UpdateCDPerso')
        shinyjs::show('div_UpdateCDPerso')
        
        }, error= function(e){
          shinyjs::hide('boxloader_UpdateCD')
          shinyjs::show('div_UpdateCD')
          showModal(modalDialog(
            title=uiErrorWhileLoading,
            size = "m",
            footer = modalButton(uiClose),
            e$message
          )
          )
      })
    }
  })
  
  ################################ Mise à jour de la table data_logging avec les données du site de reférence ###################################
  loadTableToUpdateDataLogging<-function(SpeciesTraitsSite, SpeciesTraitsRef, CorrespondingSpecies=NULL){
    SpeciesTraitsSim = data.frame(Nom.sp=SpeciesTraitsSite$Nom.sp)
    SpeciesTraitsSim = merge(SpeciesTraitsSim, SpeciesTraitsRef, all.x=T, all.y=F)
    if (sum(is.na(SpeciesTraitsSim$Id.sp))>0 && !is.null(CorrespondingSpecies)){
      for(i in 1: length(CorrespondingSpecies$Name.sp)){
        SpeciesTraitsSim$Id.sp[as.character(SpeciesTraitsSim$Nom.sp) == as.character(CorrespondingSpecies$Name.sp[i])]= SpeciesTraitsRef$Id.sp[as.character(SpeciesTraitsRef$Nom.sp) == as.character(CorrespondingSpecies$SubstitutedSpecies[i])]
        SpeciesTraitsSim$WSG[as.character(SpeciesTraitsSim$Nom.sp) == as.character(CorrespondingSpecies$Name.sp[i])]= SpeciesTraitsRef$WSG[as.character(SpeciesTraitsRef$Nom.sp) == as.character(CorrespondingSpecies$SubstitutedSpecies[i])]
        SpeciesTraitsSim$DME[as.character(SpeciesTraitsSim$Nom.sp) == as.character(CorrespondingSpecies$Name.sp[i])]= SpeciesTraitsRef$DME[as.character(SpeciesTraitsRef$Nom.sp) == as.character(CorrespondingSpecies$SubstitutedSpecies[i])]
      }
    }
    SpeciesTraitsSim <<- formattingSimData(SpeciesTraitsSim)
    #if (sum(is.na(SpeciesTraitsSim$Id.sp))==0){
      loadTableDataSpeciesTraits(SpeciesTraitsSim)
    #}
  }
  
  ################################ Mise à jour du data frame SpeciesTraitsSim des espèces traités pour la simulation ###################################
  updateSpeciesTraitsSim<-function(SpeciesTraitsSite, SpeciesTraitsRef, CorrespondingSpecies=NULL){
    SpeciesTraitsSim = data.frame(Nom.sp=SpeciesTraitsSite$Nom.sp)
    SpeciesTraitsSim = merge(SpeciesTraitsSim, SpeciesTraitsRef, all.x=T, all.y=F)
    if (length(SpeciesTraitsSim$Id.sp[is.na(SpeciesTraitsSim$Id.sp)])>0 && !is.null(CorrespondingSpecies)){
      for(i in 1: length(CorrespondingSpecies$Name.sp)){
        SpeciesTraitsSim$Id.sp[SpeciesTraitsSim$Nom.sp == CorrespondingSpeciesName.sp[i]]= SpeciesTraitsRef$Id.sp[SpeciesTraitsRef$Nom.sp == CorrespondingSpecies$SubstitutedSpecies[i]]
        SpeciesTraitsSim$WSG[SpeciesTraitsSim$Nom.sp == CorrespondingSpeciesName.sp[i]]= SpeciesTraitsRef$WSG[SpeciesTraitsRef$Nom.sp == CorrespondingSpecies$SubstitutedSpecies[i]]
        SpeciesTraitsSim$DME[SpeciesTraitsSim$Nom.sp == CorrespondingSpeciesName.sp[i]]= SpeciesTraitsRef$DME[SpeciesTraitsRef$Nom.sp == CorrespondingSpecies$SubstitutedSpecies[i]]
      }
    }
    SpeciesTraitsSim <<- formattingSimData(SpeciesTraitsSim)
  }
  
  ################################ Simulation de la dynamique forestière ##############################
  observeEvent(input$lancer_sim,{
    if(!is.null(ParamsDynFile) && file.exists(ParamsDynFile) && file.access(names = ParamsDynFile, mode = 4)==0){
      error_sim= FALSE
      ParamsDyn = formattingDynData(readRDS(ParamsDynFile))
      inFile = parseSavePath(roots3, input$lancer_sim)
      Savepath = as.character(inFile$datapath)
      ParamsSimSite = NULL
      ParamsSimSiteAll = NULL
      SimSiteName <<- input$siteParcelle
      if(!is.null(SimSiteName) && SimSiteName != getCSVSiteName()){
        if(!is.null(SimSiteNameFile) && file.exists(SimSiteNameFile) && file.access(names = SimSiteNameFile, mode = 4)==0){
          ParamsSimSiteAll = readRDS(SimSiteNameFile)
          ParamsSimSite = formattingSimData(ParamsSimSiteAll$SimData)
        }
      }
      NbClasse = 0
      if(!is.null(ParamsDyn)){
        ClassesDiam = ParamsDyn$ClassesDiam
        NbClasse = length(ClassesDiam)
      }
      simulation.input <- reactive({
        ##################################################################################################################################
        #Validation des inputs saisis par l'utilisateur.
        ##################################################################################################################################
        user_input_validation = validate_user_data_input(input = input, DataType = ParamsDyn$DataType)
        error_sim = user_input_validation$error_sim
        error_sim_msg = user_input_validation$error_sim_msg
        ##################################################################################################################################
        #Recuperation des paramètres saisis par l'utilisateur
        ##################################################################################################################################
        tarifgenerique = input$tarifgenerique
        if(tarifgenerique ==""){
          tarifgenerique = "10^(-2.96+1.93*log10(d))"
        }
        
        if (!error_sim && !is.null(input$data_logging)) {
          DF  <-  hot_to_r(input$data_logging)
          DF_SAVE <- DF
          DF$tauxPrelevement = as.numeric(DF$tauxPrelevement)
          DF$coefRecollement = as.numeric(DF$coefRecollement)
          DF$dma = as.numeric(DF$dma)
          DF$dme = as.numeric(DF$dme)
          DF$densite = as.numeric(DF$densite)
          DF$tauxPrelevement[is.na(DF$tauxPrelevement) & !is.na(DF$dma)]=100
          DF$coefRecollement[is.na(DF$coefRecollement) & !is.na(DF$dma)]=100
          DF$tauxPrelevement[is.na(DF$tauxPrelevement)]=0
          DF$coefRecollement[is.na(DF$coefRecollement)]=0
          DF$dma[is.na(DF$dma)]= Inf
          DF$tauxPrelevement = DF$tauxPrelevement/100
          DF$coefRecollement = DF$coefRecollement/100
          DF$tarifcubage[DF$tarifcubage=="NA" | DF$tarifcubage==""]= tarifgenerique
          SpeciesTraits= data.frame(Nom.sp = as.factor(DF$nomEspece), WSG = DF$densite, DME= DF$dme, DMA=DF$dma, tauxPrelevement= DF$tauxPrelevement, coefRecollement = DF$coefRecollement, tarifs = DF$tarifcubage, TarifGenerique = tarifgenerique , stringsAsFactors = FALSE)
          SpeciesTraits = merge(SpeciesTraits, data.frame(Nom.sp = SpeciesTraitsSim$Nom.sp, Id.sp = SpeciesTraitsSim$Id.sp), all.x=T, all.y=T, sort = F)
          SpeciesTraits$Nom.sp = as.factor(as.character(SpeciesTraits$Nom.sp))
          SpeciesTraits$Id.sp = as.factor(as.character(SpeciesTraits$Id.sp))
          if(!error_sim && nrow(subset(SpeciesTraits, is.na(SpeciesTraits$DME))) >0){
            error_sim= TRUE
            error_sim_msg= uiSomeSpeciesDoNotHaveDME
          }
          if(!error_sim && nrow(subset(SpeciesTraits, is.na(SpeciesTraits$WSG))) >0){
            error_sim= TRUE
            error_sim_msg= uiSomeSpeciesDoNotHaveDensity
          }
          if(!error_sim && nrow(subset(SpeciesTraits, is.na(SpeciesTraits$Id.sp))) >0){
            error_sim= TRUE
            error_sim_msg= uiSomeSpeciesDoNotHaveIdentifiant
          }
          if(!error_sim && nrow(subset(SpeciesTraits, SpeciesTraits$DMA<SpeciesTraits$DME))!=0){
            error_sim= TRUE
            error_sim_msg= uiSomeSpeciesHaveDMALessDME
          }
          if(!error_sim && nrow(subset(SpeciesTraits,SpeciesTraits$tauxPrelevement<0|SpeciesTraits$tauxPrelevement>1))!=0){
            error_sim= TRUE
            error_sim_msg= uiRemovalRateMustBeBetween0And100
          }
          if(!error_sim && nrow(subset(SpeciesTraits,SpeciesTraits$tauxRecolement<0|SpeciesTraits$tauxRecolement>1))!=0){
            error_sim= TRUE
            error_sim_msg= uiHavestingRageMustBeBetween0And100
          }
        }else if(!error_sim){
          DF_SAVE <- NULL
          if(!is.null(SpeciesTraitsSim)){
            SpeciesTraits = SpeciesTraitsSim
            DF <- data.frame(nomEspece = SpeciesTraits$Nom.sp, codeEspece= SpeciesTraits$Id.sp, dme=SpeciesTraits$DME, dma=rep(Inf, length(SpeciesTraits$Id.sp)), tauxPrelevement=rep(0, length(SpeciesTraits$Id.sp)), coefRecollement=rep(0, length(SpeciesTraits$Id.sp)), tarifcubage=rep(tarifgenerique, length(SpeciesTraits$Id.sp)), densite=SpeciesTraits$WSG, stringsAsFactors = FALSE)
            SpeciesTraits= data.frame(Nom.sp = as.factor(DF$nomEspece), Id.sp= as.factor(DF$codeEspece), WSG = DF$densite, DME= DF$dme, DMA=DF$dma, tauxPrelevement= DF$tauxPrelevement, coefRecollement = DF$coefRecollement, tarifs = DF$tarifcubage, TarifGenerique = tarifgenerique , stringsAsFactors = FALSE)
            SpeciesTraits$Nom.sp = as.factor(as.character(SpeciesTraits$Nom.sp))
            SpeciesTraits$Id.sp = as.factor(as.character(SpeciesTraits$Id.sp))
          }
        }
        if(!error_sim && !is.null(SimSiteName) && SimSiteName != getCSVSiteName()){
          SpeciesTraitsNameMissing = as.character(SpeciesTraitsSim$Nom.sp[!SpeciesTraitsSim$Nom.sp%in%ParamsDyn$SpeciesTraits$Nom.sp])
          if (length(SpeciesTraitsNameMissing) > 0 && !is.null(input$data_corresponding_species)) {
            CorrespondingSpecies <-  hot_to_r(input$data_corresponding_species)
            if(sum(CorrespondingSpecies$SubstitutedSpecies == "") > 0){
              error_sim= TRUE
              error_sim_msg= uiSomeSpeciesInCorrespondenceTableDoNotHaveCorrespondingSpecie
            }
          }
        }
        if(!error_sim && !is.null(SimSiteName) && SimSiteName == getCSVSiteName()){
          DataStart = NULL
          DataStartMat= NULL
          if (!error_sim && !is.null(input$data_start_missing_species)) {
            MissingDataStartSpecies <-  hot_to_r(input$data_start_missing_species)
            if(!error_sim && sum(MissingDataStartSpecies$SubstitutedSpecies == "") > 0){
              error_sim= TRUE
              error_sim_msg= uiSomeSpeciesInMissingTableDoNotHaveCorrespondingSpecie
            }
            
            if (!error_sim && !is.null(input$data_start_species)) {
              DataStart <-  hot_to_r(input$data_start_species)
              DataStart <- as.data.frame(lapply(DataStart[,-1], function(x) as.numeric(x)))
              DataStartMat <- as.matrix(DataStart)
            }
          }
        }
        if (!error_sim && !is.null(input$vector_damage)) {
          VD  <-  hot_to_r(input$vector_damage)
          VD_SAVE <- VD
          VD[VD=="NA" | VD==""]=0
          
          v=c()
          for (i in 1:ncol(VD)){
            v = c(v,as.numeric(VD[[i]][1]))
          }
          
          if(length(v[which(v<0|v>100)])!=0){
            error_sim= TRUE
            error_sim_msg= "Le dommage en (%) doit être compris entre 0 et 100. Veuillez corriger ces informations"
          }
        }else if(!error_sim){
          VD  <-  NULL
          VD_SAVE <- VD
          v <-  rep(0, NbClasse)
        }
        if (!error_sim && !is.null(input$vector_number_per_diameter_class)) {
          VNUM  <-  hot_to_r(input$vector_number_per_diameter_class)
          VNUM_SAVE <- VNUM
          VNUM[VNUM=="NA" | VNUM==""]=0
          Effectifs=c()
          for (i in 1:ncol(VNUM)){
            Effectifs = c(Effectifs,as.numeric(VNUM[[i]][1]))
          }
          if(length(Effectifs[which(Effectifs<0)])!=0){
            error_sim= TRUE
            error_sim_msg= "Les effectifs de l'espèces par classe de diamètre doit être positif. Veuillez corriger vos effectifs"
          }
        }else if (!error_sim){
          VNUM  <-  NULL
          VNUM_SAVE <- VNUM
          Effectifs <-  rep(0, NbClasse)
        }
        if (!error_sim && !is.null(input$vector_tauxrecrutement_per_diameter_class)) {
          VTR  <-  hot_to_r(input$vector_tauxrecrutement_per_diameter_class)
          VTR_SAVE <- VTR
          VTR[VTR=="NA" | VTR==""]=0
          ClasseDiamWeights=c()
          for (i in 1:ncol(VTR)){
            ClasseDiamWeights = c(ClasseDiamWeights,as.numeric(VTR[[i]][1]))
          }
          if(length(ClasseDiamWeights[which(ClasseDiamWeights<0)])!=0){
            error_sim= TRUE
            error_sim_msg= "Les taux de recrutement par classe de diamètre doit être positif. Veuillez corriger vos effectifs"
          }
        }else if(!error_sim){
          VNUM  <-  NULL
          VNUM_SAVE <- VNUM
          ClasseDiamWeights <-  rep(0, NbClasse)
        }
        if(!error_sim){ 
          shinyjs::show('boxloader')
          shinyjs::show('loader_sim')
          shinyjs::show('sim_encours')
          shinyjs::hide('image_success')
          shinyjs::hide('sim_finish')
          shinyjs::hide('param_sim_logging')
          shinyjs::hide('actions_sim')
          shinyjs::hide('action_save_sim_file')
          assign("hidable", TRUE, envir = .GlobalEnv)
          print(hidable)
          nbchain = input$nbchain
          Starting.plot = NULL
          Surface.zone = NULL
          if(!is.null(ParamsDyn) && ParamsDyn$DataType == "parcelle"){
            if(!is.null(SimSiteName) && SimSiteName == getCSVSiteName()){
              Starting.plot = DataStartMat
              Surface.zone = as.numeric(input$surfaceparcelle)
            }else if(!is.null(SimSiteName)){
              if(input$Allparcelle == TRUE){
                if(!is.null(ParamsSimSite)){
                  Starting.plot= as.character(unique(ParamsSimSite$Id.zone))
                }
              }else{
                Starting.plot= input$parcelle
              }
            }
          }
          Aggregation = F
          if(!is.null(ParamsDyn) && ParamsDyn$DataType == "parcelle"){
            if(!is.null(SimSiteName) && SimSiteName != getCSVSiteName()){
              StartingDate = as.numeric(input$anneedebutSimAutreSite)
              Aggregation = input$aggregate_parcels
            }else if(!is.null(SimSiteName)){
              StartingDate = as.numeric(input$anneedebutSim)
            }
          }else if(!is.null(ParamsDyn) && ParamsDyn$DataType == "sentier"){
            StartingDate = as.numeric(input$anneedebutSim)
          }
          Essence = as.character(input$espece)
          RecruitmentRate = as.numeric(input$tauxrecrutement)
          Nb.rotation = as.numeric(input$nombrerotation)
          rotation = as.numeric(input$dureerotation)
          DelayLogging =as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim)
          Fin.simu = as.numeric(input$dureesimulation)
          vector_damage = v/100
          ClasseDiamWeights = ClasseDiamWeights/100
          
          Logging.intensity = rep(0, NbClasse)
          if(!is.null(ParamsDyn) && ParamsDyn$DataType == "sentier"){
            DMA = SpeciesTraits$DMA[SpeciesTraits$Id.sp == Essence]
            Tauxprelevement = SpeciesTraits$tauxPrelevement[SpeciesTraits$Id.sp== Essence]
            Logging.intensity[DMA <= ClassesDiam] = Tauxprelevement
            if(sum(DMA <= ClassesDiam) > 0 && !DMA %in% ClassesDiam){
              Id_cl = sum(DMA>ClassesDiam)
              Logging.intensity[Id_cl] = Tauxprelevement*((ClassesDiam[Id_cl+1] - DMA)/(ClassesDiam[Id_cl+1] - ClassesDiam[Id_cl]))
            }
          }
          SimulateLogging = input$simulate_logging
          if(SimulateLogging){
            Logging="T2.MBaiki"
          }else{
            Logging= "T0"
          }
          DelayAfterLogging = as.numeric(input$delayAfterLogging)
          DelayDynamicsPostExploitation = as.numeric(input$delayDynamicsPostExploitation)
          ##################################################################################################################################
          #Calcul des paramètres du modèle en utilisant les données réelles collectées sur les arbres et 
          #contenu dans la liste d'objets ParamsDyn contenant les paramètres de la dynamique
          ##################################################################################################################################
          ParamSim = list(Nb.rotation = Nb.rotation, rotation = rotation, Fin.simu = Fin.simu, SimulateLogging= SimulateLogging, DelayLogging = DelayLogging, DelayAfterLogging = DelayAfterLogging, DelayDynamicsPostExploitation = DelayDynamicsPostExploitation, nbchain = nbchain, vector_damage = vector_damage, RecruitmentRate = RecruitmentRate, Starting.plot = Starting.plot, Aggregation= Aggregation, Essence=Essence, Effectifs=Effectifs, ClasseDiamWeights= ClasseDiamWeights, Logging.intensity = Logging.intensity, StartingDate = StartingDate, nbchain = nbchain, MySpeciesTraits = SpeciesTraits, Logging = Logging, Tarifgenerique = tarifgenerique, ParamsSimSite = ParamsSimSite, SimSiteName= SimSiteName, SpeciesTraitsSim = SpeciesTraitsSim, Surface.zone=Surface.zone)
          ##################################################################################################################################
          #Simulation proprement dite
          ##################################################################################################################################
          ResultFCM= FCM(out.InferFCM=ParamsDyn, ParamSim)
          ##################################################################################################################################
          #Sauvegarde des resultats de la simulation dans le fichier précisé par l'utilisateur et d'autres paramètres
          #utilisable ultérieurement.
          ##################################################################################################################################
          Nb.period=ResultFCM$Nb.period
          max_Temps = max(ResultFCM$Simulations$Temps)+StartingDate
          StoreTime= c(StartingDate:max_Temps);
          ResultFCM$StoreTime = StoreTime
          ResultFCM$ParamSim = ParamSim
          ResultFCM$DataType = ParamsDyn$DataType
          ResultFCM$ClassesDiam = ParamsDyn$ClassesDiam
          ResultFCM$ParamsDynFile = ParamsDynFile
          ResultFCM$DateSim = Sys.Date()
          if(ParamsDyn$DataType == "parcelle"){
            ResultFCM$Site = ParamsSimSiteAll$Descriptif$site
            ResultFCM$Pays = ParamsSimSiteAll$Descriptif$pays
          }else if(ParamsDyn$DataType == "sentier"){
            ResultFCM$Site = ParamsDyn$Descriptif$site
            ResultFCM$Pays = ParamsDyn$Descriptif$pays
          }
          updateInputForVisualization(ResultFCM)
          save(ResultFCM, file = Savepath)
          ################# Mise à jour automatique du fichier des resultats de la simulation ###############################
          CurrentResultSimDataFile <<- Savepath
          updateInfosDescriptionSim(ResultFCM)
          ###################################################################################################################
          IndicatorFile = paste0(IndicatorsFolder, "\\FileIndicateur.RData")
          if(!is.null(IndicatorFile) && file.exists(IndicatorFile) && file.access(names = IndicatorFile, mode = 4)==0){
            load(IndicatorFile)
          }else{
            listeIndicateur= list()
          }
          updateSimulatingDataOfIndicator(listeIndicateur)
          shinyjs::hide('loader_sim')
          shinyjs::hide('sim_encours')
          shinyjs::show('image_success')
          shinyjs::show('sim_finish')
          shinyjs::show('actions_sim')
          shinyjs::show('action_save_sim_file')
          shinyjs::hide('lancer_sim_col')
          shinyjs::show('gotoparameters_col')
        }else{
          showModal(modalDialog(
            title=uiError,
            size = "m",
            footer = modalButton(uiClose),
            error_sim_msg
          ) 
          )
        }
      })
      tryCatch({
        print("Start Sim")
        simulation.input()
        print("End Sim")
      }, error=function(e){
        if(hidable){
          shinyjs::hide('loader_sim')
          shinyjs::hide('sim_encours')
          shinyjs::show('image_failure')
          shinyjs::show('sim_failure')
          shinyjs::show('actions_sim')
          shinyjs::show('action_save_sim_file')
          shinyjs::hide('lancer_sim_col')
          shinyjs::show('gotoparameters_col')
        }
        showModal(modalDialog(
          title=uiError,
          size = "m",
          footer = modalButton(uiClose),
          e$message
        )
        )
      })
    }else{
      showModal(modalDialog(
        title=uiError,
        size = "m",
        footer = modalButton(uiClose),
        uiPleaseLoadDynamicParameters
      )
      )
     }
  })
  
  observeEvent(input$lancer_sim_without_save_file,{
    if(!is.null(ParamsDynFile) && file.exists(ParamsDynFile) && file.access(names = ParamsDynFile, mode = 4)==0){
      ParamsDyn = formattingDynData(readRDS(ParamsDynFile))
      error_sim= FALSE
      error_sim_msg = NULL
      Savepath = paste(ResultsSimDataFolder, paste("DefaultSimFile", ParamsDyn$DataType,'RData', sep = '.'), sep = '/')
      ParamsSimSite = NULL
      ParamsSimSiteAll = NULL
      SimSiteName <<- input$siteParcelle
      if(!is.null(SimSiteName) && SimSiteName != getCSVSiteName()){
        if(!is.null(SimSiteNameFile) && file.exists(SimSiteNameFile) && file.access(names = SimSiteNameFile, mode = 4)==0){
          ParamsSimSiteAll = readRDS(SimSiteNameFile)
          ParamsSimSite = formattingSimData(ParamsSimSiteAll$SimData)
        }
      }
      NbClasse = 0
      if(!is.null(ParamsDyn)){
        ClassesDiam = ParamsDyn$ClassesDiam
        NbClasse = length(ClassesDiam)
      }
      simulation.input <- reactive({
        ##################################################################################################################################
        #Validation des inputs saisis par l'utilisateur.
        ##################################################################################################################################
        user_input_validation = validate_user_data_input(input = input, DataType = ParamsDyn$DataType)
        error_sim = user_input_validation$error_sim
        error_sim_msg = user_input_validation$error_sim_msg
        ##################################################################################################################################
        #Recuperation des paramètres saisis par l'utilisateur
        ##################################################################################################################################
        tarifgenerique = input$tarifgenerique
        if(tarifgenerique ==""){
          tarifgenerique = "10^(-2.96+1.93*log10(d))"
        }
        
        if (!error_sim && !is.null(input$data_logging)) {
          DF  <-  hot_to_r(input$data_logging)
          DF_SAVE <- DF
          DF$tauxPrelevement = as.numeric(DF$tauxPrelevement)
          DF$coefRecollement = as.numeric(DF$coefRecollement)
          DF$dma = as.numeric(DF$dma)
          DF$dme = as.numeric(DF$dme)
          DF$densite = as.numeric(DF$densite)
          DF$tauxPrelevement[is.na(DF$tauxPrelevement) & !is.na(DF$dma)]=100
          DF$coefRecollement[is.na(DF$coefRecollement) & !is.na(DF$dma)]=100
          DF$tauxPrelevement[is.na(DF$tauxPrelevement)]=0
          DF$coefRecollement[is.na(DF$coefRecollement)]=0
          DF$dma[is.na(DF$dma)]= Inf
          DF$tauxPrelevement = DF$tauxPrelevement/100
          DF$coefRecollement = DF$coefRecollement/100
          DF$tarifcubage[DF$tarifcubage=="NA" | DF$tarifcubage==""]= tarifgenerique
          SpeciesTraits= data.frame(Nom.sp = as.factor(DF$nomEspece), WSG = DF$densite, DME= DF$dme, DMA=DF$dma, tauxPrelevement= DF$tauxPrelevement, coefRecollement = DF$coefRecollement, tarifs = DF$tarifcubage, TarifGenerique = tarifgenerique , stringsAsFactors = FALSE) 
          SpeciesTraits = merge(SpeciesTraits, data.frame(Nom.sp = SpeciesTraitsSim$Nom.sp, Id.sp = SpeciesTraitsSim$Id.sp), all.x=T, all.y=T, sort = F)
          SpeciesTraits$Nom.sp = as.factor(as.character(SpeciesTraits$Nom.sp))
          SpeciesTraits$Id.sp = as.factor(as.character(SpeciesTraits$Id.sp))
          if(!error_sim && nrow(subset(SpeciesTraits, is.na(SpeciesTraits$DME))) >0){
            error_sim= TRUE
            error_sim_msg= uiSomeSpeciesDoNotHaveDME
          }
          if(!error_sim && nrow(subset(SpeciesTraits, is.na(SpeciesTraits$WSG))) >0){
            error_sim= TRUE
            error_sim_msg= uiSomeSpeciesDoNotHaveDensity
          }
          if(!error_sim && nrow(subset(SpeciesTraits, is.na(SpeciesTraits$Id.sp))) >0){
            error_sim= TRUE
            error_sim_msg= uiSomeSpeciesDoNotHaveIdentifiant
          }
          if(!error_sim && nrow(subset(SpeciesTraits, SpeciesTraits$DMA<SpeciesTraits$DME))!=0){
            error_sim= TRUE
            error_sim_msg= uiSomeSpeciesHaveDMALessDME
          }
          if(!error_sim && nrow(subset(SpeciesTraits,SpeciesTraits$tauxPrelevement<0|SpeciesTraits$tauxPrelevement>1))!=0){
            error_sim= TRUE
            error_sim_msg= uiRemovalRateMustBeBetween0And100
          }
          if(!error_sim && nrow(subset(SpeciesTraits,SpeciesTraits$tauxRecolement<0|SpeciesTraits$tauxRecolement>1))!=0){
            error_sim= TRUE
            error_sim_msg= uiHavestingRageMustBeBetween0And100
          }
        }else if(!error_sim){
          DF_SAVE <- NULL
          if(!is.null(SpeciesTraitsSim)){
            SpeciesTraits = SpeciesTraitsSim
            DF <- data.frame(nomEspece = SpeciesTraits$Nom.sp, codeEspece= SpeciesTraits$Id.sp, dme=SpeciesTraits$DME, dma=rep(Inf, length(SpeciesTraits$Id.sp)), tauxPrelevement=rep(0, length(SpeciesTraits$Id.sp)), coefRecollement=rep(0, length(SpeciesTraits$Id.sp)), tarifcubage=rep(tarifgenerique, length(SpeciesTraits$Id.sp)), densite=SpeciesTraits$WSG, stringsAsFactors = FALSE)
            SpeciesTraits= data.frame(Nom.sp = as.factor(DF$nomEspece), Id.sp= as.factor(DF$codeEspece), WSG = DF$densite, DME= DF$dme, DMA=DF$dma, tauxPrelevement= DF$tauxPrelevement, coefRecollement = DF$coefRecollement, tarifs = DF$tarifcubage, TarifGenerique = tarifgenerique , stringsAsFactors = FALSE)
            SpeciesTraits$Nom.sp = as.factor(as.character(SpeciesTraits$Nom.sp))
            SpeciesTraits$Id.sp = as.factor(as.character(SpeciesTraits$Id.sp))
          }
        }
        if(!error_sim && !is.null(SimSiteName) && SimSiteName != getCSVSiteName()){
          SpeciesTraitsNameMissing = as.character(SpeciesTraitsSim$Nom.sp[!SpeciesTraitsSim$Nom.sp%in%ParamsDyn$SpeciesTraits$Nom.sp])
          if (length(SpeciesTraitsNameMissing) > 0 && !is.null(input$data_corresponding_species)) {
            CorrespondingSpecies <-  hot_to_r(input$data_corresponding_species)
            if(sum(CorrespondingSpecies$SubstitutedSpecies == "") > 0){
              error_sim= TRUE
              error_sim_msg= uiSomeSpeciesInCorrespondenceTableDoNotHaveCorrespondingSpecie
            }
          }
        }
        if(!error_sim && !is.null(SimSiteName) && SimSiteName == getCSVSiteName()){
          DataStart = NULL
          DataStartMat= NULL
          if (!error_sim && !is.null(input$data_start_missing_species)) {
            MissingDataStartSpecies <-  hot_to_r(input$data_start_missing_species)
            if(!error_sim && sum(MissingDataStartSpecies$SubstitutedSpecies == "") > 0){
              error_sim= TRUE
              error_sim_msg= uiSomeSpeciesInMissingTableDoNotHaveCorrespondingSpecie
            }
            
            if (!error_sim && !is.null(input$data_start_species)) {
              DataStart <-  hot_to_r(input$data_start_species)
              DataStart <- as.data.frame(lapply(DataStart[,-1], function(x) as.numeric(x)))
              DataStartMat <- as.matrix(DataStart)
            }
          }
        }
        if (!error_sim && !is.null(input$vector_damage)) {
          VD  <-  hot_to_r(input$vector_damage)
          VD_SAVE <- VD
          VD[VD=="NA" | VD==""]=0
          
          v=c()
          for (i in 1:ncol(VD)){
            v = c(v,as.numeric(VD[[i]][1]))
          }
          
          if(length(v[which(v<0|v>100)])!=0){
            error_sim= TRUE
            error_sim_msg= "Le dommage en (%) doit être compris entre 0 et 100. Veuillez corriger ces informations"
          }
        }else if(!error_sim){
          VD  <-  NULL
          VD_SAVE <- VD
          v <-  rep(0, NbClasse)
        }
        if (!error_sim && !is.null(input$vector_number_per_diameter_class)) {
          VNUM  <-  hot_to_r(input$vector_number_per_diameter_class)
          VNUM_SAVE <- VNUM
          VNUM[VNUM=="NA" | VNUM==""]=0
          Effectifs=c()
          for (i in 1:ncol(VNUM)){
            Effectifs = c(Effectifs,as.numeric(VNUM[[i]][1]))
          }
          if(length(Effectifs[which(Effectifs<0)])!=0){
            error_sim= TRUE
            error_sim_msg= uiEffectiveOfSpeciesByDiameterClassMustBePositive
          }
        }else if (!error_sim){
          VNUM  <-  NULL
          VNUM_SAVE <- VNUM
          Effectifs <-  rep(0, NbClasse)
        }
        if (!error_sim && !is.null(input$vector_tauxrecrutement_per_diameter_class)) {
          VTR  <-  hot_to_r(input$vector_tauxrecrutement_per_diameter_class)
          VTR_SAVE <- VTR
          VTR[VTR=="NA" | VTR==""]=0
          ClasseDiamWeights=c()
          for (i in 1:ncol(VTR)){
            ClasseDiamWeights = c(ClasseDiamWeights,as.numeric(VTR[[i]][1]))
          }
          if(length(ClasseDiamWeights[which(ClasseDiamWeights<0)])!=0){
            error_sim= TRUE
            error_sim_msg= uiRecruitmentRateOfSpeciesByDiameterClassMustBePositive
          }
        }else if(!error_sim){
          VNUM  <-  NULL
          VNUM_SAVE <- VNUM
          ClasseDiamWeights <-  rep(0, NbClasse)
        }
        if(!error_sim){ 
          shinyjs::show('boxloader')
          shinyjs::show('loader_sim')
          shinyjs::show('sim_encours')
          shinyjs::hide('image_success')
          shinyjs::hide('sim_finish')
          shinyjs::hide('param_sim_logging')
          shinyjs::hide('actions_sim')
          shinyjs::hide('action_save_sim_file')
          assign("hidable", TRUE, envir = .GlobalEnv)
          print(hidable)
          nbchain = input$nbchain
          Starting.plot = NULL
          Surface.zone = NULL
          if(!is.null(ParamsDyn) && ParamsDyn$DataType == "parcelle"){
            if(!is.null(SimSiteName) && SimSiteName == getCSVSiteName()){
              Starting.plot = DataStartMat
              Surface.zone = as.numeric(input$surfaceparcelle)
            }else if(!is.null(SimSiteName)){
              if(input$Allparcelle == TRUE){
                if(!is.null(ParamsSimSite)){
                  Starting.plot= as.character(unique(ParamsSimSite$Id.zone))
                }
              }else{
                Starting.plot= input$parcelle
              }
            }
          }
          Aggregation = F
          if(!is.null(ParamsDyn) && ParamsDyn$DataType == "parcelle"){
            if(!is.null(SimSiteName) && SimSiteName != getCSVSiteName()){
              StartingDate = as.numeric(input$anneedebutSimAutreSite)
              Aggregation = input$aggregate_parcels
            }else if(!is.null(SimSiteName)){
              StartingDate = as.numeric(input$anneedebutSim)
            }
          }else if(!is.null(ParamsDyn) && ParamsDyn$DataType == "sentier"){
            StartingDate = as.numeric(input$anneedebutSim)
          }
          Essence = as.character(input$espece)
          RecruitmentRate = as.numeric(input$tauxrecrutement)
          Nb.rotation = as.numeric(input$nombrerotation)
          rotation = as.numeric(input$dureerotation)
          DelayLogging =as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim)
          Fin.simu = as.numeric(input$dureesimulation)
          vector_damage = v/100
          ClasseDiamWeights = ClasseDiamWeights/100
          
          Logging.intensity = rep(0, NbClasse)
          if(!is.null(ParamsDyn) && ParamsDyn$DataType == "sentier"){
            DMA = SpeciesTraits$DMA[SpeciesTraits$Id.sp == Essence]
            Tauxprelevement = SpeciesTraits$tauxPrelevement[SpeciesTraits$Id.sp== Essence]
            Logging.intensity[DMA <= ClassesDiam] = Tauxprelevement
            if(sum(DMA <= ClassesDiam) > 0 && !DMA %in% ClassesDiam){
              Id_cl = sum(DMA>ClassesDiam)
              Logging.intensity[Id_cl] = Tauxprelevement*((ClassesDiam[Id_cl+1] - DMA)/(ClassesDiam[Id_cl+1] - ClassesDiam[Id_cl]))
            }
          }
          
          SimulateLogging = input$simulate_logging
          if(SimulateLogging){
            Logging="T2.MBaiki"
          }else{
            Logging= "T0"
          }
          DelayAfterLogging = as.numeric(input$delayAfterLogging)
          DelayDynamicsPostExploitation = as.numeric(input$delayDynamicsPostExploitation)
          ##################################################################################################################################
          #Calcul des paramètres du modèle en utilisant les données réelles collectées sur les arbres et 
          #contenu dans la liste d'objets ParamsDyn contenant les paramètres de la dynamique
          ##################################################################################################################################
          ParamSim = list(Nb.rotation = Nb.rotation, rotation = rotation, Fin.simu = Fin.simu, SimulateLogging= SimulateLogging, DelayLogging = DelayLogging, DelayAfterLogging = DelayAfterLogging, DelayDynamicsPostExploitation = DelayDynamicsPostExploitation, nbchain = nbchain, vector_damage = vector_damage, RecruitmentRate = RecruitmentRate, Starting.plot = Starting.plot, Aggregation= Aggregation, Essence=Essence, Effectifs=Effectifs, ClasseDiamWeights= ClasseDiamWeights, Logging.intensity = Logging.intensity, StartingDate = StartingDate, nbchain = nbchain, MySpeciesTraits = SpeciesTraits, Logging = Logging, Tarifgenerique = tarifgenerique, ParamsSimSite = ParamsSimSite, SimSiteName= SimSiteName, SpeciesTraitsSim = SpeciesTraitsSim, Surface.zone=Surface.zone)
          ##################################################################################################################################
          #Simulation proprement dite
          ##################################################################################################################################
          ResultFCM= FCM(out.InferFCM=ParamsDyn, ParamSim)
          ##################################################################################################################################
          #Sauvegarde des resultats de la simulation dans le fichier précisé par l'utilisateur et d'autres paramètres
          #utilisable ultérieurement.
          ##################################################################################################################################
          Nb.period=ResultFCM$Nb.period
          max_Temps = max(ResultFCM$Simulations$Temps)+StartingDate
          StoreTime= c(StartingDate:max_Temps);
          ResultFCM$StoreTime = StoreTime
          ResultFCM$ParamSim = ParamSim
          ResultFCM$DataType = ParamsDyn$DataType
          ResultFCM$ClassesDiam = ParamsDyn$ClassesDiam
          ResultFCM$ParamsDynFile = ParamsDynFile
          ResultFCM$DateSim = Sys.Date()
          if(ParamsDyn$DataType == "parcelle"){
            ResultFCM$Site = ParamsSimSiteAll$Descriptif$site
            ResultFCM$Pays = ParamsSimSiteAll$Descriptif$pays
          }else if(ParamsDyn$DataType == "sentier"){
            ResultFCM$Site = ParamsDyn$Descriptif$site
            ResultFCM$Pays = ParamsDyn$Descriptif$pays
          }
          updateInputForVisualization(ResultFCM)
          save(ResultFCM, file = Savepath)
          ################# Mise à jour automatique du fichier des resultats de la simulation ###############################
          CurrentResultSimDataFile <<- Savepath
          updateInfosDescriptionSim(ResultFCM)
          ###################################################################################################################
          IndicatorFile = paste0(IndicatorsFolder, "\\FileIndicateur.RData")
          if(!is.null(IndicatorFile) && file.exists(IndicatorFile) && file.access(names = IndicatorFile, mode = 4)==0){
            load(IndicatorFile)
          }else{
            listeIndicateur= list()
          }
          updateSimulatingDataOfIndicator(listeIndicateur)
          shinyjs::hide('loader_sim')
          shinyjs::hide('sim_encours')
          shinyjs::show('image_success')
          shinyjs::show('sim_finish')
          shinyjs::show('actions_sim')
          shinyjs::show('action_save_sim_file')
          shinyjs::hide('lancer_sim_col')
          shinyjs::show('gotoparameters_col')
        }else{
          showModal(modalDialog(
            title=uiError,
            size = "m",
            footer = modalButton(uiClose),
            error_sim_msg
          ) 
          )
        }
      })
      # tryCatch({
        print("Start Sim")
        simulation.input()
        print("End Sim")
      # }, error=function(e){
      #   if(hidable){
      #     shinyjs::hide('loader_sim')
      #     shinyjs::hide('sim_encours')
      #     shinyjs::show('image_failure')
      #     shinyjs::show('sim_failure')
      #     shinyjs::hide('action_save_sim_file')
      #     shinyjs::show('actions_sim')
      #     shinyjs::hide('lancer_sim_col')
      #     shinyjs::show('gotoparameters_col')
      #   }
      #   showModal(modalDialog(
      #     title=uiError,
      #     size = "m",
      #     footer = modalButton(uiClose),
      #     e$message
      #   )
      #   )
      # })
    }
  })
  
  
  
  ################################ Evénement au calcul du taux de reconstitution du stock de DIMAKO ######################################################
  observeEvent(input$estimate_tcs_btn, {
    if(!is.null(ParamsDynFile) && file.exists(ParamsDynFile) && file.access(names = ParamsDynFile, mode = 4)==0){
      ParamsDyn = formattingDynData(readRDS(ParamsDynFile))
      tryCatch({
        error= validate_user_data_input_for_tsc_dimako(input)
        error_estim = error$error_estim
        error_estim_msg = error$error_estim_msg
        if(!error_estim){
          shinyjs::hide('div_estimate_tcs')
          shinyjs::show('boxloader_estimate_tcs')
          tauxAccroissement = as.numeric(input$tauxaccroissement)
          tauxMortalite = as.numeric(input$tauxmortalite)/100
          VNUM  <-  hot_to_r(input$vector_number_per_diameter_class)
          VNUM_SAVE <- VNUM
          VNUM[VNUM=="NA" | VNUM==""]=0
          Effectifs=c()
          for (i in 1:ncol(VNUM)){
            Effectifs = c(Effectifs,as.numeric(VNUM[[i]][1]))
          }
          
          VD  <-  hot_to_r(input$vector_damage)
          VD_SAVE <- VD
          VD[VD=="NA" | VD==""]=0
          Logging.damage=c()
          for (i in 1:ncol(VD)){
            Logging.damage = c(Logging.damage,as.numeric(VD[[i]][1]))
          }
          Logging.damage = Logging.damage/100
          Essence = input$espece
          DataLogging  <-  hot_to_r(input$data_logging)
          DataLogging = subset(DataLogging, DataLogging$nomEspece == Essence)
          DataLogging$tauxPrelevement = as.numeric(DataLogging$tauxPrelevement)
          DataLogging$dma = as.numeric(DataLogging$dma)
          DataLogging$dme = as.numeric(DataLogging$dme)
          DataLogging$tauxPrelevement[is.na(DataLogging$tauxPrelevement) & !is.na(DataLogging$dma)]=100
          DataLogging$tauxPrelevement[is.na(DataLogging$tauxPrelevement)]=0
          DataLogging$dma[is.na(DataLogging$dma)]= Inf
          DataLogging$tauxPrelevement = DataLogging$tauxPrelevement/100
          
          dureerotation = as.numeric(input$dureerotation)
          ClassesDiam = ParamsDyn$ClassesDiam
          DME = DataLogging$dme
          DMA = DataLogging$dma
          tauxPrelevement = DataLogging$tauxPrelevement
          TRDimako = ApiDimako(Effectifs,ClassesDiam, Logging.damage, DME, DMA, tauxAccroissement, tauxMortalite, tauxPrelevement, dureerotation)
          output$estimation_tcs_value <- renderUI({
            HTML(paste0('<div id= estimation_tcs_container class="container-fluid">
                        <div class="row">
                        <div class="col-sm-12">
                        <label>Valeur estimée : </label>
                        <span>', TRDimako, '%</span>
                        </div>
                        </div>
                        </div>')
            )
          })
          shinyjs::show('result_estimation_tcs')
          shinyjs::hide('boxloader_estimate_tcs')
          shinyjs::show('div_estimate_tcs')
        }else{
          showModal(modalDialog(
            title=uiError,
            size = "m",
            footer = modalButton(uiClose),
            error_estim_msg
          )
          )
        }
      },error= function(e){
        shinyjs::hide('boxloader_estimate_tcs')
        shinyjs::show('div_estimate_tcs')
        showModal(modalDialog(
          title=uiError,
          size = "m",
          footer = modalButton(uiClose),
          e$message
        )
        )
      })
    }
  })
  
  ################################ Sauvegarde des paramètres de la dynamique forestière ##############################
  observeEvent(input$save_start_data,{
    inFile = parseSavePath(roots, input$save_start_data)
    Savepath = as.character(inFile$datapath)
    DF  <-  hot_to_r(input$data_start_species)
    write.csv2(x = DF, file = Savepath, row.names = FALSE, na="")
  })
  
  ################################ Sauvegarde des paramètres de la dynamique forestière ##############################
  observeEvent(input$save_config,{
    if(!is.null(ParamsDynFile) && file.exists(ParamsDynFile) && file.access(names = ParamsDynFile, mode = 4)==0){
      tryCatch({
        inFile = parseSavePath(roots, input$save_config)
        Savepath = as.character(inFile$datapath)
        if(!is.null(Savepath) && length(Savepath) > 0){
          shinyjs::hide('lancer_sim_col')
          shinyjs::show('boxloader_FileConfig')
          ParamsDyn = formattingDynData(readRDS(ParamsDynFile))
          ClassesDiam = ParamsDyn$ClassesDiam
          NbClasse = length(ClassesDiam)
          tarifgenerique = input$tarifgenerique
          if(tarifgenerique ==""){
            tarifgenerique = "10^(-2.96+1.93*log10(d))"
          }
          if(!is.null(input$data_logging)){
            ST_SAVE <- hot_to_r(input$data_logging)
          }else{
            ST_SAVE <- NULL
          }
          
          if(!is.null(input$data_corresponding_species)){
            CS_SAVE <-  hot_to_r(input$data_corresponding_species)
          }else{
            CS_SAVE <- NULL
          }
        
          if(!is.null(input$data_start_species)){
            DST_SAVE <-  hot_to_r(input$data_start_species)
          }else{
            DST_SAVE <- NULL
          }
          
          if(!is.null(input$data_start_missing_species)){
            DST_MS_SAVE <-  hot_to_r(input$data_start_missing_species)
          }else{
            DST_MS_SAVE <- NULL
          }
        
          if(!is.null(input$vector_damage)){
            VD_SAVE <- hot_to_r(input$vector_damage)
          }else{
            VD_SAVE <- NULL
          }
          
          if(!is.null(input$vector_number_per_diameter_class)){
            VNUM_SAVE <- hot_to_r(input$vector_number_per_diameter_class)
          }else{
            VNUM_SAVE <- NULL
          }
          
          if(!is.null(input$vector_tauxrecrutement_per_diameter_class)){
            VTR_SAVE <- hot_to_r(input$vector_tauxrecrutement_per_diameter_class)
          }else{
            VTR_SAVE <- NULL
          }
          
          nbchain = input$nbchain
          StartingDate = as.numeric(input$anneedebutSim)
          Nb.rotation = as.numeric(input$nombrerotation)
          rotation = as.numeric(input$dureerotation)
          DelayLogging =as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim)
          Fin.simu = as.numeric(input$dureesimulation)
          Check.Allparcelle = input$Allparcelle
          Starting.plot= as.numeric(input$parcelle)
          Essence = as.character(input$espece)
          RecruitementRate = as.numeric(input$tauxrecrutement)
          StartingLogging = as.numeric(input$anneefirstlogging)
          Starting.plot = NULL
          Surface.zone = NULL
          if(!is.null(ParamsDyn) && ParamsDyn$DataType == "parcelle"){
            if(!is.null(SimSiteName) && SimSiteName == getCSVSiteName()){
              Surface.zone = as.numeric(input$surfaceparcelle)
            }else if(!is.null(SimSiteName)){
              if(input$Allparcelle == TRUE){
                if(!is.null(ParamsSimSite)){
                  Starting.plot= as.character(unique(ParamsSimSite$Id.zone))
                }
              }else{
                Starting.plot= as.numeric(input$parcelle)
              }
            }
          }
          Aggregation = F
          if(!is.null(ParamsDyn) && ParamsDyn$DataType == "parcelle"){
            SimSiteName <<- input$siteParcelle
            if(!is.null(SimSiteName) && SimSiteName != getCSVSiteName()){
              StartingDate = as.numeric(input$anneedebutSimAutreSite)
              Aggregation = input$aggregate_parcels
            }else if(!is.null(SimSiteName)){
              StartingDate = as.numeric(input$anneedebutSim)
            }
          }else if(!is.null(ParamsDyn) && ParamsDyn$DataType == "sentier"){
            SimSiteName <<- NULL
            StartingDate = as.numeric(input$anneedebutSim)
          }
          SimulateLogging = input$simulate_logging
          DelayAfterLogging = as.numeric(input$delayAfterLogging)
          DelayDynamicsPostExploitation = as.numeric(input$delayDynamicsPostExploitation)
          ParamSim = list(SimulateLogging= SimulateLogging, DelayAfterLogging = DelayAfterLogging, DelayDynamicsPostExploitation = DelayDynamicsPostExploitation, SimSiteName = SimSiteName, SimSiteNameFile = SimSiteNameFile, Surface.zone = Surface.zone, StartingLogging = StartingLogging, Essence=Essence, RecruitementRate = RecruitementRate, Check.Allparcelle = Check.Allparcelle, Fin.simu = Fin.simu, Nb.rotation = Nb.rotation, rotation = rotation, DelayLogging = DelayLogging, nbchain = nbchain, vector_damage = VD_SAVE, Starting.plot = Starting.plot, Essence = Essence,  StartingDate = StartingDate, nbchain = nbchain , Tarifgenerique = tarifgenerique, DataType = ParamsDyn$DataType)
          ParamSim$vector_damage = VD_SAVE
          ParamSim$MySpeciesTraits = ST_SAVE
          ParamSim$Effectifs = VNUM_SAVE
          ParamSim$ClasseDiamWeights = VTR_SAVE
          ParamSim$DataStart = DST_SAVE
          ParamSim$DataStartMissingSpecies = DST_MS_SAVE
          ParamSim$CorrespondingSpecies = CS_SAVE
          ParamSim$SpeciesTraitsSim = SpeciesTraitsSim
          ParamSim$ParamsDynFile = ParamsDynFile
          save(ParamSim, file = Savepath)
          shinyjs::hide('boxloader_FileConfig')
          shinyjs::show('lancer_sim_col')
          showNotification("Scénario sauvegardé avec succès.", duration = 10, type = "message")
        }
      },error=function(e){
        shinyjs::hide('boxloader_FileConfig')
        shinyjs::show('lancer_sim_col')
        showModal(modalDialog(
          title=uiError,
          size = "m",
          footer = modalButton(uiClose),
          e$message
        )
        )
      })
    }else{
      showModal(modalDialog(
        title=uiError,
        size = "m",
        footer = modalButton(uiClose),
        uiPleaseLoadDynamicParameters
      ) 
      )
    }
    
  })
  
  ############ Charger un fichier de données simulées #########################
  observeEvent(input$load_sim_data,{
    tryCatch({
      
      inFile = parseFilePaths(roots3, input$load_sim_data)
      Loadpath = as.character(inFile$datapath)
      if(!is.null(Loadpath) && length(Loadpath) > 0){
        shinyjs::hide('load_sim_data')
        shinyjs::show('boxloader_SimData')
        load(Loadpath)
        updateInputForVisualization(ResultFCM)
        CurrentResultSimDataFile <<- Loadpath
        IndicatorFile = paste0(IndicatorsFolder, "\\FileIndicateur.RData")
        if(!is.null(IndicatorFile) && file.exists(IndicatorFile) && file.access(names = IndicatorFile, mode = 4)==0){
          load(IndicatorFile)
        }else{
          listeIndicateur= list()
        }
        updateSimulatingDataOfIndicator(listeIndicateur)
        updateInfosDescriptionSim(ResultFCM)
        shinyjs::hide('boxloader_SimData')
        shinyjs::show('load_sim_data')
        showNotification(uiDataLoadedSuccessfully, duration = 10, type = "message")
      }
    },error=function(e){
      shinyjs::show('load_sim_data')
      shinyjs::hide('boxloader_SimData')
      showModal(modalDialog(
        title=uiError,
        size = "m",
        footer = modalButton(uiClose),
        e$message
      ) 
      )
    })
  })
  ############ Charger le dernier paramètrage (Processus permettant de charger un scénario de simulation existant) #########################
  observeEvent(input$load_config,{
    shinyjs::hide('boxloader_FileConfig')
    shinyjs::show('lancer_sim_col')
    showModal(modalDialog(
      title=uiInformation,
      size = "m",
      footer = modalButton(uiClose),
      uiFeatureInMaintenance
    )
    )
      # tryCatch({
      # shinyjs::hide('lancer_sim_col')
      # shinyjs::show('boxloader_FileConfig')
      # inFile = parseFilePaths(roots, input$load_config)
      # Loadpath = as.character(inFile$datapath)
      # load(Loadpath)
      # SavedParamsDynFile = ParamSim$ParamsDynFile
      # if(!is.null(SavedParamsDynFile) && file.exists(SavedParamsDynFile) && file.access(names = SavedParamsDynFile, mode = 4)==0){
      #   ParamsDynFile <<- SavedParamsDynFile
      #   ParamsDyn = formattingDynData(readRDS(ParamsDynFile))
      #   if(!is.null(SimSiteNameFile) && file.exists(SimSiteNameFile) && file.access(names = SimSiteNameFile, mode = 4)==0){
      #     SimSiteNameFile <<- ParamSim$SimSiteNameFile
      #     SimSiteName <<- ParamSim$SimSiteName
      #     ParamsSimSite = formattingSimData(readRDS(SimSiteNameFile))
      #   }
      #   updateInfosSite(ParamsDyn$DataType, ParamsDyn$Descriptif$site, ParamsDyn$Descriptif$pays, ParamsDyn$Descriptif$ClassesDiam)
      #   MySpeciesTraits = ParamSim$MySpeciesTraits
      #   if(!is.null(MySpeciesTraits)){
      #     if(!is.null(ParamsDyn)){
      #     NumberSpecies = length(MySpeciesTraits$codeEspece)
      #     if(NumberSpecies == 1){
      #       height = 50
      #     }else if(NumberSpecies> 1 && NumberSpecies<= 7){
      #       height =185
      #     }else if(NumberSpecies> 7 && NumberSpecies<= 10){
      #       height = 220
      #     }else{
      #       height = 300
      #     }
      #     MySpeciesTraits$nomEspece= as.character(MySpeciesTraits$nomEspece)
      #     MySpeciesTraits$dma[MySpeciesTraits$dma=="NA"]= ""
      #     MySpeciesTraits$tauxPrelevement[MySpeciesTraits$tauxPrelevement=="NA"]= ""
      #     MySpeciesTraits$coefRecollement[MySpeciesTraits$coefRecollement=="NA"]= ""
      #     MySpeciesTraits$tarifcubage[MySpeciesTraits$tarifcubage=="NA"]= ""
      #       ColHeaders = c("Nom espèce", "D.M.E (cm)", "D.M.A (cm)", "Tx prélèvement (%)", "Coef recolement (%)", "Tarif de cubage", "Densité")
      #       output$data_logging <-rhandsontable::renderRHandsontable({
      #         rhandsontable::rhandsontable(MySpeciesTraits, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "100%", height = height) %>%
      #           hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE)%>%
      #           hot_cols(columnSorting = TRUE, colWidths= c(150, 75, 75, 150, 150, 200, 75), manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
      #           hot_col(col = "Nom espèce", readOnly = TRUE) %>%
      #           hot_col(col = "Code espèce", readOnly = TRUE)%>%
      #           hot_col(col = 2:length(ColHeaders), halign = "htCenter")
      #       })
      #     }
      #   }
      #   
      #   DataStart = ParamSim$DataStart
      #   if(!is.null(DataStart)){
      #     if(!is.null(ParamsDyn)){
      #       ColHeaders = colHeaders = c("CL Diam", names(dataStart)[-1])
      #       output$data_start_species <-rhandsontable::renderRHandsontable({
      #         rhandsontable::rhandsontable(dataStart, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "1000px", height = height) %>%
      #           hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = FALSE)%>%
      #           hot_cols(columnSorting = TRUE, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
      #           hot_col(col = 2:length(ColHeaders), halign = "htCenter")
      #       })
      #     }
      #   }
      #   
      #   DataStartMissingSpecies = ParamSim$DataStartMissingSpecies
      #   if(!is.null(DataStartMissingSpecies)){
      #     if(!is.null(ParamsDyn)){
      #       output$data_start_missing_species <-rhandsontable::renderRHandsontable({
      #         rhandsontable::rhandsontable(replacedSpecies, colHeaders = c("Espèce absente", "Remplacée par"), selectCallback = TRUE, useTypes = TRUE, width = "100%", height = 250) %>%
      #           hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu=FALSE) %>%
      #           hot_cols(columnSorting = TRUE, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
      #           hot_col(col = "Remplacée par", type = "dropdown", source = sort(ParamsDyn$SpeciesTraits$Nom.sp)) %>%
      #           hot_col(col = "Espèce absente", readOnly = TRUE)
      #       })
      #     }
      #   }
      #   
      #   CorrespondingSpecies = ParamSim$CorrespondingSpecies
      #   if(!is.null(CorrespondingSpecies)){
      #     if(!is.null(ParamsDyn)){
      #       output$data_corresponding_species <-rhandsontable::renderRHandsontable({
      #         rhandsontable::rhandsontable(CorrespondingSpecies, colHeaders = c("Espèce à simuler", "Espèce correspondante"), selectCallback = TRUE, useTypes = TRUE, width = "100%", height = height) %>%
      #           hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu=FALSE) %>%
      #           hot_cols(columnSorting = TRUE, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
      #           hot_col(col = "Espèce correspondante", type = "dropdown", source = sort(SpeciesTraitsNameRef)) %>%
      #           hot_col(col = "Espèce à simuler", readOnly = TRUE) 
      #       })
      #     }
      #   }
      #   
      #   Vector_damage = ParamSim$vector_damage
      #   if(!is.null(Vector_damage)){
      #     if(!is.null(ParamsDyn)){
      #       output$vector_damage <-rhandsontable::renderRHandsontable({
      #         ClassesDiam = ParamsDyn$ClassesDiam
      #         colwidths =1000/length(ClassesDiam)
      #         StrEval2="ColHeaders = c("
      #         StrEval3="colWidths = c("
      #         for(iter in 1:(length(ClassesDiam)-1)){
      #           StrEval2 = paste0(StrEval2, '"[', ClassesDiam[iter],', ', ClassesDiam[iter+1], '[", ')
      #           StrEval3 = paste0(StrEval3, paste0(colwidths,', '))
      #         }
      #         StrEval2 = paste0(StrEval2, '"[', ClassesDiam[length(ClassesDiam)],', Inf[")')
      #         eval(parse(text = StrEval2))
      #         StrEval3 = paste0(StrEval3, paste0(colwidths,')'))
      #         eval(parse(text = StrEval3))
      #         rhandsontable::rhandsontable(Vector_damage, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "100%") %>%
      #           hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE) %>%
      #           hot_cols(columnSorting = TRUE, colWidths= colWidths, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
      #           hot_col(col = 1:length(ColHeaders), halign = "htCenter")
      #       })
      #     }
      #   }
      #   Effectifs = ParamSim$Effectifs
      #   if(!is.null(Effectifs)){
      #     if(!is.null(ParamsDyn)){
      #       output$vector_number_per_diameter_class <-rhandsontable::renderRHandsontable({
      #         ClassesDiam = ParamsDyn$ClassesDiam
      #         colwidths =460/length(ClassesDiam)
      #         StrEval2="ColHeaders = c("
      #         StrEval3="colWidths = c("
      #         for(iter in 1:(length(ClassesDiam)-1)){
      #           StrEval2 = paste0(StrEval2, '"[', ClassesDiam[iter],', ', ClassesDiam[iter+1], '[", ')
      #           StrEval3 = paste0(StrEval3, paste0(colwidths,', '))
      #         }
      #         StrEval2 = paste0(StrEval2, '"[', ClassesDiam[length(ClassesDiam)],', Inf[")')
      #         eval(parse(text = StrEval2))
      #         StrEval3 = paste0(StrEval3, paste0(colwidths,')'))
      #         eval(parse(text = StrEval3))
      #         rhandsontable::rhandsontable(Effectifs, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "100%") %>%
      #           hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE) %>%
      #           hot_cols(columnSorting = TRUE, colWidths= colWidths, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
      #           hot_col(col = 1:length(ColHeaders), halign = "htCenter")
      # 
      #       })
      #     }
      #   }
      # 
      #   ClasseDiamWeights = ParamSim$ClasseDiamWeights
      #   if(!is.null(ClasseDiamWeights)){
      #     if(!is.null(ParamsDyn)){
      #       output$vector_tauxrecrutement_per_diameter_class <-rhandsontable::renderRHandsontable({
      #         ClassesDiam = ParamsDyn$ClassesDiam
      #         colwidths =460/length(ClassesDiam)
      #         StrEval2="ColHeaders = c("
      #         StrEval3="colWidths = c("
      #         for(iter in 1:(length(ClassesDiam)-1)){
      #           StrEval2 = paste0(StrEval2, '"[', ClassesDiam[iter],', ', ClassesDiam[iter+1], '[", ')
      #           StrEval3 = paste0(StrEval3, paste0(colwidths,', '))
      #         }
      #         StrEval2 = paste0(StrEval2, '"[', ClassesDiam[length(ClassesDiam)],', Inf[")')
      #         eval(parse(text = StrEval2))
      #         StrEval3 = paste0(StrEval3, paste0(colwidths,')'))
      #         eval(parse(text = StrEval3))
      #         rhandsontable::rhandsontable(ClasseDiamWeights, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "100%") %>%
      #           hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE) %>%
      #           hot_cols(columnSorting = TRUE, colWidths= colWidths, manualColumnResize=TRUE, fixedColumnsLeft=1)
      #         hot_col(col = 1:length(ColHeaders), halign = "htCenter")
      #       })
      #     }
      #   }
      # 
      #   updateNumericInput(session,"anneedebutSim", value=ParamSim$StartingDate)
      #   updateSelectInput(session,"anneedebutSimAutreSite", value=ParamSim$StartingDate)
      #   updateNumericInput(session,"anneefirstlogging", value=ParamSim$StartingLogging)
      #   updateTextInput(session,"dureesimulation", value=ParamSim$Fin.simu)
      #   updateNumericInput(session,"dureerotation", value=ParamSim$rotation)
      #   updateNumericInput(session,"nombrerotation", value=ParamSim$Nb.rotation)
      #   updateNumericInput(session,"nbchain", value=ParamSim$nbchain)
      #   updateTextInput(session, "tarifgenerique", value = ParamSim$Tarifgenerique)
      # 
      #   if(!is.null(ParamsDyn$DataType) && ParamsDyn$DataType == "parcelle"){
      #     updateCheckboxInput(session,"Allparcelle", value=ParamSim$Check.Allparcelle)
      #     if(!ParamSim$Check.Allparcelle){
      #       Parcelles = ParamsDyn$Descriptif$Parcelles
      #       updateSelectizeInput(session, "parcelle", choices = Parcelles, selected = ParamSim$Starting.plot, server = TRUE)
      #     }
      #     shinyjs::hide('bloc_params_logging_sentier')
      #     shinyjs::hide('bloc_espece_sentier')
      #     shinyjs::hide('bloc_recrutement_sentier')
      #     shinyjs::hide('bloc_tauxrecrutement_sentiers_sim')
      #     shinyjs::show('bloc_params_logging_parcelle')
      #     shinyjs::show('bloc_list_parcelles_sim')
      #   }else if(!is.null(ParamsDyn$DataType) && ParamsDyn$DataType == "sentier"){
      #     updateNumericInput(session,"tauxrecrutement", value=ParamSim$RecruitementRate)
      #     Essences = levels(ParamsDyn$SpeciesTraits$Id.sp)
      #     updateSelectizeInput(session, "espece", choices = Essences, selected = ParamSim$Essence, server = TRUE)
      #     updateSelectizeInput(session, "groupe_espece_indicateur", choices = ParamSim$Essence,
      #                          selected = ParamSim$Essence,  server = TRUE)
      #     updateSelectizeInput(session, "groupe_espece_strDia", choices = ParamSim$Essence,
      #                          selected = ParamSim$Essence,  server = TRUE)
      #     shinyjs::hide('bloc_params_logging_parcelle')
      #     shinyjs::hide('bloc_list_parcelles_sim')
      #     shinyjs::show('bloc_params_logging_sentier')
      #     shinyjs::show('bloc_espece_sentier')
      #     shinyjs::show('bloc_recrutement_sentier')
      #     shinyjs::show('bloc_tauxrecrutement_sentiers_sim')
      #   }
      #   shinyjs::hide('boxloader_FileConfig')
      #   shinyjs::show('lancer_sim_col')
      #   showNotification("Scénario chargé avec succès.", duration = 10, type = "message")
      # }else{
      #   shinyjs::hide('boxloader_FileConfig')
      #   shinyjs::show('lancer_sim_col')
      #   showModal(modalDialog(
      #       title=uiError,
      #       size = "m",
      #       footer = modalButton(uiClose),
      #       "Veuillez charger les paramètres de la dynamique"
      #     )
      #   )
      # }
      # },error=function(e){
      #   shinyjs::hide('boxloader_FileConfig')
      #   shinyjs::show('lancer_sim_col')
      #   showModal(modalDialog(
      #     title=uiError,
      #     size = "m",
      #     footer = modalButton(uiClose),
      #     e$message
      #   )
      #   )
      # })
  })
  
  
  ############ This function update the values of indicators to add a values of a new indicateur in a current simulation Data file #########################
  updateSimulatingDataOfIndicator<- function (listeIndicateur = NULL){
    if(file.exists(CurrentResultSimDataFile) && file.access(names = CurrentResultSimDataFile, mode = 4)==0){
      load(CurrentResultSimDataFile)
      MyParamsDyn = formattingDynData(readRDS(ResultFCM$ParamsDynFile))
      ResultFCM$ParamPlot$CDSTB=ClasseDiamSTAGB(ParamsDyn = MyParamsDyn, ParamSim =ResultFCM$ParamSim,alpha=0.08, OtherIndicator = listeIndicateur)
      save(ResultFCM,file=CurrentResultSimDataFile)
    }
  }
  
  
  ############ Ajout d'un indicateur #########################
  observeEvent(input$ajout_indicateur_btn,{
    tryCatch({
      validate(
        need(input$nom_indicateur_new != "", uiPleaseEnterIndicatorName),
        need(input$fonction_indicateur_new !="", uiPleaseEnterIndicatorFormula)
      )
      
      shinyjs::hide('action_add_IND')
      shinyjs::show('boxloader_add_IND')
      IndicatorFile = paste0(IndicatorsFolder, "\\FileIndicateur.RData")
      if(!is.null(IndicatorFile) && file.exists(IndicatorFile) && file.access(names = IndicatorFile, mode = 4)==0){
        load(IndicatorFile)
      }else{
        listeIndicateur= list()
      }
      
      error_add_ind=FALSE
      numInd = length(listeIndicateur)
      if(numInd !=0){
        i=1
        while(!error_add_ind && i<=length(listeIndicateur)){
          indicateur= listeIndicateur[[i]]
          if(indicateur$NomInd== input$nom_indicateur_new ){
            error_add_ind=TRUE
          }
          i=i+1
        }
      }
      FuncInd = input$fonction_indicateur_new
      listeIndicateurTmp= list(list(NomInd=input$nom_indicateur_new, NomFunc= paste("nomfunction", numInd, sep = ''), VarInd=paste("varind", numInd, sep = ''), Func=FuncInd ))
      if(!error_add_ind){
        listeIndicateur = append(listeIndicateur, listeIndicateurTmp)
        updateSimulatingDataOfIndicator(listeIndicateur)
        save(listeIndicateur,file=paste0(IndicatorsFolder, "\\FileIndicateur.RData"))
        ######Mise à jour de la liste de indicateurs sur l'interface utilisateur de visualisation des indicateurs###########
        updateSelectIndicator()
        shinyjs::hide('boxloader_add_IND')
        shinyjs::show('action_add_IND')
        showNotification(uiIndicatorAddedSuccessfully, duration = 10, type = "message")
      }else{
        shinyjs::hide('boxloader_add_IND')
        shinyjs::show('action_add_IND')
        showModal(modalDialog(
          title=uiError,
          size = "m",
          footer = modalButton(uiClose),
          uiAnIndicatorAlreadyHaveThisInformation
        ) 
        )
      }
    },error=function(e){
      shinyjs::hide('boxloader_add_IND')
      shinyjs::show('action_add_IND')
      showModal(modalDialog(
        title=uiError,
        size = "m",
        footer = modalButton(uiClose),
        e$message
      ) 
      )
    })
  })
  #########################Modification des indicateurs####################################
  observeEvent(input$update_indicateur_btn,{
    tryCatch({
      validate(
        need(input$nom_indicateur_update != "", uiPleaseEnterIndicatorName),
        need(input$fonction_indicateur_update !="", uiPleaseEnterIndicatorFormula)
      )
      
      shinyjs::hide('action_update_delete_IND')
      shinyjs::show('boxloader_update_IND')
      IndicatorFile = paste0(IndicatorsFolder, "\\FileIndicateur.RData")
      if(!is.null(IndicatorFile) && file.exists(IndicatorFile) && file.access(names = IndicatorFile, mode = 4)==0){
        load(IndicatorFile)
      }else{
        listeIndicateur= list()
      }
      trouve=FALSE
      indicateur=list()
      nbreInd = length(listeIndicateur)
      if(nbreInd !=0){
        i=1
        while(!trouve && i<=length(listeIndicateur)){
          indicateur = listeIndicateur[[i]]
          if(indicateur$NomInd== input$nom_indicateur_update ){
            trouve=TRUE
          }
          i=i+1
        }
        
        numInd = i-1
        FuncInd = input$fonction_indicateur_update
        indicateur= list(NomInd=input$nom_indicateur_update, NomFunc= paste("nomfunction", numInd, sep = ''), VarInd=paste("varind", numInd, sep = ''), Func=FuncInd )
        
        listeIndicateur[[numInd]] = indicateur
        updateSimulatingDataOfIndicator(listeIndicateur)
        save(listeIndicateur,file=paste0(IndicatorsFolder, "\\FileIndicateur.RData"))
        ######Mise à jour de la liste de indicateurs sur l'interface utilisateur pour la modification des indicateurs###########
        loadUpdatedIndicator()
        ######Mise à jour de la liste de indicateurs sur l'interface utilisateur pour la visualisation des indicateurs###########
        updateSelectIndicator()
        shinyjs::hide('boxloader_update_IND')
        shinyjs::show('action_update_delete_IND')
        showNotification(uiIndicatorUpdatedSuccessfully, duration = 10, type = "message")
      }else{
        shinyjs::hide('boxloader_update_IND')
        shinyjs::show('action_update_delete_IND')
        showModal(modalDialog(
          title=uiError,
          size = "m",
          footer = modalButton(uiClose),
          uiAnyIndicatorSelected
        ) 
        )
        
      }
    },error=function(e){
      shinyjs::hide('boxloader_update_IND')
      shinyjs::show('action_update_delete_IND')
      showModal(modalDialog(
        title=uiError,
        size = "m",
        footer = modalButton(uiClose),
        e$message
      ) 
      )
    })
  })
  #########################delete indicateur####################################
  observeEvent(input$delete_indicateur_btn,{
    tryCatch({
      validate(
        need(input$nom_indicateur_update != "", uiPleaseEnterIndicatorName),
        need(input$fonction_indicateur_update !="", uiPleaseEnterIndicatorFormula)
      )
      
      shinyjs::hide('action_update_delete_IND')
      shinyjs::show('boxloader_update_IND')
      IndicatorFile = paste0(IndicatorsFolder, "\\FileIndicateur.RData")
      if(!is.null(IndicatorFile) && file.exists(IndicatorFile) && file.access(names = IndicatorFile, mode = 4)==0){
        load(IndicatorFile)
      }else{
        listeIndicateur= list()
      }
      trouve=FALSE
      indicateur=list()
      nbreInd = length(listeIndicateur)
      if(nbreInd !=0){
        i=1
        while(!trouve && i<=length(listeIndicateur)){
          indicateur = listeIndicateur[[i]]
          if(indicateur$NomInd== input$nom_indicateur_update ){
            trouve=TRUE
          }
          i=i+1
        }
        numInd = i-1
        listeIndicateur = listeIndicateur[-numInd]
        updateSimulatingDataOfIndicator(listeIndicateur)
        save(listeIndicateur,file=paste0(IndicatorsFolder, "\\FileIndicateur.RData"))
        ######Mise à jour de la liste de indicateurs sur l'interface utilisateur pour la modification des indicateurs###########
        loadUpdatedIndicator()
        ######Mise à jour de la liste de indicateurs sur l'interface utilisateur pour la visualisation des indicateurs###########
        updateSelectIndicator()
        shinyjs::hide('boxloader_update_IND')
        shinyjs::show('action_update_delete_IND')
        showNotification(uiIndicatorDeletedSuccessfully, duration = 10, type = "message")
      }else{
        shinyjs::hide('boxloader_update_IND')
        shinyjs::show('action_update_delete_IND')
        showModal(modalDialog(
          title=uiError,
          size = "m",
          footer = modalButton(uiClose),
          uiAnyIndicatorSelected
        ) 
        )
      }
    },error=function(e){
      shinyjs::hide('boxloader_update_IND')
      shinyjs::show('action_update_delete_IND')
      shinyjs::info(e)
    })
  })
  
  updateInfosDescriptionSim <- function(ResultFCM){
    if(ResultFCM$DataType == "sentier"){
      Label_espece_or_parcelle = uiSimulatedSpecie
      Val_sentier_or_parcelle = ResultFCM$ParamSim$Essence
    }else if(ResultFCM$DataType == "parcelle"){
      Label_espece_or_parcelle = uiSimulatedPlots
      if(!is.vector(ResultFCM$ParamSim$Starting.plot)){
        Val_sentier_or_parcelle = paste0(uiAPlotOf," ", ResultFCM$ParamSim$Surface.zone, " ha")
      }else{
        Val_sentier_or_parcelle = paste(ResultFCM$ParamSim$Starting.plot,collapse=", ")
      }
      
    }
    output$file_sim_informations <- renderUI({
      HTML(paste0('<div id= file_sim_informations_container class="container-fluid">
                  <h4>', uiSimulatingInformations, '</h4>
                  <div class="row">
                  <div class="col-sm-4">
                  <label>',uiTypeOfInventor,' : </label>
                  <span>', ResultFCM$DataType, '</span>
                  </div>',
                  '<div class="col-sm-4">
                  <label>',uiSiteName,' : </label>
                  <span>', ResultFCM$Site, '</span>
                  </div>',
                  '<div class="col-sm-4">
                  <label>', uiCountry, ': </label>
                  <span>', ResultFCM$Pays, '</span>
                  </div>',
                  '</div>',
                  '<div class="row">
                  <div class="col-sm-4">
                  <label>',Label_espece_or_parcelle,' : </label>
                  <span>', Val_sentier_or_parcelle, '</span>
                  </div>',
                  '<div class="col-sm-4">
                  <label>',uiSimulatingDate, ': </label>
                  <span>', ResultFCM$DateSim, '</span>
                  </div>
                  </div>
                  </div>')
      )
    })
    shinyjs::show('description_file_sim')
  }
  
  updateInfosSite <- function(dataType, site, pays, ClassesDiam){
    classes_diametre = paste(ClassesDiam,collapse=", ")
    output$file_dyn_informations <- renderUI({
    HTML(paste0('<div id= file_dyn_informations_container class="container-fluid">
                  <h4>',uiSiteInformations, '</h4>
                 <div class="row">
                 <div class="col-sm-4">
                 <label>',uiTypeOfInventor,' : </label>
                 <span>', dataType, '</span>
                 </div>',
                 '<div class="col-sm-4">
                 <label>',uiSiteName,' : </label>
                 <span>', site, '</span>
                 </div>',
                 '<div class="col-sm-4">
                 <label>', uiCountry, ' : </label>
                 <span>', pays, '</span>
                 </div>
                 </div>
                <div class="row">
                  <div class="col-sm-4">
                  <label>', uiDiameterClass, ' : </label>
                 <span>', classes_diametre, '</span>
                </div>
                </div>
                </div>')
      )
    })
    shinyjs::show("description_site")
  }
  
  updateUIInputsValues<- function(Parcelles, Sites, Campagnes, ClassesDiam, Species, AnneeExploitation){
    if(!is.null(ParamsDynFile) && file.exists(ParamsDynFile) && file.access(names = ParamsDynFile, mode = 4)==0){
      ParamsDyn = formattingDynData(readRDS(ParamsDynFile))
      updateSelectInput(session, "siteParcelle",  choices = Sites, selected = ParamsDyn$Description$site)
    }
    updateSelectizeInput(session, "parcelle",  choices = levels(Parcelles),
                         selected = NULL, server = TRUE)
    CampagneMin = as.numeric(levels(Campagnes)[1])
    CampagneMax = as.numeric(levels(Campagnes)[length(levels(Campagnes))])
    updateNumericInput(session, "anneedebutSim", value=as.numeric(CampagneMin), min = NA, max = NA, step = NA)
    updateNumericInput(session, "anneefirstlogging", value=as.numeric(CampagneMin), min = NA, max = NA, step = NA)
    updateNumericInput(session, "firstyearcompare", value=AnneeExploitation, min = NA, max = NA, step = NA)
    
  }
  
  loadTableDataSpeciesTraits <- function(SpeciesTraits){
    if(!is.null(SpeciesTraits)){
      if("Nom.sp" %in% names(SpeciesTraits)) nomEspece = SpeciesTraits$Nom.sp
      else nomEspece = SpeciesTraits$Id.sp
      data_log = data.frame(nomEspece=as.character(nomEspece), dme=SpeciesTraits$DME, dma=rep("", length(SpeciesTraits$Id.sp)), tauxPrelevement=rep("", length(SpeciesTraits$Id.sp)), coefRecollement=rep("", length(SpeciesTraits$Id.sp)), tarifcubage=rep("", length(SpeciesTraits$Id.sp)), densite=SpeciesTraits$WSG, stringsAsFactors = FALSE)
      NumberSpecies = length(nomEspece)
      if(NumberSpecies== 1){
        height = 100
      }else if(NumberSpecies> 1 && NumberSpecies<= 7){
        height = 185
      }else if(NumberSpecies> 7 && NumberSpecies<= 10){
        height = 220
      }else{
        height = 300
      }
      ColHeaders = c(uiSpecieName, uiMinimumDiameterOfLogging, uiMinimumDiameterOfPlanning, uiSamplingRate, uiCoefficientOfRetreat, uiVolumeRate, uiDensity)
      output$data_logging <-rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(data_log, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "100%", height = height) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE)%>% 
          hot_cols(columnSorting = TRUE, colWidths= c(150, 75, 75, 150, 150, 200, 75), manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
          hot_col(col = 1, readOnly = TRUE) %>%
          hot_col(col = 2:length(ColHeaders), halign = "htCenter")
      })
      shinyjs::show('bloc_parameters_logging')
    }
  }
  
  loadTableDataStartSpeciesTraits <- function(SpeciesTraits, MyClassesDiam){
    if(!is.null(SpeciesTraits) && !is.null(MyClassesDiam)){
      if("Nom.sp" %in% names(SpeciesTraits)) speciesName = sort(as.character(SpeciesTraits$Nom.sp))
      else speciesName = SpeciesTraits$Id.sp
      ClassesDiamList = c()
      for(iter in 1:(length(MyClassesDiam)-1)){
        CurrentClass = paste0("[", MyClassesDiam[iter], ", ",MyClassesDiam[iter+1], "[")
        ClassesDiamList = c(ClassesDiamList, CurrentClass)
      }
      CurrentClass = paste0("[", MyClassesDiam[length(MyClassesDiam)], ", Inf[")
      ClassesDiamList = c(ClassesDiamList, CurrentClass)
      StrEval1= "dataStart = data.frame(X = ClassesDiamList, "
      StrEval2="colWidths = c(100, "
      colwidths =905/length(speciesName)+1
      for(iter in 1:(length(speciesName)-1)){
        StrEval1 = paste0(StrEval1, speciesName[iter],' = rep("", length(MyClassesDiam)), ')
        StrEval2 = paste0(StrEval2, paste0(colwidths,', '))
      }
      StrEval1 = paste0(StrEval1, speciesName[length(speciesName)],' = rep("", length(MyClassesDiam)), stringsAsFactors = FALSE)')
      eval(parse(text = StrEval1))
      StrEval2 = paste0(StrEval2, paste0(colwidths,')'))
      eval(parse(text = StrEval2))
      ClassesDiamNumber = length(MyClassesDiam)
      if(ClassesDiamNumber== 1){
        height = 50
      }else if(ClassesDiamNumber> 1 && ClassesDiamNumber<= 7){
        height = 220
      }else if(ClassesDiamNumber> 7 && ClassesDiamNumber<= 10){
        height = 250
      }else{
        height = 300
      }
      
      ColHeaders = c(uiClassDiam, speciesName)
      DataStartInit <<- dataStart
      DataStartColHeaders <<- ColHeaders
      DataStartHeight <<- height
      output$data_start_species <-rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(dataStart, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "1000px", height = height) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE)%>%
          hot_cols(columnSorting = TRUE, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
          hot_col(col = 2:length(ColHeaders), halign = "htCenter")
      })
    }
  }
  
  loadTableDataDamagesVector <- function(MyClassesDiam){
    if(!is.null(MyClassesDiam)){
      output$vector_damage <-rhandsontable::renderRHandsontable({
        StrEval1= "data_damages = data.frame("
        StrEval2="ColHeaders = c("
        StrEval3="colWidths = c("
        colwidths =1005/length(MyClassesDiam)
        for(iter in 1:(length(MyClassesDiam)-1)){
          StrEval1 = paste0(StrEval1, 'classe', iter,'=c(""), ')
          StrEval2 = paste0(StrEval2, '"[', MyClassesDiam[iter],', ', MyClassesDiam[iter+1], '[", ')
          StrEval3 = paste0(StrEval3, paste0(colwidths,', '))
        }
        StrEval1 = paste0(StrEval1, 'classe', length(MyClassesDiam),'=c(""), stringsAsFactors = FALSE)')
        eval(parse(text = StrEval1))
        StrEval2 = paste0(StrEval2, '"[', MyClassesDiam[length(MyClassesDiam)],', Inf[")')
        eval(parse(text = StrEval2))
        StrEval3 = paste0(StrEval3, paste0(colwidths,')'))
        eval(parse(text = StrEval3))
        rhandsontable::rhandsontable(data_damages, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "1000px") %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE)%>%
          hot_cols(columnSorting = TRUE, colWidths= colWidths, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
          hot_col(col = 1:length(ColHeaders), halign = "htCenter")
      })
    }
  }
  
  loadTableNumberParDiameterClassVector <- function(MyClassesDiam){
    if(!is.null(MyClassesDiam)){
      output$vector_number_per_diameter_class <-rhandsontable::renderRHandsontable({
        StrEval1= "data_number_per_diameter_class = data.frame("
        StrEval2="ColHeaders = c("
        StrEval3="colWidths = c("
        colwidths =460/length(MyClassesDiam) 
        for(iter in 1:(length(MyClassesDiam)-1)){
          StrEval1 = paste0(StrEval1, 'classe', iter,'=c(""), ')
          StrEval2 = paste0(StrEval2, '"[', MyClassesDiam[iter],', ', MyClassesDiam[iter+1], '[", ')
          StrEval3 = paste0(StrEval3, paste0(colwidths,', '))
        }
        StrEval1 = paste0(StrEval1, 'classe', length(MyClassesDiam),'=c(""), stringsAsFactors = FALSE)')
        eval(parse(text = StrEval1))
        StrEval2 = paste0(StrEval2, '"[', MyClassesDiam[length(MyClassesDiam)],', Inf[")')
        eval(parse(text = StrEval2))
        StrEval3 = paste0(StrEval3, paste0(colwidths,')'))
        eval(parse(text = StrEval3))
        rhandsontable::rhandsontable(data_number_per_diameter_class, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "1000px") %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE)%>%
          hot_cols(columnSorting = TRUE, colWidths= colWidths, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
          hot_col(col = 1:length(ColHeaders), halign = "htCenter")
      })
    }
  }
  
  loadTableContributionTauxRecrutementParDiameter <- function(MyClassesDiam){
    if(!is.null(MyClassesDiam)){
      output$vector_tauxrecrutement_per_diameter_class <-rhandsontable::renderRHandsontable({
        StrEval1= "data_tr_per_diameter_class = data.frame("
        StrEval2="ColHeaders = c("
        StrEval3="colWidths = c("
        colwidths =450/length(MyClassesDiam) 
        for(iter in 1:(length(MyClassesDiam)-1)){
          StrEval1 = paste0(StrEval1, 'classe', iter,'=c(100), ')
          StrEval2 = paste0(StrEval2, '"[', MyClassesDiam[iter],', ', MyClassesDiam[iter+1], '[", ')
          StrEval3 = paste0(StrEval3, paste0(colwidths,', '))
        }
        StrEval1 = paste0(StrEval1, 'classe', length(MyClassesDiam),'=c(100), stringsAsFactors = FALSE)')
        eval(parse(text = StrEval1))
        StrEval2 = paste0(StrEval2, '"[', MyClassesDiam[length(MyClassesDiam)],', Inf[")')
        eval(parse(text = StrEval2))
        StrEval3 = paste0(StrEval3, paste0(colwidths,')'))
        eval(parse(text = StrEval3))
        rhandsontable::rhandsontable(data_tr_per_diameter_class, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "1000px") %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE)%>%
          hot_cols(columnSorting = TRUE, colWidths= colWidths, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
          hot_col(col = 1:length(ColHeaders), halign = "htCenter")
      })
    }
  }
  
  #########Chargement des paramètres de la dynamique ###############################################
  observeEvent(input$load_dyn,{
    SimSiteName <<- getCSVSiteName();
    SimSiteNameFile <<- NULL
    # tryCatch({
      shinyjs::hide('load_dyn')
      shinyjs::hide('bloc_parameters_logging')
      shinyjs::hide('bloc_parameters_simulation')
      shinyjs::show('boxloader_FileDyn')
      inFile = parseFilePaths(roots2, input$load_dyn)
      Loadpath = as.character(inFile$datapath)
      ParamsDynFile <<- Loadpath
      if(!is.null(ParamsDynFile) && length(ParamsDynFile) > 0){
        ParamsDyn = formattingDynData(readRDS(ParamsDynFile))
        if(!is.null(ParamsDyn)){
          TypeInventaire = ParamsDyn$DataType
          ClassesDiam = ParamsDyn$ClassesDiam
          updateInfosSite(TypeInventaire, ParamsDyn$Descriptif$site, ParamsDyn$Descriptif$pays, ClassesDiam)
          NewSpeciesTraits = ParamsDyn$SpeciesTraits
          if(!is.null(NewSpeciesTraits)){
            SpeciesTraitsSim <<- formattingSimData(NewSpeciesTraits)
          }
          if(!is.null(TypeInventaire) && TypeInventaire == "sentier"){
            Essences = NULL
            if(!is.null(NewSpeciesTraits)){
              Essences = NewSpeciesTraits$Nom.sp
            }
            updateSelectizeInput(session, "espece",  choices = levels(Essences),
                                 selected = NULL, server = TRUE)
            shinyjs::hide("aggregate_parcels_bloc")
            updateCheckboxInput(session, "aggregate_parcels", value = FALSE)
            loadTableNumberParDiameterClassVector(ClassesDiam)
            loadTableContributionTauxRecrutementParDiameter(ClassesDiam)
            #loadTableDataSpeciesTraits(SpeciesTraitsSim)
            shinyjs::hide('bloc_params_logging_parcelle')
            shinyjs::hide('bloc_list_parcelles_sim')
            shinyjs::hide('bloc_start_species_number')
            shinyjs::hide('bloc_corresponding_species')
            shinyjs::hide('siteParcelle')
            shinyjs::hide('file_data_start')
            shinyjs::hide('surfaceparcelle')
            shinyjs::hide('anneedebutSimAutreSite')
            shinyjs::show('anneedebutSim')
            shinyjs::show('delayDynamicsPostExploitation')
            shinyjs::show('bloc_params_logging_sentier')
            shinyjs::show('bloc_espece_sentier')
            shinyjs::show('bloc_recrutement_sentier')
            shinyjs::show('bloc_tauxrecrutement_sentiers_sim')
            shinyjs::show('bloc_estimation_tcs_dimako')
          }else if(!is.null(TypeInventaire) && TypeInventaire == "parcelle"){
            Parcelles = as.factor(ParamsDyn$Descriptif$Parcelles)
            Campagnes = as.factor(ParamsDyn$Descriptif$Campagnes)
            Species = ParamsDyn$SpeciesTraits$Nom.sp
            AnneeExploitation = as.numeric(ParamsDyn$Descriptif$AnneeExploitation)
            Sites = getSitesListForParcelle()
            updateUIInputsValues(Parcelles, Sites, Campagnes, ClassesDiam, Species, AnneeExploitation)
            shinyjs::hide('bloc_params_logging_sentier')
            shinyjs::hide('bloc_espece_sentier')
            shinyjs::hide('bloc_recrutement_sentier')
            shinyjs::hide('bloc_tauxrecrutement_sentiers_sim')
            shinyjs::hide('bloc_list_parcelles_sim')
            shinyjs::hide('bloc_estimation_tcs_dimako')
            shinyjs::hide('delayDynamicsPostExploitation')
            shinyjs::show('anneedebutSimAutreSite')
            shinyjs::show('bloc_params_logging_parcelle')
            shinyjs::show('bloc_start_species_number')
            shinyjs::show('siteParcelle')
            shinyjs::show('file_data_start')
            shinyjs::show('surfaceparcelle')
            shinyjs::show('anneedebutSim')
          }
          loadTableDataDamagesVector(ClassesDiam)
          
        }
        shinyjs::hide('boxloader_FileDyn')
        shinyjs::show('load_dyn')
        shinyjs::show('action_save_sim_file')
        shinyjs::show('actions_sim')
        shinyjs::show('bloc_parameters_simulation')
        showNotification(uiFileImportedSuccessfully, duration = 10, type = "message")
      }else{
        shinyjs::hide('boxloader_FileDyn')
        shinyjs::show('load_dyn')
      }
      
    # },error=function(e){
    #   shinyjs::hide('boxloader_FileDyn')
    #   shinyjs::show('load_dyn')
    #   showModal(modalDialog(
    #     title=uiErrorWhileLoading,
    #     size = "m",
    #     footer = modalButton(uiClose),
    #     e$message
    #     )
    #   )
    # })
  })
  
  ######Fonction du tracé de la structure diametrique et de l'affichage des données de l'évolution dans le temps###########
observeEvent(input$plot_SCD,{
  if(!is.null(CurrentResultSimDataFile) && file.exists(CurrentResultSimDataFile) && file.access(names = CurrentResultSimDataFile, mode = 4)==0){
    #ParamsDyn = readRDS(ParamsDynFile);
    load(file = CurrentResultSimDataFile)
    if(input$whatSD == 1){
      execute_plotSCD <- reactive({
        validate(
          if(input$selectGESDAll == FALSE){
            need(input$groupe_espece_strDia != "", uiSelectSpeciesGroup)
          } 
        )
        shinyjs::hide('plot_SCD')
        shinyjs::show('boxloader_SCD')
        if(!is.null(ResultFCM$DataType) && ResultFCM$DataType == "parcelle"){
          if(input$selectGESDAll == FALSE){
            selectedSpecies = as.factor(input$groupe_espece_strDia)
            Groups=list(stand=selectedSpecies)
          }else{
            Groups = list(stand=as.factor(ResultFCM$SpeciesTraits$Nom.sp))
          }
        }else if(!is.null(ResultFCM$DataType) && ResultFCM$DataType == "sentier"){
          if(input$selectGESDAll == FALSE){
            selectedSpecies = as.factor(input$groupe_espece_strDia)
            Groups = subset(ResultFCM$SpeciesTraits,  ResultFCM$SpeciesTraits$Nom.sp%in%selectedSpecies)$Id.sp
            Groups=list(stand=as.factor(Groups))
          }else{
            Groups = list(stand=as.factor(ResultFCM$SpeciesTraits$Id.sp))
          }
        }
        
        p <- ggplot()
        ######Function PlotSCD renvoi le graphe de la structure diametrique cummulée contenant aussi ses données###########
        p <- PlotSCD(ResultFCM, Groups = Groups)
        DataToExportSCD = data.frame()
        PlotToExportSCD = p
        plotbuild <- ggplot_build(p)
        if(length(plotbuild$data) >= 1 && nrow(plotbuild$data[[1]]) != 0){
          DataToExportTmp <- NULL
          for(i in 1:length(plotbuild$data)){
            data_class <- plotbuild$data[[i]]
            data_class <- data.table(Temps=data_class$x, cumul=data_class$y, key = "Temps")
            names(data_class)[2] <- paste0(uiCumulClass, " ", i)
            if(is.null(DataToExportTmp)){
              DataToExportTmp <- data_class
            }else{
              DataToExportTmp <- merge(DataToExportTmp, data_class, all.x=T, all.y=T)
            }
          }
          DataToExportSCD = DataToExportTmp
          PlotToExportSCD = p
        }
        output$plot_strDia <- renderPlot({
          print(p)
        })
        output$data_strDia <- DT::renderDataTable(DataToExportSCD)
        save(DataToExportSCD, file = paste0(DataExportsFolder, "\\DataExportSCD.RData"))
        save(PlotToExportSCD, file = paste0(DataExportsFolder, "\\PlotToExportSCD.RData"))
      })
      
    }else{
      execute_plotSCD <- reactive({
        validate(
          if(input$selectGESDAll == FALSE){
            need(input$groupe_espece_strDia != "", uiSelectSpeciesGroup)
          }
        )
        shinyjs::hide('plot_SCD')
        shinyjs::hide('bloc_graphique_SCD')
        shinyjs::hide('bloc_donnees_SCD')
        shinyjs::show('boxloader_SCD')
        
        StoreTime = ResultFCM$StoreTime
        StartingDate = StoreTime[1]
        if(!is.null(ResultFCM$DataType) && ResultFCM$DataType == "parcelle"){
          if(input$selectGESDAll == FALSE){
            selectedSpecies = as.factor(input$groupe_espece_strDia)
            Groups=list(stand=selectedSpecies)
          }else{
            Groups = list(stand=as.factor(ResultFCM$SpeciesTraits$Nom.sp))
          }
        }else if(!is.null(ResultFCM$DataType) && ResultFCM$DataType == "sentier"){
          if(input$selectGESDAll == FALSE){
            selectedSpecies = as.factor(input$groupe_espece_strDia)
            Groups = subset(ResultFCM$SpeciesTraits,  ResultFCM$SpeciesTraits$Nom.sp%in%selectedSpecies)$Id.sp
            Groups=list(stand=as.factor(Groups))
          }else{
            Groups = list(stand=as.factor(ResultFCM$SpeciesTraits$Id.sp))
          }
        }
        myYear = input$yearslider_strDiam
        ######Function GetTabSD renvoie  la matrix des données de la structure diametrique contenu dans le fichier MyAppInterfaceWithSimulator.R###########
        tabEffs <- GetTabSD(ResultFCM, Groups = Groups, MyDate = myYear)
        StrEval = "ClassDiamPlot = c("
        ClassesDiam = ResultFCM$ClassesDiam
        for(iter in 1:(length(ClassesDiam)-1)){
          StrEval = paste0(StrEval, '"[', ClassesDiam[iter],', ', ClassesDiam[iter+1], '[", ')
        }
        StrEval = paste0(StrEval, '"[', ClassesDiam[length(ClassesDiam)],', ..[")')
        eval(parse(text = StrEval))
        data_SD = data.frame(Class= factor(ClassDiamPlot, levels = ClassDiamPlot), Effectif = tabEffs[,1], EffectifMin = tabEffs[,2], EffectifMax = tabEffs[,3], stringsAsFactors = FALSE)
        if(length(Groups$stand) > 5){
          names_species_title = paste(paste(as.character(Groups$stand)[1:5], collapse = ", "), ", ...")
        }else{
          names_species_title = paste(as.character(Groups$stand), collapse = ", ")
        }
        
        Mytitle=paste(uiPredictionDistributionNumberOfTrees, ":", names_species_title, uiByClassOfDiameterIn, input$yearslider_strDiam, sep = " ")
        p <-ggplot(data=data_SD, aes(x=Class, y=Effectif)) + geom_bar(colour="black", fill="#6AC1F1", width=.4, stat="identity")+
          geom_errorbar(aes(ymin=EffectifMin, ymax=EffectifMax), width=.2, position=position_dodge(.9)) + theme_bw() + xlab("Diameter class") + ylab("Effective by class") +
          ggtitle(Mytitle)
        output$plot_strDia <- renderPlot({
          print(p)
        })
        names(data_SD) <- c(uiClass, uiEffective, uiMinimalEffective, uiMaximalEffective)
        output$data_strDia<- DT::renderDataTable(data_SD)
        DataToExportSCD = data_SD
        PlotToExportSCD = p
        save(DataToExportSCD, file = paste0(DataExportsFolder, "\\DataExportSCD.RData"))
        save(PlotToExportSCD, file = paste0(DataExportsFolder, "\\PlotToExportSCD.RData"))
      })
    }
    tryCatch({
      print("Start plot SCD")
      execute_plotSCD()
      print("End plot SCD")
      shinyjs::hide('boxloader_SCD')
      shinyjs::show('plot_SCD')
      shinyjs::show('bloc_export_sd_rg')
      shinyjs::show('bloc_export_sd_data')
      shinyjs::show('bloc_graphique_SCD')
      shinyjs::show('bloc_donnees_SCD')
    }, error=function(e){
      showModal(modalDialog(
          title=uiError,
          size = "m",
          footer = modalButton(uiClose),
          e$message
        )
      )
      shinyjs::hide('boxloader_SCD')
      shinyjs::show('plot_SCD')
    })
  }else{
    showModal(modalDialog(
      title=uiError,
      size = "m",
      footer = modalButton(uiClose),
      uiLoadSimulatedDataFileOrStartSimulation
    ) 
    )
    
  }
})

observeEvent(input$plageClass,{
  if(input$plageClass == 1){
    shinyjs::hide('classMinCol')
    shinyjs::hide('classMaxCol')
    shinyjs::show('diamMinCol')
    shinyjs::show('diamMaxCol')
  }else if(input$plageClass == 2){
    shinyjs::hide('diamMinCol')
    shinyjs::hide('diamMaxCol')
    shinyjs::show('classMinCol')
    shinyjs::show('classMaxCol')
    
  }
})

#####################Fonction associé à l'évènement du choix du type de structure diametrique à visualiser ##############
observeEvent(input$whatSD,{
  if(input$whatSD == 1){
    shinyjs::hide('slider_SD')
  }else if(input$whatSD == 2){
    shinyjs::show('slider_SD')
  }
})

#####################Fonction associé à l'évènement du choix du type de simulation (parcelles ou sentier) ##############
observeEvent(input$simulation_type,{
  if(input$simulation_type == 1){
    shinyjs::hide('bloc_number_per_diameter_class')
  }else if(input$simulation_type== 2){
    shinyjs::show('bloc_number_per_diameter_class')
  }
})


######Fonction du tracé de l'indicateur et de l'affichage des données de l'évolution dans le temps###########
Plot_Indicateur <- function(MyTimeInterval=NULL){
  if(!is.null(CurrentResultSimDataFile) && file.exists(CurrentResultSimDataFile) && file.access(names = CurrentResultSimDataFile, mode = 4)==0){
    load(file = CurrentResultSimDataFile)
    ClassesDiam = ResultFCM$ClassesDiam
    tryCatch({
      validate(
        if(input$selectGEIAll == FALSE) need(input$groupe_espece_indicateur != "", uiSelectSpeciesGroup),
        if(input$plageClass == 1){
          need(input$diamMin <= input$diamMax, uiDiameterMinNotSupDiameterMax)
        }else if(input$plageClass == 2){
          need(input$classMin <= input$classMax, uiClassDiamMinNotSupClassDiamMax)
        } 
      )
      shinyjs::hide('afficher_indicateur')
      shinyjs::hide('bloc_graphique_Indicateur')
      shinyjs::hide('bloc_donnees_Indicateur')
      shinyjs::show('boxloader_IND')
      i= 1
      Indicator = ListOfIndicators[[i]]
      while (Indicator$NomInd != input$indicateur) {
        i= i+1
        Indicator = ListOfIndicators[[i]]
      }
      if(input$selectGEIAll == FALSE){
        selectedSpecies = as.factor(input$groupe_espece_indicateur)
        Groups=list(stand=selectedSpecies)
      }else{
        Groups = list(stand=as.factor(ResultFCM$SpeciesTraits$Nom.sp))
      }
      Verif = input$verif_indicator
      p <- ggplot()
      if(Indicator$NomInd == uiUsableVolume || Indicator$NomInd == uiUsableStock || Indicator$NomInd == uiIventoryReplishmentRate){
        ClassesDiamLikeNumber = as.numeric(ResultFCM$ParamPlot$CDSTB$ClassesDiam)
        ClasseDiamDME = findInterval(ResultFCM$SpeciesTraits$DME[1], ClassesDiam)
        MyClassesDiam = as.factor(ClassesDiamLikeNumber[ClassesDiamLikeNumber>=ClasseDiamDME])
      }else{
        if(input$plageClass == 1){
          MyClassesDiam = as.factor(c(findInterval(input$diamMin, ClassesDiam):findInterval(input$diamMax, ClassesDiam)))
        }else if(input$plageClass == 2){
          MyClassesDiam = as.factor(c(input$classMin:input$classMax))
        }
      }
      ######Fonction PlotMyIndicator renvoie le graphe de l'indicateur contenant aussi ses données###########
      p <- PlotMyIndicator(ResultFCM, Groups = Groups, Verif = Verif, MyClassesDiam = MyClassesDiam, MyTimeInterval = MyTimeInterval, MyIndicator = Indicator)
      plotbuild <- ggplot_build(p)
      DataToExport = data.frame()
      PlotToExport = p
      taille = length(plotbuild$data)
      printData= TRUE
      if(taille > 1 && nrow(plotbuild$data[[1]]) != 0){
        entete = c(uiTime)
        if(taille > 1){
          data_med <- plotbuild$data[[1]]
          temps= data_med$x
          ExpPlot = paste0("data_med <- data.table(Temps=data_med$x, Med",Indicator$VarInd,'=data_med$y, key = "Temps")', collapse = '')
          eval(parse(text = ExpPlot))
          DataToExportTmp <- data_med
          entete = c(entete, paste0(uiMedianOf," ", Indicator$VarInd, collapse = ''))
        }
        if(taille >= 2){
          data_li <- plotbuild$data[[2]]
          ExpPlot = paste0("data_li <- data.table(Temps=data_li$x, Min",Indicator$VarInd,'=data_li$y, key = "Temps")', collapse = '')
          eval(parse(text = ExpPlot))
          DataToExportTmp = merge(DataToExportTmp, data_li, all.x=T, all.y=T)
          entete = c(entete, paste0(uiMinimumOf, " ", Indicator$VarInd, collapse = ''))
        }
        if(taille >= 3){
          data_ls <- plotbuild$data[[3]]
          ExpPlot = paste0("data_ls <- data.table(Temps=data_ls$x, Max",Indicator$VarInd,'=data_ls$y, key = "Temps")', collapse = '')
          eval(parse(text = ExpPlot))
          DataToExportTmp = merge(DataToExportTmp, data_ls, all.x=T, all.y=T)
          entete = c(entete, paste0(uiMaximalOf, " ", Indicator$VarInd, collapse = ''))
        }
        if(taille >= 4){
          data_verif <- plotbuild$data[[4]]
          ExpPlot = paste0("data_verif <- data.table(Temps=data_verif$x, Verif",Indicator$VarInd,'=data_verif$y, key = "Temps")', collapse = '')
          eval(parse(text = ExpPlot))
          DataToExportTmp = merge(DataToExportTmp, data_verif, all.x=T, all.y=T)
          entete = c(entete, paste0(uiRealDataOf, " ", Indicator$VarInd, collapse = ''))
        }
      }else if(taille==1 && nrow(plotbuild$data[[1]]) != 0){
        data_verif <- plotbuild$data[[1]]
        ExpPlot = paste0("data_verif <- data.table(Temps=data_verif$x, Verif",Indicator$VarInd,'=data_verif$y, key = "Temps")', collapse = '')
        eval(parse(text = ExpPlot))
        DataToExportTmp = merge(DataToExportTmp, data_verif, all.x=T, all.y=T)
        entete = c(uiTime, entete, paste0(uiRealDataOf, " ", Indicator$VarInd, collapse = ''))
      }else{
        printData= FALSE
      }
      if(printData){
        DataToExport = DataToExportTmp
        PlotToExport = p
        names(DataToExport) <- entete
      }
      output$data_indicateur <- DT::renderDataTable(DataToExport)
      output$plot_indicateur <- renderPlot({
        print(p)
      })
      save(DataToExport, file = paste0(DataExportsFolder, "\\DataExport.RData"))
      save(PlotToExport, file = paste0(DataExportsFolder, "\\PlotToExport.RData"))
      shinyjs::hide('boxloader_IND')
      shinyjs::show('afficher_indicateur')
      shinyjs::show('bloc_export_indicateur_rg')
      shinyjs::show('bloc_export_indicateur_data')
      shinyjs::show('bloc_graphique_Indicateur')
      shinyjs::show('bloc_donnees_Indicateur')
      print("End plot Indicateur")
      }, error=function(e){
        showModal(modalDialog(
            title=uiError,
            size = "m",
            footer = modalButton(uiClose),
            e$message
          )
        )
        shinyjs::hide('boxloader_IND')
        shinyjs::show('afficher_indicateur')
    })
  }else{
    showModal(modalDialog(
        title=uiError,
        size = "m",
        footer = modalButton(uiClose),
        uiLoadSimulatedDataFileOrStartSimulation
      ) 
    )
  }
  
}
############## Function associée à l'évènement du choix des parcelles à simuler ######################
observeEvent(input$Allparcelle,{
  if(input$Allparcelle == TRUE){
    shinyjs::hide("parcelle_col")
    if(!is.null(SimSiteName) && SimSiteName != getCSVSiteName()){
      if(!is.null(SimSiteNameFile) && file.exists(SimSiteNameFile) && file.access(names = SimSiteNameFile, mode = 4)==0){
        ParamsSimSite = readRDS(SimSiteNameFile)
        ParamsSimSite = formattingSimData(ParamsSimSite$SimData)
        StartingDates = sort(unique(ParamsSimSite$Id.campagne))
        updateSelectInput(session, "anneedebutSimAutreSite",  choices = StartingDates,
                          selected = NULL )
        shinyjs::hide("anneedebutSim")
        shinyjs::hide("surfaceparcelle")
        shinyjs::show("anneedebutSimAutreSite")
      }
    }
    updateCheckboxInput(session, "aggregate_parcels", value = FALSE)
    shinyjs::show("aggregate_parcels_bloc")
  }else{
    shinyjs::show("parcelle_col")
    if(length(input$parcelle) <= 1){
      shinyjs::hide("aggregate_parcels_bloc")
      updateCheckboxInput(session, "aggregate_parcels", value = FALSE)
    }
  }
})


############## Function associée à l'évènement d'affichage d'un indicateur (courbe d'évolution et données) ######################
observeEvent(input$afficher_indicateur,{
  YearRange = input$yearrange_indicateur
  MyTimeInterval = YearRange[1]:YearRange[2]
  Plot_Indicateur(MyTimeInterval)
})

################ Event on select species  ################################
observeEvent(input$espece,{
  if(!is.null(ParamsDynFile) && file.exists(ParamsDynFile) && file.access(names = ParamsDynFile, mode = 4)==0){
    if(!is.null(input$espece) && input$espece != ""){
      tryCatch({
        Essence = input$espece
        ParamsDyn = formattingDynData(readRDS(ParamsDynFile))
        shinyjs::hide('bloc_parameters_logging')
        if(!is.null(ParamsDyn)){
          SpeciesTraitsSim <<- subset(ParamsDyn$SpeciesTraits, as.character(ParamsDyn$SpeciesTraits$Nom.sp)== Essence)
          loadTableDataSpeciesTraits(SpeciesTraitsSim)
        }
      }, error=function(e){
        showModal(modalDialog(
          title=uiError,
          size = "m",
          footer = modalButton(uiClose),
          e$message
        ) 
        )
      })
    }
  }  
}, ignoreInit = TRUE, ignoreNULL = TRUE)
################ Event on select parcels of another site ################################
observeEvent(input$siteParcelle,{
  if(!is.null(ParamsDynFile) && file.exists(ParamsDynFile) && file.access(names = ParamsDynFile, mode = 4)==0){
  tryCatch({
    shinyjs::hide('bloc_parameters_logging')
      ParamsDyn = formattingDynData(readRDS(ParamsDynFile))
      ClassesDiamRef = ParamsDyn$ClassesDiam
      SimSiteName <<- input$siteParcelle
      if(!is.null(SimSiteName) && SimSiteName != getCSVSiteName()){
        SimSiteNameFile <<- paste(DataSimFolder, paste0(SimSiteName, ".rds") , sep='/')
        if(!is.null(SimSiteNameFile) && file.exists(SimSiteNameFile) && file.access(names = SimSiteNameFile, mode = 4)==0){
          ParamsSimSite = readRDS(SimSiteNameFile)
          ClassesDiamSimSite = ParamsSimSite$ClassesDiam
          ParamsSimSite = formattingSimData(ParamsSimSite$SimData)
          if(length(ClassesDiamSimSite) == length(ClassesDiamRef) && sum(ClassesDiamSimSite%in%ClassesDiamRef) == length(ClassesDiamSimSite)){
            Parcelles = unique(ParamsSimSite$Id.zone)
            updateSelectizeInput(session, "parcelle",  choices = sort(Parcelles),
                                 selected = NULL, server = TRUE)
            updateSelectInput(session, "anneedebutSimAutreSite", choices = NULL, selected = NULL)
            shinyjs::hide("bloc_start_species_number")
            shinyjs::show("bloc_corresponding_species")
            if(!is.null(ParamsDyn) && !is.null(ParamsSimSite)){
              SpeciesTraitsSite = data.frame(Nom.sp = as.factor(levels(ParamsSimSite$Nom.sp)))
              loadTableCorrespondingSpecies(ParamsDyn$SpeciesTraits, SpeciesTraitsSite)
              loadTableToUpdateDataLogging(SpeciesTraitsSite, ParamsDyn$SpeciesTraits, CorrespondingSpecies= NULL)
            }
          }else{
            showModal(modalDialog(
                title=uiError,
                size = "m",
                footer = modalButton(uiClose),
                uiSimClassDiamDoNotCorrespondThoseOfDynamic
              ) 
            )
          }
        }
        shinyjs::hide("anneedebutSim")
        shinyjs::hide("surfaceparcelle")
        shinyjs::hide("file_data_start")
        shinyjs::show("bloc_list_parcelles_sim")
        shinyjs::show("anneedebutSimAutreSite")
      }else{
        updateSelectizeInput(session, "parcelle",  choices = NULL,
                             selected = NULL, server = TRUE)
        
        shinyjs::hide("bloc_corresponding_species")
        shinyjs::hide("bloc_list_parcelles_sim")
        shinyjs::hide("anneedebutSimAutreSite")
        if (!is.null(input$file_data_start)){
          shinyjs::show("bloc_start_species_number")
        }else{
          shinyjs::hide("bloc_start_species_number")
        }
        shinyjs::show("file_data_start")
        shinyjs::show("surfaceparcelle")
        shinyjs::show("anneedebutSim")
      }
      updateSelectInput(session, "anneedebutSimAutreSite",  choices = NULL,
                        selected = NULL )
      updateCheckboxInput(session, "Allparcelle", value = FALSE)
      updateCheckboxInput(session, "aggregate_parcels", value = FALSE)
      shinyjs::hide("aggregate_parcels_bloc")
      
  }, error= function(e){
    showNotification(uiErrorWhileChoosingSimulatedSiteOfPLots, duration = 10, type = "error")
  })
}
})
################ Event on select parcels list ################################
observeEvent(input$parcelle,  {
  if(!is.null(ParamsDynFile) && file.exists(ParamsDynFile) && file.access(names = ParamsDynFile, mode = 4)==0){
    tryCatch({
      ParamsDyn = formattingDynData(readRDS(ParamsDynFile))
      if(is.null(input$parcelle)){
        if(!is.null(SimSiteName) && SimSiteName != getCSVSiteName()){
          shinyjs::hide("bloc_start_species_number")
        }
        shinyjs::hide("aggregate_parcels_bloc")
        updateCheckboxInput(session, "aggregate_parcels", value = FALSE)
        shinyjs::hide("anneedebutSimAutreSite")
        shinyjs::show("surfaceparcelle")
        shinyjs::show("anneedebutSim")
      }else{
        selectedParcels = as.factor(input$parcelle)
        if(length(selectedParcels) > 1){
          updateCheckboxInput(session, "aggregate_parcels", value = FALSE)
          shinyjs::show("aggregate_parcels_bloc")
        }else{
          shinyjs::hide("aggregate_parcels_bloc")
          updateCheckboxInput(session, "aggregate_parcels", value = FALSE)
        }
        if(!is.null(SimSiteName) && SimSiteName != getCSVSiteName()){
          shinyjs::hide("bloc_start_species_number")
          if(!is.null(SimSiteNameFile) && file.exists(SimSiteNameFile) && file.access(names = SimSiteNameFile, mode = 4)==0){
            ParamsSimSite = readRDS(SimSiteNameFile)
            ParamsSimSite = formattingSimData(ParamsSimSite$SimData)
            StartingDates = sort(unique(ParamsSimSite$Id.campagne[ParamsSimSite$Id.zone%in%selectedParcels]))
            updateSelectInput(session, "anneedebutSimAutreSite",  choices = StartingDates,
                              selected = NULL )
            shinyjs::hide("anneedebutSim")
            shinyjs::hide("surfaceparcelle")
            shinyjs::show("anneedebutSimAutreSite")
          }
        }
      }
    }, error= function(e){
      showNotification(uiErrorWhileChoosingSimulatedPLots, duration = 10, type = "error")
    })
  }
}, ignoreNULL = FALSE)

################ Event on select species of during indicators visualization ################################
observeEvent(input$selectGEIAll,{
  if(input$selectGEIAll == TRUE){
    shinyjs::hide("groupe_espece_indicateur_col")
  }else{
    shinyjs::show("groupe_espece_indicateur_col")
  }
})

observeEvent(input$selectGESDAll,{
  if(input$selectGESDAll == TRUE){
    shinyjs::hide("groupe_espece_strDia_col")
  }else{
    shinyjs::show("groupe_espece_strDia_col")
  }
})
######Fonction de l'exportation des données de l'indicateur sous format pdf et csv###########
output$download_indic_data <- downloadHandler(
  
  filename = function() {paste(input$indicateur,Sys.Date(),'.', input$extensionData, sep='')},
  content = function(file) {
    if(file.exists(paste0(DataExportsFolder, "\\DataExport.RData")) && file.access(names = paste0(DataExportsFolder, "\\DataExport.RData"), mode = 4)==0){
      load(paste0(DataExportsFolder, "\\DataExport.RData"))
      if(input$extensionData == "pdf"){
        pdf(file = file)
        gridExtra::grid.table(DataToExport)
        dev.off()
      }else{
        write.table(DataToExport, file, col.names=TRUE, row.names = FALSE, sep=";")
      }
    }else{
      DataToExport = data.frame()
    }
    rm(DataToExport)
  }
)
######Fonction de l'exportation des données de la structure diametrique sous format pdf et csv###########
output$download_strucDiam_data <- downloadHandler(
  
  filename = function() {paste("structure_dia_cumul", Sys.Date(), '.',input$extensionSDData, sep='')},
  content = function(file) {
    if(file.exists(paste0(DataExportsFolder, "\\DataExportSCD.RData")) && file.access(names = paste0(DataExportsFolder, "\\DataExportSCD.RData"), mode = 4)==0){
      load(paste0(DataExportsFolder, "\\DataExportSCD.RData"))
      if(input$extensionSDData == "pdf"){
        pdf(file = file)
        gridExtra::grid.table(DataToExport)
        dev.off()
      }else{
      write.table(DataToExportSCD, file, col.names=TRUE, row.names = FALSE, sep=";")
      }
    }else{
      DataToExportSCD = data.frame()
    }
    rm(DataToExportSCD)
  }
)
plotStrInput <- function(MyPlot = NULL){
  
  print(MyPlot)
}

plotInput <- function(MyPlot = NULL){
  print(MyPlot)
}
######Fonction de l'exportation du graphique de l'indicateur sous format pdf, png, jpeg###########
output$download_indic_rg <- downloadHandler(
  filename =  function() {
    paste(input$indicateur,Sys.Date(), input$extension, sep=".")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    if(file.exists(paste0(DataExportsFolder, "\\PlotToExport.RData")) && file.access(names = paste0(DataExportsFolder, "\\PlotToExport.RData"), mode = 4)==0){
      load(paste0(DataExportsFolder, "\\PlotToExport.RData"))
      if(input$extension == "pdf"){
        device <- function(..., width, height) {
          grDevices::pdf(..., width = width, height = height)
        }
        ggsave(file, plot = PlotToExport, device = device(width = 17, height = 6))
      }else{
        StrEval = paste0(input$extension, '(file, width=900)')
        eval(parse(text = StrEval))
        plotInput(MyPlot = PlotToExport) # for GGPLOT
        dev.off()  # turn the device off
      }
    }else{
      PlotToExport = ggplot()
    }
    rm(PlotToExport)
  })
######Fonction de l'exportation du graphique de la structure diametrique sous format pdf, png, jpeg###########
output$download_strucDiam_rg <- downloadHandler(
  filename =  function() {
    paste("Structure_Diametrique",Sys.Date(), input$extensionSD, sep=".")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    if(file.exists(paste0(DataExportsFolder, "\\PlotToExportSCD.RData")) && file.access(names = paste0(DataExportsFolder, "\\PlotToExportSCD.RData"), mode = 4)==0){
      load(paste0(DataExportsFolder, "\\PlotToExportSCD.RData"))
      if(input$extensionSD == "pdf"){
        device <- function(..., width, height) {
          grDevices::pdf(..., width = width, height = height)
        }
        ggsave(file, plot = PlotToExportSCD, device = device(width = 17, height = 6))
      }else{
        StrEval = paste0(input$extensionSD, '(file, width=900)')
        eval(parse(text = StrEval))
        plotInput(MyPlot = PlotToExportSCD) # for GGPLOT
        dev.off()  # turn the device off
      }
    }else{
      PlotToExportSCD = ggplot()
    }
    rm(PlotToExportSCD)
  })
######Fonction de l'image de la page d'accueil au lancement de l'application###########
output$imageDynaffor <- renderImage({
  # When input$n is 3, filename is ./images/logoDynaffor
  filename <- normalizePath(file.path('./images',
                                      paste('logoDynaffor','.png', sep='')))
  
  # Return a list containing the filename and alt text
  list(src = filename,
       alt = "logo DynAfFor")
  
}, deleteFile = FALSE)
shinyjs::hide('boxloader')
######Mise à jour des inputs sur l'interface graphique du fichier ui.R au lancement de l'application###########
updateSelectizeInput(session, "parcelle",  choices = NULL,
                     selected = NULL, server = TRUE)
updateSelectizeInput(session, "groupe_espece_indicateur", choices = NULL,
                     selected = NULL,  server = TRUE)
updateSelectizeInput(session, "logging_species", choices = NULL,
                     selected = NULL,  server = TRUE)
updateSelectizeInput(session, "groupe_espece_strDia", choices = NULL,
                     selected = NULL,  server = TRUE)
choicesParcelles = NULL

# updateSliderInput(session, "yearrange_indicateur", value = NULL,
#                     min = NULL, max = NULL, step = 1)
# 
# 
# updateSliderInput(session, "yearslider_strDiam", value = NULL,
#                     min = NULL, max = NULL, step = 1)
# #alphaInd = MBaikiFormatted$alpha
updateNumericInput(session, "diamMin", value = NULL,
                   min = NULL, max = NULL, step = 1)
updateNumericInput(session, "diamMax", value = NULL,
                   min = NULL, max = NULL, step = 1)
updateNumericInput(session, "classMin", value = 1,
                   min = NULL, max = NULL, step = 1)
updateNumericInput(session, "classMax", value = NULL,
                   min = NULL, max = NULL, step = 1)

updateSelectIndicator()
loadUpdatedIndicator()
initDiameterClassList()
})



