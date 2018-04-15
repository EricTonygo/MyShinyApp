# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
rm(list = ls())
library(shinydashboard)
library(rhandsontable)
library(data.table)
library(flexmix)
library(ggplot2)
library(DT)
assign("DataFolder",  paste0(normalizePath(path="~"), "\\DafSim"), envir = .GlobalEnv)
assign("ParamsDynFile",  "", envir = .GlobalEnv)
assign("CurrentSimDataFile", "", envir = .GlobalEnv)
source("R/InferFCM.R")
source("R/ClasseDiamSTAGB.R")
source("R/StartData.r")
source("MyAppInterfaceWithSimulator.R")
source("algoExtractIndicator.R")
source("buildListOfIndicator.R",local=T)
#source("R/plot.FCM.r")

dir.exists <- function(d) {
  de <- file.info(d)$isdir
  ifelse(is.na(de), FALSE, de)
}



shinyServer(function(input, output, session){ 
  session$onSessionEnded(function() {
    stopApp()
  })
  ############################## Creation du dossier de sauvegarde des paramètres de la simulation ##############################
  ParamsimFolder = path.expand(path="~/DafSim/ParametresSimulation")
  if(!dir.exists(ParamsimFolder)) {
    dir.create(ParamsimFolder, recursive = TRUE)
    Noms1=paste("ParametresSimulation",list.files("ParametresSimulation"),sep='/')
    Noms2=paste(ParamsimFolder,list.files("ParametresSimulation"),sep='/')
    file.copy(Noms1,Noms2) 
  }
  
  ############################## Creation du dossier de sauvegarde des paramètres de la dynamique ##############################
  ParamDynFolder = path.expand(path="~/DafSim/ParametresDynamique")
  if(!dir.exists(ParamDynFolder)) {
    dir.create(ParamDynFolder, recursive = TRUE)
    Noms1=paste("ParametresDynamique",list.files("ParametresDynamique"),sep='/')
    Noms2=paste(ParamDynFolder,list.files("ParametresDynamique"),sep='/')
    file.copy(Noms1,Noms2) 
  }
  
  ############################## Creation du dossier de sauvegarde des données simulées ##############################
  SimDataFolder = path.expand(path="~/DafSim/data/Simulations")
  if(!dir.exists(SimDataFolder)) {
    dir.create(SimDataFolder, recursive = TRUE)
  }
  
  
  #volumes <- getVolumes()
  ##################################################################################################################################
  #initialisation du dossier par defaut dans lequel se feront la sauvegarde et le chargement des paramètrages des simulations
  #contenant des scéarios d'exploitation forestière et les autres parmètres de la simulation
  #Puis initialisation des fonctions de sauvegarde et chargement des fichiers au format .RData
  ##################################################################################################################################
  roots = c('Parametres Simulation'=ParamsimFolder)
  shinyFileSave(input, 'save_config', session=session, roots=roots, restrictions = system.file(package = "base"))
  shinyFileChoose(input, 'load_config', session=session,roots=roots, filetypes=c('', 'RData'))

  roots2 = c('Parametres Dynamique'=ParamDynFolder)
  shinyFileChoose(input, 'load_dyn', session=session,roots=roots2, filetypes=c('', 'rds'))
  
  roots3 = c('Données Simulées'=SimDataFolder)
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
    updateSelectInput(session, "indicateur", label = "Nom de l'indicateur", choices = vectorIndicator,
                      selected = NULL)
  }
  
  loadUpdatedIndicator<-function(){
    
    tryCatch({
      if(file.exists(paste0(DataFolder,"\\data\\FileIndicateur.RData")) && file.access(names = paste0(DataFolder,"\\data\\FileIndicateur.RData"), mode = 4)==0){
        load(paste0(DataFolder,"\\data\\FileIndicateur.RData"))
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
      
      updateSelectInput(session, "nom_indicateur_update", label = "indicateur", choices = vectorIndicator,
                        selected = NULL)
      rm(listeIndicateur)
    },error=function(e){
      showModal(modalDialog(
          title="Erreur",
          size = "m",
          footer = modalButton("Fermer"),
          e$message
        ) 
      )
    })
  }
  
  
  ##################################################################################################################################
  #Fonction associée à l'évènement de sélection du nom d'un indicateur dans la liste des indicateurs modifiable
  ##################################################################################################################################
  observeEvent(input$nom_indicateur_update, {
    tryCatch({
      if(file.exists(paste0(DataFolder, "\\data\\FileIndicateur.RData")) && file.access(names = paste0(DataFolder,"\\data\\FileIndicateur.RData"), mode = 4)==0){
        load(paste0(DataFolder, "\\data\\FileIndicateur.RData"))
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
          title="Erreur",
          size = "m",
          footer = modalButton("Fermer"),
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
  
  
  updateInputForVisualization <- function(ResultFCM = NULL){
    if(!is.null(ResultFCM)){
      StoreTime = ResultFCM$StoreTime
      DataType = ResultFCM$DataType
      ClassesDiam = ResultFCM$ClassesDiam
      ParamSim = ResultFCM$ParamSim
      updateSliderInput(session, "yearslider_strDiam", value = StoreTime[1], min = StoreTime[1], max = StoreTime[length(StoreTime)], step = 1)
      updateSliderInput(session, "yearrange_indicateur", value = c(StoreTime[1], StoreTime[length(StoreTime)]),
                        min = StoreTime[1], max = StoreTime[length(StoreTime)], step = 1)
      NbClasseDiam = length(ClassesDiam)
      updateNumericInput(session, "diamMin", value = ClassesDiam[1],
                         min = 0, max = ClassesDiam[NbClasseDiam], step = 1)
      updateNumericInput(session, "diamMax", value = ClassesDiam[NbClasseDiam],
                         min = 0, max = ClassesDiam[NbClasseDiam], step = 1)
      updateNumericInput(session, "classMin", value = 1,
                         min = 1, max = NbClasseDiam, step = 1)
      updateNumericInput(session, "classMax", value = NbClasseDiam,
                         min = 1, max = NbClasseDiam, step = 1)
      if(!is.null(DataType) &&  DataType == "sentier"){
        updateSelectizeInput(session, "groupe_espece_indicateur", choices = ParamSim$Essence,
                             selected = ParamSim$Essence,  server = TRUE)
        updateSelectizeInput(session, "groupe_espece_strDia", choices = ParamSim$Essence,
                             selected = ParamSim$Essence,  server = TRUE)
      }else if(!is.null(DataType)){
        updateSelectizeInput(session, "groupe_espece_indicateur", choices = ResultFCM$SpeciesTraits$Name.sp,
                             selected = NULL,  server = TRUE)
        updateSelectizeInput(session, "groupe_espece_strDia", choices = ResultFCM$SpeciesTraits$Name.sp,
                             selected = NULL,  server = TRUE)
      }
    }
    
  }
  
  
  ################################ Simulation de la dynamique forestière ##############################
  observeEvent(input$lancer_sim,{
    if(!is.null(ParamsDynFile) && file.exists(ParamsDynFile) && file.access(names = ParamsDynFile, mode = 4)==0){
      error_sim= FALSE
      inFile = parseSavePath(roots3, input$lancer_sim)
      Savepath = as.character(inFile$datapath)
      ParamsDyn = readRDS(ParamsDynFile)
      ClassesDiam = ParamsDyn$ClassesDiam
      NbClasse = length(ParamsDyn$ClassesDiam)
      simulation.input <- reactive({
        Date1PostLog.Check = 0
        ##################################################################################################################################
        #Validation des inputs saisis par l'utilisateur.
        ##################################################################################################################################
        validate(
          need(input$anneedebutSim != "", "Veuillez entrer la date de debut de la simulation. "),
          need(input$anneefirstlogging !="", "Veuillez entrer la date de la première exploitation. "),
          if(!is.null(ParamsDyn$DataType) && ParamsDyn$DataType != "sentier" && input$Allparcelle == FALSE){
            need(input$parcelle != "", "Veuillez selectionner les parcelles à simuler.")
          },
          if(!is.null(ParamsDyn$DataType) && ParamsDyn$DataType == "sentier"){
            need(input$espece != "", "Veuillez selectionner l'essence à simuler. ")
          },
          need(input$nombrerotation != "", "Veuillez entrer le nombre de rotation. "),
          need(input$dureesimulation !="", "Veuillez entrer la durée de la simulation. "),
          need(input$nbchain != "", "Veuillez entrer le nombre de simulation. "),
          need(input$dureerotation !="", "Veuillez entrer la durée d'une rotation. "),
          if(input$check == TRUE){
            need(!is.null(input$firstyearcompare), "veuillez renseigner la première année de comparaison avec les données réelles. ")
          },
          need(as.numeric(input$anneedebutSim) <= as.numeric(input$anneefirstlogging), "La première année d'exploitation doit être supérieure ou égale à l'année de debut de la simulation. "),
          need(((as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim))+ as.numeric(input$nombrerotation)*as.numeric(input$dureerotation)) <= as.numeric(input$dureesimulation), paste0("La durée de la simulation doit être supérieure à ",((as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim))+ as.numeric(input$nombrerotation)*as.numeric(input$dureerotation))))
          
        )
        ##################################################################################################################################
        #Recuperation des paramètres saisis par l'utilisateur
        ##################################################################################################################################
        tarifgenerique = input$tarifgenerique
        if(tarifgenerique ==""){
          tarifgenerique = "10^(-2.96+1.93*log10(d))"
        }
        if (!is.null(input$data_logging)) {
          
          DF  <-  hot_to_r(input$data_logging)
          DF_SAVE <- DF
          DF$tauxPrelevement = as.numeric(DF$tauxPrelevement)
          DF$coefRecollement = as.numeric(DF$coefRecollement)
          DF$dma = as.numeric(DF$dma)
          DF$tauxPrelevement[is.na(DF$tauxPrelevement) & !is.na(DF$dma)]=100
          DF$coefRecollement[is.na(DF$coefRecollement) & !is.na(DF$dma)]=100
          DF$tauxPrelevement[is.na(DF$tauxPrelevement)]=0
          DF$coefRecollement[is.na(DF$coefRecollement)]=0
          DF$dma[is.na(DF$dma)]= Inf
          DF$tauxPrelevement = DF$tauxPrelevement/100
          DF$coefRecollement = DF$coefRecollement/100
          DF$tarifcubage[DF$tarifcubage=="NA" | DF$tarifcubage==""]= tarifgenerique
          SpeciesTraits= data.frame(Name.sp = as.character(DF$nomEspece), Id.sp= as.character(DF$codeEspece), WSG = DF$densite, DME= DF$dme, DMA=DF$dma, tauxPrelevement= DF$tauxPrelevement, coefRecollement = DF$coefRecollement, tarifs = DF$tarifcubage, TarifGenerique = tarifgenerique , stringsAsFactors = FALSE) 
          
          if(!error_sim && nrow(subset(SpeciesTraits, SpeciesTraits$DMA<SpeciesTraits$DME))!=0){
            error_sim= TRUE
            error_sim_msg= "Il y a des espèces ayant un DMA inférieur au DME. Veuillez corriger ces informations."
          }
          if(!error_sim && nrow(subset(SpeciesTraits,SpeciesTraits$tauxPrelevement<0|SpeciesTraits$tauxPrelevement>1))!=0){
            error_sim= TRUE
            error_sim_msg= "Un taux de prélèvement doit être compris entre 0 et 100. Veuillez corriger ces informations."
          }
          if(!error_sim && nrow(subset(SpeciesTraits,SpeciesTraits$tauxRecolement<0|SpeciesTraits$tauxRecolement>1))!=0){
            error_sim= TRUE
            error_sim_msg= "Un taux de recolement doit être compris entre 0 et 1. Veuillez corriger ces informations."
          }
        }else{
          DF_SAVE <- NULL
          if(!is.null(ParamsDynFile) && file.exists(ParamsDynFile) && file.access(names = ParamsDynFile, mode = 4)==0){
            ParamsDyn = readRDS(ParamsDynFile)
            if(!is.null(ParamsDyn)){
              SpeciesTraits = ParamsDyn$SpeciesTraits
              DF <- data.frame(nomEspece = SpeciesTraits$Name.sp, codeEspece= SpeciesTraits$Id.sp, dme=SpeciesTraits$DME, dma=rep(Inf, length(SpeciesTraits$Id.sp)), tauxPrelevement=rep(0, length(SpeciesTraits$Id.sp)), coefRecollement=rep(0, length(SpeciesTraits$Id.sp)), tarifcubage=rep(tarifgenerique, length(SpeciesTraits$Id.sp)), densite=SpeciesTraits$WSG)
              SpeciesTraits= data.frame(Name.sp = as.character(DF$nomEspece), Id.sp= as.character(DF$codeEspece), WSG = DF$densite, DME= DF$dme, DMA=DF$dma, tauxPrelevement= DF$tauxPrelevement, coefRecollement = DF$coefRecollement, tarifs = DF$tarifcubage, TarifGenerique = tarifgenerique , stringsAsFactors = FALSE)
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
        }else{
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
        }else{
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
        }else{
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
            if(input$Allparcelle == TRUE){
              ParamsDyn = readRDS(ParamsDynFile)
              if(!is.null(ParamsDyn)){
                Starting.plot= as.numeric(ParamsDyn$Descriptif$Parcelles)
              }
            }else{
              Starting.plot= as.numeric(input$parcelle)
            }
          
          StartingDate = as.numeric(input$anneedebutSim)
          Essence = as.character(input$espece)
          RecruitmentRate = as.numeric(input$tauxrecrutement)
          Nb.rotation = as.numeric(input$nombrerotation)
          rotation = as.numeric(input$dureerotation)
          DelayLogging =as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim)
          Fin.simu = as.numeric(input$dureesimulation)
          Date1PostLog.Check = input$firstyearcompare
          Check = input$check
          if(is.na(Check) || !is.logical(Check)) Check = FALSE
          #MySpeciesTraits = SpeciesTraits
          vector_damage = v/100
          ClasseDiamWeights = ClasseDiamWeights/100
          
          Logging.intensity = rep(0, NbClasse)
          if(!is.null(ParamsDyn$DataType) && ParamsDyn$DataType == "sentier"){
            DMA = SpeciesTraits$DMA[SpeciesTraits$Id.sp == Essence]
            Tauxprelevement = SpeciesTraits$tauxPrelevement[SpeciesTraits$Id.sp== Essence]
            Logging.intensity[DMA <= ClassesDiam] = Tauxprelevement
            if(!DMA %in% ClassesDiam){
              Id_cl = sum(DMA>ClassesDiam)
              Logging.intensity[Id_cl] = Tauxprelevement*((ClassesDiam[Id_cl+1] - DMA)/(ClassesDiam[Id_cl+1] - ClassesDiam[Id_cl]))
            }
          }
          
          Logging="T2.MBaiki"
          ##################################################################################################################################
          #Calcul des paramètres du modèle en utilisant les données réelles collectées sur les arbres et 
          #contenu dans la liste d'objets ParamsDyn contenant les paramètres de la dynamique
          ##################################################################################################################################
          ParamSim = list(Nb.rotation = Nb.rotation, rotation = rotation, Fin.simu = Fin.simu, DelayLogging = DelayLogging, nbchain = nbchain, vector_damage = vector_damage, RecruitmentRate = RecruitmentRate, Starting.plot = Starting.plot, Essence=Essence, Effectifs=Effectifs, ClasseDiamWeights= ClasseDiamWeights, Logging.intensity = Logging.intensity, StartingDate = StartingDate, nbchain = nbchain, Check = Check, Date1PostLog.Check = Date1PostLog.Check, MySpeciesTraits = SpeciesTraits, Logging = Logging, Tarifgenerique = tarifgenerique)
          ##################################################################################################################################
          #Simulation proprement dite
          ##################################################################################################################################
          ResultFCM= FCM(out.InferFCM=ParamsDyn, ParamSim)
          ##################################################################################################################################
          #Sauvegarde des resultats de la simulation dans le fichier précisé par l'utilisateur et d'autres paramètres
          #utilisable ultérieurement.
          ##################################################################################################################################
          Nb.period=ResultFCM$ParamPlot$Nb.period
          max_Temps = max(ResultFCM$Simulations$Temps)+StartingDate
          StoreTime= c(StartingDate:max_Temps);
          ResultFCM$StoreTime = StoreTime
          ResultFCM$ParamSim = ParamSim
          ResultFCM$DataType = ParamsDyn$DataType
          ResultFCM$ClassesDiam = ParamsDyn$ClassesDiam
          ResultFCM$ParamsDynFile = ParamsDynFile
          ResultFCM$DateSim = Sys.Date()
          updateInputForVisualization(ResultFCM)
          save(ResultFCM, file = Savepath)
          CurrentSimDataFile <<- Savepath
          updateInfosDescriptionSim(ResultFCM)
          tryCatch({
            load(paste0(DataFolder, "\\data\\FileIndicateur.RData"))
          },error=function(e){
            listeIndicateur= list()
          })
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
            title="Erreur",
            size = "m",
            footer = modalButton("Fermer"),
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
          title="Erreur",
          size = "m",
          footer = modalButton("Fermer"),
          e$message
        ) 
        )
      })
    }else{
      showModal(modalDialog(
        title="Erreur",
        size = "m",
        footer = modalButton("Fermer"),
        "Veuillez charger les paramètres de la dynamique."
      ) 
      )
    }
  })
  
  observeEvent(input$lancer_sim_without_save_file,{
    if(!is.null(ParamsDynFile) && file.exists(ParamsDynFile) && file.access(names = ParamsDynFile, mode = 4)==0){
      error_sim= FALSE
      ParamsDyn = readRDS(ParamsDynFile)
      Savepath = paste(SimDataFolder, paste("DefaultSimFile", ParamsDyn$DataType,'RData', sep = '.'), sep = '/')
      ClassesDiam = ParamsDyn$ClassesDiam
      NbClasse = length(ParamsDyn$ClassesDiam)
      simulation.input <- reactive({
        Date1PostLog.Check = 0
        ##################################################################################################################################
        #Validation des inputs saisis par l'utilisateur.
        ##################################################################################################################################
        validate(
          need(input$anneedebutSim != "", "Veuillez entrer la date de debut de la simulation. "),
          need(input$anneefirstlogging !="", "Veuillez entrer la date de la première exploitation. "),
          if(!is.null(ParamsDyn$DataType) && ParamsDyn$DataType != "sentier" && input$Allparcelle == FALSE){
            need(input$parcelle != "", "Veuillez selectionner les parcelles à simuler.")
          },
          if(!is.null(ParamsDyn$DataType) && ParamsDyn$DataType == "sentier"){
            need(input$espece != "", "Veuillez selectionner l'essence à simuler. ")
          },
          need(input$nombrerotation != "", "Veuillez entrer le nombre de rotation. "),
          need(input$dureesimulation !="", "Veuillez entrer la durée de la simulation. "),
          need(input$nbchain != "", "Veuillez entrer le nombre de simulation. "),
          need(input$dureerotation !="", "Veuillez entrer la durée d'une rotation. "),
          if(input$check == TRUE){
            need(!is.null(input$firstyearcompare), "veuillez renseigner la première année de comparaison avec les données réelles. ")
          },
          need(as.numeric(input$anneedebutSim) <= as.numeric(input$anneefirstlogging), "La première année d'exploitation doit être supérieure ou égale à l'année de debut de la simulation. "),
          need(((as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim))+ as.numeric(input$nombrerotation)*as.numeric(input$dureerotation)) <= as.numeric(input$dureesimulation), paste0("La durée de la simulation doit être supérieure à ",((as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim))+ as.numeric(input$nombrerotation)*as.numeric(input$dureerotation))))
          
        )
        ##################################################################################################################################
        #Recuperation des paramètres saisis par l'utilisateur
        ##################################################################################################################################
        tarifgenerique = input$tarifgenerique
        if(tarifgenerique ==""){
          tarifgenerique = "10^(-2.96+1.93*log10(d))"
        }
        if (!is.null(input$data_logging)) {
          
          DF  <-  hot_to_r(input$data_logging)
          DF_SAVE <- DF
          DF$tauxPrelevement = as.numeric(DF$tauxPrelevement)
          DF$coefRecollement = as.numeric(DF$coefRecollement)
          DF$dma = as.numeric(DF$dma)
          DF$tauxPrelevement[is.na(DF$tauxPrelevement) & !is.na(DF$dma)]=100
          DF$coefRecollement[is.na(DF$coefRecollement) & !is.na(DF$dma)]=100
          DF$tauxPrelevement[is.na(DF$tauxPrelevement)]=0
          DF$coefRecollement[is.na(DF$coefRecollement)]=0
          DF$dma[is.na(DF$dma)]= Inf
          DF$tauxPrelevement = DF$tauxPrelevement/100
          DF$coefRecollement = DF$coefRecollement/100
          DF$tarifcubage[DF$tarifcubage=="NA" | DF$tarifcubage==""]= tarifgenerique
          SpeciesTraits= data.frame(Name.sp = as.character(DF$nomEspece), Id.sp= as.character(DF$codeEspece), WSG = DF$densite, DME= DF$dme, DMA=DF$dma, tauxPrelevement= DF$tauxPrelevement, coefRecollement = DF$coefRecollement, tarifs = DF$tarifcubage, TarifGenerique = tarifgenerique , stringsAsFactors = FALSE) 
          
          if(!error_sim && nrow(subset(SpeciesTraits, SpeciesTraits$DMA<SpeciesTraits$DME))!=0){
            error_sim= TRUE
            error_sim_msg= "Il y a des espèces ayant un DMA inférieur au DME. Veuillez corriger ces informations."
          }
          if(!error_sim && nrow(subset(SpeciesTraits,SpeciesTraits$tauxPrelevement<0|SpeciesTraits$tauxPrelevement>1))!=0){
            error_sim= TRUE
            error_sim_msg= "Un taux de prélèvement doit être compris entre 0 et 100. Veuillez corriger ces informations."
          }
          if(!error_sim && nrow(subset(SpeciesTraits,SpeciesTraits$tauxRecolement<0|SpeciesTraits$tauxRecolement>1))!=0){
            error_sim= TRUE
            error_sim_msg= "Un taux de recolement doit être compris entre 0 et 1. Veuillez corriger ces informations."
          }
        }else{
          DF_SAVE <- NULL
          if(!is.null(ParamsDynFile) && file.exists(ParamsDynFile) && file.access(names = ParamsDynFile, mode = 4)==0){
            ParamsDyn = readRDS(ParamsDynFile)
            if(!is.null(ParamsDyn)){
              SpeciesTraits = ParamsDyn$SpeciesTraits
              DF <- data.frame(nomEspece = SpeciesTraits$Name.sp, codeEspece= SpeciesTraits$Id.sp, dme=SpeciesTraits$DME, dma=rep(Inf, length(SpeciesTraits$Id.sp)), tauxPrelevement=rep(0, length(SpeciesTraits$Id.sp)), coefRecollement=rep(0, length(SpeciesTraits$Id.sp)), tarifcubage=rep(tarifgenerique, length(SpeciesTraits$Id.sp)), densite=SpeciesTraits$WSG)
              SpeciesTraits= data.frame(Name.sp = as.character(DF$nomEspece), Id.sp= as.character(DF$codeEspece), WSG = DF$densite, DME= DF$dme, DMA=DF$dma, tauxPrelevement= DF$tauxPrelevement, coefRecollement = DF$coefRecollement, tarifs = DF$tarifcubage, TarifGenerique = tarifgenerique , stringsAsFactors = FALSE)
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
        }else{
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
        }else{
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
        }else{
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
          shinyjs::hide('action_save_sim_file')
          shinyjs::hide('actions_sim')
          assign("hidable", TRUE, envir = .GlobalEnv)
          print(hidable)
          nbchain = input$nbchain
          if(input$Allparcelle == TRUE){
            ParamsDyn = readRDS(ParamsDynFile)
            if(!is.null(ParamsDyn)){
              Starting.plot= as.numeric(ParamsDyn$Descriptif$Parcelles)
            }
          }else{
            Starting.plot= as.numeric(input$parcelle)
          }
          
          StartingDate = as.numeric(input$anneedebutSim)
          Essence = as.character(input$espece)
          RecruitmentRate = as.numeric(input$tauxrecrutement)
          Nb.rotation = as.numeric(input$nombrerotation)
          rotation = as.numeric(input$dureerotation)
          DelayLogging =as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim)
          Fin.simu = as.numeric(input$dureesimulation)
          Date1PostLog.Check = input$firstyearcompare
          Check = input$check
          if(is.na(Check) || !is.logical(Check)) Check = FALSE
          #MySpeciesTraits = SpeciesTraits
          vector_damage = v/100
          ClasseDiamWeights = ClasseDiamWeights/100
          
          Logging.intensity = rep(0, NbClasse)
          if(!is.null(ParamsDyn$DataType) && ParamsDyn$DataType == "sentier"){
            DMA = SpeciesTraits$DMA[SpeciesTraits$Id.sp == Essence]
            Tauxprelevement = SpeciesTraits$tauxPrelevement[SpeciesTraits$Id.sp== Essence]
            Logging.intensity[DMA <= ClassesDiam] = Tauxprelevement
            if(!DMA %in% ClassesDiam){
              Id_cl = sum(DMA>ClassesDiam)
              Logging.intensity[Id_cl] = Tauxprelevement*((ClassesDiam[Id_cl+1] - DMA)/(ClassesDiam[Id_cl+1] - ClassesDiam[Id_cl]))
            }
          }
          
          Logging="T2.MBaiki"
          ##################################################################################################################################
          #Calcul des paramètres du modèle en utilisant les données réelles collectées sur les arbres et 
          #contenu dans la liste d'objets ParamsDyn contenant les paramètres de la dynamique
          ##################################################################################################################################
          ParamSim = list(Nb.rotation = Nb.rotation, rotation = rotation, Fin.simu = Fin.simu, DelayLogging = DelayLogging, nbchain = nbchain, vector_damage = vector_damage, RecruitmentRate = RecruitmentRate, Starting.plot = Starting.plot, Essence=Essence, Effectifs=Effectifs, ClasseDiamWeights= ClasseDiamWeights, Logging.intensity = Logging.intensity, StartingDate = StartingDate, nbchain = nbchain, Check = Check, Date1PostLog.Check = Date1PostLog.Check, MySpeciesTraits = SpeciesTraits, Logging = Logging, Tarifgenerique = tarifgenerique)
          ##################################################################################################################################
          #Simulation proprement dite
          ##################################################################################################################################
          ResultFCM= FCM(out.InferFCM=ParamsDyn, ParamSim)
          ##################################################################################################################################
          #Sauvegarde des resultats de la simulation dans le fichier précisé par l'utilisateur et d'autres paramètres
          #utilisable ultérieurement.
          ##################################################################################################################################
          Nb.period=ResultFCM$ParamPlot$Nb.period
          max_Temps = max(ResultFCM$Simulations$Temps)+StartingDate
          StoreTime= c(StartingDate:max_Temps);
          ResultFCM$StoreTime = StoreTime
          ResultFCM$ParamSim = ParamSim
          ResultFCM$DataType = ParamsDyn$DataType
          ResultFCM$ClassesDiam = ParamsDyn$ClassesDiam
          ResultFCM$ParamsDynFile = ParamsDynFile
          ResultFCM$DateSim = Sys.Date()
          updateInputForVisualization(ResultFCM)
          save(ResultFCM, file = Savepath)
          CurrentSimDataFile <<- Savepath
          updateInfosDescriptionSim(ResultFCM)
          tryCatch({
            load(paste0(DataFolder, "\\data\\FileIndicateur.RData"))
          },error=function(e){
            listeIndicateur= list()
          })
          updateSimulatingDataOfIndicator(listeIndicateur)
          shinyjs::hide('loader_sim')
          shinyjs::hide('sim_encours')
          shinyjs::show('image_success')
          shinyjs::show('sim_finish')
          shinyjs::hide('action_save_sim_file')
          shinyjs::show('actions_sim')
          shinyjs::hide('lancer_sim_col')
          shinyjs::show('gotoparameters_col')
        }else{
          showModal(modalDialog(
            title="Erreur",
            size = "m",
            footer = modalButton("Fermer"),
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
          shinyjs::hide('action_save_sim_file')
          shinyjs::show('actions_sim')
          shinyjs::hide('lancer_sim_col')
          shinyjs::show('gotoparameters_col')
        }
        showModal(modalDialog(
          title="Erreur",
          size = "m",
          footer = modalButton("Fermer"),
          e$message
        ) 
        )
      })
    }else{
      showModal(modalDialog(
        title="Erreur",
        size = "m",
        footer = modalButton("Fermer"),
        "Veuillez charger les paramètres de la dynamique."
      ) 
      )
    }
  })
  
  ################################ Sauvegarde des paramètres de la dynamique forestière ##############################
  observeEvent(input$save_config,{
    if(!is.null(ParamsDynFile) && file.exists(ParamsDynFile) && file.access(names = ParamsDynFile, mode = 4)==0){
      tryCatch({
        shinyjs::hide('lancer_sim_col')
        shinyjs::show('boxloader_FileConfig')
        inFile = parseSavePath(roots, input$save_config)
        Savepath = as.character(inFile$datapath)
        error_sim = FALSE
        ParamsDyn = readRDS(ParamsDynFile)
        ClassesDiam = ParamsDyn$ClassesDiam
        NbClasse = length(ParamsDyn$ClassesDiam)
        tarifgenerique = input$tarifgenerique
        if(tarifgenerique ==""){
          tarifgenerique = "10^(-2.96+1.93*log10(d))"
        }
        if (!is.null(input$data_logging)) {
          DF  <-  hot_to_r(input$data_logging)
          DF_SAVE <- DF
          DF$tauxPrelevement = as.numeric(DF$tauxPrelevement)
          DF$coefRecollement = as.numeric(DF$coefRecollement)
          DF$dma = as.numeric(DF$dma)
          DF$tauxPrelevement[is.na(DF$tauxPrelevement) & !is.na(DF$dma)]=100
          DF$coefRecollement[is.na(DF$coefRecollement) & !is.na(DF$dma)]=100
          DF$tauxPrelevement[is.na(DF$tauxPrelevement)]=0
          DF$coefRecollement[is.na(DF$coefRecollement)]=0
          DF$dma[is.na(DF$dma)]= Inf
          DF$tauxPrelevement = DF$tauxPrelevement/100
          DF$coefRecollement = DF$coefRecollement/100
          DF$tarifcubage[DF$tarifcubage=="NA" | DF$tarifcubage==""]= "10^(-2.96+1.93*log10(d))"
          SpeciesTraits= data.frame(Name.sp= as.character(DF$nomEspece),Id.sp= as.character(DF$codeEspece), WSG = DF$densite, DME= DF$dme, DMA=DF$dma, tauxPrelevement= DF$tauxPrelevement, coefRecollement = DF$coefRecollement, tarifs = DF$tarifcubage) 
          if(!error_sim && nrow(subset(SpeciesTraits, SpeciesTraits$DMA<SpeciesTraits$DME))!=0){
            error_sim= TRUE
            error_sim_msg= "Il y a des espèces ayant un DMA inférieur au DME. Veuillez corriger ces informations."
          }
          if(!error_sim && nrow(subset(SpeciesTraits,SpeciesTraits$tauxPrelevement<0|SpeciesTraits$tauxPrelevement>1))!=0){
            error_sim= TRUE
            error_sim_msg= "Un taux de prélèvement doit être compris entre 0 et 100. Veuillez corriger ces informations."
          }
          if(!error_sim && nrow(subset(SpeciesTraits,SpeciesTraits$tauxRecolement<0|SpeciesTraits$tauxRecolement>1))!=0){
            error_sim= TRUE
            error_sim_msg= "Un taux de recolement doit être compris entre 0 et 100. Veuillez corriger ces informations."
          }
        }else{
          DF_SAVE <- NULL
          if(!is.null(ParamsDyn)){
            SpeciesTraits = ParamsDyn$SpeciesTraits
            DF <- data.frame(nomEspece= SpeciesTraits$Name.sp, codeEspece= SpeciesTraits$Id.sp, dme=SpeciesTraits$DME, dma=rep(Inf, length(SpeciesTraits$Id.sp)), tauxPrelevement=rep(0, length(SpeciesTraits$Id.sp)), coefRecollement=rep(0, length(SpeciesTraits$Id.sp)), tarifcubage=rep("10^(-2.96+1.93*log10(d))", length(SpeciesTraits$Id.sp)), densite=SpeciesTraits$WSG)
            SpeciesTraits= data.frame(Name.sp = as.character(DF$nomEspece), Id.sp= DF$codeEspece, WSG = DF$densite, DME= DF$dme, DMA=DF$dma, tauxPrelevement= DF$tauxPrelevement, coefRecollement = DF$coefRecollement)
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
        }else{
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
        }else{
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
        }else{
          VTR  <-  NULL
          VTR_SAVE <- VTR
          ClasseDiamWeights <-  rep(0, NbClasse)
        }
        if(!error_sim){ 
          nbchain = input$nbchain
          StartingDate = as.numeric(input$anneedebutSim)
          Nb.rotation = as.numeric(input$nombrerotation)
          rotation = as.numeric(input$dureerotation)
          DelayLogging =as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim)
          Fin.simu = as.numeric(input$dureesimulation)
          Date1PostLog.Check = input$firstyearcompare
          Check.Allparcelle = input$Allparcelle
          Starting.plot= as.numeric(input$parcelle)
          Essence = as.character(input$espece)
          RecruitementRate = as.numeric(input$tauxrecrutement)
          Check = input$check
          StartingLogging = as.numeric(input$anneefirstlogging)
          ParamSim = list(StartingLogging = StartingLogging, Essence=Essence, RecruitementRate = RecruitementRate, Check.Allparcelle = Check.Allparcelle, Fin.simu = Fin.simu, Nb.rotation = Nb.rotation, rotation = rotation, DelayLogging = DelayLogging, nbchain = nbchain, vector_damage = VD_SAVE, Starting.plot = Starting.plot, Essence = Essence,  StartingDate = StartingDate, nbchain = nbchain, Check = Check, Date1PostLog.Check = Date1PostLog.Check, MySpeciesTraits = DF_SAVE , Tarifgenerique = tarifgenerique)
          ParamSim$vector_damage = VD_SAVE
          ParamSim$MySpeciesTraits = DF_SAVE
          ParamSim$Effectifs = VNUM_SAVE
          ParamSim$ClasseDiamWeights = VTR_SAVE
          ParamSim$ParamsDynFile = ParamsDynFile
          save(ParamSim, file = Savepath)
          shinyjs::hide('boxloader_FileConfig')
          shinyjs::show('lancer_sim_col')
          showNotification("Scénario sauvegardé avec succès.", duration = 10, type = "message")
        }else{
          shinyjs::hide('boxloader_FileConfig')
          shinyjs::show('lancer_sim_col')
          showModal(modalDialog(
            title="Erreur",
            size = "m",
            footer = modalButton("Fermer"),
            error_sim_msg
          ) 
          )
        }
      },error=function(e){
        shinyjs::hide('boxloader_FileConfig')
        shinyjs::show('lancer_sim_col')
        showModal(modalDialog(
          title="Erreur",
          size = "m",
          footer = modalButton("Fermer"),
          e$message
        ) 
        )
      })
    }else{
      showModal(modalDialog(
        title="Erreur",
        size = "m",
        footer = modalButton("Fermer"),
        "Veuillez charger les paramètres de la dynamique."
      ) 
      )
    }
    
  })
  
  ############ Charger un fichier de données simulées #########################
  observeEvent(input$load_sim_data,{
    tryCatch({
      shinyjs::hide('load_sim_data')
      shinyjs::show('boxloader_SimData')
      inFile = parseFilePaths(roots3, input$load_sim_data)
      Loadpath = as.character(inFile$datapath)
      load(Loadpath)
      updateInputForVisualization(ResultFCM)
      CurrentSimDataFile <<- Loadpath
      tryCatch({
        load(paste0(DataFolder, "\\data\\FileIndicateur.RData"))
      },error=function(e){
        listeIndicateur= list()
      })
      updateSimulatingDataOfIndicator(listeIndicateur)
      shinyjs::show('load_sim_data')
      shinyjs::hide('boxloader_SimData')
      updateInfosDescriptionSim(ResultFCM)
      showNotification("Données chargées avec succès.", duration = 10, type = "message")
    
    },error=function(e){
      shinyjs::show('load_sim_data')
      shinyjs::hide('boxloader_SimData')
      showModal(modalDialog(
        title="Erreur",
        size = "m",
        footer = modalButton("Fermer"),
        e$message
      ) 
      )
    })
  })
  ############ Charger le dernier paramètrage (Processus permettant de charger un scénario de simulation existant) #########################
  observeEvent(input$load_config,{
      tryCatch({
        shinyjs::hide('lancer_sim_col')
        shinyjs::show('boxloader_FileConfig')
        inFile = parseFilePaths(roots, input$load_config)
        Loadpath = as.character(inFile$datapath)
        load(Loadpath)
        SavedParamsDynFile = ParamSim$ParamsDynFile
        if(!is.null(SavedParamsDynFile) && file.exists(SavedParamsDynFile) && file.access(names = SavedParamsDynFile, mode = 4)==0){
          ParamsDynFile <<- SavedParamsDynFile
          ParamsDyn = readRDS(ParamsDynFile)
          updateInfosSite(ParamsDyn$DataType, ParamsDyn$Descriptif$site, ParamsDyn$Descriptif$pays)
          MySpeciesTraits = ParamSim$MySpeciesTraits
          NumberSpecies = length(MySpeciesTraits$codeEspece)
          if(NumberSpecies == 1){
            height = 50
          }else if(NumberSpecies> 1 && NumberSpecies<= 7){
            height =185
          }else if(NumberSpecies> 7 && NumberSpecies<= 10){
            height = 220
          }else{
            height = 300
          }
          browser();
          if(!is.null(MySpeciesTraits)){
            output$data_logging <-rhandsontable::renderRHandsontable({
              rhandsontable::rhandsontable(MySpeciesTraits, colHeaders = c("Nom espèce", "Code espèce", "D.M.E", "D.M.A", "Taux de prélèvement", "Coefficient de recolement", "Tarif de cubage", "Densité"), selectCallback = TRUE, useTypes = TRUE, width = "100%", height = height) %>%
                hot_cols(columnSorting = TRUE, colWidths= c(150, 100, 75, 75, 150, 175, 200, 75), manualColumnResize=TRUE, fixedColumnsLeft=1) #%>%
                #hot_cols(columnSorting = TRUE, fixedColumnsLeft=1) %>%
                #hot_col(col = "Nom", type = "autocomplete", source =MySpeciesTraits$Nom)
            })
          }
          Vector_damage = ParamSim$vector_damage
          if(!is.null(Vector_damage)){
              if(!is.null(ParamsDyn)){
                output$vector_damage <-rhandsontable::renderRHandsontable({
                  ClassesDiam = ParamsDyn$Descriptif$ClassesDiam
                  colwidths =1000/length(ClassesDiam) 
                  StrEval2="ColHeaders = c("
                  StrEval3="colWidths = c("
                  for(iter in 1:(length(ClassesDiam)-1)){
                    StrEval2 = paste0(StrEval2, '"[', ClassesDiam[iter],', ', ClassesDiam[iter+1], '[", ')
                    StrEval3 = paste0(StrEval3, paste0(colwidths,', '))
                  }
                  StrEval2 = paste0(StrEval2, '"[', ClassesDiam[length(ClassesDiam)],', Inf[")')
                  eval(parse(text = StrEval2))
                  StrEval3 = paste0(StrEval3, paste0(colwidths,')'))
                  eval(parse(text = StrEval3))
                  rhandsontable::rhandsontable(Vector_damage, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "100%") %>%
                    hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
                    hot_cols(columnSorting = TRUE, colWidths= colWidths, manualColumnResize=TRUE, fixedColumnsLeft=1)
                   
                })
              }
          }
          Effectifs = ParamSim$Effectifs
          if(!is.null(Effectifs)){
            if(!is.null(ParamsDyn)){
              output$vector_number_per_diameter_class <-rhandsontable::renderRHandsontable({
                ClassesDiam = ParamsDyn$Descriptif$ClassesDiam
                colwidths =460/length(ClassesDiam) 
                StrEval2="ColHeaders = c("
                StrEval3="colWidths = c("
                for(iter in 1:(length(ClassesDiam)-1)){
                  StrEval2 = paste0(StrEval2, '"[', ClassesDiam[iter],', ', ClassesDiam[iter+1], '[", ')
                  StrEval3 = paste0(StrEval3, paste0(colwidths,', '))
                }
                StrEval2 = paste0(StrEval2, '"[', ClassesDiam[length(ClassesDiam)],', Inf[")')
                eval(parse(text = StrEval2))
                StrEval3 = paste0(StrEval3, paste0(colwidths,')'))
                eval(parse(text = StrEval3))
                rhandsontable::rhandsontable(Effectifs, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "100%") %>%
                  hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
                  hot_cols(columnSorting = TRUE, colWidths= colWidths, manualColumnResize=TRUE, fixedColumnsLeft=1)
                
              })
            }
          }
          
          ClasseDiamWeights = ParamSim$ClasseDiamWeights
          if(!is.null(ClasseDiamWeights)){
            if(!is.null(ParamsDyn)){
              output$vector_tauxrecrutement_per_diameter_class <-rhandsontable::renderRHandsontable({
                ClassesDiam = ParamsDyn$Descriptif$ClassesDiam
                colwidths =460/length(ClassesDiam) 
                StrEval2="ColHeaders = c("
                StrEval3="colWidths = c("
                for(iter in 1:(length(ClassesDiam)-1)){
                  StrEval2 = paste0(StrEval2, '"[', ClassesDiam[iter],', ', ClassesDiam[iter+1], '[", ')
                  StrEval3 = paste0(StrEval3, paste0(colwidths,', '))
                }
                StrEval2 = paste0(StrEval2, '"[', ClassesDiam[length(ClassesDiam)],', Inf[")')
                eval(parse(text = StrEval2))
                StrEval3 = paste0(StrEval3, paste0(colwidths,')'))
                eval(parse(text = StrEval3))
                rhandsontable::rhandsontable(ClasseDiamWeights, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "100%") %>%
                  hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
                  hot_cols(columnSorting = TRUE, colWidths= colWidths, manualColumnResize=TRUE, fixedColumnsLeft=1)
                
              })
            }
          }
          
          updateNumericInput(session,"anneedebutSim", value=ParamSim$StartingDate)
          updateNumericInput(session,"anneefirstlogging", value=ParamSim$StartingLogging)
          updateTextInput(session,"dureesimulation", value=ParamSim$Fin.simu)
          updateNumericInput(session,"dureerotation", value=ParamSim$rotation)
          updateNumericInput(session,"nombrerotation", value=ParamSim$Nb.rotation)
          updateNumericInput(session,"nbchain", value=ParamSim$nbchain)
          updateNumericInput(session,"firstyearcompare", value=ParamSim$Date1PostLog.Check)
          updateCheckboxInput(session,"check", value=ParamSim$Check)
          updateTextInput(session, "tarifgenerique", value = ParamSim$Tarifgenerique)
          
          if(!is.null(ParamsDyn$DataType) && ParamsDyn$DataType == "parcelle"){
            updateCheckboxInput(session,"Allparcelle", value=ParamSim$Check.Allparcelle)
            if(!ParamSim$Check.Allparcelle){
              Parcelles = ParamsDyn$Descriptif$Parcelles
              updateSelectizeInput(session, "parcelle", choices = Parcelles, selected = ParamSim$Starting.plot, server = TRUE)
            }
            shinyjs::hide('bloc_params_logging_sentier')
            shinyjs::hide('bloc_espece_sentier')
            shinyjs::hide('bloc_recrutement_sentier')
            shinyjs::hide('bloc_tauxrecrutement_sentiers_sim')
            shinyjs::show('bloc_params_logging_parcelle')
            shinyjs::show('bloc_list_parcelles_sim')
          }else if(!is.null(ParamsDyn$DataType) && ParamsDyn$DataType == "sentier"){
            updateNumericInput(session,"tauxrecrutement", value=ParamSim$RecruitementRate)
            Essences = levels(ParamsDyn$SpeciesTraits$Id.sp)
            updateSelectizeInput(session, "espece", choices = Essences, selected = ParamSim$Essence, server = TRUE)
            updateSelectizeInput(session, "groupe_espece_indicateur", choices = ParamSim$Essence,
                                 selected = ParamSim$Essence,  server = TRUE)
            updateSelectizeInput(session, "groupe_espece_strDia", choices = ParamSim$Essence,
                                 selected = ParamSim$Essence,  server = TRUE)
            shinyjs::hide('bloc_params_logging_parcelle')
            shinyjs::hide('bloc_list_parcelles_sim')
            shinyjs::show('bloc_params_logging_sentier')
            shinyjs::show('bloc_espece_sentier')
            shinyjs::show('bloc_recrutement_sentier')
            shinyjs::show('bloc_tauxrecrutement_sentiers_sim')
          }
          shinyjs::hide('boxloader_FileConfig')
          shinyjs::show('lancer_sim_col')
          showNotification("Scénario chargé avec succès.", duration = 10, type = "message")
        }else{
          shinyjs::hide('boxloader_FileConfig')
          shinyjs::show('lancer_sim_col')
          showModal(modalDialog(
            title="Erreur",
            size = "m",
            footer = modalButton("Fermer"),
            "Veuillez charger les paramètres de la dynamique"
          ) 
          )
        }
      },error=function(e){
        shinyjs::hide('boxloader_FileConfig')
        shinyjs::show('lancer_sim_col')
        showModal(modalDialog(
          title="Erreur",
          size = "m",
          footer = modalButton("Fermer"),
          e$message
        ) 
        )
      })
  })
  
  
  ############ This function update the values of indicators to add a values of a new indicateur in a current simulation Data file #########################
  updateSimulatingDataOfIndicator<- function (listeIndicateur = NULL){
    if(file.exists(CurrentSimDataFile) && file.access(names = CurrentSimDataFile, mode = 4)==0){
      load(CurrentSimDataFile)
      MyParamsDyn = readRDS(ResultFCM$ParamsDynFile)
      if(tolower(MyParamsDyn$DataType)=="parcelle"){
        alphaInd = MyParamsDyn$ParamPlot$alpha
      }else if(tolower(MyParamsDyn$DataType)=="sentier"){
        alphaInd = NULL
      }else{
        alphaInd = MyParamsDyn$ParamPlot$alpha
      }
      browser();
      ResultFCM$ParamPlot$CDSTB=ClasseDiamSTAGB(ParamsDyn = MyParamsDyn, ParamSim =ResultFCM$ParamSim, alpha=alphaInd, OtherIndicator = listeIndicateur)
      save(ResultFCM,file=CurrentSimDataFile)
    }
  }
  
  
  ############ Ajout d'un indicateur #########################
  observeEvent(input$ajout_indicateur_btn,{
    tryCatch({
      validate(
        need(input$nom_indicateur_new != "", "Veuillez entrer le nom de l'indicateur. "),
        need(input$fonction_indicateur_new !="", "Veuillez entrer la formulation mathématique de l'indicateur. ")
      )
      
      shinyjs::hide('action_add_IND')
      shinyjs::show('boxloader_add_IND')
      
      tryCatch({
        load(paste0(DataFolder, "\\data\\FileIndicateur.RData"))
      },error=function(e){
        listeIndicateur= list()
      })
      
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
        save(listeIndicateur,file=paste0(DataFolder, "\\data\\FileIndicateur.RData"))
        ######Mise à jour de la liste de indicateurs sur l'interface utilisateur de visualisation des indicateurs###########
        updateSelectIndicator()
        shinyjs::hide('boxloader_add_IND')
        shinyjs::show('action_add_IND')
        showNotification("Indicateur ajouté avec succès.", duration = 10, type = "message")
      }else{
        shinyjs::hide('boxloader_add_IND')
        shinyjs::show('action_add_IND')
        showModal(modalDialog(
          title="Erreur",
          size = "m",
          footer = modalButton("Fermer"),
          "Un indicateur possède déjà une des informations de cet indicateur."
        ) 
        )
      }
    },error=function(e){
      shinyjs::hide('boxloader_add_IND')
      shinyjs::show('action_add_IND')
      showModal(modalDialog(
        title="Erreur",
        size = "m",
        footer = modalButton("Fermer"),
        e$message
      ) 
      )
    })
  })
  #########################Modification des indicateurs####################################
  observeEvent(input$update_indicateur_btn,{
    tryCatch({
      validate(
        need(input$nom_indicateur_update != "", "Veuillez selectionner un indicateur. "),
        need(input$fonction_indicateur_update !="", "Veuillez entrer la formulation mathématique de l'indicateur. ")
      )
      
      shinyjs::hide('action_update_delete_IND')
      shinyjs::show('boxloader_update_IND')
      if(file.exists(paste0(DataFolder, "\\data\\FileIndicateur.RData")) && file.access(names = paste0(DataFolder, "\\data\\FileIndicateur.RData"), mode = 4)==0){
        load(paste0(DataFolder, "\\data\\FileIndicateur.RData"))
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
        save(listeIndicateur,file=paste0(DataFolder, "\\data\\FileIndicateur.RData"))
        ######Mise à jour de la liste de indicateurs sur l'interface utilisateur pour la modification des indicateurs###########
        loadUpdatedIndicator()
        ######Mise à jour de la liste de indicateurs sur l'interface utilisateur pour la visualisation des indicateurs###########
        updateSelectIndicator()
        shinyjs::hide('boxloader_update_IND')
        shinyjs::show('action_update_delete_IND')
        showNotification("Indicateur mis à jour avec succès.", duration = 10, type = "message")
      }else{
        shinyjs::hide('boxloader_update_IND')
        shinyjs::show('action_update_delete_IND')
        showModal(modalDialog(
          title="Erreur",
          size = "m",
          footer = modalButton("Fermer"),
          "Aucun indicateur selectionné."
        ) 
        )
        
      }
    },error=function(e){
      shinyjs::hide('boxloader_update_IND')
      shinyjs::show('action_update_delete_IND')
      showModal(modalDialog(
        title="Erreur",
        size = "m",
        footer = modalButton("Fermer"),
        e$message
      ) 
      )
    })
  })
  #########################delete indicateur####################################
  observeEvent(input$delete_indicateur_btn,{
    tryCatch({
      validate(
        need(input$nom_indicateur_update != "", "Veuillez selectionner un indicateur. "),
        need(input$fonction_indicateur_update !="", "Veuillez saisir la formule mathématique de l'indicateur.")
      )
      
      shinyjs::hide('action_update_delete_IND')
      shinyjs::show('boxloader_update_IND')
      if(file.exists(paste0(DataFolder, "\\data\\FileIndicateur.RData")) && file.access(names = paste0(DataFolder, "\\data\\FileIndicateur.RData"), mode = 4)==0){
        load(paste0(DataFolder, "\\data\\FileIndicateur.RData"))
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
        save(listeIndicateur,file=paste0(DataFolder, "\\data\\FileIndicateur.RData"))
        ######Mise à jour de la liste de indicateurs sur l'interface utilisateur pour la modification des indicateurs###########
        loadUpdatedIndicator()
        ######Mise à jour de la liste de indicateurs sur l'interface utilisateur pour la visualisation des indicateurs###########
        updateSelectIndicator()
        shinyjs::hide('boxloader_update_IND')
        shinyjs::show('action_update_delete_IND')
        showNotification("Indicateur supprimé avec succès.", duration = 10, type = "message")
      }else{
        shinyjs::hide('boxloader_update_IND')
        shinyjs::show('action_update_delete_IND')
        showModal(modalDialog(
          title="Erreur",
          size = "m",
          footer = modalButton("Fermer"),
          "Aucun indicateur selectionné"
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
    ParamsDyn = readRDS(ResultFCM$ParamsDynFile)
    if(ResultFCM$DataType == "sentier"){
      Label_espece_or_parcelle = "Espèce simulée"
      Val_sentier_or_parcelle = ResultFCM$ParamSim$Essence
    }else{
      Label_espece_or_parcelle = "Parcelles simulées"
      Val_sentier_or_parcelle = paste(ResultFCM$ParamSim$Starting.plot,collapse=", ")
    }
    output$file_sim_informations <- renderUI({
      HTML(paste0('<div id= file_sim_informations_container class="container-fluid">
                  <h4>Informations sur la simulation </h4>
                  <div class="row">
                  <div class="col-sm-4">
                  <label>Type d\'inventaire : </label>
                  <span>', ResultFCM$DataType, '</span>
                  </div>',
                  '<div class="col-sm-4">
                  <label>Site : </label>
                  <span>', ParamsDyn$Descriptif$site, '</span>
                  </div>',
                  '<div class="col-sm-4">
                  <label>Pays : </label>
                  <span>', ParamsDyn$Descriptif$pays, '</span>
                  </div>',
                  '</div>',
                  '<div class="row">
                  <div class="col-sm-4">
                  <label>',Label_espece_or_parcelle,' : </label>
                  <span>', Val_sentier_or_parcelle, '</span>
                  </div>',
                  '<div class="col-sm-4">
                  <label>Date de la simulation : </label>
                  <span>', ResultFCM$DateSim, '</span>
                  </div>
                  </div>
                  </div>')
      )
    })
    shinyjs::show('description_file_sim')
  }
  
  updateInfosSite <- function(dataType, site, pays){
    output$file_dyn_informations <- renderUI({
    HTML(paste0('<div id= file_dyn_informations_container class="container-fluid">
                  <h4>Informations sur le site </h4>
                 <div class="row">
                 <div class="col-sm-4">
                 <label>Type d\'inventaire : </label>
                 <span>', dataType, '</span>
                 </div>',
                 '<div class="col-sm-4">
                 <label>Site : </label>
                 <span>', site, '</span>
                 </div>',
                 '<div class="col-sm-4">
                 <label>Pays : </label>
                 <span>', pays, '</span>
                 </div>
                 </div>
                 </div>')
      )
    })
    shinyjs::show("description_site")
  }
  
  updateUIInputsValues<- function(Parcelles, Campagnes, ClassesDiam, Species, AnneeExploitation){
    updateSelectizeInput(session, "parcelle",  choices = levels(Parcelles),
                         selected = NULL, server = TRUE)
    CampagneMin = as.numeric(levels(Campagnes)[1])
    CampagneMax = as.numeric(levels(Campagnes)[length(levels(Campagnes))])
    updateNumericInput(session, "anneedebutSim", value=as.numeric(CampagneMin), min = NA, max = NA, step = NA)
    updateNumericInput(session, "anneefirstlogging", value=AnneeExploitation, min = NA, max = NA, step = NA)
    updateNumericInput(session, "firstyearcompare", value=AnneeExploitation, min = NA, max = NA, step = NA)
    
  }
  
  loadTableDataSpeciesTraits <- function(SpeciesTraits){
    output$data_logging <-rhandsontable::renderRHandsontable({
      if("Name.sp" %in% names(SpeciesTraits)) nomEspece = SpeciesTraits$Name.sp
      else nomEspece = SpeciesTraits$Id.sp
      data_log = data.frame(nomEspece=nomEspece, codeEspece= SpeciesTraits$Id.sp, dme=SpeciesTraits$DME, dma=rep(NA, length(SpeciesTraits$Id.sp)), tauxPrelevement=rep(NA, length(SpeciesTraits$Id.sp)), coefRecollement=rep(NA, length(SpeciesTraits$Id.sp)), tarifcubage=rep(NA, length(SpeciesTraits$Id.sp)), densite=SpeciesTraits$WSG, stringsAsFactors = FALSE)
      data_log$dma = as.character(data_log$dma)
      data_log$tauxPrelevement = as.character(data_log$tauxPrelevement)
      data_log$coefRecollement = as.character(data_log$coefRecollement)
      data_log$tarifcubage = as.character(data_log$tarifcubage)
      NumberSpecies = length(SpeciesTraits$Id.sp)
      if(NumberSpecies== 1){
        height = 50
      }else if(NumberSpecies> 1 && NumberSpecies<= 7){
        height = 185
      }else if(NumberSpecies> 7 && NumberSpecies<= 10){
        height = 220
      }else{
        height = 300
      }
      rhandsontable::rhandsontable(data_log, colHeaders = c("Nom espèce", "Code espèce", "D.M.E", "D.M.A", "Taux de prélèvement", "Coefficient de recolement", "Tarif de cubage", "Densité"), selectCallback = TRUE, useTypes = TRUE, width = "100%", height = height) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)%>% 
        hot_cols(columnSorting = TRUE, colWidths= c(150, 100, 75, 75, 150, 175, 200, 100), manualColumnResize=TRUE, fixedColumnsLeft=1) #%>%
        # hot_validate_numeric(col = "D.M.E", allowInvalid=FALSE)%>%
        #hot_col(col = "Nom", type = "autocomplete", source =data_log$nomEspece)
    })
  }
  
  loadTableDataDamagesVector <- function(MyClassesDiam){
    output$vector_damage <-rhandsontable::renderRHandsontable({
      StrEval1= "data_damages = data.frame("
      StrEval2="ColHeaders = c("
      StrEval3="colWidths = c("
      colwidths =1005/length(MyClassesDiam)
      for(iter in 1:(length(MyClassesDiam)-1)){
        StrEval1 = paste0(StrEval1, 'classe', iter,'=c("NA"), ')
        StrEval2 = paste0(StrEval2, '"[', MyClassesDiam[iter],', ', MyClassesDiam[iter+1], '[", ')
        StrEval3 = paste0(StrEval3, paste0(colwidths,', '))
      }
      StrEval1 = paste0(StrEval1, 'classe', length(MyClassesDiam),'=c("NA"), stringsAsFactors = FALSE)')
      eval(parse(text = StrEval1))
      StrEval2 = paste0(StrEval2, '"[', MyClassesDiam[length(MyClassesDiam)],', Inf[")')
      eval(parse(text = StrEval2))
      StrEval3 = paste0(StrEval3, paste0(colwidths,')'))
      eval(parse(text = StrEval3))
      rhandsontable::rhandsontable(data_damages, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "1000px") %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_cols(columnSorting = TRUE, colWidths= colWidths, manualColumnResize=TRUE, fixedColumnsLeft=1) 
    })
  }
  
  loadTableNumberParDiameterClassVector <- function(MyClassesDiam){
    output$vector_number_per_diameter_class <-rhandsontable::renderRHandsontable({
      StrEval1= "data_number_per_diameter_class = data.frame("
      StrEval2="ColHeaders = c("
      StrEval3="colWidths = c("
      colwidths =460/length(MyClassesDiam) 
      for(iter in 1:(length(MyClassesDiam)-1)){
        StrEval1 = paste0(StrEval1, 'classe', iter,'=c("NA"), ')
        StrEval2 = paste0(StrEval2, '"[', MyClassesDiam[iter],', ', MyClassesDiam[iter+1], '[", ')
        StrEval3 = paste0(StrEval3, paste0(colwidths,', '))
      }
      StrEval1 = paste0(StrEval1, 'classe', length(MyClassesDiam),'=c("NA"), stringsAsFactors = FALSE)')
      eval(parse(text = StrEval1))
      StrEval2 = paste0(StrEval2, '"[', MyClassesDiam[length(MyClassesDiam)],', Inf[")')
      eval(parse(text = StrEval2))
      StrEval3 = paste0(StrEval3, paste0(colwidths,')'))
      eval(parse(text = StrEval3))
      rhandsontable::rhandsontable(data_number_per_diameter_class, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "1000px") %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_cols(columnSorting = TRUE, colWidths= colWidths, manualColumnResize=TRUE, fixedColumnsLeft=1) 
    })
  }
  
  loadTableContributionTauxRecrutementParDiameter <- function(MyClassesDiam){
    output$vector_tauxrecrutement_per_diameter_class <-rhandsontable::renderRHandsontable({
      StrEval1= "data_tr_per_diameter_class = data.frame("
      StrEval2="ColHeaders = c("
      StrEval3="colWidths = c("
      colwidths =450/length(MyClassesDiam) 
      for(iter in 1:(length(MyClassesDiam)-1)){
        StrEval1 = paste0(StrEval1, 'classe', iter,'=c("NA"), ')
        StrEval2 = paste0(StrEval2, '"[', MyClassesDiam[iter],', ', MyClassesDiam[iter+1], '[", ')
        StrEval3 = paste0(StrEval3, paste0(colwidths,', '))
      }
      StrEval1 = paste0(StrEval1, 'classe', length(MyClassesDiam),'=c("NA"), stringsAsFactors = FALSE)')
      eval(parse(text = StrEval1))
      StrEval2 = paste0(StrEval2, '"[', MyClassesDiam[length(MyClassesDiam)],', Inf[")')
      eval(parse(text = StrEval2))
      StrEval3 = paste0(StrEval3, paste0(colwidths,')'))
      eval(parse(text = StrEval3))
      rhandsontable::rhandsontable(data_tr_per_diameter_class, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "1000px") %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_cols(columnSorting = TRUE, colWidths= colWidths, manualColumnResize=TRUE, fixedColumnsLeft=1) 
    })
  }
  
  #########Chargement des paramètres de la dynamique ###############################################
  observeEvent(input$load_dyn,{
    tryCatch({
      shinyjs::hide('load_dyn')
      shinyjs::hide('bloc_parameters_logging')
      shinyjs::hide('bloc_parameters_simulation')
      shinyjs::show('boxloader_FileDyn')
      inFile = parseFilePaths(roots2, input$load_dyn)
      Loadpath = as.character(inFile$datapath)
      ParamsDynFile <<- Loadpath
      ParamsDyn = readRDS(ParamsDynFile)
      if(!is.null(ParamsDyn)){
        TypeInventaire = ParamsDyn$DataType
        updateInfosSite(TypeInventaire, ParamsDyn$Descriptif$site, ParamsDyn$Descriptif$pays)
        NewSpeciesTraits = ParamsDyn$SpeciesTraits
        ClassesDiam = ParamsDyn$Descriptif$ClassesDiam
        #alphaInd = MBaikiFormatted$alpha
        if(!is.null(NewSpeciesTraits)){
          loadTableDataSpeciesTraits(NewSpeciesTraits)
        }
        if(!is.null(TypeInventaire) && TypeInventaire == "sentier"){
          Essences = NULL
          if(!is.null(NewSpeciesTraits)){
            Essences = NewSpeciesTraits$Id.sp
          }
          updateSelectizeInput(session, "espece",  choices = levels(Essences),
                               selected = NULL, server = TRUE)
          loadTableNumberParDiameterClassVector(ClassesDiam)
          loadTableContributionTauxRecrutementParDiameter(ClassesDiam)
          shinyjs::hide('bloc_params_logging_parcelle')
          shinyjs::hide('bloc_list_parcelles_sim')
          shinyjs::show('bloc_params_logging_sentier')
          shinyjs::show('bloc_espece_sentier')
          shinyjs::show('bloc_recrutement_sentier')
          shinyjs::show('bloc_tauxrecrutement_sentiers_sim')
        }else{
          Parcelles = as.factor(ParamsDyn$Descriptif$Parcelles)
          Campagnes = as.factor(ParamsDyn$Descriptif$Campagnes)
          Species = ParamsDyn$SpeciesTraits$Name.sp
          AnneeExploitation = as.numeric(ParamsDyn$Descriptif$AnneeExploitation)
          updateUIInputsValues(Parcelles, Campagnes, ClassesDiam, Species, AnneeExploitation)
          shinyjs::hide('bloc_params_logging_sentier')
          shinyjs::hide('bloc_espece_sentier')
          shinyjs::hide('bloc_recrutement_sentier')
          shinyjs::hide('bloc_tauxrecrutement_sentiers_sim')
          shinyjs::show('bloc_params_logging_parcelle')
          shinyjs::show('bloc_list_parcelles_sim')
        }
        loadTableDataDamagesVector(ClassesDiam)
        
      }
      shinyjs::hide('boxloader_FileDyn')
      shinyjs::show('load_dyn')
      shinyjs::show('action_save_sim_file')
      shinyjs::show('actions_sim')
      shinyjs::show('bloc_parameters_logging')
      shinyjs::show('bloc_parameters_simulation')
      showNotification("Fichier importé avec succès", duration = 10, type = "message")
      
    },error=function(e){
      shinyjs::hide('boxloader_FileDyn')
      shinyjs::show('load_dyn')
      showModal(modalDialog(
        title="Erreur lors du chargement",
        size = "m",
        footer = modalButton("Fermer"),
        e$message
        ) 
      )
    })
  })
  
  ######Fonction du tracé de la structure diametrique et de l'affichage des données de l'évolution dans le temps###########
observeEvent(input$plot_SCD,{
  if(!is.null(CurrentSimDataFile) && file.exists(CurrentSimDataFile) && file.access(names = CurrentSimDataFile, mode = 4)==0){
    #ParamsDyn = readRDS(ParamsDynFile);
    load(file = CurrentSimDataFile)
    if(input$whatSD == 1){
      execute_plotSCD <- reactive({
        validate(
          if(input$selectGESDAll == FALSE){
            need(input$groupe_espece_strDia != "", "Veuillez selectionner un groupe d'espèces. ")
          } 
        )
        shinyjs::hide('plot_SCD')
        shinyjs::show('boxloader_SCD')
        if(input$selectGESDAll == FALSE){
          selectedSpecies = as.factor(input$groupe_espece_strDia)
          Groups = subset(ResultFCM$SpeciesTraits,  ResultFCM$SpeciesTraits$Name.sp%in%selectedSpecies)$Id.sp
          Groups=list(stand=as.factor(Groups))
        }else{
          Groups = list(stand=as.factor(ResultFCM$SpeciesTraits$Id.sp))
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
            names(data_class)[2] <- paste0("Cumul classe ", i)
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
        save(DataToExportSCD, file = paste0(DataFolder, "\\data\\DataExportSCD.RData"))
        save(PlotToExportSCD, file = paste0(DataFolder, "\\data\\PlotToExportSCD.RData"))
      })
      
    }else{
      execute_plotSCD <- reactive({
        validate(
          if(input$selectGESDAll == FALSE){
            need(input$groupe_espece_strDia != "", "Veuillez selectionner un groupe d'espèces. ")
          }
        )
        shinyjs::hide('plot_SCD')
        shinyjs::hide('bloc_graphique_SCD')
        shinyjs::hide('bloc_donnees_SCD')
        shinyjs::show('boxloader_SCD')
        
        StoreTime = ResultFCM$StoreTime
        StartingDate = StoreTime[1]
        if(input$selectGESDAll == FALSE){
          selectedSpecies = as.factor(input$groupe_espece_strDia)
          Groups = subset(ResultFCM$SpeciesTraits,  ResultFCM$SpeciesTraits$Name.sp%in%selectedSpecies)$Id.sp
          Groups=list(stand=as.factor(Groups))
        }else{
          Groups = list(stand=as.factor(ResultFCM$SpeciesTraits$Id.sp))
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
        data_SD = data.frame(Class= factor(ClassDiamPlot, levels = ClassDiamPlot), Effectif = tabEffs[,1], EffectifMin = tabEffs[,2], EffectifMax = tabEffs[,3])
        
        p <-ggplot(data=data_SD, aes(x=Class, y=Effectif)) + geom_bar(colour="black", fill="#6AC1F1", width=.4, stat="identity")+
          geom_errorbar(aes(ymin=EffectifMin, ymax=EffectifMax), width=.2, position=position_dodge(.9)) + theme_bw() + xlab("Diameter class") + ylab("Effective by class") +
          ggtitle(paste("Repartition of Effective for", names(Groups), "by Diameter class in", input$yearslider_strDiam, sep = " "))
        output$plot_strDia <- renderPlot({
          print(p)
        })
        names(data_SD) <- c("Classe", "Effectif", "Effectif minimal", "Effectif maximal")
        output$data_strDia<- DT::renderDataTable(data_SD)
        DataToExportSCD = data_SD
        PlotToExportSCD = p
        save(DataToExportSCD, file = paste0(DataFolder, "\\data\\DataExportSCD.RData"))
        save(PlotToExportSCD, file = paste0(DataFolder, "\\data\\PlotToExportSCD.RData"))
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
          title="Erreur",
          size = "m",
          footer = modalButton("Fermer"),
          e$message
        ) 
      )
      shinyjs::hide('boxloader_SCD')
      shinyjs::show('plot_SCD')
    })
  }else{
    showModal(modalDialog(
      title="Erreur",
      size = "m",
      footer = modalButton("Fermer"),
      "Veuillez charger un fichier de données simulées ou lancer une simulation."
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

#####################Fonction associé à l'évènement d'affichage des blocs d'ajout  et de mise à jour d'un indicateur ##############
observeEvent(input$action_indicateur,{
  if(input$action_indicateur == 1){
    shinyjs::hide('update_indicateur')
    shinyjs::show('new_indicateur')
  }else if(input$action_indicateur== 2){
    shinyjs::hide('new_indicateur')
    shinyjs::show('update_indicateur')
  }
})
######Fonction du tracé de l'indicateur et de l'affichage des données de l'évolution dans le temps###########
Plot_Indicateur <- function(MyTimeInterval=NULL){
  if(!is.null(CurrentSimDataFile) && file.exists(CurrentSimDataFile) && file.access(names = CurrentSimDataFile, mode = 4)==0){
    load(file = CurrentSimDataFile)
    ClassesDiam = ResultFCM$ClassesDiam
    tryCatch({
      validate(
        if(input$selectGEIAll == FALSE) need(input$groupe_espece_indicateur != "", "Veuillez selectionner un groupe d'espèce. "),
        if(input$plageClass == 1){
          need(input$diamMin <= input$diamMax, "le diamètre min ne peut pas être supérieur au diamètre max. ")
        }else if(input$plageClass == 2){
          need(input$classMin <= input$classMax, "la classe min ne peut pas être supérieure à la classe max. ")
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
        Groups = subset(ResultFCM$SpeciesTraits,  ResultFCM$SpeciesTraits$Name.sp%in%selectedSpecies)$Id.sp
        Groups=list(stand=as.factor(Groups))
      }else{
        Groups = list(stand=as.factor(ResultFCM$SpeciesTraits$Id.sp))
      } 
      p <- ggplot()
      if(Indicator$NomInd == "Volume exploitable" || Indicator$NomInd == "Stock exploitable"){
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
      p <- PlotMyIndicator(ResultFCM, Groups = Groups, MyClassesDiam = MyClassesDiam, MyTimeInterval = MyTimeInterval, MyIndicator = Indicator)
      plotbuild <- ggplot_build(p)
      DataToExport = data.frame()
      PlotToExport = p
      taille = length(plotbuild$data)
      printData= TRUE
      if(taille > 1 && nrow(plotbuild$data[[1]]) != 0){
        entete = c("Temps")
        if(taille > 1){
          data_med <- plotbuild$data[[1]]
          temps= data_med$x
          ExpPlot = paste0("data_med <- data.table(Temps=data_med$x, Med",Indicator$VarInd,'=data_med$y, key = "Temps")', collapse = '')
          eval(parse(text = ExpPlot))
          DataToExportTmp <- data_med
          entete = c(entete, paste0("Mediane de ", Indicator$VarInd, collapse = ''))
        }
        if(taille >= 2){
          data_li <- plotbuild$data[[2]]
          ExpPlot = paste0("data_li <- data.table(Temps=data_li$x, Min",Indicator$VarInd,'=data_li$y, key = "Temps")', collapse = '')
          eval(parse(text = ExpPlot))
          DataToExportTmp = merge(DataToExportTmp, data_li, all.x=T, all.y=T)
          entete = c(entete, paste0("Minimum de ", Indicator$VarInd, collapse = ''))
        }
        if(taille >= 3){
          data_ls <- plotbuild$data[[3]]
          ExpPlot = paste0("data_ls <- data.table(Temps=data_ls$x, Max",Indicator$VarInd,'=data_ls$y, key = "Temps")', collapse = '')
          eval(parse(text = ExpPlot))
          DataToExportTmp = merge(DataToExportTmp, data_ls, all.x=T, all.y=T)
          entete = c(entete, paste0("Maximum de ", Indicator$VarInd, collapse = ''))
        }
        if(taille >= 4){
          data_verif <- plotbuild$data[[4]]
          ExpPlot = paste0("data_verif <- data.table(Temps=data_verif$x, Verif",Indicator$VarInd,'=data_verif$y, key = "Temps")', collapse = '')
          eval(parse(text = ExpPlot))
          DataToExportTmp = merge(DataToExportTmp, data_verif, all.x=T, all.y=T)
          entete = c(entete, paste0("Données réelles de ", Indicator$VarInd, collapse = ''))
        }
      }else if(taille==1 && nrow(plotbuild$data[[1]]) != 0){
        data_verif <- plotbuild$data[[1]]
        ExpPlot = paste0("data_verif <- data.table(Temps=data_verif$x, Verif",Indicator$VarInd,'=data_verif$y, key = "Temps")', collapse = '')
        eval(parse(text = ExpPlot))
        DataToExportTmp = merge(DataToExportTmp, data_verif, all.x=T, all.y=T)
        entete = c("Temps", entete, paste0("Données réelles de ", Indicator$VarInd, collapse = ''))
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
      save(DataToExport, file = paste0(DataFolder, "\\data\\DataExport.RData"))
      save(PlotToExport, file = paste0(DataFolder, "\\data\\PlotToExport.RData"))
      shinyjs::hide('boxloader_IND')
      shinyjs::show('afficher_indicateur')
      shinyjs::show('bloc_export_indicateur_rg')
      shinyjs::show('bloc_export_indicateur_data')
      shinyjs::show('bloc_graphique_Indicateur')
      shinyjs::show('bloc_donnees_Indicateur')
      print("End plot Indicateur")
      }, error=function(e){
        showModal(modalDialog(
            title="Erreur",
            size = "m",
            footer = modalButton("Fermer"),
            e$message
          ) 
        )
        shinyjs::hide('boxloader_IND')
        shinyjs::show('afficher_indicateur')
    }) 
  }else{
    showModal(modalDialog(
        title="Erreur",
        size = "m",
        footer = modalButton("Fermer"),
        "Veuillez charger un fichier de données simulées ou lancer une simulation."
      ) 
    )
  }
  
}


############## Function associée à l'évènement d'affichage d'un indicateur (courbe d'évolution et données) ######################
observeEvent(input$afficher_indicateur,{
  YearRange = input$yearrange_indicateur
  MyTimeInterval = YearRange[1]:YearRange[2]
  Plot_Indicateur(MyTimeInterval)
})

observeEvent(input$indicateur,{
  if(input$indicateur == "Stock exploitable" || input$indicateur == "Volume exploitable"){
    shinyjs::hide('block_class_diam')
  }else{
    shinyjs::show('block_class_diam')
  }
})

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
    if(file.exists(paste0(DataFolder, "\\data\\DataExport.RData")) && file.access(names = paste0(DataFolder, "\\data\\DataExport.RData"), mode = 4)==0){
      load(paste0(DataFolder, "\\data\\DataExport.RData"))
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
    if(file.exists(paste0(DataFolder, "\\data\\DataExportSCD.RData")) && file.access(names = paste0(DataFolder, "\\data\\DataExportSCD.RData"), mode = 4)==0){
      load(paste0(DataFolder, "\\data\\DataExportSCD.RData"))
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
    if(file.exists(paste0(DataFolder, "\\data\\PlotToExport.RData")) && file.access(names = paste0(DataFolder, "\\data\\PlotToExport.RData"), mode = 4)==0){
      load(paste0(DataFolder, "\\data\\PlotToExport.RData"))
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
    if(file.exists(paste0(DataFolder, "\\data\\PlotToExportSCD.RData")) && file.access(names = paste0(DataFolder, "\\data\\PlotToExportSCD.RData"), mode = 4)==0){
      load(paste0(DataFolder, "\\data\\PlotToExportSCD.RData"))
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

updateSliderInput(session, "yearrange_indicateur", value = NULL,
                    min = NULL, max = NULL, step = 1)


updateSliderInput(session, "yearslider_strDiam", value = NULL,
                    min = NULL, max = NULL, step = 1)
#alphaInd = MBaikiFormatted$alpha
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
})
  

