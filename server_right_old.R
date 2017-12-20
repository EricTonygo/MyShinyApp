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
source("ParamInferenceMBaiki.R",local=T)
source("R/InferFCM.R")
source("R/ClasseDiamSTAGB.R")
source("R/StartData.r")
source("MyAppInterfaceWithSimulator.R")
source("algoExtractIndicator.R")
source("buildListOfIndicator.R",local=T)
#source("R/plot.FCM.r")
shinyServer(function(input, output, session){ 
  session$onSessionEnded(function() {
    stopApp()
  })
  ############################## Creation du dossier de sauvegarde des paramètres de la simulation ##############################
  ParamsimFolder = path.expand(path="~/DafSim/ParametresSimulation")
  if(!dir.exists(ParamsimFolder)) dir.create(ParamsimFolder, recursive = TRUE)
  #volumes <- getVolumes()
  ##################################################################################################################################
  #initialisation du dossier par defaut dans lequel se feront la sauvegarde et le chargement des paramètrages des simulations
  #contenant des scéarios d'exploitation forestière et les autres parmètres de la simulation
  #Puis initialisation des fonctions de sauvegarde et chargement des fichiers au format .RData
  ##################################################################################################################################
  roots = c(wd=ParamsimFolder)
  shinyFileSave(input, 'save_config', session=session, roots=roots, restrictions = system.file(package = "base"))
  shinyFileChoose(input, 'load_config', session=session,
                  roots=roots, filetypes=c('', 'RData'))
  
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
  
  if(file.exists(DataFolder)){
    file.copy(from = "./data", to = DataFolder, recursive = TRUE)
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
      shinyjs::info(e)
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
      shinyjs::info(e)
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
    shinyjs::show('lancer_sim_col')
    shinyjs::hide('gotoparameters_col')
    
  })
  ################################ Simulation de la dynamique forestière ##############################
  observeEvent(input$lancer_sim,{
    #shinyjs::info("load data/DataMBailiFCM.Rdata")
    #load("data/DataMBaikiFCM.RData")
    #shinyjs::info("loading finish")
    #test=InferFCM(MBaikiFormatted,"ParamInferenceMBaiki.R")
    error_sim= FALSE
    #hidable = FALSE
    simulation.input <- reactive({
      Date1PostLog.Check = 0
      ##################################################################################################################################
      #Validation des inputs saisis par l'utilisateur.
      ##################################################################################################################################
      validate(
        need(input$anneedebutSim != "", "Vueillez entrer la date de debut de la simulation"),
        need(input$anneefirstlogging !="", "Veuillez entrer la date de la première exploitation"),
        if(input$Allparcelle == FALSE){
          need(input$parcelle != "", "Vueillez selectionner les parcelles à simuler")
        },
        need(input$nombrerotation != "", "Vueillez entrer le nombre de rotation"),
        need(input$dureesimulation !="", "Veuillez entrer la durée de la simulation"),
        need(input$nbchain != "", "Vueillez entrer le nombre de simulation"),
        need(input$dureerotation !="", "Veuillez entrer la durée d'une rotation"),
        if(input$check == TRUE){
          need(!is.null(input$firstyearcompare), "veuillez renseigner la première année de comparaison avec les données réelles.")
        },
        need(as.numeric(input$anneedebutSim) <= as.numeric(input$anneefirstlogging), "La première année d'exploitation doit être supérieure ou égale à l'année de debut de la simulation"),
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
        DF$Taux_de_prelevement = as.numeric(DF$Taux_de_prelevement)
        DF$Coefficient_de_recolement = as.numeric(DF$Coefficient_de_recolement)
        DF$D.M.A = as.numeric(DF$D.M.A)
        DF$Taux_de_prelevement[is.na(DF$Taux_de_prelevement) & !is.na(DF$D.M.A)]=100
        DF$Coefficient_de_recolement[is.na(DF$Coefficient_de_recolement) & !is.na(DF$D.M.A)]=100
        DF$Taux_de_prelevement[is.na(DF$Taux_de_prelevement)]=0
        DF$Coefficient_de_recolement[is.na(DF$Coefficient_de_recolement)]=0
        DF$D.M.A[is.na(DF$D.M.A)]= Inf
        DF$Taux_de_prelevement = DF$Taux_de_prelevement/100
        DF$Coefficient_de_recolement = DF$Coefficient_de_recolement/100
        DF$Tarif_de_cubage[DF$Tarif_de_cubage=="NA" | DF$Tarif_de_cubage==""]= tarifgenerique
        SpeciesTraits= data.frame(Id.sp= as.character(DF$Code_espece), WSG = DF$densite, DME= DF$D.M.E, DMA=DF$D.M.A, tauxPrelevement= DF$Taux_de_prelevement, coefRecollement = DF$Coefficient_de_recolement, tarifs = DF$Tarif_de_cubage, TarifGenerique = tarifgenerique , stringsAsFactors = FALSE) 
        
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
        MBaikiSpeciesTraits = MBaikiFormatted$TraitData
        DF <- data.frame(codeEspece= MBaikiSpeciesTraits$Id.sp, dme=MBaikiSpeciesTraits$DME, dma=rep(Inf, length(MBaikiSpeciesTraits$Id.sp)), tauxPrelevement=rep(0, length(MBaikiSpeciesTraits$Id.sp)), coefRecollement=rep(0, length(MBaikiSpeciesTraits$Id.sp)), tarifcubage=rep(tarifgenerique, length(MBaikiSpeciesTraits$Id.sp)), densite=MBaikiSpeciesTraits$WSG)
        SpeciesTraits= data.frame(Id.sp= as.character(DF$codeEspece), WSG = DF$densite, DME= DF$dme, DMA=DF$dma, tauxPrelevement= DF$tauxPrelevement, coefRecollement = DF$coefRecollement, tarifs = DF$tarifcubage, TarifGenerique = tarifgenerique , stringsAsFactors = FALSE)
        #SpeciesTraits = NULL
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
      if(!error_sim){ 
        shinyjs::show('boxloader')
        shinyjs::show('loader_sim')
        shinyjs::show('sim_encours')
        shinyjs::hide('image_success')
        shinyjs::hide('sim_finish')
        shinyjs::hide('param_sim_logging')
        shinyjs::hide('actions_sim')
        assign("hidable", TRUE, envir = .GlobalEnv)
        print(hidable)
        nbchain = input$nbchain
        load(paste0(DataFolder,"\\data\\DataMBaikiFCM.RData"))
          if(input$Allparcelle == TRUE){
            Starting.plot= as.numeric(levels(MBaikiFormatted$SimulatingData$Id.zone))
          }else{
            Starting.plot= as.numeric(input$parcelle)
          }
        
        StartingDate = as.numeric(input$anneedebutSim)
        Nb.rotation = as.numeric(input$nombrerotation)
        rotation = as.numeric(input$dureerotation)
        DelayLogging =as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim)
        Fin.simu = as.numeric(input$dureesimulation)
        Date1PostLog.Check = input$firstyearcompare
        Check = input$check
        if(is.na(Check) || !is.logical(Check)) Check = FALSE
        #MySpeciesTraits = SpeciesTraits
        vector_damage = v/100
        
        Logging="T2.MBaiki"
        load(paste0(DataFolder,"\\data\\FileIndicateur.RData"))
        ##################################################################################################################################
        #Calcul des paramètres du modèle en utilisant les données réelles collectées sur les arbres et 
        #contenu dans le paramètre MBaikiFormatted
        ##################################################################################################################################
        test=InferFCM(MBaikiFormatted,"ParamInferenceMBaiki.R", SpeciesTraits = SpeciesTraits, listeIndicateur= listeIndicateur)
        save(test,file=paste0(DataFolder, "\\data\\out-inferFCM.RData"))
        tryCatch({
          rm(test)
        },error=function(e){})
        load(paste0(DataFolder,"\\data\\out-inferFCM.RData"))
        ParamSim = list(Nb.rotation = Nb.rotation, rotation = rotation, Fin.simu = Fin.simu, DelayLogging = DelayLogging, nbchain = nbchain, vector_damage = vector_damage, Starting.plot = Starting.plot, StartingDate = StartingDate, nbchain = nbchain, Check = Check, Date1PostLog.Check = Date1PostLog.Check, MySpeciesTraits = SpeciesTraits, Logging = Logging, Tarifgenerique = tarifgenerique)
        ##################################################################################################################################
        #Simulation proprement dite
        ##################################################################################################################################
        ResultTestMB= FCM(out.InferFCM=test, ParamSim)
        #load("data/ResultTestMB.RData")
        ##################################################################################################################################
        #Sauvegarde des resultats de la simulation dans le fichier "data/ResultTestMB.RData" et d'autres paramètres
        #utilisable ultérieurement.
        ##################################################################################################################################
        Nb.period=ResultTestMB$ParamPlot$Nb.period
        max_Temps = max(ResultTestMB$Simulations$Temps)+StartingDate
        StoreTime= c(StartingDate:max_Temps);
        save(StoreTime,file=paste0(DataFolder, "\\data\\lastTimeSimulate.RData"))
        updateSliderInput(session, "yearslider_strDiam", value = StoreTime[1], min = StoreTime[1], max = StoreTime[length(StoreTime)], step = 1)
        updateSliderInput(session, "yearrange_indicateur", value = c(StoreTime[1], StoreTime[length(StoreTime)]),
                          min = StoreTime[1], max = StoreTime[length(StoreTime)], step = 1)
        save(ResultTestMB,file=paste0(DataFolder, "\\data\\ResultTestMB.RData"))
        ParamSim$vector_damage = VD_SAVE
        ParamSim$MySpeciesTraits = DF_SAVE
        shinyjs::hide('loader_sim')
        shinyjs::hide('sim_encours')
        shinyjs::show('image_success')
        shinyjs::show('sim_finish')
        shinyjs::show('actions_sim')
        shinyjs::hide('lancer_sim_col')
        shinyjs::show('gotoparameters_col')
      }else{
        shinyjs::info(error_sim_msg)
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
        shinyjs::hide('lancer_sim_col')
        shinyjs::show('gotoparameters_col')
      }
     shinyjs::info(e)
      
    })
  })
  
  ################################ Sauvegarde des paramètres de la dynamique forestière ##############################
  observeEvent(input$save_config,{
    tryCatch({
      inFile = parseSavePath(roots, input$save_config)
      Savepath = as.character(inFile$datapath)
      print(Savepath)
      error_sim = FALSE
      
      tarifgenerique = input$tarifgenerique
      if(tarifgenerique ==""){
        tarifgenerique = "10^(-2.96+1.93*log10(d))"
      }
      if (!is.null(input$data_logging)) {
        DF  <-  hot_to_r(input$data_logging)
        DF_SAVE <- DF
        DF$Taux_de_prelevement = as.numeric(DF$Taux_de_prelevement)
        DF$Coefficient_de_recolement = as.numeric(DF$Coefficient_de_recolement)
        DF$D.M.A = as.numeric(DF$D.M.A)
        DF$Taux_de_prelevement[is.na(DF$Taux_de_prelevement) & !is.na(DF$D.M.A)]=100
        DF$Coefficient_de_recolement[is.na(DF$Coefficient_de_recolement) & !is.na(DF$D.M.A)]=100
        DF$Taux_de_prelevement[is.na(DF$Taux_de_prelevement)]=0
        DF$Coefficient_de_recolement[is.na(DF$Coefficient_de_recolement)]=0
        DF$D.M.A[is.na(DF$D.M.A)]= Inf
        DF$Taux_de_prelevement = DF$Taux_de_prelevement/100
        DF$Coefficient_de_recolement = DF$Coefficient_de_recolement/100
        DF$Tarif_de_cubage[DF$Tarif_de_cubage=="NA" | DF$Tarif_de_cubage==""]= "10^(-2.96+1.93*log10(d))"
        SpeciesTraits= data.frame(Id.sp= as.character(DF$Code_espece), WSG = DF$densite, DME= DF$D.M.E, DMA=DF$D.M.A, tauxPrelevement= DF$Taux_de_prelevement, coefRecollement = DF$Coefficient_de_recolement, tarifs = DF$Tarif_de_cubage) 
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
        MBaikiSpeciesTraits = MBaikiFormatted$TraitData
        DF <- data.frame(codeEspece= MBaikiSpeciesTraits$Id.sp, dme=MBaikiSpeciesTraits$DME, dma=rep(Inf, length(MBaikiSpeciesTraits$Id.sp)), tauxPrelevement=rep(0, length(MBaikiSpeciesTraits$Id.sp)), coefRecollement=rep(0, length(MBaikiSpeciesTraits$Id.sp)), tarifcubage=rep("10^(-2.96+1.93*log10(d))", length(MBaikiSpeciesTraits$Id.sp)), densite=MBaikiSpeciesTraits$WSG)
        SpeciesTraits= data.frame(Id.sp= DF$codeEspece, WSG = DF$densite, DME= DF$dme, DMA=DF$dma, tauxPrelevement= DF$tauxPrelevement, coefRecollement = DF$coefRecollement)
        #SpeciesTraits = NULL
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
      if(!error_sim){ 
        nbchain = input$nbchain
        Starting.plot= as.numeric(input$parcelle)
        StartingDate = as.numeric(input$anneedebutSim)
        Nb.rotation = as.numeric(input$nombrerotation)
        rotation = as.numeric(input$dureerotation)
        DelayLogging =as.numeric(input$anneefirstlogging)-as.numeric(input$anneedebutSim)
        Fin.simu = as.numeric(input$dureesimulation)
        Date1PostLog.Check = input$firstyearcompare
        Check.Allparcelle = input$Allparcelle
        Check = input$check
        StartingLogging = as.numeric(input$anneefirstlogging)
        ParamSim = list(StartingLogging = StartingLogging, Check.Allparcelle = Check.Allparcelle, Fin.simu = Fin.simu, Nb.rotation = Nb.rotation, rotation = rotation, DelayLogging = DelayLogging, nbchain = nbchain, vector_damage = VD_SAVE, Starting.plot = Starting.plot, StartingDate = StartingDate, nbchain = nbchain, Check = Check, Date1PostLog.Check = Date1PostLog.Check, MySpeciesTraits = DF_SAVE , Tarifgenerique = tarifgenerique)
        ParamSim$vector_damage = VD_SAVE
        ParamSim$MySpeciesTraits = DF_SAVE
        save(ParamSim, file = Savepath)
        shinyjs::info("Le paramètrage a été sauvegardé avec succès.")
      }else{
        shinyjs::info(error_sim_msg)
      }
    },error=function(e){
       shinyjs::info("Erreur de sauvegarde du scénario")
    })
    
  })
  
  ############ Charger le dernier paramètrage #########################
  observeEvent(input$load_config,{
    tryCatch({
      inFile = parseFilePaths(roots, input$load_config)
      Loadpath = as.character(inFile$datapath)
      load(Loadpath)
      MySpeciesTraits = ParamSim$MySpeciesTraits
      if(!is.null(MySpeciesTraits)){
        output$data_logging <-rhandsontable::renderRHandsontable({
          rhandsontable::rhandsontable(MySpeciesTraits, colHeaders = c("Nom", "Code_espece", "D.M.E", "D.M.A", "Taux_de_prelevement", "Coefficient_de_recolement", "Tarif_de_cubage", "densite"), selectCallback = TRUE, useTypes = TRUE, width = "100%") %>%
            hot_cols(columnSorting = TRUE, colWidths= c(150, 100, 75, 75, 150, 175, 200, 75), manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
            #hot_cols(columnSorting = TRUE, fixedColumnsLeft=1) %>%
            hot_col(col = "Nom", type = "autocomplete", source =MySpeciesTraits$Nom)
        })
      }
      Vector_damage = ParamSim$vector_damage
      if(!is.null(Vector_damage)){
        output$vector_damage <-rhandsontable::renderRHandsontable({
          source("ParamInferenceMBaiki.R",local = T)
          StrEval2="ColHeaders = c("
          StrEval3="colWidths = c("
          for(iter in 1:(length(ClassesDiam)-1)){
            StrEval2 = paste0(StrEval2, '"[', ClassesDiam[iter],', ', ClassesDiam[iter+1], '[", ')
            StrEval3 = paste0(StrEval3, '125, ')
          }
          StrEval2 = paste0(StrEval2, '"[', ClassesDiam[length(ClassesDiam)],', Inf[")')
          eval(parse(text = StrEval2))
          StrEval3 = paste0(StrEval3, '125)')
          eval(parse(text = StrEval3))
          rhandsontable::rhandsontable(Vector_damage, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "100%") %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
            hot_cols(columnSorting = TRUE, colWidths= colWidths, manualColumnResize=TRUE, fixedColumnsLeft=1) 
        })
      }
      updateNumericInput(session,"anneedebutSim", value=ParamSim$StartingDate)
      updateNumericInput(session,"anneefirstlogging", value=ParamSim$StartingLogging)
      updateTextInput(session,"dureesimulation", value=ParamSim$Fin.simu)
      updateNumericInput(session,"dureerotation", value=ParamSim$rotation)
      updateNumericInput(session,"nombrerotation", value=ParamSim$Nb.rotation)
      updateNumericInput(session,"nbchain", value=ParamSim$nbchain)
      updateNumericInput(session,"firstyearcompare", value=ParamSim$Date1PostLog.Check)
      updateCheckboxInput(session,"Allparcelle", value=ParamSim$Check.Allparcelle)
      updateCheckboxInput(session,"check", value=ParamSim$Check)
      updateTextInput(session, "tarifgenerique", value = ParamSim$Tarifgenerique)
      if(!ParamSim$Check.Allparcelle){
        updateSelectizeInput(session, "parcelle", choices = choicesParcelles, selected = ParamSim$Starting.plot, server = TRUE)
      }
      shinyjs::info("le dernier paramètrage a été chargé avec succès")
    },error=function(e){
      shinyjs::info("Erreur de chargement du scénario")
    })
  })
  
  
  ############ Ajout d'un indicateur #########################
  observeEvent(input$Ajout_indicateur,{
    tryCatch({
      validate(
        need(input$nom_indicateur_new != "", "Vueillez entrer le nom de l'indicateur"),
        need(input$fonction_indicateur_new !="", "Veuillez entrer la formulation mathématique de l'indicateur")
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
        if(file.exists(paste0(DataFolder, "\\data\\ResultTestMB.RData")) && file.access(names = paste0(DataFolder, "\\data\\ResultTestMB.RData"), mode = 4)==0){
          load(paste0(DataFolder, "\\data\\ResultTestMB.RData"))
          ResultTestMB$ParamPlot$CDSTB=ClasseDiamSTAGB("ParamInferenceMBaiki.R", SpeciesTraits =ResultTestMB$SpeciesTraits ,alpha=alphaInd, OtherIndicator = listeIndicateur)
          save(ResultTestMB,file=paste0(DataFolder, "\\data\\ResultTestMB.RData"))
        }
        save(listeIndicateur,file=paste0(DataFolder, "\\data\\FileIndicateur.RData"))
        ######Mise à jour de la liste de indicateurs sur l'interface utilisateur de visualisation des indicateurs###########
        updateSelectIndicator()
        shinyjs::hide('boxloader_add_IND')
        shinyjs::show('action_add_IND')
        shinyjs::info("Indicateur ajouté avec succès")
      }else{
        shinyjs::hide('boxloader_add_IND')
        shinyjs::show('action_add_IND')
        shinyjs::info("Un indicateur possède déjà une des informations de cet indicateur")
      }
    },error=function(e){
      shinyjs::hide('boxloader_add_IND')
      shinyjs::show('action_add_IND')
      shinyjs::info(e)
    })
  })
  #########################Modification des indicateurs####################################
  observeEvent(input$update_indicateur,{
    tryCatch({
      validate(
        need(input$nom_indicateur_update != "", "Vueillez selectionner un indicateur"),
        need(input$fonction_indicateur_update !="", "Veuillez entrer la formulation mathématique de l'indicateur")
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
        if(file.exists(paste0(DataFolder, "\\data\\ResultTestMB.RData")) && file.access(names = paste0(DataFolder, "\\data\\ResultTestMB.RData", mode = 4))==0){
          load(paste0(DataFolder, "\\data\\ResultTestMB.RData"))
          ResultTestMB$ParamPlot$CDSTB=ClasseDiamSTAGB("ParamInferenceMBaiki.R", SpeciesTraits =ResultTestMB$SpeciesTraits ,alpha=alphaInd, OtherIndicator = listeIndicateur)
          save(ResultTestMB,file=paste0(DataFolder, "\\data\\ResultTestMB.RData"))
        }
        save(listeIndicateur,file=paste0(DataFolder, "\\data\\FileIndicateur.RData"))
        ######Mise à jour de la liste de indicateurs sur l'interface utilisateur pour la modification des indicateurs###########
        loadUpdatedIndicator()
        ######Mise à jour de la liste de indicateurs sur l'interface utilisateur pour la visualisation des indicateurs###########
        updateSelectIndicator()
        shinyjs::hide('boxloader_update_IND')
        shinyjs::show('action_update_delete_IND')
        shinyjs::info("Indicateur mis à jour avec succès")
      }else{
        shinyjs::hide('boxloader_update_IND')
        shinyjs::show('action_update_delete_IND')
        shinyjs::info("Aucun indicateur selectionné.")
      }
    },error=function(e){
      shinyjs::hide('boxloader_update_IND')
      shinyjs::show('action_update_delete_IND')
      shinyjs::info(e)
    })
  })
  #########################delete indicateur####################################
  observeEvent(input$delete_indicateur,{
    tryCatch({
      validate(
        need(input$nom_indicateur_update != "", "Vueillez selectionner un indicateur"),
        need(input$fonction_indicateur_update !="", "Veuillez entrer la formulation mathématique de l'indicateur")
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
        if(file.exists(paste0(DataFolder, "\\data\\ResultTestMB.RData")) && file.access(names = paste0(DataFolder, "\\data\\ResultTestMB.RData"), mode = 4)==0){
          load(paste0(DataFolder, "\\data\\ResultTestMB.RData"))
          ResultTestMB$ParamPlot$CDSTB=ClasseDiamSTAGB("ParamInferenceMBaiki.R", SpeciesTraits =ResultTestMB$SpeciesTraits ,alpha=alphaInd, OtherIndicator = listeIndicateur)
          save(ResultTestMB,file=paste0(DataFolder, "\\data\\ResultTestMB.RData"))
        }
        save(listeIndicateur,file=paste0(DataFolder, "\\data\\FileIndicateur.RData"))
        ######Mise à jour de la liste de indicateurs sur l'interface utilisateur pour la modification des indicateurs###########
        loadUpdatedIndicator()
        ######Mise à jour de la liste de indicateurs sur l'interface utilisateur pour la visualisation des indicateurs###########
        updateSelectIndicator()
        shinyjs::hide('boxloader_update_IND')
        shinyjs::show('action_update_delete_IND')
        shinyjs::info("Indicateur supprimé avec succès")
      }else{
        shinyjs::hide('boxloader_update_IND')
        shinyjs::show('action_update_delete_IND')
        shinyjs::info("Aucun indicateur selectionné.")
      }
    },error=function(e){
      shinyjs::hide('boxloader_update_IND')
      shinyjs::show('action_update_delete_IND')
      shinyjs::info(e)
    })
  })
  
  
  ###########Creation de la table des espèces de la forêt pour les paramètres d'exploitation############
  if(file.exists(paste0(DataFolder, "\\data\\DataMBaikiFCM.RData")) && file.access(names = paste0(DataFolder, "\\data\\DataMBaikiFCM.RData"), mode = 4)==0){
    load(paste0(DataFolder, "\\data\\DataMBaikiFCM.RData"))
    MBaikiSpeciesTraits = MBaikiFormatted$TraitData
    output$data_logging <-rhandsontable::renderRHandsontable({
      if("Name.sp" %in% names(MBaikiSpeciesTraits)) nomEspece = MBaikiSpeciesTraits$Name.sp
      else nomEspece = MBaikiSpeciesTraits$Id.sp
      data_log = data.frame(nomEspece=nomEspece, codeEspece= MBaikiSpeciesTraits$Id.sp, dme=MBaikiSpeciesTraits$DME, dma=rep(NA, length(MBaikiSpeciesTraits$Id.sp)), tauxPrelevement=rep(NA, length(MBaikiSpeciesTraits$Id.sp)), coefRecollement=rep(NA, length(MBaikiSpeciesTraits$Id.sp)), tarifcubage=rep(NA, length(MBaikiSpeciesTraits$Id.sp)), densite=MBaikiSpeciesTraits$WSG)
      data_log$dma = as.character(data_log$dma)
      data_log$tauxPrelevement = as.character(data_log$tauxPrelevement)
      data_log$coefRecollement = as.character(data_log$coefRecollement)
      data_log$tarifcubage = as.character(data_log$tarifcubage)
      rhandsontable::rhandsontable(data_log, colHeaders = c("Nom", "Code_espece", "D.M.E", "D.M.A", "Taux_de_prelevement", "Coefficient_de_recolement", "Tarif_de_cubage", "densite"), selectCallback = TRUE, useTypes = TRUE, width = "100%") %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)%>% 
        hot_cols(columnSorting = TRUE, colWidths= c(150, 100, 75, 75, 150, 175, 200, 100), manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
        # hot_validate_numeric(col = "D.M.E", allowInvalid=FALSE)%>%
        hot_col(col = "Nom", type = "autocomplete", source =data_log$nomEspece)
      #print(data_log$nomEspece)
  })
}
  ######Creation de la table du vecteur des degats avec les colonnes correspondant aux classes de diamètre###########
output$vector_damage <-rhandsontable::renderRHandsontable({
  source("ParamInferenceMBaiki.R",local = T)
  StrEval1= "data_log = data.frame("
  StrEval2="ColHeaders = c("
  StrEval3="colWidths = c("
  for(iter in 1:(length(ClassesDiam)-1)){
    StrEval1 = paste0(StrEval1, 'classe', iter,'=c("NA"), ')
    StrEval2 = paste0(StrEval2, '"[', ClassesDiam[iter],', ', ClassesDiam[iter+1], '[", ')
    StrEval3 = paste0(StrEval3, '125, ')
  }
  StrEval1 = paste0(StrEval1, 'classe', length(ClassesDiam),'=c("NA"), stringsAsFactors = FALSE)')
  eval(parse(text = StrEval1))
  StrEval2 = paste0(StrEval2, '"[', ClassesDiam[length(ClassesDiam)],', Inf[")')
  eval(parse(text = StrEval2))
  StrEval3 = paste0(StrEval3, '125)')
  eval(parse(text = StrEval3))
  rhandsontable::rhandsontable(data_log, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "1000px") %>%
    hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
    hot_cols(columnSorting = TRUE, colWidths= colWidths, manualColumnResize=TRUE, fixedColumnsLeft=1) 
})
  ######Fonction du tracé de la structure diametrique et de l'affichage des données de l'évolution dans le temps###########
observeEvent(input$plot_SCD,{
  if(input$whatSD == 1){
    execute_plotSCD <- reactive({
      validate(
        if(input$selectGESDAll == FALSE){
          need(input$groupe_espece_strDia != "", "Vueillez selectionner un groupe d'espèces")
        } 
      )
      shinyjs::hide('plot_SCD')
      shinyjs::show('boxloader_SCD')
      load(paste0(DataFolder, "\\data\\ResultTestMB.RData"))
      if(input$selectGESDAll == FALSE){
        Groups=list(stand=as.factor(input$groupe_espece_strDia))
      }else{
        Groups = list(stand=as.factor(MBaikiFormatted$TraitData$Id.sp))
      }
      p <- ggplot()
      ######Function PlotSCD renvoi le graphe de la structure diametrique cummulée contenant aussi ses données###########
      p <- PlotSCD(ResultTestMB, Groups = Groups)
      DataToExportSCD = data.frame()
      PlotToExportSCD = p
      plotbuild <- ggplot_build(p)
      if(length(plotbuild$data) >= 1 && nrow(plotbuild$data[[1]]) != 0){
        data_class1 <- plotbuild$data[[1]]
        data_class1 <- data.table(Temps=data_class1$x, `Cumul de la classe 1`=data_class1$y, key = "Temps")
        DataToExportTmp <- data_class1
        data_class2 <- plotbuild$data[[2]]
        data_class2 <- data.table(Temps=data_class2$x, `Cumul de la classe 2`=data_class2$y, key = "Temps")
        DataToExportTmp <- merge(DataToExportTmp, data_class2, all.x=T, all.y=T)
        data_class3 <- plotbuild$data[[3]]
        data_class3 <- data.table(Temps=data_class3$x, `Cumul de la classe 3`=data_class3$y, key = "Temps")
        DataToExportTmp <- merge(DataToExportTmp, data_class3, all.x=T, all.y=T)
        data_class4 <- plotbuild$data[[4]]
        data_class4 <- data.table(Temps=data_class4$x, `Cumul de la classe 4`=data_class4$y, key = "Temps")
        DataToExportTmp <- merge(DataToExportTmp, data_class4, all.x=T, all.y=T)
        data_class5 <- plotbuild$data[[5]]
        data_class5 <- data.table(Temps=data_class5$x, `Cumul de la classe 5`=data_class5$y, key = "Temps")
        DataToExportTmp <- merge(DataToExportTmp, data_class5, all.x=T, all.y=T)
        data_class6 <- plotbuild$data[[6]]
        data_class6 <- data.table(Temps=data_class6$x, `Cumul de la classe 6`=data_class6$y, key = "Temps")
        DataToExportTmp <- merge(DataToExportTmp, data_class6, all.x=T, all.y=T)
        data_class7 <- plotbuild$data[[7]]
        data_class7 <- data.table(Temps=data_class7$x, `Cumul de la classe 7`=data_class7$y, key = "Temps")
        DataToExportTmp <- merge(DataToExportTmp, data_class7, all.x=T, all.y=T)
        data_class8 <- plotbuild$data[[8]]
        data_class8 <- data.table(Temps=data_class8$x, `Cumul de la classe 8`=data_class8$y, key = "Temps")
        DataToExportTmp <- merge(DataToExportTmp, data_class8, all.x=T, all.y=T)
        DataToExportSCD = DataToExportTmp
        PlotToExportSCD = p
      }
      output$plot_strDia <- renderPlot({
        print(p)
      })
      output$data_strDia <- DT::renderDataTable(DataToExportSCD)
      save(DataToExportSCD, file = paste0(DataFolder, "\\data\\DataExportSCD.RData"))
      save(PlotToExportSCD, file = paste0(DataFolder, "\\data\\PlotToExportSCD.RData"))
      rm(ResultTestMB)
    })
    
  }else{
    execute_plotSCD <- reactive({
      validate(
        if(input$selectGESDAll == FALSE){
          need(input$groupe_espece_strDia != "", "Vueillez selectionner un groupe d'espèces")
        }
      )
      shinyjs::hide('plot_SCD')
      shinyjs::show('boxloader_SCD')
      load(paste0(DataFolder, "\\data\\ResultTestMB.RData"))
      load(paste0(DataFolder, "\\data\\lastTimeSimulate.RData"))
      StartingDate = StoreTime[1]
      if(input$selectGESDAll == FALSE){
        Groups=list(stand=as.factor(input$groupe_espece_strDia))
      }else{
        Groups = list(stand=as.factor(MBaikiFormatted$TraitData$Id.sp))
      } 
      myYear = input$yearslider_strDiam
      ######Function GetTabSD renvoie  la matrix des données de la structure diametrique contenu dans le fichier MyAppInterfaceWithSimulator.R###########
      tabEffs <- GetTabSD(ResultTestMB, Groups = Groups, MyDate = myYear)
      #year_plot= myYear-StartingDate +1
      StrEval = "ClassDiamPlot = c("
      paramInferFile= "ParamInferenceMBaiki.R"
      source(paramInferFile,local = T)
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
      rm(ResultTestMB)
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
  }, error=function(e){
    shinyjs::info(e)
    shinyjs::hide('boxloader_SCD')
    shinyjs::show('plot_SCD')
  })
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

observeEvent(input$whatSD,{
  if(input$whatSD == 1){
    shinyjs::hide('slider_SD')
  }else if(input$whatSD == 2){
    shinyjs::show('slider_SD')
  }
})

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
  tryCatch({
    validate(
      if(input$selectGEIAll == FALSE) need(input$groupe_espece_indicateur != "", "Vueillez selectionner un groupe d espèce"),
      if(input$plageClass == 1){
        need(input$diamMin <= input$diamMax, "le diamètre min ne peut pas être supérieur au diamètre max")
      }else if(input$plageClass == 2){
        need(input$classMin <= input$classMax, "la classe min ne peut pas être supérieure à la classe max")
      } 
    )
    shinyjs::hide('afficher_indicateur')
    shinyjs::show('boxloader_IND')
    load(paste0(DataFolder, "\\data\\ResultTestMB.RData"))
    #load("data/FileIndicateur.RData")
    i= 1
    Indicator = ListOfIndicators[[i]]
    while (Indicator$NomInd != input$indicateur) {
      i= i+1
      Indicator = ListOfIndicators[[i]]
    }
    if(input$selectGEIAll == FALSE){
      Groups=list(stand=as.factor(input$groupe_espece_indicateur))
    }else{
      Groups = list(stand=as.factor(MBaikiFormatted$TraitData$Id.sp))
    } 
    p <- ggplot()
    if(Indicator$NomInd == "Volume exploitable" || Indicator$NomInd == "Stock exploitable"){
      paramInferFile= "ParamInferenceMBaiki.R"
      source(paramInferFile,local = T)
      ClassesDiamLikeNumber = as.numeric(ResultTestMB$ParamPlot$CDSTB$ClassesDiam)
      ClasseDiamDME = findInterval(ResultTestMB$SpeciesTraits$DME[1], ClassesDiam)
      MyClassesDiam = as.factor(ClassesDiamLikeNumber[ClassesDiamLikeNumber>=ClasseDiamDME])
    }else{
      if(input$plageClass == 1){
        MyClassesDiam = as.factor(c(findInterval(input$diamMin, ClassesDiam):findInterval(input$diamMax, ClassesDiam)))
      }else if(input$plageClass == 2){
        MyClassesDiam = as.factor(c(input$classMin:input$classMax))
      }
    }
    ######Fonction PlotSCD renvoie le graphe de l'indicateur contenant aussi ses données###########
    p <- PlotMyIndicator(ResultTestMB, Groups = Groups, MyClassesDiam = MyClassesDiam, MyTimeInterval = MyTimeInterval, MyIndicator = Indicator)
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
    rm(ResultTestMB)
    shinyjs::hide('boxloader_IND')
    shinyjs::show('afficher_indicateur')
    shinyjs::show('bloc_export_indicateur_rg')
    shinyjs::show('bloc_export_indicateur_data')
    print("End plot Indicateur")
    }, error=function(e){
      shinyjs::info(e)
      shinyjs::hide('boxloader_IND')
      shinyjs::show('afficher_indicateur')
  }) 
  
}


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
######Mise à jour des inputs sur l'interface graphique du fichier ui.R###########
updateSelectizeInput(session, "parcelle",  choices = levels(MBaikiFormatted$SimulatingData$Id.zone),
                     selected = NULL, server = TRUE)
updateSelectizeInput(session, "groupe_espece_indicateur", choices = MBaikiFormatted$TraitData$Id.sp,
                     selected = NULL,  server = TRUE)
updateSelectizeInput(session, "groupe_espece_strDia", choices = MBaikiFormatted$TraitData$Id.sp,
                     selected = NULL,  server = TRUE)
choicesParcelles = levels(MBaikiFormatted$SimulatingData$Id.zone)

if(file.exists(paste0(DataFolder, "\\data\\lastTimeSimulate.RData")) && file.access(names = paste0(DataFolder, "\\data\\lastTimeSimulate.RData"), mode = 4)==0){
  load(paste0(DataFolder, "\\data\\lastTimeSimulate.RData"))
  updateSliderInput(session, "yearrange_indicateur", value = c(StoreTime[1], StoreTime[length(StoreTime)]),
                    min = StoreTime[1], max = StoreTime[length(StoreTime)], step = 1)
}else{
  updateSliderInput(session, "yearrange_indicateur", value = as.numeric(levels(MBaikiFormatted$SimulatingData$Id.campagne)[1]),
                    min = as.numeric(levels(MBaikiFormatted$SimulatingData$Id.campagne)[1]), max = as.numeric(levels(MBaikiFormatted$SimulatingData$Id.campagne)[length(levels(MBaikiFormatted$SimulatingData$Id.campagne))]), step = 1)
}

if(file.exists(paste0(DataFolder, "\\data\\lastTimeSimulate.RData")) && file.access(names = paste0(DataFolder, "\\data\\lastTimeSimulate.RData"), mode = 4)==0){
  load(paste0(DataFolder, "\\data\\lastTimeSimulate.RData"))
  updateSliderInput(session, "yearslider_strDiam", value = StoreTime[1], min = StoreTime[1], max = StoreTime[length(StoreTime)], step = 1)
}else{
  updateSliderInput(session, "yearslider_strDiam", value = as.numeric(levels(MBaikiFormatted$SimulatingData$Id.campagne)[1]),
                    min = as.numeric(levels(MBaikiFormatted$SimulatingData$Id.campagne)[1]), max = as.numeric(levels(MBaikiFormatted$SimulatingData$Id.campagne)[length(levels(MBaikiFormatted$SimulatingData$Id.campagne))]), step = 1)
}
alphaInd = MBaikiFormatted$alpha
updateNumericInput(session, "diamMin", value = 0,
                   min = 0, max = ClassesDiam[NbClasse], step = 1)
updateNumericInput(session, "diamMax", value = ClassesDiam[NbClasse],
                   min = 0, max = ClassesDiam[NbClasse], step = 1)
updateNumericInput(session, "classMin", value = 1,
                   min = 1, max = NbClasse, step = 1)
updateNumericInput(session, "classMax", value = NbClasse,
                   min = 1, max = NbClasse, step = 1)

updateSelectIndicator()
loadUpdatedIndicator()
})
  

