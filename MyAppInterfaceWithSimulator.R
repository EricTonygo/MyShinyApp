FCM<-function(out.InferFCM=NULL,ParamSim=NULL){
  
  # VR 14-11-17
  # out.InferFCM : list of the sepcies groups for each forest dynamic processes, produced by the function InferFCM.r
  # ParamSim : R list object containning the set of parameters necessary to run FCM
  
  # The function swicht between FCM for sentier and FCM for parcelle
  
  if (!is.null(out.InferFCM$DataType) &&  out.InferFCM$DataType=="parcelle") out.FCM=FCMparcelle(out.InferFCM,ParamSim)
  else out.FCM=FCMsentier(out.InferFCM,ParamSim)
  
  
  return(out.FCM)
  
}

FCMparcelle <-function(out.InferFCM=NULL, ParamSim = NULL){
  # VR 27-03-18
  
  
  # out.InferFCM : list of the sepcies groups for each forest dynamic processes, produced by the function InferFCM.r
  # PosteriorParam : list of list of the matrix of the posterior distribution of the models parameters, produced by the function ParamPosteriorFlexmixAML
  # Starting.plot : vector of the plots from which the effectives must be used as strating point for the simulations
  # Exploitation : logging treatment : "T0","T1","T2" or "T3" or custom
  # Verif : boolean, if True the Data$Data1YearPost is used in the simulations after the first logging
  
  Starting.plot = ParamSim$Starting.plot
  StartingDate = ParamSim$StartingDate
  MySpeciesTraits = ParamSim$MySpeciesTraits
  MySpeciesTraits$Nom.sp = as.factor(MySpeciesTraits$Nom.sp)
  MySpeciesTraits$Id.sp = as.factor(MySpeciesTraits$Id.sp)
  Logging = ParamSim$Logging
  Fin.simu = ParamSim$Fin.simu
  agggregation = ParamSim$Aggregation
  Surface=ParamSim$Surface.zone
  
  Simulations=NULL
  SimData = NULL
  
  
  
  if (!is.vector(Starting.plot)){
    SimDataFile=NULL
    DataStart=Starting.plot
    Simulations=FCMplot(ParamSim,DataStart,MySpeciesTraits,out.InferFCM,LoggingFunction=Logging,Surface)
    Simulations$Id.zone=as.factor(1)
  }   
  else{ 
    NbPlot=length(Starting.plot)
    NbClasse=length(out.InferFCM$ClassesDiam)
    # Recuperation des simulations par parcelles
    
    if(agggregation){
      if(!is.null(ParamSim$ParamsSimSite)){
        SimData = ParamSim$ParamsSimSite
      }
      DataTmp=subset(SimData,Id.zone%in%Starting.plot&Id.campagne==StartingDate)
      DataTmp=merge(DataTmp,subset(MySpeciesTraits,select=c("Id.sp","Nom.sp")),by="Nom.sp",all.x=T,all.y=F)
      if (sum(is.na(DataTmp$Id.sp))>0) { 
        stop("certaines espÃ¨ces ne sont pas identifiÃ©es")}
      DataDepart=subset(DataTmp,select=c(ClassesDiam,Nom.sp))
      Effectifs=tapply(DataDepart$ClassesDiam,DataDepart$Nom.sp,function(x) as.numeric(table(x)),simplify=F)
      DataStart=sapply(Effectifs,function(x) if(is.null(x)) rep(0,NbClasse) else x)
      Surface=0
      for (j in 1:NbPlot){
        DataTmp=subset(SimData,Id.zone==Starting.plot[j]&Id.campagne==StartingDate)
        Surface=Surface+DataTmp$Surface.zone[1]
      }
      
      outTmp=FCMplot(ParamSim,DataStart,MySpeciesTraits,out.InferFCM,LoggingFunction=Logging,Surface)
      outTmp$Id.zone=paste0(Starting.plot,collapse="-")
      Simulations=outTmp
      
    }else{
      Surface=0
      for (j in 1:NbPlot){
        
        # generation of the starting data
        if(!is.null(ParamSim$ParamsSimSite)){
          SimData = ParamSim$ParamsSimSite
        }
        DataTmp=subset(SimData,Id.zone==Starting.plot[j]&Id.campagne==StartingDate)
        DataDepart=subset(DataTmp,select=c(ClassesDiam,Nom.sp))
        Effectifs=tapply(DataDepart$ClassesDiam,DataDepart$Nom.sp,function(x) as.numeric(table(x)),simplify=F)
        DataStart=sapply(Effectifs,function(x) if(is.null(x)) rep(0,NbClasse) else x)
        outTmp=FCMplot(ParamSim,DataStart,MySpeciesTraits,out.InferFCM,LoggingFunction=Logging,DataTmp$Surface.zone[1])
        outTmp$Id.zone=Starting.plot[j]
        Simulations=rbind(Simulations,outTmp)
        Surface=Surface+DataTmp$Surface.zone[1]
      }
      Surface=Surface/NbPlot
      Simulations$Id.zone=as.factor(Simulations$Id.zone)   
    }
  }
  out.FCM=list(Simulations=Simulations,DataVerif=SimData, SpeciesTraits=MySpeciesTraits,Nb.period=out.InferFCM$Nb.period,agggregation=agggregation,Starting.plot=Starting.plot,ClassesDiam=out.InferFCM$ClassesDiam,Surface=Surface,StartingDate=StartingDate, ParamSim=ParamSim, DataType="parcelle")
  class(out.FCM)<-"FCM"
  
  return(out.FCM)
  
}


FCMsentier<-function(out.InferFCM1Sp,ParamSim){
  
  #VR 07-10-17
  
  # Stand structure variables
  ClassesDiam=out.InferFCM1Sp$ClassesDiam
  NbClasse=length(ClassesDiam)  
  
  
  # Load logging parameters
  #source(ParamSimFile,local=T)
  
  
  ###################################
  # Parameters for the simulation   #
  ###################################
  
  #Date de debut simulation
  StartingDate = ParamSim$StartingDate
  
  # Effectifs de départ
  Effectifs =ParamSim$Effectifs
  
  # Essence
  essence=ParamSim$Essence
  
  # number of simulations
  nbchain=ParamSim$nbchain
  
  # Number of census between the loggings 
  rotation=ParamSim$rotation
  
  # Number of loggs
  Nb.rotation=ParamSim$Nb.rotation
  
  # Number of period before the first loggings
  DelayLogging=ParamSim$DelayLogging
  
  # Number of period of disturbance of the dynamics after logging
  DelayAfterLogging=ParamSim$DelayAfterLogging
  
  #Durée de la simulation 
  Fin.simu = ParamSim$Fin.simu
  
  # Number of period of the "boosted" dynamic after logging
  DelayDynamicsPostExploitation=ParamSim$DelayDynamicsPostExploitation

  # Recruitement
  
  ClasseDiamWeights=ParamSim$ClasseDiamWeights
  RecruitmentRate=ParamSim$RecruitmentRate
  
  # Treatment
  
  Logging.dammage=ParamSim$vector_damage
  Logging.intensity=ParamSim$Logging.intensity
  
  
  
  if (is.null(essence)) essence=names(out.InferFCM1Sp$ParaDyn)[1]
  Id.essence=which((names(out.InferFCM1Sp$ParaDyn))==essence)
  ParamDyn=out.InferFCM1Sp$ParaDyn[[Id.essence]]
  
  #Fin.simu=rotation*Nb.rotation
  
  RecEffectifs=matrix(0,ncol=4,nrow=nbchain*NbClasse*(Fin.simu+1)+NbClasse)  
  RecEffectifs[1:NbClasse,]=cbind(Effectifs,1:NbClasse,0,0)
  compteur=NbClasse+1
  
  Survival.Logging=(1-Logging.dammage)*(1-Logging.intensity)  
  Recruit.intensity=ClasseDiamWeights*RecruitmentRate/sum(ClasseDiamWeights)
  
  
  if(ParamSim$SimulateLogging){
    Exploitations=(0:Nb.rotation)*rotation+DelayLogging
    
    #Fin.simu=Exploitations[length(Exploitations)]
    
    Exploitations=Exploitations[-length(Exploitations)]
  }else{
    Exploitations = -1
  }
  
  # boucles des simulations
  for (k in 1:nbchain){
    
    
    
    Eff.cur=Effectifs
    DAL=DelayAfterLogging
    DDPE=DelayDynamicsPostExploitation
    for (j in 0:Fin.simu){
      DAL=DAL-1
      DDPE=DDPE-1
      # Mortality
      Mort=rbinom(NbClasse,Eff.cur,prob = ParamDyn[,2+(DDPE>0)*2])  
      
      
      # Recrutement + growth
      
      if (DAL<0){
        In=rpois(1,sum(Eff.cur*Recruit.intensity))
        Monte=rbinom(NbClasse,Eff.cur-Mort,prob = ParamDyn[,1+(DDPE>0)*2])
        
        # Update Eff.cur
        
        M1=c(In,Monte[-NbClasse])
        M2=Monte+Mort
        Eff.cur=Eff.cur+M1-M2    
      }else{
        Eff.cur=Eff.cur-Mort
      }
      
      
      
      
      
      # Exploitations
      
      
      if (j%in%Exploitations){
          DAL=DelayAfterLogging
          DDPE=DelayDynamicsPostExploitation+DelayAfterLogging
          Eff.cur=rbinom(NbClasse,Eff.cur,Survival.Logging)
      }
      
      
      # Stockage des Trajectoires
      
      RecEffectifs[compteur:(compteur+NbClasse-1),]=cbind(Eff.cur,1:NbClasse,j+1,k)
      compteur=compteur+NbClasse
      
      
      
      
      
    }  # Fin boucle du j, trajectoire d'effectifs
    
    
    
  } # Fin boucle du k, r?p?tition MC

  
  colnames(RecEffectifs)=c("Effectifs","ClassesDiam","Temps","iter")
  RecEffectifs=as.data.frame(RecEffectifs,stringsAsFactors = F)
  RecEffectifs$Effectifs=as.numeric(RecEffectifs$Effectifs)
  RecEffectifs$ClassesDiam=as.factor(RecEffectifs$ClassesDiam)
  RecEffectifs$Temps=as.integer(RecEffectifs$Temps)
  RecEffectifs$iter=as.integer(RecEffectifs$iter)
  RecEffectifs$Nom.sp=as.factor(essence)
  RecEffectifs$Id.sp=as.factor(essence)
  RecEffectifs$Id.zone=as.factor(1)
  
  out.FCM=list(Simulations=RecEffectifs,SpeciesTraits=out.InferFCM1Sp$SpeciesTraits,ParamPlot=out.InferFCM1Sp$ParamPlot,StartingDate=StartingDate, DataType="sentier")
  class(out.FCM)<-"FCM"
  return(out.FCM)
  
  
  
}
###############Extract Dynamique Mortality, Recruitment, Growth data for simulating species################
ExtractDynDataForSimulation <- function(SimData, Eff.curr){
  if(!is.null(SimData) && "GpRecrut"%in%names(SimData) && !is.null(Eff.curr)){
    SimData$GpRecrut = subset(SimData$GpRecrut, SimData$GpRecrut$Id.sp%in%as.factor(colnames(Eff.curr)))
    SimData$GpRecrut$Id.sp = as.character(SimData$GpRecrut$Id.sp)
    SimData$GpRecrut$Id.sp = as.factor(SimData$GpRecrut$Id.sp)
  }
  if(!is.null(SimData) && !is.null(Eff.curr)){
    SimData$Gp = subset(SimData$Gp, SimData$Gp$Id.sp%in%as.factor(colnames(Eff.curr)))
    SimData$Gp$Id.sp = as.character(SimData$Gp$Id.sp)
    SimData$Gp$Id.sp = as.factor(SimData$Gp$Id.sp)
  }
  return(SimData)
}

##################################################################


FCMplot<-function(ParamSim,Effectifs,SpeciesTraits,out.InferFCM,LoggingFunction,Surface){
  
  # VR 09-01-14
  
  SimRecrut=out.InferFCM$SimRecrut                                     
  SimGrowth=out.InferFCM$SimGrowth
  SimMort=out.InferFCM$SimMort
  
  # Load logging parameters
  #source(ParamSimFile,local=T)
  Nb.rotation = ParamSim$Nb.rotation
  rotation = ParamSim$rotation
  DelayLogging = ParamSim$DelayLogging
  nbchain = ParamSim$nbchain
  vector_damage = ParamSim$vector_damage
  Fin.simu = ParamSim$Fin.simu  
  
  # Number of period of disturbance of the dynamics after logging
  DelayAfterLogging=ParamSim$DelayAfterLogging
  
  # List  of species 
  ListeNomsp=colnames(Effectifs)
  NbIdVern=length(ListeNomsp)
  ListeNomspLogging=SpeciesTraits$Nom.sp
  # Stand structure variables
  ClassesDiam=out.InferFCM$ClassesDiam
  NbClasse=nrow(Effectifs)
  
  # Variables utiles pour fabriquer le vecteur de sortie
  RepListeNomsp=rep(ListeNomsp,each=NbClasse)
  NbObsParIter=NbIdVern*NbClasse
  
  RepClasseDiam=rep(1:NbClasse,NbIdVern)
  
  if (!ParamSim$SimulateLogging){
    Exploitations=-1
  }else{
    Exploitations=(0:Nb.rotation)*rotation+DelayLogging
    
    #Fin.simu=Exploitations[length(Exploitations)]
    
    Exploitations=Exploitations[-length(Exploitations)]
    # Buiding logging function  
    
    source("R/LoggingFunctions.r",local=T)
    LoggingFunction=get(LoggingFunction)
    
    attr(LoggingFunction,"intensity")=SpeciesTraits$tauxPrelevement
    attr(LoggingFunction,"Logging.Damage")= vector_damage
    attr(LoggingFunction,"DelayAfterLogging")=1
    
    SpeciesTraits=subset(SpeciesTraits,Nom.sp%in%ListeNomsp)
    stock.matrix=matrix(0,ncol=NbIdVern,nrow=NbClasse)
    for (i in 1:NbIdVern){
      stock.matrix[SpeciesTraits$DME[i]<=ClassesDiam,i]=1*SpeciesTraits$tauxPrelevement[i]
      test = sum(SpeciesTraits$DME[i]==ClassesDiam)==0 & SpeciesTraits$DME[i]<max(ClassesDiam)
      if (!is.null(test) && !is.na(test) && is.logical(test) && test){
        cl.inf=sum(ClassesDiam<SpeciesTraits$DME[i])
        stock.matrix[cl.inf,i]=(SpeciesTraits$DME[i]-ClassesDiam[cl.inf])*SpeciesTraits$tauxPrelevement[i]/(ClassesDiam[cl.inf+1]-ClassesDiam[cl.inf])
      }
    }
    intensity.matrix=1-stock.matrix
    Logging.Damage=attr(LoggingFunction,"Logging.Damage")
    DelayAfterLogging=attr(LoggingFunction,"DelayAfterLogging")
    
    if (length(Logging.Damage)!=NbClasse){
      stop("the logging damage vector does not match with the number of diameter classes")
    }
    #########################################################
    
    
    for (j in 1:NbClasse){
      intensity.matrix[j,]=intensity.matrix[j,]*(1-Logging.Damage[j])
    }
    
  }
  
  
  
  
  #Exploitation=LoggingFunctions(Exploitation,ListeNomsp)
  
  # Initialisation variables de sortie
  
  RecEffectifsTmp=matrix(0,ncol=5,nrow=NbObsParIter*(Fin.simu+1))
  
  
  RecEffectifs=cbind(as.vector(Effectifs),RepListeNomsp,RepClasseDiam,0,0)
  RecEffectifs=RecEffectifs[RecEffectifs[,1]>0,]
  
  
  RecruitmentModel=SimRecrut$Func
  MortalityModel=SimMort$Func
  GrowthModel=SimGrowth$Func
  
  NomIdSp=data.frame(Nom.sp=colnames(Effectifs))
  NomIdSp=merge(NomIdSp,subset(SpeciesTraits,select=c(Nom.sp,Id.sp)),all.x=T,all.y=T,sort=F)
  colnames(Effectifs)=NomIdSp$Id.sp
  ############# Extract Dynamique Data for mortality, recruitment, growth ###################
  SimRecrut=ExtractDynDataForSimulation(SimRecrut, Effectifs)                                     
  SimGrowth=ExtractDynDataForSimulation(SimGrowth, Effectifs)
  SimMort=ExtractDynDataForSimulation(SimMort, Effectifs)
  #start_time <- Sys.time()
  # boucles des simulation
  for (k in 1:nbchain){
    
    Eff.cur=Effectifs
    compteur=1:NbObsParIter
    DAL=DelayAfterLogging
    
    for (j in 0:Fin.simu){
      
      DAL=DAL-1
      
      # Mortality
      Mort=MortalityModel(Eff.cur,SimMort,Surface)  

      # Recrutement + growth
      if (DAL<0){
        In=RecruitmentModel(Eff.cur,SimRecrut,Surface)
        Monte=GrowthModel(Eff.cur,Mort,SimGrowth,Surface)
        
        # Update Eff.cur
        
        M1=rbind(In,Monte)
        M2=rbind(Monte,0)+Mort
        Eff.cur=Eff.cur+M1-M2
      }else{
        Eff.cur=Eff.cur-Mort
      }

      # Exploitations
      
      
      if (j%in%Exploitations){ 
        DAL=DelayAfterLogging
        Eff.cur=LoggingFunction(Eff.cur,intensity.matrix)
        colnames(Eff.cur)=colnames(Effectifs)
      }
      
      # Stockage des Trajectoires
      
      RecEffectifsTmp[compteur,]=cbind(as.vector(Eff.cur),RepListeNomsp,RepClasseDiam,j+1,k)
      compteur=compteur+NbObsParIter

      
    }  # Fin boucle du j, trajectoire d'effectifs
    
    RecEffectifs=rbind(RecEffectifs,RecEffectifsTmp[RecEffectifsTmp[,1]>0,])
    
  } # Fin boucle du k, r?p?tition MC
  #end_time <- Sys.time()
  #print(end_time - start_time)

  
  
  colnames(RecEffectifs)=c("Effectifs","Nom.sp","ClasseDiam","Temps","iter")
  RecEffectifs=as.data.frame(RecEffectifs,stringsAsFactors = F)
  RecEffectifs$Effectifs=as.numeric(RecEffectifs$Effectifs)
  RecEffectifs$Nom.sp=as.factor(RecEffectifs$Nom.sp)
  RecEffectifs$ClassesDiam=as.factor(RecEffectifs$ClasseDiam)
  RecEffectifs$Temps=as.integer(RecEffectifs$Temps)
  RecEffectifs$iter=as.integer(RecEffectifs$iter)
  
  
  return(RecEffectifs)
} # Fin de la fonction FCMgp


PlotSCD <-function(out.FCM,Outputs=NULL,Groups=NULL, Lab.period="years"){
  #########################################
  # Fonction qui renvoie les données de la structure diamétrique des groupes d'espèce #
  #########################################
  
  #######################
  #
  # out.FCM : data.frame produced by FCM.r function
  # Outputs : list of functions to compute on the simulations
  # Groups : list of vectors of Nom.sp to cluster for plotting
  # Lab.period : Label of the period
  # Nb.period : Number of period between the census
  # StratingDate : initial date for the simulations
  # Surface: Surface of the plot in ha 
  #
  ######################
  StartingDate=out.FCM$StartingDate
  Simulations=data.table(out.FCM$Simulations,key="Nom.sp")
  DataOutputs=data.table(out.FCM$ParamPlot$CDSTB)
  setkeyv(DataOutputs, c("Nom.sp", "ClassesDiam"))
  NbClasse=length(levels(DataOutputs$ClassesDiam))
  DataTraits=data.table(out.FCM$SpeciesTraits,key="Nom.sp")
  DataTraits$Nom.sp=as.factor(DataTraits$Nom.sp)
  if(!is.null(out.FCM$DataType) && out.FCM$DataType=="sentier"){
    Lab.period=out.FCM$ParamPlot$Lab.period
    Nb.period=out.FCM$ParamPlot$Nb.period
    Surface=out.FCM$ParamPlot$Surface
  }else if(!is.null(out.FCM$DataType) && out.FCM$DataType=="parcelle"){
    Nb.period=out.FCM$Nb.period
    Surface=out.FCM$Surface
  }
  
  
  
  # rescaling parameters at 1ha
  
  DataOutputs[,Eff:=Eff/Surface]
  
  Simulations=merge(Simulations,DataTraits,all.x=F,all.y=F)
  Simulations=merge(Simulations,DataOutputs,by=c("Nom.sp", "ClassesDiam"),all.x=F,all.y=F)
  
  
  Simulations[,Efft:=Eff*Effectifs]
  Simulations[,Temps:=Temps*Nb.period]
  
  
  Simulations[,Temps:=Temps+as.numeric(StartingDate)]
  
  Temps=seq(as.numeric(StartingDate),max(Simulations$Temps),Nb.period)
  
  if (is.null(Outputs)) OutPuts=c(1,2)
  if (is.null(Groups)) Groups=list(stand=levels(Simulations$Nom.sp))
  
  Simulations$ClassesDiam=as.factor(Simulations$ClassesDiam)
  Myggplot = ggplot()
  if(length(Groups$stand) > 5){
    names_species_title = paste(paste(as.character(Groups$stand)[1:5], collapse = ", "), ", ...")
  }else{
    names_species_title = paste(as.character(Groups$stand), collapse = ", ")
  }
  
  Mytitle=paste("Prediction de la proportion des arbres par classe de diametre pour les especes: ",names_species_title)
  
  for (g in 1:length(Groups)){
    
    SimulationTmp=subset(Simulations,Nom.sp%in%Groups[[g]])
    
    if(nrow(SimulationTmp) != 0){
      Indicateurs=SimulationTmp[,list(Effs=sum(Efft)),by="Id.zone,iter,Temps"]
      Indicateurs=Indicateurs[order(Indicateurs$Temps),]
      
      # Structure diamétriques
      
      IndicateursCD=SimulationTmp[,list(Effs=sum(Efft)),by="Id.zone,iter,Temps,ClassesDiam"]
      
      cumul=matrix(0,ncol=length(Temps),nrow=NbClasse)
      
      for (cd in 1:NbClasse){
        IndicateursCDTmp=subset(IndicateursCD,ClassesDiam==levels(IndicateursCD$ClassesDiam)[cd])
        IndicateursCDTmp=IndicateursCDTmp[,list(medEffs=median(Effs)),by="Temps"]
        IndicateursCDTmp=IndicateursCDTmp[order(IndicateursCDTmp$Temps),]
        IndexTemps=Temps%in%IndicateursCDTmp$Temps
        cumul[cd,]=cumul[max(cd-1,1),]
        cumul[cd,IndexTemps]=IndicateursCDTmp$medEffs+cumul[cd,IndexTemps]
      }
      cumul=apply(cumul,2,function(x) 100*x/x[NbClasse])  
      ORD=c(0,101)
      Myggplot <- Myggplot + ylim(ORD)

      for (cd in 1:NbClasse){
        data= data.frame(temps= Temps, cumul= cumul[cd,])
        Myggplot=Myggplot + geom_line(data=data, aes(x=temps, y=cumul, colour=letters[cd]), size=1)
        #plot(Temps,cumul[cd,],xlab=Lab.period,ylab="Proportion of trees by diameter class",t="l",ylim=ORD) 
        #par(new=T)
        
      }
    }
  }
  Myggplot <- Myggplot + xlab(Lab.period)
  Myggplot <- Myggplot + ylab("Proportion of trees by diameter class")
  Myggplot <- Myggplot + theme(legend.position=c(1, 0.95), legend.justification=c(0,1), legend.title=element_blank())
  Myggplot <- Myggplot + ggtitle(Mytitle)
  #abline(h=0)
  return(Myggplot)
}


GetTabSD <-function(out.FCM,Outputs=NULL,Groups=NULL, MyDate=NULL, Lab.period="years"){
  #########################################
  # Fonction qui renvoie les données de la structure diamétrique des groupes d'espèce #
  #########################################
  
  #######################
  #
  # out.FCM : data.frame produced by FCM.r function
  # Outputs : list of functions to compute on the simulations
  # Groups : list of vectors of Nom.sp to cluster for plotting
  # Lab.period : Label of the period
  # Nb.period : Number of period between the census
  # StratingDate : initial date for the simulations
  # Surface: Surface of the plot in ha 
  #
  ######################
  
  StartingDate=out.FCM$StartingDate
  Simulations=data.table(out.FCM$Simulations,key="Nom.sp")
  DataOutputs=data.table(out.FCM$ParamPlot$CDSTB)
  setkeyv(DataOutputs, c("Nom.sp", "ClassesDiam"))
  NbClasse=length(levels(DataOutputs$ClassesDiam))
  DataTraits=data.table(out.FCM$SpeciesTraits,key="Nom.sp")
  DataTraits$Nom.sp=as.factor(DataTraits$Nom.sp)
  if(!is.null(out.FCM$DataType) && out.FCM$DataType=="sentier"){
    Lab.period=out.FCM$ParamPlot$Lab.period
    Nb.period=out.FCM$ParamPlot$Nb.period
    Surface=out.FCM$ParamPlot$Surface
  }else if(!is.null(out.FCM$DataType) && out.FCM$DataType=="parcelle"){
    Nb.period=out.FCM$Nb.period
    Surface=out.FCM$Surface
  }
  
  
  
  # rescaling parameters at 1ha
  
  DataOutputs[,Eff:=Eff/Surface]
  
  
  Simulations=merge(Simulations,DataTraits,all.x=F,all.y=F)
  Simulations=merge(Simulations,DataOutputs,by=c("Nom.sp", "ClassesDiam"),all.x=F,all.y=F)

  
  Simulations[,Efft:=Eff*Effectifs] 
  Simulations[,Temps:=Temps*Nb.period]
  
  
  Simulations[,Temps:=Temps+as.numeric(StartingDate)]
  if(!is.null(MyDate)){
    Simulations = Simulations[Temps %in% MyDate]
    Temps = MyDate
  }else{
    Temps=seq(as.numeric(StartingDate),max(Simulations$Temps),Nb.period)
  }
  Temps=seq(as.numeric(StartingDate),max(Simulations$Temps),Nb.period)
  
  if (is.null(Outputs)) OutPuts=c(1,2)
  if (is.null(Groups)) Groups=list(stand=levels(Simulations$Nom.sp))
  
  
  Simulations$ClassesDiam=as.factor(Simulations$ClassesDiam)
  tabEffs=matrix(0,ncol=3,nrow=NbClasse)
  for (g in 1:length(Groups)){
    
    SimulationTmp=subset(Simulations,Nom.sp%in%Groups[[g]])
    
    if(nrow(SimulationTmp) != 0){
      Indicateurs=SimulationTmp[,list(Effs=sum(Efft)),by="Id.zone,iter,Temps"]
      
      
      # Structure diamétriques
      
      IndicateursCD=SimulationTmp[,list(Effs=sum(Efft)),by="Id.zone,iter,Temps,ClassesDiam"]
      
      tabEffs=matrix(0,ncol=3,nrow=NbClasse)
      for (cd in 1:NbClasse){
        IndicateursCDTmp=subset(IndicateursCD,ClassesDiam==levels(IndicateursCD$ClassesDiam)[cd])
        IndicateursCDTmp=IndicateursCDTmp[,list(liEffs=quantile(Effs,0.025, na.rm = TRUE), medEffs=median(Effs), lsEffs=quantile(Effs,0.975, na.rm = TRUE)),by="Temps"]
        tabEffs[cd,]=rep(0, 3)
        if(length(IndicateursCDTmp$medEffs)==0){
          tabEffs[cd,1]=tabEffs[cd,1]
          tabEffs[cd,2]=tabEffs[cd,2]
          tabEffs[cd,3]=tabEffs[cd,3]
        }else{
          tabEffs[cd,1]=IndicateursCDTmp$medEffs+tabEffs[cd,1]
          tabEffs[cd,2]=IndicateursCDTmp$liEffs+tabEffs[cd,2]
          tabEffs[cd,3]=IndicateursCDTmp$lsEffs+tabEffs[cd,3]
        }
        
      }
    }
  }
 
  return(tabEffs)
}

ApiDimako <- function (Effectifs,ClassesDiam, Logging.damage, DME, DMA, tauxAccroissement, tauxMortalite, tauxPrelevement, dureerotation){
  NbClasse = length(ClassesDiam)
  # Calcul du param?tre  alpha de la distribution exponentielle des diam?tres
  ClasseDiam2=c(ClassesDiam,Inf)
  PropEffectifsTotaux=Effectifs/sum(Effectifs)
  
  
  SearchAlpha=function(Alpha,NbClasse,ClasseDiam2,PropEffectifsTotaux){
    SS=0
    for (i in 1:NbClasse){
      SS=SS+(integrate(dexp,ClasseDiam2[i],ClasseDiam2[i+1],Alpha)$value-PropEffectifsTotaux[i])^2
    }
    return(SS)
  }
  res=optimize(SearchAlpha,interval=c(0.000001,0.5),NbClasse,ClasseDiam2,PropEffectifsTotaux)
  Alpha=res$minimum

  VectStockInit = rep(0, NbClasse)
  VectStockInit[ClassesDiam >= DME] = 1
  if(!DME%in%ClassesDiam){
    Cl_Inf = sum(ClassesDiam<DME)
    if(Cl_Inf == NbClasse){
      VectStockInit[Cl_Inf] = (1- pexp(DME, Alpha))/(1-pexp(ClassesDiam[Cl_Inf], Alpha))
    }else{
      VectStockInit[Cl_Inf] = (ClassesDiam[Cl_Inf + 1] - DME)/(ClassesDiam[Cl_Inf+1]- ClassesDiam[Cl_Inf])
    }
  }
  
  VectStockFinal = rep(0, NbClasse)
  DMEFinal = DME- tauxAccroissement*dureerotation
  VectStockFinal[ClassesDiam >= DMEFinal] = 1
  if(!DMEFinal%in%ClassesDiam){
    Cl_Inf = sum(ClassesDiam<DMEFinal)
    if(Cl_Inf == NbClasse){
      VectStockFinal[Cl_Inf] = (1- pexp(DMEFinal, Alpha))/(1-pexp(ClassesDiam[Cl_Inf], Alpha))
    }else{
      VectStockFinal[Cl_Inf] = (ClassesDiam[Cl_Inf+1] - DMEFinal)/(ClassesDiam[Cl_Inf+1]- ClassesDiam[Cl_Inf])
    }
  }
  
  VectDegat = 1- Logging.damage
  Mortalite = (1 - tauxMortalite)^dureerotation
  
  VectApresExploitation = rep(1, NbClasse)
  VectApresExploitation[ClassesDiam >= DMA] =1- tauxPrelevement
  if(!DMA%in%ClassesDiam){
    Cl_Inf = sum(ClassesDiam<DMA)
    if(Cl_Inf == NbClasse){
      VectApresExploitation[Cl_Inf] = 1- tauxPrelevement*(1- pexp(DMA, Alpha))/(1-pexp(ClassesDiam[Cl_Inf], Alpha))
    }else{
      VectApresExploitation[Cl_Inf] = 1- tauxPrelevement*(ClassesDiam[Cl_Inf+1] - DMA)/(ClassesDiam[Cl_Inf+1]- ClassesDiam[Cl_Inf])
    }
  }
  StockInit = sum(VectStockInit*Effectifs)
  StockFinal = sum(VectStockFinal*Effectifs*VectApresExploitation*Mortalite*VectDegat)
  RDimako = StockFinal*100/StockInit
  return(round(RDimako, 2))
  
}