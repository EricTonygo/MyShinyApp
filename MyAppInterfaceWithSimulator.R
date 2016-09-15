FCM <-function(out.InferFCM=NULL, ParamSim = NULL){
  # VR 11-10-12
  
  
  # out.InferFCM : list of the sepcies groups for each forest dynamic processes, produced by the function InferFCM.r
  # PosteriorParam : list of list of the matrix of the posterior distribution of the models parameters, produced by the function ParamPosteriorFlexmixAML
  # Starting.plot : vector of the plots from which the effectives must be used as strating point for the simulations
  # Exploitation : logging treatment : "T0","T1","T2" or "T3" or custom
  # Verif : boolean, if True the Data$Data1YearPost is used in the simulations after the first logging
  
  Check = ParamSim$Check
  Starting.plot = ParamSim$Starting.plot
  StartingDate = ParamSim$StartingDate
  MySpeciesTraits = ParamSim$MySpeciesTraits
  Logging = ParamSim$Logging
  Date1PostLog.Check = ParamSim$Date1PostLog.Check
  Fin.simu = ParamSim$Fin.simu
  
  #source(ParamSimFile,local=T)
  NbPlot=length(Starting.plot)
  Simulations=NULL
  DataVerif=NULL

  # Recuperation des simulations par parcelles
  
  for (j in 1:NbPlot){
    
    # generation of the starting data
    
    if (is.list(Starting.plot)){
      Data=list()
      Data$DataStart=Starting.plot
      Starting.plot=names(Starting.plot)
      Data$Data1YearPost=NULL
      Check=F
      Data$StartingDate=0
    }else{ 
      #browser()
      ParamStartData=list(StartingDate = StartingDate, Check = Check)
      #browser()
      
      if (Check){
        ParamStartData$Date1PostLog.Check=Date1PostLog.Check
        #browser()
      } 
      #browser()
      Data=StartData(out.InferFCM$SimulatingData,Starting.plot[j],out.InferFCM$ClassesDiam,ParamStartData)
      
    }       
    #InferFCM$SpeciesTraits= MySpeciesTraits
    #param = list(Nb.rotation = Nb.rotation, rotation = rotation, DelayLogging = DelayLogging, nbchain = nbchain, vector_damage = vector_damage)
    #browser()
    outTmp=FCMplot(ParamSim, Data$DataStart,Data$Data1YearPost,MySpeciesTraits,out.InferFCM,c(Starting.plot[j]),Logging,Check,out.InferFCM$ParamPlot$Surface)
    Simulations=rbind(Simulations,outTmp)
    DataVerif=rbind(DataVerif,Data$DataVerif)
  }
  Simulations$Id.zone=as.factor(Simulations$Id.zone)     
  
  
  out.FCM=list(Simulations=Simulations,ParamPlotFCM=Data$DataPlotFCM,DataVerif=DataVerif,SpeciesTraits=MySpeciesTraits,ParamPlot=out.InferFCM$ParamPlot,StartingDate=Data$StartingDate,Check=Check)
  class(out.FCM)<-"FCM"
  
  return(out.FCM)
}

FCMplot<-function(ParamSim, Effectifs,EffectifsPostExpAn1,SpeciesTraits,out.InferFCM,ParcelleDepart,LoggingFunction,Verif,Surface){
  
  # VR 09-01-14
  
  SimRecrut=out.InferFCM$SimRecrut                                     
  SimGrowth=out.InferFCM$SimGrowth
  SimMort=out.InferFCM$SimMort
  
  # Load logging parameters
  #source(ParamSimFile,local=T)  
  #browser()
  Nb.rotation = ParamSim$Nb.rotation
  rotation = ParamSim$rotation
  DelayLogging = ParamSim$DelayLogging
  nbchain = ParamSim$nbchain
  vector_damage = ParamSim$vector_damage
  Fin.simu = ParamSim$Fin.simu
  # List  of species 
  ListeIdsp=colnames(Effectifs)
  NbIdVern=length(ListeIdsp)
  ListeIdspLogging=SpeciesTraits$Id.sp
  #browser()
  
  # Stand structure variables
  ClassesDiam=out.InferFCM$ClassesDiam
  NbClasse=length(ClassesDiam)
  
  # Variables utiles pour fabriquer le vecteur de sortie
  RepListeIdsp=rep(ListeIdsp,each=NbClasse)
  NbObsParIter=NbIdVern*NbClasse
  RepClasseDiam=rep(1:NbClasse,NbIdVern)
  
  
  Exploitations=(0:Nb.rotation)*rotation+DelayLogging
  
  #Fin.simu=Exploitations[length(Exploitations)]
  
  Exploitations=Exploitations[-length(Exploitations)]
  
  
  #Buiding logging function  
  
  source("R/LoggingFunctions.r",local=T)
  LoggingFunction=get(LoggingFunction)
  
  attr(LoggingFunction,"intensity")=SpeciesTraits$tauxPrelevement
  attr(LoggingFunction,"Logging.Damage")= vector_damage
  attr(LoggingFunction,"DelayAfterLogging")=1
  
  SpeciesTraits=subset(SpeciesTraits,Id.sp%in%ListeIdsp)
  #NbIdVernLogging = nrow(SpeciesTraits)
  stock.matrix=matrix(0,ncol=NbIdVern,nrow=NbClasse)
  for (i in 1:NbIdVern){
    stock.matrix[SpeciesTraits$DMA[i]<=ClassesDiam,i]=1
    if (sum(SpeciesTraits$DMA[i]==ClassesDiam)==0 & SpeciesTraits$DMA[i]<max(ClassesDiam)){
      cl.inf=sum(ClassesDiam<SpeciesTraits$DMA[i])
      stock.matrix[cl.inf,i]=(SpeciesTraits$DMA[i]-ClassesDiam[cl.inf])/(ClassesDiam[cl.inf]-ClassesDiam[cl.inf-1])
    }
  }
  
  intensity.matrix=1-stock.matrix*attr(LoggingFunction,"intensity")*SpeciesTraits$coefRecollement
  Logging.Damage=attr(LoggingFunction,"Logging.Damage")
  DelayAfterLogging=attr(LoggingFunction,"DelayAfterLogging")
  
  if (length(Logging.Damage)!=NbClasse){
    stop("the logging damage vector does not match with the number of diameter classes")
  }
  #########################################################
  
  
  for (j in 1:NbClasse){
    intensity.matrix[j,]=intensity.matrix[j,]*(1-Logging.Damage[j])
  }
  
  
  
  
  
  
  #Exploitation=LoggingFunctions(Exploitation,ListeIdsp)
  
  # Initialisation variables de sortie
  
  RecEffectifsTmp=matrix(0,ncol=5,nrow=NbObsParIter*(Fin.simu+1))
  RecEffectifs=cbind(as.vector(Effectifs),RepListeIdsp,RepClasseDiam,0,0)
  RecEffectifs=RecEffectifs[RecEffectifs[,1]>0,]
  
  
  RecruitmentModel=SimRecrut$Func
  MortalityModel=SimMort$Func
  GrowthModel=SimGrowth$Func
  #browser()
  
  # boucles des simulation
  for (k in 1:nbchain){
    
    
    
    Eff.cur=Effectifs
    compteur=1:NbObsParIter
    DAL=0
    
    for (j in 0:Fin.simu){
      
      
      DAL=DAL-1
      
      # Mortality
      Mort=MortalityModel(Eff.cur,SimMort)  
      
      # Recrutement + growth
      
      if (DAL<0){
        In=RecruitmentModel(Eff.cur,SimRecrut)
        Monte=GrowthModel(Eff.cur,Mort,SimGrowth)
        
        # Update Eff.cur
        
        M1=rbind(In,Monte)
        M2=rbind(Monte,0)+Mort
        Eff.cur=Eff.cur+M1-M2    
      }else{
        Eff.cur=Eff.cur-Mort
      }
      
      
      
      
      
      # Exploitations
      
      
      if (j%in%Exploitations){
        if (j==Exploitations[1] & Verif) {Eff.cur=EffectifsPostExpAn1
        DAL=DelayAfterLogging
        }
        else{Eff.cur=LoggingFunction(Eff.cur,intensity.matrix)}
        
      }
      
      # Stockage des Trajectoires
      
      
      RecEffectifsTmp[compteur,]=cbind(as.vector(Eff.cur),RepListeIdsp,RepClasseDiam,j+1,k)
      
      compteur=compteur+NbObsParIter
      
      
      
    }  # Fin boucle du j, trajectoire d'effectifs
    
    RecEffectifs=rbind(RecEffectifs,RecEffectifsTmp[RecEffectifsTmp[,1]>0,])
    
  } # Fin boucle du k, r?p?tition MC
  
  
  
  
  
  colnames(RecEffectifs)=c("Effectifs","Id.sp","ClasseDiam","Temps","iter")
  RecEffectifs=as.data.frame(RecEffectifs,stringsAsFactors = F)
  RecEffectifs$Effectifs=as.numeric(RecEffectifs$Effectifs)
  RecEffectifs$Id.sp=as.factor(RecEffectifs$Id.sp)
  RecEffectifs$ClassesDiam=as.factor(RecEffectifs$ClasseDiam)
  RecEffectifs$Temps=as.integer(RecEffectifs$Temps)
  RecEffectifs$iter=as.integer(RecEffectifs$iter)
  RecEffectifs$Id.zone=ParcelleDepart
  
  
  return(RecEffectifs)
} # Fin de la fonction FCMgp



PlotSCD <-function(out.FCM,Outputs=NULL,Groups=NULL,Verif=NULL){
  #########################################
  # Fonction qui renvoie les données de la structure diamétrique des groupes d'espèce #
  #########################################
  
  #######################
  #
  # out.FCM : data.frame produced by FCM.r function
  # Outputs : list of functions to compute on the simulations
  # Groups : list of vectors of Id.sp to cluster for plotting
  # verif : bolean, T : plot also the real values
  # Lab.period : Label of the period
  # Nb.period : Number of period between the census
  # StratingDate : initial date for the simulations
  # Surface: Surface of the plot in ha 
  #
  ######################
  
  if (is.null(Verif)) Verif=out.FCM$Check
  StartingDate=out.FCM$StartingDate
  Simulations=data.table(out.FCM$Simulations,key="Id.sp")
  DataOutputs=data.table(out.FCM$ParamPlot$CDSTB)
  setkeyv(DataOutputs, c("Id.sp", "ClassesDiam"))
  NbClasse=length(levels(DataOutputs$ClassesDiam))
  DataTraits=data.table(out.FCM$SpeciesTraits,key="Id.sp")
  DataTraits$Id.sp=as.factor(DataTraits$Id.sp)
  Lab.period=out.FCM$ParamPlot$Lab.period
  Nb.period=out.FCM$ParamPlot$Nb.period
  Surface=out.FCM$ParamPlot$Surface
  
  #browser()
  
  # rescaling parameters at 1ha
  
  DataOutputs[,Eff:=Eff/Surface]
  
  #browser()
  Simulations=merge(Simulations,DataTraits,all.x=F,all.y=F)
  Simulations=merge(Simulations,DataOutputs,by=c("Id.sp", "ClassesDiam"),all.x=T,all.y=F)
  
  
  Simulations[,Efft:=Eff*Effectifs]
  Simulations[,Temps:=Temps*Nb.period]
  
  
  if (StartingDate==0 | is.null(out.FCM$DataVerif)) Verif=F
  Simulations[,Temps:=Temps+as.numeric(StartingDate)]
  
  Temps=seq(as.numeric(StartingDate),max(Simulations$Temps),Nb.period)
  
  if (is.null(Outputs)) OutPuts=c(1,2)
  if (is.null(Groups)) Groups=list(stand=levels(Simulations$Id.sp))
  
  if (Verif){
    DataVerif=data.table(out.FCM$DataVerif,key="Id.sp")
    DataVerif=merge(DataVerif,DataTraits,by="Id.sp",all.x=T,all.y=F)
    DataVerif=merge(DataVerif,DataOutputs,by=c("Id.sp", "ClassesDiam"),all.x=T,all.y=F)
    DataVerif[,Efft:=Eff*Effectifs]
    DataVerif[,Temps:=as.numeric(as.character(DataVerif$Id.campagne))]
  }
  
  Simulations$ClasseDiam=as.factor(Simulations$ClasseDiam)
  Myggplot = ggplot()
  Mytitle =""
  
  for (g in 1:length(Groups)){
    
    SimulationTmp=subset(Simulations,Id.sp%in%Groups[[g]])
    
    Indicateurs=SimulationTmp[,list(Effs=sum(Efft)),by="Id.zone,iter,Temps"]
    Indicateurs=Indicateurs[order(Indicateurs$Temps),]
    
    
    # Structure diamétriques
    
    IndicateursCD=SimulationTmp[,list(Effs=sum(Efft)),by="Id.zone,iter,Temps,ClasseDiam"]
    
    cumul=matrix(0,ncol=length(Temps),nrow=NbClasse)
    
    for (cd in 1:NbClasse){
      IndicateursCDTmp=subset(IndicateursCD,ClasseDiam==levels(IndicateursCD$ClasseDiam)[cd])
      IndicateursCDTmp=IndicateursCDTmp[,list(medEffs=median(Effs)),by="Temps"]
      IndicateursCDTmp=IndicateursCDTmp[order(IndicateursCDTmp$Temps),]
      IndexTemps=Temps%in%IndicateursCDTmp$Temps
      cumul[cd,]=cumul[max(cd-1,1),]
      cumul[cd,IndexTemps]=IndicateursCDTmp$medEffs+cumul[cd,IndexTemps]
    }
    cumul=apply(cumul,2,function(x) 100*x/x[NbClasse])  
    ORD=c(0,101)
    Mytitle=paste("Proportion of trees by diameter class for",names(Groups)[g])
    
    for (cd in 1:NbClasse){
      data= data.frame(temps= Temps, cumul= cumul[cd,])
      Myggplot=Myggplot + geom_line(data=data, aes(x=temps, y=cumul, colour=letters[cd]), size=1)
      #plot(Temps,cumul[cd,],xlab=Lab.period,ylab="Proportion of trees by diameter class",t="l",ylim=ORD) 
      #par(new=T)
      
    }
    
  }
  #browser()
  Myggplot <- Myggplot + xlab(Lab.period)
  Myggplot <- Myggplot + ylab("Proportion of trees by diameter class")
  Myggplot <- Myggplot + ylim(ORD)
  Myggplot <- Myggplot + theme(legend.position=c(1, 0.95), legend.justification=c(0,1), legend.title=element_blank())
  Myggplot <- Myggplot + ggtitle(Mytitle)
  #abline(h=0)
  return(Myggplot)
}

GetTabSD <-function(out.FCM,Outputs=NULL,Groups=NULL,Verif=NULL,  MyDate=NULL){
  #########################################
  # Fonction qui renvoie les données de la structure diamétrique des groupes d'espèce #
  #########################################
  
  #######################
  #
  # out.FCM : data.frame produced by FCM.r function
  # Outputs : list of functions to compute on the simulations
  # Groups : list of vectors of Id.sp to cluster for plotting
  # verif : bolean, T : plot also the real values
  # Lab.period : Label of the period
  # Nb.period : Number of period between the census
  # StratingDate : initial date for the simulations
  # Surface: Surface of the plot in ha 
  #
  ######################
  
  if (is.null(Verif)) Verif=out.FCM$Check
  StartingDate=out.FCM$StartingDate
  Simulations=data.table(out.FCM$Simulations,key="Id.sp")
  DataOutputs=data.table(out.FCM$ParamPlot$CDSTB)
  setkeyv(DataOutputs, c("Id.sp", "ClassesDiam"))
  NbClasse=length(levels(DataOutputs$ClassesDiam))
  DataTraits=data.table(out.FCM$SpeciesTraits,key="Id.sp")
  DataTraits$Id.sp=as.factor(DataTraits$Id.sp)
  Lab.period=out.FCM$ParamPlot$Lab.period
  Nb.period=out.FCM$ParamPlot$Nb.period
  Surface=out.FCM$ParamPlot$Surface
  
  #browser()
  
  # rescaling parameters at 1ha
  
  DataOutputs[,Eff:=Eff/Surface]
  
  #browser()
  Simulations=merge(Simulations,DataTraits,all.x=F,all.y=F)
  Simulations=merge(Simulations,DataOutputs,by=c("Id.sp", "ClassesDiam"),all.x=T,all.y=F)

  
  Simulations[,Efft:=Eff*Effectifs] 
  Simulations[,Temps:=Temps*Nb.period]
  
  
  if (StartingDate==0 | is.null(out.FCM$DataVerif)) Verif=F
  Simulations[,Temps:=Temps+as.numeric(StartingDate)]
  if(!is.null(MyDate)){
    Simulations = Simulations[Temps %in% MyDate]
    Temps = MyDate
  }else{
    Temps=seq(as.numeric(StartingDate),max(Simulations$Temps),Nb.period)
  }
  Temps=seq(as.numeric(StartingDate),max(Simulations$Temps),Nb.period)
  
  if (is.null(Outputs)) OutPuts=c(1,2)
  if (is.null(Groups)) Groups=list(stand=levels(Simulations$Id.sp))
  
  
  Simulations$ClasseDiam=as.factor(Simulations$ClasseDiam)
  
  for (g in 1:length(Groups)){
    
    SimulationTmp=subset(Simulations,Id.sp%in%Groups[[g]])
    
    Indicateurs=SimulationTmp[,list(Effs=sum(Efft)),by="Id.zone,iter,Temps"]
    #Indicateurs=Indicateurs[order(Indicateurs$Temps),]
    
    
    # Structure diamétriques
    
    IndicateursCD=SimulationTmp[,list(Effs=sum(Efft)),by="Id.zone,iter,Temps,ClasseDiam"]
    
    tabEffs=matrix(0,ncol=3,nrow=NbClasse)
    for (cd in 1:NbClasse){
      IndicateursCDTmp=subset(IndicateursCD,ClasseDiam==levels(IndicateursCD$ClasseDiam)[cd])
      IndicateursCDTmp=IndicateursCDTmp[,list(liEffs=quantile(Effs,0.025, na.rm = TRUE), medEffs=median(Effs), lsEffs=quantile(Effs,0.975, na.rm = TRUE)),by="Temps"]
      #IndicateursCDTmp=IndicateursCDTmp[order(IndicateursCDTmp$Temps),]
      #IndexTemps=Temps%in%IndicateursCDTmp$Temps
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
 
  return(tabEffs)
}