FCM <-function(out.InferFCM=NULL,ParamSimFile=NULL){
  
  # VR 11-10-12
  
  
  # out.InferFCM : list of the sepcies groups for each forest dynamic processes, produced by the function InferFCM.r
  # PosteriorParam : list of list of the matrix of the posterior distribution of the models parameters, produced by the function ParamPosteriorFlexmixAML
  # Starting.plot : vector of the plots from which the effectives must be used as strating point for the simulations
  # Exploitation : logging treatment : "T0","T1","T2" or "T3" or custom
  # Verif : boolean, if True the Data$Data1YearPost is used in the simulations after the first logging
  
  source(ParamSimFile,local=T)
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
      
      ParamStartData=list(Check=Check,StartingDate=StartingDate)
      if (Check) ParamStartData$Date1PostLog.Check=Date1PostLog.Check
      Data=StartData(out.InferFCM$SimulatingData,ParamSimFile,Starting.plot[j],out.InferFCM$ClassesDiam,ParamStartData)
    }       
    
    outTmp=FCMplot(ParamSimFile,Data$DataStart,Data$Data1YearPost,out.InferFCM$SpeciesTraits,out.InferFCM,c(Starting.plot[j]),LoggingFunction=Logging,Check,out.InferFCM$ParamPlot$Surface)
    Simulations=rbind(Simulations,outTmp)
    DataVerif=rbind(DataVerif,Data$DataVerif)
  }
  Simulations$Id.zone=as.factor(Simulations$Id.zone)     
  
  
  out.FCM=list(Simulations=Simulations,ParamPlotFCM=Data$DataPlotFCM,DataVerif=DataVerif,SpeciesTraits=out.InferFCM$SpeciesTraits,ParamPlot=out.InferFCM$ParamPlot,StartingDate=Data$StartingDate,Check=Check)
  class(out.FCM)<-"FCM"
  
  return(out.FCM)
  
  
}


##################################################################


FCMplot<-function(ParamSimFile,Effectifs,EffectifsPostExpAn1,SpeciesTraits,out.InferFCM,ParcelleDepart,LoggingFunction,Verif,Surface){
  
  # VR 09-01-14
  
  SimRecrut=out.InferFCM$SimRecrut                                     
  SimGrowth=out.InferFCM$SimGrowth
  SimMort=out.InferFCM$SimMort
  
  # Load logging parameters
  source(ParamSimFile,local=T)  
  
  # List  of species 
  ListeIdsp=colnames(Effectifs)
  NbIdVern=length(ListeIdsp)
  
  # Stand structure variables
  ClassesDiam=out.InferFCM$ClassesDiam
  NbClasse=length(ClassesDiam)
  
  # Variables utiles pour fabriquer le vecteur de sortie
  RepListeIdsp=rep(ListeIdsp,each=NbClasse)
  NbObsParIter=NbIdVern*NbClasse
  RepClasseDiam=rep(1:NbClasse,NbIdVern)
  
  
  Exploitations=(0:Nb.rotation)*rotation+DelayLogging
  
  Fin.simu=Exploitations[length(Exploitations)]
  
  Exploitations=Exploitations[-length(Exploitations)]
  
  
  # Buiding logging function  
  
  source("R/LoggingFunctions.r",local=T)
  LoggingFunction=get(LoggingFunction)
  
  SpeciesTraits=subset(SpeciesTraits,Id.sp%in%ListeIdsp)
  stock.matrix=matrix(0,ncol=NbIdVern,nrow=NbClasse)
  
  for (i in 1:NbIdVern){
    stock.matrix[SpeciesTraits$DME[i]<=ClassesDiam,i]=1
    if (sum(SpeciesTraits$DME[i]==ClassesDiam)==0 & SpeciesTraits$DME[i]<max(ClassesDiam)){
      cl.inf=sum(ClassesDiam<SpeciesTraits$DME[i])
      stock.matrix[cl.inf,i]=(SpeciesTraits$DME[i]-ClassesDiam[cl.inf])/(ClassesDiam[cl.inf]-ClassesDiam[cl.inf-1])
    }
  }
  
  intensity.matrix=1-stock.matrix*attr(LoggingFunction,"intensity")
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




###################################################################
# Fonctions internes
###################################################################

