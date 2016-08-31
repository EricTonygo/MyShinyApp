FormattingEcolsilva <- function(Data,zone="n_parcelle"){
  
  # VR 26-03-2015
  
  
  # Traitement :
  #
  # exploitation en 2004 sur les 18 parcelles de 1ha
  
  # on extrait les données à partir de 2006
  
  ##############################################################
  ################################################################
  
  Data$quadra=paste(Data$quadra,Data$n_parcelle,sep="")
  
  FormatData=NULL
  SimData=NULL
  NomsFormatData=c("Id.zone","Id.sp","Diam0","Diam","Id.campagne")
  NomsSimData=c("Id.zone","Id.sp","Diam","Id.campagne")
  NomsCampagnes=c(2004,2006,2008,2010,2012)
  Data$dbh[Data$code_vivant==0]=NA
  GardeTmp=c(zone,"n_essence","i_arbre","dbh")
  
  DataTmp1=subset(Data,select=GardeTmp,campagne==NomsCampagnes[1] & dbh>=19.5)

  
  for (i in 1:length(NomsCampagnes)){
    
    
    DataTmp0=DataTmp1
    colnames(DataTmp0)=c("Id.zone","Id.sp","Id.tree","Diam0")
    DataTmp1=subset(Data,select=GardeTmp,campagne==NomsCampagnes[i] & dbh>=19.5)
    colnames(DataTmp1)=c("Id.zone","Id.sp","Id.tree","Diam")
    
    
    if (i>2){
      DataTmp=merge(DataTmp0,DataTmp1,all.x=T,all.y=T)
      DataTmp=subset(DataTmp,select=c("Id.zone","Id.sp","Diam0","Diam"))
      FormatDataTmp=cbind(DataTmp,NomsCampagnes[i])               
      colnames(FormatDataTmp)=NomsFormatData
      FormatData=rbind(FormatData,FormatDataTmp)
    }

    
    SimDataTmp=cbind(DataTmp1[,c(1,2,4)],NomsCampagnes[i])    
    colnames(SimDataTmp)=NomsSimData  
    SimData=rbind(SimData,SimDataTmp)      
    
  }
  
  FormatData$Id.zone=as.factor(FormatData$Id.zone)
  FormatData$Id.sp=as.factor(FormatData$Id.sp)
  FormatData$Id.campagne=as.factor(FormatData$Id.campagne)
  FormatData$Diam0[is.na(FormatData$Diam0)]=0
  FormatData$Diam[is.na(FormatData$Diam)]=0
  FormatData=subset(FormatData,Diam+Diam0>0)
  FormatData$Id.sp=FormatData$Id.sp[,drop=T]
  ListeSp=levels(FormatData$Id.sp)
  
  SimData$Id.zone=as.factor(SimData$Id.zone)
  SimData$Id.sp=as.factor(SimData$Id.sp)
  SimData$Id.campagne=as.factor(SimData$Id.campagne)
  
  
  SimData=subset(SimData,Id.sp%in%ListeSp)
  SimData$Id.sp=SimData$Id.sp[,drop=T]
  
  DataRef=subset(SimData, Id.campagne=="2004")
  alpha=nrow(DataRef)/sum(DataRef$Diam-20)
  
  
  TraitData=unique(subset(Data,select=c(n_essence,wsg)))
  colnames(TraitData)=c("Id.sp","WSG")
  TraitData$DME=55

  TraitData=subset(TraitData,Id.sp%in%ListeSp)
  
  out=list(ClusteringData=FormatData,SimulatingData=SimData,TraitData=TraitData,alpha=alpha)
  
  
  return(out)
  
}
  
  
  