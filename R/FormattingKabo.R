FormattingKabo <- function(Data,zone="PLOT",n_essence="vernacular.name"){
  
  # VR 30-06-2015
  

  
  ##############################################################
  ################################################################
  
 
  
  FormatData=NULL
  SimData=NULL
  NomsFormatData=c("Id.zone","Id.sp","Diam0","Diam","Id.campagne")
  NomsSimData=c("Id.zone","Id.sp","Diam","Id.campagne")

  
  GardeTmp1=c(zone,n_essence,"Dia.cm.2000","Dia.cm.2012")
  GardeTmp2=c(zone,n_essence,"Dia.cm.1983","Dia.cm.2000")
  DataTmp1=cbind(subset(Data,select=GardeTmp1),2012)
  DataTmp2=cbind(subset(Data,select=GardeTmp2),2000)
  
  colnames(DataTmp1)=NomsFormatData
  colnames(DataTmp2)=NomsFormatData
  FormatData=rbind(DataTmp1,DataTmp2)
  
  
  SimData=subset(FormatData,select=NomsSimData)
  SimData2=cbind(subset(DataTmp2,select=c("Id.zone","Id.sp","Diam0")),1983)
  colnames(SimData2)=NomsSimData
  SimData=rbind(SimData,SimData2)
  SimData=subset(SimData,!is.na(Diam) & Diam>14.9)
 
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
  
  DataRef=subset(SimData, Id.campagne=="2012")
  alpha=nrow(DataRef)/sum(DataRef$Diam-14.9)
  
  # Wood specific gravity of the species (I set 0.6 for all species)
  TraitData=cbind(unique(subset(Data,select=c(n_essence))),0.6)
  colnames(TraitData)=c("Id.sp","WSG")
  
  # Minimum Logging diameter (I set 55cm for all species)
  TraitData$DME=55

  TraitData=subset(TraitData,Id.sp%in%ListeSp)
  TraitData$Id.sp=TraitData$Id.sp[,drop=T]
  out=list(ClusteringData=FormatData,SimulatingData=SimData,TraitData=TraitData,alpha=alpha)
  
  
  return(out)
  
}
  
  
  