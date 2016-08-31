FormattingMBaiki<-function(Data,TraitData){
  
  # VR 28-02-2013
  
 
  
  Data$Parcelles=Data$Replicat
  NomsCampagnes=colnames(Data)[5:31]
  Campagnes=as.numeric(colnames(Data)[5:31])
  Index=which((Campagnes[2:31]-Campagnes[1:30])==1)+1
  FormatData=NULL
  SimData=NULL
  NomsFormatData=c("Id.zone","Id.sp","Diam0","Diam","Id.campagne")
  NomsSimData=c("Id.zone","Id.sp","Diam","Id.campagne")
  
  for (i in 2:length(NomsCampagnes)){
    
    GardeTmp=c("Parcelles","Vernaculaire",NomsCampagnes[i-1],NomsCampagnes[i])
    DataTmp=subset(Data,select=GardeTmp)
 
    if (i %in% Index){
        
        FormatDataTmp=cbind(DataTmp,NomsCampagnes[i])
        colnames(FormatDataTmp)=NomsFormatData
        FormatData=rbind(FormatData,FormatDataTmp)
        
    }   
    
    SimDataTmp=cbind(DataTmp[,1:3],NomsCampagnes[i-1])    
    colnames(SimDataTmp)=NomsSimData  
    SimData=rbind(SimData,SimDataTmp)      
        
  }

  FormatData$Id.zone=as.factor(FormatData$Id.zone)
  FormatData$Id.sp=as.factor(FormatData$Id.sp)
  FormatData$Id.sp=FormatData$Id.sp[,drop=T]
  FormatData=subset(FormatData,Diam0>0 | Diam>0 )
  FormatData$Diam0[FormatData$Diam0<(-900)]=0
  FormatData$Diam[FormatData$Diam<(-900)]=0
  FormatData$Diam0=FormatData$Diam0/pi
  FormatData$Diam=FormatData$Diam/pi
  
  IndexCampagne=levels(FormatData$Id.campagne)[levels(FormatData$Id.campagne)>"1986"]
  FormatData=subset(FormatData,Id.campagne%in%IndexCampagne)
  
  SimData$Id.zone=as.factor(SimData$Id.zone)
  SimData$Id.sp=as.factor(SimData$Id.sp)
  SimData=subset(SimData,Diam>0)
  SimData$Diam=SimData$Diam/pi
  
  
  DataRef=subset(SimData, Id.campagne=="1983")
  alpha=nrow(DataRef)/sum(DataRef$Diam-10)
  
  # Trait Data
  #TraitData=NULL
  
  
  out=list(ClusteringData=FormatData,SimulatingData=SimData,TraitData=TraitData,alpha=alpha)
  
  rm(MBaiki,MBaikiSpeciesTraits,envir=globalenv())
return(out)
}
##############################################################