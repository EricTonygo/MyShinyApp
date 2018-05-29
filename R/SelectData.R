SelectData=function(DataFormated,ListeParcellesCampagnes=NULL,TraitData){

  
# ListePArcellesCampagnes data.frame : Id.zone | Id.campagne | In

DataFormated$Id.zone=as.factor(DataFormated$Id.zone)
DataFormated$Id.sp=as.factor(DataFormated$Id.sp)  



  
if (!is.null(ListeParcellesCampagnes)){
  
  DataFormated$CampPar=paste(DataFormated$Id.campagne0,DataFormated$Id.zone,sep="-")
  ListeParcellesCampagnes=subset(ListeParcellesCampagnes,In==1)
  CampParGarde=paste(ListeParcellesCampagnes$Id.campagne,ListeParcellesCampagnes$Id.zone,sep="-")
  InferData=subset(DataFormated,CampPar%in%CampParGarde,select=c("Id.zone","Id.sp","Diam0","Diam1","Id.campagne0","Id.campagne1","Surface.zone"))
}
  
  
InferData$Id.zone=InferData$Id.zone[,drop=T]  
InferData$Id.sp=InferData$Id.sp[,drop=T]  

  
  
SimData=subset(DataFormated,select=c("Id.zone","Id.sp","Diam0","Id.campagne0","Surface.zone"))
colnames(SimData)[3:4]=c("Diam","Id.campagne")

SimDataLast=subset(DataFormated,Id.campagne1==max(DataFormated$Id.campagne1),select=c("Id.zone","Id.sp","Diam1","Id.campagne1","Surface.zone"))
colnames(SimDataLast)[3:4]=c("Diam","Id.campagne")

SimData=rbind(SimData,SimDataLast)

SimData$Id.zone=as.factor(SimData$Id.zone)
SimData$Id.sp=as.factor(SimData$Id.sp)

SimData=subset(SimData,Diam>0)
 
colnames(TraitData)[6]="Nom.sp"  
TraitData=subset(TraitData,Id.sp%in%unique(SimData$Id.sp),select=c("Id.sp","WSG","DME","Nom.sp"))

TraitData$Id.sp=as.factor(TraitData$Id.sp)
TraitData$Nom.sp=TraitData$Nom.sp[,drop=T]

out=list(ClusteringData=InferData,SimulatingData=SimData,TraitData=TraitData)


}

