plot.FCM <-function(out.FCM,Outputs=NULL,Groups=NULL,Verif=NULL){


#########################################
# Function to plot simulations from FCM #
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
DataOutputs=data.table(out.FCM$ParamPlot$CDSTB,key="ClassesDiam")
NbClasse=nrow(DataOutputs)
DataTraits=data.table(out.FCM$SpeciesTraits,key="Id.sp")
DataTraits$Id.sp=as.factor(DataTraits$Id.sp)
Lab.period=out.FCM$ParamPlot$Lab.period
Nb.period=out.FCM$ParamPlot$Nb.period
Surface=out.FCM$ParamPlot$Surface



# rescaling parameters at 1ha

DataOutputs[,Eff:=Eff/Surface]
DataOutputs[,ST:=ST/Surface]
DataOutputs[,AGB:=AGB/Surface]
DataOutputs[,Vol:=Vol/Surface]

#browser()
Simulations=merge(Simulations,DataTraits,all.x=F,all.y=F)
Simulations=merge(Simulations,DataOutputs,by="ClassesDiam",all.x=T,all.y=F)

Simulations[,STt:=ST*Effectifs]
Simulations[,AGBt:=AGB*Effectifs*WSG]
Simulations[,Efft:=Eff*Effectifs]   
Simulations[,Volt:=Vol*Effectifs] 
Simulations[,Temps:=Temps*Nb.period]
    

if (StartingDate==0 | is.null(out.FCM$DataVerif)) Verif=F
Simulations[,Temps:=Temps+as.numeric(StartingDate)]

Temps=seq(as.numeric(StartingDate),max(Simulations$Temps),Nb.period)

if (is.null(Outputs)) OutPuts=c(1,2)
if (is.null(Groups)) Groups=list(stand=levels(Simulations$Id.sp))

if (Verif){
  DataVerif=data.table(out.FCM$DataVerif,key="Id.sp")
  DataVerif=merge(DataVerif,DataTraits,by="Id.sp",all.x=T,all.y=F)
  DataVerif=merge(DataVerif,DataOutputs,by="ClassesDiam",all.x=T,all.y=F)
  DataVerif[,STt:=ST*Effectifs]
  DataVerif[,AGBt:=AGB*Effectifs*WSG]
  DataVerif[,Efft:=Eff*Effectifs]
  DataVerif[,Volt:=Vol*Effectifs] 
  DataVerif[,Temps:=as.numeric(as.character(DataVerif$Id.campagne))]
}

Simulations$ClasseDiam=as.factor(Simulations$ClasseDiam)

for (g in 1:length(Groups)){

  SimulationTmp=subset(Simulations,Id.sp%in%Groups[[g]])

  Indicateurs=SimulationTmp[,list(Effs=sum(Efft),STs=sum(STt),AGBs=sum(AGBt),Vols=sum(Volt)),by="Id.zone,iter,Temps"]
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
  x11(title=paste("Proportion of trees by diameter class for",names(Groups)[g]))
  
  for (cd in 1:NbClasse){
    plot(Temps,cumul[cd,],xlab=Lab.period,ylab="Proportion of trees by diameter class",t="l",ylim=ORD) 
    par(new=T)
  }
  
  abline(h=0)
  # Graph effectifs
  IndicateursEffect=Indicateurs[,list(liEffs=quantile(Effs,0.025),medEffs=median(Effs),lsEffs=quantile(Effs,0.975)),by="Temps"]
 
  
  x11(title=paste("Effective prediction for",names(Groups)[g]))
  
  ORD=c(0,max(IndicateursEffect$lsEff*1.1))
  plot(IndicateursEffect$Temps,IndicateursEffect$medEff,xlab=Lab.period,ylab="Number of trees/ha",t="l",ylim=ORD)
  par(new=T)
  plot(IndicateursEffect$Temps,IndicateursEffect$liEff,xlab=Lab.period,ylab="Number of trees/ha",t="l",ylim=ORD,col="red")
  par(new=T)
  plot(IndicateursEffect$Temps,IndicateursEffect$lsEff,xlab=Lab.period,ylab="Number of trees/ha",t="l",ylim=ORD,col="red")
  
  if (Verif){
    DataVerifTmp=subset(DataVerif,Id.sp%in%Groups[[g]])
    DataVerifTmp=data.table(DataVerifTmp,key="Id.zone,Temps")
    IndicateursVerif=DataVerifTmp[,list(Effs=sum(Efft),STs=sum(STt),AGBs=sum(AGBt),Vols=sum(Volt)),by="Id.zone,Temps"]
    IndicateursVerif=IndicateursVerif[,list(Effs=median(Effs),STs=median(STs),AGBs=median(AGBs),Vols=median(Vols)),by="Temps"]
    points(IndicateursVerif$Temps,IndicateursVerif$Effs,pch="+",cex=0.5)
   
  }
 
  # Graph Biomasse
  x11(title=paste("Biomass prediction for",names(Groups)[g]))
  
  IndicateursBiomass=Indicateurs[,list(liAGBs=quantile(AGBs,0.025, na.rm = TRUE),medAGBs=median(AGBs),lsAGBs=quantile(AGBs,0.975, na.rm = TRUE)),by="Temps"]
  #IndicateursBiomass=IndicateursBiomass[,list(liAGB=mean(liAGBs),medAGB=mean(medAGBs),lsAGB=mean(lsAGBs)),by="Temps"]
  #browser()
  ORD=c(0,max(IndicateursBiomass$lsAGB*1.1))
  plot(IndicateursBiomass$Temps,IndicateursBiomass$medAGB,xlab=Lab.period,ylab="Biomass T/ha",t="l",ylim=ORD)
  par(new=T)
  plot(IndicateursBiomass$Temps,IndicateursBiomass$liAGB,xlab=Lab.period,ylab="Biomass T/ha",t="l",ylim=ORD,col="red")
  par(new=T)
  plot(IndicateursBiomass$Temps,IndicateursBiomass$lsAGB,xlab=Lab.period,ylab="Biomass T/ha",t="l",ylim=ORD,col="red")
  if (Verif){
    points(IndicateursVerif$Temps,IndicateursVerif$AGBs,pch="+",cex=0.5)
  }
  
  # Graph Taux reconstitution Biomasse
  
  
  x11(title=paste("Biomass recovery rate for",names(Groups)[g]))

  tmp=subset(Indicateurs,select=c(Id.zone,AGBs),iter==0)
  setnames(tmp,"AGBs","AGBsInit")
  Indicateurs=merge(Indicateurs,tmp,by="Id.zone",all.x=T,all.y=F)
 
  Indicateurs[,AGBsR:=AGBs/AGBsInit]
  IndicateursBiomass=Indicateurs[,list(liAGBs=quantile(AGBsR,0.025,na.rm=T),medAGBsR=median(AGBsR),lsAGBs=quantile(AGBsR,0.975,na.rm=T)),by="Temps"]
  IndicateursBiomass$liAGB=IndicateursBiomass$liAGB*100
  IndicateursBiomass$lsAGB=IndicateursBiomass$lsAGB*100
  IndicateursBiomass$medAGB=IndicateursBiomass$medAGB*100
 
  ORD=c(0,max(IndicateursBiomass$lsAGB*1.1))
  plot(IndicateursBiomass$Temps,IndicateursBiomass$medAGB,xlab=Lab.period,ylab="Biomass recovery rate",t="l",ylim=ORD)
  par(new=T)
  plot(IndicateursBiomass$Temps,IndicateursBiomass$liAGB,xlab=Lab.period,ylab="Biomass recovery rate",t="l",ylim=ORD,col="red")
  par(new=T)
  plot(IndicateursBiomass$Temps,IndicateursBiomass$lsAGB,xlab=Lab.period,ylab="Biomass recovery rate",t="l",ylim=ORD,col="red")
  if (Verif){
    points(IndicateursVerif$Temps,IndicateursVerif$AGBs/IndicateursVerif$AGBs[1]*100,pch="+",cex=0.5)
  }
#   outThomas=rbind(seq(1,2*length(IndicateursBiomass$Temps),by=2),IndicateursBiomass$liAGB,IndicateursBiomass$medAGB,IndicateursBiomass$lsAGB)
#   rownames(outThomas)=c("annees","Tx min","Tx median","Tx max")
#   write.csv2(outThomas,file="TxReconstBiomasseExpT3.csv")
#   
# Graph Volume

x11(title=paste("Volume prediction for",names(Groups)[g]))
IndicateursVolume=Indicateurs[,list(liVols=quantile(Vols,0.025, na.rm = T),medVols=median(Vols),lsVols=quantile(Vols,0.975, na.rm = T)),by="Temps"]
# IndicateursBiomass=IndicateursBiomass[,list(liAGB=mean(liAGBs),medAGB=mean(medAGBs),lsAGB=mean(lsAGBs)),by="Temps"]

ORD=c(0,max(IndicateursVolume$lsVols*1.1))
plot(IndicateursVolume$Temps,IndicateursVolume$medVols,xlab=Lab.period,ylab="Volume m3/ha",t="l",ylim=ORD)
par(new=T)
plot(IndicateursVolume$Temps,IndicateursVolume$liVols,xlab=Lab.period,ylab="Volume m3/ha",t="l",ylim=ORD,col="red")
par(new=T)
plot(IndicateursVolume$Temps,IndicateursVolume$lsVols,xlab=Lab.period,ylab="Volume m3/ha",t="l",ylim=ORD,col="red")
if (Verif){
  points(IndicateursVerif$Temps,IndicateursVerif$Vols,pch="+",cex=0.5)

}

}
}

#############################################
