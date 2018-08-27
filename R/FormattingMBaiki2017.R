FormattingMBaiki2017<-function(Data){
  
  # VR 20-03-2018
  
 
  
  DimMax=nrow(Data)*(ncol(Data)-5)
  
  
  Data$Parcelles=paste(Data$parcelle,Data$carre,sep="")
  NomParcelles=unique(Data$Parcelles)
  NbParcelles=length(NomParcelles)
  
  NomsCampagnes=colnames(Data)[6:35]
  Campagnes=as.numeric(substr(NomsCampagnes,6,9))
  NbCampagnes=length(Campagnes)

  
  
  FormatData=NULL
  NomsFormatData=c("Id.zone","Id.sp","Nom.sp","Diam0","Diam1","Id.campagne0","Id.campagne1","Surface.zone")
 
 # colnames(FormatData)=NomsFormatData

  compteur=1

    
  
    for (i in 1:(NbCampagnes-1)){
      
      GardeTmp=c("Parcelles","code_vernacular","vernaculaire95",NomsCampagnes[i],NomsCampagnes[i+1])

      DataTmp=subset(Data,select=GardeTmp)
      FormatDataTmp=cbind(DataTmp,Campagnes[i],Campagnes[i+1],1)
      colnames(FormatDataTmp)=NomsFormatData
      NbRowTmp=nrow(FormatDataTmp)
      FormatData=rbind(FormatData,FormatDataTmp)
      #FormatData[compteur:(compteur+NbRowTmp-1),]=FormatDataTmp
 
      compteur=compteur+NbRowTmp
      
    }


  FormatData=FormatData[1:(compteur-1),]


  FormatData=subset(FormatData,Diam0>0 | Diam1>0 )
  FormatData$Diam0[FormatData$Diam0<(-900)]=0
  FormatData$Diam1[FormatData$Diam1<(-900)]=0
  FormatData$Diam0=FormatData$Diam0/pi
  FormatData$Diam1=FormatData$Diam1/pi
  

  rm(MBaiki2017,envir=globalenv())
return(FormatData)
}
##############################################################

Table.Parcelles.Campagnes.selected.MBaiki=function(DataFormated){

ParcellesTemoin=c(131,132,133,134,161,162,163,164,241,242,243,244)

ParcellesFeux=c(231,232,233,234,241,242,243,244)

ListeCampagne=sort(unique(DataFormated$Id.campagne0))
ListeParcelles=unique(DataFormated$Id.zone)

ListeCampagneSuppNonTemoin=ListeCampagne[c(3:6)] 
ListeCampagneSuppTous=ListeCampagne[c(15:17,28:29)] 

out=data.frame(expand.grid(Id.campagne=ListeCampagne,Id.zone=ListeParcelles),In=1)
out$In[(!out$Id.zone%in%ParcellesTemoin)&(out$Id.campagne%in%ListeCampagneSuppNonTemoin)]=0    
out$In[out$Id.campagne%in%ListeCampagneSuppTous]=0  

out$In[out$Id.zone%in%ParcellesFeux]=0

return(out)

}


