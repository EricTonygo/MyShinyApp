FormattingParacou <- function(Data,TraitData,Id.zone="n_parcelle",Parcelles=1:15,Id.sp="idVern",Prospecteur=T){
  
  # VR 24-08-2012
  # VR modified 01-11-2012
  
  # Traitement :
  #
  # 0 : temoin              P1 P6 P11
  # 1 : bois d'oeuvre       P2 P7 P9
  # 2 : bois d'oeuvre + eclaircit   P3 P5 P10 
  # 3 : bois d'oeuvre + eclaircit + bois energie  P4 P8 P12
  
  ##############################################################
  ################################################################
  
  IdVernProspecteur<-function(Data){
    
    # enl?ve les palmiers
    Data=subset(Data, idVern<500 | idVern>506)
    
    # Niveau de d?termination prospecteur Max Dada (Pessou)
    
    Data$idVern82=Data$idVern
    
    Data$idVern82[Data$idVern>700 & Data$idVern<713]=401 # Gaulettes
    Data$idVern82[Data$idVern>600 & Data$idVern<700]=405 # Divers
    Data$idVern82[Data$idVern>712]=405 # Divers
    Data$idVern82[Data$idVern==0]=405 # Divers
    Data$idVern82[Data$idVern==112]=405 # Divers
    
    Data$idVern82[Data$idVern==607]=218 # Manil
    Data$idVern82[Data$idVern==608]=218
    
    Data$idVern82[Data$idVern==604]=211 # Gonfolo
    Data$idVern82[Data$idVern==605]=211
    
    Data$idVern82[Data$idVern==698]=224 # Wapa
    
    Data$idVern82[Data$idVern==226]=222 #  Tossopassa
    Data$idVern82[Data$idVern==227]=222
    
    
    Data$idVern82[Data$idVern==1076]=311 # concordance arbocel
    Data$idVern82[Data$idVern==1087]=220
    Data$idVern82[Data$idVern==1094]=101
    Data$idVern=Data$idVern82
    
    return(Data)
  }
  

  
  ###########################################################
  # Param?tres sp?cifiques aux donn?es de Paracou
  ###########################################################
  
  # Choix du niveau de d?termination
  if (Prospecteur) Data=IdVernProspecteur(Data)
  
  # Ann?es des Campagnes utilis?es par le mod?le
  Campagnes=c("1985","1987","1989","1991","1993","1995","1997","1999","2001","2003","2005","2007","2009")
  
  # Num?ros des parcelles temoins
  ParcellesTemoin=c(1,6,11)
  
  # Num?ros des parcelles temoins mise en place dans un deuxi?me temps 
  ParcellesTemoin2=c(13,14,15)
  
  # Num?ros des parcelles temoins mise en place dans un deuxi?me temps 
  ParcellesExploite=c(2,3,4,5,7,8,9,10,12)
  
  # Ann?es des campagnes ? utiliser pour les parcelles exploit?es pour la calibration du mod?le
  CampagnesExp=c("1989","1991","1993","1995","1997","1999","2001","2003","2005","2007","2009")
  
  # Ann?es des campagnes ? utiliser pour les parcelles temoins pour la calibration du mod?le
  CampagnesTemoin=c("1987","1989","1991","1993","1995","1997","1999","2001","2003","2005","2007","2009")
  
  # Ann?es des campagnes ? utiliser pour les parcelles temoins  pour la calibration du mod?le
  CampagnesTemoin2=c("1993","1995","1997","1999","2001","2003","2005","2007","2009")

  Data$Diam=Data$circonf/pi
  Data=subset(Data,campagne%in%Campagnes & n_parcelle %in% Parcelles & vivant==1,select=c(Id.zone,Id.sp,"idArbre","Diam","campagne"))
  Data[,2]=as.factor(Data[,2])
 

  
  # Construction du jeu de donn?es pour l'inf?rence
  
  FormatData=NULL
  SimData=NULL
  

 

  Data1=subset(Data,campagne==Campagnes[1])
    
    for (i in 2:length(Campagnes)){
      SimData=rbind(SimData,subset(Data1,select=c(Id.zone,Id.sp,"Diam","campagne")))
      Data1$campagne=NULL
      colnames(Data1)[4]="Diam0"
      Data2=subset(Data,campagne==Campagnes[i])
      Data3=merge(Data1,Data2,all.x=T,all.y=T)
      Data3$campagne=Campagnes[i]
      Data3$Diam[is.na(Data3$Diam)]=0
      Data3$Diam0[is.na(Data3$Diam0)]=0
      FormatData=rbind(FormatData,Data3)
      Data1=Data2
      
    }
  
  FormatData$idArbre=NULL

  FormatData=subset(FormatData,!(campagne<1993 & n_parcelle%in%ParcellesTemoin2))
  FormatData=subset(FormatData,!(campagne<1991 & n_parcelle%in%ParcellesExploite))
                    
  colnames(FormatData)=c("Id.zone","Id.sp","Diam0","Diam","Id.campagne")
  FormatData$Id.campagne=as.factor(FormatData$Id.campagne)
  FormatData$Id.zone=as.factor(FormatData$Id.zone)
  FormatData$Id.sp=as.factor(FormatData$Id.sp)
  
  Data$idArbre=NULL
  colnames(Data)=c("Id.zone","Id.sp","Diam","Id.campagne")
  Data$Id.campagne=as.factor(Data$Id.campagne)
  Data$Id.zone=as.factor(Data$Id.zone)
 
  
  colnames(SimData)=c("Id.zone","Id.sp","Diam","Id.campagne")
  SimData$Id.zone=as.factor(SimData$Id.zone)
  SimData$Id.sp=as.factor(SimData$Id.sp)
  SimData$Id.campagne=as.factor(SimData$Id.campagne)
  
  DataRef=subset(SimData,Id.zone%in%c(ParcellesTemoin,ParcellesTemoin2) & Id.campagne=="2001")
  alpha=nrow(DataRef)/sum(DataRef$Diam-10)
  
  
  
  out=list(ClusteringData=FormatData,SimulatingData=SimData,TraitData=TraitData,alpha=alpha,)
  


  
  
  rm(Paracou,ParacouSpeciesTraits,envir=globalenv())
  return(out) 
 
}




