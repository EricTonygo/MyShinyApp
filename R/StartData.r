StartData <-function(Data,Parcelle,ClassesDiam,Param){
  
  # VR 11-10-12
  # Formattage des donn?es de dynamique foresti?re pour ?tre avoir un point d?part compatible avec le programme FCM.r
  # et ?ventuellement r?cup?rer des donn?es pour v?rifier les pr?dictions du mod?le
  
  ########################################################################################################
  #
  # Description des variables d'entr?es 
  #  
  #  
  #  Data : data.frame de 4 colonnes : Id.zone, Id.sp, Id.campagne et Diam    
  #     Id.zone : facteur, identifiant des zones sur lesquelles les variables de structure du peuplement seront calcul?es
  #     Id.sp : facteur, identifiant des arbres pour les regroupers (par exemple taxon, nom vernculaire ...)
  #     Id.campagne : facteur, ann?e de la campagne de mesure
  #     Diam : numeric, valeur du diametre de l'arbre en cm, lors de la campagne de mesure. Vaut 0 si l'arbre est en mort
  #
  #  Parcelles : un vecteur contenant les noms des parcelles pour lesquelles ont souahite avoir un point de d?part
  #  AnneeDepart : un entier d?signant l'ann?e de la campagne utilis?e pour le construire le point de d?part
  #  verif : bool?en, pour d?terminer si l'on calcule des valeurs pour verifier les pr?dicitons de FCM. Si TRUE CampangesExpVerif doit ?tre renseign?.
  #  AnnnePostExpVerif :  un entier d?signant l'ann?e de la campagne utilis?e pour le construire le point de d?part post-exploitation
  #  CampagnesExpVerif : un vecteur contenant les ann?es post-exploitations que l'on souhaite compar?ee aux pr?dictions de FCM 
  #
  #######################################################################
  
  
  
  NbClasse=length(ClassesDiam)
  Parcelle=as.character(Parcelle)
  # Restriction des donn?es à la parcelle concernee
  Data=subset(Data,Id.zone==Parcelle)
  
  
  # D?coupage en classe
  
  Data$ClassesDiam=as.factor(findInterval(Data$Diam,ClassesDiam))
  
  EffectifsTotaux=0
  
  
  # Initialisation des variables de sortie
  
  if (Param$Check){
    
    Data1an=subset(Data,Id.campagne==as.character(Param$Date1PostLog.Check))
    
    DataVerif=data.table(Data,key="ClassesDiam,Id.sp,Id.zone,Id.campagne")
    DataVerif[,Efft:=1]
    DataVerif=DataVerif[,list(Effectifs=sum(Efft)),by="Id.zone,ClassesDiam,Id.sp,Id.campagne"]
    
    IndexCampagne=levels(DataVerif$Id.campagne)[levels(DataVerif$Id.campagne)>=Param$StartingDate]
    DataVerif=subset(DataVerif,Id.campagne%in%IndexCampagne)
    setkey(DataVerif,"ClassesDiam")
    
  }else{
    Data1YearPost=NULL
    DataVerif=NULL
  }
  
  DataStart=list()  
  
  # Restriction ? l'ann?e de d?part
  Data=subset(Data,Id.campagne==Param$StartingDate)
  
  # for (p in 1:length(Parcelles)){  
  
  
  # Restriction des donn?es ? la parcelle ?tudi?e et aux arbres vivants
  DataDepart=subset(Data,Id.zone==Parcelle,select=c(ClassesDiam,Id.sp))
  
  levels(DataDepart$ClassesDiam)=1:NbClasse
  #  Effectifs de d?part pour chaque idvern
  
  Effectifs=tapply(DataDepart$ClassesDiam,DataDepart$Id.sp,function(x) as.numeric(table(x)),simplify=F)
  DataStart=sapply(Effectifs,function(x) if(is.null(x)) rep(0,NbClasse) else x)
  # EffectifsTotaux=apply(DataStart,1,sum)+EffectifsTotaux
  
  # Calcul des valeurs r?elles des effectifs
  if (Param$Check){
    
    DataDepart=subset(Data1an,Id.zone==Parcelle,select=c(ClassesDiam,Id.sp))
    levels(DataDepart$ClassesDiam)=1:NbClasse
    Effectifs=tapply(DataDepart$ClassesDiam,DataDepart$Id.sp,function(x) as.numeric(table(x)),simplify=F)
    Data1YearPost=sapply(Effectifs,function(x) if(is.null(x)) rep(0,NbClasse) else x)
    
  }
  
  
  return(list(DataStart=DataStart,Data1YearPost=Data1YearPost,DataVerif=DataVerif,StartingDate=Param$StartingDate))
}

