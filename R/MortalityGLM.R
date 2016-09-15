MortalityGLM <- function(DataFormatted,ParamFiles,criterion="BIC"){
  
  # Fonction qui constitue les groupes d'IdVern par une proc?dure EM en utilisant FLEXMIX
  # Le choix du meilleur nombre de groupe est fait ? partir du crit?re ICL
  # Les groupes sont r?alis?s par classes de diam?tre pour les processue de mortalit? et croissance s?paremment
  # Les processus sont mod?lis?s par un mod?le logit utilisant des covariables (relatives ? la structure du peuplement)
  
  # Data : donn?es au format de la fonction MiseFormeDataGpIdVern
  # ListApartSpecies : list of species not clustered automaticaly
  #          example: ListApartSpecies=list(c("405"),c("107","108","109")) means model will be infered for species "405" alone 
  #                and for species "107","108","109" 
  #  
  # nbgpmax : nombre de groupes maximal essay? pour chaque processus et chaque classe
  # criterion : crit?re utilis? pour choisir le nombre de groupe "AIC", "BIC" ou "ICL"
  
  
  library(flexmix)
  
  
  
  ##################################################################################
  ##################################################################################
  
  ######################
  # Internal functions #
  ######################
  
  
  FormatDataMortalityGLM<-function(Data,ClassesDiam,NbClasse,Surface,CDSTB){
    
    
    
    library(data.table)
    
    # Liste des "esp?ces"
    ListeIdsp=levels(Data$Id.sp)
    
    # D?coupage en classe
    Data$Classe=findInterval(Data$Diam,ClassesDiam)
    Data$Classe0=findInterval(Data$Diam0,ClassesDiam)
    
    
    # Calcul surface terri?re en m2
    
    VectST=c(0,CDSTB$ST)
    Data$ST0=VectST[(Data$Classe0+1)]
    Data$Effect0=Data$Classe0>0
    
    
    
    ######
    # Calcul des tables de comp?tition
    #######
    
    Data=data.table(Data,key="Id.zone,Id.campagne,Id.sp")
    
    
    NomsSTTC=paste("STTC",ClassesDiam,sep='')
    
    
    # construction des commandes pour extraire les tables de competition
    Expr="Data[,list(sum(ST0[Classe0==1])"
    Expr=paste0(Expr,paste0(",sum(ST0[Classe0==",2:NbClasse,"])",collapse=''),"),by='Id.zone,Id.campagne']")
    
    # Calcul des tables de comp?tition
    Compet=eval(parse(text=Expr)) 
    setnames(Compet,3:ncol(Compet),c(NomsSTTC))
    
    
    
    ###############################
    # Mise en forme des donn?es de dynamique
    ########################
    
    DataDyn=subset(Data,Classe0>0)
    
    DataDyn[,mort:=as.numeric(Diam==0)]
    DataDyn[,vivant:=as.numeric(Classe>=Classe0)]
 
    
    
    
    DataDyn[,verif:=mort+vivant]
    if (DataDyn[,sum(verif==0)]>0) DataDyn=subset(DataDyn,verif==1)
    
    
    setkey(DataDyn,Id.zone,Id.sp,Id.campagne)
    tmpDyn=DataDyn[,list(mort=sum(mort),vivant=sum(vivant)),by="Id.zone,Id.campagne,Classe0"]
    
    tmpDyn[,FacteurClasse0:=as.factor(Classe0)]
    setkey(tmpDyn,FacteurClasse0)
    FacteurClassesDiam=levels(tmpDyn$FacteurClasse0)
    
    DataDynOut=vector("list",NbClasse)
    
    for (i in 1:length(FacteurClassesDiam)){
      
      tmpDynClasse=tmpDyn[FacteurClassesDiam[i],mult="all"]
      setkey(tmpDynClasse,Id.zone,Id.campagne)
      tmpCompet=subset(Compet,select=c("Id.zone","Id.campagne",NomsSTTC))
      setkey(tmpCompet,Id.zone,Id.campagne)
      tmpDynClasse=merge(tmpDynClasse,tmpCompet)
      tmpDynClasse=subset(tmpDynClasse,select=c("mort","vivant",NomsSTTC,"Id.zone","Id.campagne"))
      DataDynOut[[i]]=data.frame(tmpDynClasse)
      
    }
    
    
    
    return(DataDynOut)
  }
  
  
 
  
  
  
  
  
  
  # loading user's parameters 
  source(ParamFiles,local=T)
  
  
  
  # generating models  
  NomsSTTC=paste("STTC",ClassesDiam,sep='')
  modelMortality=list()

    exprMortality=paste0("cbind(mort,vivant)~",paste(NomsSTTC,collapse="+"))
    modelMortality=eval(parse(text=exprMortality))
    
 
  
  SimMort=list()
  #SimMort$CDSTB=ClasseDiamSTAGB(ClassesDiam,alpha=DataFormatted$alpha)
  
  
  # Formatting Data                     
  DataDyn=FormatDataMortalityGLM(DataFormatted$ClusteringData,ClassesDiam,NbClasse,Surface,SimMort$CDSTB)
  
  
  
  
  
  
  ################
  # infering mortality  processes
  ################
  
  
  
  

  ParamGpMortality=list()
  

  # Boucle sur les classes de diam?tre pour faire les groupes
  for (i in 1:NbClasse){
    
    
    DataDynTmp=DataDyn[[i]]
    res=glm(modelMortality,data=DataDynTmp,family="binomial")
    ParamGpMortality[[i]]=res$coef

  }
  
  SimMort$ParamGp=ParamGpMortality

  SimMort$Func<-function(Eff.cur,SimMort){
    
    NbClasse=nrow(Eff.cur)
    NbIdVern=ncol(Eff.cur)
    Effectifs.totaux=apply(Eff.cur,1,sum)
    STTC=Effectifs.totaux*SimMort$CDSTB$ST
    datapred=c(1,STTC)
    Morts=matrix(0,nrow=NbClasse,ncol=NbIdVern)
    
    for (cl in 1:(NbClasse)){
      PbMortality=1/(1+exp(-sum(datapred*SimMort$ParamGp[[cl]])))
   
      NbMort=rbinom(1,Effectifs.totaux[cl],PbMortality)
      if (NbMort>0) {
          tmp=table(sample(NbIdVern,NbMort,replace=T,prob=Eff.cur[cl,]))
          Morts[cl,as.numeric(names(tmp))]=as.numeric(tmp)
          test=Morts[cl,]>Eff.cur[cl,]
          if (any(test)) Morts[cl,test]=Eff.cur[cl,test]
       }
    }
    
    
    return(Morts)  
  }  
  
  
  
  
  
  return(SimMort) 
}
