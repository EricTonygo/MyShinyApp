GrowthGLM <- function(DataFormatted,ParamFiles,criterion="BIC"){
  
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
  
  
  FormatDataGrowthGLM<-function(Data,ClassesDiam,NbClasse,Surface,CDSTB){
    
    
    
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
    DataDyn[,monte:=as.numeric(Classe>Classe0)]
    DataDyn[,reste:=as.numeric(Classe==Classe0)] 
    
    
    
    DataDyn[,verif:=mort+monte+reste]
    if (DataDyn[,sum(verif==0)]>0) DataDyn=subset(DataDyn,verif==1)
    
    
    setkey(DataDyn,Id.zone,Id.sp,Id.campagne)
    tmpDyn=DataDyn[,list(mort=sum(mort),reste=sum(reste),monte=sum(monte)),by="Id.zone,Id.campagne,Id.sp,Classe0"]
    
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
      tmpDynClasse=subset(tmpDynClasse,select=c("mort","reste","monte",NomsSTTC,"Id.sp","Id.zone","Id.campagne"))
      DataDynOut[[i]]=data.frame(tmpDynClasse)
      
    }
    
    
    
    
    return(DataDynOut)
  }
  
  
  
  
  
  
  
  
  
  # loading user's parameters 
  source(ParamFiles,local=T)
  
  
  
  # generating models  
  NomsSTTC=paste("STTC",ClassesDiam,sep='')
  modelGrowth=list()
  modelGrowthGp=list()
  NbParamGrowth=rep(0,NbClasse)
 
  
    exprGrowth=paste0("cbind(monte,reste)~",paste(NomsSTTC,collapse="+"))
    modelGrowth=eval(parse(text=exprGrowth))
    

  

  
  
  SimGrowth=list()
  #SimGrowth$CDSTB=ClasseDiamSTAGB(ClassesDiam,alpha=DataFormatted$alpha)
  
  
  # Formatting Data                     
  DataDyn=FormatDataGrowthGLM(DataFormatted$ClusteringData,ClassesDiam,NbClasse,Surface,SimGrowth$CDSTB)
  
  
  
  
  
  
  ################
  # infering mortality  processes
  ################
  
  
  
  
  
  ParamGpGrowth=list()
  
  
  # Boucle sur les classes de diam?tre pour faire les groupes
  for (i in 1:(NbClasse-1)){
    
    
    DataDynTmp=DataDyn[[i]]
    res=glm(modelGrowth,data=DataDynTmp,family="binomial")
    ParamGpGrowth[[i]]=res$coef
    
  }
  
  SimGrowth$ParamGp=ParamGpGrowth
  
  SimGrowth$Func<-function(Eff.cur,Mort,SimGrowth){
    
    NbClasse=nrow(Eff.cur)
    NbIdVern=ncol(Eff.cur)
    Effectifs.totaux=apply(Eff.cur,1,sum)
    STTC=Effectifs.totaux*SimGrowth$CDSTB$ST
    datapred=c(1,STTC)
    Growths=matrix(0,nrow=(NbClasse-1),ncol=NbIdVern)
    Eff.cur=Eff.cur-Mort
    for (cl in 1:(NbClasse-1)){
      PbGrowth=1/(1+exp(-sum(datapred*SimGrowth$ParamGp[[cl]])))
      NbGrowth=rbinom(1,Effectifs.totaux[cl],PbGrowth)
      if (NbGrowth>0) {
        tmp=table(sample(NbIdVern,NbGrowth,replace=T,prob=(Eff.cur[cl,]+1e-6)))
        Growths[cl,as.numeric(names(tmp))]=as.numeric(tmp)
        test=Growths[cl,]>Eff.cur[cl,]
        if (any(test)) Growths[cl,test]=Eff.cur[cl,test]
      }
    }
    
    
    return(Growths)  
  }  
  
  
  
  
  
  return(SimGrowth) 
}
