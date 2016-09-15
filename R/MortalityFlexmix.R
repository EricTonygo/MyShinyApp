MortalityFlexmix <- function(DataFormatted,ParamFiles,criterion="BIC", SpeciesTraits= NULL, OtherIndicator= NULL){
  
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
  
  GetTabST<-function(CDSTB){
    TabST = rep(0, NbClasse)
    for (t in 1:(NbClasse)){
      TabST [t]= CDSTB$ST[CDSTB$ClassesDiam == t][1]
    }
    return(TabST)
  }
  
  FormatDataMortalityFlexmix<-function(Data,ClassesDiam,NbClasse,Surface,CDSTB){
    

    
    library(data.table)
    
    # Liste des "esp?ces"
    ListeIdsp=levels(Data$Id.sp)
    
    # D?coupage en classe
    Data$Classe=findInterval(Data$Diam,ClassesDiam)
    Data$Classe0=findInterval(Data$Diam0,ClassesDiam)
    
    
    # Calcul surface terri?re en m2
    VectST=c(0,GetTabST(CDSTB = CDSTB))
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
  
  
  
  ##################################################################
  
  
  
  UpdateListApartSpecies<-function(index){
    # updating list species managed apart
    ListApartSpecies=UserListApartSpecies
    if (length(ListApartSpecies)>0 & length(index)>0){   
      index.apart=sapply(ListApartSpecies,function(x) max(x%in%index))
      if (sum(index.apart)>0){
        for (ll in sort(which(as.logical(index.apart)),decreasing=T)){
          index.apart2=!ListApartSpecies[[ll]]%in%index.noDead
          if (sum(index.apart2)==0){ListApartSpecies[[ll]]=NULL}
          else{ListApartSpecies[[ll]]=ListApartSpecies[[ll]][index]}
        }
      }
    }
    return(ListApartSpecies)
  }  
  
  ##################################################################
  
  clustering<-function(Gp,ParamGp,compteur,Data,Model,ModelGp,Family,label){
    
    
    
    Data$Id.sp=Data$Id.sp[,drop=T]   
    Arret=T
    groups=rep(1,nrow(Data))
    
    if (criterion=="ICL"){
      res=flexmix(Model,data=Data,k=1,model=FLXMRglm(family=Family))
      crit=ICL(res)
    }else{
      res=glm(Model,data=Data,family=Family)
      crit=get(criterion)(res)
    }
    nbgp=2
    
    rm(res)
    gc()
    
    while(Arret){
      
      res=try(flexmix(ModelGp,data=Data,k=nbgp,model=FLXMRglm(family=Family)),silent=T)        
      if (!is(res,"try-error")) {
        crit.new=get(criterion)(res)
        if (crit.new>=crit | res@k<nbgp){Arret=F}
        else{
          crit=crit.new
          groups=clusters(res)
          nbgp=nbgp+1
          rm(res) 
          gc()
        }
      }else{
        print(paste("warning: flexmix failled clustering",nbgp,"groups, for the",label,"model in the class",ncol(Gp)-1))
        rm(res) 
        gc()
        Arret=F
      }
      
    }
    
    Data$gp2=groups+compteur
    
    # getting models parameters 
    
    for (j in min(Data$gp2):max(Data$gp2)){
      DataTmp=subset(Data,gp2==j)
      ModelTmp=glm(Model,data=DataTmp,family=Family)
      ParamGp[,j]=as.numeric(ModelTmp$coef)
    } 
    
    ParamGp=ParamGp[,1:j]
    
    Data=unique(subset(Data,select=c(Id.sp,gp2)))
    Gp=merge(Gp,Data,all.x=T,all.y=F)
    Gp[!is.na(Gp$gp2),ncol(Gp)-1]=Gp[!is.na(Gp$gp2),ncol(Gp)]  
    Gp$gp2=NULL  
    
    colnames(Gp)[ncol(Gp)]=paste("cl",ncol(Gp)-1,sep="")
    
    return(list(Gp=Gp,ParamGp=ParamGp))
  }
  ##############################
  
  GpParamApartSpecies<-function(Gp,ParamGp,ListApartSpecies,Model,Family,Data,compteur){
    
    for (sp in 1:length(ListApartSpecies)){
      
      compteur=compteur+1
      Gp[Gp$Id.sp%in%ListApartSpecies[[sp]],ncol(Gp)]=compteur
      DataTmp=subset(Data,Id.sp%in%ListApartSpecies[[sp]])
      ModelTmp=glm(Model,data=DataTmp,family=Family)  
      ParamGp[,compteur]=as.numeric(ModelTmp$coef) 
      Data=subset(Data,!Id.sp%in%ListApartSpecies[[sp]])
      
    }
    return(list(Gp=Gp,ParamGp=ParamGp,Data=Data,compteur=compteur))
  }
  
  ################################
  
  logit<-function(p) log(p/(1-p))
  
  
  
  ##################################################################################
  ##################################################################################
  
  
  
  
  
  
  
  
  
  
  
  
  # loading user's parameters 
  source(ParamFiles,local=T)
  

  
  # generating models  
  NomsSTTC=paste("STTC",ClassesDiam,sep='')

 
 
    exprMortality=paste0("cbind(mort,vivant)~",paste(NomsSTTC,collapse="+"))
    modelMortality=eval(parse(text=exprMortality))
    modelMortalityGp=eval(parse(text=paste(exprMortality,"|Id.sp")))
    NbParamMortality=NbClasse+1

 
  
  SimMort=list()
  SimMort$CDSTB=ClasseDiamSTAGB(ParamFile = ParamFiles,alpha=DataFormatted$alpha, SpeciesTraits= SpeciesTraits, OtherIndicator = OtherIndicator)
  
  
  # Formatting Data                     
  DataDyn=FormatDataMortalityFlexmix(DataFormatted$ClusteringData,ClassesDiam,NbClasse,Surface,SimMort$CDSTB)
 
 

  
  
  
  ################
  # clustering mortality  processes
  ################
  
  
  
  
 
  ListeIdsp=levels(DataFormatted$ClusteringData$Id.sp)
  GpMortality=data.frame(Id.sp=ListeIdsp)
  ParamGpMortality=list()
  
  
 
  # Boucle sur les classes de diam?tre pour faire les groupes
  for (i in 1:NbClasse){

    
    DataDynTmp=DataDyn[[i]]
    DataDynTmp$vivant=DataDynTmp$reste+DataDynTmp$monte
    
    
    # indentifying species with no dead
    index.noDead=names(which(tapply(DataDynTmp$mort,DataDynTmp$Id.sp,sum)==0))
    compteur.mortality=0
    GpMortality[,i+1]=0 
    ParamGpMortality[[i]]=matrix(0,nrow=NbParamMortality,ncol=length(levels(DataDynTmp$Id.sp)))
    #row.names(ParamGpMortality[[i]])=Label.paramMortality
    
    
    if (length(index.noDead)>0){
      DataNoDead=subset(DataDynTmp,Id.sp%in%index.noDead)
      DataDynTmp=subset(DataDynTmp,!Id.sp%in%index.noDead)
      noDead=T
    }else{
      noDead=F
    }
    
    
    
    # updating list species managed apart
    ListApartSpecies=UpdateListApartSpecies(index.noDead)
    
    # clustering species with some dead
    
    if (nrow(DataDynTmp)>0){
      # speceis managed apart
   
      if (length(ListApartSpecies)>0){
        res=GpParamApartSpecies(GpMortality,ParamGpMortality[[i]],ListApartSpecies,modelMortality,"binomial",DataDynTmp,compteur.mortality)
        DataDynTmp=res$Data
        GpMortality=res$Gp
        ParamGpMortality[[i]]=res$ParamGp
        compteur.mortality=res$compteur
      }
      
      if (nrow(DataDynTmp)>0){
    
        res=clustering(GpMortality,ParamGpMortality[[i]],compteur.mortality,DataDynTmp,modelMortality,modelMortalityGp,"binomial","mortality")
        GpMortality=res$Gp
        if (max(GpMortality[,i+1])>1) ParamGpMortality[[i]]=res$ParamGp
        else ParamGpMortality[[i]]=matrix(res$ParamGp,ncol=1)
      }
    }else{
      stop(paste0("Mortality model cannot be infered in diameter class ",i," because there is no tree recruited in the data"))    }
    
 

    
    
    if(noDead){
      
      for (sp in index.noDead){
        DataTmp=subset(DataNoDead,Id.sp==sp)
        datapred=cbind(1,subset(DataTmp,select=NomsSTTC))
        prob=1/(1+exp(-as.matrix(datapred)%*%as.matrix(ParamGpMortality[[i]])))    
        GpMortality[GpMortality$Id.sp==sp,i+1]=which.max(apply(prob,2,function(x)sum(dbinom(DataTmp$mort,DataTmp$vivant,x,log=T))))        
      }
    }
    # species whithout trees in the diameter class
    
    if (any(GpMortality[,i+1]==0)){
      DataDyn[[i]]$Tot=DataDyn[[i]]$reste+DataDyn[[i]]$monte+DataDyn[[i]]$mort
      tmp=cbind(tapply(DataDyn[[i]]$Tot,DataDyn[[i]]$Id.sp,sum),GpMortality[,i+1])
      tmp=tapply(tmp[,1],tmp[,2],sum)
      GpMortality[GpMortality[,i+1]==0,i+1]=as.numeric(names(tmp)[which.max(as.numeric(tmp))])
    }
    
    DataDyn[[i]]="toto"
    gc()
    
    
  
  }
  
  SimMort$ParamGp=ParamGpMortality
  SimMort$Gp=GpMortality
  SimMort$Func<-function(Eff.cur,SimMort){
    
    NbClasse=nrow(Eff.cur)
    NbIdVern=ncol(Eff.cur)
    Effectifs.totaux=apply(Eff.cur,1,sum)
    STTC=Effectifs.totaux*GetTabST(CDSTB = SimMort$CDSTB)
    datapred=c(1,STTC)
    PbMortality=matrix(nrow=NbClasse,ncol=NbIdVern)
    for (cl in 1:(NbClasse)){
      CL=1/(1+exp(-datapred%*%SimMort$ParamGp[[cl]]))
      PbMortality[cl,]=CL[SimMort$Gp[,cl+1]]
     
    }
    
    return(matrix(rbinom(NbIdVern*NbClasse,Eff.cur,PbMortality),nrow=NbClasse))  
  }  
  

  
  
  
  return(SimMort) 
}
