RecruitmentFlexmix <-function(DataFormatted,ParamFiles,criterion="BIC", OtherIndicator= NULL){

  
library(flexmix)  
  
  
  
  
##################################################################################
##################################################################################

######################
# Internal functions #
######################

FormatDataRecruitmentFlexmix <- function(Data,ClassesDiam,NbClasse,Surface,CDSTB){
  library(data.table)
  
  # Liste des "esp?ces"
  ListeIdsp=levels(Data$Id.sp)
  
  Data=data.table(Data,key="Id.zone,Id.campagne,Id.sp")
  
  # D?coupage en classe
  Data[,Classe:=findInterval(Diam,ClassesDiam)]
  Data[,Classe0:=findInterval(Diam0,ClassesDiam)]
 
  
  
  # Calcul surface terri?re en m2
  

  VectST=c(0,CDSTB$ST)
  Data[,ST0:=VectST[(Classe0+1)]]
  Data[,Effect0:=as.numeric(Classe0>0)]
  
  
  
  ######
  # Calcul des tables de comp?tition
  #######
  

  
  NomsSTTC=paste("STTC",ClassesDiam,sep='') 
  NomsNbArbres=paste("NbArbres",ClassesDiam,sep='')

  
  # construction des commandes pour extraire les tables de competition
 
 
  
  ExprRecrut="Data[,list(sum(Effect0[Classe0==1])"
  ExprRecrut=paste0(ExprRecrut,paste0(",sum(Effect0[Classe0==",2:NbClasse,"])",collapse=''))
  ExprRecrut=paste0(ExprRecrut,"),by='Id.zone,Id.campagne,Id.sp']")
  
  ExprCompet="Data[,list(sum(ST0[Classe0==1])"
  ExprCompet=paste0(ExprCompet,paste0(",sum(ST0[Classe0==",2:NbClasse,"])",collapse=''),"),by='Id.zone,Id.campagne']")                  
                      
  # Calcul des tables de comp?tition
  Compet=eval(parse(text=ExprCompet)) 
  setnames(Compet,3:ncol(Compet),NomsSTTC)
  
  CompetRecrut=eval(parse(text=ExprRecrut))
  setnames(CompetRecrut,4:ncol(CompetRecrut),NomsNbArbres)
  
  #################
  # Mise en forme des donn?es pour le recrutement
  ################
  
  Data[,Recrut:=as.numeric(Diam0==0 & Diam>0)]
  
  outRecruts=Data[,list(NbRecruts=sum(Recrut)),by="Id.zone,Id.campagne,Id.sp"]
  CompletRecrut=expand.grid(Id.zone=levels(Data$Id.zone),Id.campagne=levels(Data$Id.campagne),Id.sp=levels(Data$Id.sp))       
  CompletRecrut=data.table(CompletRecrut,key="Id.zone,Id.campagne,Id.sp")
  CompletRecrut[,toto:=0]
  outRecruts=merge(outRecruts,CompletRecrut,all.x=T,all.y=T)
  outRecruts=merge(outRecruts,Compet,all.x=F,all.y=T)
  setkey(outRecruts,Id.zone,Id.campagne,Id.sp)
  
  outRecruts=merge(outRecruts,CompetRecrut,all.x=T)
  outRecruts=data.frame(outRecruts)
  
  
  
  outRecruts=subset(outRecruts,select=c("NbRecruts",NomsNbArbres,NomsSTTC,"Id.sp","Id.zone","Id.campagne"))
  
  
  
  outRecruts[is.na(outRecruts)]=0
  return(outRecruts)
  
  
}

##################################################################################
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
  
  NbTry=5
  
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
    nbgp.new=nbgp-1
    crit.new=crit+10000
    for (i in 1:NbTry){
      res=try(flexmix(ModelGp,data=Data,k=nbgp,model=FLXMRglm(family=Family)),silent=T)        
      if (!is(res,"try-error")) {
        crit.gp=get(criterion)(res)
        if (crit.gp<crit.new){ 
          groups.new=clusters(res)
          crit.new=crit.gp
          nbgp.new=res@k
        }
      }
    }
    if (crit.new>=crit | nbgp.new<nbgp){Arret=F}
    else{
      crit=crit.new
      groups=groups.new
      nbgp=nbgp+1
      rm(res) 
      gc()
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

####################################################################

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





##################################################################################
##################################################################################
  
  
  
  
  
  
  # loading user's parameters 
  source(ParamFiles,local=T)
  
  ParamPlot=list(Lab.period=Lab.period,Nb.period=Nb.period,Surface=Surface)
  NomsSTTC=paste("STTC",ClassesDiam,sep='')
  NomsNbArbres=paste("NbArbres",ClassesDiam,sep='')
  NomsDataPred=c(NomsNbArbres,NomsSTTC)

  # generating models
  
  if (length(VarRecrut)>0){    
    indexNbA=substr(VarRecrut,1,3)=="NbA"
    VarModel=VarRecrut
    if (length(indexNbA)>0) VarModel[indexNbA]=paste("log(1+",VarModel[indexNbA],")",sep="")
    exprRecrut=paste("NbRecruts~1",paste("+",VarModel,collapse=" "))
  }else{exprRecrut="NbRecruts~1"}
 
  modelRecrut=eval(parse(text=exprRecrut)) 
  modelRecrutGp=eval(parse(text=paste(exprRecrut,"|Id.sp")))


  SimRecrut=list()
  SimRecrut$CDSTB=ClasseDiamSTAGB(ParamFile = ParamFiles,alpha=DataFormatted$alpha, OtherIndicator= OtherIndicator)


  # Formatting Data      
  DataRecrut=FormatDataRecruitmentFlexmix(DataFormatted$ClusteringData,ClassesDiam,NbClasse,Surface,SimRecrut$CDSTB)
  ListeIdsp=levels(DataFormatted$ClusteringData$Id.sp)
  GpRecrut=data.frame(Id.sp=ListeIdsp)
  GpRecrut$gp=0 

  ################
  # clustering for the recruitment process
  ################

    
  model.carac=terms(modelRecrut)
  NbParamRecrut=sum(attr(model.carac,"order"),attr(model.carac,"intercept"))
  Label.paramRecrut=c("Intercept",attr(model.carac,"term.labels"))
  
  
  Label.paramRecrut=Label.paramRecrut[1:NbParamRecrut]
  ParamGpRecruit=matrix(0,ncol=nrow(GpRecrut),nrow=NbParamRecrut)
  row.names(ParamGpRecruit)=Label.paramRecrut


  
  # identifying of species with no recruits 
  index.noRecrut=names(which(tapply(DataRecrut$NbRecruts,DataRecrut$Id.sp,sum)==0))
  
  
  if (length(index.noRecrut)>0){
    
    DataNoRecrut=subset(DataRecrut,Id.sp%in%index.noRecrut)
    DataRecrut=subset(DataRecrut,!Id.sp%in%index.noRecrut)
    norecrut=T
  }else{
    norecrut=F
  }
  
  compteur.recruit=0
  
  # updating list species managed apart
  ListApartSpecies=UpdateListApartSpecies(index.noRecrut)
  
  # clustering species with some recruits
  if (nrow(DataRecrut)>0){
    # speceis managed apart
    
    if (length(ListApartSpecies)>0){
      res=GpParamApartSpecies(GpRecrut,ParamGpRecruit,ListApartSpecies,modelRecrut,"poisson",DataRecrut,compteur.recruit)
      DataRecrut=res$Data
      GpRecrut=res$Gp
      ParamGpRecruit=res$ParamGp
      compteur.recruit=res$compteur
    }
    
    if (nrow(DataRecrut)>0){
      
      res=clustering(GpRecrut,ParamGpRecruit,compteur.recruit,DataRecrut,modelRecrut,modelRecrutGp,"poisson","recruitment")
      GpRecrut=res$Gp
      ParamGpRecruit=res$ParamGp
      
    }
  }else{
    stop("Recruiment model cannot be infered because there is no tree recruited in the data")
    
  }

  if(norecrut){
    
    for (sp in index.noRecrut){
      DataTmp=subset(DataNoRecrut,Id.sp==sp)
      datapredR=cbind(1,subset(DataTmp,select=VarRecrut))
      indexlog=which(substr(colnames(datapredR),1,3)=="NbA")
      if (length(indexlog)>0) datapredR[,indexlog]=log(1+datapredR[,indexlog])
      rr=exp(as.matrix(datapredR)%*%as.matrix(ParamGpRecruit))
      GpRecrut[GpRecrut$Id.sp==sp,2]=which.max(apply(rr,2,function(x)sum(dpois(DataTmp$NbRecr,x,log=T))))
     
    }
    
    rm(DataTmp,datapredR,indexlog,rr)
  }
  
  if (any(GpRecrut[,2]==0)){
    browser()
  }
  
  GpRecrut=GpRecrut$cl1
  NbGpRecruit=max(GpRecrut)

  # Formatting parameters
  
  if (length(VarRecrut)>0){
    ParamGpRecruit=as.matrix(ParamGpRecruit)
    ParamGpRecrut=t(ParamGpRecruit[,GpRecrut])
  }else{ParamGpRecrut=ParamGpRecruit[GpRecrut]}
  
 

  SimRecrut$Param=ParamGpRecrut
  SimRecrut$NomsDataPred=NomsDataPred
  SimRecrut$VarRecrut=VarRecrut  
  
  
  SimRecrut$Func <-function(Eff.cur,SimRecrut){
    
    Effectifs.totaux=apply(Eff.cur,1,sum)
    STTC=Effectifs.totaux*SimRecrut$CDSTB$ST
    datapred=cbind(data.frame(t(log(1+Eff.cur)),t(STTC)))
    if (length(SimRecrut$VarRecrut)>0){
      colnames(datapred)=SimRecrut$NomsDataPred
      datapred=cbind(1,subset(datapred,select=SimRecrut$VarRecrut))
      CL=apply(datapred*SimRecrut$Param,1,sum)
    }else{CL=SimRecrut$Param}
    In=rpois(length(CL),exp(CL))
    
    
    if (sum(is.na(In))>0) {

      stop("the number of trees reached infinity, you could change the recruitment model to fix it")}
    
    return(In)  
  }
  
  return(SimRecrut)  
}