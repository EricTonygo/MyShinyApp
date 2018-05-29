RecruitmentFlexmix <-function(Data,ClassesDiam,VarModel,UserListApartSpecies,criterion="BIC"){

  
library(flexmix)  
  
  
  
  
##################################################################################
##################################################################################

######################
# Internal functions #
######################

FormatDataRecruitmentFlexmix <- function(Data,ClassesDiam,NbClasse,DelaysSensus){
  library(data.table)
  
  # Liste des "esp?ces"
  ListeIdsp=levels(Data$Id.sp)
  
  Data=data.table(Data,key="Id.zone,Id.campagne0,Id.sp")
  
  # D?coupage en classe
  Data[,Classe1:=findInterval(Diam1,ClassesDiam)]
  Data[,Classe0:=findInterval(Diam0,ClassesDiam)]
 
 
  ######
  # Calcul des tables de comp?tition
  #######
  
  Data[,Effect0:=as.numeric(Classe0>0)]
  
  NomsEffC=paste("EffC",ClassesDiam,sep='')
  NomsNbArbres=paste("NbArbres",ClassesDiam,sep='')

  
  # construction des commandes pour extraire les tables de competition
 
  if (length(DelaysSensus)>1){
    
    ExprRecrut="Data[Nb.period==1,list(sum(Effect0[Classe0==1])/unique(Surface.zone)"
    ExprRecrut=paste0(ExprRecrut,paste0(",sum(Effect0[Classe0==",2:NbClasse,"])/unique(Surface.zone)",collapse=''))
    ExprRecrut=paste0(ExprRecrut,"),by='Id.zone,Id.campagne0,Id.sp']")
    
    ExprCompet="Data[Nb.period==1,list(sum(Effect0[Classe0==1])/unique(Surface.zone)"
    ExprCompet=paste0(ExprCompet,paste0(",sum(Effect0[Classe0==",2:NbClasse,"])/unique(Surface.zone)",collapse=''),",unique(Nb.period)),by='Id.zone,Id.campagne0']")   
    
    ExprRecrut2="Data[Nb.period>1,list((sum(Effect0[Classe0==1])/unique(Surface.zone)*(unique(Nb.period)+1)+sum(Effect0[Classe1==1])/unique(Surface.zone)*(unique(Nb.period)-1))*0.5"
    ExprRecrut2=paste0(ExprRecrut2,paste0(",(sum(Effect0[Classe0==",2:NbClasse,"])/unique(Surface.zone)*(unique(Nb.period)+1)+sum(Effect0[Classe1==",2:NbClasse,"])/unique(Surface.zone)*(unique(Nb.period)-1))*0.5",collapse=''))
    ExprRecrut2=paste0(ExprRecrut2,"),by='Id.zone,Id.campagne0,Id.sp']")
    
    ExprCompet2="Data[Nb.period>1,list((sum(Effect0[Classe0==1])/unique(Surface.zone)*(unique(Nb.period)+1)+sum(Effect0[Classe1==1])/unique(Surface.zone)*(unique(Nb.period)-1))*0.5"
    ExprCompet2=paste0(ExprCompet2,paste0(",(sum(Effect0[Classe0==",2:NbClasse,"])/unique(Surface.zone)*(unique(Nb.period)+1)+sum(Effect0[Classe1==",2:NbClasse,"])/unique(Surface.zone)*(unique(Nb.period)-1))*0.5",collapse=''))
    ExprCompet2=paste0(ExprCompet2,",unique(Nb.period)),by='Id.zone,Id.campagne0']")
    
    Compet=eval(parse(text=ExprCompet)) 
    Compet2=eval(parse(text=ExprCompet2)) 
    Compet=rbind(Compet,Compet2)
    setnames(Compet,3:ncol(Compet2),c(NomsEffC,"Nb.period"))
    
    CompetRecrut=eval(parse(text=ExprRecrut))
    setnames(CompetRecrut,4:ncol(CompetRecrut),NomsNbArbres)
  
    CompetRecrut2=eval(parse(text=ExprRecrut2))
    setnames(CompetRecrut2,4:ncol(CompetRecrut2),NomsNbArbres)
  
    CompetRecrut=rbind(CompetRecrut,CompetRecrut2)
    
  }else{
  
    ExprRecrut="Data[,list(sum(Effect0[Classe0==1])/unique(Surface.zone)"
    ExprRecrut=paste0(ExprRecrut,paste0(",sum(Effect0[Classe0==",2:NbClasse,"])/unique(Surface.zone)",collapse=''))
    ExprRecrut=paste0(ExprRecrut,"),by='Id.zone,Id.campagne0,Id.sp']")
    
    ExprCompet="Data[,list(sum(Effect0[Classe0==1])/unique(Surface.zone)"
    ExprCompet=paste0(ExprCompet,paste0(",sum(Effect0[Classe0==",2:NbClasse,"])/unique(Surface.zone)",collapse=''),",unique(Nb.period)),by='Id.zone,Id.campagne0']")                  
    
    Compet=eval(parse(text=ExprCompet)) 
    setnames(Compet,3:ncol(Compet),c(NomsEffC,"Nb.period"))
    
    CompetRecrut=eval(parse(text=ExprRecrut))
    setnames(CompetRecrut,4:ncol(CompetRecrut),NomsNbArbres)
  
    
    
  }      
  setkey(Compet,Id.zone,Id.campagne0)
  setkey(CompetRecrut,Id.zone,Id.campagne0,Id.sp)
    # Calcul des tables de comp?tition

  
  #################
  # Mise en forme des donn?es pour le recrutement
  ################
  
  Data[,Recrut:=as.numeric(Diam0==0 & Diam1>0)]

  outRecruts=Data[,list(NbRecruts=sum(Recrut)),by="Id.zone,Id.campagne0,Id.sp"]
  CompletRecrut=expand.grid(Id.zone=levels(Data$Id.zone),Id.sp=levels(Data$Id.sp))       
  CompletRecrut=data.table(CompletRecrut,key="Id.zone,Id.sp")
  CompletRecrut[,toto:=0]

  outRecruts=merge(outRecruts,CompletRecrut,all.x=T,all.y=T)

  outRecruts=merge(outRecruts,Compet,all.x=F,all.y=T,by=c("Id.zone","Id.campagne0"))
    setkey(outRecruts,Id.zone,Id.campagne0,Id.sp)
  
  outRecruts=merge(outRecruts,CompetRecrut,all.x=T)
  ParcelleSurface=unique(subset(Data,select=c("Id.zone","Surface.zone")))
  outRecruts=merge(outRecruts,ParcelleSurface,all.x=T)
  outRecruts$OS=log(outRecruts$Surface.zone)
 
  outRecruts=data.frame(outRecruts)


  outRecruts=subset(outRecruts,select=c("NbRecruts",NomsNbArbres,NomsEffC,"Id.sp","OS"))
  
  outRecruts[is.na(outRecruts)]=0
  return(outRecruts)
  
  
}

##################################################################################
UpdateListApartSpecies<-function(UserListApartSpecies,index){
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
    res=flexmix(Model,data=Data,k=1,model=FLXMRglm(family=Family,offset=Data$OS))
    crit=ICL(res)
  }else{
    res=glm(Model,data=Data,family=Family,offset=OS )
    crit=get(criterion)(res)
  }
  nbgp=2
  
  rm(res)
  gc()
  
  while(Arret){
    nbgp.new=nbgp-1
    crit.new=crit+10000
    for (i in 1:NbTry){
      res=try(flexmix(ModelGp,data=Data,k=nbgp,model=FLXMRglm(family=Family,offset=Data$OS)),silent=T)        
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
    ModelTmp=glm(Model,data=DataTmp,offset=OS,family=Family)
    ParamGp[,j]=as.numeric(ModelTmp$coef)
  } 
  
  ParamGp=ParamGp[,1:j]
  
  Data=unique(subset(Data,select=c(Id.sp,gp2)))
  Gp=merge(Gp,Data,all.x=T,all.y=F,sort=F)
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
    ModelTmp=glm(Model,data=DataTmp,offset=OS,family=Family)  
    ParamGp[,compteur]=as.numeric(ModelTmp$coef) 
    Data=subset(Data,!Id.sp%in%ListApartSpecies[[sp]])
    
  }
  return(list(Gp=Gp,ParamGp=ParamGp,Data=Data,compteur=compteur))
}





##################################################################################
##################################################################################
  
  
  

  NomsEffC=paste("EffC",ClassesDiam,sep='')
  NomsNbArbres=paste("NbArbres",ClassesDiam,sep='')
  NomsDataPred=c(NomsNbArbres,NomsEffC)


  
 
    
  # generating models
  


    if (length(VarModel)>0){   
      exprRecrut=paste("NbRecruts~ 1",paste("+",VarModel,collapse=" "))
    }else{exprRecrut="NbRecruts~ 1 "} 


  
 
  modelRecrut=eval(parse(text=exprRecrut)) 
  modelRecrutGp=eval(parse(text=paste(exprRecrut,"|Id.sp")))


  SimRecrut=list()
  

 
  # Formatting Data  
  DelaysSensus=unique(Data$Nb.period)
  NbClasse=length(ClassesDiam)
  DataRecrut=FormatDataRecruitmentFlexmix(Data,ClassesDiam,NbClasse,DelaysSensus)
  ListeIdsp=levels(Data$Id.sp)
  GpRecrut=data.frame(Id.sp=ListeIdsp)
  GpRecrut$gp=0 

  rm(Data)
  gc()

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
  ListApartSpecies=UpdateListApartSpecies(UserListApartSpecies,index.noRecrut)
  

  
  # clustering species with many recruits
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
      datapredR=cbind(1,subset(DataTmp,select=c(VarModel,"OS")))
      rr=exp(as.matrix(datapredR)%*%rbind(as.matrix(ParamGpRecruit),1))
      GpRecrut[GpRecrut$Id.sp==sp,2]=which.max(apply(rr,2,function(x)sum(dpois(DataTmp$NbRecr,x,log=T))))
     
    }
 
    rm(DataTmp,datapredR,rr)
  }
  
  if (any(GpRecrut[,2]==0)){
    browser()
  }
  

  GpRecrut=merge(data.frame(Id.sp=ListeIdsp),GpRecrut,sort=F)
 

  # Formatting parameters
  


   # if (length(VarModel)>0){
   #   ParamGpRecruit=as.matrix(ParamGpRecruit)
   #   ParamGpRecrut=t(ParamGpRecruit[,GpRecrut])
   # }else{ParamGpRecrut=ParamGpRecruit[GpRecrut]}
  

  SimRecrut$GpRecrut=GpRecrut
  SimRecrut$ParamGp=ParamGpRecruit
  SimRecrut$NomsDataPred=NomsDataPred
  SimRecrut$VarModel=VarModel  
  # surface=unique(DataRecrut$Surface.zone)
  # if (length(surface)>1) surface=1
  # if (length(DelaysSensus)>1) DelaysSensus=1
  # SimRecrut$Intercept.period.surface=DelaysSensus*surface
  # 
  SimRecrut$Func <-function(Eff.cur,SimRecrut,Surface){
    
    indexspgp=merge(data.frame(Ordre=1:ncol(Eff.cur),Id.sp=colnames(Eff.cur)),SimRecrut$GpRecrut,sort=F,all.x=T,all.y=F)
    indexspgp=indexspgp$cl1[order(indexspgp$Ordre)]
    if (length(SimRecrut$VarModel)>0){
      
      Effectifs.totaux=apply(Eff.cur,1,sum)
      datapred=cbind(data.frame(t(Eff.cur),t(Effectifs.totaux)))/Surface
      colnames(datapred)=SimRecrut$NomsDataPred
      datapred=cbind(1,subset(datapred,select=SimRecrut$VarModel))
      CL=apply(datapred*t(SimRecrut$ParamGp[,indexspgp]),1,sum)+log(Surface)
    }else{CL=SimRecrut$ParamGp}
    
    In=rpois(length(CL),exp(CL))
  
    
    if (sum(is.na(In))>0) {

      stop("the number of trees reached infinity, you could change the recruitment model to fix it")}
    
    return(In)  
  }
  

  
  return(SimRecrut)  
}