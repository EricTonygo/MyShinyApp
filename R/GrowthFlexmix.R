GrowthFlexmix <- function(Data,ClassesDiam,UserListApartSpecies,criterion="BIC"){
  
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
  
  

  
  ##################################################################
  
  FormatDataGrowthFlexmix<-function(Data,ClassesDiam,NbClasse,DelaysSensus){
    
    
    
    library(data.table)
    
    # Liste des "esp?ces"
    ListeIdsp=levels(Data$Id.sp)
    
    # D?coupage en classe
    Data$Classe1=findInterval(Data$Diam1,ClassesDiam)
    Data$Classe0=findInterval(Data$Diam0,ClassesDiam)
    
    
    
    Data=data.table(Data,key="Id.zone,Id.campagne0,Id.sp")
    Data[,Effect0:=as.numeric(Classe0>0)]
    
    NomsEffC=paste("EffC",ClassesDiam,sep='')
    
    ############################
    # Mise en forme des donn?es de dynamique
    ########################
    
    DataDyn=subset(Data,Classe0>0)
    
    DataDyn[,mort:=as.numeric(Diam1==0)]
    DataDyn[,monte:=as.numeric(Classe1>Classe0)]
    DataDyn[,reste:=as.numeric(Classe1==Classe0)] 
    
    
    
    DataDyn[,verif:=mort+monte+reste]
    if (DataDyn[,sum(verif==0)]>0) DataDyn=subset(DataDyn,verif==1)
    
    
    setkey(DataDyn,Id.zone,Id.sp,Id.campagne0)
    tmpDyn=DataDyn[,list(mort=sum(mort),reste=sum(reste),monte=sum(monte)),by="Id.zone,Id.campagne0,Id.sp,Classe0"]
    
    tmpDyn[,FacteurClasse0:=as.factor(Classe0)]
    setkey(tmpDyn,FacteurClasse0)
    FacteurClassesDiam=levels(tmpDyn$FacteurClasse0)
    
    DataDynOut=vector("list",NbClasse)
    
    
    if (length(DelaysSensus)==1){
      
      
      
      # construction des commandes pour extraire les tables de competition
      Expr="Data[,list(sum(Effect0[Classe0==1])/unique(Surface.zone)"
      Expr=paste0(Expr,paste0(",sum(Effect0[Classe0==",2:NbClasse,"])/unique(Surface.zone)",collapse=''),"),by='Id.zone,Id.campagne0']")
      
      # Calcul des tables de comp?tition
      Compet=eval(parse(text=Expr)) 
      setnames(Compet,3:ncol(Compet),c(NomsEffC))
      
      
      for (i in 1:length(FacteurClassesDiam)){
        
        tmpDynClasse=tmpDyn[FacteurClassesDiam[i],mult="all"]
        setkey(tmpDynClasse,Id.zone,Id.campagne0)
        tmpCompet=subset(Compet,select=c("Id.zone","Id.campagne0",NomsEffC))
        setkey(tmpCompet,Id.zone,Id.campagne0)
        tmpDynClasse=merge(tmpDynClasse,tmpCompet)
        tmpDynClasse=subset(tmpDynClasse,select=c("mort","reste","monte",NomsEffC,"Id.sp","Id.zone","Id.campagne0"))
        tmpDynClasse=subset(tmpDynClasse,monte+reste>0)
        DataDynOut[[i]]=data.frame(tmpDynClasse)
        
      }
    }else{
      compteur=0
      Noms=NULL
      for (j in sort(DelaysSensus)){
        Noms=c(Noms,paste(NomsEffC,j,sep="."))
      }
      
   
      for (j in sort(DelaysSensus)){
        compteur=compteur+1      
        # construction des commandes pour extraire les tables de competition
        if (j==1){
          
          
          
          Expr="Data[Nb.period==j,list(sum(Effect0[Classe0==1])/unique(Surface.zone)"
          Expr=paste0(Expr,paste0(",sum(Effect0[Classe0==",2:NbClasse,"])/unique(Surface.zone)",collapse=''),"),by='Id.zone,Id.campagne0']")
          
          # Calcul des tables de comp?tition
          Compet=eval(parse(text=Expr)) 
          Compet=cbind(Compet,matrix(nrow=nrow(Compet),ncol=(length(NomsEffC)*(max(DelaysSensus)-j))))
          Compet$Nb.period=j
          setnames(Compet,3:(ncol(Compet)-1),c(Noms))
          
        }else{
          
          Expr="Data[Nb.period==j,list(sum(Effect0[Classe0==1])/unique(Surface.zone)" 
          Expr=paste0(Expr,paste0(",sum(Effect0[Classe0==",2:NbClasse,"])/unique(Surface.zone)",collapse=''))
         
          for (k in 2:j){
            Expr=paste0(Expr,paste0(",sum(Effect0[Classe0==",1:NbClasse,"])/unique(Surface.zone)*",(j-k+1)/j,"+sum(Effect0[Classe1==",1:NbClasse,"])/unique(Surface.zone)*",(k-1)/j,collapse=''))
          }  
          Expr=paste0(Expr,"),by='Id.zone,Id.campagne0']")  
          
          # Calcul des tables de comp?tition
          
          CompetTmp=eval(parse(text=Expr)) 
          if (j<max(DelaysSensus)) CompetTmp=cbind(CompetTmp,matrix(nrow=nrow(CompetTmp),ncol=(length(NomsEffC)*(max(DelaysSensus)-j))))
          CompetTmp$Nb.period=j
          setnames(CompetTmp,3:(ncol(CompetTmp)-1),c(Noms))
          Compet=rbind(Compet,CompetTmp)
          
          
        }  
      }
      
      
        for (i in 1:length(FacteurClassesDiam)){
          
          tmpDynClasse=tmpDyn[FacteurClassesDiam[i],mult="all"]
          setkey(tmpDynClasse,Id.zone,Id.campagne0)
          tmpCompet=subset(Compet,select=c("Id.zone","Id.campagne0",Noms,"Nb.period"))
          setkey(tmpCompet,Id.zone,Id.campagne0)
          tmpDynClasse=merge(tmpDynClasse,tmpCompet)
          tmpDynClasse=subset(tmpDynClasse,select=c("mort","reste","monte",Noms,"Id.sp","Id.zone","Id.campagne0","Nb.period"))
          tmpDynClasse=subset(tmpDynClasse,monte+reste>0)
          DataDynOut[[i]]=data.frame(tmpDynClasse)
          
        }
        
          
      
      
      
    }
    
    
    return(DataDynOut)
  }
  
  
  
  ##################################################################
  
  
  UpdateListApartSpecies<-function(UserListApartSpecies,index){
    # updating list species managed apart
    ListApartSpecies=UserListApartSpecies
    if (length(ListApartSpecies)>0 & length(index)>0){   
      index.apart=sapply(ListApartSpecies,function(x) max(x%in%index))
      if (sum(index.apart)>0){
        for (ll in sort(which(as.logical(index.apart)),decreasing=T)){
          index.apart2=!ListApartSpecies[[ll]]%in%index
          if (sum(index.apart2)==0){ListApartSpecies[[ll]]=NULL}
          else{ListApartSpecies[[ll]]=ListApartSpecies[[ll]][index.apart2]}
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
    
    
    
    Data=unique(subset(Data,select=c(Id.sp,gp2)))
    Gp=merge(Gp,Data,all.x=T,all.y=F)
    Gp[!is.na(Gp$gp2),ncol(Gp)-1]=Gp[!is.na(Gp$gp2),ncol(Gp)]  
    Gp$gp2=NULL  
    
    colnames(Gp)[ncol(Gp)]=paste("cl",ncol(Gp)-1,sep="")
    
    return(list(Gp=Gp,ParamGp=ParamGp))
  }
  ##############################
  
  
  EMclassifLogitIter=function(Data,Data2,nbgp,NomsEffC,seuil.converge=1e-6,maxiter=15){
    
    Delta=10000
    
 
    ListeSp=unique(Data$Id.sp)
    NbSp=length(ListeSp)

    ll.sp=matrix(nrow=NbSp,ncol=nbgp)
      
    DataTmp=vector("list",NbSp )
    for (sp in 1:NbSp){
     
     for (j in unique(Data$Nb.period)) DataTmp[[sp]][[j]]=subset(Data,Id.sp==ListeSp[sp] & Nb.period==j)
    }
    
    wsp=matrix(0,nrow=NbSp,ncol=nbgp)
    wsp=data.frame(Id.sp=ListeSp,wsp)
  
   
    par=matrix(ncol=nbgp,nrow=(length(NomsEffC)+1))

    
    iterboucle=0
    stop=F
    wspgp=subset(Data,select="Id.sp")
    
    
    
    # Initialisation 
    
        priors=rep(1/nbgp,nbgp)
        
          
            check.par=T
            
          while(check.par){
          
            ListeSpInit=ListeSp
            NbSpGp=rep(1,nbgp)+rmultinom(1,(NbSp-nbgp),prob=priors)
            par.ok=0
             for (g in 1:nbgp){
    
              indexspinit=sample(ListeSpInit,NbSpGp[g])
              DataTmpInit=subset(Data,Id.sp%in%indexspinit)
              ListeSpInit=ListeSpInit[!ListeSpInit%in%indexspinit]
            
    
             par0=InitParamLogitIterModel(DataTmpInit,NomsEffC)
             DataTmpInit2=list()
             for (j in unique(DataTmpInit$Nb.period)) DataTmpInit2[[j]]=subset(DataTmpInit,Nb.period==j)
             restmp=try(optim(par0,LogLikLogitIter2,gr=NULL,DataTmpInit2,NomsEffC,method="BFGS",control=list(fnscale=-1)),silent = T)
             if (!is(restmp,"try-error")){
                          par.ok=par.ok+1
                          par[,g]=restmp$par}
            }
             if (par.ok==nbgp) check.par=F
    
        }
    
    
 
    
    ll=LogLikLogitIterMix(priors,par,Data,NomsEffC)
    Iter=ll

  
     while(Delta>seuil.converge){

       iterboucle=iterboucle+1
       
       for (sp in 1:NbSp){
         for (g in 1:nbgp){
           ll.sp[sp,g]=LogLikLogitIter2(par[,g],DataTmp[[sp]],NomsEffC)
         }
         
         
         for (g in 1:nbgp){
           wsp[sp,g+1]=priors[g]/(priors[g]+sum(priors[-g]*exp(ll.sp[sp,-g]-ll.sp[sp,g])))
           
         }
         
       }
      
        wspgp=merge(data.frame(Id.sp=wspgp$Id.sp),wsp,by="Id.sp",all.x=T,all.y=F)
  
   
        
       for (g in 1:nbgp){
             Data$W=wspgp[,g+1]
            for (j in unique(Data$Nb.period)) Data2[[j]]=subset(Data,Nb.period==j)
       
          restmp=try(optim(par[,g],LogLikLogitIter2,gr=NULL,Data2,NomsEffC,W=T,method="BFGS",control=list(fnscale=-1)),silent=T)
          if (is(restmp,"try-error")){
            stop=T
          }else par[,g]=restmp$par
  
       
      }
      
      
       if (!stop){
         
        priors=apply(wspgp[,-1],2,mean)
        ll.new=LogLikLogitIterMix(priors,par,Data,NomsEffC)

         
         Delta=abs((ll.new-ll)/ll)
        if (iterboucle>maxiter) Delta=0
        
         ll=ll.new
       }else{
         ll=-Inf
         Delta=0
       }
        if (is.na(Delta)) Delta=0
    }
 
    wsp$gp=apply(subset(wsp,select=-1),1,which.max)
   
    return(list(groups=subset(wsp,select=c(Id.sp,gp)),ll=ll))
    
    
  }
  
  
  
  
  
  
  
  
  
  
  ##################################################################
  
  clusteringLogitIter<-function(Gp,ParamGp,compteur,Data,NomsEffC){
    
    Data$Id.sp=Data$Id.sp[,drop=T]   
    Arret=T
    ListeSp=levels(Data$Id.sp)
    groups=data.frame(Id.sp=ListeSp,gp=1)
 
    
    if (criterion=="ICL"){
      print("ICL non supporté avec inventaire irrégulier, remplacé par BIC")
      criterion="BIC"
    }
    par0=InitParamLogitIterModel(Data,NomsEffC)
    
    Data2=list()
    for (j in unique(Data$Nb.period)) Data2[[j]]=subset(Data,Nb.period==j)
   
    res=optim(par0,LogLikLogitIter2,gr=NULL,Data2,NomsEffC,method="BFGS",control=list(fnscale=-1))
    param=matrix(res$par,ncol=1)
    
    if (criterion=="AIC"){
      crit=-2*res$value+2*length(NomsEffC)
    }else{
      crit=-2*res$value+log(nrow(Data))*length(NomsEffC)
    }
   
   
    nbgp=2
   
    rm(res)
    gc()
    Tol.em=0.01
    MaxIter.em=25

    while(Arret){
      
      ll=-Inf
      res=NULL
      NbTry=4*nbgp
      
      for (i in 1:NbTry){
        
         res.new=EMclassifLogitIter(Data,Data2,nbgp,NomsEffC,Tol.em,MaxIter.em)
         if (res.new$ll>ll){
           res=res.new
           if (max(res$groups$gp)==nbgp) ll=res.new$ll
         } 
      }
      ll=0
      paramtmp=matrix(nrow=(length(NomsEffC)+1),ncol=nbgp)
      for (g in 1:nbgp){
      
        indexsp=subset(res$groups,gp==g,select="Id.sp")
        if (nrow(indexsp)>0){
            DataTmp=subset(Data,Id.sp%in%indexsp$Id.sp)
            par0=InitParamLogitIterModel(DataTmp,NomsEffC)
            resgptmp=try(optim(par0,LogLikLogitIter,gr=NULL,DataTmp,NomsEffC,method="BFGS",control=list(fnscale=-1)),silent=T)
              if (!is(resgptmp,"try-error")) {
                paramtmp[,g]=resgptmp$par
                ll=ll+resgptmp$value
               }else{ll=-Inf}
        }else{ll=-Inf}
      }
      res$ll=ll
      
      if (criterion=="AIC"){
        crit.new=-2*res$ll+2*nbgp*(length(NomsEffC)+1)
      }else{
        crit.new=-2*res$ll+log(nrow(Data))*nbgp*(length(NomsEffC)+1)
      }
      
   
      
         if (crit.new>=crit[(nbgp-1)]){
           Arret=F
           crit=rbind(1:nbgp,c(crit,crit.new))
           rownames(crit)=c("Nb group",criterion)
           }
        else{
          param=paramtmp
          crit=c(crit,crit.new)
          groups=res$groups
          nbgp=nbgp+1
          rm(res) 
          gc()
        }

    }
    print(crit)

    
    #  getting models parameters
    ParamGp[,(compteur+1):(compteur+max(groups$gp))]=param
    
    groups$gp2=groups$gp+compteur
    groups$gp=NULL
    
    Gp=merge(Gp,groups,all.x=T,all.y=F)
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
  
  ##############################
  
  GpParamApartSpeciesLogitIter<-function(Gp,ParamGp,ListApartSpecies,NomsEffC,Data,compteur){
    
    for (sp in 1:length(ListApartSpecies)){
      
      
      compteur=compteur+1
      Gp[Gp$Id.sp%in%ListApartSpecies[[sp]],ncol(Gp)]=compteur
      
      DataTmp=subset(Data,Id.sp%in%ListApartSpecies[[sp]])
    
      
      par0=InitParamLogitIterModel(DataTmp,NomsEffC)
      ModelTmp=optim(par0,LogLikLogitIter,gr=NULL,DataTmp,NomsEffC,method="BFGS",control=list(fnscale=-1))
      
      
      ParamGp[,compteur]=as.numeric(ModelTmp$par) 
      
      Data=subset(Data,!Id.sp%in%ListApartSpecies[[sp]])
      }
      
    
    return(list(Gp=Gp,ParamGp=ParamGp,Data=Data,compteur=compteur))
  }
  
  ################################
  
  logit<-function(p) log(p/(1-p))
  
  invlogit<-function(x) 1/(1+exp(-x))

  
  ################################
  
  LogLikLogitIter = function(param,Data,NomVar,W=F){
    
    
   
    NbIter=unique(Data$Nb.period)
    
    ll=0
    
    for (j in 1:length(NbIter)){
      
      proba=1
      DataTmp=subset(Data,Nb.period==NbIter[j])
      
      for (i in 1:NbIter[j]){
        VarModel=paste(NomVar,i,sep=".")

        Covar2=param[1]
        for (k in 2:length(param)){
          Covar2=Covar2+param[k]*DataTmp[,colnames(DataTmp)==VarModel[k-1]]
        }
        proba=proba*(1-invlogit(Covar2))
      }
      
      if (!W) ll=ll+sum(DataTmp$monte*log(1-proba)+DataTmp$reste*log(proba))
      else ll=ll+sum((DataTmp$monte*log(1-proba)+DataTmp$reste*log(proba))*DataTmp$W)
      
    }
     
    
    
    return(ll)
  }
  
  ################################
  
  LogLikLogitIterMix = function(priors,param,Data,NomVar){
    
  if (priors[1]<1e-6) {
    index=which.max(priors)
    tmp=priors[1]
    priors[1]=priors[index]
    priors[index]=tmp
    
    tmp=param[,index]
    param[,1]=param[,index]
    param[,index]=tmp
  }  
    
    NbIter=unique(Data$Nb.period)
    
    ll.Data=matrix(ncol=length(priors),nrow=nrow(Data))
 
    
    for (g in 1:length(priors)){
        
           compteur=1
          for (j in 1:length(NbIter)){
            
            proba=1
            DataTmp=subset(Data,Nb.period==NbIter[j])
            
            for (i in 1:NbIter[j]){
              VarModel=paste(NomVar,i,sep=".")
              
              Covar2=param[1,g]
              for (k in 2:nrow(param)){
                Covar2=Covar2+param[k,g]*DataTmp[,colnames(DataTmp)==VarModel[k-1]]
              }
              proba=proba*(1-invlogit(Covar2))
            }
        
            compteur1=compteur+length(proba)-1 
          ll.Data[compteur:compteur1,g]=DataTmp$monte*log(1-proba)+DataTmp$reste*log(proba)
          compteur=compteur1+1  
              
          }
      
    }

    tmp=matrix(exp(ll.Data[,-1]-ll.Data[,1]),ncol=(length(priors)-1))
    tmp2=0
   
    for (g in 2:length(priors)){tmp2=tmp2+tmp[,g-1]*priors[g]}
    tmp2=1+tmp2/priors[1]
    
    ll=nrow(Data)*log(priors[1])+sum(ll.Data[,1])+sum(log(tmp2))
   
  
    return(ll)
  }
  
  
  ################################
  
  LogLikLogitIter2 = function(param,Data,NomVar,W=F){
    
    

    NbIter=length(Data)
    
    ll=0
    
    for (j in 1:NbIter){
      
      proba=1
      nbperiod=Data[[j]]$Nb.period[1]
      
      if (!is.na(nbperiod)) {
      for (i in 1:nbperiod){
        VarModel=paste(NomVar,i,sep=".")
        
        Covar2=param[1
                     ]
        for (k in 2:length(param)){
          Covar2=Covar2+param[k]*Data[[j]][,colnames(Data[[j]])==VarModel[k-1]]
        }
        proba=proba*(1-invlogit(Covar2))
      }
      }
      
      if (W) ll=ll+sum((Data[[j]]$monte*log(1-proba)+Data[[j]]$reste*log(proba))*Data[[j]]$W) 
      else ll=ll+sum(Data[[j]]$monte*log(1-proba)+Data[[j]]$reste*log(proba))
       
      
    }
    
    
    
    return(ll)
  }
  
  
  
 
  
  ############################
  

  
  
  InitParamLogitIterModel = function(Data,NomVar,W=F){
    
   
    VarModel=paste(NomVar,1,sep=".")

    modelglm=paste0("cbind(monte,reste)~",paste(VarModel,collapse="+"))
    if (!W) reslm=try(glm(modelglm,data=Data,family = binomial),silent=T)
    else reslm=try(glm(modelglm,data=Data,family = binomial,weights=W),silent=T)
    
    if (!is(reslm,"try-error") & reslm$converged) par=reslm$coefficients
    else par=rep(0,(length(NomVar)+1))
    return(par)    
    
 
  }
  
  
  
  ##################################################################################
  ##################################################################################
  
  DelaysSensus=unique(Data$Nb.period)
  NbClasse=length(ClassesDiam)
  
  

  
  # Formatting Data                     
  DataDyn=FormatDataGrowthFlexmix(Data,ClassesDiam,NbClasse,DelaysSensus)
  

  
  
  # generating models  
  
  
  NomsEffC=paste("EffC",ClassesDiam,sep='')

  
  exprGrowth=paste0("cbind(monte,reste)~",paste(NomsEffC,collapse="+"))
  modelGrowth=eval(parse(text=exprGrowth))
  modelGrowthGp=eval(parse(text=paste(exprGrowth,"|Id.sp")))
  NbParamGrowth=NbClasse+1
  
  SimGrowth=list()

 
  
  ################
  # clustering growth processes
  ################
  
  ListeIdsp=levels(Data$Id.sp)
  GpGrowth=data.frame(Id.sp=ListeIdsp)
  ParamGpGrowth=list()
  
 
  
  # Boucle sur les classes de diam?tre pour faire les groupes
  for (i in 1:(NbClasse-1)){
       
    #
    # clustering growth process
    #
    
    DataDynTmp=DataDyn[[i]]
    compteur.growth=0
    GpGrowth[,i+1]=0 
    ParamGpGrowth[[i]]=matrix(0,nrow=NbParamGrowth,ncol=length(levels(DataDynTmp$Id.sp)))

 
    # indentifying species with no growth
    
    index.noGrowth=names(which(tapply(DataDynTmp$monte,DataDynTmp$Id.sp,sum)==0))
    
    if (length(index.noGrowth)>0){
      DataNoGrowth=subset(DataDynTmp,Id.sp%in%index.noGrowth)
      DataDynTmp=subset(DataDynTmp,!Id.sp%in%index.noGrowth)
      noGrowth=T
    }else{
      noGrowth=F
    }
    
    
  
    # updating list species managed apart
    ListApartSpecies=UpdateListApartSpecies(UserListApartSpecies,index.noGrowth)
    
    
    if (nrow(DataDynTmp)>0){
      # speceis managed apart
      
      if (length(ListApartSpecies)>0){
        if (length(DelaysSensus)==1){
           res=GpParamApartSpecies(GpGrowth,ParamGpGrowth[[i]],ListApartSpecies,modelGrowth,"binomial",DataDynTmp,compteur.growth)
        }else{
          res=GpParamApartSpeciesLogitIter(GpGrowth,ParamGpGrowth[[i]],ListApartSpecies,NomsEffC,DataDynTmp,compteur.growth)
        }
        DataDynTmp=res$Data
        GpGrowth=res$Gp
        ParamGpGrowth[[i]]=res$ParamGp
        compteur.growth=res$compteur      
      }
      
      if (nrow(DataDynTmp)>0){
        if (length(DelaysSensus)==1){
      
          res=clustering(GpGrowth,ParamGpGrowth[[i]],compteur.growth,DataDynTmp,modelGrowth,modelGrowthGp,"binomial","growth")
        }else{
         
          res=clusteringLogitIter(GpGrowth,ParamGpGrowth[[i]],compteur.growth,DataDynTmp,NomsEffC)
        }
       
        GpGrowth=res$Gp
       
        ParamGpGrowth[[i]]=matrix(res$ParamGp[,1:max(GpGrowth[,i+1])],ncol=max(GpGrowth[,i+1]))
     
      }
      
    }else{
      stop(paste0("Growth model cannot be infered in diameter class ",i," because there is no growth in the data"))
    }    
    
    colnames(GpGrowth)[i+1]=paste("cl",i,sep="")
    
    
    if(noGrowth){ 
      
      for (sp in index.noGrowth){
        
        DataTmp=subset(DataNoGrowth,Id.sp==sp)
        
        if (length(DelaysSensus)==1){
        datapred=cbind(1,subset(DataTmp,select=NomsEffC))
        prob=1/(1+exp(-as.matrix(datapred)%*%as.matrix(ParamGpGrowth[[i]])))            
        GpGrowth[GpGrowth$Id.sp==sp,i+1]=which.max(apply(prob,2,function(x)sum(dbinom(DataTmp$monte,(DataTmp$reste+DataTmp$monte),x,log=T))))        
        }else{
    
          ll.sp=rep(0,ncol(ParamGpGrowth[[i]]))
    
          for (g in 1:length(ll.sp)){
            ll=try(LogLikLogitIter(ParamGpGrowth[[i]][,g],DataTmp,NomsEffC))
            if (!is(ll,"try-error")) ll.sp[g]==ll 
            else browser()
          }
          GpGrowth[GpGrowth$Id.sp==sp,i+1]=which.max(ll.sp)
        }
        
        }
    }
    
    #
    # species whithout trees in the diameter class
    #
    
    if (any(GpGrowth[,i+1]==0)){
      
      DataDyn[[i]]$Tot=DataDyn[[i]]$reste+DataDyn[[i]]$monte+DataDyn[[i]]$mort
      tmp=tapply(DataDyn[[i]]$Tot,DataDyn[[i]]$Id.sp,sum)
      tmp=data.frame(Id.sp=names(tmp),Eff=as.numeric(tmp))
      tmp=merge(tmp,GpGrowth,by="Id.sp")
      tmp=tapply(tmp[,2],tmp[,ncol(tmp)],sum)
      GpGrowth[GpGrowth[,i+1]==0,i+1]=as.numeric(names(tmp)[which.max(as.numeric(tmp))])
    }
    
    DataDyn[[i]]="toto"
    gc()
    
  }
  
  names(ParamGpGrowth)=paste("cl",1:(NbClasse-1),sep="")
  
  SimGrowth$ParamGp=ParamGpGrowth
  SimGrowth$Gp=GpGrowth
 
  
  SimGrowth$Func<-function(Eff.cur,Mort,SimGrowth,Surface){
    
    indexspgp=merge(data.frame(Ordre=1:ncol(Eff.cur), Id.sp=colnames(Eff.cur)),SimGrowth$Gp,sort=F,all.x=T,all.y=F)
    SimGrowth$Gp = indexspgp[order(indexspgp$Ordre), -1]
    NbClasse=nrow(Eff.cur)
    NbIdVern=ncol(Eff.cur)
    Effectifs.totaux=apply(Eff.cur,1,sum)/Surface

    datapred=c(1,Effectifs.totaux)
    PbGrowth=matrix(nrow=(NbClasse-1),ncol=NbIdVern)
    Eff.cur=Eff.cur-Mort
    for (cl in 1:(NbClasse-1)){
      CL=1/(1+exp(-datapred%*%SimGrowth$ParamGp[[cl]]))   
      PbGrowth[cl,]=CL[SimGrowth$Gp[,cl+1]]
    }
    
    return(matrix(rbinom(NbIdVern*(NbClasse-1),Eff.cur[1:(NbClasse-1),],PbGrowth),nrow=(NbClasse-1)))
  }
    
  
  
  return(SimGrowth)
  
}

