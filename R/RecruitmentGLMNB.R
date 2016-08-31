RecruitmentGLMNB <- function(DataFormatted,ParamFiles,criterion="BIC"){
 
  library(MASS)
  ##################################################################################
  ##################################################################################
  
  ######################
  # Internal functions #
  ######################
  
  FormatDataRecruitmentGLMNB <- function(Data,ClassesDiam,NbClasse,Surface,CDSTB){
    library(data.table)
    
    # Liste des "esp?ces"
    ListeIdsp=levels(Data$Id.sp)
    
    Data=data.table(Data,key="Id.zone,Id.campagne,Id.sp")
    
    # D?coupage en classe
   # Data[,Classe:=findInterval(Diam,ClassesDiam)]
    Data[,Classe0:=findInterval(Diam0,ClassesDiam)]
    
    
    
    # Calcul surface terri?re en m2
    
    
    VectST=c(0,CDSTB$ST)
    Data[,ST0:=VectST[(Classe0+1)]]
    Data[,Recrut:=as.numeric(Diam0==0 & Diam>0)]
    
    
    ######
    # Calcul des tables de comp?tition
    #######
    
    
    
    NomsSTTC=paste("STTC",ClassesDiam,sep='') 
  
    
    
    # construction des commandes pour extraire les tables de competition
    

    Expr1="Data[,list(NbRecruts=sum(Recrut)"
    Expr2=""
    for (i in 1:length(ClassesDiam)){
      Expr2=paste(Expr2,",sum(ST0[Classe0==",i,"])",sep='')
    }
    Expr3="),by='Id.zone,Id.campagne']"
    ExprRecrut=paste(Expr1,Expr2,Expr3,sep='')
    
    outRecruts=eval(parse(text=ExprRecrut)) 
    setnames(outRecruts,3:ncol(outRecruts),c("NbRecruts",NomsSTTC))
    Configs=unique(subset(Data,select=c("Id.zone","Id.campagne")))
    NbConfigs=nrow(Configs)

    if (nrow(outRecruts)<NbConfigs){
      CompletRecrut=unique(subset(Data,select=c("Id.zone","Id.campagne")))
      CompletRecrut=data.table(CompletRecrut,key="Id.zone,Id.campagne")
      outRecruts=merge(outRecruts,CompletRecrut,all.x=T,all.y=T) 
      outRecruts[is.na(outRecruts)]=0 
    }
  
    
    outRecrutsPred=Data[,list(NbRecruts=sum(Recrut)),by='Id.zone,Id.campagne,Id.sp']
    if (nrow(outRecrutsPred)<(NbConfigs*length(ListeIdsp))){
      index=expand.grid(1:NbConfigs,ListeIdsp)
      CompletRecrut=cbind(Configs[index[,1],],index[,2])
      setnames(CompletRecrut,3,"Id.sp")
      outRecrutsPred=merge(outRecrutsPred,CompletRecrut,all.x=T,all.y=T)
      outRecrutsPred[is.na(outRecrutsPred)]=0 
    }
    
    outRecruts=subset(outRecruts,select=c("NbRecruts",NomsSTTC,"Id.zone","Id.campagne"))
    outRecruts=data.frame(outRecruts)
    outRecrutsPred=data.frame(outRecrutsPred)
    
    
    
    return(list(outRecruts=outRecruts,outRecrutsPred=outRecrutsPred))
    
    
  }
  
  ##################################################################################  
  
  

  # loading user's parameters 
  source(ParamFiles,local=T)
  
  ParamPlot=list(Lab.period=Lab.period,Nb.period=Nb.period,Surface=Surface)
  NomsSTTC=paste("STTC",ClassesDiam,sep='')

  NomsDataPred=c(NomsSTTC)
  
  # generating models
  
  if (length(VarRecrut)>0){    
    exprRecrut=paste("NbRecruts~1",paste("+",VarRecrut,collapse=" "))
  }else{exprRecrut="NbRecruts~1"}
  
  modelRecrut=eval(parse(text=exprRecrut)) 
 
  
  
  SimRecrut=list()
  SimRecrut$CDSTB=ClasseDiamSTAGB(ClassesDiam,alpha=DataFormatted$alpha)
  
  
  # Formatting Data      
  DataR=FormatDataRecruitmentGLMNB(DataFormatted$ClusteringData,ClassesDiam,NbClasse,Surface,SimRecrut$CDSTB)
  DataRecrut=DataR$outRecruts
  DataRecrut2=DataR$outRecrutsPred
  
  res=glm.nb(modelRecrut,data=DataRecrut)

  ListeSp=levels(DataRecrut2$Id.sp)
  FreqMatrix=matrix(0,ncol=length(ListeSp),nrow=nrow(DataRecrut))
 
  for (i in 1:nrow(DataRecrut)){
     DataRecrutTmp=subset(DataRecrut2,Id.zone==DataRecrut$Id.zone[i] & Id.campagne==DataRecrut$Id.campagne[i],select=c("Id.sp","NbRecruts"))
     FreqMatrix[i,]=(DataRecrutTmp$NbRecruts[order(DataRecrutTmp$Id.sp)]+1/length(ListeSp))/(DataRecrut$NbRecruts[i]+1)
  }
  

  FreqMatrix=cbind(DataRecrut$NbRecruts,FreqMatrix)
  colnames(FreqMatrix)=c("NbRecruts",ListeSp)

SimRecrut$theta=res$theta
SimRecrut$Param=as.numeric(res$coef)
SimRecrut$FreqMatrix=FreqMatrix


SimRecrut$Func <-function(Eff.cur,SimRecrut){
  
  Effectifs.totaux=apply(Eff.cur,1,sum)
  STTC=Effectifs.totaux*SimRecrut$CDSTB$ST
  datapred=c(1,STTC)
  CL=sum(datapred*SimRecrut$Param)
  NbRecrut=rnbinom(1,SimRecrut$theta,mu=exp(CL))
  weight=dpois(SimRecrut$FreqMatrix[,1],exp(CL))
  sw=sum(weight)
  if (sw==0){
    print("Warning: The state of the simulation is out of the range of the data used to infer the model. The validity of the predicitions is not established.")
    weight[]=1
    sw=sum(weight)
  }
  weight=weight/sw
  prop=weight%*%SimRecrut$FreqMatrix[,-1]
  if (sum(is.na(prop))>0) browser()
  In=as.numeric(rmultinom(1,NbRecrut,prop))
  return(In)
}

return(SimRecrut)
}
