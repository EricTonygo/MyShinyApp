InferFCM1Sp<-function(DataFormatted,ParamFiles,criterion){
  
  source(ParamFiles,local=T)
  ParamPlot=list(Lab.period=Lab.period,Nb.period=Nb.period,Surface=Surface)
  ParamPlot$CDSTB=ClasseDiamSTAGB(ClassesDiam,alpha=DataFormatted$alpha)
  
  DataDynInfer=DataFormatted$ClusteringData
  DataDynOut=vector("list",length(DataDynInfer))
  names(DataDynOut)=names(DataDynInfer)


    
  for (esp in 1:length(DataDynInfer)){
    
    Data=DataDynInfer[[esp]]
    DataDynOut[[esp]]=matrix(nrow=NbClasse,ncol=4)
    
  ###############################
  # Mise en forme des données de dynamique
  ########################
  
    
    # Découpage en classe
    Data$Classe=findInterval(Data$Diam,ClassesDiam)
    Data$Classe0=findInterval(Data$Diam0,ClassesDiam)
    
    
  

    DataDyn=subset(Data,Classe0>0)
    
    DataDyn$mort=as.numeric(DataDyn$Diam==0)
    DataDyn$monte=as.numeric(DataDyn$Classe>DataDyn$Classe0)
    DataDyn$reste=as.numeric(DataDyn$Classe==DataDyn$Classe0) 
    
    
    
    DataDyn$verif=DataDyn$mort+DataDyn$monte+DataDyn$reste
    if (sum(DataDyn$verif==0)>0) DataDyn=subset(DataDyn,verif==1)
    
    
   # Calcul proba dyn
    
    for (i in c(T,F)){
      DataDynTmp=subset(DataDyn,Logged==i)
      for (j in 1:NbClasse){
        DataDynTmpTmp=subset(DataDynTmp,Classe0==j)
        
        DataDynOut[[esp]][j,i+1]=sum(DataDynTmpTmp$monte+prior.growth[1])/(sum(DataDynTmpTmp$monte)+sum(DataDynTmpTmp$reste)+prior.growth[2])
        DataDynOut[[esp]][j,i+3]=sum(DataDynTmpTmp$mort+prior.mort[1])/(sum(DataDynTmpTmp$monte)+sum(DataDynTmpTmp$reste)+sum(DataDynTmpTmp$mort)+prior.mort[2])
      }    
      colnames(DataDynOut[[esp]])=c("growth-T","Mort-T","Growth-E","Mort-E")
      DataDynOut[[esp]][NbClasse,c(1,3)]=0
    }
  }
    return(list(ParaDyn=DataDynOut,ParamPlot=ParamPlot,SpeciesTraits=DataFormatted$TraitData,ClassesDiam=ClassesDiam,DataType="sentier"))
  }
  
  
  
  
