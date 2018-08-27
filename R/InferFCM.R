InferFCM<- function(DataSelected=NULL, ClassesDiam=NULL, Models=NULL, VarRecrut=NULL, UserListApartSpecies=NULL, NomFileSimData=NULL, criterion="BIC"){
 

#source(ParamFiles,local=T)

# Delete trees with DBH lower than the smallest ClassesDiam  

IndexDelete=(DataSelected$ClusteringData$Diam0>0 & DataSelected$ClusteringData$Diam0<ClassesDiam[1]) | 
            (DataSelected$ClusteringData$Diam1>0 & DataSelected$ClusteringData$Diam1<ClassesDiam[1])
DataSelected$ClusteringData=subset(DataSelected$ClusteringData,!IndexDelete)  

IndexDelete=(DataSelected$SimulatingData$Diam>0 & DataSelected$SimulatingData$Diam<ClassesDiam[1])
DataSelected$SimulatingData=subset(DataSelected$SimulatingData,!IndexDelete)  
  
# Models Inference  

  
DataSelected$ClusteringData$Nb.period=DataSelected$ClusteringData$Id.campagne1-DataSelected$ClusteringData$Id.campagne0

Nb.period=unique(DataSelected$ClusteringData$Nb.period)
if (length(Nb.period)>1) Nb.period=1


#ParamPlot=list(Lab.period=Lab.period,Nb.period=Nb.period,Surface=Surface)
#ParamPlot$CDSTB=ClasseDiamSTAGB(ClassesDiam,alpha=0.08)#DataFormatted$alpha)
  
# loading of the functions of the dynamics models
expr=paste0('R/',Models$Recruitment,".R")
source(expr)
expr=paste0('R/',Models$Mortality,".R")
source(expr)
expr=paste0('R/',Models$Growth,".R")
source(expr)

SimRecrut=get(Models$Recruitment)(DataSelected$ClusteringData,ClassesDiam,VarRecrut,UserListApartSpecies)
print("Inference of the recruiment process done")

SimGrowth=get(Models$Growth)(DataSelected$ClusteringData,ClassesDiam,UserListApartSpecies,criterion)
print("Inference of the growth process done")

SimMort=get(Models$Mortality)(DataSelected$ClusteringData,ClassesDiam,UserListApartSpecies,criterion)  
print("Inference of the mortality process done")

  
# Generating start and check dataset

ListeSp=levels(DataSelected$ClusteringData$Id.sp)
DataSelected$SimulatingData=subset(DataSelected$SimulatingData,Id.sp%in%ListeSp)
DataSelected$SimulatingData=merge(DataSelected$SimulatingData,subset(DataSelected$TraitData,select=c(Id.sp,Nom.sp)))
DataSelected$SimulatingData$Nom.sp=DataSelected$SimulatingData$Nom.sp[,drop=T]

DataSelected$SimulatingData$ClassesDiam=as.factor(findInterval(DataSelected$SimulatingData$Diam,ClassesDiam))
DataSelected$SimulatingData$Diam=NULL  
DataSelected$SimulatingData$Id.sp=NULL

SimData=list(SimData=DataSelected$SimulatingData,ClassesDiam=ClassesDiam)
NomFileSimData=paste0(NomFileSimData,".RDS")
saveRDS(SimData,file=NomFileSimData)



return(list(SimRecrut=SimRecrut,SimGrowth=SimGrowth,SimMort=SimMort,SpeciesTraits=DataSelected$TraitData,Nb.period=Nb.period,ClassesDiam=ClassesDiam,DataType="parcelle"))

}