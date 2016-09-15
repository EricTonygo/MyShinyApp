InferFCM<- function(DataFormatted,ParamFiles,criterion="BIC", SpeciesTraits= NULL, listeIndicateur= NULL){
source(ParamFiles, local = T)
ParamPlot=list(Lab.period=Lab.period,Nb.period=Nb.period,Surface=Surface)
ParamPlot$CDSTB=ClasseDiamSTAGB(ParamFile = ParamFiles, SpeciesTraits= SpeciesTraits, alpha=DataFormatted$alpha, OtherIndicator = listeIndicateur)
#browser()
# loading of the functions of the dynamics models
expr=paste0('R/',Models$Recruitment,".R")
source(expr)
expr=paste0('R/',Models$Mortality,".R")
source(expr)
expr=paste0('R/',Models$Growth,".R")
source(expr)

SimRecrut=get(Models$Recruitment)(DataFormatted,ParamFiles, SpeciesTraits= SpeciesTraits, OtherIndicator= listeIndicateur)
print("Inference of the recruiment process done")
SimGrowth=get(Models$Growth)(DataFormatted,ParamFiles, SpeciesTraits= SpeciesTraits, OtherIndicator= listeIndicateur)
print("Inference of the growth process done")
SimMort=get(Models$Mortality)(DataFormatted,ParamFiles, SpeciesTraits= SpeciesTraits, OtherIndicator= listeIndicateur)  
print("Inference of the mortality process done")

return(list(SimRecrut=SimRecrut,SimGrowth=SimGrowth,SimMort=SimMort,ParamPlot=ParamPlot,SimulatingData=DataFormatted$SimulatingData,SpeciesTraits=DataFormatted$TraitData,ClassesDiam=ClassesDiam))

}