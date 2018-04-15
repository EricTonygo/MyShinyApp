InferFCM<- function(ParamsDyn,criterion="BIC", ParamSim= NULL, listeIndicateur= NULL){
ClassesDiam = ParamsDyn$ClassesDiam
ParamPlot=list(Lab.period= ParamsDyn$ParamPlot$Lab.period, Nb.period=ParamsDyn$ParamPlot$Nb.period, Surface=ParamsDyn$ParamPlot$Surface)
if(tolower(ParamsDyn$DataType)=="parcelle"){
  alphaInd = ParamsDyn$ParamPlot$alpha
}else if(tolower(ParamsDyn$DataType)=="sentier"){
  alphaInd = NULL
}else{
  alphaInd = ParamsDyn$ParamPlot$alpha
}
ParamPlot$CDSTB=ClasseDiamSTAGB(ParamsDyn = ParamsDyn, ParamSim = ParamSim, alpha=alphaInd, OtherIndicator = listeIndicateur)
Models = ParamsDyn$ParamPlot$Models
# loading of the functions of the dynamics models
expr=paste0('R/',Models$Recruitment,".R")
source(expr)
expr=paste0('R/',Models$Mortality,".R")
source(expr)
expr=paste0('R/',Models$Growth,".R")
source(expr)

SimRecrut=get(Models$Recruitment)(ParamsDyn, SpeciesTraits= ParamSim$SpeciesTraits, OtherIndicator= listeIndicateur)
print("Inference of the recruiment process done")
SimGrowth=get(Models$Growth)(ParamsDyn, SpeciesTraits= ParamSim$SpeciesTraits, OtherIndicator= listeIndicateur)
print("Inference of the growth process done")
SimMort=get(Models$Mortality)(ParamsDyn, SpeciesTraits= ParamSim$SpeciesTraits, OtherIndicator= listeIndicateur)  
print("Inference of the mortality process done")

return(list(SimRecrut=SimRecrut,SimGrowth=SimGrowth,SimMort=SimMort,ParamPlot=ParamPlot,SimulatingData=ParamsDyn$SimulatingData,SpeciesTraits=ParamsDyn$SpeciesTraits,ClassesDiam=ClassesDiam))

}