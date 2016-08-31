ClasseDiamSTAGB<-function(ParamFile=NULL,Data=NULL,alpha=NULL, OtherIndicator=NULL){
  source(ParamFile,local=T)
# Calcul des indicateurs utiles pour plot.FCM

NbClasse=length(ClassesDiam)  
ClasseDiam2=c(ClassesDiam,Inf)


if (is.null(alpha)){
Effectifs=tapply(DataDepart$ClassesDiam,DataDepart$Id.sp,function(x) as.numeric(table(x)),simplify=F)
DataStart[[p]]=sapply(Effectifs,function(x) if(is.null(x)) rep(0,NbClasse) else x)
EffectifsTotaux=apply(DataStart[[p]],1,sum)+EffectifsTotaux

# Calcul du param?tre  alpha de la distribution exponentielle des diam?tres

PropEffectifsTotaux=EffectifsTotaux/sum(EffectifsTotaux)


SearchAlpha=function(Alpha,NbClasse,ClasseDiam2,PropEffectifsTotaux){
  SS=0
  for (i in 1:NbClasse){
    SS=SS+(integrate(dexp,ClasseDiam2[i],ClasseDiam2[i+1],Alpha)$value-PropEffectifsTotaux[i])^2
  }
  return(SS)
}


res=optimize(SearchAlpha,interval=c(0.000001,0.5),NbClasse,ClasseDiam2,PropEffectifsTotaux)
alpha=res$minimum
}



ST=rep(0,NbClasse)
Biomass=rep(0,NbClasse)
Vol=rep(0,NbClasse)
Circ = rep(0,NbClasse)

SurfT<-function(d) 0.25*pi*d^2
Biom <- function(d) {
  lD=log(d)
  return(exp(-1.499+2.148*lD+0.207*lD*lD-0.0281*lD*lD*lD))
}

Volume<-function(d){
  vol=10^(-2.96+1.93*log10(d))
  #vol[d<55]=0
  return(vol)
}  

#circonference<-function(d) 0.25*pi*d



for (i in 1:NbClasse){
  tmp=integrate(function(x) alpha*SurfT(x)*exp(-alpha*x)/(exp(-alpha*ClasseDiam2[i])-exp(-alpha*ClasseDiam2[i+1])),ClasseDiam2[i],ClasseDiam2[i+1])
  ST[i]=tmp$value
  tmp=integrate(function(x) alpha*Biom(x)*exp(-alpha*x)/(exp(-alpha*ClasseDiam2[i])-exp(-alpha*ClasseDiam2[i+1])),ClasseDiam2[i],ClasseDiam2[i+1])
  Biomass[i]=tmp$value
  tmp=integrate(function(x) alpha*Volume(x)*exp(-alpha*x)/(exp(-alpha*ClasseDiam2[i])-exp(-alpha*ClasseDiam2[i+1])),ClasseDiam2[i],ClasseDiam2[i+1])
  Vol[i]=tmp$value
}
# ram?ne les effectifs
Eff=rep(1,NbClasse)
# ram?ne la surface terri?re en m2 
ST=ST/10000
# ram?ne biomasse en Tonne 
Biomass=Biomass/1000  


ExpReturn = "DataOutputs=data.frame(Eff=Eff,ST=ST,AGB=Biomass,Vol=Vol, "
if(is.list(OtherIndicator)){
  for(i in 1:length(OtherIndicator)){
    indicator = OtherIndicator[[i]]
    Initial = paste0(indicator$VarInd, "= rep(0,NbClasse)",collapse='')
    eval(parse(text = Initial))
    Function = paste0(indicator$NomFunc, "<-", indicator$Func,collapse='')
    eval(parse(text = Function))
    ExpTmp=paste0("for (i in 1:NbClasse){tmp = integrate(function(x) alpha*",indicator$NomFunc,"(x)*exp(-alpha*x)/(exp(-alpha*ClasseDiam2[i])-exp(-alpha*ClasseDiam2[i+1])),ClasseDiam2[i],ClasseDiam2[i+1]); ", indicator$VarInd,"[i]=tmp$value}" ,collapse='')
    eval(parse(text = ExpTmp))
    ExpReturn = paste0(ExpReturn, indicator$VarInd, "=", indicator$VarInd, ", ", collapse = '')
  }
}
ExpReturn=paste0(ExpReturn, "ClassesDiam=as.factor(1:NbClasse))", collapse = '')
eval(parse(text = ExpReturn))
return(DataOutputs)
}


