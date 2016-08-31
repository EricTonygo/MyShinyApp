# A logging function applies on a matrix of number of trees by diameter classes (rows)  X species (column)  
  

# Temoin : no logging 

T0<-function(EffectifsT0,intensity.matrix){return(EffectifsT0)}
attr(T0,"intensity")=0
attr(T0,"Logging.Damage")=rep(0,NbClasse)
attr(T0,"DelayAfterLogging")=0




#################
#
# Traitement 1 Paracou : exploitation bois d'oeuvre
#
########################

T1.Paracou <- function(EffectifsT1,intensity.matrix){
  
  EffectifsT1=matrix(rbinom(prod(dim(EffectifsT1)),EffectifsT1,prob=intensity.matrix),nrow=nrow(EffectifsT1))
  return(EffectifsT1)
}
attr(T1.Paracou,"intensity")=0.6
attr(T1.Paracou,"Logging.Damage")=c(0.20542566,0.09022201,0.06831683,0.05656566,0.05656566,0.04504505,0,0)
attr(T1.Paracou,"DelayAfterLogging")=1


#################
#
# Traitement 2 Paracou : exploitation bois d'oeuvre + ?claircies (empoisonement)
#
########################

T2.Paracou <- function(EffectifsT2,intensity.matrix){
  intensity.matrix[ClassesDiam>=50,as.numeric(ListeIdsp)>=400]=0
  EffectifsT2=matrix(rbinom(prod(dim(EffectifsT2)),EffectifsT2,prob=intensity.matrix),nrow=nrow(EffectifsT2))
  return(EffectifsT2)
}
#attr(T2.Paracou,"intensity")=0.6
#attr(T2.Paracou,"Logging.Damage")=c(0.20542566,0.09022201,0.06831683,0.05656566,0.05656566,0.04504505)
#attr(T2.Paracou,"DelayAfterLogging")=1


#################
#
# Traitement 3 Paracou : exploitation bois d'oeuvre + ?claircies (empoisonement) + bois energie
#
########################

T3.Paracou <- function(EffectifsT3,intensity.matrix){
  intensity.matrix[ClassesDiam>=40,as.numeric(ListeIdsp)>=400]=0
  EffectifsT3=matrix(rbinom(prod(dim(EffectifsT3)),EffectifsT3,prob=intensity.matrix),nrow=nrow(EffectifsT3))
  return(EffectifsT3)
}
attr(T3.Paracou,"intensity")=0.6
attr(T3.Paracou,"Logging.Damage")=c(0.20542566,0.09022201,0.06831683,0.05656566,0.05656566,0.04504505)
attr(T3.Paracou,"DelayAfterLogging")=1

####################
####################




#################
#
# Traitement 1 MBaiki : exploitation arbre de 16 essences commerciales avec DBH>80cm
#
########################

T1.MBaiki <- function(EffectifsT1,intensity.matrix){
  
  EffectifsT1=matrix(rbinom(prod(dim(EffectifsT1)),EffectifsT1,prob=intensity.matrix),nrow=nrow(EffectifsT1))
  return(EffectifsT1)
}
attr(T1.MBaiki,"intensity")=1
attr(T1.MBaiki,"Logging.Damage")=c(0.20542566,0.09022201,0.06831683,0.05656566,0.05656566,0.04504505)
attr(T1.MBaiki,"DelayAfterLogging")=1


#################
#
# Traitement 2 Paracou : exploitation arbre de 16 essences commerciales avec DBH>80cm + eclaircies (empoisonement)
#
########################

T2.MBaiki <- function(EffectifsT2,intensity.matrix){
  intensity.matrix[ClassesDiam>=50,as.numeric(ListeIdspLogging)>=400]=0
  EffectifsT2=matrix(rbinom(prod(dim(EffectifsT2)),EffectifsT2,prob=intensity.matrix),nrow=nrow(EffectifsT2))
  return(EffectifsT2)
}
#attr(T2.MBaiki,"intensity")=1
#attr(T2.MBaiki,"Logging.Damage")=c(0.20542566,0.09022201,0.06831683,0.06831683,0.05656566,0.05656566,0.04504505,0)
#attr(T2.MBaiki,"DelayAfterLogging")=1









