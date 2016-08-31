rm(list=ls(all=TRUE))


# main functions


source("R/InferFCM.R")
source("R/ClasseDiamSTAGB.R")
source("R/StartData.r")
source("R/FCM.r")
source("R/plot.FCM.r")


# Package
library(data.table)
library(flexmix)

####################################
# MBaiki: 2 possibilit?s
##################################


# 1- Regeneration du fichier de donn?es

source('R/FormattingMBaiki.R') # MBaiki specific function
load("data/MBaiki.RData")
load("data/MBaikiTraitData.RData")

MBaikiFormatted=FormattingMBaiki(MBaiki,MBaikiSpeciesTraits)
MBaikiFormatted$TraitData$WSG[is.na(MBaikiFormatted$TraitData$WSG)]=0.571634821
save(MBaikiFormatted,file="data/DataMBaikiFCM.RData")

# 2- Utilisation du dernier fichier de donn?es g?n?r?
load("data/DataMBaikiFCM.RData")

test=InferFCM(MBaikiFormatted,"ParamInferenceMBaiki.R")

ResultTestMB=FCM(out.InferFCM=test,"ParamSimMBaiki.R")
plot(ResultTestMB)