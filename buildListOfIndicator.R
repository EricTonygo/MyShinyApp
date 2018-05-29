buildListIndicator<-function(){
  IndicatorFile = paste0(IndicatorsFolder, "\\FileIndicateur.RData")
  if(!is.null(IndicatorFile) && file.exists(IndicatorFile) && file.access(names = IndicatorFile, mode = 4)==0){
    load(IndicatorFile)
  }else{
    listeIndicateur= list()
  }
  
  #Biomasse
  tmp = list(list(NomInd=uiBiomass, NomFunc= "Biomasse", VarInd="AGB"))
  listeIndicateur = append(listeIndicateur, tmp)
  #Surface terriere
  tmp = list(list(NomInd=uiBasalArea, NomFunc= "ST", VarInd="ST"))
  listeIndicateur = append(listeIndicateur, tmp)
  #Nombre d’arbres
  tmp = list(list(NomInd=uiNumberOfTrees, NomFunc= "Effectif", VarInd="Eff"))
  listeIndicateur = append(listeIndicateur, tmp)
  #Volume d'arbres
  tmp = list(list(NomInd=uiVolumeOfTrees, NomFunc= "Volume", VarInd="Vol"))
  listeIndicateur = append(listeIndicateur, tmp)
  #Volume exploitable
  tmp = list(list(NomInd=uiUsableVolume, NomFunc= "VolumeExp", VarInd="Vol"))
  listeIndicateur = append(listeIndicateur, tmp)
  #Stock exploitable
  tmp = list(list(NomInd=uiUsableStock, NomFunc= "EffectifExp", VarInd="Eff"))
  listeIndicateur = append(listeIndicateur, tmp)
  #Taux de reconstitution du stock
  tmp = list(list(NomInd=uiIventoryReplishmentRate, NomFunc= "TauxReconstition", VarInd="Eff"))
  listeIndicateur = append(listeIndicateur, tmp)
  return(listeIndicateur)
}
