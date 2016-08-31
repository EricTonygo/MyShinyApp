buildListIndicator<-function(){
  tryCatch({
    load("data/FileIndicateur.RData")
    listeIndicateurTmp = listeIndicateur
  },error=function(e){
    listeIndicateurTmp = list()
  })
  
  #Biomasse
  tmp = list(list(NomInd="Biomasse", NomFunc= "Biomasse", VarInd="AGB"))
  listeIndicateurTmp = append(listeIndicateurTmp, tmp)
  #Surface terriere
  tmp = list(list(NomInd="Surface terriere", NomFunc= "ST", VarInd="ST"))
  listeIndicateurTmp = append(listeIndicateurTmp, tmp)
  #Nombre d’arbres
  tmp = list(list(NomInd="Nombre d'arbres", NomFunc= "Effectif", VarInd="Eff"))
  listeIndicateurTmp = append(listeIndicateurTmp, tmp)
  
  #Stock exploitable
  tmp = list(list(NomInd="Stock exploitable", NomFunc= "EffectifExp", VarInd="Eff"))
  listeIndicateurTmp = append(listeIndicateurTmp, tmp)
  #Volume d'arbres
  tmp = list(list(NomInd="Volume d'arbres", NomFunc= "Volume", VarInd="Vol"))
  listeIndicateurTmp = append(listeIndicateurTmp, tmp)
  #Volume exploitable
  tmp = list(list(NomInd="Volume exploitable", NomFunc= "VolumeExp", VarInd="Vol"))
  listeIndicateurTmp = append(listeIndicateurTmp, tmp)
  #Taux de reconstitution du stock
  tmp = list(list(NomInd="Taux de reconstitution du stock", NomFunc= "TauxReconstition", VarInd="AGB"))
  listeIndicateurTmp = append(listeIndicateurTmp, tmp)
  return(listeIndicateurTmp)
}
