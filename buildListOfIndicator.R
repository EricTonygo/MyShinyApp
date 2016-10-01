buildListIndicator<-function(){
  if(file.exists("data/FileIndicateur.RData") && file.access(names = "data/FileIndicateur.RData", mode = 4)==0){
    load("data/FileIndicateur.RData")
  }else{
    listeIndicateur= list()
  }
  
  #Biomasse
  tmp = list(list(NomInd="Biomasse", NomFunc= "Biomasse", VarInd="AGB"))
  listeIndicateur = append(listeIndicateur, tmp)
  #Surface terriere
  tmp = list(list(NomInd="Surface terriere", NomFunc= "ST", VarInd="ST"))
  listeIndicateur = append(listeIndicateur, tmp)
  #Nombre d’arbres
  tmp = list(list(NomInd="Nombre d'arbres", NomFunc= "Effectif", VarInd="Eff"))
  listeIndicateur = append(listeIndicateur, tmp)
  
  #Stock exploitable
  tmp = list(list(NomInd="Stock exploitable", NomFunc= "EffectifExp", VarInd="Eff"))
  listeIndicateur = append(listeIndicateur, tmp)
  #Volume d'arbres
  tmp = list(list(NomInd="Volume d'arbres", NomFunc= "Volume", VarInd="Vol"))
  listeIndicateur = append(listeIndicateur, tmp)
  #Volume exploitable
  tmp = list(list(NomInd="Volume exploitable", NomFunc= "VolumeExp", VarInd="Vol"))
  listeIndicateur = append(listeIndicateur, tmp)
  #Taux de reconstitution du stock
  tmp = list(list(NomInd="Taux de reconstitution du stock", NomFunc= "TauxReconstition", VarInd="AGB"))
  listeIndicateur = append(listeIndicateur, tmp)
  return(listeIndicateur)
}
