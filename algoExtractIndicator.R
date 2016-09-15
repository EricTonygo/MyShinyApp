PlotMyIndicator <-function(out.FCM,Outputs=NULL,Groups=NULL,Verif=NULL, MyClassesDiam=NULL, MyTimeInterval=NULL, MyIndicator=NULL){
  #########################################
  # Fonction qui renvoie les données de la structure diamétrique des groupes d'espèce #
  #########################################
  
  #######################
  #
  # out.FCM : data.frame produced by FCM.r function
  # Outputs : list of functions to compute on the simulations
  # Groups : list of vectors of Id.sp to cluster for plotting
  # verif : bolean, T : plot also the real values
  # Lab.period : Label of the period
  # Nb.period : Number of period between the census
  # StratingDate : initial date for the simulations
  # Surface: Surface of the plot in ha 
  #
  ######################
  #rm(list = list(ResultTestMB, i, Indicator, listeIndicateur))
  if (is.null(Verif)) Verif=out.FCM$Check
  StartingDate=out.FCM$StartingDate
  Simulations=data.table(out.FCM$Simulations,key="Id.sp")
  DataOutputs=data.table(out.FCM$ParamPlot$CDSTB)
  setkeyv(DataOutputs, c("Id.sp", "ClassesDiam"))
  #browser()
  if(!is.null(MyClassesDiam)){
    DataOutputs = DataOutputs[ClassesDiam %in% MyClassesDiam]
  }
  NbClasse=length(levels(DataOutputs$ClassesDiam))
  DataTraits=data.table(out.FCM$SpeciesTraits,key="Id.sp")
  DataTraits$Id.sp=as.factor(DataTraits$Id.sp)
  Lab.period=out.FCM$ParamPlot$Lab.period
  Nb.period=out.FCM$ParamPlot$Nb.period
  Surface=out.FCM$ParamPlot$Surface
  
  
  
  # rescaling parameters at 1ha
  
  ExpEvalSim = paste0("DataOutputs[,", MyIndicator$VarInd,":=",MyIndicator$VarInd,"/Surface]", collapse = '')
  eval(parse(text = ExpEvalSim))
  
  #browser()
  Simulations=merge(Simulations,DataTraits,by="Id.sp", all.x=F,all.y=F)
  Simulations=merge(Simulations,DataOutputs,by=c("Id.sp", "ClassesDiam"),all.x=F,all.y=F)

  if(MyIndicator$VarInd == "AGB"){
    Simulations[,AGBt:=AGB*Effectifs*WSG]
  }else{
    ExpEvalSim = paste0("Simulations[,",MyIndicator$VarInd,"t:=",MyIndicator$VarInd,"*Effectifs]",collapse = '')
    eval(parse(text = ExpEvalSim))
  }
  
  Simulations[,Temps:=Temps*Nb.period]
  
  
  if (StartingDate==0 | is.null(out.FCM$DataVerif)) Verif=F
  Simulations[,Temps:=Temps+as.numeric(StartingDate)]
  if(!is.null(MyTimeInterval)){
    Simulations = Simulations[Temps %in% MyTimeInterval]
    Temps = MyTimeInterval
  }else{
    Temps=seq(as.numeric(StartingDate),max(Simulations$Temps),Nb.period)
  }
  
  
  if (is.null(Outputs)) OutPuts=c(1,2)
  if (is.null(Groups)) Groups=list(stand=levels(Simulations$Id.sp))
  
  if (Verif){
    DataVerif=data.table(out.FCM$DataVerif,key="Id.sp")
    DataVerif=merge(DataVerif,DataTraits,by="Id.sp",all.x=T,all.y=F)
    DataVerif=merge(DataVerif,DataOutputs,by=c("Id.sp","ClassesDiam"),all.x=T,all.y=F)
    DataVerif=DataVerif[ClassesDiam %in% MyClassesDiam]
    ExpEvalSim = paste0("DataVerif[,",MyIndicator$VarInd,"t:=",MyIndicator$VarInd,"*Effectifs]", collapse = '')
    eval(parse(text = ExpEvalSim))
    DataVerif[,Temps:=as.numeric(as.character(DataVerif$Id.campagne))]
  }
  
  Simulations$ClasseDiam=as.factor(Simulations$ClasseDiam)
  Myggplot = ggplot()
  Mytitle=""
  for (g in 1:length(Groups)){
    
    SimulationTmp=subset(Simulations,Id.sp%in%Groups[[g]])
    #browser()
    
    ExpEvalSim = paste0("Indicateurs=SimulationTmp[,list(", MyIndicator$VarInd,"s=sum(",MyIndicator$VarInd,'t)),by="Id.zone,iter,Temps"]',collapse = '')
    eval(parse(text = ExpEvalSim))
    Indicateurs=Indicateurs[order(Indicateurs$Temps),]
    
    
    Mytitle=paste(MyIndicator$NomInd,"prediciton",names(Groups)[g])
    ExpEvalSim = paste0("Indicateurs",MyIndicator$VarInd,"=Indicateurs[,list(li",MyIndicator$VarInd,"s=quantile(",MyIndicator$VarInd,"s,0.025, na.rm = T),med",MyIndicator$VarInd,"s=median(",MyIndicator$VarInd,"s),ls", MyIndicator$VarInd,"s=quantile(",MyIndicator$VarInd,'s,0.975, na.rm = T)),by="Temps"]',collapse = '')
    eval(parse(text = ExpEvalSim))
    
    if(MyIndicator$NomInd == "Taux de reconstitution"){
      ExpEvalSim = paste0("tmp=subset(Indicateurs,select=c(Id.zone,", MyIndicator$VarInd,"s),iter==0)", collapse = '')
      eval(parse(text = ExpEvalSim))
      ExpEvalSim = paste0('setnames(tmp,"',MyIndicator$VarInd,'s","',MyIndicator$VarInd,"sInit)", collapse = '')
      eval(parse(text = ExpEvalSim))
      Indicateurs=merge(Indicateurs,tmp,by="Id.zone",all.x=T,all.y=F)
      
      ExpEvalSim = paste0("Indicateurs[,",MyIndicator$VarInd,"sR:=",MyIndicator$VarInd,"s/",MyIndicator$VarInd,"sInit]", collapse = '')
      eval(parse(text = ExpEvalSim))
      ExpEvalSim = paste0("Indicateurs",MyIndicator$VarInd,"=Indicateurs[,list(li",MyIndicator$VarInd,"s=quantile(",MyIndicator$VarInd,"sR,0.025,na.rm=T),med",MyIndicator$VarInd,"sR=median(",MyIndicator$VarInd,"sR),ls",MyIndicator$VarInd,"s=quantile(",MyIndicator$VarInd,'sR,0.975,na.rm=T)),by="Temps"]', collapse = '')
      eval(parse(text = ExpEvalSim))
      ExpEvalSim = paste0("Indicateurs",MyIndicator$VarInd,"$li",MyIndicator$VarInd,"=Indicateurs", MyIndicator$VarInd,"$li",MyIndicator$VarInd,"*100", collapse = '')
      eval(parse(text = ExpEvalSim))
      ExpEvalSim = paste0("Indicateurs",MyIndicator$VarInd,"$ls",MyIndicator$VarInd,"=Indicateurs", MyIndicator$VarInd,"$ls",MyIndicator$VarInd,"*100", collapse = '')
      eval(parse(text = ExpEvalSim))
      ExpEvalSim = paste0("Indicateurs",MyIndicator$VarInd,"$med",MyIndicator$VarInd,"=Indicateurs", MyIndicator$VarInd,"$med",MyIndicator$VarInd,"*100", collapse = '')
      eval(parse(text = ExpEvalSim))
    }
    
    ExpEvalSim = paste0("ORD=c(0,max(Indicateurs",MyIndicator$VarInd,"$ls",MyIndicator$VarInd,"s*1.1))",collapse = '')
    eval(parse(text = ExpEvalSim))
    ExpEvalSim = paste0("data= data.frame(temps= Indicateurs",MyIndicator$VarInd,"$Temps, med", MyIndicator$VarInd,"s= Indicateurs",MyIndicator$VarInd,"$med",MyIndicator$VarInd,"s)",collapse = '')
    eval(parse(text = ExpEvalSim))
    ExpEvalSim = paste0("Myggplot <- Myggplot + geom_line(data=data, aes(x=temps, y=med", MyIndicator$VarInd,'s, colour="Med', MyIndicator$VarInd,'"), size=1)', collapse = '')
    eval(parse(text = ExpEvalSim))
    ExpEvalSim = paste0("data= data.frame(temps= Indicateurs",MyIndicator$VarInd,"$Temps, li",MyIndicator$VarInd,"s= Indicateurs",MyIndicator$VarInd,"$li",MyIndicator$VarInd,"s)",collapse = '')
    eval(parse(text = ExpEvalSim))
    ExpEvalSim = paste0("Myggplot <- Myggplot + geom_line(data=data, aes(x=temps, y=li",MyIndicator$VarInd,'s, colour="Min',MyIndicator$VarInd,'"), size=1)',collapse = '')
    eval(parse(text = ExpEvalSim))
    ExpEvalSim = paste0("data= data.frame(temps= Indicateurs",MyIndicator$VarInd,"$Temps, ls",MyIndicator$VarInd,"s= Indicateurs",MyIndicator$VarInd,"$ls",MyIndicator$VarInd,")",collapse = '')
    eval(parse(text = ExpEvalSim))
    ExpEvalSim = paste0("Myggplot <- Myggplot + geom_line(data=data, aes(x=temps, y=ls",MyIndicator$VarInd,'s, colour="Max', MyIndicator$VarInd,'"), size=1)',collapse = '')
    eval(parse(text = ExpEvalSim))
    if (Verif){
      DataVerifTmp=subset(DataVerif,Id.sp%in%Groups[[g]])
      DataVerifTmp=data.table(DataVerifTmp,key="Id.zone,Temps")
      ExpEvalSim = paste0("IndicateursVerif=DataVerifTmp[,list(", MyIndicator$VarInd,"s=sum(",MyIndicator$VarInd,'t)),by="Id.zone,Temps"]',collapse = '')
      eval(parse(text = ExpEvalSim))
      ExpEvalSim = paste0("IndicateursVerif=IndicateursVerif[,list(",MyIndicator$VarInd,"s=median(",MyIndicator$VarInd ,'s)),by="Temps"]')
      eval(parse(text = ExpEvalSim))
      ExpEvalSim = paste0("data= data.frame(temps= IndicateursVerif$Temps, verif",MyIndicator$VarInd,"= IndicateursVerif$",MyIndicator$VarInd,"s)", collapse = '')
      eval(parse(text =  ExpEvalSim))
      ExpEvalSim = paste0("Myggplot=Myggplot + geom_point(data=data, aes(x=temps, y=verif",MyIndicator$VarInd, ', colour="Verif',MyIndicator$VarInd,'"), size=1)', collapse = '')
      eval(parse(text =  ExpEvalSim))    
    }
  }
  #browser()
  Myggplot <- Myggplot + xlab(Lab.period)
  Myggplot <- Myggplot + ylab(paste0(MyIndicator$NomInd," ", MyIndicator$Unite,"/ha",collapse = ''))
  Myggplot <- Myggplot + ylim(ORD)
  Myggplot <- Myggplot + theme(legend.position=c(.65, 0.95), legend.justification=c(0,1), legend.title=element_blank())
  Myggplot <- Myggplot + ggtitle(Mytitle)+ theme_bw() 
  #abline(h=0)
  return(Myggplot)
}