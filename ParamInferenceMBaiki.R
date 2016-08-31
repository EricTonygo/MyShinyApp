################################################################
# Parameter for the inference of the models for MBaiki dataset #
################################################################

# WARNING : Modifying the below parameters implies to run the function ClusteringWithFlexmix before FCM


# Diameter classes in cm 
ClassesDiam=c(9,20,30,45,60,80,100,120)
NbClasse=length(ClassesDiam)



# Census plot aera ha
Surface=1

# name of the period used for plot
Lab.period="years"

# periodicity of the census 
Nb.period=1


# Variables modèles recrutement
# NbArbresXX : effectifs de l'espèce dans la classe [XX XX+1]
# SSTXX : surface terrière du peuplement au dessus du diamètre XX
# EffectXX : effectifs du peuplement au-desssus du diamètre XX


#VarRecrut=c("STTC9","STTC30","STTC50","STTC70","STTC90","STTC110") 
Models=list(Recruitment="RecruitmentFlexmix",Growth="GrowthFlexmix",Mortality="MortalityFlexmix")

VarRecrut=c("NbArbres20","NbArbres30","NbArbres45","NbArbres60","NbArbres80","NbArbres100","NbArbres120",
            "STTC9","STTC20","STTC30","STTC45","STTC60","STTC80","STTC100","STTC120") #




# Esp?ces ? ne pas regrouper automatiquement
UserListApartSpecies=list()



