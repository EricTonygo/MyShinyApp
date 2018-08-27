#include <Rcpp.h>
using namespace Rcpp;

bool contains(NumericVector X, double z) { 
  return std::find(X.begin(), X.end(), z)!=X.end(); }

// [[Rcpp::export]]
StringMatrix doMySimulationLoop(int nbchain, int Finsimu, int NbObsParIter, NumericMatrix Effectifs, int DelayAfterLogging, NumericVector Exploitations,
                                 Function RecruitmentModel, Function MortalityModel, Function GrowthModel, Function LoggingFunction, List SimMort, 
                                 StringVector RepListeNomsp, List SimRecrut, List SimGrowth, double Surface, NumericMatrix intensityMatrix, 
                                 IntegerVector RepClasseDiam,  StringMatrix RecEffectifsTmp, StringMatrix RecEffectifs){
  NumericMatrix EffCur;
  IntegerVector compteur;
  int DAL;
  int sizeVector;
  int sizeMatrixRow;
  IntegerMatrix Mort;
  IntegerVector In;
  IntegerMatrix Monte;
  IntegerMatrix M1;
  IntegerMatrix M2;
  StringMatrix MatrixEffectifTmp;
  StringVector MatrixEffectifTmpRow;
  StringVector RecEffectifsTmpFirstCol;
  Environment_Impl<PreserveStorage> base = Environment::base_env();
  Function rbind = base["rbind"];
  Function cbind = base["cbind"];
  Function asDotVector = base["as.vector"];
  Function asDotNumeric = base["as.numeric"];
  Function asDotCharacter = base["as.character"];
  Function eval_R = base["eval"];
  Function parse_R = base["parse"];
  int sizeM = 0;
  int sizeCompteur = 0;
  
  for(int k = 1; k <= nbchain; k++){
    
    EffCur = Effectifs;
    compteur = seq_len(NbObsParIter);
    DAL = DelayAfterLogging;
    for(int j= 0; j<= Finsimu; j++){
      
      DAL=DAL-1;
      // Mortality
      Mort=MortalityModel(EffCur,SimMort,Surface);

      //Recrutement + growth
      if (DAL<0){
        In=RecruitmentModel(EffCur,SimRecrut,Surface);
        Monte=GrowthModel(EffCur,Mort,SimGrowth,Surface);

      //Update Eff.cur

        M1=rbind(In,Monte);
        M2=rbind(Monte,0);
        sizeM = M2.nrow()*M2.ncol();
        // M2=rbind(Monte,0)+ Mort
        for(int l=0; l<sizeM; l++){
          M2[l]= M2[l] + Mort[l];
        }

        // EffCur=EffCur+M1-M2
        for(int l=0; l<sizeM; l++){
          EffCur[l]= EffCur[l] + M1[l] - M2[l];
        }

      }else{
        sizeM = EffCur.nrow()*EffCur.ncol();
        //EffCur = EffCur-Mort
        for(int l=0; l<sizeM; l++){
          EffCur[l]= EffCur[l] - Mort[l];
        }
      }

      //Exploitations

      // j%in%Exploitations";
      if (contains(Exploitations, j)){
        DAL=DelayAfterLogging;
        EffCur=LoggingFunction(EffCur,intensityMatrix);
        //colnames(EffCur)=colnames(Effectifs)
        colnames(EffCur) = colnames(Effectifs);
      }
      //
      //Stockage des Trajectoires
      //
      sizeCompteur = compteur.size();
      MatrixEffectifTmp = cbind(asDotVector(EffCur),RepListeNomsp,RepClasseDiam,j+1,k);
      sizeMatrixRow = MatrixEffectifTmp.ncol();
      // RecEffectifsTmp[compteur,]=cbind(as.vector(EffCur),RepListeNomsp,RepClasseDiam,j+1,k)
      for(int l=0; l<sizeCompteur; l++){
        for(int m=0; m<sizeMatrixRow; m++){
          RecEffectifsTmp(compteur[l]-1, m) = MatrixEffectifTmp(l,m);
        }
      }
      // compteur=compteur+NbObsParIter;
      for(int l=0; l<sizeCompteur; l++){
        compteur[l]=compteur[l]+NbObsParIter;
      }

    }
     //RecEffectifs=rbind(RecEffectifs,RecEffectifsTmp[RecEffectifsTmp[,1]>0,])
    RecEffectifsTmpFirstCol =  RecEffectifsTmp(_, 0);
    sizeVector = RecEffectifsTmpFirstCol.size();
    for(int n= 0; n < sizeVector; n++){
      if(RecEffectifsTmpFirstCol[n] != "0"){
        RecEffectifs=rbind(RecEffectifs,RecEffectifsTmp(n, _));
      }
    }

  }

  return RecEffectifs;

}