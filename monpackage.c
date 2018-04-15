#include<R.h>
#include<Rmath.h>

void somme(double* X, double* res, int* lg){
    int i;
    for(i=0; i<*lg; i++) *res += X[i];
}
