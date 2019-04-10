#include <Rcpp.h>
#include <math.h>
#include <iostream>

// [[Rcpp::plugins(openmp)]]

using namespace Rcpp;


// [[Rcpp::export]]
NumericVector dosomething_(NumericVector x, float time) {

  int size = x.size();
  NumericVector out(size);

  for(int i = 0; i < (size); i++){
    out[i]= atan(x[i]*x[i] / x[i] ) / (x[i] + x[i]);
  }

  return out;
}

