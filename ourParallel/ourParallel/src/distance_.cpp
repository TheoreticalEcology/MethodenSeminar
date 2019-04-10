#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;




// [[Rcpp::export]]
NumericMatrix distance_(NumericMatrix x) {

  int rows = x.rows();
  int columns = x.cols();
  NumericMatrix result(rows, rows);

  for(int r_a = 0; r_a < rows; r_a++){
    for(int r_b = r_a; r_b < rows; r_b++){
      double tmp;
      double tmp2;
      for(int c = 0; c < columns; c++){
        tmp2 = x(r_a, c) - x(r_b, c);
        tmp += tmp2*tmp2;
      }
      result(r_a, r_b) = sqrt(tmp);
    }
  }

  return result;
}

