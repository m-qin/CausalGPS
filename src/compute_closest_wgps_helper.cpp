#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector compute_closest_wgps_helper(NumericVector a,
                                          NumericVector b,
                                          NumericVector cd,
                                          double sc) {

    // a is the subset of data
    // b is the original data

    //TODO: stop crashes due to type of compilers. I commented it out.
    //if (sc < 0 || sc > 1) stop("Scale (sc) should be in [0,1] range.");

    int size_a = a.size();
    int size_b = b.size();
    int min_index = 0;
    double min_val = 0;
    double tmp_val = 0;

    NumericVector tmp(size_a);
    IntegerVector out(size_b);

    for(int i = 0; i < size_b; ++i) {
      for(int j=0; j < size_a; ++j) {
        tmp_val = abs(b[i]-a[j])*sc + cd[j];

        if (j==0){
          min_val = tmp_val;
          min_index = j;
          continue;
        }

        if (tmp_val < min_val){
          min_val = tmp_val;
          min_index = j;
        }
      }

      out[i] = min_index + 1;
    }
    return out;
}
