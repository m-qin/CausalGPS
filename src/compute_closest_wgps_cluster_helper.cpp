#include <Rcpp.h>
using namespace Rcpp;

#ifdef _OPENMP
#include <omp.h>
#else
#define omp_get_num_threads()  1
#define omp_get_thread_num()   0
#define omp_get_max_threads()  1
#define omp_get_thread_limit() 1
#define omp_get_num_procs()    1
#define omp_set_nested(a)   // empty statement to remove the call
#define omp_get_wtime()        0
#endif

// [[Rcpp::plugins(openmp)]]

// [[Rcpp::export]]
IntegerVector compute_closest_wgps_cluster_helper(NumericVector a,
                                                  NumericVector b,
                                                  NumericVector cd,
                                                  NumericVector e,
                                                  NumericVector f,
                                                  double thresh_cluster,
                                                  double thresh_min,
                                                  double sc,
                                                  int nthread) {

  // a is the subset of data
  // b is the original data
  // e is the cluster-level data for a.
  // f is the cluster-level data for b.
  // thresh_cluster is defining the minimum absolute difference boundary for
  // cluster assignment.
  // thresh_min is defining the minimum acceptable difference.

  int size_a = a.size();
  int size_b = b.size();

  IntegerVector out(size_b);

#if defined(_OPENMP)
  //int nthread = omp_get_max_threads();
  omp_set_num_threads(nthread);
#pragma omp parallel for
#endif
  for(int i = 0; i < size_b; ++i) {

    // ic: in cluster
    // oc: out of cluster

    double tmp_val = 0;

    int min_index_ic = 0;
    int min_index_oc = 0;

    double min_val_ic = 0;
    double min_val_oc = 0;

    double subtract_val = 0;

    double subtract_val_ef = 0;

    // flag to initial values.
    int initiate_ic = 0;
    int initiate_oc = 0;

    // Initiate an integer vector to store cluster status.
    IntegerVector cluster_status(size_a);

    // Pre-allocate
    for (int iter1=0; iter1 < size_a; ++iter1){
      cluster_status[iter1] = 0;
    }

    // Based on vector e and f[i], decide if that data index is in the
    // cluster or not.

    for (int iter2=0; iter2 < size_a; ++iter2){

      subtract_val_ef = (f[i] - e[iter2]);

      if (subtract_val_ef <= thresh_cluster){
        cluster_status[iter2] = 1;
      }
    }

    for(int j=0; j < size_a; ++j) {

      subtract_val = (b[i]-a[j])*sc;

      if (subtract_val < 0){subtract_val *= -1;}

      tmp_val =  subtract_val + cd[j];

      if (initiate_oc == 0 && cluster_status[j] == 0){
        min_val_oc = tmp_val;
        min_index_oc = j;
        initiate_oc = 1;
        continue;
      }

      if (initiate_ic == 0 && cluster_status[j] == 1){
        min_val_ic = tmp_val;
        min_index_ic = j;
        initiate_ic = 1;
        continue;
      }

      if (tmp_val < min_val_oc && cluster_status[j] == 0){
        min_val_oc = tmp_val;
        min_index_oc = j;
      }

      if (tmp_val < min_val_ic && cluster_status[j] == 1){
        min_val_ic = tmp_val;
        min_index_ic = j;
      }

    }

    if (min_val_ic < thresh_min || min_val_ic < min_val_oc){
      out[i] = min_index_ic + 1;
    } else {
      out[i] = min_index_oc + 1;
    }
  }
  return out;
}

