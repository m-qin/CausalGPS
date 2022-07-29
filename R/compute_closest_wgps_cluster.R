#' @title
#' Find the closest data in subset to the original data in a two-stage approach
#'
#' @description
#' A function to compute the closest data in subset of data to the original data
#' based on four attributes: 4 vectors and scalar (vector of size one).
#'
#' @param a  Vector of the first attribute values for subset of data.
#' @param b  Vector of the first attribute values for all data.
#' @param c  Vector of the second attribute values for subset of data.
#' @param d  Vector of size one for the second attribute value.
#' @param e  Vector of cluster-level variable associated with subset of data.
#' @param f  vector of cluster-level variable associated with all data.
#' @param thresh_cluster An double value that determines boundary of the clusters.
#' @param thresh_min A double value that determines if the min value is acceptable or not.
#' @param sc Scale parameter to give weight for two mentioned measurements.
#' @param nthread Number of available cores.
#'
#' @return
#' The function returns index of subset data that is closest to the original data
#' sample.
#'
#' @keywords internal
#'
compute_closest_wgps_cluster <- function(a, b, c, d, e, f,
                                         thresh_cluster,
                                         thresh_min,
                                         sc, nthread){

  if (!is.numeric(a) ||
      !is.numeric(b) ||
      !is.numeric(c) ||
      !is.numeric(d) ||
      !is.numeric(e) ||
      !is.numeric(f) ||
      !is.numeric(thresh_cluster) ||
      !is.numeric(thresh_min) ||
      !is.numeric(sc)){
    stop('Input values for compute_closest_wgps should be numeric.')
  }

  if (length(a) < 1 ||
      length(b) < 1 ||
      length(c) < 1 ){
    stop('Input values for compute_closest_wgps cannot be empty values.')
  }

  if (length(d) != 1){
    stop('Expecting a scaler number for d.')
  }

  if (length(sc) != 1){
    stop('Expecting a scaler number for sc(scale).')
  }

  if (length(a) != length(c)){
    stop('Expecting equal length for a and c.')
  }

  if (sc < 0 || sc > 1 ){
    stop('Expecting sc in [0,1] range.')
  }

  if ((length(a) != length(e)) || (length(b) != length(f))){
    stop('Cluster-level data should have the same length for each variable.')
  }

  if (thresh_cluster <= 0 || thresh_min <= 0 ){
    stop('Expecting a positive values for thresholds.')
  }


  c_minus_d <- abs(c-d)*(1-sc)

  #wm <- compute_closest_wgps_helper(a, b, c_minus_d, sc, nthread)

  wm <- compute_closest_wgps_cluster_helper(a, b, c_minus_d, e, f,
                                            thresh_cluster, thresh_min,
                                            sc, nthread)

  return(wm)
}
