#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// [[Rcpp::export]]
NumericVector get_obs_error_correlation_matrix(NumericVector cor_mat,
                                               int n_obs,
                                               NumericVector location,
                                               NumericVector year,
                                               NumericVector stratum,
                                               NumericVector source,
                                               NumericVector details,
                                               double correlation_different_location,
                                               double correlation_different_year,
                                               double correlation_different_strata,
                                               double correlation_different_source,
                                               double correlation_same_source_different_details) {
    if (n_obs < 1) return cor_mat;
    
    for (int i = 0; i < n_obs - 1; i++) {
        for (int j = i + 1; j < n_obs; j++) {
            if (location.length() > 0) {
                if (location[i] != location[j])
                    cor_mat[j * n_obs + i] *= correlation_different_location;
            }
            if (year[i] != year[j])
                cor_mat[j * n_obs + i] *= correlation_different_year;
            if (stratum[i] != stratum[j])
                cor_mat[j * n_obs + i] *= correlation_different_strata;
            if (source[i] != source[j])
                cor_mat[j * n_obs + i] *= correlation_different_source;
            else if (details[i] != details[j])
                    cor_mat[j * n_obs + i] *= correlation_same_source_different_details;
            
            cor_mat[i * n_obs + j] = cor_mat[j * n_obs + i];
        }
    }
    
    return cor_mat;
}


/***

# Add an 'R' after the last * to reactivate this code for testing.
n_obs = 5
cor.mat = rep(1, n_obs**2)
# loc.vec = c(1,1,2,3,2)
loc.vec = numeric(0)
year.vec = c(1,2,1,1,1)
stratum.vec = c(1,2,1,2,1)
source.vec = c(1,1,1,2,2)
details.vec = c(1,1,2,3,3)
corr.dif.loc = 0.5
corr.dif.year = 1
corr.dif.strata = 1
corr.dif.src = 1
corr.same.src.dif.dets = 1
bb = get_obs_error_correlation_matrix(cor.mat,
                                      n_obs,
                                      loc.vec,
                                      year.vec,
                                      stratum.vec,
                                      source.vec,
                                      details.vec,
                                      corr.dif.loc,
                                      corr.dif.year,
                                      corr.dif.strata,
                                      corr.dif.src,
                                      corr.same.src.dif.dets)
*/
