#include <Rcpp.h>
using namespace Rcpp;


//-----------------------------------------//
//-- GENERAL INCREMENT/OVERWRITE HELPERS --//
//--  (These allow for modify-in-place)  --//
//-----------------------------------------//

// dst_indices and src_indices should be as in R (ie, indexed from 1)
// [[Rcpp::export]]
NumericVector compute_dx(NumericVector state,
                         double time,
                         NumericVector quantity_scratch)
{
    
    
    //-- TRANSITIONS --//
    //-- BIRTHS --//
    //-- DEATHS --//
    //-- TRANSMISSION --//
    //-- RE-SCALING THE POPULATION --//
}