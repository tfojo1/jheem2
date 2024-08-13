#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector generate_lag_matrix_indices(
        NumericVector years,
        NumericVector locations,
        NumericVector strata,
        NumericVector sources,
        int n
) {
    //
    IntegerVector pairs;
    
    // For each observation, find whether there is another observation that is the same but one year back
    for (int i=0; i<n; i++) {
        for (int j=0; j<n; j++) {
            if (i==j) continue;
            if (strata[i]==strata[j] && locations[i]==locations[j] && sources[i]==sources[j]) {
                if (years[i] == years[j] + 1) {
                    pairs.push_back(i); // newer
                    pairs.push_back(j); // older
                }
            }
            
        }
    }
    return pairs;
}

// [[Rcpp::export]]
NumericVector apply_lag_to_vector(
    NumericVector mean,
    IntegerVector L,
    NumericVector output,
    int n
) {
    int m = L.length() / 2;
    
    // Each element of the output is the difference of two elements of the mean vector
    for (int i=0; i<m; i++) {
        int greater_index = L[2*i];
        int lesser_index = L[2*i + 1];
        output[i] = mean[greater_index] - mean[lesser_index];
    }
    return(output);
}

// [[Rcpp::export]]
NumericVector apply_lag_to_matrix(
    NumericVector input,
    IntegerVector L,
    NumericVector output,
    int n
) {
    // Using SIGMA = L %*% sigma %*% t(L)
    // results in every cell of output being the sum/difference of 4 input cells
    
    // output cell row i column j uses combinations of the indices referenced in
    // pairs i and pairs j of the compressed lag matrix L.
    
    // pair i is used to find the input rows, and
    // pair j is used to find the input columns
    
    // if the i'th pair is input indices X,W (meaning X comes after W)
    // and the j'th pair is input indices Z,Y (meaning Z comes after Y)
    
    // then the four contributors to output cell in row i, column j are the inputs at:
    // row X, col Z
    // row X, col Y *-1
    // row W, col Z *-1
    // row W, col Y
    
    int m = L.length() / 2;
    
    for (int i=0; i<m; i++) {
        int row_pair_greater = L[2*i];
        int row_pair_lesser = L[2*i + 1];
        for (int j=i; j<m; j++) {
            int col_pair_greater = L[2*j];
            int col_pair_lesser = L[2*j + 1];
            
            int output_index = i*m + j;
            output[output_index] += input[row_pair_greater*n + col_pair_greater];
            output[output_index] -= input[row_pair_greater*n + col_pair_lesser];
            output[output_index] -= input[row_pair_lesser*n + col_pair_greater];
            output[output_index] += input[row_pair_lesser*n + col_pair_lesser];
            
            // fill in lower triangular side of matrix
            if (i!=j)
                output[j*m + i] = output[i*m + j];
        }
    }
    
    return output;
}

/***
# can put an 'R' after the third * to make this active
 mm = data.frame(year=as.numeric(c('2010', '2011', '2012', '2012', '2011', '2014')),
                location=rep(1, 6),
                stratum=c(1,1,1,2,2,2),
                source=rep(1, 6))
L=generate_lag_matrix_indices(mm$year, mm$location, mm$stratum, mm$source, 6)
sim.mean = c(2, 5, 3, 5, 7, 4)
lag.mean = apply_lag_to_vector(sim.mean, L, rep(0, length(L)/2), length(sim.mean))
sigma = matrix(1:6, nrow=6) %*% matrix(1:6, ncol=6)
algorithmic.sigma.matrix = matrix(apply_lag_to_matrix(as.vector(sigma), L, rep(0, (length(L)/2)^2), length(sim.mean)), nrow=3)

# compare to standard matrix operations
Lmat = matrix(c(-1,0,0,1,-1,0,0,1,0,0,0,1,0,0,-1,0,0,0), nrow=3)
correct.sigma.matrix = Lmat %*% sigma %*% t(Lmat)
*/