#include <Rcpp.h>
#include <vector>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector generate_transformation_matrix_indices(
    NumericVector transformation_matrix,
    int m,
    int n) {
    
    // Create sparse representation of the transformation matrix 
    std::vector<std::vector<int>> sparse_indices;
    for (int i=0; i<n; i++) {
        std::vector<int> sparse_column;
        for (int j=0; j<m; j++) {
            if (transformation_matrix[i*m + j]) {
                sparse_column.push_back(j);
            }
        }
        sparse_indices.push_back(sparse_column);
    }
    
    // Convert to a vector consisting of triples (i,j,k)
    // Note that for space reasons, (2,3,k) will be present but not (3,2,k)
    // TODO change triples to IntegerVector
    std::vector<int> triples;
    for (int k=0; k<n; k++) {
        for (unsigned int a=0; a<sparse_indices[k].size(); a++) {
            for (unsigned int b=a; b<sparse_indices[k].size(); b++) {
                triples.push_back(sparse_indices[k][a]);
                triples.push_back(sparse_indices[k][b]);
                triples.push_back(k);
            }
        }
    }
    
    // Return a NumericVector holding the triples
    return wrap(triples);
}

/***
// [[Rcpp::export]]
void get_basic_likelihood_mean(
    const NumericVector sim_numerator,
    const List transformation_matrix_row_oriented_indices,
    NumericVector mean
) {
    for (int i=0; i<transformation_matrix_row_oriented_indices.length(); i++) {
        for (int j=0; j<transformation_matrix_row_oriented_indices[i].length(); j++) {
            mean[i] += sim_numerator[j];
        }
    }
}
 ***/

// [[Rcpp::export]]
/*
 * This function computes the transformed covariance matrix using using only the
 * nonzero operations in A = MVM^T. First, the sparse, binary transformation
 * matrix was stored in a condensed form recording which rows have nonzero
 * elements in each column. Each position i,j in the transformed covariance
 * matrix is the sum of the products along one row of MV and one column of M^T,
 * which is itself a row of M. In the dense case, this will be the sum of *m*
 * products. In the actual sparse case, only a few of these products will be
 * nonzero and contribute to the sum, and this occurs exactly when there is a
 * nonzero element in the same column for a pair of rows in M (one of which
 * becomes a column of M^T). If a column *k* of M has nonzero elements in *s*
 * different rows, then that column will contribute a product to the positions
 * i,j in A where i,j is every pairing of the rows *s*. Other columns may
 * contribute to that position i,j, but if we loop across columns of M and
 * merely add the contribution from each column as it comes, we will eventually
 * create the full sum of nonzero products comprising A[i,j]. The role of
 * covariance matrix itself is simple since it is diagonal; each column *k* of M
 * will be multiplied by the kth diagonal element of the covariance matrix.
 * Therefore, each contribution to A[i,j] becomes 1 * V[k,k] = V[k,k]/ Creating
 * the sparse representation occurs during instantiate time. I believe the
 * transformation algorithm here has a big-O of O(k*s^2) where *k* is the number
 * of columns with nonzero elements and *s* is the number of rows with nonzero
 * elements in each column. While *k* is expected to be the same as the width of
 * M, *n*, *s* is expected to be quite small. In the limit as *k* goes to *n*
 * and *s* goes to *m*, this algorithm approaches regular matrix multiplication
 * runtime. The reason *s* is squared is because this is number of pairs of s
 * elements. Making sigma will take *m*-squared time, possibly making it the
 * more expensive operation.
 */
NumericVector get_basic_likelihood_sigma(
    const NumericVector sim_numerator,
    const NumericVector sim_denominator,
    const NumericVector transformation_matrix_indices,
    const NumericVector measurement_error_cov_matrix,
    int m,
    NumericVector sigma
) {
    // generate model imperfection covariance diagonal matrix
    std::vector<double> V;
    V.reserve(sim_numerator.length());
    for (int i=0; i<sim_numerator.length(); i++) {
        V.push_back(sim_numerator[i] * (1 - sim_numerator[i] / sim_denominator[i]));
    }
    
    // copy measurement_error_cov_matrix into sigma
    for (int i=0; i<measurement_error_cov_matrix.length(); i++) {
        sigma[i] = measurement_error_cov_matrix[i];
    }
    
    // add to sigma MVM^T
    for (int h=0; h<transformation_matrix_indices.length(); h+=3) {
        int i = transformation_matrix_indices[h];
        int j = transformation_matrix_indices[h + 1];
        int k = transformation_matrix_indices[h + 2];
        
        sigma[j*m + i] += V[k];
        
        if (i != j) {
            sigma[i*m + j] += V[k];
        }
    }
    
    return sigma;
}

/*** 
mat = rbind(c(1,1,0,0),
            c(0,0,1,1),
            c(0,1,0,1))
smat = generate_transformation_matrix_indices(mat, 3,4)
sim.numerator.d = rep(10,4)
sim.denominator.d = rep(20,4)
measure_error_matrix = matrix(0, nrow=3, ncol=3)
sigma = matrix(0, nrow=3, ncol=3)

sigma = get_basic_likelihood_sigma(
    sim.numerator.d,
    sim.denominator.d,
    smat,
    measure_error_matrix,
    3,
    sigma
)

model.imp.cov.matrix = diag(
    sim.numerator.d * (1 - sim.numerator.d / sim.denominator.d),
    nrow=4, ncol=4)
mat %*% model.imp.cov.matrix %*% t(mat)
*/