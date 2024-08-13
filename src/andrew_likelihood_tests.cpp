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
void do_matrix_multiply_A_specified_interpolated_blocks_increment(double *A,
                                                                  double *B,
                                                                  double *dst,
                                                                  int m_A,
                                                                  int m_B,
                                                                  int n_B,
                                                                  int n_blocks,
                                                                  int *A_block_per_row)
{
    int m_per_block = m_B / n_blocks;
    int block, k;
    double sum;
    
    
    for (int j=0; j<n_B; j++)
    {
        for (int i=0; i<m_A; i++)
        {
            block = A_block_per_row[i];
            
            sum = 0;
            for (int k_in_block=0; k_in_block<m_per_block; k_in_block++)
            {   
                k = k_in_block*n_blocks + block;
                sum += A[k*m_A + i] * B[j*m_B + k];
            }
            
            dst[j*m_A + i] += sum;
        }
    }
}

// [[Rcpp::export]]
void do_matrix_multiply_A_specified_interpolated_blocks_increment(double *A,
                                                                  double *B,
                                                                  double *dst,
                                                                  double *A_blocks_per_row,
                                                                  int m_A,
                                                                  int m_B,
                                                                  int n_B,
                                                                  int n_blocks)
{
    int m_per_block = m_B / n_blocks;
    int block, k;
    double sum;
    
    
    for (int j=0; j<n_B; j++)
    {
        for (int i=0; i<m_A; i++)
        {
            for (int z=0; z<n_blocks; z++) // hardly adds to runtime because the irrelevant iterations will only perform the one IF statement
            {
                if (A_blocks_per_row[z*m_A + i] == 1)
                {
                    block = z;
                    
                    sum = 0;
                    for (int k_in_block=0; k_in_block<m_per_block; k_in_block++)
                    {   
                        k = k_in_block*n_blocks + block;
                        sum += A[k*m_A + i] * B[j*m_B + k];
                    }
                    
                    dst[j*m_A + i] += sum;
                }
            }
        }
    }
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

// [[Rcpp::export]]
NumericVector do_the_thing()

/*** R
M = c(1,0,0,0,0,0,0,0,
     0,1,0,0,0,0,0,0,
     0,0,1,0,0,0,0,0,
     0,0,0,0,0,0,0,1,
     0,0,0,0,1,0,1,0)
B = diag(nrow=8)
*/
