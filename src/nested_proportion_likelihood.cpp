
#include <Rcpp.h>
using namespace Rcpp;


//=====================//
//== GENERAL HELPERS ==//
//=====================//

void fill_arr(double *arr,
              double value,
              int len)
{
    for (int i=0; i<len; i++)
        arr[i] = value;
}

void add_arrs(double *arr1,
              double *arr2,
              double *dst,
              int len)
{
    for (int i=0; i<len; i++)
        dst[i] = arr1[i] + arr2[i];
}


void add_three_arrs(double *arr1,
                    double *arr2,
                    double *arr3,
                    double *dst,
                    int len)
{
    for (int i=0; i<len; i++)
        dst[i] = arr1[i] + arr2[i] + arr3[i];
}

void multiply_arrs(double *arr1,
                   double *arr2,
                   double *dst,
                   int len)
{
    for (int i=0; i<len; i++)
        dst[i] = arr1[i] * arr2[i];
}

void multiply_arr1_by_1_minus_arr2(double *arr1,
                                   double *arr2,
                                   double *dst,
                                   int len)
{
    for (int i=0; i<len; i++)
        dst[i] = arr1[i] * (1-arr2[i]);
}

// arr1 - arr2
void subtract_arrs(double *arr1,
                   double *arr2,
                   double *dst,
                   int len)
{
    for (int i=0; i<len; i++)
        dst[i] = arr1[i] - arr2[i];
}

// can be done in place on arr
void apply_arr_mask(double *arr,
                    double *dst,
                    int *mask,
                    int len)
{
    int index_to = 0;
    for (int index_from=0; index_from<len; index_from++)
    {
        if (mask[index_from])
        {
            dst[index_to] = arr[index_from];
            index_to++;
        }
    }
}

//=======================//
//== MATRIX OPERATIONS ==//
//=======================//

//== STRAIGHT-UP MATRIX MULTIPLICATION ==//

void do_matrix_multiply(double *A,
                        double *B,
                        double *dst,
                        int m_A,
                        int shared_dimension,
                        int n_B)
{
    double sum;
    
    for (int j=0; j<n_B; j++)
    {
        for (int i=0; i<m_A; i++)
        {
            sum = 0;
            for (int k=0; k<shared_dimension; k++)
            {
                sum += A[k*m_A + i] * B[j*shared_dimension + k];
            }
            dst[j*m_A + i] = sum;
        }
    }
}


//same as do matrix multiply, but with B in row-major order (ie, transposed)
void do_matrix_multiply_B_transposed(double *A,
                                     double *B,
                                     double *dst,
                                     int m_A,
                                     int shared_dimension,
                                     int m_B)
{
    double sum;
    
    
    for (int j=0; j<m_B; j++)
    {
        for (int i=0; i<m_A; i++)
        {
            sum = 0;
            for (int k=0; k<shared_dimension; k++)
            {
                sum += A[k*m_A + i] * B[k*m_B + j];
            }
            dst[j*m_A + i] = sum;
        }
    }
}

//== MATRIX MULTIPLICATION WHERE WE KNOW THE RESULT WILL BE SYMMETRIC ==//

// Takes advantage of the fact that we know the resulting matrix must be symmetrical to save on calculations
// By this framing, we know that m_B = n_A and n_B = m_A
void do_matrix_multiply_symmetric_result(double *A,
                                         double *B,
                                         double *dst,
                                         int m_A,
                                         int n_A)
{
    double sum;
    for (int i=0; i<m_A; i++)
    {
        // do the diagonal element
        sum = 0;
        for (int k=0; k<n_A; k++)
        {
            sum += A[k*m_A + i] * B[i*n_A + k];
        }
        dst[i*m_A + i] = sum;
        
        // do the off-diagonal elements
        for (int j=i+1; j<m_A; j++)
        {
            sum = 0;
            for (int k=0; k<n_A; k++)
            {
                sum += A[k*m_A + i] * B[j*n_A + k];
            }
            
            dst[j*m_A + i] = dst[i*m_A + j] = sum;
        }
    }
}

//same as do matrix multiply symmetric result, but with B in row-major order (ie, transposed)
void do_matrix_multiply_symmetric_result_B_transposed(double *A,
                                                      double *B,
                                                      double *dst,
                                                      int m_A,
                                                      int n_A)
{
    double sum;
    for (int i=0; i<m_A; i++)
    {
        // do the diagonal element
        sum = 0;
        for (int k=0; k<n_A; k++)
        {
            sum += A[k*m_A + i] * B[k*m_A + i];
        }
        dst[i*m_A + i] = sum;
        
        // do the off-diagonal elements
        for (int j=i+1; j<m_A; j++)
        {
            sum = 0;
            for (int k=0; k<n_A; k++)
            {
                sum += A[k*m_A + i] * B[k*m_A + j];
            }
            
            dst[j*m_A + i] = dst[i*m_A + j] = sum;
        }
    }
}

// calculated M %*% D %*% t(M)
// where D is a vector of length n_A representing the diagonal values of a diagonal matrix 
void do_matrix_multiply_M_D_M_transpose(double *M,
                                        double *D,
                                        double *dst,
                                        int m,
                                        int n)
{
    double sum;
    for (int j=0; j<m; j++)
    {   
        sum = 0;
        for (int k=0; k<n; k++)
        {
            sum += D[k] * M[k*m + j] * M[k*m + j];
        }
        dst[j*m + j] = sum;
        
        for (int i=j+1; i<m; i++)
        {
            sum = 0;
            for (int k=0; k<n; k++)
            {
                sum += D[k] * M[k*m + i] * M[k*m + j];
            }
            dst[j*m + i] = dst[i*m + j] = sum;
        }
    }
}

//can be done in place on M
void do_matrix_multiply_D_M_D(double *D,
                              double *M,
                              double *dst,
                              int n)
{
    for (int j=0; j<n; j++)
    {
        for (int i=0; i<n; i++)
        {
            dst[j*n + i] = M[j*n + i] * D[i] * D[j];
        }
    }
}

//== MATRIX MULTIPLICATION WITH A BLOCK DIAGONAL ==//

void do_matrix_multiply_A_block_diagonal(double *A,
                                         double *B,
                                         double *dst,
                                         int n_blocks,
                                         int n_per_block,
                                         int n_B)
{
    int i,k;
    int m_A = n_per_block * n_blocks; //since A is square, m_A = n_A = m_B
    double sum;
    
    for (int block=0; block<n_blocks; block++)
    {
        for (int i_in_block=0; i_in_block<n_per_block; i_in_block++)
        {
            i = block * n_per_block + i_in_block;
            
            for (int j=0; j<n_B; j++)
            {
                sum = 0;
                for (int k_in_block=0; k_in_block<n_per_block; k_in_block++)
                {
                    k = block * n_per_block + k_in_block;
                    sum += A[k*m_A + i] * B[j*m_A + k];
                }
                dst[j*m_A + i] = sum;
            }
        }
    }
}

void do_matrix_multiply_A_block_diagonal_B_transposed(double *A,
                                                      double *B,
                                                      double *dst,
                                                      int n_blocks,
                                                      int n_per_block,
                                                      int m_B)
{
    int i,k;
    int m_A = n_per_block * n_blocks;
    double sum;
    
    for (int block=0; block<n_blocks; block++)
    {
        for (int i_in_block=0; i_in_block<n_per_block; i_in_block++)
        {
            i = block * n_per_block + i_in_block;
            
            for (int j=0; j<m_B; j++)
            {
                sum = 0;
                for (int k_in_block=0; k_in_block<n_per_block; k_in_block++)
                {
                    k = block * n_per_block + k_in_block;
                    sum += A[k*m_A + i] * B[k*m_B + j];
                }
                dst[j*m_A + i] = sum;
            }
        }
    }
}

//== MATRIX MULTIPLICATION with an INTERPOLATED BLOCK DIAGONAL MATRIX ==//

void do_matrix_multiply_A_interpolated_block(double *A,
                                             double *B,
                                             double *dst,
                                             int n_blocks,
                                             int m_per_block,
                                             int n_per_block,
                                             int n_B)
{
    int m_A = n_blocks * m_per_block;
    int m_B = n_blocks * n_per_block;
    int i, k;
    double sum;
    
    for (int block=0; block<n_blocks; block++)
    {
        for (int i_in_block=0; i_in_block<m_per_block; i_in_block++)
        {
            i = i_in_block*n_blocks + block;
            
            for (int j=0; j<n_B; j++)
            {
                sum = 0;
                for (int k_in_block=0; k_in_block<n_per_block; k_in_block++)
                {
                    k = k_in_block*n_blocks + block;
                    sum += A[k*m_A + i] * B[j*m_B + k];
                }
                dst[j*m_A + i] = sum;
            }
        }
    }
}

void do_matrix_multiply_B_interpolated_block_transposed_symmetric_result(double *A,
                                                                         double *B,
                                                                         double *dst,
                                                                         int m_A,
                                                                         int n_A,
                                                                         int n_blocks)
{
    int m_B = m_A; //since this will be a symmetric result, and B is transposed
    int m_per_block = m_B / n_blocks;
    int n_per_block = n_A / n_blocks;
    
    int j,k;
    double sum;
    for (int block=0; block<n_blocks; block++)
    {
        for (int j_in_block=0; j_in_block<m_per_block; j_in_block++)
        {
            j = j_in_block*n_blocks + block;
            
            sum = 0;
            for (int k_in_block=0; k_in_block<n_per_block; k_in_block++)
            {
                k = k_in_block*n_blocks + block;
                
                sum += A[k*m_A + j] * B[k*m_B + j];
            }
            dst[j*m_A + j] = sum;
            
            for (int i=j+1; i<m_A; i++)
            {   
                sum = 0;
                for (int k_in_block=0; k_in_block<n_per_block; k_in_block++)
                {
                    k = k_in_block*n_blocks + block;
                    
                    sum += A[k*m_A + i] * B[k*m_B + j];
                }
                dst[j*m_A + i] = dst[i*m_A + j] = sum;
            }
        }
    }
}

void do_matrix_multiply_A_interpolated_block_B_double_masked(double *A,
                                                             double *B,
                                                             double *dst,
                                                             int m_A,
                                                             int n_blocks,
                                                             int *row_mask,
                                                             int row_mask_len,
                                                             int *col_mask,
                                                             int col_mask_len)
{
    // figure out the dimensions of the B matrix from the masks
    int m_B = 0;
    for (int k_mapped=0; k_mapped<row_mask_len; k_mapped++)
        m_B += row_mask[k_mapped];
    
    int n_B = 0;
    for (int k_mapped=0; k_mapped<col_mask_len; k_mapped++)
        n_B += col_mask[k_mapped];
    
    int m_per_block = m_A / n_blocks;
    int n_per_block = m_B / n_blocks; //m_B = n_A
    
    
    // set up index mappings
    
    int mapped_row_indices[m_B];
    int mapped_col_indices[n_B];
    
    int k=0;
    for (int k_mapped=0; k_mapped<row_mask_len; k_mapped++)
    {
        if (row_mask[k_mapped])
        {
            mapped_row_indices[k] = k_mapped;
            k++;
        }
    }
    
    k=0;
    for (int k_mapped=0; k_mapped<col_mask_len; k_mapped++)
    {
        if (col_mask[k_mapped])
        {
            mapped_col_indices[k] = k_mapped;
            k++;
        }
    }
    
    
    // iterate across the matrices and set up sums
    int i, j_mapped, k_mapped;
    double sum;
    for (int j=0; j<n_B; j++)
    {
        j_mapped = mapped_col_indices[j];
        for (int block=0; block<n_blocks; block++)
        {
            for (int i_in_block=0; i_in_block<m_per_block; i_in_block++)
            {
                i = i_in_block*n_blocks + block;
                
                sum = 0;
                for (int k_in_block=0; k_in_block<n_per_block; k_in_block++)
                {
                    k = k_in_block*n_blocks + block;
                    k_mapped = mapped_row_indices[k];
                    
                    sum += A[k*m_A + i] * B[j_mapped*row_mask_len + k_mapped];
                }
                dst[j*m_A + i] = sum;
            }
        }
    }
}

void do_matrix_multiply_A_interpolated_block_B_row_masked(double *A,
                                                          double *B,
                                                          double *dst,
                                                          int n_blocks,
                                                          int m_per_block,
                                                          int n_B,
                                                          int *mask,
                                                          int mask_len)
{
    int m_A = n_blocks * m_per_block;
    
    int m_B = 0;
    for (int k_mapped=0; k_mapped<mask_len; k_mapped++)
        m_B += mask[k_mapped];
    int n_per_block = m_B / n_blocks;
    
    int mapped_indices[m_B];
    int k=0;
    for (int k_mapped=0; k_mapped<mask_len; k_mapped++)
    {
        if (mask[k_mapped])
        {
            mapped_indices[k] = k_mapped;
            k++;
        }
    }
    
    int i, k_mapped;
    double sum;
    
    for (int j=0; j<n_B; j++)
    {
        for (int block=0; block<n_blocks; block++)
        {
            for (int i_in_block=0; i_in_block<m_per_block; i_in_block++)
            {
                i = i_in_block*n_blocks + block;
                
                sum = 0;
                for (int k_in_block=0; k_in_block<n_per_block; k_in_block++)
                {
                    k = k_in_block*n_blocks + block;
                    k_mapped = mapped_indices[k];
                    
                    sum += A[k*m_A + i] * B[j*mask_len + k_mapped];
                }
                dst[j*m_A + i] = sum;
            }
        }
    }
}

void do_matrix_multiply_A_interpolated_block_B_row_masked_symmetric_result(double *A,
                                                                           double *B,
                                                                           double *dst,
                                                                           int n_blocks,
                                                                           int m_per_block,
                                                                           int n_B,
                                                                           int *mask,
                                                                           int mask_len)
{
    int m_A = n_blocks * m_per_block;
    
    int m_B = 0;
    for (int k_mapped=0; k_mapped<mask_len; k_mapped++)
        m_B += mask[k_mapped];
    int n_per_block = m_B / n_blocks;
    
    int mapped_indices[m_B];
    int k=0;
    for (int k_mapped=0; k_mapped<mask_len; k_mapped++)
    {
        if (mask[k_mapped])
        {
            mapped_indices[k] = k_mapped;
            k++;
        }
    }
    
    int i, k_mapped;
    double sum;
    
    for (int block=0; block<n_blocks; block++)
    {
        for (int i_in_block=0; i_in_block<m_per_block; i_in_block++)
        {
            i = i_in_block*n_blocks + block;
            
            for (int j=i; j<n_B; j++)
            {
                sum = 0;
                for (int k_in_block=0; k_in_block<n_per_block; k_in_block++)
                {
                    k = k_in_block*n_blocks + block;
                    k_mapped = mapped_indices[k];
                    
                    sum += A[k*m_A + i] * B[j*mask_len + k_mapped];
                }
                
                dst[j*m_A + i] = sum;
                if (i!=j)
                    dst[i*m_A + j] = sum;
            }
        }
    }
}

void do_matrix_multiply_A_col_masked_B_interpolated_block_transposed(double *A,
                                                                     double *B,
                                                                     double *dst,
                                                                     int m_A,
                                                                     int *mask,
                                                                     int mask_len,
                                                                     int n_blocks,
                                                                     int m_per_block)
{
    int m_B = n_blocks * m_per_block;
    int n_A = 0;
    for (int k_mapped=0; k_mapped<mask_len; k_mapped++)
        n_A += mask[k_mapped];
    int n_per_block = n_A / n_blocks;
    
    int mapped_indices[n_A];
    int k=0;
    for (int k_mapped=0; k_mapped<mask_len; k_mapped++)
    {
        if (mask[k_mapped])
        {
            mapped_indices[k] = k_mapped;
            k++;
        }
    }
    
    int j, k_mapped;
    double sum;
    
    for (int i=0; i<m_A; i++)
    {
        for (int block=0; block<n_blocks; block++)
        {
            for (int j_in_block=0; j_in_block<m_per_block; j_in_block++)
            {
                j = j_in_block*n_blocks + block;
                
                sum = 0;
                for (int k_in_block=0; k_in_block<n_per_block; k_in_block++)
                {
                    k = k_in_block*n_blocks + block;
                    k_mapped = mapped_indices[k];
                    
                    sum += A[k_mapped*mask_len + i] * B[k*m_B + j];
                }
                dst[j*m_A + i] = sum;
            }
        }
    }
}

//== MATRIX MULTIPLICATION WHERE BLOCKS ARE EXPLICITLY SPECIFIED ==//

// does NOT overwrite dst
// instead adds to dst
// 
// the rows of B are partitioned into blocks
// block_per_row specifies which block each row of A references
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

void do_matrix_multiply_A_specified_interpolated_blocks_result_symmetric_increment(double *A,
                                                                                   double *B,
                                                                                   double *dst,
                                                                                   int m_A,
                                                                                   int m_B,
                                                                                   int n_blocks,
                                                                                   int *A_block_per_row)
{
    int n_B = m_A;
    int m_per_block = m_B / n_blocks;
    int block, k;
    double sum;
    
    
    for (int i=0; i<m_A; i++)
    {
        block = A_block_per_row[i];
        
        sum = 0;
        for (int k_in_block=0; k_in_block<m_per_block; k_in_block++)
        {
            k = k_in_block*n_blocks + block;
            sum += A[k*m_A + i] * B[i*m_B + k];
        }
        dst[i*m_A + i] += sum;
        
        for (int j=i+1; j<n_B; j++)
        {
            sum = 0;
            for (int k_in_block=0; k_in_block<m_per_block; k_in_block++)
            {
                k = k_in_block*n_blocks + block;
                sum += A[k*m_A + i] * B[j*m_B + k];
            }
            
            dst[j*m_A + i] += sum;
            dst[i*m_A + j] += sum;
        }
    }
}

void do_matrix_multiply_B_transposed_specified_interpolated_blocks(double *A,
                                                                   double *B,
                                                                   double *dst,
                                                                   int m_A,
                                                                   int m_B,
                                                                   int n_B,
                                                                   int n_blocks,
                                                                   int *B_block_per_row)
{
    int n_per_block = n_B / n_blocks;
    int block, k;
    double sum;
    
    for (int j=0; j<m_B; j++) // since we will transpose B, iterate up to m_B not n_B
    {
        block = B_block_per_row[j];
        
        for (int i=0; i<m_A; i++)
        {
            sum = 0;
            for (int k_in_block=0; k_in_block<n_per_block; k_in_block++)
            {
                k = k_in_block*n_blocks + block;
                sum += A[k*m_A + i] * B[k*m_B + j];
            }
            
            dst[j*m_A + i] = sum;
        }
    }
}

//== MATRIX INVERSION and LDL DECOMPOSITION ==/

// the diagonals of dst are the diagonal values of D
// below the diagonal are the values of L
void do_ldl_decompose_matrix(double *M,
                             double *dst,
                             int n)
{
    double sum;
    double diag_value;
    for (int j=0; j<n; j++)
    {
        sum = 0;
        for (int k=0; k<j; k++)
        {
            sum += dst[k*n + j] * dst[k*n + j] * dst[k*n + k];
        }
        diag_value = dst[j*n + j] = M[j*n+j] - sum; 
        //        dst[j*n + j] = sqrt(M[j*n+j] - sum); // for the std cholesky
        
        for (int i=j+1; i<n; i++)
        {
            sum = 0;
            for (int k=0; k<j; k++)
            {
                sum += dst[k*n + i] * dst[k*n + j] * dst[k*n + k];
            }
            dst[j*n + i] = (M[j*n + i] - sum) / diag_value;
            //            dst[j*n + i] = (1 / dst[j*n + j] * (M[j*n + i] - sum));
        }
    }
}

// safe for dst to be the same as M
void do_invert_matrix(double *M,
                      double *dst,
                      int n,
                      double *scratch)
{
    // get the LDL decomposition into scratch
    do_ldl_decompose_matrix(M, scratch, n);
    
    //rename for readibility
    double *L = scratch;
    
    // We are going to solve LDL X = I for X
    // in two steps
    // 1) Forward substitute to solve L u = I for u
    // 2) Backward substitute to solve DL X = u for X <--> L X = inv(D) u
    
    double sum;
    double *x; 
    
    // Step 1 - the forward substitution - one column at a time
    for (int j=0; j<n; j++)
    {
        x = dst + j*n; //x references the jth column 
        
        //since we know the matrix is the identity, can ignore x values above the diagonal
        for (int i=0; i<j; i++)
            x[i] = 0;
        
        x[j] = 1; //setting for i=j. This ignores the diagonal value in the LDL, which is the D, and is 1 in the actual 'L'
        
        for (int i=j+1; i<n; i++)
        {
            sum = 0;
            for (int k=j; k<i; k++) 
            {
                sum += L[k*n + i] * x[k];
            }
            x[i] = -sum;
        }
    }
    
    // Step 1.5 - apply the D^-1
    
    for (int j=0; j<n; j++)
    {
        for (int i=j; i<n; i++)
        {
            dst[j*n + i] /= scratch[i*n + i];
        }
    }
    
    // Step 2 - the forward substitution
    // u is now in dst
    for (int j=0; j<n; j++)
    {
        x = dst + j*n; //x references the jth column 
        //x[n-1] = x[n-1]; - we don't actually need to execute this statement
        for (int i=n-2; i>=j; i--)
        {
            sum = 0;
            for (int k=n-1; k>i; k--)
            {
                sum += L[i*n + k] * x[k];
            }
            x[i] = (x[i] - sum); //note that before this statement u[i] is stored at the same address x[i] will be
        }
        
        for (int i=0; i<j; i++)
            x[i] = dst[i*n+j];
    }
}


NumericVector double_to_numeric_vector(double *arr,
                                       int len)
{
    NumericVector rv(len);
    for (int i=0; i<len; i++)
        rv[i] = arr[i];
    
    return (rv);
}

//-----------------------//
//-- THE MAIN FUNCTION --//
//-----------------------//


// [[Rcpp::export]]
List get_nested_proportion_likelihood_components(NumericMatrix p,
                                                 NumericMatrix n,
                                                 
                                                 List year_metalocation_n_multipliers,
                                                 List year_metalocation_n_multiplier_sd,
                                                 List year_metalocation_p_bias,
                                                 List year_metalocation_p_sd,
                                                 
                                                 double metalocation_p_correlation,
                                                 double metalocation_n_multiplier_correlation,
                                                 
                                                 List year_metalocation_to_year_obs_n_mapping,
                                                 
                                                 List obs_n,
                                                 
                                                 List obs_n_cov_matrices,
                                                 
                                                 LogicalVector year_metalocation_to_year_condition_on_location_mask,
                                                 
                                                 NumericMatrix year_metalocation_to_year_condition_on_location_mapping,
                                                 
                                                 LogicalVector year_metalocation_to_year_obs_location_mask,
                                                 
                                                 NumericMatrix year_metalocation_to_year_obs_location_mapping,
                                                 
                                                 List year_loc_stratum_to_obs_mapping,
                                                 
                                                 List year_metalocation_to_obs_mapping,
                                                 
                                                 IntegerVector obs_year_index,
                                                 
                                                 NumericVector obs_p,
                                                 
                                                 NumericMatrix obs_error
)
{   
    //-- Some General Variables --//
    int n_years = p.nrow();
    int n_strata = p.ncol();
    int n_obs = obs_p.length();
    NumericMatrix map_mat = year_loc_stratum_to_obs_mapping[0];
    int n_obs_locations = map_mat.ncol() / n_years;
    NumericMatrix mat = year_metalocation_p_sd[0];
    int n_metalocations = mat.ncol();
    int n_year_metalocation = n_metalocations * n_years;
    int n_year_obs_location = n_obs_locations * n_years;
    int n_condition_on = year_metalocation_to_year_condition_on_location_mapping.nrow() / n_years;
    
    int max_n_stratum_obs_n = 0;
    for (int d=0; d<n_strata; d++)
    {
        NumericMatrix mat2 = year_metalocation_to_year_obs_n_mapping[d];
        if (mat2.ncol() > max_n_stratum_obs_n)
            max_n_stratum_obs_n = mat2.ncol();
    }
    
    //-- Re-index (R indexes from 1, C++ from 0) --//
    
    int obs_year_index_arr[n_obs];
    for (int t=0; t<n_obs; t++)
        obs_year_index_arr[t] = obs_year_index[t]-1;
    
    
    //-- Some scratch arrays --//
    
    int scratch_span = n_year_metalocation*n_year_metalocation;
    if ((max_n_stratum_obs_n*max_n_stratum_obs_n)>scratch_span)
        scratch_span = max_n_stratum_obs_n*max_n_stratum_obs_n;
    if ((n_condition_on*n_years*n_condition_on*n_years)>scratch_span)
        scratch_span = n_condition_on*n_years*n_condition_on*n_years;
    if ((n_year_obs_location*n_year_obs_location)>scratch_span)
        scratch_span = n_year_obs_location*n_year_obs_location;
    
    int big_scratch_span = 3*scratch_span;
    if (big_scratch_span < (n_obs*n_obs))
        big_scratch_span = n_obs * n_obs;
    
    double big_scratch[big_scratch_span];
    double scratch1[scratch_span];
    double *scratch2 = big_scratch;
    double *scratch3 = scratch2 + scratch_span;
    
    //-- Vectorize some scalars --//
    //^ These don't really need to be a vector; just future-proofing ourselves in case
    //  we someday want to allow the correlation to vary by metalocation
    double theta[n_metalocations];
    fill_arr(theta, metalocation_n_multiplier_correlation, n_metalocations);
    
    double phi[n_metalocations];
    fill_arr(phi, metalocation_p_correlation, n_metalocations);
    
    //-- Set up the mean and covariance for n --//
    
    
    //== THE MEAN AND COVARIANCE MATRICES *BY STRATUM* ==//
    
    
    double *stratum_n;
    
    int index1,index2,total_index;
    double stratum_obs_n[max_n_stratum_obs_n];
    int n_stratum_obs_n;
    double *stratum_mapped_obs_n;
    double *U;
    
    double *M;
    double T[n_year_metalocation*n_year_metalocation];
    double T_Mt[n_year_metalocation * max_n_stratum_obs_n]; // not sure if we could optimize these into a scratch array
    double gamma[n_year_metalocation * max_n_stratum_obs_n];
    
    
    // some short vectors. Not worth doubling with scratch because they are so small
    double tau[n_year_metalocation];
    double lambda[n_year_metalocation];
    double nu[n_year_metalocation];
    double mu[n_year_metalocation];
    double alpha[n_year_metalocation];
    double o[n_years];
    double mean_year_obs_loc[n_obs_locations * n_years];
    
    // some variables invariant to strata (renamed by their math names)
    
    int n_metalocations_for_conditioning = year_metalocation_to_year_condition_on_location_mapping.ncol() / n_years;
    int *mask = year_metalocation_to_year_condition_on_location_mask.begin();
    double *B = year_metalocation_to_year_obs_location_mapping.begin();
    
    
    // The main variables we are producing out of the loop
    NumericVector mean_v_object(n_obs);
    double *mean_v = mean_v_object.begin();
    fill_arr(mean_v, 0, n_obs);
    
    NumericVector obs_n_aggregated_object(n_obs);
    double *obs_n_aggregated = obs_n_aggregated_object.begin();
    fill_arr(obs_n_aggregated, 0, n_obs);
    
    NumericMatrix cov_mat_object(n_obs);
    double *cov_mat = cov_mat_object.begin();
    fill_arr(cov_mat, 0, n_obs*n_obs);
    
    NumericVector obs_v_object(n_obs);
    double *obs_v = obs_v_object.begin();
    
    LogicalVector msa_totals_constrained(1);
    msa_totals_constrained[0] = true;
    
    //= The Main Loop over Strata ==/
    
    for (int d=0; d<n_strata; d++)
    {   
        //== General variables ==//
        stratum_n = n.begin() + d*n_years;
        
        //== Set up the mean and covariance for n ==//
        // Calculate tau
        NumericMatrix mat1 = year_metalocation_n_multiplier_sd[d];
        for (int i=0; i<n_metalocations; i++)
        {
            for (int t=0; t<n_years; t++)
            {
                tau[i*n_years + t] = n[d*n_years + t] * mat1[i*n_years + t];
            }
        }
        
        // Calculate T matrix
        fill_arr(T, 0, n_year_metalocation*n_year_metalocation);
        
        for (int i=0; i<n_metalocations; i++)
        {
            for (int t=0; t<n_years; t++)
            {
                index1 = i*n_years + t;
                T[index1*n_year_metalocation + index1] = tau[index1] * tau[index1];
                
                for (int s=t+1; s<n_years; s++)
                {
                    index2 = i*n_years + s;
                    T[index1*n_year_metalocation + index2] = 
                        T[index2*n_year_metalocation + index1] =  theta[i] * tau[index1] * tau[index2];
                }
            }
        }
        
        //== Condition on observed n's ==//    
        // Fold in mapped obs n together with metalocation n's
        stratum_mapped_obs_n = ((NumericVector) obs_n[d]).begin();
        int stratum_n_mapped_obs_n = ((NumericVector) obs_n[d]).length();
        NumericMatrix u_object = obs_n_cov_matrices[d];
        U = u_object.begin();
        
        n_stratum_obs_n = stratum_n_mapped_obs_n + n_years;
        for (int i=0; i<stratum_n_mapped_obs_n; i++)
            stratum_obs_n[i] = stratum_mapped_obs_n[i];
        for (int i=0; i<n_years; i++) // add in the simulated n's for the MSA/primary location
            stratum_obs_n[stratum_n_mapped_obs_n + i] = stratum_n[i];
        
        // Calculate Gamma Matrix        
        NumericMatrix mat2 = year_metalocation_to_year_obs_n_mapping[d];
        M = mat2.begin(); //M is interpolated block diagonal - one block for each year
        
        do_matrix_multiply_A_block_diagonal_B_transposed(T, M, T_Mt,
                                                         n_metalocations, n_years, n_stratum_obs_n);
        do_matrix_multiply_A_interpolated_block(M, T_Mt, scratch1,
                                                n_years, n_stratum_obs_n/n_years, n_metalocations, n_stratum_obs_n);
        
        add_arrs(scratch1, U, scratch1, n_stratum_obs_n*n_stratum_obs_n); 
        
        double *inv_M_T_Mt_plus_U = scratch1;
        do_invert_matrix(scratch1, inv_M_T_Mt_plus_U, n_stratum_obs_n, scratch2);//store it in the same place: scratch1
        
        do_matrix_multiply(T_Mt, inv_M_T_Mt_plus_U, gamma, 
                           n_year_metalocation, n_stratum_obs_n, n_stratum_obs_n);
        // we no longer need scratch1/inv_M_T_Mt_plus_U - we've put everything we need into gamma
        
        
        // Calculate Lambda
        NumericMatrix mat3 = year_metalocation_n_multipliers[d];
        for (int i=0; i<n_metalocations; i++)
        {
            for (int t=0; t<n_years; t++)
            {
                lambda[i*n_years + t] = n[d*n_years + t] * mat3[i*n_years + t];
            }
        }
        
        // Calculate Nu    
        
        do_matrix_multiply_A_interpolated_block(M, lambda, scratch1, n_years, n_stratum_obs_n/n_years, n_metalocations, 1);
        subtract_arrs(stratum_obs_n, scratch1, scratch1, n_stratum_obs_n);
        double *kappa_minus_M_lambda = scratch1;
        double *backup_lambda = scratch3;
        for (int i=0; i<n_year_metalocation; i++)
            backup_lambda[i] = lambda[i];
        
        bool success_with_no_negative_n = false;
        
        // we're going to loop here to make sure we don't have any negative nu
        int safety_counter = 0;
        double nu_inflation = 1;
        bool constrain_msa_totals = true;
        bool reset_to_not_constrain_msa_totals = true;
        while (!success_with_no_negative_n)
        {
            if (reset_to_not_constrain_msa_totals)
            {
                nu_inflation = 1;
                constrain_msa_totals = false;
                msa_totals_constrained[0] = false;
                
                for (int i=0; i<n_year_metalocation; i++)
                {
                    if (lambda[i] != backup_lambda[i])
                    {
                        for (int j=stratum_n_mapped_obs_n; j<stratum_n_mapped_obs_n+n_years; j++)  
                            // by iterating only through the MSAs obs locations, we are adding "to_add" to the obs.n for the non-MSA locations
                        {
                            if (M[j + i*n_stratum_obs_n]>0)
                                kappa_minus_M_lambda[j] += lambda[i] - backup_lambda[i];
                        }
                        lambda[i] = backup_lambda[i];
                    }
                }
                
                reset_to_not_constrain_msa_totals = false;
            }
            
            do_matrix_multiply(gamma, kappa_minus_M_lambda, nu,
                               n_year_metalocation, n_stratum_obs_n, 1);
            add_arrs(nu, lambda, nu, n_year_metalocation);
            
            success_with_no_negative_n = true;
            
            for (int i=0; i<n_year_metalocation; i++)
            {
                if (nu[i] == R_PosInf || nu[i] == R_NegInf)
                {
                    if (constrain_msa_totals)
                    {
                        reset_to_not_constrain_msa_totals = true;
                        success_with_no_negative_n = false;
                    }
                    else
                    {
                        Rcout << "Error in calculate_nested_likelihood_components() - we have ended up with infinite nu in trying to get rid of negative nu values; we're going to need a new strategy\n";
                        return(R_NilValue);
                    }
                }
                
                if (nu[i]<0)
                {
                    double to_inflate_by = -nu[i] * nu_inflation;
                    lambda[i] += to_inflate_by;
                    
                    success_with_no_negative_n = false;

                    if (constrain_msa_totals)
                    {
                        for (int j=stratum_n_mapped_obs_n; j<stratum_n_mapped_obs_n+n_years; j++)  
                            // by iterating only through the MSAs obs locations, we are adding "to_add" to the obs.n for the non-MSA locations
                        {
                            if (M[j + i*n_stratum_obs_n]>0)
                                kappa_minus_M_lambda[j] -= to_inflate_by;
    
                        }
                    }
                }
            }
            
            safety_counter++;
            nu_inflation *= 2;
            
            if (safety_counter==20)
            {
                reset_to_not_constrain_msa_totals = true;
            }
            
            if (safety_counter>50)
            {
                Rcout << "Error in calculate_nested_likelihood_components() - we have looped 50 times trying to get rid of negative nu without success; we're going to have to recode for a new strategy\n";
                return(R_NilValue);
            }
        }
        

        // Calculate Psi
        double *psi = T; // we are going to overwrite the T array with psi
        
        
        double *gamma_M_t = scratch2;
        // below: note that t(T_Mt) = MT (because T is symmetric)
        do_matrix_multiply_B_transposed(gamma, T_Mt, gamma_M_t, 
                                        n_year_metalocation, n_stratum_obs_n, n_year_metalocation);
        
        subtract_arrs(T, gamma_M_t, psi, n_year_metalocation*n_year_metalocation);
        
        
        //== Set up the mean vectors and covariance matrices for metalocation y's (n*p) ==//
        
        // the mean (mu) - p + bias
        NumericMatrix stratum_year_metalocation_p_bias = year_metalocation_p_bias[d];
        for (int i=0; i<n_metalocations; i++)
        {
            for (int t=0; t<n_years; t++)
            {
                mu[i*n_years + t] = p[d*n_years + t] + stratum_year_metalocation_p_bias[i*n_years + t];
            }
        }
        
        NumericMatrix mat4 = year_metalocation_p_sd[d];
        double *sigma = mat4.begin();
        
        multiply_arrs(mu, nu, alpha, n_year_metalocation);
        
        
        // The delta matrix (which we can overwrite into psi)
        double *delta = psi;
        for (int i=0; i<n_metalocations; i++)
        {
            for (int t=0; t<n_years; t++)
            {
                index1 = i*n_years + t;
                // for i==j
                //  for s==t and i==j
                
                total_index = index1*n_year_metalocation + index1;
                
                delta[total_index] = nu[index1] * mu[index1] * (1-mu[index1]) +
                    psi[total_index] * mu[index1] * mu[index1] +
                    sigma[index1] * sigma[index1] * (nu[index1] * (nu[index1] - 1) + psi[total_index]);
                
                // this is the fully written out version of the above, which collapses terms
                //   kept here for future checking the math
                //   
                //  delta[total_index] = nu[index1] * mu[index1] * (1-mu[index1]) +
                //      sigma[index1] * sigma[index1] * nu[index1] * (nu[index1] - 1) +
                //      psi[total_index] * mu[index1] * mu[index1] +
                //      sigma[index1] * sigma[index1] * psi[total_index];
                
                //  for s!=t and i==j
                for (int s=t+1; s<n_years; s++)
                {
                    index2 = i*n_years + s;
                    total_index = index2*n_year_metalocation + index1;
                    
                    delta[total_index] = 
                        delta[index1*n_year_metalocation + index2] =
                        mu[index1] * mu[index2] * psi[total_index] +
                        phi[i] * sigma[index1] * sigma[index2] *
                        (nu[index1] * nu[index2] + psi[total_index]);
                }
                
                // for i!=j
                for (int j=i+1; j<n_metalocations; j++)
                {
                    for (int s=0; s<n_years; s++)
                    {
                        index2 = j*n_years + s;
                        total_index = index2*n_year_metalocation + index1;
                        
                        delta[total_index] = 
                            delta[index1*n_year_metalocation + index2] =
                            mu[index1] * mu[index2] * psi[total_index];
                    }
                }
            }
        }
        
        //== Condition on MSA (y) totals ==//
        
        // B is interpolated block diagonal 
        //      n_years blocks  x  n_obs_location rows, n_metalocations columns per block
        // M (below) is interpolated block
        //      n_years blocks  x  n_condition_on rows, n_metalocations_for_conditioning columns per block
        // Gamma will be
        //      n_year_obs_location  x  n_years * n_condition_on
        
        //== Make the gamma matrix ==//
        
        M = year_metalocation_to_year_condition_on_location_mapping.begin();
        
        double *delta_Mt = scratch1;
        
        
        do_matrix_multiply_A_col_masked_B_interpolated_block_transposed(delta, M, delta_Mt,
                                                                        n_year_metalocation,
                                                                        mask, n_year_metalocation,
                                                                        n_years,
                                                                        n_condition_on);
        
        
        do_matrix_multiply_A_interpolated_block_B_row_masked_symmetric_result(M, delta_Mt, scratch2,
                                                                              n_years,
                                                                              n_condition_on,
                                                                              n_years * n_condition_on,
                                                                              mask,
                                                                              n_year_metalocation);
        
        do_invert_matrix(scratch2, scratch2, n_years * n_condition_on, scratch3);    
        
        double *B_delta_Mt = scratch3;
        do_matrix_multiply_A_interpolated_block_B_row_masked(B, delta_Mt, scratch3,
                                                             n_years,
                                                             n_obs_locations,
                                                             n_years * n_condition_on,
                                                             year_metalocation_to_year_obs_location_mask.begin(),
                                                             n_year_metalocation);
        
        
        
        // This is going to overwrite the prior gamma - it means something new in this context
        do_matrix_multiply(B_delta_Mt, scratch2, gamma,
                           n_years * n_obs_locations,
                           n_years * n_condition_on,
                           n_years * n_condition_on);
        // ^ this gamma is n_year_obs_location x n_years*n_condition_on
        
        //^ gamma is calculated, scratch1 and scratch2 are free
        //  scratch3 is holding B_delta_Mt
        
        //== Use the gamma matrix to get the mean ==//
        //  NB: scratch3 is still in use, holding B_delta_Mt
        
        //-- Step 1: set up o = p * n
        multiply_arrs(p.begin() + d*n_years, n.begin() + d*n_years, o, n_years);
        
        
        
        
        //-- Step 2: gamma.by.stratum[[d]] %*% (o - M %*% alpha[mask])
        do_matrix_multiply_A_interpolated_block_B_row_masked(M, alpha, scratch1,
                                                             n_years,
                                                             n_condition_on,
                                                             1,
                                                             mask,
                                                             n_year_metalocation);
        
        subtract_arrs(o, scratch1, scratch2, n_years*n_condition_on);
        
        
        
        do_matrix_multiply(gamma, scratch2, mean_year_obs_loc,
                           n_year_obs_location, n_condition_on * n_years, 1);
        // scratch1 and scratch2 now free
        // mean_year_obs_loc is holding our intermediate value
        // scratch3 still in use from above
        
        //-- Step 3: B %*% alpha[year.metalocation.to.year.obs.location.mask]
        
        do_matrix_multiply_A_interpolated_block_B_row_masked(B, alpha, scratch1,
                                                             n_years,
                                                             n_obs_locations,
                                                             1,
                                                             year_metalocation_to_year_obs_location_mask.begin(),
                                                             n_year_metalocation);
        
        //-- Step 4: Add them together
        add_arrs(scratch1, mean_year_obs_loc, mean_year_obs_loc, n_obs_locations*n_years);
        // scratch1 and scratch2 now free
        // scratch3 still in use from above
        // we no longer need alpha
        
        
        //== Use the gamma matrix to get the covariance matrix ==//
        
        //-- Step 1: set up the o-var array
        double *o_var = o;
        multiply_arr1_by_1_minus_arr2(o, p.begin() + d*n_years, o_var, n_years);
        
        //-- Step 2:
        // B %*% delta[year.metalocation.to.year.obs.location.mask,year.metalocation.to.year.obs.location.mask] %*% Bt - 
        // Into scratch1
        
        double *B_delta_Bt = scratch1;
        do_matrix_multiply_A_interpolated_block_B_double_masked(B,
                                                                delta,
                                                                scratch2,
                                                                n_year_obs_location,
                                                                n_years,
                                                                year_metalocation_to_year_obs_location_mask.begin(),
                                                                n_year_metalocation,
                                                                year_metalocation_to_year_obs_location_mask.begin(),
                                                                n_year_metalocation);
        
        do_matrix_multiply_B_interpolated_block_transposed_symmetric_result(scratch2,
                                                                            B,
                                                                            B_delta_Bt,
                                                                            n_year_obs_location,
                                                                            n_year_metalocation,
                                                                            n_years);
        
        // B_delta_Bt is now in scratch1
        // B_delta_Mt is still in scratch3
        // scratch2 is free
        
        //-- Step 3:
        // gamma %*% M %*% delta[mask,year.metalocation.to.year.obs.location.mask] %*% Bt
        // Into scratch2
        
        double *gamma_M_delta_Bt= scratch2;
        do_matrix_multiply_symmetric_result_B_transposed(gamma, B_delta_Mt, gamma_M_delta_Bt,
                                                         n_year_obs_location, n_years*n_condition_on);
        
        // B_delta_Bt is in scratch1
        // gamma_M_delta_Bt is in scratch2
        // scratch3 is free
        
        //-- Step 4:
        // gamma %*% omega %*% t()gamma)
        // Into scratch3
        double *gamma_omega_gamma_t = scratch3;
        do_matrix_multiply_M_D_M_transpose(gamma, o_var, gamma_omega_gamma_t,
                                           n_year_obs_location, n_years*n_condition_on);
        // B_delta_Bt is in scratch1
        // gamma_M_delta_Bt is in scratch2
        // gamma_omega_gamma_t is in scratch3
        
        //-- Step 5: Put it together
        double *cov_mat_year_obs_loc = scratch1;
        for (int i=0; i<n_year_obs_location*n_year_obs_location; i++)
            cov_mat_year_obs_loc[i] = B_delta_Bt[i] - gamma_M_delta_Bt[i] + gamma_omega_gamma_t[i];
        
        // cov_mat_year_obs_loc is in scratch1
        // scratch2 and scratch3 are free
        
        
        //== AGGREGATE ACROSS STRATA ==//  
        
        NumericMatrix mapping_mat = year_loc_stratum_to_obs_mapping[d];
        M = mapping_mat.begin();
        
        
        // The mean
        do_matrix_multiply_A_specified_interpolated_blocks_increment(M,
                                                                     mean_year_obs_loc,
                                                                     mean_v,
                                                                     n_obs,
                                                                     n_year_obs_location,
                                                                     1,
                                                                     n_years,
                                                                     obs_year_index_arr);
        
        // The cov mat
        do_matrix_multiply_B_transposed_specified_interpolated_blocks(cov_mat_year_obs_loc,
                                                                      M,
                                                                      big_scratch,
                                                                      n_year_obs_location,
                                                                      n_obs,
                                                                      n_year_obs_location,
                                                                      n_years,
                                                                      obs_year_index_arr);
        
        
        do_matrix_multiply_A_specified_interpolated_blocks_result_symmetric_increment(M,
                                                                                      big_scratch,
                                                                                      cov_mat,
                                                                                      n_obs,
                                                                                      n_year_obs_location,
                                                                                      n_years,
                                                                                      obs_year_index_arr);
        
        // The aggregated n's
        NumericMatrix mapping_mat2 = year_metalocation_to_obs_mapping[d];
        M = mapping_mat2.begin();
        
        do_matrix_multiply_A_specified_interpolated_blocks_increment(M,
                                                                     nu,
                                                                     obs_n_aggregated,
                                                                     n_obs,
                                                                     n_year_metalocation,
                                                                     1,
                                                                     n_years,
                                                                     obs_year_index_arr);
        
        
        
        
        
        //-- for testing --//
    }
    
    
    //== ADD TO THE COVARIANCE MATRIX (measurement error and var inflation) ==//
    
    do_matrix_multiply_D_M_D(obs_n_aggregated, obs_error.begin(), big_scratch, n_obs);
    add_arrs(cov_mat, big_scratch, cov_mat, n_obs*n_obs);
    //    multiply_arrs(cov_mat, var_inflation.begin(), cov_mat, n_obs*n_obs);
    
    
    //== UPDATE THE OBS VECTOR TO A COUNT (MULTIPLY BY n_obs) ==//
    
    multiply_arrs(obs_p.begin(), obs_n_aggregated, obs_v, n_obs);
    
    //== RETURN ==//
    
    List rv = List::create(Named("obs.v") = obs_v_object,
                           _["obs.n"] = obs_n_aggregated_object,
                           _["mean.v"] = mean_v_object,
                           _["cov.mat"] = cov_mat_object,
                           _["msa.totals.constrained"] = msa_totals_constrained);
    
    return (rv);
}