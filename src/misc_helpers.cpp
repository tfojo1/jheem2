#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool sorted_vectors_overlap(NumericVector x, NumericVector y)
{
    int j=0;
    for (int i=0; i<x.length() && j<y.length(); i++)
    {
        while(y[j]<x[i] && j<y.length())
            j++;
        
        if (x[i] == y[j])
            return (true);
    }
    
    return (false);
}

// [[Rcpp::export]]
NumericVector setdiff_sorted_vectors(NumericVector x, NumericVector y)
{
    int keep_indices[x.length()];
    int n_keep = 0;
    int j = 0;
    
    // Figure out which elements of x we're going to keep
    for (int i=0; i<x.length(); i++)
    {
        while (y[j]<x[i] && j<y.length())
            j++;
        
        if (j==y.length() || x[i] != y[j])
        {
            keep_indices[n_keep] = i;
            n_keep++;
        }
    }
    
    // Transfer x
    NumericVector rv(n_keep);
    for (int i=0; i<n_keep; i++)
        rv[i] = x[ keep_indices[i] ];
    
    // Return
    return (rv);
}

// [[Rcpp::export]]
RObject union_sorted_vectors(List vectors)
{
    int n_non_empty_vectors = 0;
    int non_empty_vector_indices[vectors.length()];
    int max_v_len = 0;
    int summed_v_len = 0;
    
    // Figure out which are non-empty vectors
    for (int i=0; i<vectors.length(); i++)
    {
        if (vectors[i]!=R_NilValue)
        {
            int len = ((NumericVector) vectors[i]).length();
            if (len > 0)
            {
                non_empty_vector_indices[n_non_empty_vectors] = i;
                n_non_empty_vectors++;
        
                summed_v_len += len;
                if (len > max_v_len)
                    max_v_len = len;
            }
        }
    }
    
    // If we have 0 or 1 non-empty vectors, we can just return now
    if (n_non_empty_vectors==0)
        return (R_NilValue);
    else if (n_non_empty_vectors==1)
        return ((RObject) vectors[non_empty_vector_indices[0]]);
    
    // Set up a scratch array with expected size
    double expected_size_factor = 2;
    double increment_size_factor = 1.5;
    int sorted_capacity = max_v_len * expected_size_factor;
    if (sorted_capacity>summed_v_len)
        sorted_capacity = summed_v_len;
    
    double *sorted_values;
    sorted_values = (double *) malloc(sorted_capacity * sizeof(double));

    // Initial set-up
    
    int indices_into_vectors[n_non_empty_vectors];
    double current_value;
    double proposed_value;
    int n_sorted = 0;
    NumericVector v; // a holder
    
    v = (NumericVector) vectors[non_empty_vector_indices[0]];
    proposed_value = v[0];
    for (int i=0; i<n_non_empty_vectors; i++)
    {
        v = (NumericVector) vectors[non_empty_vector_indices[i]];
        if (v[0] < proposed_value)
            proposed_value = v[0];
        
        indices_into_vectors[i] = 0;
    }
    
    //-- Iterate through --//
    
    bool all_done = false;
    while (!all_done)
    {
        // If we need to, expand our sorted vector
        if (n_sorted == sorted_capacity)
        {
            int new_sorted_capacity = sorted_capacity * increment_size_factor;
            double *new_sorted = (double *) malloc(new_sorted_capacity * sizeof(double));
            for (int i=0; i<sorted_capacity; i++)
                new_sorted[i] = sorted_values[i];
            
            free(sorted_values);
            sorted_values = new_sorted;
            sorted_capacity = new_sorted_capacity;
        }
        
        // Store the proposed value
        sorted_values[n_sorted] = proposed_value;
        n_sorted++;
        current_value = proposed_value;
        
        // Get the next proposed value:
        //      Advance all vector indices to the next candidate
        //      Selected the lowest from all the candidates to be our new value
        //      We're all done if all vectors are advanced past their length
        all_done = true;
        for (int i=0; i<n_non_empty_vectors; i++)
        {
            v = (NumericVector) vectors[non_empty_vector_indices[i]];
            while ( indices_into_vectors[i] < v.length() && v[ indices_into_vectors[i] ] <= current_value)
                indices_into_vectors[i]++;
            
            if (indices_into_vectors[i] < v.length())
            {
                all_done = false;
            
                if (v[ indices_into_vectors[i] ] < proposed_value || proposed_value==current_value)
                    proposed_value = v[ indices_into_vectors[i] ];
            }
        }
    }
    
    // Copy into exact size numeric vector to return
    NumericVector rv(n_sorted);
    for (int i=0; i<n_sorted; i++)
        rv[i] = sorted_values[i];
    
    // Deallocate sorted_values
    free(sorted_values);
    
    // Return
    return (rv);
}


NumericVector OLD_union_sorted_vectors(List vectors)
{
    int n_vectors = vectors.length();
    
    if (n_vectors==1)
    {
        NumericVector to_return = ((NumericVector) vectors[0]);
        return (to_return);
    }
    
    double expected_size_factor = 2;
    int max_v_size = 0;
    for (int i=0; i<n_vectors; i++)
    {
        int i_length = ((NumericVector) vectors[i]).length();
        if (i_length > max_v_size)
            max_v_size = i_length;
    }
    
    // pull the first vector
    NumericVector raw_vector = (NumericVector) vectors[0];
    
    // set up the scratch vectors
    int scratch_capacity = max_v_size * expected_size_factor;

    double *scratch1 = (double *) malloc(scratch_capacity * sizeof(double));
    double *scratch2 = scratch1; //we init to scratch1 to avoid a warning. If we will really use it, we will allocate below
    if (n_vectors > 2)
        scratch2 = (double *) malloc(scratch_capacity * sizeof(double));
    
    // set up indices for loop
    int index1;
    int index2;
    
    int n1 = raw_vector.length();
    int n2;
    int n_dst;
    
    int capacity1 = 0; //the init value doesn't actually matter - we just do it to avoid an error
    int capacity2;
    int capacity_dst = scratch_capacity;
    
    double *v1 = raw_vector.begin();
    double *v2;
    double *v_dst = scratch1;

    for (int v_index=1; v_index<n_vectors; v_index++)
    {
        raw_vector = (NumericVector) vectors[v_index];
        n2 = raw_vector.length();
        v2 = raw_vector.begin();
        
        index1 = 0;
        index2 = 0;
        n_dst = 0;
        
      
        while(index1 < n1 && index2 < n2)
        {
            //expand the array
            if (n_dst == capacity_dst) 
            {
                capacity_dst += max_v_size * (expected_size_factor-1);
                double *new_dst = (double *) malloc(capacity_dst * sizeof(double));
                for (int j=0; j<n_dst; j++)
                    new_dst[j] = v_dst[j];
                free(v_dst);
                v_dst = new_dst;
            }

            // copy and advance the indices
            if (v1[index1] < v2[index2]) // copy from 1 and advance 1
            {
                v_dst[n_dst] = v1[index1];
                index1++;
                n_dst++;
            }
            else if (v2[index2] < v1[index1]) // copy from 2 and advance 2
            {
                v_dst[n_dst] = v2[index2];
                index2++;
                n_dst++;
            }
            else //v1[index1] == v2[index2] - copy from 1 and advance both
            {
                v_dst[n_dst] = v1[index1];
                index1++;
                index2++;
                n_dst++;
            }
        }
        
        // finish out what's left from 1 or 2
        while (index1 < n1)
        {
            if (n_dst == capacity_dst) 
            {
                capacity_dst += max_v_size * (expected_size_factor-1);
                double *new_dst = (double *) malloc(capacity_dst * sizeof(double));
                for (int j=0; j<n_dst; j++)
                    new_dst[j] = v_dst[j];
                free(v_dst);
                v_dst = new_dst;
            }
            
            v_dst[n_dst] = v1[index1];
            index1++;
            n_dst++;
        }
        while (index2 < n2)
        {
            if (n_dst == capacity_dst) 
            {
                capacity_dst += max_v_size * (expected_size_factor-1);
                double *new_dst = (double *) malloc(capacity_dst * sizeof(double));
                for (int j=0; j<n_dst; j++)
                    new_dst[j] = v_dst[j];
                free(v_dst);
                v_dst = new_dst;
            }
            
            v_dst[n_dst] = v2[index2];
            index2++;
            n_dst++;
        }
        
        // swap v1 and v_dst - use v2 as the holder
        v2 = v1;
        capacity2 = capacity1;
        
        v1 = v_dst;
        n1 = n_dst;
        capacity1 = capacity_dst;
        if (v_index==1)
        {
            v_dst = scratch2;
            capacity_dst = scratch_capacity;
        }
        else
        {
            v_dst = v2;
            capacity_dst = capacity2;
        }
    }
    
    // Package it up into a numeric vector
    NumericVector rv(n1);
    for (int i=0; i<n1; i++)
        rv[i] = v1[i];
    
    free(v1);
    if (n_vectors>2)
        free(v_dst);
    
    return (rv);
}