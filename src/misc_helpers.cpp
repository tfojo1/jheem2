#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool character_vectors_overlap(CharacterVector x, CharacterVector y)
{
    for (int i=0; i<x.length(); i++)
    {
        for (int j=0; j<y.length(); j++)
        {
            if (x[i] == y[j])
                return (true);
        }
    }
    
    return (false);
}

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
    bool any_non_null_vectors = false;
    int n_non_empty_vectors = 0;
    int non_empty_vector_indices[vectors.length()];
    int summed_v_len = 0;
    
    // Figure out which are non-empty vectors
    for (int i=0; i<vectors.length(); i++)
    {
        if (vectors[i]!=R_NilValue)
        {
            any_non_null_vectors = true;
            
            int len = ((NumericVector) vectors[i]).length();
            if (len > 0)
            {
                non_empty_vector_indices[n_non_empty_vectors] = i;
                n_non_empty_vectors++;
        
                summed_v_len += len;
            }
        }
    }
    
    // If we have 0 or 1 non-empty vectors, we can just return now
    if (n_non_empty_vectors==0)
    {
        if (any_non_null_vectors)
            return (NumericVector(0));
        else
            return (R_NilValue);
    }
    else if (n_non_empty_vectors==1)
        return ((RObject) vectors[non_empty_vector_indices[0]]);
    
    // set up our temporarty holding array
    double sorted_values[summed_v_len];
        // stack memory is cheap. We'll set up an array with potentially WAY more space than we need
        //      because doing something clever with malloc to reduce memory is probably still
        //      more expensive

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
    
    // Return
    return (rv);
}

