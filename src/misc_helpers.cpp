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
        while(y[j]<x[i] && j<(y.length()-1))
            j++;
        
        if (x[i] == y[j])
            return (true);
    }
    
    return (false);
}

// [[Rcpp::export]]
NumericVector setdiff_sorted_vectors(NumericVector x, NumericVector y)
{
    if (y.length()==0 || x.length()==0)
        return (x);
    
    int keep_indices[x.length()];
    int n_keep = 0;
    int j = 0;
    
    // Figure out which elements of x we're going to keep
    for (int i=0; i<x.length(); i++)
    {
        while (j<y.length() && y[j]<x[i])
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

// [[Rcpp::export]]
RObject intersect_sorted_vectors(List vectors)
{
    int n_non_empty_vectors = 0;
    int non_empty_vector_indices[vectors.length()];
    int min_v_len;
    
    // Figure out which are non-empty vectors
    for (int i=0; i<vectors.length(); i++)
    {
        if (vectors[i]!=R_NilValue)
        {
            int len = ((NumericVector) vectors[i]).length();
            if (len==0)
            {
                return (vectors[i]);
            }
            else if (len > 0)
            {
                if (n_non_empty_vectors==0)
                    min_v_len = len;
                else if (len < min_v_len)
                    min_v_len = len;
                
                non_empty_vector_indices[n_non_empty_vectors] = i;
                n_non_empty_vectors++;
            }
        }
    }
    
    // If we have 0 or 1 non-empty vectors, we can just return now
    if (n_non_empty_vectors==0) //this means they were all NULL
        return (R_NilValue);
    else if (n_non_empty_vectors==1)
        return ((RObject) vectors[non_empty_vector_indices[0]]);
    
    // set up our temporarty holding array
    double kept_values[min_v_len];
    // stack memory is cheap. We'll set up an array with potentially more space than we need
    //      because doing something clever with malloc to reduce memory is probably still
    //      more expensive
    
    // Initial set-up
    NumericVector v = vectors[0];
    double min_value = v[0];
    int k_with_min = 0;
    int k = 1;
    int n_kept = 0;
    
    int indices_into_vectors[n_non_empty_vectors];
    indices_into_vectors[0] = 1;
    for (int i=0; i<n_non_empty_vectors; i++)
        indices_into_vectors[i] = 0;
    
    while (true)
    {
        v = vectors[k];
        
        if (k == k_with_min)
        {
            // store it
            kept_values[n_kept] = min_value;
            n_kept++;
            
            indices_into_vectors[k]++;
            if (indices_into_vectors[k] >= v.length()) // we are all done
                break;
            min_value = v[ indices_into_vectors[k] ];
        }
        else
        {
            while (indices_into_vectors[k] < v.length() && 
                   v[ indices_into_vectors[k] ] < min_value)
                indices_into_vectors[k]++;
            
            if (indices_into_vectors[k] >= v.length()) // we are all done
                break;
            
            if (v[ indices_into_vectors[k] ] > min_value)
            {
                min_value = v[ indices_into_vectors[k] ];
                k_with_min = k;
            }
        }
        
//        indices_into_vectors[k]++;
        
        k++;
        if (k == n_non_empty_vectors)
            k = 0;
    }
    
    // Copy into exact size numeric vector to return
    NumericVector rv(n_kept);
    for (int i=0; i<n_kept; i++)
        rv[i] = kept_values[i];
    
    // Return
    return (rv);
}

// [[Rcpp::export]]
List interpolate_sorted_vectors(NumericVector v1,
                                NumericVector v2,
                                bool indicator1,
                                bool indicator2)
{
    int n1 = v1.length();
    int n2 = v2.length();
    int n = n1 + n2;
    int i1 = 0;
    int i2 = 0;
    
    NumericVector interpolated(n);
    LogicalVector indicators(n);
    
    for (int i=0; i<n; i++)
    {
        if (i1 == n1 || 
            (i2 < n2 && v2[i2] < v1[i1]))
        {
            interpolated[i] = v2[i2];
            indicators[i] = indicator2;
            i2++;
        }
        else
        {
            interpolated[i] = v1[i1];
            indicators[i] = indicator1;
            i1++;
        }
    }
    
    List rv = List::create(Named("interpolated") = interpolated , _["indicators"] = indicators);
    return (rv);
}


// [[Rcpp::export]]
bool any_overlap_character(CharacterVector c1,
                           CharacterVector c2)
{
    for (int i=0; i<c1.length(); i++)
    {
        for (int j=0; j<c2.length(); j++)
        {
            if (c1[i]==c2[j])
                return (true);
        }
    }
    
    return (false);
}


// [[Rcpp::export]]
bool any_setdiff_character(CharacterVector x,
                           CharacterVector y)
{
    for (int i=0; i<x.length(); i++)
    {
        bool matched;
        for (int j=0; j<y.length() && !matched; j++)
        {
            matched = x[i]==y[j];
        }
        
        if (!matched)
            return (true);
    }
    
    return (false);
}