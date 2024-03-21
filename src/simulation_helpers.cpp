
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector do_optimized_get(List numerators,
                               List denominators,
                               List info_by_outcome,
                               int n_to_per_outcome)
{
    int n_outcomes = info_by_outcome.length();
    
    NumericVector dst(n_to_per_outcome * n_outcomes);
    double numerator_array[n_to_per_outcome * n_outcomes];
    double denominator_array[n_to_per_outcome * n_outcomes];
    
    for (int out=0; out<info_by_outcome.length(); out++)
    {
        List outcome_info = info_by_outcome[out];
        
        IntegerVector from_indices = outcome_info["from.indices"];
        IntegerVector to_indices = outcome_info["to.indices"];
        
        int n_from = from_indices.length();
        
        bool has_denominator = denominators[out]!=R_NilValue;
        double *write_into;
        if (has_denominator)
            write_into = numerator_array;
        else
            write_into = dst.begin();
        
        NumericVector from_numerator = numerators[out];
        
        if (has_denominator) //otherwise write_into is dst, which is already padded with zeros
        {
            for (int j=0; j<n_to_per_outcome; j++)
                numerator_array[ to_indices[j] ] = 0;
        }
        
        for (int i=0; i<n_from; i++)
            write_into[ to_indices[i] ] += from_numerator[ from_indices[i] ];
        
        if (has_denominator)
        {
            for (int j=0; j<n_to_per_outcome; j++)
                denominator_array[ to_indices[j] ] = 0;
            
            NumericVector from_denominator = denominators[out];
            for (int i=0; i<n_from; i++)
                denominator_array[ to_indices[i] ] += from_denominator[ from_indices[i] ];
            for (int j=0; j<n_to_per_outcome; j++)
            {
                int index = to_indices[j];
                if (denominator_array[index]!=0) // treats 0/0 as 0to avoid NAs
                    dst[index] = numerator_array[index] / denominator_array[index];
            }
        }
    }
    
    return (dst);
}


// [[Rcpp::export]]
RObject get_year_indices_for_optimized_info(NumericVector outcome_years,
                                            NumericVector target_years,
                                            int n_before_year_dimension,
                                            int n_after_year_dimension)
{
    int n_outcome_years = outcome_years.length();
    int n_target_years = target_years.length();
    
    IntegerVector rv(n_before_year_dimension * n_target_years * n_after_year_dimension);
    
    int target_to_outcome_year_indices[n_target_years];
    for (int i=0; i<n_target_years; i++)
    {
        bool found = false;
        for (int j=0; j<n_outcome_years; j++)
        {
            if (target_years[i] == outcome_years[j])
            {
                target_to_outcome_year_indices[i] = j;
                found = true;
                break;
            }
        }
        
        if (!found)
            return (R_NilValue);
    }
    
    for (int i_after=0; i_after<n_after_year_dimension; i_after++)
    {
        int from_index_init = i_after * n_before_year_dimension * n_outcome_years;
        int to_index_init = i_after * n_before_year_dimension * n_target_years;
        for (int i_year=0; i_year<n_target_years; i_year++)
        {
            int from_index_base = from_index_init + target_to_outcome_year_indices[i_year] * n_before_year_dimension;
            int to_index_base = to_index_init + i_year * n_before_year_dimension;
            for (int i_before=0; i_before<n_before_year_dimension; i_before++)
            {
                rv[to_index_base + i_before] = from_index_base + i_before;
            }
        }
    }
    
    return (rv);
}