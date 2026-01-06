
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector do_optimized_get(List numerators,
                               List denominators,
                               List info_by_outcome,
                               int n_to_per_outcome,
                               bool avoid_infinite,
                               bool na_rm)
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
        IntegerVector result_indices = outcome_info["result.indices"];
        
        bool pull_numerator_only = outcome_info["pull.numerator.only"];
        bool pull_denominator_only = outcome_info["pull.denominator.only"];
        bool pull_numerator_denominator_ratio = outcome_info["pull.numerator.denominator.ratio"];
        
        int n_from = from_indices.length();
        
        double *write_into;
        if (pull_numerator_denominator_ratio)
            write_into = numerator_array;
        else
            write_into = dst.begin();
        
        NumericVector pull_from;
        if (pull_denominator_only)
            pull_from = denominators[out];
        else
            pull_from = numerators[out];
        
        if (pull_numerator_denominator_ratio) //otherwise write_into is dst, which is already padded with zeros
        {
            for (int j=0; j<n_to_per_outcome; j++)
                numerator_array[ result_indices[j] ] = 0;
        }
        
        if (na_rm)
        {
            LogicalVector from_is_na = is_na(pull_from);
            for (int i=0; i<n_from; i++)
            {
                if (!from_is_na[ from_indices[i] ])
                    write_into[ to_indices[i] ] += pull_from[ from_indices[i] ];
            }
        }
        else
        {
            for (int i=0; i<n_from; i++)
                write_into[ to_indices[i] ] += pull_from[ from_indices[i] ];
        }
        
        if (pull_numerator_denominator_ratio)
        {
            for (int j=0; j<n_to_per_outcome; j++)
                denominator_array[ result_indices[j] ] = 0;
            
            NumericVector from_denominator = denominators[out];
            if (na_rm)
            {
                LogicalVector from_is_na = is_na(from_denominator);
                for (int i=0; i<n_from; i++)
                {
                    if (!from_is_na[ from_indices[i] ])
                        denominator_array[ to_indices[i] ] += from_denominator[ from_indices[i] ];
                }
            }
            else
            {
                for (int i=0; i<n_from; i++)
                    denominator_array[ to_indices[i] ] += from_denominator[ from_indices[i] ];
            }
            
            if (avoid_infinite)
            {
                for (int j=0; j<n_to_per_outcome; j++)
                {
                    int index = result_indices[j];
                    if (denominator_array[index]!=0) // treats 0/0 as 0to avoid NAs
                        dst[index] = numerator_array[index] / denominator_array[index];
                }
            }
            else
            {
                for (int j=0; j<n_to_per_outcome; j++)
                {
                    int index = result_indices[j];
                    dst[index] = numerator_array[index] / denominator_array[index];
                }
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