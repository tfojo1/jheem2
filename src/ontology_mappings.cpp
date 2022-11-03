// CODE TO SPEED UP ONTOLOGY //


#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void apply_one_dim_ontology_mapping(NumericVector src,
                                    NumericVector dst,
                                    IntegerVector src_dims,
                                    IntegerVector dst_dims,
                                    int map_dimension,
                                    IntegerVector from_indices,
                                    IntegerVector to_indices)
{
    int n_dim = src_dims.length();
    int n_map = from_indices.length();
    
        
    // Put indices into C++ reference (from 0) from R reference (from 1)
    map_dimension--;
    for (int i=0; i<n_map; i++)
    {
        from_indices[i]--;
        to_indices[i]--;
    }
    
    
    // calculate n_before, n_after
    int n_before = 1;
    for (int i=0; i<map_dimension; i++)
        n_before *= src_dims[i];
    
    int n_after = 1;
    for (int i=map_dimension+1; i<n_dim; i++)
        n_after *= src_dims[i];
    
    int n_before_after_dst = n_before * dst_dims[map_dimension];
    int n_before_after_src = n_before * src_dims[map_dimension];
    
    
    // iterate through the arrays, and add from source into dst
    double *rv = dst.begin();
    double *arr = src.begin();
    for (int i_after=0; i_after<n_after; i_after++)
    {
        for (int i_map=0; i_map<n_map; i_map++)
        {
            for (int i_before=0; i_before<n_before; i_before++)
            {
                rv[i_after * n_before_after_dst + to_indices[i_map] * n_before + i_before] +=
                        arr[i_after * n_before_after_src + from_indices[i_map] * n_before + i_before];
            }
        }
    }
}

// [[Rcpp::export]]
void apply_two_dim_ontology_mapping(NumericVector src,
                                    NumericVector dst,
                                    IntegerVector src_dims,
                                    IntegerVector dst_dims,
                                    int map_dimension_1,
                                    int map_dimension_2,
                                    IntegerVector from_indices_1,
                                    IntegerVector to_indices_1,
                                    IntegerVector from_indices_2,
                                    IntegerVector to_indices_2)
{
    int n_dim = src_dims.length();
    int n_map = from_indices_1.length();
    
    // Put indices into C++ reference (from 0) from R reference (from 1)
    map_dimension_1--;
    map_dimension_2--;
    for (int i=0; i<n_map; i++)
    {
        from_indices_1[i]--;
        to_indices_1[i]--;
        
        from_indices_2[i]--;
        to_indices_2[i]--;
    }
    
    // Swap if needed
    int *from1;
    int *from2;
    int *to1;
    int *to2;
    
    if (map_dimension_2 < map_dimension_1)
    {
        int temp = map_dimension_1;
        map_dimension_1 = map_dimension_2;
        map_dimension_2 = temp;
        
        from1 = from_indices_2.begin();
        to1 = to_indices_2.begin();
        
        from2 = from_indices_1.begin();
        to2 = to_indices_1.begin();
    }
    else
    {
        from1 = from_indices_1.begin();
        to1 = to_indices_1.begin();
        
        from2 = from_indices_2.begin();
        to2 = to_indices_2.begin();
    }
    
    
    // calculate n_before, n_after
    int n_before = 1;
    for (int i=0; i<map_dimension_1; i++)
        n_before *= src_dims[i];
    
    int n_between = 1;
    for (int i=map_dimension_1+1; i<map_dimension_2; i++)
        n_between *= src_dims[i];
    
    int n_after = 1;
    for (int i=map_dimension_2+1; i<n_dim; i++)
        n_after *= src_dims[i];
    
    int n_before_between_dst = n_before * dst_dims[map_dimension_1];
    int n_before_between_src = n_before * src_dims[map_dimension_1];
    
    int n_before_2_dst = n_before_between_dst * n_between;
    int n_before_2_src = n_before_between_src * n_between;
    
    int n_before_after_dst = n_before_2_dst * dst_dims[map_dimension_1];
    int n_before_after_src = n_before_2_src * src_dims[map_dimension_2];
    
    
    // iterate through the arrays, and add from source into dst
    double *rv = dst.begin();
    double *arr = src.begin();
    for (int i_after=0; i_after<n_after; i_after++)
    {
        for (int i_map=0; i_map<n_map; i_map++)
        {
            for (int i_between=0; i_between<n_between; i_between++)
            {
                for (int i_before=0; i_before<n_before; i_before++)
                {
                    rv[i_after * n_before_after_dst + 
                        to2[i_map] * n_before_2_dst + 
                        i_between * n_before_between_dst +
                        to1[i_map] * n_before +
                        i_before] +=
                            arr[i_after * n_before_after_src + 
                                from2[i_map] * n_before_2_src +
                                i_between * n_before_between_src +
                                from1[i_map] * n_before +
                                i_before];
                }
            }
        }
    }
}