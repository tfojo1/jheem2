// CODE TO SPEED UP ONTOLOGY //


#include <Rcpp.h>
using namespace Rcpp;


RObject do_execute_ontology_mapping(List src_dim_names,
                                    List dst_dim_names,
                                    NumericVector src,
                                    NumericVector dst,
                                    CharacterVector from_values,
                                    CharacterVector to_values,
                                    bool get_matrix,
                                    bool na_rm)
{
    //----------------------------//
    //----------------------------//
    //-- PROCESS THE ATTRIBUTES --//
    //----------------------------//
    //----------------------------//
    
    //-- Process dst attributes --//
    if (dst_dim_names.attr("names")==R_NilValue)
        return (R_NilValue);
    CharacterVector dst_dimensions = dst_dim_names.names();
    
    int n_dst_dims = dst_dimensions.length();
    
    int dst_dims[n_dst_dims];
    CharacterVector tmp;
    for (int j=0; j<n_dst_dims; j++)
        dst_dims[j] = ((CharacterVector) dst_dim_names[j]).length();
    
    //-- Process src attributes --//
    if (src_dim_names.attr("names")==R_NilValue)
        return (R_NilValue);
    CharacterVector src_dimensions = src_dim_names.names();
    
    int n_src_dims = src_dimensions.length();
    
    int src_dims[n_src_dims];
    for (int j=0; j<n_src_dims; j++)
        src_dims[j] = ((CharacterVector) src_dim_names[j]).length();
    
    
    //-- Process from_values --//

    if (from_values.attr("dimnames")==R_NilValue)
        return (R_NilValue);
    List from_dim_names = from_values.attr("dimnames");
    if (from_dim_names.length()<2 || from_dim_names[1]==R_NilValue)
        return (R_NilValue);
    
    CharacterVector from_dimensions = from_dim_names[1];
    
    
    int n_from_dims = from_dimensions.length();
    int n_non_from_dims = n_src_dims - n_from_dims;
    
    int from_dim_indices[n_from_dims];
    int non_from_dim_indices[n_non_from_dims];
    
    int non_from_dims[n_non_from_dims];
    int n_non_from = 1;
    int n_before_non_from[n_non_from_dims];
    int n_before_from[n_from_dims];
     
    int non_from_dim_index = 0;
    int n_before = 1;
    
    bool found_match;
    for (int i=0; i<n_src_dims; i++)
    {
        found_match = false;
        for (int j=0; j<n_from_dims && !found_match; j++)
        {
            if (src_dimensions[i] == from_dimensions[j])
            {
                found_match = true;
                
                from_dim_indices[j] = i;
                n_before_from[j] = n_before;
            }
        }
        
        if (!found_match)
        {
            non_from_dim_indices[non_from_dim_index] = i;
            non_from_dims[non_from_dim_index] = src_dims[i];
            n_non_from *= non_from_dims[non_from_dim_index];
            n_before_non_from[non_from_dim_index] = n_before;
            
            non_from_dim_index++;
        }
        
        n_before *= src_dims[i];
    }

    if (from_values.attr("dim")==R_NilValue)
        return (R_NilValue);
    IntegerVector from_dims = from_values.attr("dim");
    int n_values = from_dims[0];
    
    //-- Process to_values --//
    
    if (to_values.attr("dimnames")==R_NilValue)
        return (R_NilValue);
    List to_dim_names = to_values.attr("dimnames");
    if (to_dim_names.length()<2 || to_dim_names[1]==R_NilValue)
        return (R_NilValue);
    
    CharacterVector to_dimensions = (CharacterVector) to_dim_names[1];
    int which_to_dimensions_to_use[to_dimensions.length()];
    
    // Calculate the number of 'to' dimensions actually in the array
    //  We are allowed to ignore omitted to dimensions (as long as at least one is included)
    int n_to_dims = 0;
    for (int j=0; j<to_dimensions.length(); j++)
    {
        for (int k=0; k<dst_dimensions.length(); k++)
        {
            if (to_dimensions[j] == dst_dimensions[k])
            {
                which_to_dimensions_to_use[n_to_dims] = j;
                n_to_dims++;
                break;
            }
        }
    }
    
    if (n_to_dims==0)
        return (R_NilValue);
    
    int n_non_to_dims = n_dst_dims - n_to_dims;
    
    int to_dim_indices[n_to_dims];
    int non_to_dim_indices[n_non_to_dims+1];
    
    int non_to_dims[n_non_to_dims];
    int n_non_to = 1;
    int n_before_non_to[n_non_to_dims];
    int n_before_to[n_to_dims];
    
    int non_to_dim_index = 0;
    n_before = 1;
    
    for (int i=0; i<n_dst_dims; i++)
    {
        found_match = false;
        for (int j=0; j<n_to_dims && !found_match; j++)
        {
            if (dst_dimensions[i] == to_dimensions[which_to_dimensions_to_use[j] ])
            {
                found_match = true;
                to_dim_indices[j] = i;
                n_before_to[j] = n_before;
            }
        }
        
        if (!found_match) 
        {
            non_to_dim_indices[non_to_dim_index] = i;
            non_to_dims[non_to_dim_index] = dst_dims[i];
            n_before_non_to[non_to_dim_index] = n_before;
            
            non_to_dim_index++;
        }
        
        n_before *= dst_dims[i];
    }
    
    if (to_values.attr("dim")==R_NilValue)
        return (R_NilValue);
    IntegerVector to_dims = to_values.attr("dim");
    if (to_dims[0] != n_values)
        return (R_NilValue);
//return (R_NilValue);    
    //----------------------------------//
    //----------------------------------//
    //-- SET UP THE DIMENSION INDICES --//
    //----------------------------------//
    //----------------------------------//
    
    int n_dst = 1;
    for (int i=0; i<n_dst_dims; i++)
        n_dst *= dst_dims[i];
    
    // parse the integer value for each from_dim_values;
    int from_dim_values[n_values][n_from_dims];
    int n_mapped_from = 0;
    CharacterVector dim_values;
    int mapped_from_values_indices[n_values];
    

    bool all_dims_mapped;
    for (int i=0; i<n_values; i++)
    {
        all_dims_mapped = true;
        for (int j=0; j<n_from_dims && all_dims_mapped; j++)
        {
            dim_values = src_dim_names[from_dim_indices[j]];
            from_dim_values[i][j] = -1;
            
            for (int k=0; k<dim_values.length(); k++)
            {
                if (from_values[j*n_values + i] == dim_values[k])
                {
                    from_dim_values[n_mapped_from][j] = k;
                    break;
                }
            }
            
            // if we didn't find a match, we will just ignore this row in from_values/to_values
            all_dims_mapped = from_dim_values[i][j]!=-1;
        }
        
        if (all_dims_mapped)
        {
            mapped_from_values_indices[n_mapped_from] = i;
            n_mapped_from++;
        }
    }
    
    
//return (R_NilValue);      
    int to_dim_values[n_mapped_from][n_to_dims];
    int n_mapped = 0;
    int mapped_to_values_indices[n_mapped_from];
    
    
    for (int i=0; i<n_mapped_from; i++)
    {
        all_dims_mapped = true;
        for (int j=0; j<n_to_dims && all_dims_mapped; j++)
        {
            dim_values = dst_dim_names[to_dim_indices[j]];
            to_dim_values[i][j] = -1;
            for (int k=0; k<dim_values.length(); k++)
            {
                if (to_values[ j*n_values + mapped_from_values_indices[i] ] == dim_values[k])
                {
                    to_dim_values[n_mapped][j] = k;
                    break;
                }
            }

            all_dims_mapped = to_dim_values[i][j]!=-1;
        }
        
        // if we didn't find a match, we will just ignore this row in from_values/to_values
        if (all_dims_mapped)
        {
            mapped_to_values_indices[n_mapped] = i;
            n_mapped++;
        }
    }
    
    if (n_mapped==0) //there is nothing to map - no changes to make to dst
        return (dst);
    

    // cut out any from values that were not used
    for (int i=0; i<n_mapped; i++)
    {
        for (int j=0; j<n_from_dims; j++)
            from_dim_values[i][j] = from_dim_values[ mapped_to_values_indices[i] ][j];
    }
    
    int max_non_from_dim=0;
    for (int j=0; j<n_non_from_dims; j++)
    {
        if (non_from_dims[j] > max_non_from_dim)
            max_non_from_dim = non_from_dims[j];
    }
    
    
    // Map non_from to non_to dimensions
    int non_to_to_non_from_dims[n_non_to_dims];
   
    for (int j=0; j<n_non_to_dims; j++)
    {
        non_to_to_non_from_dims[j] = -1;
        for (int k=0; k<n_non_from_dims; k++)
        {
            if (dst_dimensions[ non_to_dim_indices[j] ] == src_dimensions[ non_from_dim_indices[k] ])
            {
                non_to_to_non_from_dims[j] = k;
                break;
            }
        }
        
        if (non_to_to_non_from_dims[j]==-1)
            return (R_NilValue);
    }
    
    // Map non_from to non_to dimension values
    int non_from_to_non_to_dim_values[n_non_to_dims][max_non_from_dim];
    
    CharacterVector src_values;
    CharacterVector dst_values;
    for (int i=0; i<n_non_to_dims; i++)
    {
        int non_from_d = non_to_to_non_from_dims[i];
        src_values = (CharacterVector) src_dim_names[non_from_dim_indices[non_from_d]];
        dst_values = (CharacterVector) dst_dim_names[non_to_dim_indices[i]];
        
        for (int j=0; j<src_values.length(); j++)
        {
            non_from_to_non_to_dim_values[i][j] = -1;
            for (int k=0; k<dst_values.length(); k++)
            {
                if (src_values[j] == dst_values[k])
                {
                    non_from_to_non_to_dim_values[i][j] = k;
                    break;
                }
            }
            
            if (non_from_to_non_to_dim_values[i][j] == -1)
                return (R_NilValue);
        }
    }
    
    //----------------------------------------------//
    //----------------------------------------------//
    //-- ITERATE THROUGH AND MOVE from SRC to DST --//
    //----------------------------------------------//
    //----------------------------------------------//
    
    int n_for_untouched = (!get_matrix) * n_dst;
    
    bool dst_is_untouched[n_for_untouched];
    for (int i=0; i<n_for_untouched; i++)
        dst_is_untouched[i] = true;
    
    // temp variables for the iteration
    int non_from_dim_values[n_non_from_dims];
    for (int j=0; j<n_non_from_dims; j++)
        non_from_dim_values[j] = 0;
    
    int base_src_index, src_index, base_dst_index, dst_index;
    
    // iterate
    for (int i=0; i<n_non_from; i++)
    {
        base_src_index = 0;
        for (int j=0; j<n_non_from_dims; j++)
            base_src_index += n_before_non_from[j] * non_from_dim_values[j];
        
        base_dst_index = 0;
        for (int j=0; j<n_non_to_dims; j++)
            base_dst_index += n_before_non_to[j] * 
                non_from_to_non_to_dim_values[j][ non_from_dim_values[non_to_to_non_from_dims[j]] ];;

        for (int map_index=0; map_index<n_mapped; map_index++)
        {
            src_index = base_src_index;
            for (int j=0; j<n_from_dims; j++)
                src_index += n_before_from[j] * from_dim_values[map_index][j];
            
            dst_index = base_dst_index;
            for (int j=0; j<n_to_dims; j++)
                dst_index += n_before_to[j] * to_dim_values[map_index][j];
            
            if (get_matrix)
                dst[dst_index + n_dst*src_index] = 1;
            else
            {
                if (!na_rm || !NumericVector::is_na(src[src_index])) // write the value
                {
                    if (dst_is_untouched[dst_index])
                    {
                        dst[dst_index] = src[src_index];
                        dst_is_untouched[dst_index] = false;
                    }
                    else
                        dst[dst_index] += src[src_index];
                }
            }
        }
        
        
        // Update the non_from_dim_values
        for (int j=0; j<n_non_from_dims; j++)
        {
            if (non_from_dim_values[j]==(non_from_dims[j]-1)) //it's at the max - set to zero and increment the value for the next dimension
                non_from_dim_values[j] = 0;
            else
            {
                non_from_dim_values[j]++;
                break;
            }
        }
    }
    
    //-- Return --//
    return (dst);
}


// [[Rcpp::export]]
RObject apply_ontology_mapping(NumericVector src,
                               NumericVector dst,
                               CharacterVector from_values,
                               CharacterVector to_values,
                               bool na_rm)
{   
    //-- Process dst attributes --//
    if (dst.attr("dimnames")==R_NilValue)
        return (R_NilValue);
    List dst_dim_names = dst.attr("dimnames");
    
    //-- Process src attributes --//
    if (src.attr("dimnames")==R_NilValue)
        return (R_NilValue);
    List src_dim_names = src.attr("dimnames");
    
    //-- Call the sub-function --//
    return (do_execute_ontology_mapping(src_dim_names,
                                        dst_dim_names,
                                        src,
                                        dst,
                                        from_values,
                                        to_values,
                                        false,
                                        na_rm));
}

// [[Rcpp::export]]
RObject get_ontology_mapping_matrix(List src_dim_names,
                                    List dst_dim_names,
                                    NumericVector dst,
                                    CharacterVector from_values,
                                    CharacterVector to_values)
{
    return (do_execute_ontology_mapping(src_dim_names,
                                        dst_dim_names,
                                        NULL,
                                        dst,
                                        from_values,
                                        to_values,
                                        true,
                                        true));
}