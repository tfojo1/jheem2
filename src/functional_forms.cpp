// CODE TO SPEED UP MODELS //


#include <Rcpp.h>
using namespace Rcpp;



// Returns NULL if an error
// Otherwise, returns a list with two elements: $access.indices and $mapping.indices
// [[Rcpp::export]]
RObject calculate_main_effect_indices(List target_dim_names,
                                      CharacterVector alpha_dimensions,
                                      List alpha_dim_values)
{
    // Initial set-up
    int n_alphas = alpha_dimensions.length();
    int n_dim = target_dim_names.length();
    
    if (target_dim_names.attr("names")==R_NilValue)
        return (R_NilValue);
    CharacterVector dimensions = target_dim_names.names();
    
    // set up dims for target_dim_names
    int dims[target_dim_names.length()];
    for (int i=0; i<n_dim; i++)
        dims[i] = ((CharacterVector) target_dim_names[i]).length();
    
    // set up ns before/in/after dim
    int n_before_dim[n_dim];
    int n_after_dim[n_dim];
    
    n_before_dim[0] = 1;
    n_after_dim[n_dim-1] = 1;
    for (int i=1; i<n_dim; i++)
        n_before_dim[i] = n_before_dim[i-1] * dims[i-1];
    
    for (int i=n_dim-2; i>=0; i--)
        n_after_dim[i] = n_after_dim[i+1] * dims[i+1];
    

    // Set up indexing in to alphas
    int alpha_dims[n_alphas];
    int alpha_indices[n_alphas];
    CharacterVector dim_values;
    RObject alpha_val;
    int j;
    for (int i=0; i<n_alphas; i++)
    {
        // figure out dimension for the ith alpha
        for (j=0; j<n_dim; j++)
        {
            if (alpha_dimensions[i] == dimensions[j])
            {
                alpha_dims[i] = j;
                break;
            }
        }
        
        if (j==n_dim) // no dimension matching name
            return (R_NilValue);
        
        
        // figure out the index for ith alpha
        alpha_val = alpha_dim_values[i];
        if (is<CharacterVector>(alpha_val))
        {
            CharacterVector char_val = (CharacterVector) alpha_val;
            dim_values = (CharacterVector) target_dim_names[ alpha_dims[i] ];
            for (j=0; j<dims[ alpha_dims[i] ]; j++)
            {
                if (dim_values[j] == char_val[0])
                {
                    alpha_indices[i] = j;
                    break;
                }
            }
            
            if (j==dims[ alpha_dims[i] ])
                return (R_NilValue);
        }
        else if (is<IntegerVector>(alpha_val))
        {
            int index = ((IntegerVector) alpha_val)[0];
            if (index<1 || index>dims[ alpha_dims[i] ])
                return (R_NilValue);
            alpha_indices[i] = index-1;
        }
        else if (is<NumericVector>(alpha_val))
        {
            int index = (int) ((IntegerVector) alpha_val)[0];
            if (index<1 || index>dims[ alpha_dims[i] ])
                return (R_NilValue);
            alpha_indices[i] = index-1;
        }
        else
            return (R_NilValue);
            
    }
    
    // Calculate the total number of indices we are going to calculate
    int n = 0;
    for (int i=0; i<n_alphas; i++)
        n += n_before_dim[ alpha_dims[i] ] * n_after_dim[ alpha_dims[i] ];

    // Set up to iterate
    int index = 0;
    int d;
    NumericVector access_indices(n);
    NumericVector mapping_indices(n);
    
    // Iterate through the array
    for (int i=0; i<n_alphas; i++)
    {
        d = alpha_dims[i];
        for (int i_after=0; i_after<n_after_dim[d]; i_after++)
        {
            for (int i_before=0; i_before<n_before_dim[d]; i_before++)
            {
                access_indices[index] = 1 + i_before +
                    alpha_indices[i] * n_before_dim[d] +
                    i_after * n_before_dim[d] * dims[d];
                
                mapping_indices[index] = 1 + i; //the "1+" puts it into R indexing, which is from 1
                
                index++;
            }
        }
    }
    
    // Package up and return
    return(List::create(Named("access.indices") = access_indices,
                        _["mapping.indices"] = mapping_indices));
}

//-- HELPER --//

// Returns false if error, true otherwise
bool do_map_indices(int &dim_index,
                    int *dim_value_indices,
                    CharacterVector dimension_for_values,
                    List dimension_values,
                    CharacterVector dimensions,
                    List dim_names)
{
    // Identify the index of the dimension
    dim_index = -1;
    for (int i=0; i<dimensions.length(); i++)
    {
        if (dimensions[i] == dimension_for_values[0])
        {
            dim_index = i;
            break;
        }
    }
    
    if (dim_index == -1)
        return (false);
    
    // Identify the indices for values
    CharacterVector map_to_values = (CharacterVector) dim_names[dim_index];
    RObject val;
    CharacterVector char_val;
    for (int i=0; i<dimension_values.length(); i++)
    {
        val = dimension_values[i];
        if (is<CharacterVector>(val))
        {
            dim_value_indices[i] = -1;
            char_val = (CharacterVector) val;
            for (int j=0; j<map_to_values.length(); j++)
            {
                if (map_to_values[j] == char_val[0])
                {
                    dim_value_indices[i] = j;
                    break;
                }
            }
            
            if (dim_value_indices[i] == -1)
                return (false);
        }
        else if (is<IntegerVector>(val))
        {
            int index = ((IntegerVector) val)[0];
            if (index<1 || index>map_to_values.length())
                return (false);
            dim_value_indices[i] = index-1;
        }
        else if (is<NumericVector>(val))
        {
            int index = (int) ((IntegerVector) val)[0];
            if (index<1 || index>map_to_values.length())
                return (false);
            dim_value_indices[i] = index-1;
        }
        else
            return (false);
    }
    
    
    // Return
    return (true);
}



// [[Rcpp::export]]
RObject calculate_two_way_interaction_indices(List target_dim_names,
                                              CharacterVector alpha_dimension1,
                                              List alpha_dim1_values,
                                              CharacterVector alpha_dimension2,
                                              List alpha_dim2_values)
{
    // Initial set-up
    int n_interactions = alpha_dim1_values.length();
    int n_dim = target_dim_names.length();
    
    if (target_dim_names.attr("names")==R_NilValue)
        return (R_NilValue);
    CharacterVector dimensions = target_dim_names.names();
    
    // set up dims for target_dim_names
    int dims[target_dim_names.length()];
    for (int i=0; i<n_dim; i++)
        dims[i] = ((CharacterVector) target_dim_names[i]).length();
    
    // map the dimensions and values to indices
    int dim1, dim2;
    int dim1_values[n_interactions];
    int dim2_values[n_interactions];
    
    if (!do_map_indices(dim1,
                   dim1_values,
                   alpha_dimension1,
                   alpha_dim1_values,
                   dimensions,
                   target_dim_names))
        return (R_NilValue);
    
    if (!do_map_indices(dim2,
                   dim2_values,
                   alpha_dimension2,
                   alpha_dim2_values,
                   dimensions,
                   target_dim_names))
        return (R_NilValue);
    
    // make sure dim 1 is less than dim 2
    int* dv1;
    int *dv2;
    if (dim1<dim2)
    {
        dv1 = dim1_values;
        dv2 = dim2_values;
    }
    else
    {
        int temp = dim2;
        dim2 = dim1;
        dim1 = temp;
        
        dv1 = dim2_values;
        dv2 = dim1_values;
    }
    
    
    
    // Calculate n for each section we'll iterate through
    int n_before_1 = 1;
    for (int i=0; i<dim1; i++)
        n_before_1 *= dims[i];
    int n_dim1 = dims[dim1];
    int n_between_1_2 = 1;
    for (int i=(dim1+1); i<dim2; i++)
        n_between_1_2 *= dims[i];
    int n_dim2 = dims[dim2];
    int n_after_2 = 1;
    for (int i=(dim2+1); i<n_dim; i++)
        n_after_2 *= dims[i];
    
    int n_before_between = n_before_1 * n_dim1;
    int n_before_2 = n_before_between * n_between_1_2;
    int n_before_after = n_before_2 * n_dim2;
    
    // Calculate the total n and set up the return value structures
    
    int n = n_interactions * n_before_1 * n_between_1_2 * n_after_2;
    NumericVector access_indices(n);
    NumericVector mapping_indices(n);
    int index = 0;
    
    // Iterate through the array and add (or set)
    for (int i_interaction = 0; i_interaction<n_interactions; i_interaction++)
    {
        for (int i_after = 0; i_after<n_after_2; i_after++)
        {
            for (int i_between = 0; i_between<n_between_1_2; i_between++)
            {
                for (int i_before=0; i_before<n_before_1; i_before++)
                {
                    access_indices[index] = 1 + i_after*n_before_after + 
                        dv2[i_interaction]*n_before_2 + 
                        i_between*n_before_between +
                        dv1[i_interaction]*n_before_1 +
                        i_before;
                    
                    mapping_indices[index] = 1 + i_interaction; //the "1+" puts it into R indexing, which is from 1
                    
                    index++;
                }
            }
        }
    }
    
    
    // Package up and return
    return(List::create(Named("access.indices") = access_indices,
                        _["mapping.indices"] = mapping_indices));
}

// [[Rcpp::export]]
RObject calculate_three_way_interaction_indices(List target_dim_names,
                                              CharacterVector alpha_dimension1,
                                              List alpha_dim1_values,
                                              CharacterVector alpha_dimension2,
                                              List alpha_dim2_values,
                                              CharacterVector alpha_dimension3,
                                              List alpha_dim3_values)
{
    // Initial set-up
    int n_interactions = alpha_dim1_values.length();
    int n_dim = target_dim_names.length();
    
    if (target_dim_names.attr("names")==R_NilValue)
        return (R_NilValue);
    CharacterVector dimensions = target_dim_names.names();
    
    // set up dims for target_dim_names
    int dims[target_dim_names.length()];
    for (int i=0; i<n_dim; i++)
        dims[i] = ((CharacterVector) target_dim_names[i]).length();
    
    // map the dimensions and values to indices
    int dim1, dim2, dim3;
    int dim1_values[n_interactions];
    int dim2_values[n_interactions];
    int dim3_values[n_interactions];
    
    if (!do_map_indices(dim1,
                        dim1_values,
                        alpha_dimension1,
                        alpha_dim1_values,
                        dimensions,
                        target_dim_names))
        return (R_NilValue);
    
    if (!do_map_indices(dim2,
                        dim2_values,
                        alpha_dimension2,
                        alpha_dim2_values,
                        dimensions,
                        target_dim_names))
        return (R_NilValue);
    
    if (!do_map_indices(dim3,
                        dim3_values,
                        alpha_dimension3,
                        alpha_dim3_values,
                        dimensions,
                        target_dim_names))
        return (R_NilValue);
    
    // order dim1, dim2, dim3 ascending
    int* dv1;
    int* dv2;
    int* dv3;
    if (dim1<dim2 && dim1<dim3)
    {
        dv1 = dim1_values;
        // don't need to reset dim1
        if (dim2<dim3)
        {
            dv2 = dim2_values;
            dv3 = dim3_values;
            // don't need to change dim2 or dim3
        }
        else
        {
            dv2 = dim3_values;
            dv3 = dim2_values;
            
            int temp = dim3;
            dim3 = dim2;
            dim2 = temp;
        }
    }
    else if (dim2<dim3) //dim2 is first
    {
        dv1 = dim2_values;
        if (dim1 < dim3) // just need to swap dim1 and dim2
        {
            dv2 = dim1_values;
            dv3 = dim3_values;
            
            int temp = dim1;
            dim1 = dim2;
            dim2 = temp;
        }
        else // rotate all 3 dims
        {
            dv2 = dim3_values;
            dv3 = dim1_values;
            
            int temp = dim1;
            dim1 = dim2;
            dim2 = dim3;
            dim3 = temp;
        }
    }
    else //dim3 is first
    {
        dv1 = dim3_values;
        if (dim1 < dim2) // rotate all 3 dims
        {
            dv2 = dim1_values;
            dv3 = dim2_values;
            
            int temp = dim1;
            dim1 = dim3;
            dim3 = dim2;
            dim2 = temp;
        }
        else // just need to swap dim1 and dim3
        {
            dv2 = dim2_values;
            dv3 = dim1_values;
            
            int temp = dim3;
            dim3 = dim1;
            dim1 = temp;
        }
    }
    

    // Calculate n for each section we'll iterate through
    int n_before_1 = 1;
    for (int i=0; i<dim1; i++)
        n_before_1 *= dims[i];
    int n_dim1 = dims[dim1];
    int n_between_1_2 = 1;
    for (int i=(dim1+1); i<dim2; i++)
        n_between_1_2 *= dims[i];
    int n_dim2 = dims[dim2];
    int n_between_2_3 = 1;
    for (int i=(dim2+1); i<dim3; i++)
        n_between_2_3 *= dims[i];
    int n_dim3 = dims[dim3];
    int n_after_3 = 1;
    for (int i=(dim3+1); i<n_dim; i++)
        n_after_3 *= dims[i];
    
    int n_before_between_1_2 = n_before_1 * n_dim1;
    int n_before_2 = n_before_between_1_2 * n_between_1_2;
    int n_before_between_2_3 = n_before_2 * n_dim2;
    int n_before_3 = n_before_between_2_3 * n_between_2_3;
    int n_before_after = n_before_3 * n_dim3;
    
    // Calculate the total n and set up the return value structures
    
    int n = n_interactions * n_before_1 * n_between_1_2 * n_between_2_3 * n_after_3;
    NumericVector access_indices(n);
    NumericVector mapping_indices(n);
    int index = 0;
    
    // Iterate through the array and add (or set)
    
    for (int i_interaction = 0; i_interaction<n_interactions; i_interaction++)
    {
        for (int i_after = 0; i_after<n_after_3; i_after++)
        {
            for (int i_between_2_3 = 0; i_between_2_3<n_between_2_3; i_between_2_3++)
            {
                for (int i_between_1_2 = 0; i_between_1_2<n_between_1_2; i_between_1_2++)
                {
                    for (int i_before=0; i_before<n_before_1; i_before++)
                    {
                        access_indices[index] = 1 + i_after*n_before_after + 
                            dv3[i_interaction]*n_before_3 +
                            i_between_2_3*n_before_between_2_3 +
                            dv2[i_interaction]*n_before_2 + 
                            i_between_1_2*n_before_between_1_2 +
                            dv1[i_interaction]*n_before_1 +
                            i_before;
                            
                        mapping_indices[index] = 1 + i_interaction; //the "1+" puts it into R indexing, which is from 1
                        
                        index++;
                    }
                }
            }
        }
    }

    // Package up and return
    return(List::create(Named("access.indices") = access_indices,
                        _["mapping.indices"] = mapping_indices));
}

// [[Rcpp::export]]
RObject calculate_four_way_interaction_indices(List target_dim_names,
                                                CharacterVector alpha_dimension1,
                                                List alpha_dim1_values,
                                                CharacterVector alpha_dimension2,
                                                List alpha_dim2_values,
                                                CharacterVector alpha_dimension3,
                                                List alpha_dim3_values,
                                                CharacterVector alpha_dimension4,
                                                List alpha_dim4_values)
{
    // Initial set-up
    int n_interactions = alpha_dim1_values.length();
    int n_dim = target_dim_names.length();
    
    if (target_dim_names.attr("names")==R_NilValue)
        return (R_NilValue);
    CharacterVector dimensions = target_dim_names.names();
    
    // set up dims for target_dim_names
    int dims[target_dim_names.length()];
    for (int i=0; i<n_dim; i++)
        dims[i] = ((CharacterVector) target_dim_names[i]).length();
    
    // map the dimensions and values to indices
    int dim1, dim2, dim3, dim4;
    int dim1_values[n_interactions];
    int dim2_values[n_interactions];
    int dim3_values[n_interactions];
    int dim4_values[n_interactions];
    
    if (!do_map_indices(dim1,
                        dim1_values,
                        alpha_dimension1,
                        alpha_dim1_values,
                        dimensions,
                        target_dim_names))
        return (R_NilValue);
    
    if (!do_map_indices(dim2,
                        dim2_values,
                        alpha_dimension2,
                        alpha_dim2_values,
                        dimensions,
                        target_dim_names))
        return (R_NilValue);
    
    if (!do_map_indices(dim3,
                        dim3_values,
                        alpha_dimension3,
                        alpha_dim3_values,
                        dimensions,
                        target_dim_names))
        return (R_NilValue);
    
    if (!do_map_indices(dim4,
                        dim4_values,
                        alpha_dimension4,
                        alpha_dim4_values,
                        dimensions,
                        target_dim_names))
        return (R_NilValue);
    
    // order dim1, dim2, dim3 ascending
    int* dv1;
    int* dv2;
    int* dv3;
    int* dv4;
    if (dim1<dim2 && dim1<dim3 && dim1<dim4) //dim1 is first
    {
        dv1 = dim1_values;
        // don't need to reset dim1
        if (dim2<dim3 && dim2<dim4) //dim2 is second
        {
            dv2 = dim2_values;
            
            if (dim3<dim4) //1,2,3,4
            {
                dv3 = dim3_values;
                dv4 = dim4_values;
                
                //don't need to switch
            }
            else //1,2,4,3
            {
                dv3 = dim4_values;
                dv4 = dim3_values;
                
                //switch dim3,4
                int temp = dim4;
                dim4 = dim3;
                dim3 = temp;
            }
        }
        else if (dim3<dim4) //dim3 is second
        {
            dv2 = dim3_values;
            
            if (dim2<dim4) //1,3,2,4
            {
                dv3 = dim2_values;
                dv4 = dim4_values;
                
                // switch dim2 and dim3
                int temp = dim2;
                dim2 = dim4;
                dim4 = temp;
            }
            else //1,3,4,2
            {
                dv3 = dim4_values;
                dv4 = dim2_values;
                
                //switch dim2,3,4
                int temp = dim2;
                dim2 = dim3;
                dim3 = dim4;
                dim4 = temp;
            }
        }
        else //dim4 is second
        {
            dv2 = dim4_values;
            
            if (dim2<dim3) //1,4,2,3
            {
                dv3 = dim2_values;
                dv4 = dim3_values;
                
                //switch dim4,2,3
                int temp = dim2;
                dim2 = dim4;
                dim4 = dim3;
                dim3 = temp;
            }
            else //1,4,3,2
            {
                dv3 = dim3_values;
                dv4 = dim2_values;
                
                //switch dim4,2
                int temp = dim2;
                dim2 = dim4;
                dim4 = temp;
            }
        }
    }
    else if (dim2<dim3 && dim2<dim4) //dim2 is first
    {
        dv1 = dim2_values;
        if (dim1<dim3 && dim1<dim4) // dim1 is second
        {
            dv2 = dim1_values;
            
            if (dim3<dim4) //2,1,3,4
            {
                dv3 = dim3_values;
                dv4 = dim4_values;
                
                //switch dim1,2
                int temp = dim1;
                dim1 = dim2;
                dim2 = temp;
            }
            else //2,1,4,3
            {
                dv3 = dim4_values;
                dv4 = dim3_values;
                
                //switch dim1,2
                int temp = dim1;
                dim1 = dim2;
                dim2 = temp;
                
                //switch dim3,4
                temp = dim3;
                dim3 = dim4;
                dim4 = temp;
            }
        }
        else if (dim3<dim4) // dim3 is second
        {
            dv2 = dim3_values;
            
            if (dim1<dim4) //2,3,1,4
            {
                dv3 = dim1_values;
                dv4 = dim4_values;
                
                //switch dim1,2,3
                int temp = dim1;
                dim1 = dim2;
                dim2 = dim3;
                dim3 = temp;
            }
            else //2,3,4,1
            {
                dv3 = dim4_values;
                dv4 = dim1_values;
                
                //switch all
                int temp = dim1;
                dim1 = dim2;
                dim2 = dim3;
                dim3 = dim4;
                dim4 = temp;
            }
        }
        else // dim4 is second
        {
            dv2 = dim3_values;
            
            if (dim1<dim3) //2,4,1,3
            {
                dv3 = dim1_values;
                dv4 = dim3_values;
                
                //switch all
                int temp = dim1;
                dim1 = dim2;
                dim2 = dim4;
                dim4 = dim3;
                dim3 = temp;
            }
            else //2,4,3,1
            {
                dv3 = dim3_values;
                dv4 = dim1_values;
                
                //switch dim2,4,1
                int temp = dim1;
                dim1 = dim2;
                dim2 = dim4;
                dim4 = temp;
            }
        }
    }
    else if (dim3<dim4) //dim3 is first
    {
        dv1 = dim3_values;
        
        if (dim1<dim2 && dim1<dim4) //dim1 is second
        {
            dv2 = dim1_values;
            
            if (dim2<dim4) //3,1,2,4
            {
                dv3 = dim2_values;
                dv4 = dim4_values;
                
                //switch dim3,1,2
                int temp = dim1;
                dim1 = dim3;
                dim3 = dim2;
                dim2 = temp;
            }
            else //3,1,4,2
            {
                dv3 = dim4_values;
                dv4 = dim2_values;
                
                //switch all
                int temp = dim1;
                dim1 = dim3;
                dim3 = dim4;
                dim4 = dim2;
                dim2 = temp;
            }
        }
        else if (dim2<dim4) //dim2 is second
        {
            dv2 = dim2_values;
            
            if (dim1<dim4) //3,2,1,4
            {
                dv3 = dim1_values;
                dv4 = dim4_values;
                
                //switch dim3,1
                int temp = dim1;
                dim1 = dim3;
                dim3 = temp;
            }
            else //3,2,4,1
            {
                dv3 = dim4_values;
                dv4 = dim1_values;
                
                //switch dim3,4,1
                int temp = dim1;
                dim1 = dim3;
                dim3 = dim4;
                dim4 = temp;
            }
        }
        else //dim4 is second
        {
            dv2 = dim4_values;
            
            if (dim1<dim2) //3,4,1,2
            {
                dv3 = dim1_values;
                dv4 = dim2_values;
                
                //switch dim1,3
                int temp = dim1;
                dim1 = dim3;
                dim3 = temp;
                
                //switch dim2,4
                temp = dim2;
                dim2 = dim4;
                dim4 = temp;
            }
            else //3,4,2,1
            {
                dv3 = dim2_values;
                dv4 = dim1_values;
                
                //switch all
                int temp = dim1;
                dim1 = dim3;
                dim3 = dim2;
                dim2 = dim4;
                dim4 = temp;
            }
        }
    }
    else//dim4 is first
    {
        dv1 = dim4_values;
        
        if (dim1<dim2&& dim1<dim3) //dim1 is second
        {
            dv2 = dim1_values;
            
            if (dim2<dim3) //4,1,2,3
            {
                dv3 = dim2_values;
                dv4 = dim3_values;
                
                //switch all
                int temp = dim1;
                dim1 = dim4;
                dim4 = dim3;
                dim3 = dim2;
                dim2 = temp;
            }
            else //4,1,3,2
            {
                dv3 = dim3_values;
                dv4 = dim2_values;
                
                //switch dims4,1,2
                int temp = dim1;
                dim1 = dim4;
                dim4 = dim2;
                dim2 = temp;
            }
        }
        else if (dim2<dim3) //dim2 is second
        {
            dv2 = dim2_values;
            
            if (dim1<dim3) //4,2,1,3
            {
                dv3 = dim1_values;
                dv4 = dim3_values;
                
                //switch dim4,1,3
                int temp = dim1;
                dim1 = dim4;
                dim4 = dim3;
                dim3= temp;
            }
            else //4,2,3,1
            {
                dv3 = dim3_values;
                dv4 = dim1_values;
                
                //switch dim4,1
                int temp = dim1;
                dim1 = dim4;
                dim4 = temp;
            }
        }
        else //dim3 is second
        {
            dv2 = dim3_values;
            
            if (dim1<dim2) //4,3,1,2
            {
                dv3 = dim1_values;
                dv4 = dim2_values;
                
                //switch all
                int temp = dim1;
                dim1 = dim4;
                dim4 = dim2;
                dim2 = dim3;
                dim3 = temp;
            }
            else //4,3,2,1
            {
                dv3 = dim2_values;
                dv4 = dim1_values;
                
                //switch dim4,1
                int temp = dim1;
                dim1 = dim4;
                dim4 = temp;
                
                //switch dim3,2
                temp = dim2;
                dim2 = dim3;
                dim3 = temp;
            }
        }
    }
    
    
    // Calculate n for each section we'll iterate through
    int n_before_1 = 1;
    for (int i=0; i<dim1; i++)
        n_before_1 *= dims[i];
    int n_dim1 = dims[dim1];
    int n_between_1_2 = 1;
    for (int i=(dim1+1); i<dim2; i++)
        n_between_1_2 *= dims[i];
    int n_dim2 = dims[dim2];
    int n_between_2_3 = 1;
    for (int i=(dim2+1); i<dim3; i++)
        n_between_2_3 *= dims[i];
    int n_dim3 = dims[dim3];
    int n_between_3_4 = 1;
    for (int i=(dim3+1); i<dim4; i++)
        n_between_3_4*= dims[i];
    int n_dim4 = dims[dim4];
    int n_after_4 = 1;
    for (int i=(dim4+1); i<n_dim; i++)
        n_after_4 *= dims[i];
    
    int n_before_between_1_2 = n_before_1 * n_dim1;
    int n_before_2 = n_before_between_1_2 * n_between_1_2;
    int n_before_between_2_3 = n_before_2 * n_dim2;
    int n_before_3 = n_before_between_2_3 * n_between_2_3;
    int n_before_between_3_4 = n_before_3 * n_dim3;
    int n_before_4 = n_before_between_3_4 * n_between_3_4;
    int n_before_after = n_before_4 * n_dim4;
    
    
    // Calculate the total n and set up the return value structures
    
    int n = n_interactions * n_before_1 * n_between_1_2 * n_between_2_3 * n_between_3_4 * n_after_4;
    NumericVector access_indices(n);
    NumericVector mapping_indices(n);
    int index = 0;
    
    
    // Iterate through the array and add (or set)
    
    for (int i_interaction = 0; i_interaction<n_interactions; i_interaction++)
    {
        for (int i_after = 0; i_after<n_after_4; i_after++)
        {
            for (int i_between_3_4 = 0; i_between_3_4<n_between_3_4; i_between_3_4++)
            {
                for (int i_between_2_3 = 0; i_between_2_3<n_between_2_3; i_between_2_3++)
                {
                    for (int i_between_1_2 = 0; i_between_1_2<n_between_1_2; i_between_1_2++)
                    {
                        for (int i_before=0; i_before<n_before_1; i_before++)
                        {
                            access_indices[index] = 1 + i_after*n_before_after + 
                                dv4[i_interaction]*n_before_4 +
                                i_between_3_4*n_before_between_3_4 +
                                dv3[i_interaction]*n_before_3 +
                                i_between_2_3*n_before_between_2_3 +
                                dv2[i_interaction]*n_before_2 + 
                                i_between_1_2*n_before_between_1_2 +
                                dv1[i_interaction]*n_before_1 +
                                i_before;
                            
                            mapping_indices[index] = 1 + i_interaction; //the "1+" puts it into R indexing, which is from 1
                            
                            index++;
                        }
                    }
                }
            }
        }
    }
    
    // Package up and return
    return(List::create(Named("access.indices") = access_indices,
                        _["mapping.indices"] = mapping_indices));
}


    



//-- OLD VERSIONS of FUNCTIONS --//
//  (The below were the functions when we directly added/overwrote alphas into array;
//   now we pre-calculate indices and use the indices to quickly add or overwrite.
//   We keep these functions here in case of future changes to code)

// [[Rcpp::export]]
void do_add_alphas_to_arr(NumericVector arr,
                          IntegerVector dims,
                          NumericVector alpha_values,
                          IntegerVector alpha_dims, //indexed from 1, as in R
                          IntegerVector alpha_indices //indexed from 1, as in R
)
{
    // some set up
    int n_alphas = alpha_values.length();
    int n_dim = dims.length();
    
    // set up ns before/in/after dim
    int *n_in_dim = dims.begin();
    int n_before_dim[n_dim];
    int n_after_dim[n_dim];
    
    n_before_dim[0] = 1;
    n_after_dim[n_dim-1] = 1;
    for (int i=1; i<n_dim; i++)
        n_before_dim[i] = n_before_dim[i-1] * n_in_dim[i-1];
    
    for (int i=n_dim-2; i>=0; i--)
        n_after_dim[i] = n_after_dim[i+1] * n_in_dim[i+1];
    
    // re-index alpha_dims and alpha_indices from zero
    for (int i=0; i<n_alphas; i++)
    {
        alpha_dims[i]--;
        alpha_indices[i]--;
    }
    
    // set up the rv
    double *a = arr.begin();
    
    int dim;

    // iterate through the array
    for (int i=0; i<n_alphas; i++)
    {
        dim = alpha_dims[i];
        for (int i_before=0; i_before<n_before_dim[dim]; i_before++)
        {
            for (int i_after=0; i_after<n_after_dim[dim]; i_after++)
            {
                a[i_before +//* n_after_dim[dim] +
                    alpha_indices[i] * n_before_dim[dim] +
                    i_after * n_before_dim[dim] * n_in_dim[dim] ] += alpha_values[i];
            }
        }
    }
}

void do_set_alphas_to_arr(NumericVector arr,
                          IntegerVector dims,
                          NumericVector alpha_values,
                          int alpha_dim, //indexed from 1, as in R
                          IntegerVector alpha_indices)
{
    // some set up
    int n_alphas = alpha_values.length();
    int n_dim = dims.length();
    int n_before = 1;
    int n_after = 1;
    
    // re-index alpha_dims and alpha_indices from zero
    for (int i=0; i<n_alphas; i++)
        alpha_indices[i]--;
   
    alpha_dim--;
    
    // calcualte before/after
    for (int i=0; i<alpha_dim; i++)
        n_before = n_before * dims[i];
    
    for (int i=n_dim-1; i>alpha_dim; i--)
        n_after = n_after * dims[i];
    
    int n_before_after = n_before * dims[alpha_dim];
    
    double *a = arr.begin();
    
    // iterate through
    
    for (int i_after=0; i_after<n_after; i_after++)
    {
        for (int i=0; i<n_alphas; i++)
        {
            for (int i_before=0; i_before<n_before; i_before++)
            {
                a[i_after * n_before_after +
                    i * n_before + i_before] = alpha_values[i];
            }
        }
    }
}

// [[Rcpp::export]]
void do_add_or_set_two_way_interaction_alphas_to_arr(NumericVector arr,
                                                     IntegerVector dims,
                                                     int dim1,
                                                     IntegerVector dim1_values,
                                                     int dim2,
                                                     IntegerVector dim2_values,
                                                     NumericVector values,
                                                     bool add
)
{
    // make sure dim 1 is less than dim 2
    int* dv1;
    int *dv2;
    if (dim1<dim2)
    {
        dv1 = dim1_values.begin();
        dv2 = dim2_values.begin();
    }
    else
    {
        int temp = dim2;
        dim2 = dim1;
        dim1 = temp;
        
        dv1 = dim2_values.begin();
        dv2 = dim1_values.begin();
    }
    
    int n_dim = dims.length();
    int n_interactions = values.length();
    
    double *a = arr.begin();
    
    // move from R indexing (from 1) to C++ indexing (from 0)
    dim1--;
    dim2--;
    for (int i_interaction=0; i_interaction<n_interactions; i_interaction++)
    {
        dv1[i_interaction]--;
        dv2[i_interaction]--;
    }
    
    // Calculate n for each section we'll iterate through
    int n_before_1 = 1;
    for (int i=0; i<dim1; i++)
        n_before_1 *= dims[i];
    int n_dim1 = dims[dim1];
    int n_between_1_2 = 1;
    for (int i=(dim1+1); i<dim2; i++)
        n_between_1_2 *= dims[i];
    int n_dim2 = dims[dim2];
    int n_after_2 = 1;
    for (int i=(dim2+1); i<n_dim; i++)
        n_after_2 *= dims[i];
    
    int n_before_between = n_before_1 * n_dim1;
    int n_before_2 = n_before_between * n_between_1_2;
    int n_before_after = n_before_2 * n_dim2;
    
    // Iterate through the array and add (or set)
    
    if (add)
    {
        for (int i_after = 0; i_after<n_after_2; i_after++)
        {
            for (int i_between = 0; i_between<n_between_1_2; i_between++)
            {
                for (int i_interaction = 0; i_interaction<n_interactions; i_interaction++)
                {
                    for (int i_before=0; i_before<n_before_1; i_before++)
                    {
                        a[i_after*n_before_after + 
                            dv2[i_interaction]*n_before_2 + 
                            i_between*n_before_between +
                            dv1[i_interaction]*n_before_1 +
                            i_before] += values[i_interaction];
                    }
                }
            }
        }
    }
    else
    {
        for (int i_after = 0; i_after<n_after_2; i_after++)
        {
            for (int i_between = 0; i_between<n_between_1_2; i_between++)
            {
                for (int i_interaction = 0; i_interaction<n_interactions; i_interaction++)
                {
                    for (int i_before=0; i_before<n_before_1; i_before++)
                    {
                        a[i_after*n_before_after + 
                            dv2[i_interaction]*n_before_2 + 
                            i_between*n_before_between +
                            dv1[i_interaction]*n_before_1 +
                            i_before] = values[i_interaction];
                    }
                }
            }
        }
    }
}

// [[Rcpp::export]]
void do_add_or_set_three_way_interaction_alphas_to_arr(NumericVector arr,
                                                       IntegerVector dims,
                                                       int dim1,
                                                       IntegerVector dim1_values,
                                                       int dim2,
                                                       IntegerVector dim2_values,
                                                       int dim3,
                                                       IntegerVector dim3_values,
                                                       NumericVector values,
                                                       bool add
)
{
    // order dim1, dim2, dim3 ascending
    int* dv1;
    int* dv2;
    int* dv3;
    if (dim1<dim2 && dim1<dim3)
    {
        dv1 = dim1_values.begin();
        // don't need to reset dim1
        if (dim2<dim3)
        {
            dv2 = dim2_values.begin();
            dv3 = dim3_values.begin();
            // don't need to change dim2 or dim3
        }
        else
        {
            dv2 = dim3_values.begin();
            dv3 = dim2_values.begin();
            
            int temp = dim3;
            dim3 = dim2;
            dim2 = temp;
        }
    }
    else if (dim2<dim3) //dim2 is first
    {
        dv1 = dim2_values.begin();
        if (dim1 < dim3) // just need to swap dim1 and dim2
        {
            dv2 = dim1_values.begin();
            dv3 = dim3_values.begin();
            
            int temp = dim1;
            dim1 = dim2;
            dim2 = temp;
        }
        else // rotate all 3 dims
        {
            dv2 = dim3_values.begin();
            dv3 = dim1_values.begin();
            
            int temp = dim1;
            dim1 = dim2;
            dim2 = dim3;
            dim3 = temp;
        }
    }
    else //dim3 is first
    {
        dv1 = dim3_values.begin();
        if (dim1 < dim2) // rotate all 3 dims
        {
            dv2 = dim1_values.begin();
            dv3 = dim2_values.begin();
            
            int temp = dim1;
            dim1 = dim3;
            dim3 = dim2;
            dim2 = temp;
        }
        else // just need to swap dim1 and dim3
        {
            dv2 = dim2_values.begin();
            dv3 = dim1_values.begin();
            
            int temp = dim3;
            dim3 = dim1;
            dim1 = temp;
        }
    }
    
    
    int n_dim = dims.length();
    int n_interactions = values.length();
    
    double *a = arr.begin();
    
    // move from R indexing (from 1) to C++ indexing (from 0)
    dim1--;
    dim2--;
    dim3--;
    for (int i_interaction=0; i_interaction<n_interactions; i_interaction++)
    {
        dv1[i_interaction]--;
        dv2[i_interaction]--;
        dv3[i_interaction]--;
    }
    
    // Calculate n for each section we'll iterate through
    int n_before_1 = 1;
    for (int i=0; i<dim1; i++)
        n_before_1 *= dims[i];
    int n_dim1 = dims[dim1];
    int n_between_1_2 = 1;
    for (int i=(dim1+1); i<dim2; i++)
        n_between_1_2 *= dims[i];
    int n_dim2 = dims[dim2];
    int n_between_2_3 = 1;
    for (int i=(dim2+1); i<dim3; i++)
        n_between_2_3 *= dims[i];
    int n_dim3 = dims[dim3];
    int n_after_3 = 1;
    for (int i=(dim3+1); i<n_dim; i++)
        n_after_3 *= dims[i];
    
    int n_before_between_1_2 = n_before_1 * n_dim1;
    int n_before_2 = n_before_between_1_2 * n_between_1_2;
    int n_before_between_2_3 = n_before_2 * n_dim2;
    int n_before_3 = n_before_between_2_3 * n_between_2_3;
    int n_before_after = n_before_3 * n_dim3;
    
    // Iterate through the array and add (or set)
    
    if (add)
    {
        for (int i_after = 0; i_after<n_after_3; i_after++)
        {
            for (int i_between_2_3 = 0; i_between_2_3<n_between_2_3; i_between_2_3++)
            {
                for (int i_between_1_2 = 0; i_between_1_2<n_between_1_2; i_between_1_2++)
                {
                    for (int i_interaction = 0; i_interaction<n_interactions; i_interaction++)
                    {
                        for (int i_before=0; i_before<n_before_1; i_before++)
                        {
                            a[i_after*n_before_after + 
                                dv3[i_interaction]*n_before_3 +
                                i_between_2_3*n_before_between_2_3 +
                                dv2[i_interaction]*n_before_2 + 
                                i_between_1_2*n_before_between_1_2 +
                                dv1[i_interaction]*n_before_1 +
                                i_before] += values[i_interaction];
                        }
                    }
                }
            }
        }
    }
    else // set
    {
        for (int i_after = 0; i_after<n_after_3; i_after++)
        {
            for (int i_between_2_3 = 0; i_between_2_3<n_between_2_3; i_between_2_3++)
            {
                for (int i_between_1_2 = 0; i_between_1_2<n_between_1_2; i_between_1_2++)
                {
                    for (int i_interaction = 0; i_interaction<n_interactions; i_interaction++)
                    {
                        for (int i_before=0; i_before<n_before_1; i_before++)
                        {
                            a[i_after*n_before_after + 
                                dv3[i_interaction]*n_before_3 +
                                i_between_2_3*n_before_between_2_3 +
                                dv2[i_interaction]*n_before_2 + 
                                i_between_1_2*n_before_between_1_2 +
                                dv1[i_interaction]*n_before_1 +
                                i_before] = values[i_interaction];
                        }
                    }
                }
            }
        }
    }
}



// [[Rcpp::export]]
void do_add_or_set_four_way_interaction_alphas_to_arr(NumericVector arr,
                                                       IntegerVector dims,
                                                       int dim1,
                                                       IntegerVector dim1_values,
                                                       int dim2,
                                                       IntegerVector dim2_values,
                                                       int dim3,
                                                       IntegerVector dim3_values,
                                                       int dim4,
                                                       IntegerVector dim4_values,
                                                       NumericVector values,
                                                       bool add
)
{
    // order dim1, dim2, dim3 ascending
    int* dv1;
    int* dv2;
    int* dv3;
    int* dv4;
    if (dim1<dim2 && dim1<dim3 && dim1<dim4) //dim1 is first
    {
        dv1 = dim1_values.begin();
        // don't need to reset dim1
        if (dim2<dim3 && dim2<dim4) //dim2 is second
        {
            dv2 = dim2_values.begin();
            
            if (dim3<dim4) //1,2,3,4
            {
                dv3 = dim3_values.begin();
                dv4 = dim4_values.begin();
                
                //don't need to switch
            }
            else //1,2,4,3
            {
                dv3 = dim4_values.begin();
                dv4 = dim3_values.begin();
                
                //switch dim3,4
                int temp = dim4;
                dim4 = dim3;
                dim3 = temp;
            }
        }
        else if (dim3<dim4) //dim3 is second
        {
            dv2 = dim3_values.begin();
            
            if (dim2<dim4) //1,3,2,4
            {
                dv3 = dim2_values.begin();
                dv4 = dim4_values.begin();
                
                // switch dim2 and dim3
                int temp = dim2;
                dim2 = dim4;
                dim4 = temp;
            }
            else //1,3,4,2
            {
                dv3 = dim4_values.begin();
                dv4 = dim2_values.begin();
                
                //switch dim2,3,4
                int temp = dim2;
                dim2 = dim3;
                dim3 = dim4;
                dim4 = temp;
            }
        }
        else //dim4 is second
        {
            dv2 = dim4_values.begin();
            
            if (dim2<dim3) //1,4,2,3
            {
                dv3 = dim2_values.begin();
                dv4 = dim3_values.begin();
                
                //switch dim4,2,3
                int temp = dim2;
                dim2 = dim4;
                dim4 = dim3;
                dim3 = temp;
            }
            else //1,4,3,2
            {
                dv3 = dim3_values.begin();
                dv4 = dim2_values.begin();
                
                //switch dim4,2
                int temp = dim2;
                dim2 = dim4;
                dim4 = temp;
            }
        }
    }
    else if (dim2<dim3 && dim2<dim4) //dim2 is first
    {
        dv1 = dim2_values.begin();
        if (dim1<dim3 && dim1<dim4) // dim1 is second
        {
            dv2 = dim1_values.begin();
            
            if (dim3<dim4) //2,1,3,4
            {
                dv3 = dim3_values.begin();
                dv4 = dim4_values.begin();
                
                //switch dim1,2
                int temp = dim1;
                dim1 = dim2;
                dim2 = temp;
            }
            else //2,1,4,3
            {
                dv3 = dim4_values.begin();
                dv4 = dim3_values.begin();
                
                //switch dim1,2
                int temp = dim1;
                dim1 = dim2;
                dim2 = temp;
                
                //switch dim3,4
                temp = dim3;
                dim3 = dim4;
                dim4 = temp;
            }
        }
        else if (dim3<dim4) // dim3 is second
        {
            dv2 = dim3_values.begin();
            
            if (dim1<dim4) //2,3,1,4
            {
                dv3 = dim1_values.begin();
                dv4 = dim4_values.begin();
                
                //switch dim1,2,3
                int temp = dim1;
                dim1 = dim2;
                dim2 = dim3;
                dim3 = temp;
            }
            else //2,3,4,1
            {
                dv3 = dim4_values.begin();
                dv4 = dim1_values.begin();
                
                //switch all
                int temp = dim1;
                dim1 = dim2;
                dim2 = dim3;
                dim3 = dim4;
                dim4 = temp;
            }
        }
        else // dim4 is second
        {
            dv2 = dim3_values.begin();
            
            if (dim1<dim3) //2,4,1,3
            {
                dv3 = dim1_values.begin();
                dv4 = dim3_values.begin();
                
                //switch all
                int temp = dim1;
                dim1 = dim2;
                dim2 = dim4;
                dim4 = dim3;
                dim3 = temp;
            }
            else //2,4,3,1
            {
                dv3 = dim3_values.begin();
                dv4 = dim1_values.begin();
                
                //switch dim2,4,1
                int temp = dim1;
                dim1 = dim2;
                dim2 = dim4;
                dim4 = temp;
            }
        }
    }
    else if (dim3<dim4) //dim3 is first
    {
        dv1 = dim3_values.begin();
        
        if (dim1<dim2 && dim1<dim4) //dim1 is second
        {
            dv2 = dim1_values.begin();
            
            if (dim2<dim4) //3,1,2,4
            {
                dv3 = dim2_values.begin();
                dv4 = dim4_values.begin();
                
                //switch dim3,1,2
                int temp = dim1;
                dim1 = dim3;
                dim3 = dim2;
                dim2 = temp;
            }
            else //3,1,4,2
            {
                dv3 = dim4_values.begin();
                dv4 = dim2_values.begin();
                
                //switch all
                int temp = dim1;
                dim1 = dim3;
                dim3 = dim4;
                dim4 = dim2;
                dim2 = temp;
            }
        }
        else if (dim2<dim4) //dim2 is second
        {
            dv2 = dim2_values.begin();
            
            if (dim1<dim4) //3,2,1,4
            {
                dv3 = dim1_values.begin();
                dv4 = dim4_values.begin();
                
                //switch dim3,1
                int temp = dim1;
                dim1 = dim3;
                dim3 = temp;
            }
            else //3,2,4,1
            {
                dv3 = dim4_values.begin();
                dv4 = dim1_values.begin();
                
                //switch dim3,4,1
                int temp = dim1;
                dim1 = dim3;
                dim3 = dim4;
                dim4 = temp;
            }
        }
        else //dim4 is second
        {
            dv2 = dim4_values.begin();
            
            if (dim1<dim2) //3,4,1,2
            {
                dv3 = dim1_values.begin();
                dv4 = dim2_values.begin();
                
                //switch dim1,3
                int temp = dim1;
                dim1 = dim3;
                dim3 = temp;
                
                //switch dim2,4
                temp = dim2;
                dim2 = dim4;
                dim4 = temp;
            }
            else //3,4,2,1
            {
                dv3 = dim2_values.begin();
                dv4 = dim1_values.begin();
                
                //switch all
                int temp = dim1;
                dim1 = dim3;
                dim3 = dim2;
                dim2 = dim4;
                dim4 = temp;
            }
        }
    }
    else//dim4 is first
    {
        dv1 = dim4_values.begin();
        
        if (dim1<dim2&& dim1<dim3) //dim1 is second
        {
            dv2 = dim1_values.begin();
            
            if (dim2<dim3) //4,1,2,3
            {
                dv3 = dim2_values.begin();
                dv4 = dim3_values.begin();
                
                //switch all
                int temp = dim1;
                dim1 = dim4;
                dim4 = dim3;
                dim3 = dim2;
                dim2 = temp;
            }
            else //4,1,3,2
            {
                dv3 = dim3_values.begin();
                dv4 = dim2_values.begin();
                
                //switch dims4,1,2
                int temp = dim1;
                dim1 = dim4;
                dim4 = dim2;
                dim2 = temp;
            }
        }
        else if (dim2<dim3) //dim2 is second
        {
            dv2 = dim2_values.begin();
            
            if (dim1<dim3) //4,2,1,3
            {
                dv3 = dim1_values.begin();
                dv4 = dim3_values.begin();
                
                //switch dim4,1,3
                int temp = dim1;
                dim1 = dim4;
                dim4 = dim3;
                dim3= temp;
            }
            else //4,2,3,1
            {
                dv3 = dim3_values.begin();
                dv4 = dim1_values.begin();
                
                //switch dim4,1
                int temp = dim1;
                dim1 = dim4;
                dim4 = temp;
            }
        }
        else //dim3 is second
        {
            dv2 = dim3_values.begin();
            
            if (dim1<dim2) //4,3,1,2
            {
                dv3 = dim1_values.begin();
                dv4 = dim2_values.begin();
                
                //switch all
                int temp = dim1;
                dim1 = dim4;
                dim4 = dim2;
                dim2 = dim3;
                dim3 = temp;
            }
            else //4,3,2,1
            {
                dv3 = dim2_values.begin();
                dv4 = dim1_values.begin();
                
                //switch dim4,1
                int temp = dim1;
                dim1 = dim4;
                dim4 = temp;
                
                //switch dim3,2
                temp = dim2;
                dim2 = dim3;
                dim3 = temp;
            }
        }
    }
    
    
    int n_dim = dims.length();
    int n_interactions = values.length();
    
    double *a = arr.begin();
    
    // move from R indexing (from 1) to C++ indexing (from 0)
    dim1--;
    dim2--;
    dim3--;
    dim4--;
    for (int i_interaction=0; i_interaction<n_interactions; i_interaction++)
    {
        dv1[i_interaction]--;
        dv2[i_interaction]--;
        dv3[i_interaction]--;
        dv4[i_interaction]--;
    }
    
    // Calculate n for each section we'll iterate through
    int n_before_1 = 1;
    for (int i=0; i<dim1; i++)
        n_before_1 *= dims[i];
    int n_dim1 = dims[dim1];
    int n_between_1_2 = 1;
    for (int i=(dim1+1); i<dim2; i++)
        n_between_1_2 *= dims[i];
    int n_dim2 = dims[dim2];
    int n_between_2_3 = 1;
    for (int i=(dim2+1); i<dim3; i++)
        n_between_2_3 *= dims[i];
    int n_dim3 = dims[dim3];
    int n_between_3_4 = 1;
    for (int i=(dim3+1); i<dim4; i++)
        n_between_3_4*= dims[i];
    int n_dim4 = dims[dim4];
    int n_after_4 = 1;
    for (int i=(dim4+1); i<n_dim; i++)
        n_after_4 *= dims[i];
    
    int n_before_between_1_2 = n_before_1 * n_dim1;
    int n_before_2 = n_before_between_1_2 * n_between_1_2;
    int n_before_between_2_3 = n_before_2 * n_dim2;
    int n_before_3 = n_before_between_2_3 * n_between_2_3;
    int n_before_between_3_4 = n_before_3 * n_dim3;
    int n_before_4 = n_before_between_3_4 * n_between_3_4;
    int n_before_after = n_before_4 * n_dim4;
    
    // Iterate through the array and add (or set)
    
    if (add)
    {
        for (int i_after = 0; i_after<n_after_4; i_after++)
        {
            for (int i_between_3_4 = 0; i_between_3_4<n_between_3_4; i_between_3_4++)
            {
                for (int i_between_2_3 = 0; i_between_2_3<n_between_2_3; i_between_2_3++)
                {
                    for (int i_between_1_2 = 0; i_between_1_2<n_between_1_2; i_between_1_2++)
                    {
                        for (int i_interaction = 0; i_interaction<n_interactions; i_interaction++)
                        {
                            for (int i_before=0; i_before<n_before_1; i_before++)
                            {
                                a[i_after*n_before_after + 
                                    dv4[i_interaction]*n_before_4 +
                                    i_between_3_4*n_before_between_3_4 +
                                    dv3[i_interaction]*n_before_3 +
                                    i_between_2_3*n_before_between_2_3 +
                                    dv2[i_interaction]*n_before_2 + 
                                    i_between_1_2*n_before_between_1_2 +
                                    dv1[i_interaction]*n_before_1 +
                                    i_before] += values[i_interaction];
                            }
                        }
                    }
                }
            }
        }
    }
    else // set
    {
        for (int i_after = 0; i_after<n_after_4; i_after++)
        {
            for (int i_between_3_4 = 0; i_between_3_4<n_between_3_4; i_between_3_4++)
            {
                for (int i_between_2_3 = 0; i_between_2_3<n_between_2_3; i_between_2_3++)
                {
                    for (int i_between_1_2 = 0; i_between_1_2<n_between_1_2; i_between_1_2++)
                    {
                        for (int i_interaction = 0; i_interaction<n_interactions; i_interaction++)
                        {
                            for (int i_before=0; i_before<n_before_1; i_before++)
                            {
                                a[i_after*n_before_after + 
                                    dv4[i_interaction]*n_before_4 +
                                    i_between_3_4*n_before_between_3_4 +
                                    dv3[i_interaction]*n_before_3 +
                                    i_between_2_3*n_before_between_2_3 +
                                    dv2[i_interaction]*n_before_2 + 
                                    i_between_1_2*n_before_between_1_2 +
                                    dv1[i_interaction]*n_before_1 +
                                    i_before] = values[i_interaction];
                            }
                        }
                    }
                }
            }
        }
    }
}

