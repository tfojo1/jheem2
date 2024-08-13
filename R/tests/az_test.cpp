#include <Rcpp.h>
using namespace Rcpp;

//---------------------//
    //-- ARRAY OVERWRITE --//
    //---------------------//
    
    // Returns NULL (R_NilValue) if error
// Otherwise, returns a pointer to the modified NumericVector
//   Will modify dst_array in place if possible
// [[Rcpp::export]]
RObject do_array_overwrite(NumericVector dst_array,
                           NumericVector src_array,
                           List dimension_values)
{
    //-- PART 1: SET UP ATTRIBUTES OF ARRAYS --//
        
    //-- Part 1a: Pull dst dimensions and names --//
    if (dst_array.attr("dimnames")==R_NilValue)
        return (R_NilValue);
    List dst_dim_names = dst_array.attr("dimnames");
    
    if (dst_dim_names.attr("names")==R_NilValue)
        return (R_NilValue);
    CharacterVector dst_dimensions = dst_dim_names.names();
    
    if (dst_array.attr("dim")==R_NilValue)
        return (R_NilValue);
    IntegerVector dst_dims = dst_array.attr("dim");
    
    int n_dst_dims = dst_dims.length();
    
    //-- Part 1b: Map the write dimensions (from dimension_values to dst) --//
    if (dimension_values.attr("names")==R_NilValue)
        return (R_NilValue);
    CharacterVector access_dimensions = dimension_values.names();
    
    int max_dim_length = 0;
    for (int j=0; j<n_dst_dims; j++)
    {
        if (dst_dims[j]>max_dim_length)
            max_dim_length = dst_dims[j];
    }
    
    // for starters, we code a one-to-one mapping for write to dst
    int write_dims[n_dst_dims];
    int write_to_dst_dim_values[n_dst_dims][max_dim_length];
    for (int j=0; j<n_dst_dims; j++)
    {
        write_dims[j] = dst_dims[j];
        for (int m=0; m<dst_dims[j]; m++)
            write_to_dst_dim_values[j][m] = m;
    }
    Rcout << "max_dim_length: " << max_dim_length << "\n";
    // now, for each dimension in dimension values, we overwrite the default
    for (int k=0; k<dimension_values.length(); k++)
    {
        // identify the dimension index into dst that it maps to
        int dst_dim_index = -1;
        for (int j=0; j<n_dst_dims; j++)
        {
            if (dst_dimensions[j] == access_dimensions[k])
            {
                dst_dim_index = j;
                break;
            }
        }
        if (dst_dim_index==-1)
            return (R_NilValue);
        
        
        // map the access values to dst values
        RObject elem = dimension_values[k];
        
        if (is<CharacterVector>(elem))
        {
            CharacterVector dst_values = (CharacterVector) dst_dim_names[dst_dim_index];
            CharacterVector values = (CharacterVector) elem;
            write_dims[dst_dim_index] = values.length();
            
            for (int write_index=0; write_index<write_dims[dst_dim_index]; write_index++)
            {
                write_to_dst_dim_values[dst_dim_index][write_index] = -1;
                for (int dst_index=0; dst_index<dst_dims[dst_dim_index]; dst_index++)
                {
                    if (dst_values[dst_index]==values[write_index])
                    {
                        write_to_dst_dim_values[dst_dim_index][write_index] = dst_index;
                        break;
                    }
                }
                
                if (write_to_dst_dim_values[dst_dim_index][write_index] == -1)
                    return (R_NilValue);
            }
        }
        
        else if (is<IntegerVector>(elem))
        {
            IntegerVector values = (IntegerVector) elem;
            write_dims[dst_dim_index] = values.length();
            
            for (int write_index=0; write_index<write_dims[dst_dim_index]; write_index++)
            {
                write_to_dst_dim_values[dst_dim_index][write_index] = values[write_index]-1;
                if (values[write_index]<0 || values[write_index]>=dst_dims[dst_dim_index])
                    return (R_NilValue);
            }
        }
        else if (is<NumericVector>(elem))
        {
            NumericVector values = (NumericVector) elem;
            write_dims[dst_dim_index] = values.length();
            
            for (int write_index=0; write_index<write_dims[dst_dim_index]; write_index++)
            {
                write_to_dst_dim_values[dst_dim_index][write_index] = ((int) values[write_index])-1;
                if (values[write_index]<0 || values[write_index]>=dst_dims[dst_dim_index])
                    return (R_NilValue);
                if ( values[write_index] != (((double) ((int) values[write_index]))) )
                    return (R_NilValue);
            }
        }
        else if (is<LogicalVector>(elem))
        {
            LogicalVector values = (LogicalVector) elem;
            write_dims[dst_dim_index] = values.length();
            
            if (values.length() != dst_dims[dst_dim_index])
                return (R_NilValue);
            
            for (int dst_index=0; dst_index<dst_dims[dst_dim_index]; dst_index++)
            {
                if (values[dst_index])
                {
                    write_to_dst_dim_values[dst_dim_index][ write_dims[dst_dim_index] ] = dst_index;
                    write_dims[dst_dim_index]++;
                }
            }
        }
        else
            return (R_NilValue);
        
        if (write_dims[dst_dim_index] == 0)
            return (R_NilValue);
        
    }
    
    //-- PART 3: MAP SRC DIMENSIONS AND VALUES to WRITE VALUES --//
        //-- Part 1b: pull n_src_dims --//
        // The trick here is that we need to handle the case where src_array has only one dimension
    //  and has names set but not dimnames
    
    
    //int n_src_dims;
    
    int n_src_dims = 0;
    List src_dim_names;
    
    
    // First identify the number of dimensions
    if (src_array.attr("dimnames")==R_NilValue)
    {
        // This might still be OK if either
        // The names of src_array are set AND they are a subset of the names of a dimension in dst_array
        // OR it is a length 1 vector
        Rcout << "Ran line 165" << "\n";
        if (src_array.attr("names")==R_NilValue)
        {
            //The only option is if we are length 1
            if (src_array.length()==1){
                n_src_dims = 0;
                Rcout << "Ran line 170" << "\n";
            }
                
            else
                return (R_NilValue);
        }
        else
        {
            n_src_dims = 1;
        }

    }
    else
    {
        src_dim_names = src_array.attr("dimnames");
        //n_src_dims = src_dim_names.length();
        Rcout << "Ran line 187!" << "\n";
    }
    Rcout << "n_src_dims at Line 188: " << n_src_dims << "\n";
    
    //-- Part 1c: Figure out the src dimensions --//
    
    int *src_dims;
    
    int src_dims_if_dimnames_null[1];

    if (n_src_dims>0)
    {
        if (src_array.attr("dim")==R_NilValue)
        {
            src_dims_if_dimnames_null[0] = src_array.length();
            src_dims = src_dims_if_dimnames_null;
        }
        else
        {
            src_dims = ((IntegerVector) src_array.attr("dim")).begin();
        }
    }
    
    //-- PART 3: SET UP FOR TABULATION OF SRC INDICES --//
    
    //-- Part 3a: Set up mappings between src and dist --//#      
    int src_to_write_dims[n_src_dims];

    // Set up the holder for the dim value mapping
    max_dim_length = 0;
    for (int i=0; i<n_src_dims; i++)
    {
        if (src_dims[i]>max_dim_length)
            max_dim_length = src_dims[i];
    }
    
    
    // indexed [src_dimension][write_dimension_value]
    int x = n_src_dims>0 ? n_src_dims : 1;
    int write_to_src_dim_values[x][max_dim_length];
    //int write_to_src_dim_values[n_src_dims][max_dim_length]; //@ warning: 0 length array
    
    // Actually map the dimensions and values
    if (n_src_dims>0)
    {
        if (src_array.attr("dim")==R_NilValue)
        {
            CharacterVector src_values = src_array.names();
            CharacterVector dst_values;
            
            // The basic idea here:
                // If there is a dst dimension such that all the write dimension values of that dst dimension
            //  are contained within our src names, then that is the mapping
            src_to_write_dims[0] = -1;
            for (int j=0; j<n_dst_dims; j++)
            {
                dst_values = (CharacterVector) dst_dim_names[j];
                
                int write_val_index;
                for (write_val_index=0; write_val_index<write_dims[j]; write_val_index++)
                {
                    write_to_src_dim_values[0][write_val_index] = -1;
                    for (int src_val_index=0; src_val_index<src_dims[0]; src_val_index++)
                    {
                        if (src_values[src_val_index]==dst_values[write_to_dst_dim_values[j][write_val_index]])
                        {
                            write_to_src_dim_values[0][write_val_index] = src_val_index;
                            break; //we found the match - stop searching
                        }
                    }
                    
                    if (write_to_src_dim_values[0][write_val_index] == -1) 
                        break; // we did not find a match for the value, give up on this dst dimension
                }
                
                if (write_val_index==write_dims[j]) 
                {
                    //we found matches for all values in the j dimension
                    // if we had already found a match, return false (an error - more than one dst dimension matches)
                    // otherwise, set it
                    if (src_to_write_dims[0]==-1)
                        src_to_write_dims[0] = j;
                    else
                        return (R_NilValue);
                }
            }
            
            if (src_to_write_dims[0]==-1)
                return (R_NilValue);
        }
        else
        {
            // Pull dimension names
            if (src_dim_names.attr("names")==R_NilValue)
                return (R_NilValue);
            CharacterVector src_dimensions = src_dim_names.names();
            
            // Map src to dst/write dimensions
            for (int i=0; i<n_src_dims; i++)
            {
                src_to_write_dims[i] = -1;
                for (int j=0; j<n_dst_dims; j++)
                {
                    if (src_dimensions[i]==dst_dimensions[j])
                    {
                        src_to_write_dims[i] = j;
                        break;
                    }
                }
                
                if (src_to_write_dims[i]==-1)
                    return (R_NilValue);
            }
            
            // Map write to src dimension values
            CharacterVector src_values;
            CharacterVector dst_values;
            for (int i=0; i<n_src_dims; i++)
            {
                int write_d = src_to_write_dims[i];
                src_values = (CharacterVector) src_dim_names[i];
                dst_values = (CharacterVector) dst_dim_names[write_d];
                
                for (int j=0; j<write_dims[write_d]; j++)
                {
                    write_to_src_dim_values[i][j] = -1;
                    for (int k=0; k<src_values.length(); k++)
                    {
                        if (dst_values[ write_to_dst_dim_values[write_d][j] ] == src_values[k])
                        {
                            write_to_src_dim_values[i][j] = k;
                            break;
                        }
                    }
                    
                    if (write_to_src_dim_values[i][j] == -1)
                        return (R_NilValue);
                }
            }
            
        }
    }
    
    
    //-- Part 3b: Tabulate source and destination dims --//
    int n_write = 1;
    
    for (int j=0; j<n_dst_dims; j++)
        n_write = n_write * write_dims[j];
    
    //@ my replacement
    int n_before_src[x];
    if (n_src_dims > 0) {
        n_before_src[0] = 1;
        for (int i=1; i<n_src_dims; i++)
            n_before_src[i] = n_before_src[i-1] * src_dims[i-1];
    }
    
    //@ these 2 lines break it, related to n_src_dims being zero, so n_before_src is an array of length zero
    /*
    int n_before_src[n_src_dims];
    n_before_src[0] = 1;
    */
    
    int n_before_dst[n_dst_dims];
    n_before_dst[0] = 1;
    
    for (int j=1; j<n_dst_dims; j++)
        n_before_dst[j] = n_before_dst[j-1] * dst_dims[j-1];
    
    
    //-- PART 4: LOOP THROUGH WRITE INDICES --//
    int write_dim_values[n_dst_dims];
    for (int j=0; j<n_dst_dims; j++)
        write_dim_values[j] = 0;
    int src_index, dst_index;
    
    for (int k=0; k<n_write; k++)
    {
        
        // Calculate what the index is into the src array
        src_index = 0;
        for (int i=0; i<n_src_dims; i++)
        {
            src_index += n_before_src[i] * 
                write_to_src_dim_values[i][ write_dim_values[src_to_write_dims[i]] ];
        }
        
        // Calculate what the index is into the dst array
        dst_index = 0;
        
        for (int j=0; j<n_dst_dims; j++)
            dst_index += n_before_dst[j] * 
            write_to_dst_dim_values[j][ write_dim_values[j] ];
        
        // Pull from src to dst
        dst_array[dst_index] = src_array[src_index];
        
        // Update the dst_dim_values
        for (int j=0; j<n_dst_dims; j++)
        {
            if (write_dim_values[j]==(write_dims[j]-1)) //it's at the max - set to zero and increment the value for the next dimension
                write_dim_values[j] = 0;
            else
            {
                write_dim_values[j]++;
                break;
            }
        }
        
    }
    
    return (dst_array);
}