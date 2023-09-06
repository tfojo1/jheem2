#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
List apply_foregrounds(List values,
                       NumericVector value_times
                       List after_values,
                       List foregrounds,
                       List indices_per_effect_per_foreground)
{
    int n_frgd = foregrounds.length();
    int n_times = value_times.length();
    
    for (int f=0; f<n_frgd; f++)
    {
        List frgd = (List) foregrounds[i];
        
        List effects = (List) frgd['effects'];
        int n_effects = effects.length();
        
        List indices_per_effect = (List) indices_per_effect_per_foregrounds[f];
        
        for (int e=0; e<n_effects; e++)
        {
            List one_effect = (List) effects[e];
            int start_time = (int) one_effect["start.time"];
            int end_time = (int) one_effect["end.time"];
            IntegerVector effect_times = (IntegerVector) one_effect["times"];
            NumericVector effect_values = (NumericVector) one_effect["effect.values"];
            
            int n_effect_times = effect_times.length();
            
            bool effect_is_multiplier = one_effect["apply.effect.as.multiplier"];
            bool effect_is_addend = one_effect["apply.effect.as.addend"];
            bool effect_is_overwrite = !effect_is_multiplier && !effect_is_addend;
            
            IntegerVector indices = (IntegerVector) indices_per_effect[i_effect];
            int n_indices = indices.length();
            
            // i indexes the time/values
            // j indexes within the effect
            
            // set up the index into values
            // advance it such that it is between start_time and effect_times[0]
            int i=0;
            while (value_times[i] < start_time)
                i++;
            
            int j = -1; //index into effect_times
            double val_before = 0;
            if (effect_is_multiplier)
                val_before = 1;
         //   else if (effect_is_addend)
        //        val_before = 0;
            double val_after = effect_values[0];
            double val, weight;
            double val_to_write_after;
            
            double time_before = start_time;
            double time_after = effect_times[0];
            
            double arr;
            bool write_value, write_after_value;
            bool interpolate_abs_start, interpolate_abs_end;
            
            while (j <= n_effect_times)
            {
                // at the start of this loop, we know that
                //   value_times[i] >= time_before
                while (value_times[i] <= time_after)
                {
                    // Figure out what we need to overwrite
                    
                    if (time_before == time_after)
                    // we need to write values[i] using with val_before
                    // and after_values[i] using val_after
                    // we don't need to interpolate here - we know we're right at
                    //  value_times[i] == time_before == time_after
                    {
                        if (j==-1) //we're at the start time and don't need to overwrite values
                            write_value = false;
                        else
                        {
                            write_value = true;
                            val = val_before;
                        }
                        
                        if (j==n_effects) // we're at the end time and don't need to overwrite after_values
                            write_after_value = false;
                        else
                        {
                            write_after_value = true;
                            val_to_write_after = val_after;
                        }
                        
                        interpolate_abs_start = false;
                        interpolate_abs_end = false;
                    }
                    else
                    {
                        write_value = j!=-1;
                        write_after_value = j!=n_effect_times && after_values[i]!=R_NilValue;
                        
                        if (time_before == R_NegInf)
                        {
                            weight_before = 1;
                            weight_after = 0;
                            val = val_to_write_after = value_before;
                        }
                        else if (time_after == R_PosInf)
                        {
                            weight_before = 0;
                            weight_after = 1;
                            val = val_to_write_after = val_after;
                        }
                        else
                        {
                            weight_before = (time_after - value_times[i]) / (time_after - time_before);
                            weight_after = 1 - weight_before;
                            val = val_to_write_after = weight_before * val_before + weight_after * val_after;
                        }
                        
                        interpolate_abs_start = j==-1;
                        interpolate_abs_end = j==n_effect_times;
                    }
                
                    // do the overwrite
                    if (write_value)
                    {
                        arr = (NumericVector) values[i];
                        
                        if (effect_is_multiplier)
                        {
                            for (int k=0; k<n_indices; k++)
                                arr[ indices[k] ] *= val;
                        }
                        else if (effect_is_addend)
                        {
                            for (int k=0; k<n_indices; k++)
                                arr[ indices[k] ] += val;
                        }
                        else if (interpolate_abs_start) // need to interpolate between the start value and the 1st foreground value
                        {
                            if (weight_after==1)
                            {
                                for (int k=0; k<n_indices; k++)
                                    arr[ indices[k] ] = val_after;
                            }
                            else if (weight_after > 0)
                            {
                                for (int k=0; k<n_indices; k++)
                                    arr[ indices[k] ] = arr[ indices[k] ] * weight_before + val_after * weight_after;
                            }
                            // if weight_before == 1, don't need to to anything
                                
                        }
                        else if (interpolate_abs_end) // need to interpolate between the end value and the 1st foreground value
                        {
                            if (weight_before==1)
                            {
                                for (int k=0; k<n_indices; k++)
                                    arr[ indices[k] ] = val_before;
                            }
                            else if (weight_before > 0)
                            {
                                for (int k=0; k<n_indices; k++)
                                    arr[ indices[k] ] = arr[ indices[k] ] * weight_after + val_before * weight_before;
                            }
                            // if weight_after == 1, don't need to to anything
                        }
                        else
                        {
                            for (int k=0; k<n_indices; k++)
                                arr[ indices[k] ] = val;
                        }
                        
                        values[i] = arr;
                    }
                    
                    if (write_after_value)
                    {
                        arr = (NumericVector) after_values[i];
                        
                        if (effect_is_multiplier)
                        {
                            for (int k=0; k<n_indices; k++)
                                arr[ indices[k] ] *= val_to_write_after;
                        }
                        else if (effect_is_addend)
                        {
                            for (int k=0; k<n_indices; k++)
                                arr[ indices[k] ] += val_to_write_after;
                        }
                        else if (interpolate_abs_start) // need to interpolate between the start value and the 1st foreground value
                        {
                            if (weight_after==1)
                            {
                                for (int k=0; k<n_indices; k++)
                                    arr[ indices[k] ] = val_after;
                            }
                            else if (weight_after > 0)
                            {
                                for (int k=0; k<n_indices; k++)
                                    arr[ indices[k] ] = arr[ indices[k] ] * weight_before + val_after * weight_after;
                            }
                            // if weight_before == 1, don't need to to anything
                            
                        }
                        else if (interpolate_abs_end) // need to interpolate between the end value and the 1st foreground value
                        {
                            if (weight_before==1)
                            {
                                for (int k=0; k<n_indices; k++)
                                    arr[ indices[k] ] = val_before;
                            }
                            else if (weight_before > 0)
                            {
                                for (int k=0; k<n_indices; k++)
                                    arr[ indices[k] ] = arr[ indices[k] ] * weight_after + val_before * weight_before;
                            }
                            // if weight_after == 1, don't need to to anything
                        }
                        else
                        {
                            for (int k=0; k<n_indices; k++)
                                arr[ indices[k] ] = val_to_write_after;
                        }
                        
                        after_values[i] = arr;
                    }
                    
                    //increment to the next value point
                    i++;
                }
                
                // update for the next iteration of the loop
                j++;
                val_before = val_after;
                time_before = time_after;
                if (j==n_effect_times)
                {
                    if (effect_is_multiplier)
                        val_after = 1;
                    else if (effect_is_addend)
                        val_after = 0;
                    
                    time_after = end_time;
                }
                else
                {
                    val_after = effect_values[j];
                    time_after = effect_times[j];
                }
            }
            
            
        }
    }
    
    // make the list and return
}




// [[Rcpp::export]]
List apply_foreground_values(List values,
                             NumericVector value_times,
                             List effects,
                             List indices_per_effect)
{
    int n_effects = effects.length();
    int n_times = value_times.length();
    
    for (int i_effect=0; i_effect<n_effects; i_effect++)
    {
        // Pull the structures for the intervention effect
        List one_effect = (List) effects[i_effect];
        int start_time = (int) one_effect["start.time"];
        int end_time = (int) one_effect["end.time"];
        IntegerVector effect_times = (IntegerVector) one_effect["times"];
        
        int n_effect_times = effect_times.length();
        
        bool effect_is_multiplier = one_effect["apply.effect.as.multiplier"];
        bool effect_is_addend = one_effect["apply.effect.as.addend"];
        bool effect_is_overwrite = !effect_is_multiplier && !effect_is_addend;
            
        IntegerVector indices = (IntegerVector) indices_per_effect[i_effect];
        
        
        // Figure out what times we have to apply to
        int first_index_to_modify = 0;
        int last_index_to_modify = n_times-1;
        
        // NB: We can ignore the start and end times because we assume (require) that they have already been linearly interpolated
        while (first_index_to_modify<n_times && value_times[first_index_to_modify]<=start_time)
            first_index_to_modify++;
        
        while (last_index_to_modify>=first_index_to_modify && value_times[last_index_to_modify]>=end_time)
            last_index_to_modify--;
        
        int i = first_index_to_modify;
        int within_effect_index = 0;
        double effect_to_apply;
        
        // iterate through all the times between the start time and the first effect time
        while (i < last_index_to_modify && value_times[i] < effect_times[0])
        {
            // do some stuff
            i++;
        }
        
        // iterate through times between first time and last time for the effect
        while (i < last_index_to_modify && value_times[i] <= effect_times[n_effect_times-1])
        {
            // do some stuff
            while (effect_times[within_effect_index]<value_times[i]) //by meeting the above while criterion, there is always a within_effect_index with an effect_time >= value_times[i]
                within_effect_index++;
            
            if (value_times[i] == effect_times[within_effect_index]) // apply the value directly
                effect_to_apply = effect_values[within_effect_index];
            else // interpolate
                
            
            i++;
        }
        
        // iterate through times between last effect time and end time
        while (i < last_index_to_modify)
        {
            // do some stuff
            i++;
        }
        
    }
    
    return (values);
}

double interpolate_value_with_endpoints(NumericVector values,
                                        NumericVector times,
                                        double start_value,
                                        double start_time,
                                        double end_value,
                                        double end_time,
                                        double desired_time)
{
    int n_times = times.length()
    if (n_times==1)
        values[0];
    else
    {
        int i_before=0;
    }
}
