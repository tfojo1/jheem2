#include <Rcpp.h>
using namespace Rcpp;



//-------------//
//-- HELPERS --//
//-------------//


void do_convert_scale(double *values,
                      CharacterVector from_scale,
                      CharacterVector to_scale,
                      int *indices,
                      int n)
{
    if (from_scale[0]==to_scale[0] ||
        (from_scale[0]=="proportion" && to_scale[0]=="proportion.leaving") ||
        (from_scale[0]=="proportion.leaving" && to_scale[0]=="proportion") ||
        (from_scale[0]=="odds" && to_scale[0]=="odds.leaving") ||
        (from_scale[0]=="odds.leaving" && to_scale[0]=="odds") ||
        (from_scale[0]=="non.negative.number" && to_scale[0]=="number"))
    {} // do nothing
    else if ((from_scale[0]=="number" || from_scale[0]=="non.negative.number") &&
             (to_scale[0]=="number" || to_scale[0]=="non.negative.number"))
    {} // do nothing
    else if (from_scale[0]=="rate")
    {
        if (to_scale[0]=="proportion" || to_scale[0]=="proportion.leaving")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = 1 - std::exp(values[ indices[i] ]);
        }
        else if (to_scale[0]=="proportion.staying")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = std::exp(-values[ indices[i] ]);
        }
        else if (to_scale[0]=="time")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = 1 / values[ indices[i] ];
        }
        else if (to_scale[0]=="odds" || to_scale[0]=="odds.leaving")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = std::exp(values[ indices[i] ]) - 1;
        }
        else if (to_scale[0]=="odds.staying")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = 1 / (std::exp(values[ indices[i] ]) - 1);
        }
    }
    else if (from_scale[0]=="proportion" || from_scale[0]=="proportion.leaving")
    {
        if (to_scale[0]=="rate")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = -std::log(1-values[ indices[i] ]);
        }
        else if (to_scale[0]=="proportion.staying")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = 1 - values[ indices[i] ];
        }
        else if (to_scale[0]=="time")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = -1 / std::log(1 - values[ indices[i] ]);
        }
        else if (to_scale[0]=="odds" || to_scale[0]=="odds.leaving")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = values[ indices[i] ] / (1 - values[ indices[i] ]);
        }
        else if (to_scale[0]=="odds.staying")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = (1 - values[ indices[i] ]) / values[ indices[i] ];
        }
    }
    else if (from_scale[0]=="proportion.staying")
    {
        if (to_scale[0]=="rate")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = -std::log(values[ indices[i] ]);
        }
        else if (to_scale[0]=="proportion" || to_scale[0]=="proportion.leaving")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = 1 - values[ indices[i] ];
        }
        else if (to_scale[0]=="time")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = -1 / std::log(values[ indices[i] ]);
        }
        else if (to_scale[0]=="odds" || to_scale[0]=="odds.leaving")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = (1 - values[ indices[i] ]) / values[ indices[i] ];
        }
        else if (to_scale[0]=="odds.staying")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = values[ indices[i] ] / (1 - values[ indices[i] ]);
        }
    }
    else if (from_scale[0]=="time")
    {
        if (to_scale[0]=="rate")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = 1 / values[ indices[i] ];
        }
        else if (to_scale[0]=="proportion" || to_scale[0]=="proportion.leaving")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = 1 - std::exp(-1/values[ indices[i] ]);
        }
        else if (to_scale[0]=="proportion.staying")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = std::exp(-1/values[ indices[i] ]);
        }
        else if (to_scale[0]=="odds" || to_scale[0]=="odds.leaving")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = std::exp(1/values[ indices[i] ]) - 1;
        }
        else if (to_scale[0]=="odds.staying")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = 1 / (std::exp(1/values[ indices[i] ]) - 1);
        }
    }
    else if (from_scale[0]=="odds" || from_scale[0]=="odds.leaving")
    {
        if (to_scale[0]=="rate")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = std::log(values[ indices[i] ] + 1);
        }
        else if (to_scale[0]=="proportion" || to_scale[0]=="proportion.leaving")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = values[ indices[i] ] / (1 + values[ indices[i] ]);
        }
        else if (to_scale[0]=="proportion.staying")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = 1 - values[ indices[i] ] / (1 + values[ indices[i] ]);
        }else if (to_scale[0]=="time")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = 1 / (std::log(values[ indices[i] ] + 1));
        }
        else if (to_scale[0]=="odds.staying")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = 1 / values[ indices[i] ];
        }
    }
    else if (from_scale[0]=="odds.staying")
    {
        if (to_scale[0]=="rate")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = std::log(1/values[ indices[i] ] + 1);
        }
        else if (to_scale[0]=="proportion" || to_scale[0]=="proportion.leaving")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = 1 - values[ indices[i] ] / (1 + values[ indices[i] ]);
        }
        else if (to_scale[0]=="proportion.staying")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = values[ indices[i] ] / (1 + values[ indices[i] ]);
        }
        else if (to_scale[0]=="time")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = 1 / std::log(1/values[ indices[i] ] + 1);
        }
        else if (to_scale[0]=="odds" || to_scale[0]=="odds.leaving")
        {
            for (int i=0; i<n; i++)
                values[ indices[i] ] = 1 / values[ indices[i] ];
        }
    }
    else
    {} //do nothing. This is an error, but we're assuming that this has already been checked and we are not going to make this error
    
    return;
}

// a function that actually overwrites foreground values into an array
NumericVector do_foreground_overwrite(NumericVector arr,
                                      double *scratch_arr,
                                      bool effect_is_multiplier,
                                      bool effect_is_addend,
                                      bool interpolate_abs_start,
                                      bool interpolate_abs_end,
                                      bool allow_less,
                                      bool allow_greater,
                                      double val,
                                      double val_before,
                                      double val_after,
                                      double weight_before,
                                      double weight_after,
                                      CharacterVector scale,
                                      CharacterVector effect_scale,
                                      IntegerVector indices,
                                      int n_indices)
{
    if (effect_is_multiplier)
    {
        if ((allow_less || val > 1) &&
            (allow_greater || val < 1))
        {
            do_convert_scale(arr.begin(),
                             scale, effect_scale,
                             indices.begin(), n_indices);
            
            for (int k=0; k<n_indices; k++)
                arr[ indices[k] ] *= val;
            
            do_convert_scale(arr.begin(),
                             effect_scale, scale,
                             indices.begin(), n_indices);
        }
    }
    else if (effect_is_addend)
    {
        if ((allow_less || val > 0) &&
            (allow_greater || val < 0))
        {
            do_convert_scale(arr.begin(),
                             scale, effect_scale,
                             indices.begin(), n_indices);
            
            for (int k=0; k<n_indices; k++)
                arr[ indices[k] ] += val;
            
            do_convert_scale(arr.begin(),
                             effect_scale, scale,
                             indices.begin(), n_indices);
        }
    }
    else if (interpolate_abs_start) // need to interpolate between the start value and the 1st foreground value
    {
        // No need to convert scale beforehand - we have already converted the scratch_arr
        // NB: all scales conversions are monotonic, so >/< in the original scale are the same in the converted scale
        
        if (weight_after==1)
        {
            if (!allow_less)
            {
                for (int k=0; k<n_indices; k++)
                {
                    if (val_after > arr[ indices[k] ])
                        arr[ indices[k] ] = val_after;
                }
            }
            else if (!allow_greater)
            {
                for (int k=0; k<n_indices; k++)
                {
                    if (val_after < arr[ indices[k ]])
                        arr[ indices[k] ] = val_after;
                }
            }
            else
            {
                for (int k=0; k<n_indices; k++)
                    arr[ indices[k] ] = val_after;
            }
        }
        else if (weight_after > 0)
        {
            if (!allow_less)
            {
                double one_val;
                for (int k=0; k<n_indices; k++)
                {
                    one_val = scratch_arr[ indices[k] ] * weight_before + val_after * weight_after;
                    if (one_val > arr[ indices[k] ])
                        arr[ indices[k] ] = one_val;
                }
            }
            else if (!allow_greater)
            {
                double one_val;
                for (int k=0; k<n_indices; k++)
                {
                    one_val = scratch_arr[ indices[k] ] * weight_before + val_after * weight_after;
                    if (one_val < arr[ indices[k] ])
                        arr[ indices[k] ] = one_val;
                }
            }
            else
            {
                for (int k=0; k<n_indices; k++)
                    arr[ indices[k] ] = scratch_arr[ indices[k] ] * weight_before + val_after * weight_after;
            }
        }
        // if weight_before == 1, don't need to to anything
        
        do_convert_scale(arr.begin(),
                         effect_scale, scale,
                         indices.begin(), n_indices);
        
    }
    else if (interpolate_abs_end) // need to interpolate between the end value and the 1st foreground value
    {
        // No need to convert scale beforehand - we have already converted the scratch_arr
        
        if (weight_before==1)
        {
            if (!allow_less)
            {
                for (int k=0; k<n_indices; k++)
                {
                    if (val_before > arr[ indices[k] ])
                        arr[ indices[k] ] = val_before;
                }
            }
            else if (!allow_greater)
            {
                for (int k=0; k<n_indices; k++)
                {
                    if (val_before < arr[ indices[k ]])
                        arr[ indices[k] ] = val_before;
                }
            }
            else
            {
                for (int k=0; k<n_indices; k++)
                    arr[ indices[k] ] = val_before;
            }
        }
        else if (weight_before > 0)
        {
            if (!allow_less)
            {
                double one_val;
                for (int k=0; k<n_indices; k++)
                {
                    one_val = scratch_arr[ indices[k] ] * weight_after + val_before * weight_before;
                    if (one_val > arr[ indices[k] ])
                        arr[ indices[k] ] = one_val;
                }
            }
            else if (!allow_greater)
            {
                double one_val;
                for (int k=0; k<n_indices; k++)
                {
                    one_val = scratch_arr[ indices[k] ] * weight_after + val_before * weight_before;
                    if (one_val < arr[ indices[k] ])
                        arr[ indices[k] ] = one_val;
                }
            }
            else
            {
                for (int k=0; k<n_indices; k++)
                    arr[ indices[k] ] = scratch_arr[ indices[k] ] * weight_after + val_before * weight_before;
            }
        }
        // if weight_after == 1, don't need to to anything
        
        do_convert_scale(arr.begin(),
                         effect_scale, scale,
                         indices.begin(), n_indices);
    }
    else
    {
        // No need to convert scale beforehand - we are just going to overwrite anyway
        
        if (!allow_less)
        {
            for (int k=0; k<n_indices; k++)
            {
                if (val > arr[ indices[k] ])
                    arr[ indices[k] ] = val;
            }
        }
        else if (!allow_greater)
        {
            for (int k=0; k<n_indices; k++)
            {
                if (val < arr[ indices[k ]])
                    arr[ indices[k] ] = val;
            }
        }
        else
        {
            for (int k=0; k<n_indices; k++)
                arr[ indices[k] ] = val;
        }
        
        do_convert_scale(arr.begin(),
                         effect_scale, scale,
                         indices.begin(), n_indices);
    }
    
    return (arr);
}

//-----------------------//
//-- THE MAIN FUNCTION --//
//-----------------------//

// indices_per_effect_per_foreground should be in C++ indexing (ie, indexed from ZERO)
//
// [[Rcpp::export]]
List apply_foregrounds(List values,
                       NumericVector value_times,
                       List after_values,
                       NumericVector times_to_apply_to,
                       List foregrounds,
                       List indices_per_effect_per_foreground,
                       CharacterVector scale)
{
    //-- Process the foregrounds a bit --//
    int n_frgd = foregrounds.length();
    int n_times = value_times.length();
    
    // figure out the first and last times we will have to deal with
    double first_start_time = R_PosInf;
    double last_end_time = R_NegInf;
    for (int f=0; f<n_frgd; f++)
    {
        List frgd = (List) foregrounds[f];
        double one_start_time = frgd["min.start.time"];
        double one_end_time = frgd["max.end.time"];
        
        if (one_start_time < first_start_time)
            first_start_time = one_start_time;
        
        if (one_end_time > last_end_time)
            last_end_time = one_end_time;
    }
    
    
    //-- Map the times to apply to to indices --//
    //   (we only care about indices in the range of the foregrounds)
    int indices_to_apply_to[times_to_apply_to.length()];
    int index = 0;
    int n_values_to_apply_to = 0;
    for (int i=0; i<times_to_apply_to.length() && times_to_apply_to[i]<=last_end_time; i++)
    {
        if (times_to_apply_to[i] >= first_start_time)
        {
            // this will work if every time to apply to is in value times - otherwise we err or loop forever
            // by construction, we require in the R code that times_to_apply_to is a subset of value_times
            while (value_times[index] < times_to_apply_to[i])
                index++;
            
            indices_to_apply_to[n_values_to_apply_to] = index;
            n_values_to_apply_to++;
        }
    }
    
    if (n_values_to_apply_to > 0)
    {
        //-- Now, make sure each value points to a distinct object --//
        //   (if not, we need to make a copy before we go modifying them)
    
        int i_last = n_times-1;
        while (value_times[i_last] > last_end_time && i_last>0)
            i_last--;
        
        // make sure values don't overlap
        for (int i_index=0; i_index<n_values_to_apply_to; i_index++)
        {
            int i = indices_to_apply_to[i_index];
            
            NumericVector v = (NumericVector) values[i];
            bool need_to_check_v = true;
            bool need_to_check_after = (after_values[i] != R_NilValue);
            NumericVector v_after;
            if (need_to_check_after)
                v_after = (NumericVector) after_values[i];
            
            // make sure value for this time does not overlap with value for any other time
            for (int j=0; j<n_times && need_to_check_v; j++)
            {
                NumericVector v_check = (NumericVector) values[j];
                
                if (i != j && v.begin() == v_check.begin())
                {
                    v = NumericVector(v.length());
                    for (int k=0; i<v.length(); k++)
                        v[k] = v_check[k];
                    
                    values[i] = v;
                    need_to_check_v = false;
                }
            }
            
            // make sure value_after for this time does not overlap with value or value_after for any other time
            for (int j=0; j<n_times && need_to_check_after; j++)
            {
                NumericVector v_check = (NumericVector) values[j];
                
                if (v_after.begin() == v_check.begin())
                {
                    v_after = NumericVector(v_after.length());
                    for (int k=0; i<v_after.length(); k++)
                        v_after[k] = v_check[k];
                    
                    after_values[i] = v_after;
                    need_to_check_after = false;
                }
                else if (i != j && after_values[j] != R_NilValue)
                {
                    v_check = after_values[j];
                    
                    if (v_after.begin() == v_check.begin())
                    {
                        v_after = NumericVector(v_after.length());
                        for (int k=0; i<v_after.length(); k++)
                            v_after[k] = v_check[k];
                        
                        after_values[i] = v_after;
                        need_to_check_after = false;
                    }
                }
            }
        }
        
    //    return (List::create(Named("values") = values , _["after.values"] = after_values));
        
        //-- set up a scratch array for converting the scale of first or last values --//
        NumericVector first_value = values[0];
        int value_len = first_value.length();
        double scratch_arr[value_len];
        
        for (int f=0; f<n_frgd; f++)
        {
            List frgd = (List) foregrounds[f];
            
            List effects = frgd["effects"];
            int n_effects = effects.length();
            
            List indices_per_effect = (List) indices_per_effect_per_foreground[f];
            
            for (int e=0; e<n_effects; e++)
            {
                List one_effect = (List) effects[e];
                double start_time = (double) one_effect["start.time"];
                double end_time = (double) one_effect["end.time"];
                bool allow_less = (bool) one_effect["allow.values.less.than.otherwise"];
                bool allow_greater = (bool) one_effect["allow.values.greater.than.otherwise"];
                NumericVector effect_times = one_effect["times"];
                NumericVector effect_values = one_effect["effect.values"];
                CharacterVector effect_scale = one_effect["scale"];
                
                int n_effect_times = effect_times.length();
                
                bool effect_is_multiplier = one_effect["apply.effects.as.multiplier"];
                bool effect_is_addend = one_effect["apply.effects.as.addend"];
                //bool effect_is_overwrite = !effect_is_multiplier && !effect_is_addend;
                
                IntegerVector indices = (IntegerVector) indices_per_effect[e];
                int n_indices = indices.length();
                
                // i indexes the time/values
                // j indexes within the effect
                
                // set up the index into values
                // advance it such that it is between start_time and effect_times[0]
                int i_index=0;
                while (value_times[ indices_to_apply_to[i_index] ] < start_time)
                    i_index++;
                int i = indices_to_apply_to[i_index];
                
                int j = -1; //index into effect_times
                double val_before = 0;
                if (effect_is_multiplier)
                    val_before = 1;
             //   else if (effect_is_addend)
            //        val_before = 0;
                double val_after = effect_values[0];
                double val;
                double val_to_write_after;
                
                double time_before = start_time;
                double time_after = effect_times[0];
                
                NumericVector arr;
                bool write_value, write_after_value;
                bool interpolate_abs_start, interpolate_abs_end;
                double weight_before, weight_after;
                bool have_copied_start_val = false;
                bool have_copied_end_val = false;
             
// Rcout << "n_values_to_apply_to = " << n_values_to_apply_to << ", n_effect_times = " << n_effect_times << "\n";
                // We're going to do two nested loops
                // 1) An outer loop (over j) that goes through effect times
                // 2) An inner loop (over i_index/i) that goes through all the value times for which this effect time j applies
                while (j <= n_effect_times)
                {
// Rcout << "**** j = " << j << ", time_before = " << time_before << ", time_after = " << time_after << "\n";
                    // at the start of this loop, we know that
                    //   value_times[i] >= time_before
                    while (i_index < n_values_to_apply_to && value_times[i] <= time_after)
                    {
                        // Figure out what we need to overwrite
                        
// Rcout << "** i = " << i << ", value_times[i] = " << value_times[i] << "\n";
                        
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
                            
                            if (j==n_effect_times) // we're at the end time and don't need to overwrite after_values
                                write_after_value = false;
                            else
                            {
                                write_after_value = true;
                                val_to_write_after = val_after;
                                
                                if (after_values[i]==R_NilValue) // need to create a copy to overwrite
                                {
                                    NumericVector to_copy = values[i];
                                    arr = NumericVector(to_copy.length());
                                    for (int k=0; k<to_copy.length(); k++)
                                        arr[k] = to_copy[k];
                                    
                                    after_values[i] = arr;
                                }
                            }
                            
                            interpolate_abs_start = false;
                            interpolate_abs_end = false;
                        }
                        else
                        {
                            write_value = j!=-1 || value_times[i]!=time_before;
                            write_after_value = j!=n_effect_times && after_values[i]!=R_NilValue;
                            
                            if (time_before == R_NegInf)
                            {
                                weight_before = 1;
                                weight_after = 0;
                                val = val_to_write_after = val_before;
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
                        
                        // If we need to, copy the start or end values into scratch_arr
                        //   and transform them into the scale for interpolating
                        if (interpolate_abs_start && !have_copied_start_val)
                        {
                            int start_value_index = i;
                            while (value_times[start_value_index] != start_time && start_value_index > 0)
                                start_value_index--;
                                // Assuming that one of the value_times == start_time, then start_value index will point at that value/time
                                //  And in our R code in the engine, we require that if any time between the start time and first effect time is 
                                //  in foreground times, then foreground times must also include the start time
                            
                            arr = (NumericVector) values[start_value_index];
                            for (int k=0; k<n_indices; k++)
                                scratch_arr[ indices[k] ] = arr[ indices[k] ];
                            
                            do_convert_scale(scratch_arr,
                                             scale, effect_scale,
                                             indices.begin(), n_indices);
                            
                            have_copied_start_val = true;
                            have_copied_end_val = false; //theoretically shouldn't need to mess with this flag, but will just to protect against future changes in flow
                        }
                        
                        if (interpolate_abs_end && !have_copied_end_val)
                        {
                            int end_value_index = i;
                            while (value_times[end_value_index] != end_time && end_value_index < n_times)
                                end_value_index++;
                            // Assuming that one of the value_times == end_time, then start_value index will point at that value/time
                            //  And in our R code in the engine, we require that if any time between the last effect time and end time is 
                            //  in foreground times, then foreground times must also include the end time
                            
                            arr = (NumericVector) values[end_value_index];
                            for (int k=0; k<n_indices; k++)
                                scratch_arr[ indices[k] ] = arr[ indices[k] ];
                            
                            do_convert_scale(scratch_arr,
                                             scale, effect_scale,
                                             indices.begin(), n_indices);
                            
                            have_copied_start_val = false; //theoretically shouldn't need to mess with this flag, but will just to protect against future changes in flow
                            have_copied_end_val = true;
                        }

                        // do the overwrite
                        if (write_value)
                        {
// Rcout << "Overwrite at time " << value_times[i] << "\n";
                            values[i] = do_foreground_overwrite(values[i],
                                                                scratch_arr,
                                                                effect_is_multiplier,
                                                                effect_is_addend,
                                                                interpolate_abs_start,
                                                                interpolate_abs_end,
                                                                allow_less,
                                                                allow_greater,
                                                                val,
                                                                val_before,
                                                                val_after,
                                                                weight_before,
                                                                weight_after,
                                                                scale,
                                                                effect_scale,
                                                                indices,
                                                                n_indices);
                        }
                        
                        if (write_after_value)
                        {
                            after_values[i] = do_foreground_overwrite(after_values[i],
                                                                      scratch_arr,
                                                                      effect_is_multiplier,
                                                                      effect_is_addend,
                                                                      interpolate_abs_start,
                                                                      interpolate_abs_end,
                                                                      allow_less,
                                                                      allow_greater,
                                                                      val_to_write_after,
                                                                      val_before,
                                                                      val_after,
                                                                      weight_before,
                                                                      weight_after,
                                                                      scale,
                                                                      effect_scale,
                                                                      indices,
                                                                      n_indices);
                        }
                        
                        //increment to the next value point
                        i_index++;
                        if (i_index < n_values_to_apply_to) //if this condition is false, the condition in the while loop will catch it
                            i = indices_to_apply_to[i_index];
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
                        // else - don't need val after. We are going to interpolate to the end value
                        
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
    }
        
    // make the list and return
    List rv = List::create(Named("values") = values , _["after.values"] = after_values);
    
    return (rv);
}
