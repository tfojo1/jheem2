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
            
            int n_effect_times = effect_times.length();
            
            bool effect_is_multiplier = one_effect["apply.effect.as.multiplier"];
            bool effect_is_addend = one_effect["apply.effect.as.addend"];
            bool effect_is_overwrite = !effect_is_multiplier && !effect_is_addend;
            
            IntegerVector indices = (IntegerVector) indices_per_effect[i_effect];
            
            // i indexes the time/values
            // j indexes within the effect
            
            int i=0;
            int j=-1;
            
            
        }
    }
    
    
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
