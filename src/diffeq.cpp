#include <Rcpp.h>
using namespace Rcpp;

//---------------//
//---------------//
//-- CONSTANTS --//
//---------------//
//---------------//

const int N_GROUPS = 2;

const int INFECTED_GROUP = 0;
const int UNINFECTED_GROUP = 1;

//-------------//
//-------------//
//-- HELPERS --//
//-------------//
//-------------//

// this is duplicated in engine_optimizations.cpp
void do_interpolate_quantity_elementwise(double *dst,
                                         List values,
                                         List after_values,
                                         NumericVector times,
                                         List value_applies,
                                         List after_value_applies,
                                         double time,
                                         int i_before,
                                         int i_after)
{
    int n_times = times.length();
    
    NumericVector one_val = values[0];
    LogicalVector one_val_applies;
    int len = one_val.length();
    
    if (i_before < 0)
        i_before = 0;
    
    if (i_after >= n_times)
        i_after = n_times-1;
    
    one_val_applies = after_value_applies[i_before];
    bool before_all_applies = one_val_applies.length()==1 && one_val_applies[0];
    
    one_val_applies = value_applies[i_after];
    bool after_all_applies = one_val_applies.length()==1 && one_val_applies[0];
    
    int i_before_for_k, i_after_for_k;
    double val_before, val_after;
    
    for (int k=0; k<len; k++)
    {
        // find the before value and index
        i_before_for_k = i_before;
        if (before_all_applies)
        {
            one_val = after_values[i_before];
            val_before = one_val[k];
        }
        else
        {
            while (i_before_for_k >= 0)
            {
                one_val_applies = after_value_applies[i_before_for_k];
                if (one_val_applies[k])
                {
                    one_val = after_values[i_before_for_k];
                    val_before = one_val[k];
                    break;
                }
                else
                {
                    one_val_applies = value_applies[i_before_for_k];
                    if ((one_val_applies.length()==1 && one_val_applies[0]) || one_val_applies[k])
                    {
                        one_val = values[i_before_for_k];
                        val_before = one_val[k];
                        break;
                    }
                }
                
                i_before_for_k--;
            }
        }
        
        // find the after value and index
        i_after_for_k = i_after;
        if (after_all_applies)
        {
            one_val = values[i_after];
            val_after = one_val[k];
        }
        else
        {
            while (i_after < n_times)
            {
                one_val_applies = value_applies[i_after_for_k];
                if ((one_val_applies.length()==1 && one_val_applies[0]) ||one_val_applies[k])
                {
                    one_val = values[i_after_for_k];
                    val_after = one_val[k];
                    break;
                }
                else
                {
                    one_val_applies = after_value_applies[i_after_for_k];
                    if ((one_val_applies.length()==1 && one_val_applies[0]) || one_val_applies[k])
                    {
                        one_val = after_values[i_after_for_k];
                        val_after = one_val[k];
                        break;
                    }
                }
            }
        }
        
        // Put them together
        if (i_before_for_k == -1)
            dst[k] = val_after;
        else if (time == times[i_before_for_k] || times[i_before_for_k] == R_NegInf ||
                 i_after_for_k == n_times)
            dst[k] = val_before;
        else if (time == times[i_after_for_k] || times[i_after_for_k] == R_PosInf)
            dst[k] = val_after;
        else
        {
            // Need to interpolate the value between before and after
            
            double before_weight = (times[i_after] - time) / (times[i_after] - times[i_before]);
            double after_weight = (time - times[i_before]) / (times[i_after] - times[i_before]);
            
            dst[k] = before_weight * val_before + after_weight * val_after;
        }
    }
}



// quant is a List with the following 
//  $times - A NumericVectors
//  $values - a List of NumericVectors, corresponding to the array value at each time
//  $scratch_offset - an integer (length-1 integer vector) index into quantity_scratch_vector where interpolated values for the quantity should go. Indexed from 0

double *get_quantity_value_for_time(List quant,
                                    double *quantity_scratch_vector,
                                    double time)
{
    bool is_single_value = quant["is.single.value"];
    if (is_single_value)
    {
        List values = quant["values"];
        NumericVector one_val = (NumericVector) values[0];
        return (one_val.begin());
    }
    else
    {
        NumericVector times = quant["times"];
        int n_times = times.length();
        List values = quant["values"];
        List after_values = quant["after.values"];
        List value_applies = quant["value.applies"];
        List after_value_applies = quant["after.value.applies"];
        
        NumericVector one_val = values[0];
        LogicalVector one_val_applies;
        int len = one_val.length();
        
        int scratch_offset = quant["scratch_offset"];
        double *scratch = quantity_scratch_vector + scratch_offset;
        
        if (n_times==1)
        {
            // Don't need to do any calculations, just return the pointer to the val or after_val array
            if (time <= times[0])
            {
                one_val = (NumericVector) values[0];
                return (one_val.begin());
            }
            else
            {
                NumericVector one_val = (NumericVector) after_values[0];
                return (one_val.begin());
            }
        }
        else
        {
            double first_time = times[0];
            double last_time = times[n_times-1];
            
            if (time <= first_time)
            {
                one_val_applies = value_applies[0];
                
                if (one_val_applies.length()==1 && one_val_applies[0])
                {
                    // Don't need to do any calculations, just return the pointer to the first val array
                    one_val = (NumericVector) values[0];
                    return (one_val.begin());
                }
                else //we're going to have to iterate forward
                {
                    for (int k=0; k<len; k++)
                    {
                        for (int i=0; i<n_times; i++)
                        {
                            one_val_applies = value_applies[i];
                            if (one_val_applies[k])
                            {
                                one_val = values[i];
                                scratch[k] = one_val[k];
                                break;
                            }
                            else
                            {
                                one_val_applies = after_value_applies[i];
                                if (one_val_applies[k])
                                {
                                    one_val = after_values[i];
                                    scratch[k] = one_val[k];
                                    break;
                                }
                            }
                        }
                    }
                    
                    return (scratch);
                }
            }
            else if (time >= last_time)
            {
                one_val_applies = after_value_applies[n_times-1];
                
                if (one_val_applies.length()==1 && one_val_applies[0])
                {
                    // Don't need to do any calculations, just return the pointer to the last after_val array
                    one_val = (NumericVector) after_values[n_times-1];
                    return (one_val.begin());
                }
                else //we're going to have to iterate backward
                {
                    for (int k=0; k<len; k++)
                    {
                        for (int i=(n_times-1); i>=0; i--)
                        {
                            one_val_applies = after_value_applies[i];
                            if (one_val_applies[k])
                            {
                                one_val = after_values[i];
                                scratch[k] = one_val[k];
                                break;
                            }
                            else
                            {
                                one_val_applies = value_applies[i];
                                if (one_val_applies[k])
                                {
                                    one_val = values[i];
                                    scratch[k] = one_val[k];
                                    break;
                                }
                            }
                        }
                    }
                    
                    return (scratch);
                }
            }
            else // we have to interpolate between two times
            {
                // Figure out which two indices we are interpolating between
                int i_after = 1;
                
                while (times[i_after] <= time) //this will terminate, otherwise, would have been caught by the time >= last_time condition above
                    i_after++;
                int i_before = i_after - 1;
                
                LogicalVector val_before_applies = after_value_applies[i_before];
                LogicalVector val_after_applies = after_value_applies[i_after];
                
                if ((time == times[i_before] || times[i_before] == R_NegInf) &&
                    (val_before_applies.length()==1 && val_before_applies[0]))
                {
                    // Don't need to do any calculations, just return the pointer to the array at before time
                    one_val = (NumericVector) values[i_before];
                    return (one_val.begin());
                }
                else if ((time == times[i_after] || times[i_after] == R_PosInf) &&
                         (val_after_applies.length()==1 && val_after_applies[0]))
                {
                    // Don't need to do any calculations, just return the pointer to the array at after time
                    one_val = (NumericVector) values[i_after];
                    return (one_val.begin());
                }
                else
                {
                    // Need to interpolate the value between before and after
                    // Store that in quantity_scratch_vector
                    // And return a pointer to its place in quantity_scratch_vector
                    
                    if (val_before_applies.length()==1 && val_before_applies[0] &&
                        val_after_applies.length()==1 && val_after_applies[0])
                    {
                        // We know before and after indices up front
                        NumericVector val_before = (NumericVector) after_values[i_before];
                        NumericVector val_after = (NumericVector) values[i_after];
                        
                        //Rcout << "i_before = " << i_before<< ", i_after = " << i_after << "\n";    
                        //Rcout << "val_before.length() = " << val_before.length() << ", val_after.length() = " << val_after.length() << ", len = " << len << "\n";                        
                        
                        double before_weight = (times[i_after] - time) / (times[i_after] - times[i_before]);
                        double after_weight = (time - times[i_before]) / (times[i_after] - times[i_before]);
                        
                        //Rcout << "before_weight = " << before_weight << ", after_weight = " << after_weight << "\n";
                        //Rcout << "scratch_offset = " << scratch_offset << ", scratch_offset + len = " << scratch_offset + len << "\n";
                        //return (one_val.begin()); 
                        for (int k=0; k<len; k++)
                            scratch[k] = before_weight * val_before[k] + after_weight * val_after[k];
                    }
                    else
                    {
                        // We have to find before or after index (or both) for each element
                        do_interpolate_quantity_elementwise(scratch, //double *dst,
                                                            values, //List values,
                                                            after_values, //List after_values,
                                                            times, //NumericVector times,
                                                            value_applies, //List value_applies,
                                                            after_value_applies, //List after_value_applies,
                                                            time,
                                                            i_before,
                                                            i_after);
                    }
                    
                    return (scratch);
                }
            }
        }
    }
}




void do_tracking(List trackers,
                 double *values,
                 double **quantities,
                 double *dx)
{
    for (int i_track=0; i_track<trackers.length(); i_track++)
    {
        List tracker = trackers[i_track];
        
        int multiply_by_quantity_index = tracker["multiply_by_quantity_index"];
        int n_to_track = tracker["n"];
        
        int offset = tracker["offset_into_tracked"];
        double *dx_track = dx + offset;
        
        IntegerVector state_indices = tracker["state_indices"];
        IntegerVector track_indices = tracker["track_indices"];
        
        if (multiply_by_quantity_index>=0)
        {
            double *quantity = quantities[multiply_by_quantity_index];
            IntegerVector multiply_by_indices = tracker["multiply_by_indices"];    
            for (int i=0; i<n_to_track; i++)
                dx_track[ track_indices[i] ] += values[ state_indices[i] ] * quantity[ multiply_by_indices[i] ];
        }
        else
        {
            for (int i=0; i<n_to_track; i++)
                dx_track[ track_indices[i] ] += values[ state_indices[i] ];
        }
    }
}

//-----------------------//
//-----------------------//
//-- THE MAIN FUNCTION --//
//-----------------------//
//-----------------------//

// quantities_info - a list that contains information for each quantity we will use
// 
// 
// settings - a list with elements
//  $state_length - an int representing the length of the total dx array
//  $indices_into_state_and_dx - A named IntegerVector giving the indices into state/dx for different components of the dx to return
//                               The expected names are "infected", "uninfected", "tracked_transitions", "tracked_mortality", "tracked_incidence", "tracked_remission", "tracked_population"
//  $state_and_dx_sizes - A named IntegerVector giving the sizes for different components of the dx to return
//                        The expected names are "infected", "uninfected"
//  
// quantity_scratch_vector - A preallocated NumericVector that has just the right length for all quantities we will need to interpolate
// 
// scratch_vector - A preallocated NumericVector into which we can put temporary values from calculations  
//  
// quantities_info - a list with one element per quantity we use in computing the dx
//  Each element is a list with elements
//      $is.single.value - A single logical value, indicating whether there is just a single value for all times
//      $times - a numeric vector
//      $values - A list of numeric vectors
//      $after.values - A list of numeric vectors
//      $value.applies - A list of logical vectors
//      $after.value.applies - A list of logical vectors
//      $scratch_offset - an integer (length-1 integer vector) index into quantity_scratch where interpolated values for the quantity should go. Indexed from 0
//
//    
// natality_info is a list with one element for each group (ie, two elements: infected and uninfected)
//  Each of these elements is a list with the following elements
//      $from_group - An integer indicating the group/population to which fertility rates are applied. Either INFECTED_GROUP (0) or UNINFECTED_GROUP (1)
//      $to_group - An integer indicating the group/population into which offspring are born. Either INFECTED_GROUP (0) or UNINFECTED_GROUP (1)
//      $n_parent_categories - An integer indicating how many 'parent categories' there are
//          **NB: A *parent-category* is a set of compartments in the parent ontology (may be just one compartment)
//                for whom all births are distributed into the same offspring compartments the same way (ie, according to the same proportions)
//      $n_parent_compartments_per_parent_category - An integer indicating how many individual parent compartments per parent category there are
//      $n_offspring_compartments_per_parent_category - An integer indicating how many individual compartments per parent category offspring will be distributed into
//
//      $fertility_quantity_index - An index into quantities giving the quantity representing fertility rate. Indexed from zero
//      $birth_proportions_quantity_index - An index into quantities giving the quantity representing birth proportions. Indexed from zero
//      
//      $state_indices_for_parent_categories - A list of integer vectors, one for each parent category, giving indices into the state array of parents.
//      $fertility_rate_indices_for_parent_categories - A list of integer vectors, one for each parent category, giving the indices into the fertility rates quantity.
//      
//      $offspring_indices_for_parent_categories - A list of integer vectors, one for each parent category, giving indices into the state array for offspring.
//      $birth_proportion_indices_for_parent_categories - A list of integer vectors, one for each parent category, giving the indices into the birth_proportions quantity.
//      
//      $from_birth_trackers - A List of length n_to_groups. Each element is a list of trackers that track births into the corresponding to group ACCORDING TO WHERE THEY WERE BORN FROM
//      $by_incidence_trackers - A List of length n_to_groups. Each element is a list of trackers that track births WHICH ARE INCIDENT CASES into the corresponding to group ACCORDING TO WHERE THEY WERE BORN FROM
//      $to_birth_trackers - A List of length n_to_groups. Each element is a list of trackers that track births into the corresponding to group
//      $to_incidence_trackers - A List of length n_to_groups. Each element is a list of trackers that track births WHICH ARE INCIDENT CASES into the corresponding to group
//  
// transitions_info - a list with one element per transition we need to apply to compute the dx
//  Each element is a list with elements
//      $group - An integer indicating the group/population to which the transition applies. Either INFECTED_GROUP (0) or UNINFECTED_GROUP (1)
//      $quantity_index - The index into quantities_info denoting the quantity which gives the transition rates. Indexed from 0
//      $n - an integer representing the number of compartments that have a transition out of them in this
//      $state_from_indices - An integer vector, giving the indices into the state from which transitions leave
//      $rate_indices - An integer vector of indices into the transition rate vector for each transition
//      $state_to_indices - An integer vector, giving the indices into the state which each transition goes into
//      $trackers - A list, with one "tracker" for every tracked transition which this transition need to go into.
//                  (See below for documentation of trackers)
//
// mortality_info is a list 
//  Each element is a list with elements
//      $group - An integer indicating the group/population to which the mortality applies. Either INFECTED_GROUP (0) or UNINFECTED_GROUP (1)
//      $n - An integer giving the number of compartments to which mortality applies
//      $quantity_index - The index into quantities_info denoting the quantity which gives the mortality rates. Indexed from 0
//      $rate_indices - An IntegerVector representing the indices into the mortality rates quantity which correspond to state indices
//      $trackers - A list, with one "tracker" for every tracked quantity which this mortality needs to go into.
//                  (See below for documentation of trackers)
//
// infections_info is a list
//  Each element is a list with elements:
//      $contact_quantity_index - An index into quantities giving the quantity representing the contact matrix. Indexed from zero
//      $transmissibility_quantity_index - An index into quantities giving the quantity representing the transmissibility. Indexed from zero
//      $susceptibility_quantity_index - An index into quantities giving the quantity representing the susceptibility. Indexed from zero
//      $new_infection_proportions_quantity_index - An index into quantities giving the quantity representing the proportions according to which new infections are distributed. Indexed from zero
//      
//      $n_from_contacts - An integer giving the number of rows in the contact matrix
//      $n_to_contacts - An integer giving the number of columns in the contact matrix
//      
//      $state_indices_for_to_contacts - An IntegerVector giving indices into the uninfected state array. Represents a matrix with n_to_contacts columns
//      $susceptibility_indices_for_to_contacts - An IntegerVector giving indices into the susceptibility quantity. Represents a matrix with n_to_contacts columns
//      
//      $state_indices_for_transmitting_from_contacts - An IntegerVector giving indices into the infected state array for compartments which can transmit. Represents a matrix with n_from_contacts columns
//      $transmissibility_indices_for_transmitting_from_contacts - An IntegerVector giving indices into the transmissibility quantity for compartments which can transmit. Represents a matrix with n_from_contacts columns
//      
//      $denominator_infected_indices_for_from_contacts - An IntegerVector giving indices into the infected state array. Represents a matrix with n_from_contacts columns
//      $denominator_uninfected_indices_for_from_contacts - An IntegerVector giving indices into the UNinfected state array. Represents a matrix with n_from_contacts columns
//      
//      $new_infection_proportions_indices - An IntegerVector giving indices into the new_infections_proportions quantity. Represents a matrix with one column for each compartment in the uninfected state
//      $new_infection_state_indices - An IntegerVector giving indices into the infected state array. Represents a matrix with one column for each compartment in the uninfected state
//            
//      $contact_indices_for_to_contacts - A List of n_to_contacts IntegerVectors. Each element is n_from_contacts long. contact_indices_for_to_contacts[[to]][from] gives the index into the contact matrix quantity corresponding to transmissions from [from] to [to]
//
//      $to_trackers - A list, with one "tracker" for every tracked quantity which tracks the infected ("to") state of individuals after they become infected
//      $from_trackers - A list, with one "tracker" for every tracked quantity which tracks the uninfected ("from") state of individuals who will be infected before they become infected
//      $by_trackers - A list, with one "tracker" for every tracked quantity which tracks the infected compartments that are responsible for generating infections ("by") in other compartments
//      
// remission_info is a list 
//  Each element is a list with elements
//      $n_from - An integer indicating how many compartments undergo remission from this remission element
//      $n_to_per_from - A single integer indicating how many to (uninfected) compartments each from (infected) compartment is partitions into as it undergoes remission
//      
//      $remission_quantity_index - The index into quantities_info denoting the quantity which gives the remission rates. Indexed from 0
//      $proportions_quantity_index -  The index into quantities_info denoting the quantity which gives the proportions according to which remitting individuals are distributed into the uninfected state. Indexed from 0
//
//      $remission_rate_indices - An IntegerVector of length n_from representing the indices into the remission rates quantity for each compartment undergoing remission here
//      $from_state_indices - An IntegerVector of length n_from, giving the index into the infected state of each compartment undergoing remission here
//
//      $to_indices_for_from - A list with n_from elements, each of which is an IntegerVector fo length n_to_per_from giving the indices into the uninfected state of each to compartment that from compartments go into as they undergo remission
//      $proportions_indices_for_from - A list with n_from elements, each of which is an IntegerVector fo length n_to_per_from giving the indices into the proportions quantity of each to compartment that from compartments go into as they undergo remission
//
//      $from_trackers - A list, with one "tracker" for every tracked quantity which tracks the infected ("from") state of individuals who will undergo remission before they undergo remission
//      $to_trackers - A list, with one "tracker" for every tracked quantity which tracks the uninfected ("to") state of individuals after they undergo remission
//                  (See below for documentation of trackers)      
//                
// fixed_strata_info is a list
//  Each element is itself a list, with elements
//      $applies_after_time - A single numeric value. Times equal to or after this but less than $applies_before_time will have strata fixed
//      $applies_before_time - A single numeric value
//      $fix_strata - A single logical indicator of whether to fix strata or not
//
//      $n_fixed_strata - A single int value
//      $n_uninfected_compartments_per_fixed_stratum - A single int value
//      $n_infected_compartments_per_fixed_stratum - A single int value
//  
//      $uninfected_indices_for_stratum - A list of n_fixed_strata IntegerVectors, each of which represents indices into the uninfected state for a stratum to fix
//      $infected_indices_for_stratum - A list of n_fixed_strata IntegerVectors, each of which represents indices into the infected state for a stratum to fix
//
// population_trackers is a list with one element for each group (infected, uninfected)
//  Each element is a list of trackers for that group (See below)               
//                    
//  *trackers (the elements of transitions[[x]]$trackers, incidence_trackers, population_trackers, mortality_trackers)
//          Each element is a list with elements:
//          $offset_into_tracked - an integer offset into dx
//          $n - an integer representing the number of elements in the state array
//          $group - An integer indicating the group/population to which the tracking applies. Either INFECTED_GROUP (0) or UNINFECTED_GROUP (1)
//          $multiply_by_quantity_index - an integer index into quantities_info denoting which quantity to multiply by before storing in tracked.
//                                        Indexed from 0. Set to -1 if no quantity to multiply by
//          $state_indices - an IntegerVector of length n, indicating the indices of state to store. Indexed from zero
//          $track_indices - an IntegerVector of length n, indicating the indices into the dx_track[offset_into_tracked] to store into. Indexed from zero
//          $multiply_by_indices - If there is a multiply_by_quantity, an IntegerVector indicating the indices into quantities[multiply_by_quantity_index] to multiply each state value by. Indexed from zero
//          * note, for i in 0:(n-1), we do dx_track_<x>[ offset_into_tracked + track_indices[i] ] = state[ <to track>[i] * quantities[multiply_by_quantity_index][ multiply_by_indices[i] ]
// 
// 
// 
// COMMENTS ON THE SCRATCH VECTORS
// scratch_vector comprises scratch1, scratch2, scratch3, and scratch4 (back-to-back)
// scratch1 must have length to accomodate:
// - the infected state (for counting transmissibility for arbitrarily-sized subsets of infected)
// scratch2 must have length to accomodate:
// - the infected state (for tracking new infections "to")
// - the larger of the uninfected or infected states (for tracking births "to") - although since scratch3 and scratch4 are not used for births, we will be fine
// - The uninfected state (for tracking remissions "to")
// scratch3 must have length to accomodate:
// - the uninfected state (for tracking new infections "from")
// scratch4 must have length to accomodate:
// - the largest number of from comparments for any contact matrix (or, conservatively, the infected state)
// The total scratch must have length to accomodate:
// - infected or uninfected state (whichever is greater, for tracking transitions, mortality, and births)
// 
// [[Rcpp::export]]
NumericVector compute_dx(NumericVector state,
                         double time,
                         List settings,
                         NumericVector quantity_scratch_vector,
                         NumericVector scratch_vector,
                         List quantities_info,
                         List natality_info,
                         List mortality_info,
                         List transitions_info,
                         List infections_info,
                         List remission_info,
                         List fixed_strata_info,
                         List population_trackers)
{
    bool debug = false;
    if (debug)
        Rcout << "Starting compute_dx()\n";
    //--------------------//
    //-- INITIAL SET-UP --//
    //--------------------//
    
    //-- Unpack Settings --//
    int state_length = settings["state_length"];
    IntegerVector indices_into_state_and_dx = settings["indices_into_state_and_dx"];
    IntegerVector state_and_dx_sizes = settings["state_and_dx_sizes"];
    
    //-- Unpack n's --//
    int n_infected = state_and_dx_sizes["infected"];
    int n_uninfected = state_and_dx_sizes["uninfected"];
    
    int n_for_group[N_GROUPS];
    n_for_group[INFECTED_GROUP] = n_infected;
    n_for_group[UNINFECTED_GROUP] = n_uninfected;
    
    //-- Index into State --//
    double *infected = state.begin() + indices_into_state_and_dx["infected"];
    double *uninfected = state.begin() + indices_into_state_and_dx["uninfected"];
    
    double *sub_states_for_group[N_GROUPS];
    sub_states_for_group[INFECTED_GROUP] = infected;
    sub_states_for_group[UNINFECTED_GROUP] = uninfected;
    
    //-- Set-up Return Value, and aliases its subsets --//
    NumericVector dx(state_length);
    for (int i=0; i<state_length; i++)
        dx[i] = 0;
    
    double *dx_infected = dx.begin() + indices_into_state_and_dx["infected"];
    double *dx_uninfected = dx.begin() + indices_into_state_and_dx["uninfected"];
    //    double *dx_tracked_transitions = dx.begin() + indices_into_state_and_dx["tracked_transitions"];
    //    double *dx_tracked_births = dx.begin() + indices_into_state_and_dx["tracked_births"];
    //    double *dx_tracked_mortality = dx.begin() + indices_into_state_and_dx["tracked_mortality"];
    //    double *dx_tracked_incidence = dx.begin() + indices_into_state_and_dx["tracked_incidence"];
    //    double *dx_tracked_remission = dx.begin() + indices_into_state_and_dx["tracked_remission"];
    //    double *dx_tracked_population = dx.begin() + indices_into_state_and_dx["tracked_population"];
    
    double *dx_for_group[N_GROUPS];
    dx_for_group[INFECTED_GROUP] = dx_infected;
    dx_for_group[UNINFECTED_GROUP] = dx_uninfected;
    
    
    if (debug)
        Rcout << "Done with initial set up\n";
    
    
    
    if (debug)
        Rcout << "Setting up quantity values for time...\n";
    //-- Get the Values for Each Quantity for this time --//
    double *quantities[quantities_info.length()];
    
    //test 
    //int i_test = 77;
    //quantities[i_test] = get_quantity_value_for_time((List) quantities_info[i_test], quantity_scratch_vector.begin(), time);
    
    for (int i_quant=0; i_quant<quantities_info.length(); i_quant++)
    {
        //        Rcout << "- quantity " << i_quant << "\n";
        quantities[i_quant] = get_quantity_value_for_time((List) quantities_info[i_quant], quantity_scratch_vector.begin(), time);
    }
    if (debug)
        Rcout << "  Done.\n";
    
    //-- Alias the scratch vector --//
    double *scratch = scratch_vector.begin();
    double *scratch1 = scratch;
    double *scratch2 = scratch1 + n_infected;
    double *scratch3 = scratch2 + n_infected;
    double* scratch4 = scratch3 + n_uninfected;
    
    
    //------------------------------------//
    //-- ENFORCE INFECTED/UNINFECED >=0 --//
    //------------------------------------//
    
    for (int i=0; i<n_uninfected; i++)
    {
        if (uninfected[i] < 0)
            uninfected[i] = 0;
    }
    
    for (int i=0; i<n_infected; i++)
    {
        if (infected[i] < 0)
            infected[i] = 0;
    }
    
    //------------//
    //-- BIRTHS --//
    //------------//
    
    if (debug)
        Rcout << "Doing natality...";
    int n_natalities = natality_info.length();
    for (int i_natality=0; i_natality<n_natalities; i_natality++)
    {
        List one_natality_info = natality_info[i_natality];
        
        // Pull Basic Elements for the Births
        int from_group = one_natality_info["from_group"];
        int to_group = one_natality_info["to_group"];
        int n_parent_categories = one_natality_info["n_parent_categories"];
        int n_parent_compartments_per_parent_category = one_natality_info["n_parent_compartments_per_parent_category"];
        int n_offspring_compartments_per_parent_category = one_natality_info["n_offspring_compartments_per_parent_category"];
        
        // Set up the state_sub, dx_offspring
        double *state_sub = sub_states_for_group[from_group];
        double *dx_offspring = dx_for_group[to_group];
        
        // Pull the Quantities (Fertility Rates, Birth Proportions)
        int quantity_index = one_natality_info["fertility_quantity_index"];
        double *fertility_rates = quantities[quantity_index];
        
        quantity_index = one_natality_info["birth_proportions_quantity_index"];
        double *birth_proportions = quantities[quantity_index];
        
        // Figure out if we need to track
        List from_birth_trackers = one_natality_info["from_birth_trackers"];
        List by_incidence_trackers = one_natality_info["by_incidence_trackers"];
        List to_birth_trackers = one_natality_info["to_birth_trackers"];
        List to_incidence_trackers = one_natality_info["to_incidence_trackers"];
        
        bool need_to_track_from = from_birth_trackers.length() > 0 ||
            by_incidence_trackers.length() > 0;
        bool need_to_track_to = to_birth_trackers.length() > 0 ||
            to_incidence_trackers.length() > 0;
        
        
        if (need_to_track_to)
        {
            for (int i=0; i<n_for_group[to_group]; i++)
                scratch2[i] = 0;
        }
        
        // Pull indices for the from group
        List state_indices_for_parent_categories = one_natality_info["state_indices_for_parent_categories"];
        List fertility_rate_indices_for_parent_categories = one_natality_info["fertility_rate_indices_for_parent_categories"];
        
        // Pull indices for the to group
        List offspring_indices_for_parent_categories = one_natality_info["offspring_indices_for_parent_categories"];
        List birth_proportion_indices_for_parent_categories = one_natality_info["birth_proportion_indices_for_parent_categories"];
        
        // Iterate through each from parent category 
        double births;
        double births_from;
        double births_to;
        IntegerVector offspring_indices;
        IntegerVector birth_proportion_indices;
        for (int from=0; from<n_parent_categories; from++)
        {
            births_from  = 0;
            
            // Calculate the Births
            IntegerVector parent_category_state_indices = state_indices_for_parent_categories[from];
            IntegerVector parent_category_fertility_rate_indices = fertility_rate_indices_for_parent_categories[from];
            
            for (int i=0; i<n_parent_compartments_per_parent_category; i++)
            {
                births = state_sub[ parent_category_state_indices[i] ] * fertility_rates[ parent_category_fertility_rate_indices[i] ];
                births_from += births;
                
                if (need_to_track_from)
                    scratch1[ parent_category_state_indices[i] ] = births;
            }
            
            // Distribute the Births
            offspring_indices = offspring_indices_for_parent_categories[from];
            birth_proportion_indices = birth_proportion_indices_for_parent_categories[from];
            
            for (int i=0; i<n_offspring_compartments_per_parent_category; i++)
            {
                births_to = births_from * birth_proportions[ birth_proportion_indices[i] ];
                dx_offspring[ offspring_indices[i] ] += births_to;
                
                if (need_to_track_to)
                    scratch2[ offspring_indices[i] ] += births_to;
            }
        }
        
        // Tracking
        do_tracking(from_birth_trackers,
                    scratch1, //values
                    quantities,
                    dx.begin());
        
        do_tracking(by_incidence_trackers,
                    scratch1, //values
                    quantities,
                    dx.begin());
        
        do_tracking(to_birth_trackers,
                    scratch2, //values
                    quantities,
                    dx.begin());
        
        do_tracking(to_incidence_trackers,
                    scratch2, //values
                    quantities,
                    dx.begin());
        
    }
    if (debug)
        Rcout << "Done.\n";
    
    
    //---------------//
    //-- MORTALITY --//
    //---------------//
    
    if (debug)
        Rcout << "Doing Mortality...";
    int n_mortalities = mortality_info.length();
    
    for (int i_mort=0; i_mort<n_mortalities; i_mort++)
    {
        List one_mort_info = mortality_info[i_mort];
        
        // Set up indices into state and dx
        int group = one_mort_info["group"];
        
        int n = one_mort_info["n"];
        double *dx_for_mort = dx_for_group[group];
        double *sub_state = sub_states_for_group[group];
        
        // Pull mortality rate and indices
        int quantity_index = one_mort_info["quantity_index"];
        double *mortality_rates = quantities[quantity_index];
        
        IntegerVector mortality_rate_indices = one_mort_info["rate_indices"];
        
        // Set up for tracking
        List trackers = one_mort_info["trackers"];
        bool need_to_track = trackers.length() > 0;
        
        double deaths;
        for (int i=0; i<n; i++)
        {
            deaths = sub_state[i] * mortality_rates[ mortality_rate_indices[i] ];
            
            dx_for_mort[i] -= deaths;
            if (need_to_track)
                scratch[i] = deaths;
        }
        
        // Track the mortality if needed
        if (need_to_track)
        {
            do_tracking(trackers,
                        scratch, //values
                        quantities,
                        dx.begin());
            //dx_tracked_mortality);
        }
        
    }
    if (debug)
        Rcout << "Done.\n"; 
    
    //-----------------//
    //-- TRANSITIONS --//
    //-----------------//
    
    
    if (debug)
        Rcout << "Doing Transitions...";    
    int n_transitions = transitions_info.length();
    
    for (int i_trans=0; i_trans<n_transitions; i_trans++)
    {
        List trans = transitions_info[i_trans];
        
        // Get a pointer to the section of the state that represents the population to which this transition applies
        //  and to the corresponding section of the dx for the state
        int group = trans["group"];
        double *state_sub = infected;
        double *dx_state_for_transition = dx_infected;
        int n_state_sub = n_infected;
        if (group == UNINFECTED_GROUP)
        {
            state_sub = uninfected;
            dx_state_for_transition = dx_uninfected;
            n_state_sub = n_uninfected;
        }
        
        // Get a pointer to the trates
        int quantity_index = trans["quantity_index"];
        double *trates = quantities[ quantity_index ];
        
        // Get the indices
        int n_transition = trans["n"];
        IntegerVector trate_indices = trans["rate_indices"];
        IntegerVector state_from_indices = trans["state_from_indices"];
        IntegerVector state_to_indices = trans["state_to_indices"];
        
        // Set up for tracking
        List trackers = trans["trackers"];
        bool need_to_track = trackers.length() > 0;
        
        // Iterate through state_sub and calculate the dx
        // Fold into dx and store into scratch
        double val;
        int from_index;
        for (int i=0; i<n_transition; i++)
        {
            from_index = state_from_indices[i];
            val = state_sub[from_index] * trates[ trate_indices[i] ];
            
            dx_state_for_transition[from_index] -= val;
            dx_state_for_transition[ state_to_indices[i] ] += val;
            
            if (need_to_track)
                scratch[from_index] = val;
        }
        
        // Track the transition if needed
        if (need_to_track)
        {
            do_tracking(trackers,
                        scratch, //values
                        quantities,
                        dx.begin());
        }
    }
    
    if (debug)
        Rcout << "Done.\n";
    
    //----------------//
    //-- INFECTIONS --//
    //----------------//
    
    if (debug)
        Rcout << "Doing infections...";
    double *aggregate_from_transmissibility = scratch1;
    double *to_infection_tracking = scratch2;
    double *from_infection_tracking = scratch3;
    double *cached_from_denominators = scratch4;
    
    int n_infections_info = infections_info.length();
    for (int i_inf=0; i_inf<n_infections_info; i_inf++)
    {
        List one_infections_info = infections_info[i_inf];
        
        // Pull some metadata
        int n_from_contacts = one_infections_info["n_from_contacts"];
        int n_to_contacts = one_infections_info["n_to_contacts"];
        
        // Set up for Tracking
        List from_trackers = one_infections_info["from_incidence_trackers"];
        List to_trackers = one_infections_info["to_incidence_trackers"];
        List by_trackers = one_infections_info["by_incidence_trackers"];
        
        bool need_to_track_to = to_trackers.length() > 0;
        bool need_to_track_from = from_trackers.length() > 0;
        bool need_to_track_by = by_trackers.length() > 0;
        
        
        // Pull the Relevant Quantities (Contact Matrix, Susceptibility, Transmissibility)
        int quantity_index = one_infections_info["contact_quantity_index"];
        double *contact_matrix = quantities[quantity_index];
        
        quantity_index = one_infections_info["susceptibility_quantity_index"];
        double *susceptibility = quantities[quantity_index];
        
        quantity_index = one_infections_info["transmissibility_quantity_index"];
        double *transmissibility = quantities[quantity_index];
        
        quantity_index = one_infections_info["new_infection_proportions_quantity_index"];
        double *new_infection_proportions = quantities[quantity_index];
        
        
        // Pull Indices for "To" (uninfected), Susceptibility, and New Infection Proportions
        List state_indices_for_to_contacts = one_infections_info["state_indices_for_to_contacts"];
        List susceptibility_indices_for_to_contacts = one_infections_info["susceptibility_indices_for_to_contacts"];
        IntegerVector to_state_indices = state_indices_for_to_contacts[0];
        IntegerVector susceptibility_indices;
        int n_compartments_per_to_contact = to_state_indices.length();
        
        List new_infection_proportions_indices_for_to_contacts = one_infections_info["new_infection_proportions_indices_for_to_contacts"];
        List new_infection_state_indices_for_to_contacts = one_infections_info["new_infection_state_indices_for_to_contacts"]; 
        
        List new_infection_proportions_indices_for_to_compartments;
        List new_infection_state_indices_for_to_compartments = new_infection_state_indices_for_to_contacts[0];
        
        IntegerVector new_infection_proportions_indices;
        IntegerVector new_infection_state_indices = new_infection_state_indices_for_to_compartments[0];
        int n_infected_compartments_per_uninfected = new_infection_state_indices.length();
        
        // Pull Indices for "From" (infected) and Transmissibility
        List state_indices_for_transmitting_from_contacts = one_infections_info["state_indices_for_transmitting_from_contacts"];
        List transmissibility_indices_for_transmitting_from_contacts = one_infections_info["transmissibility_indices_for_transmitting_from_contacts"];
        List denominator_infected_indices_for_from_contacts = one_infections_info["denominator_infected_indices_for_from_contacts"];
        List denominator_uninfected_indices_for_from_contacts = one_infections_info["denominator_uninfected_indices_for_from_contacts"];
        
        IntegerVector transmissibility_indices;
        
        IntegerVector from_state_indices = state_indices_for_transmitting_from_contacts[0];
        int n_transmitting_per_from_contact = from_state_indices.length();
        
        IntegerVector denominator_infected_indices = denominator_infected_indices_for_from_contacts[0];
        int n_denominator_infected_per_from_contact = denominator_infected_indices.length();
        
        IntegerVector denominator_uninfected_indices = denominator_uninfected_indices_for_from_contacts[0];
        int n_denominator_uninfected_per_from_contact = denominator_uninfected_indices.length();
        
        // Pull Contact Indices
        List contact_indices_for_to_contacts =  one_infections_info["contact_indices_for_to_contacts"];
        
        // STEP 1: Calculate the Aggregate Transmissibility Coming from each Category of From Compartments
        double numerator;
        double denominator;
        for (int from=0; from<n_from_contacts; from++)
        {
            from_state_indices = state_indices_for_transmitting_from_contacts[from];
            transmissibility_indices = transmissibility_indices_for_transmitting_from_contacts[from];
            denominator_infected_indices = denominator_infected_indices_for_from_contacts[from];
            denominator_uninfected_indices = denominator_uninfected_indices_for_from_contacts[from];
            
            denominator = 0;
            
            for (int i=0; i<n_denominator_infected_per_from_contact; i++)
                denominator += infected[ denominator_infected_indices[i] ];
            
            for (int i=0; i<n_denominator_uninfected_per_from_contact; i++)
                denominator += uninfected[ denominator_uninfected_indices[i] ];
            
            if (denominator==0)
                aggregate_from_transmissibility[from] = 0;
            else
            {
                numerator = 0;
                for (int i=0; i<n_transmitting_per_from_contact; i++)
                    numerator += infected[ from_state_indices[i] ] *
                        transmissibility[ transmissibility_indices[i] ];
                
                aggregate_from_transmissibility[from] = numerator / denominator;
            }
            
            if (need_to_track_by)
                cached_from_denominators[from] = denominator;
        }
        
        // Clear the tracking counts
        if (need_to_track_to)
        {
            for (int i=0; i<n_infected; i++)
                to_infection_tracking[i] = 0;
        }
        
        // Declare some variables we will use for calculations
        double from_force_of_infection;
        double infections;
        double distributed_infections;
        int uninfected_state_index;
        int infected_state_index;
        
        // Iterate through the to categories in the matrix (columns)
        for (int to=0; to<n_to_contacts; to++)
        {
            to_state_indices = state_indices_for_to_contacts[to];
            susceptibility_indices = susceptibility_indices_for_to_contacts[to];
            
            new_infection_proportions_indices_for_to_compartments = new_infection_proportions_indices_for_to_contacts[to];
            new_infection_state_indices_for_to_compartments = new_infection_state_indices_for_to_contacts[to];
            
            // Get the row in the matrix
            IntegerVector contact_indices_for_to = (IntegerVector) contact_indices_for_to_contacts[to];
            //double *contact_row = contact_matrix + to * n_from_contacts;
            
            // STEP 2: Sum up the Contact Rates * From Transmissibility
            //          for All From Categories for a To Category
            // Iterate through the from categories in the matrix (rows)
            from_force_of_infection = 0;
            for (int from=0; from<n_from_contacts; from++)
                from_force_of_infection += aggregate_from_transmissibility[from] * contact_matrix[ contact_indices_for_to[from] ];
            
            // Iterate through each to compartment in the to category   
            for (int i=0; i<n_compartments_per_to_contact; i++)
            {
                new_infection_proportions_indices = new_infection_proportions_indices_for_to_compartments[i];
                new_infection_state_indices = new_infection_state_indices_for_to_compartments[i];
                
                // STEP 3: For Each To Category, Multiply its Susceptibility by the From Force of Infection
                //          to get the number of New Infections. Remove these from Uninfected State
                uninfected_state_index = to_state_indices[i];
                infections = uninfected[uninfected_state_index] * 
                    susceptibility[ susceptibility_indices[i] ] * 
                    from_force_of_infection;
                dx_uninfected[uninfected_state_index] -= infections;
                
                if (need_to_track_from)
                    from_infection_tracking[uninfected_state_index] = infections;
                
                for (int j=0; j<n_infected_compartments_per_uninfected; j++)
                {
                    // STEP 4: Distribute the New Infections Across Compartments in the Infected State
                    distributed_infections = infections * new_infection_proportions[ new_infection_proportions_indices[j] ];
                    
                    infected_state_index = new_infection_state_indices[j];
                    dx_infected[infected_state_index] += distributed_infections;
                    
                    if (need_to_track_to)
                        to_infection_tracking[infected_state_index] = distributed_infections;
                }
            }
        }
        
        
        // Do the Tracking (if we need to)
        if (need_to_track_from)
        {
            do_tracking(from_trackers,
                        from_infection_tracking, //values
                        quantities,
                        dx.begin());
        }
        
        if (need_to_track_to)
        {
            do_tracking(to_trackers,
                        to_infection_tracking, //values
                        quantities,
                        dx.begin());
        }
        
        if (need_to_track_by) // we are not optimizing for this since it is not usual to do it
            // it's basically another nested for loop over the contact matrix, so inefficient
        {
            double *by_infection_tracking = scratch2;
            double aggregate_susceptibility;
            double denominator;
            int from_state_index;
            for (int from=0; from<n_from_contacts; from++)
            {
                from_state_indices = state_indices_for_transmitting_from_contacts[from];
                transmissibility_indices = transmissibility_indices_for_transmitting_from_contacts[from];
                
                aggregate_susceptibility = 0;
                for (int to=0; to<n_to_contacts; to++)
                {
                    to_state_indices = state_indices_for_to_contacts[to];
                    susceptibility_indices = susceptibility_indices_for_to_contacts[to];
                    
                    for (int i=0; i<n_compartments_per_to_contact; i++)
                    {
                        aggregate_susceptibility += uninfected[ to_state_indices[i] ] * 
                            susceptibility[ susceptibility_indices[i] ] *
                            contact_matrix[to * n_from_contacts + from];
                    }
                }
                
                denominator = cached_from_denominators[from];
                for (int i=0; i<n_transmitting_per_from_contact; i++)
                {
                    from_state_index = from_state_indices[i];
                    
                    if (denominator==0)
                        by_infection_tracking[from_state_index] = 0;
                    else
                        by_infection_tracking[from_state_index] = infected[from_state_index] *
                            transmissibility[ transmissibility_indices[i] ] *
                            aggregate_susceptibility /
                                denominator;
                }
                
            }
            
            do_tracking(by_trackers,
                        by_infection_tracking, //values
                        quantities,
                        dx.begin());
        }
    }
    
    if (debug)
        Rcout << "Done.\n";
    
    //---------------//
    //-- REMISSION --//
    //---------------//
    
    if (debug)
        Rcout << "Doing remission...";
    int n_remissions = remission_info.length();
    for (int i_remission=0; i_remission<n_remissions; i_remission++)
    {
        List one_remission_info = remission_info[i_remission];
        
        // Pull remission rate and indices
        int remission_quantity_index = one_remission_info["remission_quantity_index"];
        double *remission_rates = quantities[remission_quantity_index];
        IntegerVector remission_rate_indices = one_remission_info["remission_rate_indices"];
        IntegerVector from_state_indices = one_remission_info["from_state_indices"];
        
        // Pull proportions and indices
        int proportions_quantity_index = one_remission_info["proportions_quantity_index"];
        double *remission_proportions = quantities[proportions_quantity_index];
        List proportions_indices_for_from = one_remission_info["proportions_indices_for_from"];
        
        int n_from = one_remission_info["n_from"];
        int n_to_per_from = one_remission_info["n_to_per_from"];
        List to_indices_for_from = one_remission_info["to_indices_for_from"];
        
        
        // Set up for tracking
        List from_trackers = one_remission_info["from_trackers"];
        List to_trackers = one_remission_info["to_trackers"];
        bool need_to_track_from = from_trackers.length() > 0;
        bool need_to_track_to = to_trackers.length() > 0;
        
        if (need_to_track_from)
        {
            for (int i=0; i<n_infected; i++)
                scratch1[i] = 0;
        }
        
        if (need_to_track_to)
        {
            for (int i=0; i<n_uninfected; i++)
                scratch2[i] = 0;
        }
        
        // Scratch variables for calculations
        double remissions;
        double value;
        IntegerVector proportion_indices;
        IntegerVector to_indices;
        for (int from=0; from<n_from; from++)
        {
            remissions = infected[ from_state_indices[from] ] * remission_rates[ remission_rate_indices[from] ];
            dx_infected[from] -= remissions;
            
            proportion_indices = proportions_indices_for_from[from];
            to_indices = to_indices_for_from[from];
            
            for (int to=0; to<n_to_per_from; to++)
            {
                value = remissions * remission_proportions[ proportion_indices[to] ];
                dx_uninfected[ to_indices[to] ] += value;
                
                if (need_to_track_from)
                    scratch1[ from_state_indices[from] ] = value;
                
                if (need_to_track_to)
                    scratch2[ to_indices[to] ] += value;
            }
            
        }
        
        // Track if needed
        if (need_to_track_from)
        {
            do_tracking(from_trackers,
                        scratch1, //values
                        quantities,
                        dx.begin());
        }
        
        if (need_to_track_to)
        {
            do_tracking(to_trackers,
                        scratch2, //values
                        quantities,
                        dx.begin());
        }
    }
    
    if (debug)
        Rcout << "Done.\n"; 
    //------------------------//
    //-- FIXED STRATA SIZES --//
    //------------------------//
    
    
    // Decide if we need to keep strata sizes constant
    // And, if so, what strata we need to keep constant
    if (debug)
        Rcout << "Doing fixed strata...";    
    int apply_fixed_strata_index = -1;
    for (int i=0; i<fixed_strata_info.length(); i++)
    {
        List one_fixed_strata_info = fixed_strata_info[i];
        double applies_after_time = one_fixed_strata_info["applies_after_time"];
        double applies_before_time = one_fixed_strata_info["applies_before_time"];
        
        if (time >= applies_after_time && time <= applies_before_time)
        {
            bool fix = one_fixed_strata_info["fix_strata"];
            if (fix)
                apply_fixed_strata_index = i;
            
            break;
        }
    }
    
    
    if (apply_fixed_strata_index != -1)
    {
        List apply_fixed_strata_info = fixed_strata_info[apply_fixed_strata_index];
        
        // Pull settings
        int n_fixed_strata = apply_fixed_strata_info["n_fixed_strata"];
        int n_uninfected_compartments_per_fixed_stratum = apply_fixed_strata_info["n_uninfected_compartments_per_fixed_stratum"];
        int n_infected_compartments_per_fixed_stratum = apply_fixed_strata_info["n_infected_compartments_per_fixed_stratum"];
        
        List uninfected_indices_for_stratum = apply_fixed_strata_info["uninfected_indices_for_stratum"];
        List infected_indices_for_stratum = apply_fixed_strata_info["infected_indices_for_stratum"];
        
        // Declare scratch variables
        double total_dx_for_stratum;
        double total_pop_in_stratum;
        IntegerVector uninfected_indices;
        IntegerVector infected_indices;
        int index;
        
        // Loop through each stratum
        for (int stratum=0; stratum<n_fixed_strata; stratum++)
        {
            // STEP 1: Calculate the total dx and total pop in each stratum
            total_dx_for_stratum = 0;
            total_pop_in_stratum = 0;
            
            uninfected_indices = uninfected_indices_for_stratum[stratum];
            infected_indices = infected_indices_for_stratum[stratum];
            
            for (int i=0; i<n_uninfected_compartments_per_fixed_stratum; i++)
            {
                index = uninfected_indices[i];
                total_dx_for_stratum += dx_uninfected[index];
                total_pop_in_stratum += uninfected[index];
            }
            
            for (int i=0; i<n_infected_compartments_per_fixed_stratum; i++)
            {
                index = infected_indices[i];
                total_dx_for_stratum += dx_infected[index];
                total_pop_in_stratum += infected[index];
            }
            
            // STEP 2: Distribute the inverse of the total_dx_for_stratum 
            //         to dx_infected and dx_uninfected according to the size of each stratum
            if (total_pop_in_stratum != 0)
            {
                for (int i=0; i<n_uninfected_compartments_per_fixed_stratum; i++)
                {
                    index = uninfected_indices[i];
                    dx_uninfected[index] -= total_dx_for_stratum * uninfected[index] / total_pop_in_stratum;
                }
                
                for (int i=0; i<n_infected_compartments_per_fixed_stratum; i++)
                {
                    index = infected_indices[i];
                    dx_infected[index] -= total_dx_for_stratum * infected[index] / total_pop_in_stratum;
                }
            }
        }
    }
    if (debug)
        Rcout << "Done.\n";    
    
    //-------------------------//
    //-- TRACKED POPULATIONS --//
    //-------------------------//
    
    if (debug)
        Rcout << "Doing population trackers...";    
    for (int group=0; group<N_GROUPS; group++)
    {
        List trackers_for_group = population_trackers[group];
        if (trackers_for_group.length() > 0)
        {
            do_tracking(trackers_for_group,
                        sub_states_for_group[group], //values
                                            quantities,
                                            dx.begin());
        }
    }    
    if (debug)
        Rcout << "Done.\n";   
    //------------//
    //-- RETURN --//
    //------------//
    
    return (dx);
    
}


