# organization
# we have our instructions
# they tell us our data (common ontology) stratifications
# from that we can determine our model stratifications and therefore strata

# things that are organized by model stratum:
# - List year_metalocation_n_multipliers
# - List year_metalocation_n_multiplier_sd
# - List year_metalocation_p_bias
# - List year_metalocation_p_sd
# - List obs_n
# - List year_metalocation_to_year_obs_n_mapping
# - List obs_n_plus_conditioned_error_variances
# - List year_loc_stratum_to_obs_mapping
# - List year_metalocation_to_obs_mapping

# when I pull obs.p, I'll see stratifications I got no data for. I could also see what strata I got no data for, which could be more.
# with my obs.p, I'm going to want a mapping that goes from the model strata to the obs.
# that will be year_loc_stratum_to_obs_mapping.
# actually, obs.p will b the vector organized by metalocation somehow?
# For each model stratum, we'll know which single observation corresponds to a year/metalocation pair.


# --- get locations that have data for the outcome in the years in question --- #
# --- pull observations of p for each requested stratum, throwing out NAs and keeping track of what stratifications are left --- #
# --- determine common ontology based on pulled data and mappings created --- #
# --- find metalocations based on which locations had data --- #
# --- create obs.p by aggregating data from locations that are part of the same metalocation, leaving a vector with coordinates year * metalocation * stratum? --- #
#    --- we'll have to know which model strata correspond to an observation
# --- find n-multipliers by pulling n data for each model stratum and taking ratios vs. msa by metalocation
# --- find obs.n by pulling n data for each model stratum, sometimes filling in missing data (somehow)
# --- ... more mappings?

# when I have my metadata from the first pull, I should keep track of not only the stratum name, but also the stratification (since it will be our keep.dimensions during the n.multipliers pulls)
# Or rather, I can have an accompanying list that says the keep dimensions for each member of the metadata.
# then I can aggregate by metalocation using a series of masks. I could get a subset of the metadata and accompanying lists by metalocation. Or I could just copy the metadata and replace location with metalocation name.
# aggregation might not need that. Or maybe it does. Maybe it's easier to keep track of what's left after aggregation if I just make a new column that has the stratification in the same way that I currently have the stratum.
# then, after aggregation, it's still there. Then I can make an accompanying list of keep dimensions and another of corresponding model stratum. Then I can make a list with each model stratum and its observation(s) (if year).
