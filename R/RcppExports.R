# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

overwrite_arr <- function(dst, dst_indices, src, src_indices) {
    .Call(`_jheem2_overwrite_arr`, dst, dst_indices, src, src_indices)
}

add_to_arr <- function(dst, dst_indices, src, src_indices) {
    .Call(`_jheem2_add_to_arr`, dst, dst_indices, src, src_indices)
}

overwrite_arr_with_scalar <- function(dst, overwrite_with) {
    .Call(`_jheem2_overwrite_arr_with_scalar`, dst, overwrite_with)
}

add_scalar_to_arr <- function(dst, to_add) {
    .Call(`_jheem2_add_scalar_to_arr`, dst, to_add)
}

do_array_overwrite <- function(dst_array, src_array, dimension_values) {
    .Call(`_jheem2_do_array_overwrite`, dst_array, src_array, dimension_values)
}

do_expand_array <- function(dst_array, src_array) {
    .Call(`_jheem2_do_expand_array`, dst_array, src_array)
}

do_get_expand_indices <- function(dst_array, src_dim_names, index_from) {
    .Call(`_jheem2_do_get_expand_indices`, dst_array, src_dim_names, index_from)
}

do_access_overwrite <- function(dst, src, dst_indices, src_indices) {
    .Call(`_jheem2_do_access_overwrite`, dst, src, dst_indices, src_indices)
}

do_access_add <- function(dst, src, dst_indices, src_indices) {
    .Call(`_jheem2_do_access_add`, dst, src, dst_indices, src_indices)
}

do_access_subtract <- function(dst, src, dst_indices, src_indices) {
    .Call(`_jheem2_do_access_subtract`, dst, src, dst_indices, src_indices)
}

do_access_multiply <- function(dst, src, dst_indices, src_indices) {
    .Call(`_jheem2_do_access_multiply`, dst, src, dst_indices, src_indices)
}

do_access_divide <- function(dst, src, dst_indices, src_indices) {
    .Call(`_jheem2_do_access_divide`, dst, src, dst_indices, src_indices)
}

get_obs_error_correlation_matrix <- function(cor_mat, n_obs, location, year, stratum, source, details, correlation_different_location, correlation_different_year, correlation_different_strata, correlation_different_source, correlation_same_source_different_details, is_autoregressive_one) {
    .Call(`_jheem2_get_obs_error_correlation_matrix`, cor_mat, n_obs, location, year, stratum, source, details, correlation_different_location, correlation_different_year, correlation_different_strata, correlation_different_source, correlation_same_source_different_details, is_autoregressive_one)
}

get_multiplier_correlation_matrix <- function(cor_mat, n_obs, year, correlation_different_year, is_autoregressive_one) {
    .Call(`_jheem2_get_multiplier_correlation_matrix`, cor_mat, n_obs, year, correlation_different_year, is_autoregressive_one)
}

compute_dx <- function(state, time, settings, quantity_scratch_vector, scratch_vector, quantities_info, natality_info, mortality_info, transitions_info, infections_info, remission_info, fixed_strata_info, population_trackers) {
    .Call(`_jheem2_compute_dx`, state, time, settings, quantity_scratch_vector, scratch_vector, quantities_info, natality_info, mortality_info, transitions_info, infections_info, remission_info, fixed_strata_info, population_trackers)
}

apply_foregrounds <- function(values, value_times, after_values, times_to_apply_to, foregrounds, indices_per_effect_per_foreground, scale) {
    .Call(`_jheem2_apply_foregrounds`, values, value_times, after_values, times_to_apply_to, foregrounds, indices_per_effect_per_foreground, scale)
}

do_calculate_quantity_background_value <- function(quantity, missing_times, specification_metadata, location, engine, check_consistency, error_prefix) {
    .Call(`_jheem2_do_calculate_quantity_background_value`, quantity, missing_times, specification_metadata, location, engine, check_consistency, error_prefix)
}

do_collapse_according_to_indices <- function(arr, large_indices, small_indices, small_n) {
    .Call(`_jheem2_do_collapse_according_to_indices`, arr, large_indices, small_indices, small_n)
}

do_interpolate <- function(values, value_times, desired_times) {
    .Call(`_jheem2_do_interpolate`, values, value_times, desired_times)
}

interpolate_values_when_do_not_apply <- function(values, times, value_applies_for_time) {
    .Call(`_jheem2_interpolate_values_when_do_not_apply`, values, times, value_applies_for_time)
}

do_calculate_outcome_numerator_and_denominator <- function(outcome_name, ode_results, engine, check_consistency) {
    invisible(.Call(`_jheem2_do_calculate_outcome_numerator_and_denominator`, outcome_name, ode_results, engine, check_consistency))
}

calculate_main_effect_indices <- function(target_dim_names, alpha_dimensions, alpha_dim_values) {
    .Call(`_jheem2_calculate_main_effect_indices`, target_dim_names, alpha_dimensions, alpha_dim_values)
}

calculate_two_way_interaction_indices <- function(target_dim_names, alpha_dimension1, alpha_dim1_values, alpha_dimension2, alpha_dim2_values) {
    .Call(`_jheem2_calculate_two_way_interaction_indices`, target_dim_names, alpha_dimension1, alpha_dim1_values, alpha_dimension2, alpha_dim2_values)
}

calculate_three_way_interaction_indices <- function(target_dim_names, alpha_dimension1, alpha_dim1_values, alpha_dimension2, alpha_dim2_values, alpha_dimension3, alpha_dim3_values) {
    .Call(`_jheem2_calculate_three_way_interaction_indices`, target_dim_names, alpha_dimension1, alpha_dim1_values, alpha_dimension2, alpha_dim2_values, alpha_dimension3, alpha_dim3_values)
}

calculate_four_way_interaction_indices <- function(target_dim_names, alpha_dimension1, alpha_dim1_values, alpha_dimension2, alpha_dim2_values, alpha_dimension3, alpha_dim3_values, alpha_dimension4, alpha_dim4_values) {
    .Call(`_jheem2_calculate_four_way_interaction_indices`, target_dim_names, alpha_dimension1, alpha_dim1_values, alpha_dimension2, alpha_dim2_values, alpha_dimension3, alpha_dim3_values, alpha_dimension4, alpha_dim4_values)
}

do_add_alphas_to_arr <- function(arr, dims, alpha_values, alpha_dims, alpha_indices) {
    invisible(.Call(`_jheem2_do_add_alphas_to_arr`, arr, dims, alpha_values, alpha_dims, alpha_indices))
}

do_add_or_set_two_way_interaction_alphas_to_arr <- function(arr, dims, dim1, dim1_values, dim2, dim2_values, values, add) {
    invisible(.Call(`_jheem2_do_add_or_set_two_way_interaction_alphas_to_arr`, arr, dims, dim1, dim1_values, dim2, dim2_values, values, add))
}

do_add_or_set_three_way_interaction_alphas_to_arr <- function(arr, dims, dim1, dim1_values, dim2, dim2_values, dim3, dim3_values, values, add) {
    invisible(.Call(`_jheem2_do_add_or_set_three_way_interaction_alphas_to_arr`, arr, dims, dim1, dim1_values, dim2, dim2_values, dim3, dim3_values, values, add))
}

do_add_or_set_four_way_interaction_alphas_to_arr <- function(arr, dims, dim1, dim1_values, dim2, dim2_values, dim3, dim3_values, dim4, dim4_values, values, add) {
    invisible(.Call(`_jheem2_do_add_or_set_four_way_interaction_alphas_to_arr`, arr, dims, dim1, dim1_values, dim2, dim2_values, dim3, dim3_values, dim4, dim4_values, values, add))
}

do_project_logistic_tail <- function(intercept, slope, slope_with_future, future_slope, future_slope_after_year, span, min, max, logistic_after_value, anchor_year, years) {
    .Call(`_jheem2_do_project_logistic_tail`, intercept, slope, slope_with_future, future_slope, future_slope_after_year, span, min, max, logistic_after_value, anchor_year, years)
}

generate_lag_matrix_indices <- function(years, locations, strata, sources, n) {
    .Call(`_jheem2_generate_lag_matrix_indices`, years, locations, strata, sources, n)
}

apply_lag_to_vector <- function(mean, L, output, n) {
    .Call(`_jheem2_apply_lag_to_vector`, mean, L, output, n)
}

apply_lag_to_matrix <- function(input, L, output, n) {
    .Call(`_jheem2_apply_lag_to_matrix`, input, L, output, n)
}

generate_transformation_matrix_indices <- function(transformation_matrix, m, n) {
    .Call(`_jheem2_generate_transformation_matrix_indices`, transformation_matrix, m, n)
}

OLD_generate_transformation_matrix_indices <- function(transformation_matrix, m, n) {
    .Call(`_jheem2_OLD_generate_transformation_matrix_indices`, transformation_matrix, m, n)
}

generate_transformation_matrix_row_oriented_indices <- function(transformation_matrix, m, n) {
    .Call(`_jheem2_generate_transformation_matrix_row_oriented_indices`, transformation_matrix, m, n)
}

get_basic_likelihood_mean <- function(sim_numerator, transformation_matrix_row_oriented_indices, m, mean) {
    .Call(`_jheem2_get_basic_likelihood_mean`, sim_numerator, transformation_matrix_row_oriented_indices, m, mean)
}

get_basic_likelihood_sigma <- function(sim_numerator, sim_denominator, transformation_matrix_indices, measurement_error_cov_matrix, m, sigma, Poisson) {
    .Call(`_jheem2_get_basic_likelihood_sigma`, sim_numerator, sim_denominator, transformation_matrix_indices, measurement_error_cov_matrix, m, sigma, Poisson)
}

character_vectors_overlap <- function(x, y) {
    .Call(`_jheem2_character_vectors_overlap`, x, y)
}

sorted_vectors_overlap <- function(x, y) {
    .Call(`_jheem2_sorted_vectors_overlap`, x, y)
}

setdiff_sorted_vectors <- function(x, y) {
    .Call(`_jheem2_setdiff_sorted_vectors`, x, y)
}

union_sorted_vectors <- function(vectors) {
    .Call(`_jheem2_union_sorted_vectors`, vectors)
}

intersect_sorted_vectors <- function(vectors) {
    .Call(`_jheem2_intersect_sorted_vectors`, vectors)
}

interpolate_sorted_vectors <- function(v1, v2, indicator1, indicator2) {
    .Call(`_jheem2_interpolate_sorted_vectors`, v1, v2, indicator1, indicator2)
}

any_overlap_character <- function(c1, c2) {
    .Call(`_jheem2_any_overlap_character`, c1, c2)
}

any_setdiff_character <- function(x, y) {
    .Call(`_jheem2_any_setdiff_character`, x, y)
}

get_nested_proportion_likelihood_components <- function(p, n, year_metalocation_n_multipliers, year_metalocation_n_multiplier_sd, year_metalocation_p_bias, year_metalocation_p_sd, metalocation_p_correlation, metalocation_n_multiplier_correlation, year_metalocation_to_year_obs_n_mapping, obs_n, obs_n_cov_matrices, year_metalocation_to_year_condition_on_location_mask, year_metalocation_to_year_condition_on_location_mapping, year_metalocation_to_year_obs_location_mask, year_metalocation_to_year_obs_location_mapping, year_loc_stratum_to_obs_mapping, year_metalocation_to_obs_mapping, obs_year_index, obs_p, obs_error) {
    .Call(`_jheem2_get_nested_proportion_likelihood_components`, p, n, year_metalocation_n_multipliers, year_metalocation_n_multiplier_sd, year_metalocation_p_bias, year_metalocation_p_sd, metalocation_p_correlation, metalocation_n_multiplier_correlation, year_metalocation_to_year_obs_n_mapping, obs_n, obs_n_cov_matrices, year_metalocation_to_year_condition_on_location_mask, year_metalocation_to_year_condition_on_location_mapping, year_metalocation_to_year_obs_location_mask, year_metalocation_to_year_obs_location_mapping, year_loc_stratum_to_obs_mapping, year_metalocation_to_obs_mapping, obs_year_index, obs_p, obs_error)
}

apply_ontology_mapping <- function(src, dst, from_values, to_values, na_rm) {
    .Call(`_jheem2_apply_ontology_mapping`, src, dst, from_values, to_values, na_rm)
}

get_ontology_mapping_matrix <- function(src_dim_names, dst_dim_names, dst, from_values, to_values) {
    .Call(`_jheem2_get_ontology_mapping_matrix`, src_dim_names, dst_dim_names, dst, from_values, to_values)
}

get_ontology_mapping_indices <- function(src_dim_names, dst_dim_names, from_values, to_values) {
    .Call(`_jheem2_get_ontology_mapping_indices`, src_dim_names, dst_dim_names, from_values, to_values)
}

do_get_reverse_indices_from_forward <- function(forward_indices, n_from) {
    .Call(`_jheem2_do_get_reverse_indices_from_forward`, forward_indices, n_from)
}

populate_outcomes_array <- function(desired_times, char_desired_times, n_per_time, new_values, new_times, old_values, old_times, prior_sim_index) {
    .Call(`_jheem2_populate_outcomes_array`, desired_times, char_desired_times, n_per_time, new_values, new_times, old_values, old_times, prior_sim_index)
}

do_optimized_get <- function(numerators, denominators, info_by_outcome, n_to_per_outcome, avoid_infinite) {
    .Call(`_jheem2_do_optimized_get`, numerators, denominators, info_by_outcome, n_to_per_outcome, avoid_infinite)
}

get_year_indices_for_optimized_info <- function(outcome_years, target_years, n_before_year_dimension, n_after_year_dimension) {
    .Call(`_jheem2_get_year_indices_for_optimized_info`, outcome_years, target_years, n_before_year_dimension, n_after_year_dimension)
}

