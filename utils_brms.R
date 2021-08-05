#
# For licensing see accompanying LICENSE file.
# Copyright (C) 2020 Apple Inc. All Rights Reserved.
#

#' Build brms model formula 
#' @param input arguments 
#' @return brms model formula based on input argument specifications
#' 
build_model_formula = function(args) {
    if (args$model_type == 'main') {
        brms_formula = "log_growth_rate ~ 1 + log_popest + temperature_smooth_2 + mask_wear_always_frequently_combined_state + mobility_pc1_full_dat_avg_lag0_3_1 + mobility_pc1_full_dat_avg_lag0_3_2 + mobility_pc1_full_dat_avg_lag0_3_3 + mobility_pc1_full_dat_avg_lag0_3_4 + (1 + mobility_pc1_full_dat_avg_lag0_3_1 + mobility_pc1_full_dat_avg_lag0_3_2 + mobility_pc1_full_dat_avg_lag0_3_3 + mobility_pc1_full_dat_avg_lag0_3_4 | csa)"
    } else if (args$model_type == 'pitfall_collinear') {
        brms_formula = "log_growth_rate ~ 1 + log_popest + temperature_smooth_2 + mask_wear_always_frequently_combined_state + mobility_retail_and_recreation_1 + mobility_retail_and_recreation_2 + mobility_retail_and_recreation_3 + mobility_retail_and_recreation_4 + mobility_workplaces_1 + mobility_workplaces_2 + mobility_workplaces_3 + mobility_workplaces_4 + (1 + mobility_retail_and_recreation_1 + mobility_retail_and_recreation_2 + mobility_retail_and_recreation_3 + mobility_retail_and_recreation_4 + mobility_workplaces_1 + mobility_workplaces_2 + mobility_workplaces_3 + mobility_workplaces_4 | csa)"
    } else if (args$model_type == 'pitfall_overflexible_mobility') {
        brms_formula = "log_growth_rate ~ 1 + log_popest + temperature_smooth_2 + mask_wear_always_frequently_combined_state + mobility_pc1_full_dat_avg_lag0_3_1 + mobility_pc1_full_dat_avg_lag0_3_2 + mobility_pc1_full_dat_avg_lag0_3_3 + mobility_pc1_full_dat_avg_lag0_3_4 + mobility_pc1_full_dat_avg_lag0_3_5 + mobility_pc1_full_dat_avg_lag0_3_6 + mobility_pc1_full_dat_avg_lag0_3_7 + mobility_pc1_full_dat_avg_lag0_3_8 + mobility_pc1_full_dat_avg_lag0_3_9 + mobility_pc1_full_dat_avg_lag0_3_10 + mobility_pc1_full_dat_avg_lag0_3_11 + mobility_pc1_full_dat_avg_lag0_3_12 + (1 + mobility_pc1_full_dat_avg_lag0_3_1 + mobility_pc1_full_dat_avg_lag0_3_2 + mobility_pc1_full_dat_avg_lag0_3_3 + mobility_pc1_full_dat_avg_lag0_3_4 + mobility_pc1_full_dat_avg_lag0_3_5 + mobility_pc1_full_dat_avg_lag0_3_6 + mobility_pc1_full_dat_avg_lag0_3_7 + mobility_pc1_full_dat_avg_lag0_3_8 + mobility_pc1_full_dat_avg_lag0_3_9 + mobility_pc1_full_dat_avg_lag0_3_10 + mobility_pc1_full_dat_avg_lag0_3_11 + mobility_pc1_full_dat_avg_lag0_3_12 | csa)"
    } else if (args$model_type == 'pitfall_overflexible_temperature') {
        brms_formula = "log_growth_rate ~ 1 + log_popest + mask_wear_always_frequently_combined_state + temperature_smooth_2_1 + temperature_smooth_2_2 + temperature_smooth_2_3 + mobility_pc1_full_dat_avg_lag0_3_1 + mobility_pc1_full_dat_avg_lag0_3_2 + mobility_pc1_full_dat_avg_lag0_3_3 + (1 + temperature_smooth_2_1 + temperature_smooth_2_2 + temperature_smooth_2_3 + mobility_pc1_full_dat_avg_lag0_3_1 + mobility_pc1_full_dat_avg_lag0_3_2 + mobility_pc1_full_dat_avg_lag0_3_3 | csa)"
    } else if (args$model_type == 'clustering_none_mob_constant') {
        brms_formula = "log_growth_rate ~ 1 + log_popest + temperature_smooth_2 + mask_wear_always_frequently_combined_state + mobility_pc1_full_dat_avg_lag0_3"
    } else if (args$model_type == 'clustering_none_mob_tv') {
        brms_formula = "log_growth_rate ~ 1 + log_popest + temperature_smooth_2 + mask_wear_always_frequently_combined_state + mobility_pc1_full_dat_avg_lag0_3_1 + mobility_pc1_full_dat_avg_lag0_3_2 + mobility_pc1_full_dat_avg_lag0_3_3 + mobility_pc1_full_dat_avg_lag0_3_4 + (1 | csa)"
    } else if (args$model_type == 'clustering_region_mob_constant') {
        brms_formula = "log_growth_rate ~ 1 + log_popest + temperature_smooth_2 + mask_wear_always_frequently_combined_state + mobility_pc1_full_dat_avg_lag0_3 + (-1 + mobility_pc1_full_dat_avg_lag0_3 | region) + (1 | csa)"
    } else if (args$model_type == 'clustering_region_mob_tv') {
        brms_formula = "log_growth_rate ~ 1 + log_popest + temperature_smooth_2 + mask_wear_always_frequently_combined_state + mobility_pc1_full_dat_avg_lag0_3_1 + mobility_pc1_full_dat_avg_lag0_3_2 + mobility_pc1_full_dat_avg_lag0_3_3 + mobility_pc1_full_dat_avg_lag0_3_4 + (1 + mobility_pc1_full_dat_avg_lag0_3_1 + mobility_pc1_full_dat_avg_lag0_3_2 + mobility_pc1_full_dat_avg_lag0_3_3 + mobility_pc1_full_dat_avg_lag0_3_4 | region) + (1 | csa)"
    } else if (args$model_type == 'clustering_csa_mob_constant') {
        brms_formula = "log_growth_rate ~ 1 + log_popest + temperature_smooth_2 + mask_wear_always_frequently_combined_state + mobility_pc1_full_dat_avg_lag0_3 + (1 + mobility_pc1_full_dat_avg_lag0_3 | csa)"
    } else if (args$model_type == 'clustering_csa_mob_tv') {
        brms_formula = "log_growth_rate ~ 1 + log_popest + temperature_smooth_2 + mask_wear_always_frequently_combined_state + mobility_pc1_full_dat_avg_lag0_3_1 + mobility_pc1_full_dat_avg_lag0_3_2 + mobility_pc1_full_dat_avg_lag0_3_3 + mobility_pc1_full_dat_avg_lag0_3_4 + (1 + mobility_pc1_full_dat_avg_lag0_3_1 + mobility_pc1_full_dat_avg_lag0_3_2 + mobility_pc1_full_dat_avg_lag0_3_3 + mobility_pc1_full_dat_avg_lag0_3_4 | csa)"
    } else if (args$model_type == 'sg') {
        brms_formula = "log_growth_rate ~ 1 + log_popest + temperature_smooth_2 + mask_wear_always_frequently_combined_state + completely_home_prop_7dav_lag0_3_1 + completely_home_prop_7dav_lag0_3_2 + completely_home_prop_7dav_lag0_3_3 + completely_home_prop_7dav_lag0_3_4 + (1 + completely_home_prop_7dav_lag0_3_1 + completely_home_prop_7dav_lag0_3_2 + completely_home_prop_7dav_lag0_3_3 + completely_home_prop_7dav_lag0_3_4 | csa)"
    } else {
        stop(paste0("Model type ", args$model_type, " not defined"))
    }
    return(brms_formula)
}

#' Create time varying coefficient names 
#' @param input arguments 
#' @return list of new variable names for each time varying component. 
#' (e.g., for a three dimensional time varying basis and a variable x three new variables are created: x_1, x_2, x_3 corresponding to each time varying component.)
#' 
build_dyn_names = function(args) {
  dynamic_vars = args$dyn_vars
  if (is.null(args$knot_dates) & is.null(args$spline_dim)) {
    return(dynamic_vars)
  }
  if (!is.null(args$knot_dates)) {
    n_waves = length(args$knot_dates) + 1
  } else {
    n_waves = args$spline_dim
  }
  dyn_covs_spline_names = c()
  for (this_var in dynamic_vars) {
    dyn_covs_spline_names = c(dyn_covs_spline_names, paste0(this_var, "_", 1:n_waves))
  }
  return(dyn_covs_spline_names)
}

#' Prepare data for model fitting with brms 
#'  
#' @param input arguments 
#' 
#' @return 
#' Optionally download raw data from online repositories and impute missing values. 
#' Preprocessing: weekly aggregation, county-level exclusions, training/testing splits, variable standardization, constructs time varying basis representation
#' 
#' Returns a list of 
#'    1) training data 
#'    2) testing data
#'    3) location and scale parameters for each standarized variable
#'    4) basis representation for time varying coefficients 
#' 
prepare_data = function(args) {
    if (args$get_process_impute_data) {
      weekly_dat = load_process_estimate_impute_save_data(args$data_path)
    }
    weekly_dat = read_csv(args$data_path)
    weekly_dat = fips_exclusion_filter(weekly_dat, args)
    dat = data_split_and_transform(weekly_dat, args)
    dat = build_basis(dat, args)
    dat = add_variable_quantiles_to_splits(dat, args)
    dat = add_wave_ids_to_splits(dat)
}