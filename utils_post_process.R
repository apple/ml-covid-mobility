#
# For licensing see accompanying LICENSE file.
# Copyright (C) 2020 Apple Inc. All Rights Reserved.
#

## Utils to post process unconstrained piecewise constant models 
## so that all estimated coefs are >= 0 

#' Threshold estimated tv coefs so that coef <- max(0, coef) 
#' @param coefs in tibble 
#' @param coef_names to apply transform 
#' @return tibble of thresholded coefs for all coefs in coef_names, i.e. coef <- max(0, coef) 
positive_part_coefs = function(coefs, coef_names) {
  coefs = coefs %>% filter(term %in% coef_names) %>% mutate(.value = pmax(.value, 0))
  return(coefs)
}

#' Zero out covariate values where the estimated coefs is <= 0 in at least perc_mcmc_negative_thresh percent of the posterior samples
#' @param args command line args 
#' @param fit pwc model fit object
#' @param split data split containing covariates that need to be zero'd out
#' @return dat object with updated weekly_dat tibble containing zero'd out covariate values for coefs where the est. coefs is negative for at least perc_mcmc_negative_thresh percent of the posterior samples
zero_covariates_with_negative_coefs = function(args, fit, dat, perc_mcmc_negative_thresh = 0.5) {
  dyn_covs_spline_names = build_dyn_names(args)
  dyn_draws = get_coef_draws_by_group(fit, args$dyn_grouping, dyn_covs_spline_names)
  group = if_else(is.null(args$dyn_grouping), "pop", args$dyn_grouping)
  negative_mob_coefs_group = dyn_draws[[group]] %>% 
    mutate(neg_coef = 1 * (.value < 0)) %>% 
    group_by_at(c(group, "term")) %>% 
    summarize(perc_mcmc_negative = mean(neg_coef)) %>% 
    ungroup()  %>% filter(perc_mcmc_negative >= perc_mcmc_negative_thresh) %>% select(all_of(group), term) %>% distinct()  

  augmented_knot_dates = c(as.Date('2000-01-01'), as.Date(args$knot_dates), as.Date('2100-01-01'))
  df = dat$weekly_dat
  for (wave_i in 1:(length(args$knot_dates) + 1)) {
      wave_mob = paste0(args$dyn_vars, "_", wave_i)
      group_to_zero = unique((negative_mob_coefs_group %>% filter(term == wave_mob))[[group]])
      ind = (df$last_date <= augmented_knot_dates[wave_i + 1]) * (df$last_date > augmented_knot_dates[wave_i]) * (df[[group]] %in% group_to_zero) 
      df[ind == 1, wave_mob] = 0 
    }
  dat$weekly_dat = df
  return(dat)
} 
