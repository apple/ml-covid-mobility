#
# For licensing see accompanying LICENSE file.
# Copyright (C) 2020 Apple Inc. All Rights Reserved.
#

#' Extract and then combine coefs to get true effect at each group-level, rather
#' than the way the coefs are stored (as random effects / offsets from the next 
#' highest group, eg region coefs are offsets from the overall pop level fixed effect)
#' 
#' @param model Fitted model object
#' @param model_grouping Type of grouping for the vars you'd like to extract
#' @param var_names Vector of names for variables to extract

#' @return List of total coef effect by group. 
#'   Eg the "region" coefs = pop level FE + region REs
get_coef_draws_by_group = function(model, model_grouping, var_names) {
  #population level
  df_pop = model %>% 	gather_draws(`b_.*`, regex = TRUE)  %>% 
    mutate(term = str_sub(.variable, start = 3)) %>% 
    filter(term %in% var_names) %>%
    ungroup() %>%
    select(-c(".variable"))
  
  if (is.null(model_grouping)) {
    return(list("pop"=df_pop))
    
  } else if (model_grouping == "region") {
    df_region = model %>% gather_draws(r_region[region, term]) %>% 
      filter(term %in% var_names)
    
    df_region_pop_coefs = df_region %>% left_join(df_pop, by = c("term", ".chain", ".iteration", ".draw")) %>%
      mutate(.value = .value.x + .value.y) %>% 
      ungroup() %>% 
      select(-.variable, -.value.x, -.value.y) 
    
    return(list("pop"=df_pop, 
                "region"=df_region_pop_coefs))
    
   } else if (model_grouping == "csa") {
    df_csa = model %>% gather_draws(r_csa[csa, term]) %>% 
      filter(term %in% var_names)
    
    df_csa_pop_coefs = df_csa %>% left_join(df_pop, by = c("term", ".chain", ".iteration", ".draw")) %>%
      mutate(.value = .value.x + .value.y) %>% 
      ungroup() %>% 
      select(-.variable, -.value.x, -.value.y) 
    
    return(list("pop"=df_pop, 
                "csa"=df_csa_pop_coefs))
    
  } else {
    stop("Error! Unknown group type!")
  }

}

#' Helper function for time-varying coefficient models: 
#' get summary stats for the coefficients over time, converting them
#' from the way they're fit as low-dimenstional vectors.
#' 
#' @param spline_coef_draws Coefs for spline vars, from get_coef_draws_by_group
#' @param spline_basis Spline basis matrix
#' @param spline_dim Dimension of the spline basis
#' @param all_vars Which vars with tv-coefs to extract
#' @param unq_dates Dates to pass out with coef values 
#' @param lo_q Optional, lower quantile for summary
#' @param hi_q Optional, upper quantile for summary
#' 
#' @return tibble with summary stats for coefs back in time-space 
get_dyn_tvcoef_summaries = function(spline_coef_draws,
                                     spline_basis,
                                     spline_dim,
                                     all_vars,
                                     unq_dates,
                                     lo_q=.025,
                                     hi_q=.975) {
  
  tv_coefs = list()
  for (dyn_var in all_vars) {
    this_spline_coefs = spline_coef_draws %>% 
      filter(term %in% paste0(dyn_var,"_",1:spline_dim)) %>%
      pivot_wider(names_from="term", values_from=".value") %>%
      select(paste0(dyn_var,"_",1:spline_dim))

    this_tv_coefs = spline_basis %*% t(this_spline_coefs) #T x num_samps
       
    this_tv_coef_summ_dat = tibble(
      var = dyn_var,
      date = unq_dates,
      mean = apply(this_tv_coefs, 1, mean),
      lo = apply(this_tv_coefs, 1, function(x) {quantile(x,lo_q)}),
      hi = apply(this_tv_coefs, 1, function(x) {quantile(x,hi_q)})
    )
    tv_coefs[[dyn_var]] = this_tv_coef_summ_dat
  }
  tv_coefs = bind_rows(tv_coefs)
  
  return(tv_coefs)
}


#' Helper func for time-varying coefficient models, to convert samples in spline-basis space,
#' where the coefs are dim spline_dim, into fitted values given training data.
#' 
#' @param spline_coef_draws A subset of coef samples of interest, from get_coef_draws_by_group function
#' @param static_coef_draws Coef samples for static variables of interest (eg temp, population)
#' @param model The actual fitted model object
#' @param data_subset Subset of data where you'd like fitted vals at
#' @param spline_dim Dimension of the spline
#' @param dyn_vars Which vars with tv-coefs to extract fitted vals for (eg mobility)
#' @param static_vars Which vars with static coefs to get fitted values for (eg temp)
#' @param lo_q Optional, lower quantile for summary
#' @param hi_q Optional, upper quantile for summary
#'
#' @return Tibble with summary stats for fitted vals at each point in time, eg 
#' for each week returns mean & CIs for fitted vals across data in input
#' data_subset, from each week.  Run this func in a loop over data subsets 
#' to get fitteds from different regions/subregions/CSAs.
get_dyn_vars_fitted_summaries = function(spline_coef_draws,
                                         static_coef_draws,
                                         model,
                                         data_subset,
                                         spline_dim,
                                         dyn_vars,
                                         static_vars,
                                         lo_q=.025,hi_q=.975) {
  
  outcome_var = "log_growth_rate" 
  fitted_vals_summ_dat = list()
  
  #get fitted values from each dynamic variable first
  for (dyn_var in dyn_vars) {
    this_var_spline_names = paste0(dyn_var,"_",1:spline_dim)
    
    this_spline_coefs = spline_coef_draws %>% 
      filter(term %in% this_var_spline_names) %>%
      pivot_wider(names_from="term", values_from=".value") %>%
      select(this_var_spline_names)
    
    var_dat = data_subset %>% select(this_var_spline_names) #N x spline_dim
    fitted_vals = as.matrix(var_dat) %*% t(this_spline_coefs) #N x num.samps
     
    unq_dates = sort(unique(data_subset$last_date))
    this_fitted_vals_summ = list()
    for (i in 1:length(unq_dates)) {
      d = unq_dates[i]
      inds = data_subset$last_date==d
      all_vals = as.vector(fitted_vals[inds,])
      this_fitted_vals_summ[[i]] = tibble(
        var = dyn_var,
        date = d,
        mean = mean(all_vals),
        lo = quantile(all_vals,lo_q),
        hi = quantile(all_vals,hi_q), 
        type = "fitted_scale"
      )
    }
    fitted_vals_summ_dat[[dyn_var]] = bind_rows(this_fitted_vals_summ)
  }
  
  #static vars next 
  for (static_var in static_vars) {
    this_static_coefs = static_coef_draws %>% 
      filter(term == static_var) %>%
      select(.value) #num_samps x 1
    
    var_dat = data_subset %>% select(static_var) #N x 1
    fitted_vals = outer(as.matrix(var_dat)[,1], as.matrix(this_static_coefs)[,1]) #N x num_samps
    
    unq_dates = sort(unique(data_subset$last_date))
    this_fitted_vals_summ = list()
    for (i in 1:length(unq_dates)) {
      d = unq_dates[i]
      inds = data_subset$last_date==d
      all_vals = as.vector(fitted_vals[inds,])
      this_fitted_vals_summ[[i]] = tibble(
        var = static_var,
        date = d,
        mean = mean(all_vals),
        lo = quantile(all_vals,lo_q),
        hi = quantile(all_vals,hi_q),
        type = "fitted_scale"
      )
    }
    fitted_vals_summ_dat[[static_var]] = bind_rows(this_fitted_vals_summ)
  }
  
  unq_dates = sort(unique(data_subset$last_date))
  this_outcomes_summ = list()
  for (i in 1:length(unq_dates)) {
    d = unq_dates[i]
    inds = data_subset$last_date==d
    all_vals = as.vector(data_subset[inds,] %>% select(outcome_var) %>% unlist())
    this_outcomes_summ[[i]] = tibble(
      var = paste0("outcome: ",outcome_var),
      date = d,
      mean = mean(all_vals),
      lo = quantile(all_vals,lo_q),
      hi = quantile(all_vals,hi_q),
      type = "fitted_scale"
    )
  }
  fitted_vals_summ_dat[[outcome_var]] = bind_rows(this_outcomes_summ)
  
  ### and lastly, return the overall total fitted values 
  total_fitteds = fitted(model, data_subset, summary=F, allow_new_levels=T, sample_new_levels="uncertainty") # num_samps x num_obs
  this_fitteds_summ = list()
  for (i in 1:length(unq_dates)) {
    d = unq_dates[i]
    inds = data_subset$last_date==d
    all_vals = as.vector(total_fitteds[,inds])
    this_fitteds_summ[[i]] = tibble(
      var = "total_fitted",
      date = d,
      mean = mean(all_vals),
      lo = quantile(all_vals,lo_q),
      hi = quantile(all_vals,hi_q),
      type = "fitted_scale"
    )
  }
  fitted_vals_summ_dat[["total_fitted"]] = bind_rows(this_fitteds_summ)
  
  fitted_vals_summ_dat = bind_rows(fitted_vals_summ_dat)
  return(fitted_vals_summ_dat)
}

