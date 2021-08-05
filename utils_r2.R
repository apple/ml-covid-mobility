#
# For licensing see accompanying LICENSE file.
# Copyright (C) 2020 Apple Inc. All Rights Reserved.
#

#' Calculate R2 for specified grouping by sample
#' @param df_preds_data dataframe with predictions for each sample and with all grouping variables 
#' @param grouping_vars disaggregation variable 
#' @param response_var response variable to calculate residuals 
#' @return R2 values for each sample for each level of the grouping_vars
calculate_r2_across_samples_by_group = function(df_preds_data, grouping_vars, response_var = "log_growth_rate") {
  df_preds_data[["resid"]] = df_preds_data[[response_var]] - df_preds_data[["ypred"]]
  df_r2_by_group = df_preds_data %>% 
                      group_by_at(grouping_vars) %>% 
                      summarize(var_mu_s = var(ypred), var_rss = var(resid), r2 = var_mu_s / (var_mu_s + var_rss), n = n()) %>% 
                      ungroup()  
  return(df_r2_by_group)
}

#' Get model fitted values
#' @param fit brms fitted object
#' @param df dataset to calculate fitted values 
#' @param nsamples number of fitted values to draw from posterior (e.g. dim(df)[[1]] * nsamples)
#' @return df_pred tibble of predicted values aligned with input data
#' 
get_model_fitted_values = function(fit, df, nsamples) {
    ypreds = fitted(fit, df, summary = FALSE, allow_new_levels = TRUE, sample_new_levels = "uncertainty", nsamples = nsamples)   
    df_preds = bind_cols(data.frame(t(ypreds)) %>% tibble(), df) %>% pivot_longer(1:dim(ypreds)[[1]], names_to = "pred_sample", values_to = "ypred")    
  return(df_preds)
}

#' Calculate R2 over replicated experiments for different disaggregation variables
#' 
#' @param fit model fit object 
#' @param df data dataframe 
#' @param disaggregate_vars list of variables to disaggregate R^2 by, e.g. "region", "week", or "" for the whole dataset 
#' @param nsamples number of samples to extract from fitted object
#' @param lo_q lower quantile value
#' @param hi_q upper quantile value
#' 
#' @details R2 values for each disaggregation variable. 
#' @return tibble with summary stats from R2 samples
get_model_R2_metrics = function(fit,
                                df,
                                disaggregate_vars = list(""), 
                                nsamples = NULL, 
                                lo_q = .025, hi_q = .975) {

  n_disaggregations = length(disaggregate_vars)
  stopifnot(n_disaggregations > 0)
  r2_dat = vector("list", n_disaggregations)
  df_preds = get_model_fitted_values(fit, df, nsamples)
  # calculate per sample R2 for each disaggregation 
  for (var_i in 1:n_disaggregations) {
    var_x = disaggregate_vars[[var_i]]
    if (var_x[1] == "") {
        grouping_vars = c("pred_sample") # overall model fit
    } else {
        grouping_vars = c("pred_sample", var_x)  
    }
    df_r2 = calculate_r2_across_samples_by_group(df_preds, grouping_vars) 
    r2_dat[[var_i]] = df_r2
  } 
r2_by_var = vector("list", n_disaggregations)
for (var_i in 1:n_disaggregations) {
  var_x = disaggregate_vars[[var_i]]
  df_r2 = r2_dat[[var_i]]
  if (var_x[1] == "") { 
        df_tmp = df_r2 %>% 
                            summarize(qLow = quantile(r2, lo_q, na.rm = TRUE), 
                                      q50 = quantile(r2, 0.5, na.rm = TRUE), 
                                      avg = mean(r2, na.rm = TRUE),
                                      qHigh = quantile(r2, hi_q, na.rm = TRUE),
                                      ct = n[1]) %>% 
                            mutate(group_val = "all", type = "full_model") 
      } else {
        if (length(var_x) == 1) {
          df_tmp = df_r2 %>% group_by_at(var_x) %>% 
                                  summarize(qLow = quantile(r2, lo_q, na.rm = TRUE), 
                                            q50 = quantile(r2, 0.5, na.rm = TRUE), 
                                            avg = mean(r2, na.rm = TRUE),
                                            qHigh = quantile(r2, hi_q, na.rm = TRUE),
                                            ct = n[1]) %>% 
                                  rename(group_val = as.name(var_x)) %>% 
                                  mutate(type = var_x, group_val = as.character(group_val))           
        } else { # more than three variables => need to build out column names manually
          df_tmp = df_r2 %>% group_by_at(var_x) %>% 
                                  summarize(qLow = quantile(r2, lo_q, na.rm = TRUE), 
                                            avg = mean(r2, na.rm = TRUE),
                                            q50 = quantile(r2, 0.5, na.rm = TRUE), 
                                            qHigh = quantile(r2, hi_q, na.rm = TRUE),
                                            ct = n[1]) 
          df_tmp = as.data.frame(df_tmp)
          # build out column names 
          for (i in 1:length(var_x)) {
            if (i == 1) {
              df_tmp[, "group_val"] = df_tmp[, var_x[i]]
            } else {
              df_tmp[, "group_val"] = paste(df_tmp[, "group_val"], df_tmp[, var_x[i]], sep = "_")                      
            }
            df_tmp[,var_x[i]] = NULL
          }
          df_tmp$type = paste0(var_x, collapse = "_")
        }
      } # end summarize over different variable types
    r2_by_var[[var_i]] = df_tmp 
  } # end combine over configs
  df_r2_summary = bind_rows(r2_by_var) 
  return(df_r2_summary)
}

#' Compute disaggregated R2 from brms model object and save output in flat csv file 
#' @param args input arguments
#' @param save_dir directory to save R2 csvs 
#' @param dat input data used to train the model (needed for disaggregation variables not used in model fitting)
#' @param fit brms fitted model object 
#' 
#' @details Compute disaggregated R2 from brms model object and save output in flat csv file, optionally, applying post-processing for negative estimated time varying coefs
compute_and_save_r2 = function(args, save_dir, dat, fit) {
    if (args$postprocess) {
        dat = zero_covariates_with_negative_coefs(args, fit, dat)
    }
    df_r2_out = get_model_R2_metrics(fit, dat$weekly_dat, disaggregate_vars = list("", "last_date"))
    write_csv(df_r2_out, path = file.path(save_dir, "r2.csv"))
}
