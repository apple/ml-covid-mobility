#
# For licensing see accompanying LICENSE file.
# Copyright (C) 2020 Apple Inc. All Rights Reserved.
#

#' Exclude FIPS based on hard coded combination of log-growth-rate values and population size 
#' @param weekly_dat imputed data  
#' @param args input args 
#' @param LOG_GROWTH_RATE_HARD_FILTER max(abs(LGR)) to allow in a county
#' @param LOG_GROWTH_RATE_SOFT_FILTER max(abs(LGR)) to allow in a county if population is less than POP_FILTER
#' @param POP_FILTER population level for soft filter to apply 
#' @return weekly_dat with any fips that violate the above conditions removed 
fips_exclusion_filter = function(weekly_dat, args, LOG_GROWTH_RATE_HARD_FILTER = 2,
                               LOG_GROWTH_RATE_SOFT_FILTER = 1.5,
                               POP_FILTER = 50000) {
  # Log growth rate filter 
  fips_in = (weekly_dat %>% filter(complete.cases(.)) %>% group_by(fips, log_popest) %>%
                                 summarize(extreme_value = max(abs(log_growth_rate))) %>%
                                 filter(extreme_value < LOG_GROWTH_RATE_HARD_FILTER) %>%
                                 filter(!(extreme_value > LOG_GROWTH_RATE_SOFT_FILTER && log_popest < log(POP_FILTER)))
                 )$fips

  print(paste0("Total counties in dataset = ", length(unique(weekly_dat$fips)), " . After exclusion total = ", length(fips_in)))

  weekly_dat = weekly_dat %>% filter(fips %in% fips_in)

  weekly_dat = weekly_dat %>% ungroup() %>%
                        select(fips,
                               county_state,
                               csa,
                               state,
                               subregion,
                               region,
                               last_date,
                               week,
                               log_growth_rate,
                               average_temperature,
                               mobility_grocery_and_pharmacy,
                               mobility_residential,
                               mobility_transit_stations,
                               mobility_parks,
                               mobility_retail_and_recreation,
                               mobility_workplaces,
                               completely_home_prop_7dav,
                               mask_wear_always_frequently_combined_state,
                               log_popest)
  return(weekly_dat)
}


#' Construct 1d summary of mobility variables 
#' @param weekly_dat list containing training and testing splits 
#' @return weekly_dat tibble with new 1d summary 
#' @details Create new mobility variable = first principal component of all mobility variables using full training dataset, constrained so that mobility_workplace weights > 0 
build_first_pc_whole_data = function(weekly_dat) {
	df = weekly_dat %>% 
            select(mobility_grocery_and_pharmacy, mobility_parks, mobility_residential, mobility_retail_and_recreation, mobility_transit_stations, mobility_workplaces) %>% 
            data.frame()

	df = (df - matrix(colMeans(df), ncol = dim(df)[[2]], nrow = dim(df)[[1]], byrow = TRUE)) 
	stopifnot(sum(abs(colMeans(df)) > 1e-10) == 0)

	pZ = prcomp(df, center = FALSE, scale = FALSE)
	perc_explained = summary(pZ)$importance[3,1]
	print(paste0("Percent of variation explained with first component: ", perc_explained * 100, " %"))
	# force sign to be positive for workplace mobility 
	if (pZ$rotation[6, 1] < 0) {
	  sgn_fix = -1
	} else { 
	  sgn_fix = 1      
	}
	pc1_train = sgn_fix * as.matrix(df) %*% pZ$rotation[ , 1]
	weekly_dat = cbind(weekly_dat, data.frame(mobility_pc1_full_dat = pc1_train))
	stopifnot(sum(abs(as.numeric(abs(pc1_train)) -  abs(pZ$x[ , 1])) > 1e-10) == 0)
	return(weekly_dat)
}



#' Augment data with additional features, split into train/test, and any other data preprocessing 
#' @param weekly_dat filtered weekly data 
#' @param args input args 
#' @return fully processed (standarized and smoothed for select covariates, 1d summary for mobility variables, remaining missing values removed) tibble with additional variables 
data_split_and_transform = function(weekly_dat, args) {
  dat_loc_scales = standardize_variables(weekly_dat, exclude_cont_vars = c("log_growth_rate", "last_date", "week", "csa", "mask_wear_always_frequently_combined_state"))
  weekly_dat = dat_loc_scales$df
  weekly_dat = build_first_pc_whole_data(weekly_dat)

  df_tmp = weekly_dat %>%
    select(fips, last_date, mobility_pc1_full_dat, completely_home_prop_7dav, average_temperature) %>% 
    group_by(fips) %>%
    arrange(last_date) %>%
    mutate(
      temperature_smooth_2 = moving_average(average_temperature, 3),
      mobility_pc1_full_dat_avg_lag0_3 = moving_average(mobility_pc1_full_dat, 4),
      completely_home_prop_7dav_lag0_3 = moving_average(completely_home_prop_7dav, 4)
    ) %>%
    ungroup() %>%
    select(-mobility_pc1_full_dat, -average_temperature, -completely_home_prop_7dav)

  #make sure no NA's introduced from moving averages
  stopifnot(sum(is.na(df_tmp)) == 0 )
  weekly_dat = weekly_dat %>% left_join(df_tmp, by = c("fips", "last_date")) %>% filter(complete.cases(.))
  
  return(list(dat=weekly_dat, loc_scale_factors=dat_loc_scales$loc_scale_factors))
}

#' Standardize variables 
#' 
#' @param df of covariates 
#' @param exclude_cont_vars exclude these continuous variables from standardization 
#' 
#' @return list of 1) df of mean 0 and scaled covariates. 2) df of variables and associated location/scale transformations
#' 
#' @details 
#' For all continuous variables, scale by the mean and divide by 2 * sd.
#' 
standardize_variables = function(df, exclude_cont_vars = NULL) {
	continuous_variables = sapply(df, is.double)
	continuous_variables = names(continuous_variables[continuous_variables])
	inds = !(continuous_variables %in% exclude_cont_vars)
	continuous_variables = continuous_variables[inds]

	means = sapply(df[,continuous_variables], mean)
	sds = sapply(df[, continuous_variables], sd)
	for (var in continuous_variables) {
		df[, var] = (df[, var] - means[var]) / (2 * sds[var])	
	}
	return(list(df = df, loc_scale_factors = data.frame(name = continuous_variables, loc_shift = means, scale_shift = 2 * sds)))
}

#' Helper function to get wave ids for a set of dates, given some knots and a function to add in the wave ids during loading of a train/test split.
#' @param all_dates list of dates
#' @param knot_dates p.w. constant knot dates
#' @return list of wave ids for each all_dates
get_wave_ids = function(all_dates, knot_dates) {
  n_knots = length(knot_dates)
  augmented_knot_dates = c(as.Date('2000-01-01'), as.Date(knot_dates), as.Date('2100-01-01'))
    wave_ids = rep(NA, length(all_dates))
  for (wave in 1:(n_knots+1)) {
    wave_inds = all_dates <= augmented_knot_dates[wave + 1] & all_dates > augmented_knot_dates[wave]
    wave_ids[wave_inds] = wave
  }
  return(wave_ids)
}

#' Helper function to add wave ids to splits 
#' @param dat list of weekly data, basis functions, unique dates, and standarization params
#' @param default_knotdates default pw constant knots 
#' @return updated data structure with wave ids column in weekly data
add_wave_ids_to_splits = function(dat, default_knotdates = c('2020-05-23','2020-08-22','2020-11-21')) {
  dat$weekly_dat$wave = get_wave_ids(dat$weekly_dat$last_date, default_knotdates)
  return(dat)
}

#' Helper function to add in variables based on quantiles of covariates, for model subgroup performance.
#'
#' @param dat list of weekly data, basis functions, unique dates, and standarization params
#' @param args input args 
#' @return updated data structure with population and mobility quantiles 
add_variable_quantiles_to_splits = function(dat, args) {
  weekly_dat = dat$weekly_dat
  #un-transform populations for getting quantile groups 
  fips_pops = weekly_dat %>%
    mutate(popest = exp(
      dat$loc_scale_factors["log_popest","scale_shift"]*log_popest +
        dat$loc_scale_factors["log_popest","loc_shift"])
    ) %>%
    filter(week == max(week)) %>%
    select(fips,popest)
  #manual county population bins
  fips_pop_quantiles_names = c("< 25k", "25k - 100k", "100k - 250k", "> 250k")
  fips_pops$fips_pop_quantiles = 0
  fips_pops$fips_pop_quantiles[fips_pops$popest < 25000] = "< 25k"
  fips_pops$fips_pop_quantiles[fips_pops$popest >= 25000 & fips_pops$popest < 100000] = "25k - 100k"
  fips_pops$fips_pop_quantiles[fips_pops$popest >= 100000 & fips_pops$popest < 250000] = "100k - 250k"
  fips_pops$fips_pop_quantiles[fips_pops$popest >= 250000] = "> 250k"
  fips_pops$fips_pop_quantiles = factor(fips_pops$fips_pop_quantiles, levels = fips_pop_quantiles_names)
  weekly_dat = weekly_dat %>% left_join(fips_pops %>% select(fips, fips_pop_quantiles), by=c("fips"))
  # Add in quantiles of mobility: assume 20 total bins (eg 5% quantiles)
  weekly_dat$mob_qs = ntile(weekly_dat$mobility_pc1_full_dat, 20)
  # everything in the middle quartiles -> a single bin 
  weekly_dat$mob_qs[weekly_dat$mob_qs >= 6 & weekly_dat$mob_qs <= 15] = 10.5
  weekly_dat$mob_qs[weekly_dat$mob_qs == 2 | weekly_dat$mob_qs == 3] = 2.5
  weekly_dat$mob_qs[weekly_dat$mob_qs == 4 | weekly_dat$mob_qs == 5] = 4.5
  weekly_dat$mob_qs[weekly_dat$mob_qs == 18 | weekly_dat$mob_qs == 19] = 18.5
  weekly_dat$mob_qs[weekly_dat$mob_qs == 16 | weekly_dat$mob_qs == 17] = 16.5
  return(list(weekly_dat = weekly_dat, basis = dat$basis, 
             unq_dates = dat$unq_dates, loc_scale_factors = dat$loc_scale_factors))
}