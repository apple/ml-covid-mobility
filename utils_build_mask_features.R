#
# For licensing see accompanying LICENSE file.
# Copyright (C) 2020 Apple Inc. All Rights Reserved.
#

library(tidyverse)
## Function to enforce boundary conditions for interpolated values
MIN_MASK_VALUE = 0 
MAX_MASK_VALUE = 0.95
CDC_DATE = as.Date('2020-04-04')
PEW_JUNE_DATE = as.Date('2020-06-07')
NYT_JULY_DATE = as.Date('2020-07-08')
PEW_AUGUST_DATE = as.Date('2020-08-10')
CMU_START_DATE = as.Date('2020-09-08')

#' Simple point-wise min-max 
#' @param x value to threshold between [min, max], a scalar
#' @param min min. value to threshold 
#' @param max max. value to threshold 
#' @return thresholded value 
enforce_boundary_conditions = function(x, min, max) {
	stopifnot(length(x) == 1)
      if (x < min) return(min)
      if (x > max) return(max)
      return(x)
}

#' Helper function to linearly interpolate between Pew June and August data 
#' @param date to interpolate 
#' @param state_x state for interpolation 
#' @param df_pew_scaling tibble containing state level NYT mask value and the slope between the 
#' Pew June and August data for the state's subregion
#' 
#' @return interpolated value for the specified date 
pew_interpolant_state = function(date, state_x, df_pew_scaling) {
     df = df_pew_scaling %>% filter(state == state_x)
     y_t = df$slope_per_day * (as.numeric(date - NYT_JULY_DATE)) + df$mask_wear_always_frequently_NYT_state
     return(y_t)
}

#' Interpolate next point along a line between last point and CMU FB survey date. 
#' @param interpolate_date current date to interpolate 
#' @param last_interpolated_date last date interpolated 
#' @param last_interpolated_value 
#' @param state_x state of interest 
#' @param df_cmu_weekly tibble of counties and corresponding state level mask usage and usage change over the last week
#' @return next interpolated value along line
fb_interpolant_state = function(interpolate_date, last_interpolated_date, last_interpolated_value, state_x, df_cmu_weekly) {
      df_cmu = df_cmu_weekly %>% filter(state == state_x) %>% arrange(last_date)
      y0 = last_interpolated_value
      y1 = df_cmu$mask_avg_last_week[1]
      x1 = df_cmu$last_date[1]
      n_days_between = as.numeric(x1 - last_interpolated_date)
      m = (y1 - y0) / n_days_between
      y_t = m * (as.numeric(interpolate_date - x1)) + y1 
      return(y_t)     
}

#' Interpolate between Pew surveys, then to CMU survey, use CMU survey once it exists 
#' @param dates dates to interpolate 
#' @param state_x state to interpolate 
#' @param df_pew_scaling tibble of counties and daily rate of change in corresponding state mask usage between Pew surveys
#' @param df_cmu_weekly tibble of counties and corresponding state level mask usage and usage change over the last week
#' @return numeric array of interpolated mask use values corresponding to input dates and state_x
#' @details Two stage interpolant: interpolate between two Pew survey dates and then to the state level CMU September data. Use CMU surveys afterwards. 
interpolate_pew_cmu_state = function(dates, state_x, df_pew_scaling, df_cmu_weekly) {
      n_dates = length(dates)
      mask_use = numeric(n_dates)
      for (date_i in 1:n_dates) {
            date_x = dates[date_i]
            if (date_x <= CDC_DATE) { 
                  mask_use[date_i] = 0 
            } else if (date_x > CDC_DATE && date_x <= PEW_JUNE_DATE) { 
                  mask_use[date_i] = enforce_boundary_conditions(
                                                      pew_interpolant_state(PEW_JUNE_DATE, state_x, df_pew_scaling), 
                                                      MIN_MASK_VALUE, MAX_MASK_VALUE)

            } else if (date_x > PEW_JUNE_DATE && date_x <= PEW_AUGUST_DATE) { 
                  mask_use[date_i] = enforce_boundary_conditions(
                                                      pew_interpolant_state(date_x, state_x, df_pew_scaling), 
                                                      MIN_MASK_VALUE, MAX_MASK_VALUE)

            } else if (date_x > PEW_AUGUST_DATE && date_x <= CMU_START_DATE) { 
                  mask_use[date_i] = enforce_boundary_conditions(
                                                      fb_interpolant_state(date_x, dates[date_i - 1], mask_use[date_i - 1], state_x, df_cmu_weekly), 
                                                      MIN_MASK_VALUE, MAX_MASK_VALUE) 
            } else {
                  mask_use[date_i] = (df_cmu_weekly %>% filter(state == state_x, last_date == date_x))$mask_avg_last_week[1]
            } 
      }
      return(mask_use)
}

#' Interpolate between June Pew and CMU survey, then use CMU survey once it exists 
#' @param dates dates to interpolate 
#' @param state_x state to interpolate 
#' @param df_cmu_scaling tibble of counties and daily rate of change in corresponding state mask usage between June Pew survey and CMU survey start date
#' @param df_cmu_weekly tibble of counties and corresponding state level mask usage and usage change over the last week
#' @param df_pew_scaling tibble of counties and daily rate of change in corresponding state mask usage between Pew surveys
#' @return numeric array of interpolated mask use values corresponding to input dates and state_x
#' @details Interpolate between June Pew survey and CMU September data. Use CMU surveys afterwards.
interpolate_cmu_state = function(dates, state_x, df_cmu_scaling, df_cmu_weekly, df_pew_scaling) {
      n_dates = length(dates)
      mask_use = numeric(n_dates)
      for (date_i in 1:n_dates) {
            date_x = dates[date_i]
            if (date_x <= CDC_DATE) { 
                  mask_use[date_i] = 0 

            } else if (date_x > CDC_DATE && date_x <= PEW_JUNE_DATE) { 
                  mask_use[date_i] = enforce_boundary_conditions(
                                                      pew_interpolant_state(PEW_JUNE_DATE, state_x, df_pew_scaling), 
                                                      MIN_MASK_VALUE, MAX_MASK_VALUE) 
            } else if (date_x > PEW_JUNE_DATE && date_x <= CMU_START_DATE) { 
                  mask_use[date_i] = enforce_boundary_conditions(
                                                      fb_interpolant_state(date_x, dates[date_i - 1], mask_use[date_i - 1], state_x, df_cmu_weekly),
                                                      MIN_MASK_VALUE, MAX_MASK_VALUE) 
            } else {
                  mask_use[date_i] = (df_cmu_weekly %>% filter(state == state_x, last_date == date_x))$mask_avg_last_week[1]
            } 
      }
      return(mask_use)
}

#' Build mask features for each state 
#' @param df_states_dates tibble of counties and dates to build features 
#' @param df_pew_scaling tibble of counties and daily rate of change in corresponding state mask usage between Pew surveys
#' @param df_cmu_scaling tibble of counties and daily rate of change in corresponding state mask usage between June Pew survey and CMU survey start date
#' @param df_cmu_weekly tibble of counties and corresponding state level mask usage and usage change over the last week
#' 
#' @return tibble of counties, dates, and mask features for each county filled in using a two stage interpolant 
build_mask_state_level_features = function(df_states_dates, df_pew_scaling, df_cmu_scaling, df_cmu_weekly) {  
      states = unique(df_states_dates$state)
      df_pew_scaling_week = NULL 
      for(state_i in 1:length(states)) {
            state_x = states[state_i]
            dates = (df_states_dates %>% filter(state == state_x) %>% select(last_date) %>% distinct() %>% arrange(last_date))$last_date
            # Baseline interpolant: 
            mask_use = interpolate_pew_cmu_state(dates, state_x, df_pew_scaling, df_cmu_weekly)
            # If this simple interpolant results in a decrease at August date
            # interpolate to the CMU September date instead of interpolating to the August date
            date_check_0 = which(dates <= PEW_AUGUST_DATE)
            date_check_0 = date_check_0[length(date_check_0)]
            date_check_1 = which(dates >= CMU_START_DATE)
            date_check_1 = date_check_1[1]
            if (mask_use[date_check_1] - mask_use[date_check_0] < 0) {
                  mask_use = interpolate_cmu_state(dates, state_x, df_cmu_scaling, df_cmu_weekly, df_pew_scaling)            
            }
            df_pew_scaling_week = bind_rows(df_pew_scaling_week, tibble(last_date = dates, mask_wear_always_frequently_combined_state = mask_use, state = state_x))
      }
      return(df_pew_scaling_week)
}

