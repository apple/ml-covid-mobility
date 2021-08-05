#
# For licensing see accompanying LICENSE file.
# Copyright (C) 2020 Apple Inc. All Rights Reserved.
#

source("utils.R")

#' Download mask data and interpolate between CDC guidance date, Pew research study dates, NYT dates, and CMU mask survey start dates. 
#'
#' @param inc_fips Vector of FIPS codes for counties to include.
#' @param weekly_avgimp_dat tibble of weekly averaged imputed data 
#' 
#' @return tibble with interpolated mask values for each county 
get_mask_data = function(unq_county_fips, weekly_avgimp_dat) {
  CDC_DATE = as.Date('2020-04-04')
  PEW_JUNE_DATE = as.Date('2020-06-07')
  NYT_JULY_DATE = as.Date('2020-07-08')
  PEW_AUGUST_DATE = as.Date('2020-08-10')
  CMU_START_DATE = as.Date('2020-09-08')

  ## Load data 
  pew_mask = read_csv("./data/pew_mask_use_by_subregion.csv") %>% mutate(date = as.Date(date, format = "%m/%d/%y"))
  df_state_subregion_map = read_csv("./data/state_subregion.csv")

  pew_scaling = pew_mask %>% pivot_wider(subregion, names_from = date, values_from = mask_wear_all_or_most) %>% 
        rename(mask_june = `2020-06-07`, mask_august = `2020-08-10`) %>% 
        mutate(n_days_between = as.numeric(PEW_AUGUST_DATE - PEW_JUNE_DATE), 
            slope_per_day = (mask_august - mask_june) / n_days_between)

  col_types = cols(
                    .default = col_double(),
                    fips = col_character(),
                    county_state = col_character(),
                    region = col_character(),
                    subregion = col_character(),
                    state = col_character(),
                    csa_state_other = col_character(), 
                    last_date = readr::col_date(format = "")
                  )
    
  ### Load & process NYT mask data
  raw_NYT_dat = get_county_mask_static_data(unq_county_fips)
  NYT_dat = raw_NYT_dat %>%
    mutate(mask_wear_always_frequently_NYT = mask_wear_always + mask_wear_frequently) %>%
    select(fips, mask_wear_always_frequently_NYT)

  ## Get county -> subregion mappings 
  df_pew_scaling = weekly_avgimp_dat %>% select(fips, state) %>% distinct() %>%
                                  left_join(NYT_dat, by="fips") %>%
                        left_join(df_state_subregion_map) %>% 
                                  left_join(pew_scaling)

  # Get FB survey data
  data_state <- covidcast::covidcast_signal("fb-survey", signal = "smoothed_wwearing_mask", start_day = as.Date("2020-09-08"), 
                  end_day = as.Date("2021-03-05"), geo_type = "state") %>% tibble()

  cmu_data = data_state %>% left_join(df_state_subregion_map) %>% 
                            select(state, time_value, value) %>% 
                            rename(last_date = time_value, cmu_mask_val = value) %>% 
                            mutate(cmu_mask_val = cmu_mask_val / 100) %>% 
                            arrange(state, last_date) %>% 
                            group_by(state) %>% 
                            mutate(mask_avg_last_week = moving_average(cmu_mask_val, mean, 7)) %>% 
                            right_join(df_pew_scaling %>% select(fips, state))


  df_cmu_scaling = cmu_data %>% filter(last_date == CMU_START_DATE + 1) %>% 
          select(state, fips, mask_avg_last_week) %>% 
          left_join(df_state_subregion_map %>% select(state, subregion)) %>% 
          left_join(pew_mask %>% pivot_wider(subregion, names_from = date, values_from = mask_wear_all_or_most) %>% 
          rename(mask_june = `2020-06-07`, mask_august = `2020-08-10`)) %>% 
          mutate(n_days_between = as.numeric(CMU_START_DATE - PEW_JUNE_DATE), 
            slope_per_day = (mask_avg_last_week - mask_june) / n_days_between)  %>% 
          mutate(slope_per_day = if_else(slope_per_day < 0, 0, slope_per_day)) %>% 
          left_join(df_pew_scaling %>% select(fips, mask_wear_always_frequently_NYT))


  df_cmu_weekly = cmu_data %>% 
            right_join(weekly_avgimp_dat %>% select(last_date, fips) %>% distinct()) %>% 
            ungroup() %>% 
            filter(complete.cases(.)) %>% 
            arrange(state, fips, last_date) %>% 
            group_by(state, fips) %>% 
            mutate(delta = mask_avg_last_week - lag(mask_avg_last_week))

  ## Create state level features 
  df_pew_scaling = df_pew_scaling %>% 
                    group_by(state) %>% 
                    summarize(mask_wear_always_frequently_NYT_state = mean(mask_wear_always_frequently_NYT)) %>% 
                    left_join(df_state_subregion_map) %>% 
                    left_join(pew_scaling)

  df_states_dates = weekly_avgimp_dat %>% select(state, last_date) %>% distinct()
  df_state = build_mask_state_level_features(df_states_dates, df_pew_scaling, df_cmu_scaling, df_cmu_weekly)
  ### Fix NAs 
  all_states = sort(unique(df_state$state))
  for (this_state in all_states) {
    if (is.na(df_state$mask_wear_always_frequently_combined_state[df_state$state==this_state & df_state$last_date=="2021-02-20"])) {
      old_mask_val = df_state$mask_wear_always_frequently_combined_state[df_state$state==this_state & df_state$last_date=="2021-02-13"]
      df_state$mask_wear_always_frequently_combined_state[df_state$state==this_state & df_state$last_date=="2021-02-20"] = old_mask_val
    }
  }
  return(df_state)
}