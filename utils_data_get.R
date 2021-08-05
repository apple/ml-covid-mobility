#
# For licensing see accompanying LICENSE file.
# Copyright (C) 2020 Apple Inc. All Rights Reserved.
#

library(lubridate)
library(tidyverse)
library(covidcast)
library(tictoc)
library(doParallel)
library(incidental)
library(mice) 
library(splines)
library(readxl)
source("./utils_build_mask_features.R")
source("./build_mask_features.R")

#' Download and filter NYT COVID-19 data. Note the hard coded path.
#'
#' @param min_num_days Positive integer number for minimum number of days of data for a county to be included
#' @param min_num_cases Positive integer number for minimum number of total cases for a county to be included
#' 
#' @return Filtered tibble with daily covid data from NYT
get_NYT_covid_data = function(min_num_days = 105, min_num_cases = 250) {
  
  NYT_data_path = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
  NYT_raw_dat = read_csv(NYT_data_path)
  
  NYT_covid_dat = NYT_raw_dat %>%
    group_by(county,state) %>%
    filter(n() >= min_num_days) %>%  #sufficient number of days 
    filter(cases[length(cases)] >= min_num_cases) %>% #sufficient number of cases
    filter(county != "New York City" & county != "Unknown") %>%
    filter(!is.na(fips)) %>%
    mutate(cases.yesterday = c(0,cases[1:(length(cases)-1)])) %>%
    mutate(deaths.yesterday = c(0,deaths[1:(length(deaths)-1)])) %>%
    mutate(new.cases = cases - cases.yesterday) %>%
    mutate(new.deaths = deaths - deaths.yesterday) %>%
    select(-c("cases.yesterday", "deaths.yesterday", "cases", "deaths")) %>%
    ungroup %>%
    mutate(new.cases = pmax(new.cases,0)) %>%
    mutate(new.deaths = pmax(new.deaths,0)) %>%
    mutate(county_state = str_c(county, state, sep=", ")) %>%
    select(c("county_state","state","fips","date","new.cases","new.deaths")) 
  
  return(NYT_covid_dat)
}

#' Download and filter NYC COVID-19 data. This is done separate from the NYT
#' data pull, as they lump together all 5 boroughs into one NYC record. Note
#' the hard coded path.
#'
#' @return Filtered tibble with daily covid data for the 5 NYC boroughs
get_NYC_covid_data = function() {
  
  NYC_data_path = "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/data-by-day.csv"
  NYC_raw_dat = read_csv(NYC_data_path)
  dates = as.Date(NYC_raw_dat$date_of_interest, "%m/%d/%Y")

  BK_fips = "36047"
  BK_county_state = "Kings, New York"
  BK_dat = tibble(county_state=BK_county_state, state = "New York", fips=BK_fips, date=dates, 
                  new.cases = NYC_raw_dat$BK_CASE_COUNT, new.deaths = NYC_raw_dat$BK_DEATH_COUNT)
  
  BX_fips = "36005"
  BX_county_state = "Bronx, New York"
  BX_dat = tibble(county_state=BX_county_state, state = "New York", fips=BX_fips, date=dates, 
                  new.cases = NYC_raw_dat$BX_CASE_COUNT, new.deaths = NYC_raw_dat$BX_DEATH_COUNT)
  
  MN_fips = "36061"
  MN_county_state = "New York, New York"
  MN_dat = tibble(county_state=MN_county_state, state = "New York", fips=MN_fips, date=dates, 
                  new.cases = NYC_raw_dat$MN_CASE_COUNT, new.deaths = NYC_raw_dat$MN_DEATH_COUNT)
  
  QN_fips = "36081"
  QN_county_state = "Queens, New York"
  QN_dat = tibble(county_state=QN_county_state, state = "New York", fips=QN_fips, date=dates, 
                  new.cases = NYC_raw_dat$QN_CASE_COUNT, new.deaths = NYC_raw_dat$QN_DEATH_COUNT)
  
  SI_fips = "36085"
  SI_county_state = "Richmond, New York"
  SI_dat = tibble(county_state=SI_county_state, state = "New York", fips=SI_fips, date=dates, 
                  new.cases = NYC_raw_dat$SI_CASE_COUNT, new.deaths = NYC_raw_dat$SI_DEATH_COUNT)
  
  NYC_case_dat = bind_rows(BK_dat,BX_dat,MN_dat,QN_dat,SI_dat)
  
  return(NYC_case_dat)
}

#' Download and filter US county-level weather data. We just keep average
#' temperature, but other fields also exist (eg rainfall, humidity).
#'
#' @param inc_fips Vector of FIPS codes for counties to include.
#' @param start_date String denoting earliest date to retain weather data. 
#'   By default the start date is 2/16/20, a Sunday, as the earliest date
#'   that we have Google mobility data for is 2/15/20, and our downstream
#'   analyses are binned at a weekly level assume that weeks start on Sundays.
#' 
#' @return Filtered tibble with daily weather data for all included counties.
get_county_temp_data = function(inc_fips, start_date = "2020-02-16") {
  
  index_data_path = "https://storage.googleapis.com/covid19-open-data/v2/index.csv"
  weather_data_path = "https://storage.googleapis.com/covid19-open-data/v2/weather.csv"
  raw_weather_dat = read_csv(weather_data_path)
  raw_index_dat = read_csv(index_data_path)
  
  index_dat = raw_index_dat %>%
    filter(country_code == "US") %>%
    select(key, subregion2_code) %>%
    rename(fips = subregion2_code) %>%
    filter(!is.na(fips))
  
  weather_dat = raw_weather_dat %>%
    inner_join(index_dat, "key") %>%
    select(c("fips","date","average_temperature")) %>%
    filter(fips %in% inc_fips) %>%
    filter(date >= start_date)
  
  return(weather_dat)
}

#' Download and filter US county-level Google mobility data.
#'
#' @param inc_fips Vector of FIPS codes for counties to include.
#' @param start_date String denoting earliest date to retain mobility data. 
#'   By default the start date is 2/16/20, a Sunday, as the earliest date
#'   that we have Google mobility data for is 2/15/20, and our downstream
#'   analyses binned at a weekly level assume that weeks start on Sundays.
#' 
#' @return Filtered tibble with daily Google mobility data for all included counties.
get_county_google_mobility_data = function(inc_fips, start_date="2020-02-16") {
  
  google_mobility_data_path = "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
  raw_google_mobility_dat = read_csv(google_mobility_data_path, 
                                     col_types=cols(
                                       country_region_code=col_character(),
                                       country_region=col_character(),
                                       sub_region_1=col_character(),
                                       sub_region_2=col_character(),
                                       metro_area=col_character(),
                                       iso_3166_2_code=col_character(),
                                       census_fips_code=col_character(),
                                       place_id=col_character()
                                     ))
  
  google_mobility_dat = raw_google_mobility_dat %>%
    filter(country_region_code=="US") %>%
    filter(census_fips_code %in% inc_fips) %>%
    select(c("census_fips_code","date","retail_and_recreation_percent_change_from_baseline",
             "grocery_and_pharmacy_percent_change_from_baseline",
             "parks_percent_change_from_baseline","transit_stations_percent_change_from_baseline",
             "workplaces_percent_change_from_baseline","residential_percent_change_from_baseline")) %>%
    rename(
      fips = "census_fips_code",
      mobility_retail_and_recreation = "retail_and_recreation_percent_change_from_baseline",
      mobility_grocery_and_pharmacy = "grocery_and_pharmacy_percent_change_from_baseline",
      mobility_parks = "parks_percent_change_from_baseline",
      mobility_transit_stations = "transit_stations_percent_change_from_baseline",
      mobility_workplaces = "workplaces_percent_change_from_baseline",
      mobility_residential = "residential_percent_change_from_baseline"
    ) %>%
    filter(date >= start_date) 
  
  return(google_mobility_dat)
}

#' Load, filter and lightly touch up county-level static mask-wearing data. Note the
#' hard coded path to NYT for the survey data.
#'
#' @param inc_fips Vector of FIPS codes for counties to include.
#' 
#' @return Filtered tibble with static county-level mask-wearing data.
get_county_mask_static_data = function(inc_fips) {

  mask_data_path = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv"
  mask_raw_dat = read_csv(mask_data_path) 
  
  mask_dat = mask_raw_dat %>%
    rename(fips = "COUNTYFP",
           mask_wear_never = "NEVER",
           mask_wear_rarely = "RARELY",
           mask_wear_sometimes = "SOMETIMES",
           mask_wear_frequently = "FREQUENTLY",
           mask_wear_always = "ALWAYS") %>%
    filter(fips %in% inc_fips)
  
  return(mask_dat)
}

#' Download and filter US county-level SafeGraph mobility (completely at home) data.
#'
#' @param start_date String denoting earliest date to retain mobility data. 
#'   By default the start date is 2/16/20, a Sunday.
#' 
#' @return Filtered tibble with daily SG mobility data for all included counties.
get_sg_mobility_data = function(start_date) {
  df = covidcast_signal("safegraph",  signal = "completely_home_prop_7dav", 
                                      start_day = start_date, 
                                      end_day = format(Sys.Date(), "%Y-%m-%d"), 
                                      geo_type = "county") %>% tibble()

  df = df %>% rename(fips = geo_value) %>% 
                    mutate(week = epiweek(time_value) + if_else(time_value > "2020-12-31" & epiweek(time_value) < 53, 53, 0)) %>% 
                    group_by(fips, week) %>% 
                    summarize(last_date = last(time_value), completely_home_prop_7dav = mean(value, na.rm = T))
}

#' Load county-level population estimates (from the US Census, 2018). Note that 
#' there are many other county-level static variables in this repo, but we just
#' keep population estimates for our models.
#'
#' @param inc_fips Vector of FIPS codes for counties to include.
#' 
#' @return Filtered tibble with county populations.
get_county_pops = function(inc_fips) {
  county_pop_path = "https://github.com/JieYingWu/COVID-19_US_County-level_Summaries/blob/master/data/counties_only.csv?raw=true"
  county_pop_dat = read_csv(county_pop_path) %>%
    rename(fips=FIPS, popest="POP_ESTIMATE_2018") %>%
    select(fips, popest) %>%
    filter(fips %in% inc_fips) %>%
    mutate(log_popest = log(popest + .01))
  
  return(county_pop_dat)
}


#' Run incidental to estimate county-level incidence curves
#'
#' @param county_covid_case_dat Tibble of covid case data, containing dates
#'   and new cases per day.
#' @param num_cores Integer number of cores to use for parallel processing. 
#' 
#' @return Filtered tibble with estimates of daily county-level incidence.
estimate_county_incidence = function(county_covid_case_dat, num_cores = 64) {
  
  unq_fips = unique(county_covid_case_dat$fips)
  n_fips = length(unq_fips)
  
  cl = makeCluster(num_cores, type="FORK", outfile="")
  registerDoParallel(cl)
  
  county_inc_list = foreach(i = 1:n_fips) %dopar% {
    
    start_t = Sys.time()
    this_dat = county_covid_case_dat %>% filter(fips == unq_fips[i] & !is.na(new.cases))
    this_dates = this_dat$date
    this_cases = this_dat$new.cases
    
    #NOTE: can probably be a little less aggressive about some of the args here,
    # like how many hyperparams to try and how many CV restarts to use
    incidental_out = fit_incidence(
      reported = this_cases,
      delay_dist = covid_delay_dist$case,
      dof_grid = seq(4, 20, 2),
      dof_method = "bic",
      lam_grid = 10**seq(1,-8,length.out=60),
      lam_method = "val",
      percent_thresh = 2,
      val_restarts = 5,
      seed = 8675309
    )
    
    #NOTE: add 53 (leap week; not 52) to 2021 data to avoid join conflicts on same 2020 week
    #  also catches edge case at new year: week 52 = 12/20-26, 53 = 12/27-1/2, 1=1/3-9, ...
    this_weeks = epiweek(this_dates) + if_else(this_dates > "2020-12-31" & epiweek(this_dates) < 53, 53, 0)
    
    #first and last week need to be tossed unless complete week of 7 days (no partial weeks)
    valid_weeks = as.numeric(names(table(this_weeks))[table(this_weeks)==7])
    date_keep_inds = this_weeks %in% valid_weeks
    valid_dates = this_dates[date_keep_inds]
    
    week_start_inds = seq(from=1,to=length(valid_dates)-6,by=7)
    week_end_inds = seq(from=7,to=length(valid_dates),by=7)
    last_days_per_week = valid_dates[week_end_inds] #for weekly data summary
    
    valid_daily_infection_means = incidental_out$Ihat[date_keep_inds]
    valid_daily_infection_samps = incidental_out$Isamps[,date_keep_inds] #pruned fractional first/last weeks off
    
    ### aggregate weekly 
    n_weeks = length(week_start_inds)
    weekly_infection_samps = matrix(data=NA, nrow=nrow(valid_daily_infection_samps),ncol=n_weeks)
    for (ii in 1:n_weeks) {
      weekly_infection_samps[,ii] = rowSums(valid_daily_infection_samps[,week_start_inds[ii]:week_end_inds[ii]])
    }
    weekly_infection_means = sapply(1:n_weeks, function(j) sum(valid_daily_infection_means[week_start_inds[j]:week_end_inds[j]]))
    
    ### get growth rates 
    weekly_growth_rate_samps = weekly_infection_samps[,2:n_weeks] / (weekly_infection_samps[,1:(n_weeks-1)] + .1)
    weekly_log_growth_rate_samps = log(weekly_growth_rate_samps + .001)

    ### finally build out relevant df to save
    end_t = Sys.time()
    print(sprintf("finished %d / %d in %.1f secs, %d total cases over %d days", i, n_fips, 
                  as.numeric(end_t - start_t), sum(this_cases), length(this_cases)))
    
    inc_dat = tibble(fips = this_dat$fips[1],
                     county_state = this_dat$county_state[1],
                     state = this_dat$state[1],
                     week = valid_weeks,
                     last_date = last_days_per_week,
                     
                     ##### hold onto distribution summaries for weekly infections & log growth rates,
                     #####  in case those are wanted downstream to show uncertainty from incidental 
                     
                     infections_mean = weekly_infection_means,
                     infections_2.5 = apply(weekly_infection_samps, 2, quantile, probs=c(.025)),
                     infections_25 = apply(weekly_infection_samps, 2, quantile, probs=c(.25)),
                     infections_75 = apply(weekly_infection_samps, 2, quantile, probs=c(.75)),
                     infections_97.5 = apply(weekly_infection_samps, 2, quantile, probs=c(.975)),
                     
                     log_growth_rate_mean = c(NA,apply(weekly_log_growth_rate_samps, 2, mean)),
                     log_growth_rate_2.5 = c(NA,apply(weekly_log_growth_rate_samps, 2, quantile, probs=c(.025))),
                     log_growth_rate_25 = c(NA,apply(weekly_log_growth_rate_samps, 2, quantile, probs=c(.25))),
                     log_growth_rate_75 = c(NA,apply(weekly_log_growth_rate_samps, 2, quantile, probs=c(.75))),
                     log_growth_rate_97.5 = c(NA,apply(weekly_log_growth_rate_samps, 2, quantile, probs=c(.975)))
    )
  }
  stopCluster(cl)
  
  all_county_inc_dat = bind_rows(county_inc_list)
  return(all_county_inc_dat)
}

#' Convert daily dynamic covariate data to weekly mean. Note that weeks are defined to
#' start on Sundays and end on Saturdays.
#'
#' @param daily_covariate_dat Tibble of daily dynamic covariate (temp/mobility) data
#' 
#' @return Tibble with daily data aggregated to weekly level.
make_weekly_dynamic_covariate_data = function(daily_covariate_dat) {
  
  weekly_covariate_dat = daily_covariate_dat %>%
    mutate(week = epiweek(date) + if_else(date > "2020-12-31" & epiweek(date) < 53, 53, 0)) %>%
    group_by(fips,week) %>%
    summarize(
      county_state = last(county_state),
      state = last(state),
      last_date = last(date),
      average_temperature = mean(average_temperature, na.rm=T),
      mobility_retail_and_recreation = mean(mobility_retail_and_recreation, na.rm=T),
      mobility_grocery_and_pharmacy = mean(mobility_grocery_and_pharmacy, na.rm=T),
      mobility_parks = mean(mobility_parks, na.rm=T),
      mobility_transit_stations = mean(mobility_transit_stations, na.rm=T),
      mobility_workplaces = mean(mobility_workplaces, na.rm=T),
      mobility_residential = mean(mobility_residential, na.rm=T)
    ) %>%
    ungroup() %>%
    select(c("fips","county_state","state","week","last_date","average_temperature",
             "mobility_retail_and_recreation","mobility_grocery_and_pharmacy",
             "mobility_parks","mobility_transit_stations","mobility_workplaces",
             "mobility_residential"))    
  
  return(weekly_covariate_dat)
}

#' Build out relevant grouping variables and clustering on counties for use in multilevel models.
#'
#' @param static_covariate_dat Tibble of static county data (just population estimate for our models)
#' 
#' Build out the following nested group structures:
#'   - counties (lowest)
#'   - CSAs or state-level "other" categories for counties not in CSA or in singleton CSA
#'   - subregions/divisions (9 nationally)
#'   - regions (4 nationally)
#'   
#' NOTE: some 10% or so of CSAs contain counties that span multiple subregions (and hence regions).
#'   To handle these edge cases, we'll assign the CSA to the subregion (and region) with
#'   the highest total population among counties in that CSA.    
#'   
#' @return Tibble of county-level population data, with additional columns for grouping variables
get_county_groupings = function(static_covariate_dat) {
  
  # https://en.wikipedia.org/wiki/List_of_regions_of_the_United_States
  #Census divisions/"subregions" (narrower) & regions (broader)
  
  #divisions first
  new_england_northeast_states = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont")
  mid_atlantic_northeast_states = c("New Jersey", "New York", "Pennsylvania")
  east_north_central_midwest_states = c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin")
  west_north_central_midwest_states = c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota")
  south_atlantic_south_states = c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", 
                                  "South Carolina", "Virginia", "District of Columbia", "West Virginia")
  east_south_central_south_states = c("Alabama", "Kentucky", "Mississippi", "Tennessee")
  west_south_central_south_states = c("Arkansas", "Louisiana", "Oklahoma", "Texas")
  mountain_west_states = c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming")
  pacific_west_states = c("Alaska", "California", "Hawaii", "Oregon", "Washington")
  
  static_covariate_dat$subregion = NA
  static_covariate_dat$subregion[static_covariate_dat$state %in% new_england_northeast_states] = "New_England"
  static_covariate_dat$subregion[static_covariate_dat$state %in% mid_atlantic_northeast_states] = "Mid_Atlantic"
  static_covariate_dat$subregion[static_covariate_dat$state %in% east_north_central_midwest_states] = "East_North_Central"
  static_covariate_dat$subregion[static_covariate_dat$state %in% west_north_central_midwest_states] = "West_North_Central"
  static_covariate_dat$subregion[static_covariate_dat$state %in% south_atlantic_south_states] = "South_Atlantic"
  static_covariate_dat$subregion[static_covariate_dat$state %in% east_south_central_south_states] = "East_South_Central"
  static_covariate_dat$subregion[static_covariate_dat$state %in% west_south_central_south_states] = "West_South_Central"
  static_covariate_dat$subregion[static_covariate_dat$state %in% mountain_west_states] = "Mountain"
  static_covariate_dat$subregion[static_covariate_dat$state %in% pacific_west_states] = "Pacific"
  
  #regions next
  northeast_states = c(new_england_northeast_states,mid_atlantic_northeast_states)
  midwest_states = c(east_north_central_midwest_states,west_north_central_midwest_states)
  south_states = c(south_atlantic_south_states,east_south_central_south_states,west_south_central_south_states)
  west_states = c(mountain_west_states,pacific_west_states)
  
  static_covariate_dat$region = NA
  static_covariate_dat$region[static_covariate_dat$state %in% northeast_states] = "Northeast"
  static_covariate_dat$region[static_covariate_dat$state %in% midwest_states] = "Midwest"
  static_covariate_dat$region[static_covariate_dat$state %in% south_states] = "South"
  static_covariate_dat$region[static_covariate_dat$state %in% west_states] = "West"
  
  #download CSA data file from census & touch up
  csa_data_url = "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/delineation-files/list1_2020.xls"
  csa_file = tempfile()
  download.file(csa_data_url, csa_file, mode="wb")
  raw_csa_dat = read_excel(csa_file, skip=2)
  raw_csa_dat = raw_csa_dat[1:(nrow(raw_csa_dat)-4),]
  
  csa_dat = raw_csa_dat %>% 
    rename(
      csa = "CSA Code",
      csa_name = "CSA Title",
      fips_state = "FIPS State Code",
      fips_county = "FIPS County Code"
    ) %>%
    mutate(
      fips = paste0(fips_state,fips_county),
      csa = as.character(csa) ) %>% 
    select(fips, csa, csa_name)   
  
  static_covariate_dat = static_covariate_dat %>%
    left_join(csa_dat, by = c("fips")) %>%
    relocate(region, subregion, csa, .after=state)
  
  CSA_cts = table(static_covariate_dat$csa)
  min_num_counties_per_csa = 2
  
  keep_CSAs = names(CSA_cts)[as.numeric(CSA_cts) >= min_num_counties_per_csa]
  toss_CSAs = names(CSA_cts)[as.numeric(CSA_cts) < min_num_counties_per_csa]
  
  ### build out an "other" category by state for counties not falling in existing CSA
  static_covariate_dat$csa_state_other = static_covariate_dat$csa
  static_covariate_dat$csa_state_other[static_covariate_dat$csa_state_other %in% toss_CSAs] = NA
  
  for (this_state in state.name) {
    this_inds = (is.na(static_covariate_dat$csa_state_other) &
                   static_covariate_dat$state == this_state)
    this_state_abb = state.abb[which(state.name==this_state)]
    static_covariate_dat$csa_state_other[this_inds] = paste0(this_state_abb,"-other")
  }
  static_covariate_dat = static_covariate_dat %>% 
    relocate(csa_state_other, .after=csa)
  
  unq_CSA_groups = unique(static_covariate_dat$csa_state_other)
  
  #manually fix subregions & regions for CSAs on boundaries, based on one with largest population
  for (this_csa in unq_CSA_groups) {
    this_inds = static_covariate_dat$csa_state_other == this_csa
    
    this_pops = exp(static_covariate_dat$log_popest[this_inds])
    this_regions = static_covariate_dat$region[this_inds]
    this_subregions = static_covariate_dat$subregion[this_inds]
    
    unq_subregions = unique(this_subregions)
    n_unq_subregions = length(unq_subregions)
    
    if (n_unq_subregions > 1) {
      subregion_pops = c()
      for (this_subregion in unq_subregions) {
        subregion_pops = c(subregion_pops, sum(this_pops[this_subregions == this_subregion]))
      }
      
      #get subregion and corresponding region where most population in this CSA resides
      maxpop_subregion = unq_subregions[subregion_pops == max(subregion_pops)]
      maxpop_region = this_regions[this_subregions == maxpop_subregion][1]
      
      #hard-code subregion and region
      static_covariate_dat$subregion[this_inds] = maxpop_subregion
      static_covariate_dat$region[this_inds] = maxpop_region
    }
  }
  
  ### TEST: should all be 1...
  n_subregions_per_csa = c()
  for (this_csa in unique(static_covariate_dat$csa_state_other)) {
    n_subregions_per_csa = c(n_subregions_per_csa, length(unique(
      filter(static_covariate_dat, csa_state_other==this_csa) %>% select(subregion) %>% unlist )) )
  }
  stopifnot(all(n_subregions_per_csa == 1))
  
  n_regions_per_subregion = c()
  for (this_subreg in unique(static_covariate_dat$subregion)) {
    n_regions_per_subregion = c(n_regions_per_subregion, length(unique(
      filter(static_covariate_dat, subregion==this_subreg) %>% select(region) %>% unlist )) )
  }
  stopifnot(all(n_regions_per_subregion == 1))
  
  return(static_covariate_dat)
}


#' Impute the preprocessed dynamic county-level covariate data.
#' 
#' @param weekly_dat Tibble with only relevant dynamic data, no covid outcome fields.
#' @param static_covariate_dat Tibble with the static covariate county-level data, assumes no missing values.
#' @param num_imps Number of multiple imputed datasets to return.
#' @param num_mice_iters Number of iters to run the mice algorithm.
#' @param seed Random seed.
#' @param num_cores Integer number of cores to use for parallel processing. 
#' 
#' @return Giant stacked tibble with imputed values and a new column denoting imputation number.
impute_dynamic_data = function(weekly_dat, 
                               static_covariate_dat,
                               num_imps = 30, 
                               num_mice_iters = 30, 
                               seed = 8675309,
                               num_cores = 30) {
  
  if (num_cores > num_imps) {num_cores = num_imps} #in case more cores used during incidental
  
  comp_static_preds = static_covariate_dat %>% select(fips,subregion,csa_state_other,log_popest)
  cl = makeCluster(num_cores, type="FORK", outfile="")
  registerDoParallel(cl)
  
  all_imputed_weekly_preds = foreach(imp_num = 1:num_imps) %dopar% {
    print(sprintf("starting imputation %d",imp_num))
  
    #use a simple spline basis to encode a temporal trend for each variable to be imputed,
    # and we'll let the trend vary by CSA and subregion. 
    weekly_spline_dim = 4
    week_spline_bases = ns(weekly_dat$std_week, df = weekly_spline_dim, intercept=FALSE)
    
    all_weekly_preds_dat = weekly_dat %>% 
      inner_join(comp_static_preds,by = c("fips")) %>%
      mutate(
        sp_week1 = week_spline_bases[,1],
        sp_week2 = week_spline_bases[,2],
        sp_week3 = week_spline_bases[,3],
        sp_week4 = week_spline_bases[,4],
        subregion = factor(subregion),
        csa_state_other = factor(csa_state_other))
    
    all_weekly_preds_modelmat = data.frame(model.matrix(formula(~  
      subregion*(sp_week1 + sp_week2 + sp_week3 + sp_week4) +
      csa_state_other*(sp_week1 + sp_week2 + sp_week3 + sp_week4)
    ), 
    data=all_weekly_preds_dat))
    #drop overall intercept (assuming there's one by default in mice)
    all_weekly_preds_modelmat = all_weekly_preds_modelmat[,2:dim(all_weekly_preds_modelmat)[2]]
    
    #drop extra cols as these are in the modelmat now as full no-pooling/FE features
    all_weekly_preds_dat = all_weekly_preds_dat %>%
      select(-c("std_week","sp_week1","sp_week2","sp_week3","sp_week4","subregion","csa_state_other"))
    
    all_weekly_preds_mice = cbind(all_weekly_preds_dat, all_weekly_preds_modelmat)
    n_cols_all_weekly_preds_dat = dim(all_weekly_preds_dat)[2] #only will return these columns of data out
    
    #don't use index vars (fips,week) for imputing so strip off & join at end
    fips_weeks_dat = all_weekly_preds_mice[,1:2] 
    all_weekly_preds_mice = all_weekly_preds_mice[,3:dim(all_weekly_preds_mice)[2]]
    
    print(sprintf("running imputation %d",imp_num))
    
    imps_all = mice(all_weekly_preds_mice, m = 1, maxit = num_mice_iters, 
                    method = "pmm", seed = seed+10*imp_num)
    
    print(sprintf("finished imputation %d",imp_num))
    
    # get imputed vals & cleanup before returning only (fips,week) key & imputed vals
    comp_all_preds = complete(imps_all,1)
    comp_all_preds = cbind(fips_weeks_dat, comp_all_preds)
    comp_all_preds = comp_all_preds[,1:n_cols_all_weekly_preds_dat] %>%
      mutate(imputation = imp_num) %>%
      relocate(imputation, .after=fips)
    
    comp_all_preds
  }
  stopCluster(cl)
  
  final_imputed_weekly_preds = bind_rows(all_imputed_weekly_preds)
  return(final_imputed_weekly_preds)
}


#' Load and process all data sources, estimate infections from cases, and 
#' impute missing mobility & temp values.
#' 
#' @param path_for_modeling_data Final output path
#' @param num_cores number of cores for parallel processing
#' @param start_date Start date for first week.
#' @param end_date_output Last date where data to be outputted ends.
#' @param end_date_incidental Last date where data used in incidental ends.
#'  This field exists basically so results are backwards compatible with when we 
#'  ran this preprocessing pipeline last for the paper, on 3/6/21.
#' @param end_date_impute Last date to use in running mice imputation, also
#'  exists largely for backwards compatibility.
#' @param min_num_days Min number of days a county needs through start_date.
#' @param min_num_cases Min number of cases a county needs through start_date.
#' @param seed_for_mice Seed for mice.
#' @param num_imps Number of imputations in mice.
#' @param num_mice_iters Number of imputations to run per mice imputation.
#' 
#' @return Nothing: write out a data file to main repo directory to load and use for modeling
load_process_estimate_impute_save_data = function(path_for_modeling_data="./data",
                                                  num_cores=64,
                                                  start_date = "2020-02-16",
                                                  end_date_output = "2021-02-20",
                                                  end_date_incidental = "2021-03-06",
                                                  end_date_impute = "2021-02-27",
                                                  min_num_days = 140,
                                                  min_num_cases = 250,
                                                  seed_for_mice = 321,
                                                  num_imps = 30,
                                                  num_mice_iters = 30
                                                  ) {
  
  ##### Load in all the data sources and do filtering
  NYT_covid_dat = get_NYT_covid_data(min_num_days, min_num_cases) 
  NYC_covid_dat = get_NYC_covid_data()
  covid_case_dat = bind_rows(NYT_covid_dat,NYC_covid_dat) %>%
    arrange(fips, date)
  unq_county_fips = unique(covid_case_dat$fips)
  print(sprintf("Loaded covid data for %d counties with min of %d days and %d cases", 
                length(unq_county_fips),min_num_days,min_num_cases))
  
  temp_dat = get_county_temp_data(unq_county_fips, start_date)
  print(sprintf("Only %d / %d counties have weather data...", 
                length(unique(temp_dat$fips)),length(unq_county_fips)))
  
  google_mobility_dat = get_county_google_mobility_data(unq_county_fips, start_date)
  print(sprintf("Only %d / %d counties have Google mobility data...", 
                length(unique(google_mobility_dat$fips)),length(unq_county_fips)))
  
  daily_covid_covariate_dat = covid_case_dat %>%
    full_join(temp_dat, by=c("fips","date")) %>%
    full_join(google_mobility_dat, by=c("fips","date")) %>%
    select(c("fips","county_state","state","date","new.cases","average_temperature",
             "mobility_retail_and_recreation","mobility_grocery_and_pharmacy",
             "mobility_parks","mobility_transit_stations","mobility_workplaces",
             "mobility_residential")) %>%
    filter(date >= start_date) %>%
    arrange(fips, date)
  
  #NOTE: remove territories explicitly now, rather than passively in filtering 
  # to locations with available Google data, as before
  fips_county_states = daily_covid_covariate_dat %>%
    select(c("fips","county_state","state")) %>%
    filter(!is.na(state)) %>%
    distinct() %>%
    filter(!(state %in% c("Puerto Rico","Virgin Islands")))
  
  #fix issue where county_state & state are NA for date before first covid case
  daily_covid_covariate_dat = daily_covid_covariate_dat %>%
    select(-county_state,-state) %>%
    inner_join(fips_county_states, by=c("fips")) %>%
    relocate(county_state, state, .after=fips)
  
  #do filtering for enough days & total cases before end_date_incidental
  unq_county_fips = daily_covid_covariate_dat %>%
    filter(date <= end_date_output) %>%
    select(fips,new.cases) %>%
    filter(!is.na(new.cases)) %>%
    group_by(fips) %>%
    summarize(
      fips = last(fips),
      n_days_of_cases = length(new.cases),
      total_cases = sum(new.cases)
    ) %>%
    filter(n_days_of_cases >= min_num_days) %>%
    filter(total_cases >= min_num_cases) %>%
    select(fips) 
  unq_county_fips = unq_county_fips$fips
  
  daily_covid_covariate_dat = daily_covid_covariate_dat %>%
    filter(fips %in% unq_county_fips) %>%
    filter(date <= end_date_incidental) 
  
  print(sprintf("%d counties left for just 50 states + DC after applying case count filters on end_date",length(unq_county_fips)))
  
  static_covariate_dat = get_county_pops(unq_county_fips) %>%
    left_join(fips_county_states, by=c("fips")) %>%
    relocate(county_state, state, .after=fips)

  static_covariate_dat = get_county_groupings(static_covariate_dat)

  ##### Estimate weekly infection incidence and growth rates using incidental
  weekly_covid_dat = estimate_county_incidence(daily_covid_covariate_dat, num_cores=num_cores)
  
  weekly_covariate_dat = make_weekly_dynamic_covariate_data(daily_covid_covariate_dat)
  weekly_covid_covariate_dat = weekly_covid_dat %>%
    right_join(weekly_covariate_dat, by=c("fips","county_state","state","week","last_date")) %>%
    arrange(fips,week) %>%
    filter(last_date <= end_date_impute) 
    
  grouping_dat = static_covariate_dat %>%
    select(fips, region, subregion, csa_state_other)
  
  ##### impute missing temp and mobility data
  weekly_covariate_imp_dat = weekly_covid_covariate_dat %>%
    select(c("fips","week","average_temperature",
             "mobility_retail_and_recreation","mobility_grocery_and_pharmacy",
             "mobility_parks","mobility_transit_stations","mobility_workplaces",
             "mobility_residential"
    )) %>%
    mutate(std_week = (week - mean(week)) / sd(week))
  
  imputed_weekly_preds = impute_dynamic_data(weekly_covariate_imp_dat, 
                                             static_covariate_dat,
                                             num_imps=num_imps, 
                                             num_mice_iters=num_mice_iters, 
                                             seed=seed_for_mice, 
                                             num_cores=num_cores)
  
  #join imputations back in with data. keep the earliest data even before
  # first recorded cases, for use in lagging mobility!
  final_imputed_data = weekly_covid_covariate_dat %>%
    select(-c("average_temperature","mobility_retail_and_recreation","mobility_grocery_and_pharmacy",
              "mobility_parks","mobility_transit_stations","mobility_workplaces","mobility_residential")) %>%
    inner_join(imputed_weekly_preds, by=c("fips","week")) %>%
    inner_join(grouping_dat, by=c("fips")) %>%
    relocate(region, subregion, csa_state_other, imputation, last_date, .after=state) %>%
    rename(csa = csa_state_other) %>%
    filter(last_date <= end_date_output)
  
  ## Average over all imputations and write out
  weekly_avgimp_dat = final_imputed_data %>% 
    group_by(fips, county_state, state, region, subregion, csa, last_date, week) %>%
    summarize(
      #outcomes: these were not actually imputed (so values are same per imputation)
      infections_mean = mean(infections_mean),
      infections_2.5 = mean(infections_2.5),
      infections_25 = mean(infections_25),
      infections_75 = mean(infections_75),
      infections_97.5 = mean(infections_97.5),
      
      log_growth_rate = mean(log_growth_rate_mean),
      log_growth_rate_2.5 = mean(log_growth_rate_2.5),
      log_growth_rate_25 = mean(log_growth_rate_25),
      log_growth_rate_75 = mean(log_growth_rate_75),
      log_growth_rate_97.5 = mean(log_growth_rate_97.5),
      
      log_popest = mean(log_popest),
      
      #covariates: these were actually imputed
      average_temperature = mean(average_temperature),
      mobility_retail_and_recreation = mean(mobility_retail_and_recreation),
      mobility_grocery_and_pharmacy = mean(mobility_grocery_and_pharmacy),
      mobility_parks = mean(mobility_parks),
      mobility_workplaces = mean(mobility_workplaces),
      mobility_transit_stations = mean(mobility_transit_stations),
      mobility_residential = mean(mobility_residential)
    ) %>% ungroup()
  
  write_csv(weekly_avgimp_dat, file.path(path_for_modeling_data, "fully_imputed_data.csv"))

  # Add in weekly SG and mask data
  sg_mobility_dat = get_sg_mobility_data(unq_county_fips, start_date)
  write_csv(sg_mobility_dat, file.path(path_for_modeling_data, "sg_data.csv"))

  mask_dat = get_mask_data(unq_county_fips, weekly_avgimp_dat)
  write_csv(mask_dat, file.path(path_for_modeling_data, "mask_data.csv"))

  weekly_avgimp_dat = weekly_avgimp_dat %>% 
                        left_join(sg_mobility_dat, by=c("fips","week")) %>% 
                        left_join(mask_dat, by=c("state","week"))
  write_csv(weekly_avgimp_dat, file.path(path_for_modeling_data, "fully_processed_data.csv"))
  
  print("Save complete! Data has been processed and ready to be loaded for modeling!")
}
