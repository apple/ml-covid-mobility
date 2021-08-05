# It’s complicated: characterizing the time-varying relationship between cell phone mobility and COVID-19 spread in the US

This code accompanies the research paper, [It’s complicated: characterizing the time-varying relationship between cell phone mobility and COVID-19 spread in the US](https://www.medrxiv.org/content/10.1101/2021.04.24.21255827v1). In this paper, we propose an interpretable statistical model to identify spatiotemporal variation in the association between mobility and COVID-19 infection rates.

## Introduction

This model is intended to help explain the relationship between human mobility (as captured by cell phones, and recorded by Google) and the spread of SARS-CoV2 / COVID-19 in the United States. In fall 2020, when we started this investigation, it was unclear to what extent these mobility variables were associated with the spread of the virus.  Although it was clear that unprecedented drops in mobility in March and April of 2020 preceded declining infection rates, there was limited evidence about whether this strong correlation held over time, and to what extent it varied by location. Our linear mixed effects model with a time-varying coefficient for the effect of mobility addresses this question. 

We focus on the weekly log growth rate of infections, i.e. log( infections_t / infections_{t-1} ), where t denotes the week.  To estimate weekly infections, we utilize our previously developed methodology, described in full in [this preprint](https://www.medrxiv.org/content/10.1101/2020.10.16.20212753v1.full), in estimating infection incidence from reported case data.  

## Getting Started 

This code is designed to work with R version 4.0.2 along with the following packages:

```
argparse:2.0.3, bayesplot:1.7.2, brms:2.14.0, cmdstanr:0.1.3, covidcast:0.3.0, doParallel:1.0.15, ggdist:2.2.0, gridExtra:2.3, incidental:0.1, lubridate:1.7.9, mice:3.11.0, readxl:1.3.1, rstan:2.21.2, scales:1.1.1, splines2:0.4.1, tictoc:1.0, tidybayes:2.1.1, tidyverse:1.3.0, usmap:0.5.1.
```

### Running the Code 

We provide tools to reproduce the main analyses in our paper. Specifically, we provide functionality to 1) download and preprocess publicly available COVID-19 and Google mobility data and 2) fit statistical models to explore the time-varying relationship between cell phone mobility and COVID-19 spread. The following code snippet can be used to download and preprocess data.
```
source("utils_data_get.R")
save_dir = "./data/"
num_cores = 4
load_process_estimate_impute_save_data(save_dir, num_cores = num_cores)
```

We provide the script `fit_model.R` to sample from the posterior distribution for a class of models that relate COVID-19 spread and mobility. For example, 

```
Rscript fit_model.R 
```

fits the main model model in our paper and saves the model file to `./model_output`. The RMarkdown file `examples.md` shows different R2 disaggregations and produces the main figures from our paper. 

## Data Sources

We use [Google's Commmunity Mobility Reports](https://www.google.com/covid19/mobility/) as our data source for mobility. Their mobility is aggregated at the US County level each day into 6 coarse categories: retail & recreation, grocery & pharmacy, parks, transit stations, workplaces, and residential. See [here](https://support.google.com/covid19-mobility/answer/9825414?hl=en&ref_topic=9822927) for more information on the data.

We also obtain information on county-level temperature from [COVID-19 Open Data](https://github.com/GoogleCloudPlatform/covid-19-open-data), county-level population estimates from [this repo](https://github.com/JieYingWu/COVID-19_US_County-level_Summaries) of COVID-19 US county-level summaries, COVID-19 case counts from the [New York Times](https://github.com/nytimes/covid-19-data) and [New York City Department of Health](https://github.com/nychealth/coronavirus-data), mask-wearing adherence from the [New York Times](https://github.com/nytimes/covid-19-data/tree/master/mask-use), [Pew Research](https://www.pewresearch.org/fact-tank/2020/06/23/most-americans-say-they-regularly-wore-a-mask-in-stores-in-the-past-month-fewer-see-others-doing-it/), and the [Delphi](https://github.com/cmu-delphi/delphi-epidata) API for epidemiological data. 

### Preprocessing

We use RIDE, the Robust Incidence Deconvolution Estimator, to estimate the unknown true number of infections each day per US county, using the [incidental R package](https://cran.r-project.org/web/packages/incidental/index.html). We use multiple imputation and the [mice R package](https://cran.r-project.org/web/packages/mice/index.html) to impute missing covariate values (i.e. mobility values) for each county at the weekly level; see the paper supplement and the `utils_data_get.R` script in this repo for additional details. 

We exclude counties with less than 250 total COVID-19 cases as of the last date considered, February 20, 2021, which removes 176 counties. Next, we exclude counties with extreme growth patterns, where any weekly absolute growth rate exceeds 2 (removing 8 counties), or absolute growth rates exceeds 1.5 and the county has less than 50, 000 people (removing 8 counties). These restrictions remove outliers that arise from difficult to model events, such as prison outbreaks in sparsely populated counties.

## References 

It’s complicated: characterizing the time-varying relationship between cell phone mobility and COVID-19 spread in the US.
Sean Jewell, Joseph Futoma, Lauren Hannah, Andrew C. Miller, Nicholas J. Foti, Emily B. Fox
medRxiv 2021.04.24.21255827; doi: https://doi.org/10.1101/2021.04.24.21255827

Statistical deconvolution for inference of infection time series.
Andrew C. Miller, Lauren Hannah, Joseph Futoma, Nicholas J. Foti, Emily B. Fox, Alexander D’Amour, Mark Sandler, Rif A. Saurous, Joseph A. Lewnard 
medRxiv 2020.10.16.20212753; doi: https://doi.org/10.1101/2020.10.16.20212753
