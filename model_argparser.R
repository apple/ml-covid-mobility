#
# For licensing see accompanying LICENSE file.
# Copyright (C) 2020 Apple Inc. All Rights Reserved.
#

#' Parse input args to setup modeling job
#' 
#' @param parser object, an instantiation of ArgumentParser.
#' 
#' @return List of parsed args.
parse_args = function(parser) {
  parser$add_argument('--save_dir', type = 'character', help = 'file path for saving model output',
                      default = "./model_output")
  parser$add_argument('--data_path', type = 'character', help = 'file path for input data',
      default = "./data/fully_processed_data.csv.gz")
  parser$add_argument('--model_type', type = 'character', default = 'main', help = 'Specify model from options: main, pitfall_collinear, pitfall_overflexible_mobility, pitfall_overflexible_temperature, clustering_none_mob_constant, clustering_none_mob_tv, clustering_region_mob_constant, clustering_region_mob_tv, clustering_csa_mob_constant, clustering_csa_mob_tv, sg')
  parser$add_argument('--postprocess', type = 'character', default = 'TRUE')
  parser$add_argument('--get_process_impute_data', type = 'character', default = 'FALSE', help= 'Whether to pull, process and impute data or load from pre-computed data_path')
  mcmc_opt = parser$add_argument_group('mcmc_opt', 'MCMC options')
  mcmc_opt$add_argument('--n_chains', type = 'integer', default = 2)
  mcmc_opt$add_argument('--n_threads', type = 'integer', default = 32)
  mcmc_opt$add_argument('--n_iter', type = 'integer', default = 7000)  
  mcmc_opt$add_argument('--adapt_delta', type = 'double', default = 0.9995)
  mcmc_opt$add_argument('--max_treedepth', type = 'integer', default = 25)
  mcmc_opt$add_argument('--n_warmup', type = 'integer', default = 2000)
  mcmc_opt$add_argument('--n_thin', type = 'integer', default = 5) 
  args = parser$parse_args()
  return(args)
}

#' Create some additional arguments around model type and bases used.
#' 
#' @param args List of parsed args so far.
#' 
#' @return List of parsed args, with new args added based on existing args.
build_basis_args = function(args) {
    args$spline_dim = NULL
    args$spline_type = NULL
    args$knot_dates = NULL
    if (args$model_type %in% c('main', 'clustering_none_mob_tv', 'clustering_region_mob_tv', 'clustering_csa_mob_tv')) {
        args$knot_dates = c('2020-05-23','2020-08-22','2020-11-21')
        args$dyn_vars = 'mobility_pc1_full_dat_avg_lag0_3'
    } else if (args$model_type == 'pitfall_collinear') {
        args$knot_dates = c('2020-05-23','2020-08-22','2020-11-21')
        args$dyn_vars = c('mobility_retail_and_recreation', 'mobility_workplaces')
    } else if (args$model_type == 'pitfall_overflexible_mobility') {
        args$knot_dates = c('2020-04-04', '2020-05-09', '2020-06-06', '2020-07-04', '2020-08-08', '2020-09-05', '2020-10-10', '2020-11-07', '2020-12-05', '2021-01-02', '2021-02-06')
        args$dyn_vars = 'mobility_pc1_full_dat_avg_lag0_3'
    } else if (args$model_type == 'pitfall_overflexible_temperature') {
        args$dyn_vars = c('temperature_smooth_2', 'mobility_pc1_full_dat_avg_lag0_3')
        args$spline_dim = 3
        args$spline_type = 'ns'
    } else if (args$model_type == 'sg') {
        args$knot_dates = c('2020-05-23','2020-08-22','2020-11-21')
        args$dyn_vars = 'completely_home_prop_7dav_lag0_3'
    }
    return(args)
}

#' Create some additional arguments around grouping variables for the hierarchical model.
#' 
#' @param args List of parsed args so far.
#' 
#' @return List of parsed args, with new args added based on existing args.
build_dyn_grouping_args = function(args) {
    if (args$model_type %in% c('main', 'pitfall_collinear', 'pitfall_overflexible_mobility', 'pitfall_overflexible_temperature', 'clustering_csa_mob_constant', 'clustering_csa_mob_tv', 'sg')) {
        args$dyn_grouping = "csa"
    } else if (args$model_type %in% c('clustering_region_mob_constant', 'clustering_region_mob_tv')) {
        args$dyn_grouping = "region"
    } else if (args$model_type %in% c('clustering_none_mob_constant', 'clustering_none_mob_tv')) {
        args$dyn_grouping = NULL
    }
  return(args)
}

#' Build out additional args and convert certain string args to logicals.
#' 
#' @param args List of parsed args so far.
#' 
#' @return List of parsed args, with new and updated args.
correct_parsed_types = function(args) {
  args$postprocess = logical_parser(args$postprocess)
  args$get_process_impute_data = logical_parser(args$get_process_impute_data)
  args = build_basis_args(args)
  args = build_dyn_grouping_args(args)
  return(args)
}
