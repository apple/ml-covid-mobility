#
# For licensing see accompanying LICENSE file.
# Copyright (C) 2020 Apple Inc. All Rights Reserved.
#

#!/usr/bin/env Rscript
library(argparse)
library(tidyverse)
library(brms)
library(tidybayes)
source("model_argparser.R")
source("utils_brms.R")
source("utils_build_basis.R")
source("utils_data.R")
source("utils_extract_coefs.R")
source("utils_post_process.R")
source("utils_r2.R")
source("utils.R")

parser = ArgumentParser(prog = 'PROG')
args = parse_args(parser)
args = correct_parsed_types(args)
dat = prepare_data(args)
model_formula = build_model_formula(args)

fit = brm(model_formula, 
            data = as.data.frame(dat$weekly_dat),
        	  chains = args$n_chains, 
            cores = args$n_chains, 
           	iter = args$n_iter, 
            warmup = args$n_warmup,
            thin = args$n_thin,
           	backend = "cmdstanr",
            threads = threading(args$n_threads),
           	control = list(adapt_delta = args$adapt_delta, 
           	               max_treedepth = args$max_treedepth))

# Print MCMC object
print(prior_summary(fit))
print(summary(fit))

# Save model file and get R2's and plots
save_dir = args$save_dir
make_dir(save_dir)
saveRDS(fit, file = file.path(save_dir, "fitted_model.rds"))

compute_and_save_r2(args, save_dir, dat, fit)
