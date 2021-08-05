#
# For licensing see accompanying LICENSE file.
# Copyright (C) 2020 Apple Inc. All Rights Reserved.
#

#' Create piecewise constant basis based on knot dates. 
#'  
#' @param knot_dates List of dates for p.w. constant knot locations
#' @param unq_dates  List of all unique dates in the whole dataset
#' 
#' @return length(unq_dates) * (length(knot_dates) + 1) matrix of 1s and 0s denoting whether a date is in wave i
#' 
create_pwc_basis = function(knot_dates, unq_dates) {
    n_knots = length(knot_dates)
    basis = matrix(0, nrow = length(unq_dates), ncol = n_knots + 1) # number of waves
    augmented_knot_dates = c(as.Date('2000-01-01'), as.Date(knot_dates), as.Date('2100-01-01'))
    for (i in 1:(n_knots + 1)) {
      wave_i = (unq_dates <= augmented_knot_dates[i + 1]) * (unq_dates > augmented_knot_dates[i])
      basis[wave_i == 1, i] = 1      
    }
    return(basis)
}


#' Create spline basis.
#'  
#' @param spline_dim Dimension of natural spline
#' @param unq_dates List of all unique dates in the whole dataset
#' @param spline_type Optional argument denoting type of spline (M or natural)
#' 
#' @return length(unq_dates) * spline_dim matrix 
create_spline_basis = function(spline_dim, unq_dates, spline_type = "mSpline") {
  if (!(spline_type %in% c("mSpline","ns"))) {
    stop("Spline type not implemented!")
  }
  if (spline_type=="ns") {
    basis = ns(unq_dates, spline_dim, intercept=TRUE)
  }
  if (spline_type=="mSpline") {
    #Cubic splines if dim is at least 4
    basis = splines2::mSpline(unq_dates, df = spline_dim, intercept=TRUE, 
                              Boundary.knots = range(unq_dates), 
                              degree = min(3,spline_dim-1))
  }
  return(basis)
}



#' Build new variables based on knot dates / spline dim
#' 
#' @param args List of parsed args
#' @param df Tibble with variables to create features 
#' @param unq_dates Unique dates in full basis
#' 
#' @return List with data with extra columns for the bases, as well as the basis matrix
build_new_features = function(args, df, unq_dates) {
  ## edge case: for models where we want dynamic grouping, but NO basis, no time-varying effect
  if (is.null(args$spline_dim) & is.null(args$knot_dates)) {
    return(list(df = df, basis = NULL))
  }
  
  vars = args$dyn_vars
  stopifnot(sum(!(vars %in% colnames(df))) == 0) # all variables must be in dataset
    
  dyn_covs_expanded_dat = list()
  unq_fips = unique(df$fips)

  if (is.null(args$spline_dim)) {
    n_knots = length(args$knot_dates)
    stopifnot(n_knots > 0)
    basis = create_pwc_basis(args$knot_dates, unq_dates)
  } else {
    n_knots = args$spline_dim - 1
    basis = create_spline_basis(args$spline_dim, unq_dates, args$spline_type)
  }

 # create fips-level covariates based on basis  
 for (i in 1:length(unq_fips)) {
    this_dat = df %>% filter(fips == unq_fips[i])
    ## extract basis 
    inds = unq_dates %in% this_dat$last_date
    this_basis = basis[inds, ]
    
    this_basis_list = list()
    for (var_i in 1:length(vars)) {
      var_x_rep = do.call(cbind, rep(list(this_dat[, vars[var_i]]), n_knots + 1))
      var_x_basis = var_x_rep * this_basis  
      this_basis_list[[var_i]] = var_x_basis
    }
    this_basis_dat = do.call(cbind, this_basis_list)

    # name cols based on original variables     
    dyn_covs_expanded_names = c()
    for (this_var in vars) {
      dyn_covs_expanded_names = c(dyn_covs_expanded_names, paste0(this_var, "_", 1:(n_knots + 1)))
    }

    colnames(this_basis_dat) = dyn_covs_expanded_names
    this_basis_dat = tibble(data.frame(this_basis_dat)) %>% mutate(fips = this_dat$fips, last_date = this_dat$last_date) 
    dyn_covs_expanded_dat[[i]] = this_basis_dat

  } # end over fips

  dyn_covs_expanded_dat = bind_rows(dyn_covs_expanded_dat)
  df_expanded = inner_join(df, dyn_covs_expanded_dat, by=c("fips", "last_date"))
  stopifnot(nrow(df) == nrow(df_expanded)) 
  return(list(df = df_expanded, basis = basis))
}

#' Build basis and associated transformed variables.
#' 
#' @param dat 
#' @param args input args 
#' 
#' @return List with expanded data that now includes variables for the basis,
#'  the basis itself, dates used to create basis, and locs / scales for variable transforms.
build_basis = function(dat, args) { 
  weekly_dat = dat$dat
  unq_dates = sort(unique(weekly_dat$last_date))
  df_expanded = build_new_features(args, weekly_dat, unq_dates)
  dat = list(weekly_dat = df_expanded$df, basis = df_expanded$basis, 
             unq_dates = unq_dates, loc_scale_factors = dat$loc_scale_factors)
  return(dat)
}

