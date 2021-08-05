#
# For licensing see accompanying LICENSE file.
# Copyright (C) 2020 Apple Inc. All Rights Reserved.
#

#' Make new directory
#' @param dir path string of new directory to create
make_dir = function(dir) {
  if(!dir.exists(dir)) {
      dir.create(dir, recursive=TRUE)
  }
}

#' Cast string of "TRUE"/"FALSE" into logical types
#' @param x string of "TRUE" or "FALSE"
#' @return logical equivalent of input string 
logical_parser = function(x) {
  if(x == "TRUE") {
    return(TRUE)
  } else if (x == "FALSE") {
    return(FALSE)
  } else {
    stop(paste0("Invalid input: ", x))
  }
}

#' Calucalte n day moving average of input array x
#' @param x array 
#' @param n int 
#' @return y array of size dim(x)[[1]], with entry y[i] equal to the average of x[max(i-n+1, 1):i]
#'
moving_average = function(x, n) {
    y = stats::filter(x, rep(1/n, n), sides = 1) 
    y[1:n] = cumsum(x[1:n]) / 1:n
    return(as.numeric(y))
}
