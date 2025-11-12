#' Calculate UPL assuming normally distributed emissions data
#' @param data Data from either the best source or top performers,
#' must have a column with numeric `emissions`.
#' @param future_runs Integer of future runs to use in prediction, the default
#' is `3` since compliance uses 1 test average of 3 runs.
#' @param significance Level of significance from 0 to 1, the default is `0.99`.
#' @param emissions variable name or column number corresponding to the
#' emissions used for selecting top performing sources.
#' @returns Upper predictive limit (UPL) at significance level for the average
#' of the number of future test runs.
#' @export
Normal_UPL = function(data, emissions, future_runs = 3, significance = 0.99){
  future_runs = as.integer(future_runs)
  if (!is.integer(future_runs)){
    stop("future_runs must be a positive integer")
  }
  if (future_runs < 1){
    stop("future_runs must be a positive integer")
  }
  if (significance >= 1){
    stop("significance must be greater then 0 and less than 1")
  }
  if (significance <= 0){
    stop("significance must be greater then 0 and less than 1")
  }
  data_temp = tibble::tibble(emissions = data[[emissions]])
  n = length(data_temp$emissions)
  if (n < 3){
    stop("Need at least 3 observations for UPL calculation")
  }
  df = n - 1
  tscore = stats::qt(significance, df)
  emission_mean = mean(data_temp$emissions)
  var.s = sum((data_temp$emissions - emission_mean)^2) * (1 / (n - 1))
  if (var.s == 0){
    stop("Cannot perform UPL calculation on data with 0 variance")
  }
  UPL_normal = emission_mean + tscore * sqrt(var.s * (1 / n + 1 / future_runs))
  return(UPL_normal)
}
