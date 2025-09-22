#' Calculate UPL assuming normally distributed emissions data
#' @param dataset Emissions data from either the best source or top performers, must have a column named 'emissions'
#' @param future_tests Integer of future runs to use in prediction, the default is 3 since compliance uses 1 test average of 3 runs.
#' @param significance Level of significance from 0 to 1, the default is 0.99.
#' @returns upper predictive limit at significance level for the average of the number of future test runs
#' @export
Normal_UPL=function(dataset,future_tests=3,significance=0.99){
  n=length(dataset$emissions)
  df=n-1
  tscore=stats::qt(significance,df)
  emission_mean=mean(dataset$emissions)
  var.s=sum((dataset$emissions-emission_mean)^2)*(1/(n-1))
  PI99_norm=emission_mean+tscore*sqrt(var.s*(1/n+1/future_tests))
  return(PI99_norm)
}
