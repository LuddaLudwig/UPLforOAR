#' Calculate UPL assuming lognormally distributed emissions data
#' @param data Emissions data from either the best source or top performers,
#' must have a column named 'emissions'
#' @param future_tests Integer of future runs to use in prediction, the default
#' is 3 since compliance uses 1 test average of 3 runs.
#' @param significance Level of significance from 0 to 1, the default is 0.99.
#' @returns upper predictive limit at significance level for the average of the
#' number of future test runs
#' @export
#' @references "An upper prediction limit for the arithmetic mean of a lognormal
#' random variable" authored by Dulal Kumar Bhaumik and Robert David Gibbons 2004
Lognormal_UPL=function(data,future_tests=3,significance=0.99){
  data$ln_emiss=log(data$emissions)
  data$ln_emiss=replace(data$ln_emiss,
                           !is.finite(data$ln_emiss),NA)
  n=length(data$emissions)
  mean_log=mean(data$ln_emiss,na.rm=TRUE)
  sd_log=stats::sd(data$ln_emiss,na.rm=TRUE)
  # using Gram-Charlmier Series A distribution approximation
  beta2=(exp(4*sd_log^2)+2*exp(3*sd_log^2)+3*exp(2*sd_log^2)-3)/(future_tests*(exp(sd_log^2)-1)^2)+3*(1-1/future_tests)
  beta1=sqrt(exp(sd_log^2)-1)*(exp(sd_log^2)+2)/sqrt(future_tests)
  zvals=seq(-5,5,by=0.0101)
  Fgzs=abs((1-(beta1/6)*(3*zvals-zvals^3)+(beta2-3)*(3-6*zvals^2+zvals^4)/24)*stats::dnorm(zvals))
  Fg_cdf=cumsum(Fgzs/sum(Fgzs))
  zscore1=zvals[Fg_cdf>significance][1]
  mean_convert=exp(mean_log+(sd_log^2)/2)
  part2=sqrt(future_tests*exp(2*mean_log+sd_log^2)*(exp(sd_log^2)-1)+future_tests^2*exp(2*mean_log+sd_log^2)*(((sd_log^2)/n)+(sd_log^4)/(2*(n-1))))
  PI99_lnorm=mean_convert+(zscore1/future_tests)*part2
  return(PI99_lnorm)
}
