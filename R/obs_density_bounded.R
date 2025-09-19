#' Calculates the density of emissions observations
#' @param dataset Emissions data from either the best source or top performers, must have a column named 'emissions'
#' @param up upper limit for possible range of emissions data
#' @param low lower limit for possible range of emissions data, default is 0.
#' @param xvals ordered sequence of emissions to define emission densities along
#' @references Salvatore D. Tomarchio, Antonio Punzo, Modelling the Loss Given Default Distribution via a Family of Zero-and-one Inflated Mixture Models, Journal of the Royal Statistical Society Series A: Statistics in Society, Volume 182, Issue 4, October 2019, Pages 1247–1266, https://doi.org/10.1111/rssa.12466
#' @returns a list containing two tibbles, Obs_onPoint with exact emission observations and densities, and obs_den_df with densities for every position given in xvals.
#' @export
obs_density_bounded=function(dataset,up=1,low=0,xvals){
  bw=sd(dataset$emissions)*nrow(dataset)^(-2/5)
  invisible(capture.output(obs_density_test<-bde::bde(dataset$emissions,xvals,estimator = 'betakernel',
                       lower.limit=low,upper.limit=up,b=bw)))
  int_test=sfsmisc::integrate.xy(obs_density_test@dataPointsCache,
                        obs_density_test@densityCache)
  int_complete=abs(int_test-1)>0.05
  if (int_complete==FALSE){
    invisible(capture.output(obs_density1<-bde::bde(dataset$emissions,dataset$emissions,estimator = 'betakernel',
                     lower.limit=low,upper.limit=up,b=bw)))
    invisible(capture.output(obs_density2<-bde::bde(dataset$emissions,xvals,estimator = 'betakernel',
                     lower.limit=low,upper.limit=up,b=bw)))
    Obs_onPoint=tibble(emissions=obs_density1@dataPointsCache,
                       ydens=obs_density1@densityCache)
    obs_den_df=tibble(x=obs_density2@dataPointsCache,
                      y=obs_density2@densityCache)
  } else {
    while (int_complete){
      if ((int_test-1)>0){
        bw=bw*1.25
        invisible(capture.output(obs_density_test<-bde::bde(dataset$emissions,xvals,estimator = 'betakernel',
                             lower.limit=low,upper.limit=up,b=bw)))
        int_test=sfsmisc::integrate.xy(obs_density_test@dataPointsCache,
                              obs_density_test@densityCache)
        int_complete=abs(int_test-1)>0.05
      } else if ((int_test-1)<0){
        bw=bw/1.25
        invisible(capture.output(obs_density_test<-bde::bde(dataset$emissions,xvals,estimator = 'betakernel',
                             lower.limit=low,upper.limit=up,b=bw)))
        int_test=sfsmisc::integrate.xy(obs_density_test@dataPointsCache,
                              obs_density_test@densityCache)
        int_complete=abs(int_test-1)>0.05
      }
      invisible(capture.output(obs_density1<-bde::bde(dataset$emissions,dataset$emissions,estimator = 'betakernel',
                       lower.limit=low,upper.limit=up,b=bw)))
      invisible(capture.output(obs_density2<-bde::bde(dataset$emissions,xvals,estimator = 'betakernel',
                       lower.limit=low,upper.limit=up,b=bw)))
      Obs_onPoint=tibble(emissions=obs_density1@dataPointsCache,
                         ydens=obs_density1@densityCache)
      obs_den_df=tibble(x=obs_density2@dataPointsCache,
                        y=obs_density2@densityCache)
    }
  }
  output=list(Obs_onPoint=Obs_onPoint,obs_den_df=obs_den_df)
  return(output)
}
