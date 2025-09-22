#' Calculates the density of emissions observations
#' @param dataset Emissions data from either the best source or top performers, must have a column named 'emissions'
#' @param xvals ordered sequence of emissions to define emission densities along.
#' @param up optional upper limit to bound density, default is Inf.
#' @param low optional lower limit to bound density, default is 0.
#' @param bw optional bandwidth, default is NULL in which case bw = sd(emissions)*n^(-2/5), where n is number of emissions.
#' @param kernel kernel choice for density function, default is 'gamma' defined on (0,Inf). Other options include c("gaussian1","gaussian2","beta1","beta2","fb","fbl","fbu","rigaussian")
#' @references Bandwidth is set following
#' @returns a list containing two tibbles, Obs_onPoint with exact emission observations and densities, and obs_den_df with densities for every position given in xvals.
#' @export
obs_density=function(dataset,xvals,up=Inf,low=0,kernel='gamma',bw=NULL){
  if (is.null(bw)){
    bw=stats::sd(dataset$emissions)*nrow(dataset)^(-2/5)
  }
  Obs_onPoint=np::npuniden.boundary(X=dataset$emissions,Y=dataset$emissions,
                                a=low,b=up,proper=TRUE,kertype = kernel,h=bw)
  obs_den_df=np::npuniden.boundary(X=dataset$emissions,Y=xvals,a=low,b=up,
                               proper=TRUE,kertype = kernel,h=bw)
  obs_den_df=tibble::tibble(x=xvals,y=obs_den_df$f)
  Obs_onPoint=tibble::tibble(emissions=dataset$emissions,ydens=Obs_onPoint$f)
  test_int=sfsmisc::integrate.xy(obs_den_df$x,obs_den_df$y)
  if (abs(test_int-1)>0.05){
    warning("density distribution does not integrate to 1, consider adjusting bandwidth or kernel choice")
  }
  output=list(Obs_onPoint=Obs_onPoint,obs_den_df=obs_den_df)
  return(output)
}
