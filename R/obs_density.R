#' Calculates the density of emissions observations
#' @param data Emissions data from either the best source or top performers,
#' must have a column named 'emissions'.
#' @param xvals Ordered sequence of emissions to define emission densities along.
#' Default is NULL, in which case xvals is a 1024 length sequence.
#' between 0 and 3*max(data$emissions)
#' @param up Optional upper limit to bound density, default is Inf.
#' @param low Optional lower limit to bound density, default is 0.
#' @param bw Optional bandwidth, default is NULL in which case
#' bw = sd(emissions)*n^(-2/5), where n is number of emissions.
#' @param kernel Kernel choice for density function, default is 'gamma' defined
#' on (0,Inf). Other options include.
#' c("gaussian1","gaussian2","beta1","beta2","fb","fbl","fbu","rigaussian")
#' @references Bandwidth is set following
#' @returns A list containing two tibbles, Obs_onPoint with exact emission
#' observations and densities, and obs_den_df with densities for every
#' position given in xvals.
#' @export
obs_density=function(data,xvals=NULL,up=Inf,low=0,kernel='gamma',bw=NULL){
  if (is.null(bw)){
    bw=stats::sd(data$emissions)*nrow(data)^(-2/5)
  }
  if (is.null(xvals)){
    xvals=seq(0,3*max(data$emissions),length.out=1024)
  }
  Obs_onPoint=np::npuniden.boundary(X=data$emissions,Y=data$emissions,
                                a=low,b=up,proper=TRUE,kertype = kernel,h=bw)
  obs_den_df=np::npuniden.boundary(X=data$emissions,Y=xvals,a=low,b=up,
                               proper=TRUE,kertype = kernel,h=bw)
  obs_den_df=tibble::tibble(x_hat=xvals,ydens=obs_den_df$f)
  Obs_onPoint=tibble::tibble(emissions=data$emissions,ydens=Obs_onPoint$f)
  test_int=sfsmisc::integrate.xy(obs_den_df$x_hat,obs_den_df$ydens)
  if (abs(test_int-1)>0.05){
    warning("density distribution does not integrate to 1, consider adjusting
            bandwidth or kernel choice")
  }
  output=list(Obs_onPoint=Obs_onPoint,obs_den_df=obs_den_df)
  return(output)
}
