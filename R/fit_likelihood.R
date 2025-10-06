#' fit_likelihood() calculates the error between fitted density and
#' observed density distributions
#' @description
#' This function takes the list of results from output_likelihood() and compares
#' the predicted density distributions to observed density distributions,
#' estimating the SSE (sum of squared errors) and counts the number of emissions
#' observations with densities that have overlapping 95% CI with predicted densities.
#'
#' @param likelihood_result Output list from output_likelihood()
#' @returns The sum of squared error (SSE) between predicted and observed
#' probability densities, and the count of emissions whose 95% CI around
#' predicted probability densities overlaps observed probability densities.
#' Also includes the integration of predicted pdf, distribution type used, and
#' merged data sets of observed and predicted densities at each emission value
#' and each xval, named obs_pdf_dat and xhat_pdf_dat respectively. The
#' obs_pdf_dat also includes the upper and lower 95% and median around predicted
#' pdf. The UPL estimate from the output_likelihood() as well.
fit_likelihood=function(likelihood_result){
  obs_pdf_temp=likelihood_result$obs_pdf
  pred_pdf_temp=likelihood_result$pred_pdf
  obs_dens_results=obs_density(data=obs_pdf_temp,
                               xvals=pred_pdf_temp$x_hat)
  Obs_onPoint=obs_dens_results$Obs_onPoint
  obs_pdf_dat=dplyr::left_join(Obs_onPoint,obs_pdf_temp,
                               by='emissions',multiple='any')
  obs_pdf_dat$inCI=NA
  for (k in 1:nrow(obs_pdf_dat)){
    if ((obs_pdf_dat$ydens[k]>obs_pdf_dat$low[k])&
        (obs_pdf_dat$ydens[k]<obs_pdf_dat$up[k])){
      obs_pdf_dat$inCI[k]=1
    } else {
      obs_pdf_dat$inCI[k]=0
    }
  }
  obs_den_df=obs_dens_results$obs_den_df
  xhat_pdf_dat=dplyr::full_join(pred_pdf_temp,obs_den_df,by='x_hat')
  SSE=(sum((obs_pdf_dat$ydens-obs_pdf_dat$med)^2))
  pdf_integral=sfsmisc::integrate.xy(pred_pdf_temp$x_hat,pred_pdf_temp$pdf_hat)
  fit_temp=list(distr=likelihood_result$distr,pdf_integral=pdf_integral,
                SSE=SSE,good_vals=sum(obs_pdf_dat$inCI),obs_pdf_dat=obs_pdf_dat,
                xhat_pdf_dat=xhat_pdf_dat,UPL_Bayes=likelihood_result$UPL_Bayes)
  return(fit_temp)
}
