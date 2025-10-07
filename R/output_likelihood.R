#' Organizes mcmc output from run_likelihood()
#' @param significance Level of significance from 0 to 1, the default is 0.99.
#' @param jags_model_run The output list returned from run_likelihood(),
#' which includes the jags model 'run_results', likelihood distribution type,
#' and data, xvals, and future_tests used as inputs to run_likelihood().
#' @returns A list including 'distr' the distribution used in write_likelihood(),
#' the 'predicted_mean' the mean of the fitted distribution, 'UPL_Bayes' the
#' upper predictive limit based on the 'significance' level and average
#' distribution of 'future_tests' number of draws, 'obs_pdf' the predicted
#' probability density at each observation, and 'pred_pdf' the predicted
#' probability density at each point in xvals.
#' @description
#' Output_likelihood() takes the jags_model_run produced by run_likelihood(),
#' merges the mcmc chains and calculates the UPL as well as
#' providing the predicted pdf and metrics.
#'
output_likelihood=function(jags_model_run,significance=0.99){
  distribution=jags_model_run$distribution
  data=jags_model_run$data
  future_tests=jags_model_run$future_tests
  xvals=jags_model_run$xvals
  if (distribution=="Skewed"){
    xi_quant=as.matrix(runjags::combine.mcmc(
      coda::as.mcmc.list(jags_model_run$run_results, vars="xi")))
    omega_quant=as.matrix(runjags::combine.mcmc(
      coda::as.mcmc.list(jags_model_run$run_results,vars="omega")))
    alpha_quant=as.matrix(runjags::combine.mcmc(
      coda::as.mcmc.list(jags_model_run$run_results,vars="alpha")))
    hat_quant=matrix(nrow=length(xi_quant),ncol=future_tests,data=NA)
    pdf_obs=matrix(ncol=nrow(data),nrow=length(xi_quant),data=NA)
    pdf_hat=matrix(ncol=length(xvals),nrow=length(xi_quant),data=NA)
    for (i in 1:length(xi_quant)){
      set.seed(12)
      Fy_sn=sn::dsn(xvals,xi=(xi_quant[i]),
                omega=(omega_quant[i]),alpha=(alpha_quant[i]))
      set.seed(12)
      pdf_obs[i,]=sn::dsn(data$emissions,xi=(xi_quant[i]),
                      omega=(omega_quant[i]),alpha=(alpha_quant[i]))
      pdf_hat[i,]=Fy_sn
      if (all(Fy_sn==0)){
        for (k in 1:future_tests){
          hat_quant[i,k]=NA
        }
      } else {
        for (k in 1:future_tests){
          set.seed(12)
          hat_quant[i,k]=sample(x=xvals,size=1,prob=Fy_sn,replace=T)
        }
      }
    }
    hat_quant=tibble::as_tibble(hat_quant, .name_repair='minimal')
  } else {
      pdf_obs=as.matrix(runjags::combine.mcmc(
        coda::as.mcmc.list(jags_model_run$run_results,vars="pdf_obs")))
      hat_quant=tibble::as_tibble(as.matrix(runjags::combine.mcmc(
        coda::as.mcmc.list(jags_model_run$run_results,vars="emission_hat"))))
      pdf_hat=as.matrix(runjags::combine.mcmc(
        coda::as.mcmc.list(jags_model_run$run_results, vars="pdf_hat")))
  }
  names(hat_quant)=sprintf('run%s',seq(1:future_tests))
  run3_mean=rowMeans(hat_quant)
  pred_99_3rep=stats::quantile(as.matrix(stats::na.omit(run3_mean)),
                               probs=c(significance))

  pdf_hat_quant=matrixStats::colQuantiles(pdf_hat,probs=c(0.5))
  density_hat=tibble::tibble(pdf_hat=pdf_hat_quant,x_hat=xvals)
  density_hat=subset(density_hat,is.finite(density_hat$pdf_hat))
  pdf_obs_quant=tibble::as_tibble(
    matrixStats::colQuantiles(pdf_obs,probs = c(0.025,0.5,0.975)),
    .name_repair='minimal')
  names(pdf_obs_quant)=c('low','med','up')
  pdf_obs_quant$emissions=data$emissions
  density_hat$distr=rep(distribution,nrow(density_hat))
  pdf_obs_quant$distr=rep(distribution,nrow(pdf_obs_quant))
  pred_mean=mean(hat_quant$run1,na.rm=TRUE)
  output=list("predicted_mean"=pred_mean,"UPL_Bayes"=pred_99_3rep,
              "obs_pdf"=pdf_obs_quant,'pred_pdf'=density_hat,distr=distribution)
  return(output)
}
