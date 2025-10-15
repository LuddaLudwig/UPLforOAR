#' Tests for convergence in likelihood parameters
#' @param jags_model_run The output list returned from [run_likelihood()].
#' @returns A tibble of parameters and convergence results from [coda::gelman.diag()],
#' Values greater than 1.2 indicate problems in convergence. Values between 1.1
#' and 1.2 indicate weak convergence. Values less than 1.1 indicate good
#' convergence. This test indicates if the 3 mcmc chains are will mixed and
#' stable but is insufficient as the only indicator of convergence. Visual plots
#' of posterior distributions should be investigated as well.
#' @description
#' Gelman-Rubin convergence tests for each defining parameter in the likelihood
#' distribution.
#' @export
#'
converge_likelihood=function(jags_model_run){
  distribution=jags_model_run$distribution
  gelman_list=c()
  convYN=c()
  if (distribution=="Skewed"){
    params_list=c('omega','xi','alpha')
  }
  if (distribution=="Normal"){
    params_list=c('emission_mean','emission_sd')
  }
  if (distribution=="Lognormal"){
    params_list=c('u_ln','sd_ln')
  }
  if (distribution=='Gamma'){
    params_list=c('rate_em','shape_em')
  }
  if (distribution=='Beta'){
    params_list=c('alpha_em','beta_em')
  }
  for (i in 1:length(params_list)){
    param=params_list[i]
    result=coda::gelman.diag(coda::as.mcmc.list(jags_model_run$run_results,
                                                vars=param))
    gelman_list[i]=result$psrf[1]
    if (result$psrf[1]>1.2){
      convYN[i]="No"
    } else if (result$psrf[1]<1.1){
      convYN[i]="Yes"
    } else if ((result$psrf[1]>1.1)&(result$psrf[1]<1.2)){
      convYN[i]="Weak convergence"
    }
  }
  results=tibble::tibble(params=params_list,
                         gelman_diag=gelman_list,
                         convYN=convYN)
  distr_names=tibble::tibble(distr=rep(distribution,nrow(results)))
  results=cbind(distr_names,results)
  return(results)
}
