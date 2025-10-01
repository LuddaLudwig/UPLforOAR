#' Tests for convergence in likelihood parameters
#' @param jags_model_run The output list returned from run_likelihood(),
#' which includes the jags model 'run_results', likelihood distribution type,
#' @returns A tibble of parameters and convergence results from gelman.diag(),
#' Values much greater than 1 indicate problems in convergence.
#'
converge_likelihood=function(jags_model_run){
  distribution=jags_model_run$distribution
  gelman_list=list()
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
  }
  results=tibble::tibble(params=params_list,gelman_diag=gelman_list)
  results$distr=rep(distribution,nrow(results))
  return(results)
}
