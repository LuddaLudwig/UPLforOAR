#' Tests for convergence in likelihood parameters with a hierarchical group structure
#' @param jags_model_run The output list returned from [run_likelihoodGroup()].
#' @returns A tibble of parameters and convergence results from [coda::gelman.diag()],
#' Values greater than 1.2 indicate problems in convergence. Values between 1.1
#' and 1.2 indicate weak convergence. Values less than 1.1 indicate good
#' convergence. This test indicates if the 3 mcmc chains are will mixed and
#' stable but is insufficient as the only indicator of convergence. Visual plots
#' of posterior distributions should be investigated as well.
#' @description
#' Gelman-Rubin convergence tests for each defining parameter in the likelihood
#' distribution. Rather than independent runs defining the population
#' distribution, hierarchical dependency within groups is allowed with the
#' group-level distribution parameters drawn from the overall population distribution.
#' @export
#'
converge_likelihoodGroup = function(jags_model_run){
  distribution = jags_model_run$distribution
  if (distribution == "Skewed"){
    params_list = c('pop_xi_mu', 'pop_omega_mu',
                    'pop_alpha_mu',  'pop_xi_sd', 'pop_omega_sd',
                    'pop_alpha_sd', 'omega', 'xi', 'alpha')
  }
  if (distribution == "Normal"){
    params_list = c('pop_mu_mu', 'pop_sd_mu', 'pop_mu_sd', 'pop_sd_sd',
                    'group_sd', 'group_mu')
  }
  if (distribution == "Lognormal"){
    params_list = c('pop_mu_mu', 'pop_sd_mu', 'pop_mu_sd', 'pop_sd_sd',
                    'group_sd', 'group_mu')
  }
  if (distribution == 'Gamma'){
    params_list = c('pop_rate_mu', 'pop_shape_mu', 'pop_rate_sd', 'pop_shape_sd',
                    'group_rate', 'group_shape')
  }
  if (distribution == 'Beta'){
    params_list = c('pop_alpha_mu', 'pop_beta_mu', 'pop_alpha_sd', 'pop_beta_sd',
                    'group_alpha', 'group_beta')
  }
  n_groups = length(unique(jags_model_run$data[[jags_model_run$group]]))
  n_pop_params = length(params_list) / 3 * 2
  params = params_list[1:n_pop_params]
  for (j in 1:(n_pop_params/2)){
    params = c(params, sprintf(paste0(params_list[n_pop_params+j], '[%s]'),
                       seq(1:n_groups)))
  }
  gelman_list = c()
  for (i in 1:length(params)){
    param = params[i]
    result = coda::gelman.diag(
      coda::as.mcmc.list(jags_model_run$run_results, vars = param))
    gelmans = as.vector(result$psrf[, 1])
    gelman_list = c(gelman_list, gelmans)
  }
  results = tibble::tibble(params = params, gelman_diag = gelman_list)
  results$convYN = NA
  results$convYN = replace(results$convYN, results$gelman_diag > 1.2, "No")
  results$convYN = replace(results$convYN, results$gelman_diag < 1.1, "Yes")
  results$convYN = replace(results$convYN,
                           ((result$gelman_diag > 1.1) &
                              (result$gelman_diag < 1.2)), "Weak convergence")
  distr_names = tibble::tibble(distr = rep(distribution, nrow(results)))
  results = cbind(distr_names, results)
  return(results)
}
