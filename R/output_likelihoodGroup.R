#' Organizes mcmc output from [run_likelihoodGroup()] with a hierarchical group structure
#' @param significance Level of significance from 0 to 1, the default is `0.99`.
#' @param jags_model_run The output list returned from [run_likelihoodGroup()],
#' which includes the jags model `run_results`, likelihood distribution type,
#' and `data`, `xvals`, and `future_runs` used as inputs to [run_likelihoodGroup()].
#' @returns A list including `distr`, the distribution used in [write_likelihoodGroup()],
#' `predicted_mean`, the mean of the fitted distribution, `UPL_Bayes`, the
#' upper predictive limit based on the `significance` level and average
#' distribution of `future_runs` number of draws, `obs_pdf`, the predicted
#' probability density at each observation, and `pred_pdf`, the predicted
#' probability density at each point in `xvals`.
#' @description
#' Output_likelihood() takes the `jags_model_run` produced by [run_likelihoodGroup()],
#' merges the mcmc chains and calculates the UPL as well as
#' providing the predicted pdf and metrics. Rather than
#' independent runs defining the population distribution, hierarchical
#' dependency within groups is allowed with the
#' group-level distribution parameters drawn from the overall population distribution.
#' @export
output_likelihoodGroup = function(jags_model_run, significance = 0.99){
  if (significance >= 1){
    stop("significance must be greater then 0 and less than 1")
  }
  if (significance <= 0){
    stop("significance must be greater then 0 and less than 1")
  }
  distribution = jags_model_run$distribution
  data = jags_model_run$data
  future_runs = jags_model_run$future_runs
  xvals = jags_model_run$xvals
  minY = jags_model_run$minY
  maxY = jags_model_run$maxY
  group = jags_model_run$group
  if (distribution == "Skewed"){
    xi_pop = as.matrix(runjags::combine.mcmc(
      coda::as.mcmc.list(jags_model_run$run_results, vars = "pop_xi_mu")))
    omega_pop = as.matrix(runjags::combine.mcmc(
      coda::as.mcmc.list(jags_model_run$run_results, vars = "pop_omega_mu")))
    alpha_pop = as.matrix(runjags::combine.mcmc(
      coda::as.mcmc.list(jags_model_run$run_results, vars = "pop_alpha_mu")))
    xi_group = as.matrix(runjags::combine.mcmc(
      coda::as.mcmc.list(jags_model_run$run_results, vars = "xi")))
    omega_group = as.matrix(runjags::combine.mcmc(
      coda::as.mcmc.list(jags_model_run$run_results, vars = "omega")))
    alpha_group = as.matrix(runjags::combine.mcmc(
      coda::as.mcmc.list(jags_model_run$run_results, vars = "alpha")))
    hat_quant = matrix(nrow = length(xi_pop), ncol = future_runs, data = NA)
    pdf_obs = matrix(ncol = nrow(data), nrow = length(xi_pop), data = NA)
    pdf_hat = matrix(ncol = length(xvals), nrow = length(xi_pop), data = NA)
    for (i in 1:length(xi_pop)){
      set.seed(12)
      Fy_sn = sn::dsn(xvals, xi = (xi_pop[i]),
                      omega = (omega_pop[i]),
                      alpha = (alpha_pop[i]))
      set.seed(12)
      pdf_obs[i,] = sn::dsn(data$emissions, xi = (xi_pop[i]),
                            omega = (omega_pop[i]),
                            alpha = (alpha_pop[i]))
      pdf_hat[i,] = Fy_sn
      if (all(Fy_sn == 0)){
        for (k in 1:future_runs){
          hat_quant[i,k] = NA
        }
      } else {
        for (k in 1:future_runs){
          set.seed(12)
          hat_quant[i,k] = sample(x = xvals, size = 1,
                                  prob = Fy_sn, replace = T)
        }
      }
    }
    hat_quant = tibble::as_tibble(hat_quant, .name_repair = 'minimal')
    group_quant = matrix(nrow = nrow(xi_group),
                         ncol = length(unique(data[[group]])), data = NA)
    for (i in 1:nrow(xi_group)){
      for (j in 1:length(unique(data[[group]]))){
        Fy_sn = sn::dsn(xvals, xi = (xi_group[i, j]),
                        omega = (omega_group[i, j]),
                        alpha = (alpha_group[i, j]))
        if (all(Fy_sn == 0)){
          group_quant[i, j] = NA
          } else {
            group_quant[i, j] = sample(x = xvals, size = 1,
                                       prob = Fy_sn, replace = T)
          }
      }
    }
    group_quant=tibble::as_tibble(group_quant)
  } else {
    pdf_obs = as.matrix(runjags::combine.mcmc(
      coda::as.mcmc.list(jags_model_run$run_results, vars = "pdf_obs")))
    hat_quant = tibble::as_tibble(as.matrix(runjags::combine.mcmc(
      coda::as.mcmc.list(jags_model_run$run_results, vars = "emission_hat"))))
    pdf_hat = as.matrix(runjags::combine.mcmc(
      coda::as.mcmc.list(jags_model_run$run_results, vars = "pdf_hat")))
    group_quant=tibble::as_tibble(as.matrix(runjags::combine.mcmc(
      coda::as.mcmc.list(jags_model_run$run_results, vars="group"))))
  }
  names(hat_quant) = sprintf('run%s', seq(1:future_runs))
  run3_mean = rowMeans(hat_quant)
  pred_99_3rep = stats::quantile(as.matrix(stats::na.omit(run3_mean)),
                                 probs = c(significance))
  pdf_hat_quant = matrixStats::colQuantiles(pdf_hat, probs = c(0.5))
  density_hat = tibble::tibble(pdf_hat = pdf_hat_quant, x_hat = xvals)
  density_hat = subset(density_hat, is.finite(density_hat$pdf_hat))
  pdf_obs_quant = tibble::as_tibble(
    matrixStats::colQuantiles(pdf_obs, probs = c(0.025 ,0.5, 0.975)),
    .name_repair = 'minimal')
  names(group_quant) = levels(data[[group]])
  group_long = tidyr::pivot_longer(group_quant, cols = 1:ncol(group_quant),
                          names_to = 'groups',values_to = 'emissions')
  names(pdf_obs_quant) = c('low', 'med', 'up')
  pdf_obs_quant$emissions = data$emissions
  density_hat$distr = rep(distribution, nrow(density_hat))
  pdf_obs_quant$distr = rep(distribution, nrow(pdf_obs_quant))
  pred_mean = mean(hat_quant$run1, na.rm = TRUE)
  output = list("predicted_mean" = pred_mean, "UPL_Bayes" = pred_99_3rep,
                "obs_pdf" = pdf_obs_quant, 'pred_pdf' = density_hat,
                distr = distribution, minY = minY, maxY = maxY,
                group_dat = group_long)
  return(output)
}
