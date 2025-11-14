#' BayesianGroups_UPL() wraps `_likelihoodGroup()` functions into results comparable for
#' multiple distributions with a hierarchical group structure
#' @param distr_list A list including one or more of
#' `c('Normal', 'Skewed', 'Lognormal', 'Gamma', 'Beta')`. Note that if prior
#' bounds are supplied manually, only one distribution can be used.
#' @param future_runs Integer of future runs to use in prediction, the default
#' is `3` since compliance uses 1 test average of 3 runs.
#' @param significance Level of significance from 0 to 1, the default is `0.99`.
#' @param xvals Ordered sequence of emissions at which to predict probability
#' density. Default is `NULL`, in which case `x_hat` is a 1024 length sequence
#' between `0` and `3 * max(data$emissions)`.
#' @param maxY The maximum emission value possible, used to truncate likelihood
#' distributions and set upper ranges on prior distributions, if not specified
#' manually. Default is `NULL`, in which case is is calculated as `3 * max(data$emissions)`.
#' @param minY The minimum emission value possible, used to truncate likelihood
#' distributions. Default is 0.
#' @param data Data from either the best source or top performers,
#' must have a column with numeric `emissions` and a column with character or
#' factor `group` used for hierarchical structure.
#' @param group Variable name or column number corresponding to the variable name in the data
#' set by which to group for the hierarchical structure. If the group is not a
#' factor it will be coerced using as.factor(). To avoid having unknown factor
#' levels, please convert to factor first.
#' @param emissions Variable name or column number corresponding to the
#' emissions used for selecting top performing sources.
#' @param prior_list Optional list of [stats::dunif()] upper and lower bounds for prior
#' distributions. For `'Normal'` and `'Lognormal'` they are ordered
#' `c(pop_sd_mu_low, pop_sd_mu_high, pop_mu_mu_low, pop_mu_mu_high, pop_sd_sd_low, pop_sd_sd_high, pop_mu_sd_low, pop_mu_sd_high)`.
#' For `'Skewed'` they are ordered
#' `c(pop_omega_mu_low, pop_omega_mu_high, pop_xi_mu_low, pop_xi_mu_high, pop_alpha_mu_low, pop_alpha_mu_high, pop_omega_sd_low, pop_omega_sd_high,  pop_xi_sd_low, pop_xi_sd_high, pop_alpha_sd_low, pop_alpha_sd_high)`.
#' For `'Gamma'` they are ordered
#' `c(pop_rate_mu_low, pop_rate_mu_high, pop_shape_mu_low, pop_shape_mu_high, pop_rate_sd_low, pop_rate_sd_high, pop_shape_sd_low, pop_shape_sd_high)`.
#' For `'Beta'` they are ordered
#' `c(pop_alpha_mu_low, pop_alpha_mu_high, pop_beta_mu_low, pop_beta_mu_high, pop_alpha_sd_low, pop_alpha_sd_high, pop_beta_sd_low, pop_beta_sd_high)`.
#' @param convergence_report Default is `FALSE`, if a report containing
#' convergence figures should be generated with results. If `TRUE`, a document
#' Bayesian_UPL_convergence_MMDDYYY_HHMM.pdf will be written to the current
#' working directory. Note that this is an Rmarkdown document that requires
#' either LaTex or MikTek installed in order to render the PDF.
#' @param manual_prior Default is `FALSE`, if priors should be specified manually
#' or be uninformative calculated from range of emissions data. Note that if you
#' are supplying priors manually than you can only run one type of distribution
#' at a time.
#' @param random Default is `FALSE` where random seeds are defined via `.RNG.name`
#' and `.RNG.seed` and returned as `state` so JAGS runs will be exactly reproducible.
#' Changing to `TRUE` will generate new random states to use for `.RNG.name` and
#' `.RNG.state` instead, also returned as `state` so the results can be
#' recreated exactly if desired.
#' @param RNG.state Optional setting to specify a list of three lists setting the
#' `.RNG.name` and `.RNG.state` for each MCMC chain. The default is a fixed set of
#' RNG states so the results are always reproducible. If `random = TRUE` the RNG
#' state is set randomly instead.
#' @param up Argument passed to [obs_density()] inside [fit_likelihood()].
#' Optional upper limit to bound density, default is `Inf`.
#' @param low Argument passed to [obs_density()] inside [fit_likelihood()].
#' Optional lower limit to bound density, default is `0`.
#' @param bw Argument passed to [obs_density()] inside [fit_likelihood()].
#' Optional bandwidth, default is `NULL` in which case
#' `bw = sd(emissions) * n^(-2/5)`, where `n` is number of emissions. The bandwidth
#' can also be provided manually, or searched for using least squares cross-validation
#' by `bw = "cv.ls"` or likelihood cross-validation with `bw = "cv.ml"`.
#' @param kernel Argument passed to [obs_density()] inside [fit_likelihood()].
#' Kernel choice for density function, default is `gamma` defined
#' on `(0,Inf)`. Other options include:
#' `c('gaussian1', 'gaussian2', 'beta1', 'beta2', 'fb', 'fbl', 'fbu', 'rigaussian')`.
#' See [np::npuniden.boundary()] for more information on kernel options.
#' @returns A list of tibble results from [setup_likelihoodGroup()], [run_likelihoodGroup()],
#' [output_likelihoodGroup()], [obs_density()], [fit_likelihood()], and
#' [converge_likelihoodGroup()] for each distribution in `distr_list`.
#' @export
#' @description
#' For each distribution in `distr_list`, [BayesianGroups_UPL()] will [setup_likelihoodGroup()],
#' [run_likelihoodGroup()], organize mcmc results in [output_likelihoodGroup()], test for
#' convergence of likelihood parameters using [converge_likelihoodGroup()], and
#' calculate goodness of fit metrics using [fit_likelihood()]. Rather than
#' independent runs defining the population distribution, hierarchical
#' dependency within groups is allowed with the group-level distribution
#' parameters drawn from the overall population distribution. Results include
#' `$fit_table`: a tibble with the `UPL`, `pdf_integral`, `SSE`, and count of
#' observations within 95 percent CI for each distribution in `'distr_list'`,
#' `$conv_output`: a tibble with the parameters, Gelman-Rubin diagnostics, and if
#' the converged for distribution in `distr_list`, `$obs_pdf_dat`: a tibble with
#' the emissions observations, corresponding observation densities, median,
#' upper, and lower 95 percent CI around predicted densities for the distribution in
#' `distr_list`, and a 1 if the observation is within the 95 percent CI, a 0 otherwise,
#' and `$pred_pdf_dat`:a tibble with the predicted probability density `pdf_hat`,
#' the observation density `ydens` for each value in the range of emissions in
#' `x_hat`. The maximum emission value of distributions, `maxY`, the ordered range
#' emissions to predict to `xvals`, and the prior distributions and initial
#' values are all automatically supplied from the emissions data to be fully
#' encompassing and uninformative by default. They can be supplied manually
#' instead however by supplying `maxY`, `xvals`, or setting `manual_prior = TRUE`
#' with corresponding lower and upper limits in `prior_list`. If manual priors
#' are used, only a single distribution can be run at a time in `distr_list`.
#'
BayesianGroups_UPL = function(distr_list = c('Normal', 'Skewed', 'Lognormal', 'Gamma', 'Beta'),
                        data, emissions, group = 'sources',
                        future_runs = 3, significance = 0.99,
                        xvals = NULL, maxY = NULL, minY = 0,
                        RNG.state = NULL, up = Inf, low = 0,
                        kernel = 'gamma', bw = NULL,
                        convergence_report = FALSE, random = FALSE,
                        manual_prior = FALSE, prior_list = NULL){
  if (convergence_report == TRUE){
    figs_list = list()
  }
  mod_output_list = c()
  conv_output = tibble::tibble()
  if (manual_prior){
    if (length(distr_list) > 1){
      stop('You can only run one distribution at a time if supplying priors manually')
    }
    distribution = distr_list[1]
    mod_bayes = setup_likelihoodGroup(distribution = distribution, data = data,
                                      emissions = emissions, group = group,
                                      manual_prior = manual_prior,
                                      RNG.state = RNG.state,
                                      random = random, prior_list = prior_list)
    mod_run = run_likelihoodGroup(model_input = mod_bayes, maxY = maxY, minY = minY,
                                  future_runs = future_runs, xvals = xvals)
    manual_prior = mod_bayes$manual_prior
    mod_output = output_likelihoodGroup(jags_model_run = mod_run,
                                        significance = significance)
    mod_fit = fit_likelihood(likelihood_result = mod_output, up = up, low = low,
                             kernel = kernel, bw = bw)
    mod_output_list[[1]] = mod_fit
    mod_converge = converge_likelihoodGroup(mod_run)
    conv_output = rbind(conv_output, mod_converge)
    if (convergence_report == TRUE){
      fig_set = converge_figs(distribution, mod_run)
      figs_list[[1]] = fig_set
      rm(fig_set)
    }
    rm(mod_run, mod_output, mod_fit)
    gc()
  }
  if (!manual_prior){
    for (j in 1:length(distr_list)){
      distribution = distr_list[j]
      if (random == TRUE){
        if (j == 1){
          random = TRUE
        } else if (j > 1){
          random = FALSE
          RNG.state = mod_output_list[[j-1]]$state
        }
      }
      mod_bayes = setup_likelihoodGroup(distribution = distribution, data = data,
                                        emissions = emissions, group = group,
                                        RNG.state = RNG.state,
                                        manual_prior = FALSE, random = random)
      mod_run = run_likelihoodGroup(model_input = mod_bayes, maxY = maxY,
                                    minY = minY, future_runs = future_runs,
                                    xvals = xvals)
      mod_output = output_likelihoodGroup(jags_model_run = mod_run,
                                          significance = significance)
      mod_fit = fit_likelihood(likelihood_result = mod_output, up = up, low = low,
                               kernel = kernel, bw = bw)
      mod_output_list[[j]] = mod_fit
      mod_converge = converge_likelihoodGroup(mod_run)
      conv_output = rbind(conv_output, mod_converge)
      if (convergence_report == TRUE){
        fig_set = converge_figs(distribution, mod_run)
        figs_list[[j]] = fig_set
        rm(fig_set)
      }
      rm(mod_run, mod_output, mod_fit)
      gc()
    }
  }
  if (convergence_report == TRUE){
    current_wd = getwd()
    template_path = system.file("templates", package = "UPLforOAR",
                                mustWork = TRUE)
    rmarkdown::render(paste0(template_path, '/convergence_template.Rmd'),
                      output_dir = current_wd,
                      output_file = paste0('BayesianGroup_UPL_convergence_',
                                           format(Sys.time(), "%m%d%Y-%H%M")))
  }
  fit_table = tibble::tibble(distr = unlist(lapply(mod_output_list, '[[','distr')),
                             UPL = (as.numeric(lapply(mod_output_list, '[[','UPL_Bayes'))),
                             SSE = (as.numeric(lapply(mod_output_list, '[[','SSE'))),
                             Obs_in_CI = (as.numeric(lapply(mod_output_list, '[[','good_vals'))),
                             pdf_integral = (as.numeric(lapply(mod_output_list, '[[','pdf_integral')))
  )
  state = mod_output_list[[length(distr_list)]]$state
  obs_pdf_dat = tibble::tibble()
  for (i in 1:length(distr_list)){
    obs_temp = mod_output_list[[i]]$obs_pdf_dat
    obs_pdf_dat = rbind(obs_pdf_dat, obs_temp)
  }
  pred_pdf_dat = tibble::tibble()
  for (i in 1:length(distr_list)){
    pred_temp = mod_output_list[[i]]$xhat_pdf_dat
    pred_pdf_dat = rbind(pred_pdf_dat, pred_temp)
  }
  if (any(conv_output$convYN != "Yes")){
    warning('Some parameters have not converged')
  }
  return_list = list(fit_table = fit_table,
                     conv_output = conv_output,
                     obs_pdf_dat = obs_pdf_dat,
                     pred_pdf_dat = pred_pdf_dat,
                     state = state)
  return(return_list)
}
