#' Runs JAGS model scripts for chosen likelihood with a hierarchical group structure
#' @description
#' Runs the JAGS model from [setup_likelihood()] model_code output using [runjags::run.jags()].
#' Traces all variables in `par_list` running 3 chains in parallel with `burnin` and
#' `adapt` of 10,000 each and keeping 10,000 iterations per chain. Rather than
#' independent runs defining the population distribution, hierarchical
#' dependency within groups is allowed with the
#' group-level distribution parameters drawn from the overall population distribution.
#' @returns `runjags` object named `run_results`, likelihood distribution from the
#' JAGS model script, as well as `data` and `xval`s used as inputs.
#' @param model_input Results from [setup_likelihoodGroup()], including JAGS model
#' script, emissions data, distribution, initial values list, and parameters to
#' monitor.
#' @param future_runs Integer of future runs to use in prediction, the default
#' is `3` since compliance uses 1 test average of 3 runs.
#' @param xvals Ordered sequence of emissions at which to predict probability
#' density. Default is `NULL`, in which case `x_hat` is a 1024 length
#' sequence between `0` and `3 * max(data$emissions)` or `minY` and `maxY` if
#' they are specified.
#' @param maxY The maximum emission value possible, used to truncate likelihood
#' distributions and set upper ranges on prior distributions.
#' Default is `NULL`, in which case it is calculated as `3 * maximum(data$emissions)`.
#' @param minY The minimum emission value possible, used to truncate likelihood
#' distributions. Default is 0.
#' @param group Character string corresponding to the variable name in the data
#' set by which to group for the hierarchical structure. If the group is not a
#' factor it will be coerced using as.factor(). To avoid having unknown factor
#' levels, please convert to factor first
#' (set using `as.factor(data$group_name)` if needed). Defaults to `'sources'`.
#' @export
run_likelihoodGroup = function(model_input, xvals = NULL, minY = 0,
                          maxY = NULL, future_runs = 3, group = 'sources'){
  future_runs = as.integer(future_runs)
  if (!is.integer(future_runs)){
    stop("future_runs must be a positive integer")
  }
  if (future_runs < 1){
    stop("future_runs must be a positive integer")
  }
  manual_prior = model_input$manual_prior
  data = model_input$data
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = FALSE)
  verify_install = runjags::testjags(silent = TRUE)
  if (!verify_install$JAGS.available){
    stop('Please install JAGS')
  }
  if (is.null(maxY)){
    maxY = 3 * max(data$emissions)
  }
  if ((model_input$distribution == 'Beta' ) & (maxY > 1)){
    stop('Cannot use beta distribution with max emissions greater than 1')
  }
  if (!is.factor(data[[group]])){
    warning('Group variable is not a factor and will be coerced using as.factor()')
    group_var = as.factor(data[[group]])
  } else {
    group_var = data[[group]]
  }
  n_groups = length(unique(group_var))
  n.adapt = 10000
  n.update = 10000
  n.iter = 10000
  if (is.null(xvals)){
    xvals = seq(minY, maxY, length.out = 1024)
  }
  if ((model_input$distribution == 'Beta') & (max(xvals) > 1)){
    stop('Cannot use beta distribution with max xvals greater than 1')
  }
  if (!manual_prior){
    data_list = list(emission_xi = data$emissions, n_groups = n_groups,
                     group_j = group_var,
                     n_draws = future_runs, minY = minY,
                     sdOfLogY = stats::sd(log(data$emissions), na.rm = T),
                     maxY = maxY, sdY = stats::sd(data$emissions),
                     meanOfLogY = mean(log(data$emissions), na.rm = T),
                     pi = pi, x_hat = xvals, n_x_hat = length(xvals))
  } else if (manual_prior){
    if (length(model_input$prior_list) == 4){
      data_list = list(emission_xi = data$emissions, n_groups = n_groups,
                       n_draws = future_runs, minY = minY, group_j = group_var,
                       sdOfLogY = stats::sd(log(data$emissions), na.rm = T),
                       maxY = maxY, sdY = stats::sd(data$emissions),
                       meanOfLogY = mean(log(data$emissions), na.rm = T),
                       pi = pi, x_hat = xvals, n_x_hat = length(xvals),
                       low1 = model_input$prior_list[1],
                       up1 = model_input$prior_list[2],
                       low2 = model_input$prior_list[3],
                       up2 = model_input$prior_list[4])
    } else if (length(model_input$prior_list) == 6){
      data_list = list(emission_xi = data$emissions, n_groups = n_groups,
                       n_draws = future_runs, minY = minY, group_j = group_var,
                       sdOfLogY = stats::sd(log(data$emissions), na.rm = T),
                       maxY = maxY, sdY = stats::sd(data$emissions),
                       meanOfLogY = mean(log(data$emissions), na.rm = T),
                       pi = pi, x_hat = xvals, n_x_hat = length(xvals),
                       low1 = model_input$prior_list[1],
                       up1 = model_input$prior_list[2],
                       low2 = model_input$prior_list[3],
                       up2 = model_input$prior_list[4],
                       low3 = model_input$prior_list[5],
                       up3 = model_input$prior_list[6])
    }
  }
  cl3 = parallel::makeCluster(3)
  rjm = suppressWarnings(runjags::run.jags(model = model_input$model_code,
                                           data = data_list,
                                           monitor = model_input$par_list,
                                           method = 'rjparallel',
                                           summarise = FALSE, cl = cl3,
                                           n.chains = length(model_input$dat_inits),
                                           burnin = n.update,
                                           adapt = n.adapt, sample = n.iter,
                                           inits = model_input$dat_inits))
  output = list(run_results = rjm, distribution = model_input$distribution,
                manual_prior = manual_prior, maxY = maxY, minY = minY,
                data = model_input$data, xvals = xvals, future_runs = future_runs)
  parallel::stopCluster(cl3)
  return(output)
}
