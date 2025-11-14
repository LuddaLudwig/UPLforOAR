#' Sets up path to JAGS script with a hierarchical group structure,
#' initial values, and variable list to monitor
#' @description
#' This function defines the jagsmodel script to call based on the selected
#' distribution. It also defines the initial values and variables to monitor.
#' Rather than independent runs defining the population
#' distribution, hierarchical dependency within groups is allowed with the
#' group-level distribution parameters drawn from the overall population distribution.
#' @param distribution Any of `'Normal'`, `'Gamma'`, `'Skewed'`, `'Lognormal'`,
#' or `'Beta'`. If using a custom model script, set as `'Custom'`.
#' @param data Data from either the best source or top performers,
#' must have a column with numeric `emissions` and a column with character or
#' factor `group` used for hierarchical structure.
#' @param emissions Variable name or column number corresponding to the
#' emissions used for selecting top performing sources.
#' @param group Variable name or column number corresponding to the variable name in the data
#' set by which to group for the hierarchical structure. If the group is not a
#' factor it will be coerced using as.factor(). To avoid having unknown factor
#' levels, please convert to factor first.
#' (set using `as.factor(data$group_name)` if needed). Defaults to `'sources'`.
#' @param manual_prior Default is `FALSE`, priors are uninformative and calculated
#' from range of emissions data. if `TRUE` priors should be specified manually in
#' `prior_list`.
#' @param prior_list Optional list of [stats::dunif()] upper and lower bounds for prior
#' distributions. For `'Normal'` and `'Lognormal'` they are ordered
#' `c(pop_sd_mu_low, pop_sd_mu_high, pop_mu_mu_low, pop_mu_mu_high, pop_sd_sd_low, pop_sd_sd_high, pop_mu_sd_low, pop_mu_sd_high)`.
#' For `'Skewed'` they are ordered
#' `c(pop_omega_mu_low, pop_omega_mu_high, pop_xi_mu_low, pop_xi_mu_high, pop_alpha_mu_low, pop_alpha_mu_high, pop_omega_sd_low, pop_omega_sd_high,  pop_xi_sd_low, pop_xi_sd_high, pop_alpha_sd_low, pop_alpha_sd_high)`.
#' For `'Gamma'` they are ordered
#' `c(pop_rate_mu_low, pop_rate_mu_high, pop_shape_mu_low, pop_shape_mu_high, pop_rate_sd_low, pop_rate_sd_high, pop_shape_sd_low, pop_shape_sd_high)`.
#' For `'Beta'` they are ordered
#' `c(pop_alpha_mu_low, pop_alpha_mu_high, pop_beta_mu_low, pop_beta_mu_high, pop_alpha_sd_low, pop_alpha_sd_high, pop_beta_sd_low, pop_beta_sd_high)`.
#' @param random Default is `FALSE` where random seeds are defined via `.RNG.name`
#' and `.RNG.seed` and returned as `state` so JAGS runs will be exactly reproducible.
#' Changing to `TRUE` will generate new random states to use for `.RNG.name` and
#' `.RNG.state` instead, also returned as `state` so the results can be
#' recreated exactly if desired.
#' @param RNG.state Optional setting to specify a list of three lists setting the
#' `.RNG.name` and `.RNG.state` for each MCMC chain. The default is a fixed set of
#' RNG states so the results are always reproducible. If `random = TRUE` the RNG
#' state is set randomly instead.
#' @param custom_model String for the file path location and name
#' (i.e. "working_directory/Custom_JAGS.R") if using a custom JAGS model script.
#' @param custom_params List of parameters to monitor in addition to
#' `c('emission_hat', 'pdf_obs', 'pdf_hat', 'group_emiss')` if using a custom model.
#' @param custom_init List of three lists with initial values for MCMC chains
#' corresponding to parameters in `custom_params`.
#' @returns Object `model_code`, which is a string for the written R script that
#' JAGS can call, `par_list` which is the list of parameters traced while running
#' the JAGS model, `dat_inits` which is a list of initial parameter values and
#' random seeds for 3 chains, and the distribution used in likelihood model. Also
#' included are a data set with `emissions` and `group` and `prior_list` if
#' applicable to be passed along for use in [run_likelihoodGroup()].
#' @export
setup_likelihoodGroup = function(distribution, data, emissions,
                                 manual_prior = FALSE, group = 'sources',
                                 prior_list = NULL, random = FALSE,
                                 RNG.state = NULL,
                                 custom_model = NULL, custom_params = NULL,
                                 custom_init = NULL){
  JAGS_path = system.file("JAGS", package = "UPLforOAR", mustWork = TRUE)
  data_temp = tibble::tibble(emissions = data[[emissions]],
                             group = data[[group]])
  if (any(is.na(data_temp$emissions))){
    warning('Emissions with NA values have been removed')
    data_temp = na.omit(data_temp)
  }
  if (any(is.na(data_temp$group))){
    warning('Groups with NA values have been removed')
    data_temp = na.omit(data_temp)
  }
  if (!is.numeric(data_temp$emissions)){
    stop("Emissions must be numeric")
  }
  mu = mean(data_temp$emissions)
  sigma = stats::sd(data_temp$emissions)
  maxX = max( data_temp$emissions)
  if (sigma == 0){
    stop("Cannot calculate UPL with zero variance data")
  }
  if (!is.character(emissions)){
    emissions = colnames(data)[emissions]
  }
  if (!is.character(group)){
    group = colnames(data)[group]
  }
  data_names = c(emissions, group)
  if (is.null(RNG.state)){
    RNG.state = list(list(".RNG.name" = "base::Wichmann-Hill",
                          ".RNG.seed" = 5),
                     list(".RNG.name" = "base::Marsaglia-Multicarry",
                          ".RNG.seed" = 12),
                     list(".RNG.name" = "base::Super-Duper",
                          ".RNG.seed" = 151))
  }
  if(!manual_prior){
    if (distribution == 'Custom'){
      JAGS_model = runjags::read.jagsfile(custom_model)
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat', 'group_emiss',
                   custom_params)
      data_inits_temp = list(custom_init[[1]],
                             custom_init[[2]],
                             custom_init[[3]])
    }
    if (distribution == "Normal"){
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/EmissionGroup_normal_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'pop_mu_mu', 'pop_sd_mu', 'pop_mu_sd', 'pop_sd_sd',
                   'group_sd', 'group_mu', 'group_emiss')
      data_inits_temp = list(
        list('pop_mu_mu' = mu, 'pop_sd_mu' = sigma,
             'pop_mu_sd' = sigma, 'pop_sd_sd' = sigma),
        list('pop_mu_mu' = 1.5 * mu, 'pop_sd_mu' = 0.5 * sigma,
             'pop_mu_sd' = 0.1 * sigma, 'pop_sd_sd' = 10 * sigma),
        list('pop_mu_mu' = 0.5 * mu, 'pop_sd_mu' = 1.5 * sigma,
             'pop_mu_sd' = 10 * sigma, 'pop_sd_sd' = 0.1 * sigma))
    } else if (distribution == "Lognormal"){
      ln_emiss = log(data_temp$emissions)
      ln_mu = mean(ln_emiss, na.rm = TRUE)
      ln_sig = stats::sd(ln_emiss, na.rm=  TRUE)
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/EmissionGroup_lnorm_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'pop_mu_mu', 'pop_sd_mu', 'pop_mu_sd', 'pop_sd_sd',
                   'group_sd', 'group_mu', 'group_emiss')
      data_inits_temp = list(
        list('pop_mu_mu' = ln_mu, 'pop_sd_mu' = ln_sig,
             'pop_mu_sd' = ln_sig, 'pop_sd_sd' = ln_sig),
        list('pop_mu_mu' = 1.5 * ln_mu, 'pop_sd_mu' = 0.5 * ln_sig,
             'pop_mu_sd' = 0.1 * ln_sig, 'pop_sd_sd' = 10 * ln_sig),
        list('pop_mu_mu' = 0.5 * ln_mu, 'pop_sd_mu' = 1.5 * ln_sig,
             'pop_mu_sd' = 10 * ln_sig, 'pop_sd_sd' = 0.1 * ln_sig))
    } else if (distribution == "Skewed"){
      skew1 = min(0.99, abs((1 / length(data_temp$emissions)) *
                              sum(((data_temp$emissions - mu) / sigma)^3)))
      delta = sqrt((pi / 2) * ((abs(skew1)^(2 / 3)) /
                                 ((abs(skew1)^(2 / 3)) + ((4 - pi) / 2)^(2 / 3))))
      delta = delta * abs(skew1) / skew1
      alpha = delta / sqrt(1 - delta^2)
      omega = sigma / sqrt(1 - 2 * delta^2 / pi) #must be positive
      xi = mu - omega * delta * sqrt(2 / pi)
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/EmissionGroup_skewed_JAGS.R'))
      par_list = c('omega', 'xi', 'alpha', 'pop_xi_mu', 'pop_omega_mu',
                   'pop_alpha_mu',  'pop_xi_sd', 'pop_omega_sd',
                   'pop_alpha_sd')
      data_inits_temp = list(
        list('pop_xi_mu' = xi, 'pop_omega_mu' = omega, 'pop_alpha_mu' = alpha,
             'pop_xi_sd' = maxX / 10,  'pop_omega_sd' = 0.1 * maxX,
             'pop_alpha_sd' = 5),
        list('pop_xi_mu' = 1.5 * xi, 'pop_omega_mu' = 0.5 * omega,
             'pop_alpha_mu' = 0.5 * alpha,
             'pop_xi_sd' = maxX / 100,  'pop_omega_sd' = 0.5 * maxX, 'pop_alpha_sd' = 10),
        list('pop_xi_mu' = 0.5 * xi, 'pop_omega_mu' = 1.5 * omega,
             'pop_alpha_mu' = 1.5 * alpha,
             'pop_xi_sd' = maxX / 50,  'pop_omega_sd' = 0.01 * maxX,
             'pop_alpha_sd' = 1))
    } else if (distribution == 'Gamma'){
      shape = mu^2 / sigma^2
      rate = mu / sigma^2
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/EmissionGroup_gamma_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'pop_rate_mu', 'pop_shape_mu', 'pop_rate_sd', 'pop_shape_sd',
                   'group_rate', 'group_shape', 'group_emiss')
      data_inits_temp = list(
        list('pop_rate_mu' = rate, 'pop_shape_mu' = shape,
             'pop_rate_sd' = 0.1 * rate, 'pop_shape_sd' = 0.1 * shape),
        list('pop_rate_mu' = 1.5 * rate, 'pop_shape_mu' = 0.5 * shape,
             'pop_rate_sd' = 0.5 * rate, 'pop_shape_sd' = 0.5 * shape),
        list('pop_rate_mu' = 0.5 * rate, 'pop_shape_mu' = 1.5 * shape,
             'pop_rate_sd' = 0.3 * rate, 'pop_shape_sd' = 0.3 * shape))
    } else if (distribution == 'Beta'){
      if (min(data_temp$emissions) < 0){
        stop('Cannot use beta distribution with emissions less than 0')
      }
      if (max(data_temp$emissions) > 1){
        stop('Cannot use beta distribution with emissions greater than 1')
      }
      alpha = mu^2 / sigma^2 - mu^3 / sigma^2 - mu
      beta = mu - 1 + mu / sigma^2 + mu^3 / sigma^2 - 2 * mu^2 / sigma^2
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/EmissionGroup_beta_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'pop_alpha_mu', 'pop_beta_mu', 'pop_alpha_sd', 'pop_beta_sd',
                   'group_alpha', 'group_beta', 'group_emiss')
      data_inits_temp = list(
        list('pop_beta_mu' = beta, 'pop_alpha_mu' = alpha,
             'pop_beta_sd' = 0.1 * beta, 'pop_alpha_sd' = 0.5 * alpha),
        list('pop_beta_mu' = 1.5 * beta, 'pop_alpha_mu' = 0.5 * alpha,
             'pop_beta_sd' = 0.3 * beta, 'pop_alpha_sd' = 0.3 * alpha),
        list( 'pop_beta_mu' = 0.5 * beta, 'pop_alpha_mu' = 1.5 * alpha,
             'pop_beta_sd' = 0.5 * beta, 'pop_alpha_sd' = 0.1 * alpha))
    }
    if (random){
      new_state = parallel.seeds('base::BaseRNG', 3)
    } else if (!random){
      new_state = RNG.state
    }
    data_inits = list(c(data_inits_temp[[1]], new_state[[1]]),
                      c(data_inits_temp[[2]], new_state[[2]]),
                      c(data_inits_temp[[3]], new_state[[3]]))
    output = list(model_code = JAGS_model, par_list = par_list, data = data_temp,
                  distribution = distribution, dat_inits = data_inits,
                  manual_prior = manual_prior, data_names = data_names,
                  state = new_state)
  } else if (manual_prior){
    if (is.null(prior_list)){
      stop('Please provide a list of upper and lower bounds for manual priors')
    }
    if (distribution == "Normal"){
      if (any(c(prior_list[1:2], prior_list[5:8]) <= 0)){
        stop('Prior limits must be positive')
      }
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/Manual_emissionGroup_normal_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'pop_mu_mu', 'pop_sd_mu', 'pop_mu_sd', 'pop_sd_sd',
                   'group_sd','group_mu','group_emiss')
      data_inits_temp = list(
        list('pop_mu_mu' = mean(c(prior_list[3], prior_list[4])),
             'pop_sd_mu' = mean(c(prior_list[1], prior_list[2])),
             'pop_mu_sd' = mean(c(prior_list[7], prior_list[8])),
             'pop_sd_sd' = mean(c(prior_list[5], prior_list[6]))),
        list('pop_mu_mu' = 0.9 * prior_list[4],
             'pop_sd_mu' = 0.9 * prior_list[2],
             'pop_mu_sd' = 0.9 * prior_list[8],
             'pop_sd_sd' = 0.9 * prior_list[6]),
        list('pop_mu_mu' = 1.1 * prior_list[3],
             'pop_sd_mu' = 1.1 * prior_list[1],
             'pop_mu_sd' = 1.1 * prior_list[7],
             'pop_sd_sd' = 1.1 * prior_list[5]))
    } else if (distribution == "Lognormal"){
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/Manual_emissionGroup_lnorm_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'pop_mu_mu', 'pop_sd_mu', 'pop_mu_sd', 'pop_sd_sd',
                   'group_sd','group_mu','group_emiss')
      if (any(c(prior_list[1:2], prior_list[5:8]) <= 0)){
        stop('Prior limits must be positive')
      }
      if (prior_list[4] < 0){
        initial1 = 1.1 * prior_list[4]
      } else if (prior_list[4] > 0){
        initial1 = 0.9 * prior_list[4]
      }
      if (prior_list[3] < 0){
        initial2 = 0.9 * prior_list[3]
      } else if (prior_list[3] > 0){
        initial2 = 1.1 * prior_list[3]
      }
      data_inits_temp = list(
        list('pop_mu_mu' = mean(c(prior_list[3], prior_list[4])),
             'pop_sd_mu' = mean(c(prior_list[1], prior_list[2])),
             'pop_mu_sd' = mean(c(prior_list[7], prior_list[8])),
             'pop_sd_sd' = mean(c(prior_list[5], prior_list[6]))),
        list('pop_mu_mu' = initial1,
             'pop_sd_mu' = 0.9 * prior_list[2],
             'pop_mu_sd' = 0.9 * prior_list[8],
             'pop_sd_sd' = 0.9 * prior_list[6]),
        list('pop_mu_mu' = initial2,
             'pop_sd_mu' = 1.1 * prior_list[1],
             'pop_mu_sd' = 1.1 * prior_list[7],
             'pop_sd_sd' = 1.1 * prior_list[5]))
    } else if (distribution == "Skewed"){
      if (any(c(prior_list[1:2], prior_list[7:12]) <= 0)){
        stop('Prior limits must be positive')
      }
      if (prior_list[4] < 0){
        initial1 = 1.1 * prior_list[4]
      } else if (prior_list[4] > 0){
        initial1 = 0.9 * prior_list[4]
      }
      if (prior_list[3] < 0){
        initial2 = 0.9 * prior_list[3]
      } else if (prior_list[3] > 0){
        initial2 = 1.1 * prior_list[3]
      }
      if (prior_list[6] < 0){
        initial3 = 1.1 * prior_list[6]
      } else if (prior_list[6] > 0){
        initial3 = 0.9 * prior_list[6]
      }
      if (prior_list[5] < 0){
        initial4 = 0.9 * prior_list[5]
      } else if (prior_list[5] > 0){
        initial4 = 1.1 * prior_list[5]
      }
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/Manual_emissionGroup_skewed_JAGS.R'))
      par_list = c('omega', 'xi', 'alpha', 'pop_omega_mu', 'pop_omega_sd',
                   'pop_xi_mu', 'pop_xi_sd', 'pop_alpha_mu', 'pop_alpha_sd')
      data_inits_temp = list(
        list('pop_xi_mu' = mean(c(prior_list[3], prior_list[4])),
             'pop_omega_mu' = mean(c(prior_list[1], prior_list[2])),
             'pop_alpha_mu' = mean(c(prior_list[5], prior_list[6])),
             'pop_xi_sd' = mean(c(prior_list[9], prior_list[10])),
             'pop_omega_sd' = mean(c(prior_list[7], prior_list[8])),
             'pop_alpha_sd' = mean(c(prior_list[11], prior_list[12]))),
        list('pop_xi_mu' = initial1,
             'pop_xi_sd' = 1.1 * prior_list[9],
             'pop_omega_mu' = 1.1 * prior_list[1],
             'pop_omega_sd' = 1.1 * prior_list[7],
             'pop_alpha_mu' = initial4,
             'pop_alpha_sd' = 1.1 * prior_list[11]),
        list('pop_xi_mu' = initial2, 'pop_xi_sd' = 0.9 * prior_list[10],
             'pop_omega_mu' = 0.9 * prior_list[2],
             'pop_omega_sd' = 0.9 * prior_list[8],
             'pop_alpha_mu' = initial3),
             'pop_alpha_sd' = 0.9 * prior_list[12])
    } else if (distribution == 'Gamma'){
      if (any(prior_list <= 0)){
        stop('Prior limits must be positive')
      }
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/Manual_emissionGroup_gamma_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'pop_rate_mu', 'pop_shape_mu', 'pop_rate_sd', 'pop_shape_sd',
                   'group_rate', 'group_shape', 'group_emiss')
      data_inits_temp = list(
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 5,
             'pop_rate_mu' = mean(c(prior_list[1], prior_list[2])),
             'pop_shape_mu' = mean(c(prior_list[3], prior_list[4])),
             'pop_rate_sd' = mean(c(prior_list[5], prior_list[6])),
             'pop_shape_sd' = mean(c(prior_list[7], prior_list[8]))),
        list('pop_rate_mu' = 1.1 * prior_list[1],
             'pop_shape_mu' = 1.1 * prior_list[3],
             'pop_rate_sd' = 1.1 * prior_list[5],
             'pop_shape_sd' = 1.1 * prior_list[7]),
        list('pop_rate_mu' = 0.9 * prior_list[2],
             'pop_shape_mu' = 0.9 * prior_list[4],
             'pop_rate_sd' = 0.9 * prior_list[6],
             'pop_shape_sd' = 0.9 * prior_list[8]))
    } else if (distribution == 'Beta'){
      if (min(data_temp$emissions) < 0){
        stop('Cannot use beta distribution with emissions less than 0')
      }
      if (max(data_temp$emissions) > 1){
        stop('Cannot use beta distribution with emissions greater than 1')
      }
      if (any(prior_list[5:8] <= 0)){
        stop('Prior limits must be positive')
      }
      if (prior_list[2] < 0){
        initial1 = 1.1 * prior_list[2]
      } else if (prior_list[2] > 0){
        initial1 = 0.9 * prior_list[2]
        if (initial1 < (-1)){
          initial1 = -0.99999
        }
      }
      if (prior_list[1] < 0){
        initial2 = 0.9 * prior_list[1]
      } else if (prior_list[1] > 0){
        initial2 = 1.1 * prior_list[1]
        if (initial2 < (-1)){
          initial2 = -0.99999
        }
      }
      if (prior_list[4] < 0){
        initial3 = 1.1 * prior_list[4]
      } else if (prior_list[4] > 0){
        initial3 = 0.9 * prior_list[4]
        if (initial3 < (-1)){
          initial3 = -0.99999
        }
      }
      if (prior_list[3] < 0){
        initial4 = 0.9 * prior_list[3]
      } else if (prior_list[3] > 0){
        initial4 = 1.1 * prior_list[3]
        if (initial4 < (-1)){
          initial4 = -0.99999
        }
      }
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/Manual_emissionGroup_beta_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'pop_alpha_mu', 'pop_beta_mu', 'pop_alpha_sd', 'pop_beta_sd',
                   'group_alpha', 'group_beta', 'group_emiss')
      data_inits_temp = list(
        list('pop_beta_mu' = mean(c(prior_list[3], prior_list[4])),
             'pop_alpha_mu' = mean(c(prior_list[1], prior_list[2])),
             'pop_beta_sd' = mean(c(prior_list[7], prior_list[8])),
             'pop_alpha_sd' = mean(c(prior_list[5], prior_list[6]))),
        list('pop_beta_mu' = initial3,
             'pop_alpha_mu' = initial1,
             'pop_beta_sd' = 0.9 * prior_list[8],
             'pop_alpha_sd' = 0.9 * prior_list[6]),
        list('pop_beta_mu' = initial4,
             'pop_alpha_mu' = initial2,
             'pop_beta_sd' = 1.1 * prior_list[7],
             'pop_alpha_sd' = 1.1 * prior_list[5]))
    }
    if (random){
      new_state = parallel.seeds('base::BaseRNG', 3)
    } else if (!random){
      new_state = RNG.state
    }
    data_inits = list(c(data_inits_temp[[1]], new_state[[1]]),
                      c(data_inits_temp[[2]], new_state[[2]]),
                      c(data_inits_temp[[3]], new_state[[3]]))
    output = list(model_code = JAGS_model, par_list = par_list, data = data_temp,
                  manual_prior = manual_prior, distribution = distribution,
                  dat_inits = data_inits, prior_list = prior_list,
                  data_names = data_names, state = new_state)
  }
  return(output)
}
