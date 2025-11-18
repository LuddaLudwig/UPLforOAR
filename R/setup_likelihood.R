#' Sets up path to JAGS script, initial values, and variable list to monitor
#' @description
#' This function defines the jagsmodel script to call based on the selected
#' distribution. It also defines the initial values and variables to monitor.
#' @param distribution Any of `'Normal'`, `'Gamma'`, `'Skewed'`, `'Lognormal'`,
#' or `'Beta'`. If using a custom model script, set as `'Custom'`.
#' @param data Data from either the best source or top performers,
#' must have a column with numeric `emissions`.
#' @param emissions variable name or column number corresponding to the
#' emissions used for selecting top performing sources.
#' @param manual_prior Default is `FALSE`, priors are uninformative and calculated
#' from range of emissions data. if `TRUE` priors should be specified manually in
#' `prior_list`.
#' @param prior_list Optional list of [stats::dunif()] upper and lower bounds for prior
#' distributions. For `'Normal'` they are ordered `c(sd_low, sd_high, mean_low, mean_high)`.
#' For `'Lognormal'` they are ordered `c(log_sd_low, log_sd_high, log_mean_low, log_mean_high)`.
#' For `'Skewed'` they are ordered `c(omega_low, omega_high, xi_low, xi_high, alpha_low, alpha_high)`.
#' For `'Gamma'` they are ordered `c(rate_low, rate_high, shape_low, shape_high)`. For
#' `'Beta'` they are ordered `c(alpha_low, alpha_high, beta_low, beta_high)`.
#' @param RNG.state Optional setting to specify a list of three lists setting the
#' `.RNG.name` and `.RNG.state` for each MCMC chain. The default is a fixed set of
#' RNG states so the results are always reproducible. If `random = TRUE` the RNG
#' state is set randomly instead.
#' @param random Default is `FALSE` where random seeds are defined via `.RNG.name`
#' and `.RNG.seed` and returned as `state` so JAGS runs will be exactly reproducible.
#' Changing to `TRUE` will generate new random states to use for `.RNG.name` and
#' `.RNG.state` instead, also returned as `state` so the results can be
#' recreated exactly if desired.
#' @param custom_model String for the file path location and name
#' (i.e. "working_directory/Custom_JAGS.R") if using a custom JAGS model script.
#' @param custom_params List of parameters to monitor in addition to
#' c('emission_hat', 'pdf_obs', 'pdf_hat') if using a custom model.
#' @param custom_init List of three lists with initial values for MCMC chains
#' corresponding to parameters in `custom_params`.
#' @returns Object `model_code`, which is a string for the written R script that
#' JAGS can call, `par_list` which is the list of parameters traced while running
#' the JAGS model, `dat_inits` which is a list of initial parameter values and
#' random seeds for 3 chains, and the distribution used in likelihood model.
#' @export
setup_likelihood = function(distribution, data, emissions,
                            manual_prior = FALSE,
                            prior_list = NULL, random = FALSE,
                            RNG.state = NULL,
                            custom_model = NULL, custom_params = NULL,
                            custom_init = NULL){
  JAGS_path = system.file("JAGS", package = "UPLforOAR", mustWork = TRUE)
  data_temp = tibble::tibble(emissions = data[[emissions]])
  if (any(is.na(data_temp$emissions))){
    warning('Emissions with NA values have been removed')
    data_temp = stats::na.omit(data_temp)
  }
  if (!is.numeric(data_temp$emissions)){
    stop("Emissions must be numeric")
  }
  mu = mean(data_temp$emissions)
  sigma = stats::sd(data_temp$emissions)
  if (sigma == 0){
    stop("Cannot calculate UPL with zero variance data")
  }
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
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat', custom_params)
      data_inits_temp = list(custom_init[[1]],
                             custom_init[[2]],
                             custom_init[[3]])
    }
    if (distribution == "Normal"){
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                               '/Emission_normal_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'emission_mean', 'emission_sd')
      data_inits_temp = list(
        list('emission_mean' = mu, 'emission_sd' = sigma),
        list('emission_mean' = 1.5 * mu, 'emission_sd' = 0.5 * sigma),
        list('emission_mean' = 0.5 * mu, 'emission_sd' = 1.5 * sigma))
    } else if (distribution == "Lognormal"){
      ln_emiss = log(data_temp$emissions)
      ln_mu = mean(ln_emiss, na.rm = TRUE)
      ln_sig = stats::sd(ln_emiss, na.rm=  TRUE)
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                               '/Emission_lnorm_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'u_ln', 'sd_ln')
      data_inits_temp = list(
        list('u_ln' = ln_mu, 'sd_ln' = ln_sig),
        list('u_ln' = 1.5 * ln_mu, 'sd_ln' = 0.5 * ln_sig),
        list('u_ln' = 0.5 * ln_mu, 'sd_ln' = 1.5 * ln_sig))
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
                                               '/Emission_skewed_JAGS.R'))
      par_list = c('omega', 'xi', 'alpha')
      data_inits_temp = list(
        list('xi' = xi, 'omega' = omega, 'alpha' = alpha),
        list('xi' = 1.5 * xi, 'omega' = 0.5 * omega, 'alpha' = 0.5 * alpha),
        list('xi' = 0.5 * xi, 'omega' = 1.5 * omega, 'alpha' = 1.5 * alpha))
    } else if (distribution == 'Gamma'){
      shape = mu^2 / sigma^2
      rate = mu / sigma^2
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                               '/Emission_gamma_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'rate_em', 'shape_em')
      data_inits_temp = list(
        list('rate_em' = rate, 'shape_em' = shape),
        list('rate_em' = 1.5 * rate, 'shape_em' = 0.5 * shape),
        list('rate_em' = 0.5 * rate, 'shape_em' = 1.5 * shape))
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
                                               '/Emission_beta_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'alpha_em', 'beta_em')
      data_inits_temp = list(
        list('beta_em' = beta, 'alpha_em' = alpha),
        list('beta_em' = 1.5 * beta, 'alpha_em' = 0.5 * alpha),
        list('beta_em' = 0.5 * beta, 'alpha_em' = 1.5 * alpha))
    }
    if (random){
      new_state = rjags::parallel.seeds('base::BaseRNG', 3)
    } else if (!random){
      new_state = RNG.state
    }
    data_inits = list(c(data_inits_temp[[1]], new_state[[1]]),
                      c(data_inits_temp[[2]], new_state[[2]]),
                      c(data_inits_temp[[3]], new_state[[3]]))
    output = list(model_code = JAGS_model, par_list = par_list, data = data_temp,
                  distribution = distribution, dat_inits = data_inits,
                  manual_prior = manual_prior, state = new_state)
  } else if (manual_prior){
    if (is.null(prior_list)){
      stop('Please provide a list of upper and lower bounds for manual priors')
    }
    if (distribution == "Normal"){
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                               '/Manual_emission_normal_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'emission_mean', 'emission_sd')
      if (any(prior_list[1:2] <= 0)){
        stop('Prior limits must be positive')
      }
      data_inits_temp = list(
        list('emission_mean' = mean(c(prior_list[3], prior_list[4])),
             'emission_sd' = mean(c(prior_list[1], prior_list[2]))),
        list('emission_mean' = 0.9 * prior_list[4],
             'emission_sd' = 0.9 * prior_list[2]),
        list('emission_mean' = 1.1 * prior_list[3],
             'emission_sd' = 1.1 * prior_list[1]))
    } else if (distribution == "Lognormal"){
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                               '/Manual_emission_lnorm_JAGS.R'))
      par_list=c('emission_hat', 'pdf_obs', 'pdf_hat',
                 'u_ln', 'sd_ln')
      if (any(prior_list[1:2] <= 0)){
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
        list('u_ln' = mean(c(prior_list[3], prior_list[4])),
             'sd_ln' = mean(c(prior_list[1], prior_list[2]))),
        list('u_ln' = initial1, 'sd_ln' = 0.9 * prior_list[2]),
        list('u_ln' = initial2, 'sd_ln' = 1.1 * prior_list[1]))
    } else if (distribution == "Skewed"){
      if (any(prior_list[1:2] <= 0)){
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
                                               '/Manual_emission_skewed_JAGS.R'))
      par_list = c('omega', 'xi', 'alpha')
      data_inits_temp = list(
        list('xi' = mean(c(prior_list[3], prior_list[4])),
             'omega' = mean(c(prior_list[1], prior_list[2])),
             'alpha' = mean(c(prior_list[5], prior_list[6]))),
        list('xi' = initial1, 'omega' = 1.1 * prior_list[1],
             'alpha' = initial4),
        list('xi' = initial2, 'omega' = 0.9 * prior_list[2], 'alpha' = initial3))
    } else if (distribution == 'Gamma'){
      if (any(prior_list <= 0)){
        stop('Prior limits must be positive')
      }
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                               '/Manual_emission_gamma_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                 'rate_em', 'shape_em')
      data_inits_temp = list(
        list('rate_em' = mean(c(prior_list[1], prior_list[2])),
             'shape_em' = mean(c(prior_list[3], prior_list[4]))),
        list('rate_em' = 1.1 * prior_list[1],
             'shape_em' = 1.1 * prior_list[3]),
        list('rate_em' = 0.9 * prior_list[2],
             'shape_em' = 0.9 * prior_list[4]))
    } else if (distribution == 'Beta'){
      if (min(data_temp$emissions) < 0){
        stop('Cannot use beta distribution with emissions less than 0')
      }
      if (max(data_temp$emissions) > 1){
        stop('Cannot use beta distribution with emissions greater than 1')
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
        if (initial1 < (-1)){
          initial1 = -0.99999
        }
      }
      if (prior_list[4] < 0){
        initial3 = 1.1 * prior_list[4]
      } else if (prior_list[4] > 0){
        initial3 = 0.9 * prior_list[4]
        if (initial1 < (-1)){
          initial1 = -0.99999
        }
      }
      if (prior_list[3] < 0){
        initial4 = 0.9 * prior_list[3]
      } else if (prior_list[3] > 0){
        initial4 = 1.1 * prior_list[3]
        if (initial1 < (-1)){
          initial1 = -0.99999
        }
      }
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                               '/Manual_emission_beta_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                 'alpha_em', 'beta_em')
      data_inits_temp = list(
        list('beta_em' = mean(c(prior_list[3], prior_list[4])),
             'alpha_em' = mean(c(prior_list[1], prior_list[2]))),
        list('beta_em' = initial3,
             'alpha_em' = initial1),
        list('beta_em' = initial4,
             'alpha_em' = initial2))
    }
    if (random){
      new_state = rjags::parallel.seeds('base::BaseRNG', 3)
    } else if (!random){
      new_state = RNG.state
    }
    data_inits = list(c(data_inits_temp[[1]], new_state[[1]]),
                      c(data_inits_temp[[2]], new_state[[2]]),
                      c(data_inits_temp[[3]], new_state[[3]]))
    output = list(model_code = JAGS_model, par_list = par_list, data = data_temp,
                manual_prior = manual_prior, distribution = distribution,
                dat_inits = data_inits, prior_list = prior_list,
                state = new_state)
  }
  return(output)
}
