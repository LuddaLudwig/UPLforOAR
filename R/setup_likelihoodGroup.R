#' Sets up path to JAGS script with a hierarchical group structure,
#' initial values, and variable list to monitor
#' @description
#' This function defines the jagsmodel script to call based on the selected
#' distribution. It also defines the initial values and variables to monitor.
#' Rather than independent runs defining the population
#' distribution, hierarchical dependency within groups is allowed with the
#' group-level distribution parameters drawn from the overall population distribution.
#' @param distribution Any of `'Normal'`, `'Gamma'`, `'Skewed'`, `'Lognormal'`, or `'Beta'`.
#' @param data Emissions data from either the best source or top performers,
#' must have a column named `emissions`.
#' @param manual_prior Default is `FALSE`, priors are uninformative and calculated
#' from range of emissions data. if `TRUE` priors should be specified manually in
#' `prior_list`.
#' @param prior_list Optional list of [stats::dunif()] upper and lower bounds for prior
#' distributions. For `'Normal'` and `'Lognormal'` they are ordered
#' `c(pop_sd_mu_low, pop_sd_mu_high, pop_mu_mu_low, pop_mu_mu_high, pop_sd_sd_low, pop_sd_sd_high, pop_mu_sd_low, pop_mu_sd_high)`.
#' For `'Skewed'` they are ordered `c(omega_low, omega_high, xi_low, xi_high, alpha_low, alpha_high)`.
#' For `'Gamma'` they are ordered `c(rate_low, rate_high, shape_low, shape_high)`. For
#' `'Beta'` they are ordered `c(alpha_low, alpha_high, beta_low, beta_high)`.
#' @param random Default is `FALSE` where random seeds are defined via `.RNG.name`
#' and `.RNG.seed` so JAGS runs will be exactly reproducible. Changing to `TRUE`
#' will use random values for `.RNG.name` and `.RNG.seed` instead.
#' @returns Object `model_code`, which is a string for the written R script that
#' JAGS can call, `par_list` which is the list of parameters traced while running
#' the JAGS model, `dat_inits` which is a list of initial parameter values and
#' random seeds for 3 chains, and the distribution used in likelihood model.
#' @export
setup_likelihoodGroup = function(distribution, data, manual_prior = FALSE,
                            prior_list = NULL, random = FALSE){
  JAGS_path = system.file("JAGS", package = "UPLforOAR", mustWork = TRUE)
  if (("emissions" %in% names(data)) == FALSE){
    stop("data must have numeric column named 'emissions' ")
  }
  if (!is.numeric(data$emissions)){
    stop("Emissions must be numeric")
  }
  mu = mean(data$emissions)
  sigma = stats::sd(data$emissions)
  maxX = max( data$emissions)
  if (sigma == 0){
    stop("Cannot calculate UPL with zero variance data")
  }
  if(!manual_prior){
    if (distribution == "Normal"){
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/EmissionGroup_normal_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'pop_mu_mu', 'pop_sd_mu', 'pop_mu_sd', 'pop_sd_sd',
                   'group_sd','group_mu','group')
      data_inits = list(
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 5,
             'pop_mu_mu' = mu, 'pop_sd_mu' = sigma,
             'pop_mu_sd' = sigma, 'pop_sd_sd' = sigma),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 12,
             'pop_mu_mu' = 1.5 * mu, 'pop_sd_mu' = 0.5 * sigma,
             'pop_mu_sd' = 0.1 * sigma, 'pop_sd_sd' = 10 * sigma),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 151,
             'pop_mu_mu' = 0.5 * mu, 'pop_sd_mu' = 1.5 * sigma,
             'pop_mu_sd' = 10 * sigma, 'pop_sd_sd' = 0.1 * sigma))
    } else if (distribution == "Lognormal"){
      ln_emiss = log(data$emissions)
      ln_mu = mean(ln_emiss, na.rm = TRUE)
      ln_sig = stats::sd(ln_emiss, na.rm=  TRUE)
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/EmissionGroup_lnorm_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'pop_mu_mu', 'pop_sd_mu', 'pop_mu_sd', 'pop_sd_sd',
                   'group_sd','group_mu','group')
      data_inits = list(
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 5,
             'pop_mu_mu' = ln_mu, 'pop_sd_mu' = ln_sig,
             'pop_mu_sd' = ln_sig, 'pop_sd_sd' = ln_sig),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 12,
             'pop_mu_mu' = 1.5 * ln_mu, 'pop_sd_mu' = 0.5 * ln_sig,
             'pop_mu_sd' = 0.1 * ln_sig, 'pop_sd_sd' = 10 * ln_sig),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 151,
             'pop_mu_mu' = 0.5 * ln_mu, 'pop_sd_mu' = 1.5 * ln_sig,
             'pop_mu_sd' = 10 * ln_sig, 'pop_sd_sd' = 0.1 * ln_sig))
    } else if (distribution == "Skewed"){
      skew1 = min(0.99, abs((1 / length(data$emissions)) *
                              sum(((data$emissions - mu) / sigma)^3)))
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
      data_inits = list(
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 5,
             'pop_xi_mu' = xi, 'pop_omega_mu' = omega, 'pop_alpha_mu' = alpha,
             'pop_xi_sd' = xi / 10,  'pop_omega_sd' = 0.1 * maxX,
             'pop_alpha_sd' = 5),
        list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 12,
             'pop_xi_mu' = 1.5 * xi, 'pop_omega_mu' = 0.5 * omega,
             'pop_alpha_mu' = 0.5 * alpha,
             'pop_xi_sd' = xi,  'pop_omega_sd' = 0.5 * maxX, 'pop_alpha_sd' = 10),
        list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 151,
             'pop_xi_mu' = 0.5 * xi, 'pop_omega_mu' = 1.5 * omega,
             'pop_alpha_mu' = 1.5 * alpha,
             'pop_xi_sd' = xi * 10,  'pop_omega_sd' = 0.01 * maxX,
             'pop_alpha_sd' = 1))
    } else if (distribution == 'Gamma'){
      shape = mu^2 / sigma^2
      rate = mu / sigma^2
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/EmissionGroup_gamma_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'pop_rate_mu', 'pop_shape_mu', 'pop_rate_sd', 'pop_shape_sd',
                   'group_rate', 'group_shape', 'group')
      data_inits = list(
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 5,
             'pop_rate_mu' = rate, 'pop_shape_mu' = shape,
             'pop_rate_sd' = 0.1 * rate, 'pop_shape_sd' = 0.1 * shape),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 12,
             'pop_rate_mu' = 1.5 * rate, 'pop_shape_mu' = 0.5 * shape,
             'pop_rate_sd' = 0.5 * rate, 'pop_shape_sd' = 0.5 * shape),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 151,
             'pop_rate_mu' = 0.5 * rate, 'pop_shape_mu' = 1.5 * shape,
             'pop_rate_sd' = 0.3 * rate, 'pop_shape_sd' = 0.3 * shape))
    } else if (distribution == 'Beta'){
      if (min(data$emissions) < 0){
        stop('Cannot use beta distribution with emissions less than 0')
      }
      if (max(data$emissions) > 1){
        stop('Cannot use beta distribution with emissions greater than 1')
      }
      alpha = mu^2 / sigma^2 - mu^3 / sigma^2 - mu
      beta = mu - 1 + mu / sigma^2 + mu^3 / sigma^2 - 2 * mu^2 / sigma^2
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/EmissionGroup_beta_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'pop_alpha_mu', 'pop_beta_mu', 'pop_alpha_sd', 'pop_beta_sd',
                   'group_alpha', 'group_beta', 'group')
      data_inits = list(
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 5,
             'pop_beta_mu' = beta, 'pop_alpha_mu' = alpha,
             'pop_beta_sd' = 0.1 * beta, 'pop_alpha_sd' = 0.5 * alpha),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 12,
             'pop_beta_mu' = 1.5 * beta, 'pop_alpha_mu' = 0.5 * alpha,
             'pop_beta_sd' = 0.3 * beta, 'pop_alpha_sd' = 0.3 * alpha),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 151,
             'pop_beta_mu' = 0.5 * beta, 'pop_alpha_mu' = 1.5 * alpha,
             'pop_beta_sd' = 0.5 * beta, 'pop_alpha_sd' = 0.1 * alpha))
    }
    if (random){
      data_inits = list(
        data_inits[[1]][names(data_inits[[1]]) %in%
                          c(".RNG.name", ".RNG.seed") == FALSE],
        data_inits[[2]][names(data_inits[[2]]) %in%
                          c(".RNG.name", ".RNG.seed") == FALSE],
        data_inits[[3]][names(data_inits[[3]]) %in%
                          c(".RNG.name", ".RNG.seed") == FALSE])
    }
    output = list(model_code = JAGS_model, par_list = par_list, data = data,
                  distribution = distribution, dat_inits = data_inits,
                  manual_prior = manual_prior)
  } else if (manual_prior){
    if (is.null(prior_list)){
      stop('Please provide a list of upper and lower bounds for manual priors')
    }
    if (distribution == "Normal"){
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/Manual_emissionGroup_normal_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'pop_mu_mu', 'pop_sd_mu', 'pop_mu_sd', 'pop_sd_sd',
                   'group_sd','group_mu','group')
      data_inits = list(
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 5,
             'pop_mu_mu' = mean(c(prior_list[3], prior_list[4])),
             'pop_sd_mu' = mean(c(prior_list[1], prior_list[2])),
             'pop_mu_sd' = mean(c(prior_list[7], prior_list[8])),
             'pop_sd_sd' = mean(c(prior_list[5], prior_list[6]))),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 12,
             'pop_mu_mu' = 0.9 * prior_list[4],
             'pop_sd_mu' = 0.9 * prior_list[2],
             'pop_mu_sd' = 0.9 * prior_list[8],
             'pop_sd_sd' = 0.9 * prior_list[6]),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 151,
             'pop_mu_mu' = 1.1 * prior_list[3],
             'pop_sd_mu' = max(1.1 * prior_list[1], 0.0000000001),
             'pop_mu_sd' = max(1.1 * prior_list[7], 0.0000000001),
             'pop_sd_sd' = max(1.1 * prior_list[5], 0.0000000001)))
    } else if (distribution == "Lognormal"){
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/Manual_emissionGroup_lnorm_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'pop_mu_mu', 'pop_sd_mu', 'pop_mu_sd', 'pop_sd_sd',
                   'group_sd','group_mu','group')
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
      data_inits = list(
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 5,
             'u_ln' = mean(c(prior_list[3], prior_list[4])),
             'sd_ln' = mean(c(prior_list[1], prior_list[2]))),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 12,
             'u_ln' = initial1, 'sd_ln' = 0.9 * prior_list[2]),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 151,
             'u_ln' = initial2, 'sd_ln' = max(1.1 * prior_list[1], 0.000001)))
    } else if (distribution == "Skewed"){
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
      par_list = c('omega', 'xi', 'alpha')
      data_inits = list(
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 5,
             'xi' = mean(c(prior_list[3], prior_list[4])),
             'omega' = mean(c(prior_list[1], prior_list[2])),
             'alpha' = mean(c(prior_list[5], prior_list[6]))),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 12,
             'xi' = initial1, 'omega' = max(1.1 * prior_list[1], 0.00001),
             'alpha' = initial4),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 151,
             'xi' = initial2, 'omega' = 0.9 * prior_list[2], 'alpha' = initial3))
    } else if (distribution == 'Gamma'){
      if (prior_list[3] <= 0){
        initial2 = 0.000001
      } else if (prior_list[3] > 0){
        initial2 = 1.1 * prior_list[3]
      }
      if (prior_list[1] <= 0){
        initial1 = 0.000001
      } else if (prior_list[1] > 0){
        initial1 = 1.1 * prior_list[1]
      }
      JAGS_model = runjags::read.jagsfile(paste0(JAGS_path,
                                                 '/Manual_emissionGroup_gamma_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'rate_em', 'shape_em')
      data_inits = list(
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 5,
             'rate_em' = mean(c(prior_list[1], prior_list[2])),
             'shape_em' = mean(c(prior_list[3], prior_list[4]))),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 12,
             'rate_em' = initial1,
             'shape_em' = initial2),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 151,
             'rate_em' = 0.9 * prior_list[2],
             'shape_em' = 0.9 * prior_list[4]))
    } else if (distribution == 'Beta'){
      if (min(data$emissions) < 0){
        stop('Cannot use beta distribution with emissions less than 0')
      }
      if (max(data$emissions) > 1){
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
                                                 '/Manual_emissionGroup_beta_JAGS.R'))
      par_list = c('emission_hat', 'pdf_obs', 'pdf_hat',
                   'alpha_em', 'beta_em')
      data_inits = list(
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 5,
             'beta_em' = mean(c(prior_list[3], prior_list[4])),
             'alpha_em' = mean(c(prior_list[1], prior_list[2]))),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 12,
             'beta_em' = initial3,
             'alpha_em' = initial1),
        list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 151,
             'beta_em' = initial4,
             'alpha_em' = initial2))
    }
    if (random){
      data_inits = list(
        data_inits[[1]][names(data_inits[[1]]) %in%
                          c(".RNG.name", ".RNG.seed") == FALSE],
        data_inits[[2]][names(data_inits[[2]]) %in%
                          c(".RNG.name", ".RNG.seed") == FALSE],
        data_inits[[3]][names(data_inits[[3]]) %in%
                          c(".RNG.name", ".RNG.seed") == FALSE])
    }
    output = list(model_code = JAGS_model, par_list = par_list, data = data,
                  manual_prior = manual_prior, distribution = distribution,
                  dat_inits = data_inits, prior_list = prior_list)
  }
  return(output)
}
