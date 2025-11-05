#' Writes likelihood scripts for JAGS model calls with a hierarchical group structure
#' @description
#' This function writes an R script for JAGS to call based on the selected
#' distribution and prior. The priors are uninformative and
#' set based on emissions data, unless specified manually via
#' [setup_likelihoodGroup()]. The likelihood distributions are truncated to
#' `(minY, maxY)`, where `minY` and `maxY` can be specified or used with the
#' default `minY = 0` and `maxY = 3 * max(data$emissions)` in
#' [run_likelihood()]. Rather than independent runs defining the population
#' distribution, hierarchical dependency within groups is allowed with the
#' group-level distribution parameters drawn from the overall population distribution.
#' @param distribution Any of `'Normal'`, `'Gamma'`, `'Skewed'`, `'Lognormal'`,
#' or `'Beta'`.
#' @param write_wd Default is `NULL`, in which case the JAGS scripts are written
#' into inst/JAGS folder in package directory. This is the location
#' [run_likelihoodGroup()] will look for the JAGS scripts assigned via
#' [setup_likelihoodGroup()].
#' @param manual_prior Default is `FALSE`, if priors should be specified
#' manually or be uninformative calculated from range of emissions data.
#' @returns object `model_code`, which is a string for the written R script that
#' JAGS can call and the distribution used in likelihood model.
#' @export
write_likelihoodGroup = function(distribution,
                            manual_prior = FALSE, write_wd = NULL){
  current_wd = getwd()
  if (is.null(write_wd)){
    write_wd = 'inst/JAGS/'
  }
  setwd(write_wd)
  if (!manual_prior){
    if (distribution == "Normal"){
      JAGS_model = "EmissionGroup_normal_JAGS.R"
      cat("# Normal with groups
      model {
      # priors
          pop_mu_mu ~ dunif(0,maxY)
          pop_mu_sd ~ dunif(0, 1000 * sdY)T(0, ) # must be positive
          pop_sd_mu ~ dunif(0, maxY)T(0, ) # must be positive
          pop_sd_sd ~ dunif(0, 1000 * sdY)T(0, ) # must be positive

          for (j in 1:n_groups){
                    group_sd[j] ~ dnorm(pop_sd_mu, 1 / (pop_sd_sd^2))T(0, ) # must be positive
                    group_mu[j] ~ dnorm(pop_mu_mu, 1 / (pop_mu_sd^2))
          }
      #likelihood
          for (i in 1:length(emission_xi)) {
            emission_xi[i] ~ dnorm(group_mu[group_j[i]],
              1 / (group_sd[group_j[i]]^2))T(minY, maxY)
            pdf_obs[i] = dnorm(emission_xi[i], group_mu[group_j[i]],
              1 / (group_sd[group_j[i]]^2))
          }

      # derived quantities
          for (g in 1:n_groups){
            group[g] ~ dnorm(group_mu[g], 1 / (group_sd[g]^2))T(minY, maxY)
          }
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k] ~ dnorm(pop_mu_mu, 1 / (pop_sd_mu^2))T(minY, maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h] = dnorm(x_hat[h], pop_mu_mu, 1 / (pop_sd_mu^2))
          }
        }", file = JAGS_model)
    } else if (distribution == "Lognormal"){
      JAGS_model = "EmissionGroup_lnorm_JAGS.R"
      cat("# Lognormal with groups
      model {
      # priors

          # pop_mu_mu is in log-space, so it can be negative
          pop_mu_mu ~ dnorm(meanOfLogY, 0.001 / sdOfLogY^2)
          pop_mu_sd ~ dunif(0, 1000 * sdOfLogY)T(0, ) # must be positive
          pop_sd_mu ~ dunif(0, 1000 * sdOfLogY)T(0, ) # must be positive
          pop_sd_sd ~ dunif(0, 1000 * sdOfLogY)T(0, ) # must be positive

          for (j in 1:n_groups){
                    group_sd[j] ~ dnorm(pop_sd_mu, 1 / (pop_sd_sd^2))T(0, )# must be positive
                    group_mu[j] ~ dnorm(pop_mu_mu, 1 / (pop_mu_sd^2))
          }
      #likelihood
          for (i in 1:length(emission_xi)){
            emission_xi[i] ~ dlnorm(group_mu[group_j[i]],
              1 / (group_sd[group_j[i]]^2))T(minY, maxY)
            pdf_obs[i] = dlnorm(emission_xi[i], group_mu[group_j[i]],
              1 / (group_sd[group_j[i]]^2))
          }
      # derived quantities
          for (g in 1:n_groups){
            group[g] ~ dlnorm(group_mu[g], 1 / (group_sd[g]^2))T(minY, maxY)
          }
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k] ~ dlnorm(pop_mu_mu, 1 / (pop_sd_mu^2))T(minY, maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h] = dlnorm(x_hat[h], pop_mu_mu, 1 / (pop_sd_mu^2))
          }
        }", file = JAGS_model)
    } else if (distribution == "Skewed"){
      JAGS_model = "EmissionGroup_skewed_JAGS.R"
      cat("# Skewed with groups
      data {
              for(i in 1:length(emission_xi)){
              zeros[i] = 0
              }
        }
        model {
      # priors

          pop_omega_mu ~ dunif(0, 100 * maxY) #must be positive
          pop_xi_mu ~ dnorm(0, 1 / (100 * maxY))
          pop_alpha_mu ~ dunif(-100, 100)

          pop_omega_sd ~ dunif(0, 100 * maxY)T(0, ) # must be positive
          pop_xi_sd ~ dunif(0, (100 * maxY))T(0, ) # must be positive
          pop_alpha_sd ~ dunif(0, 100)T(0, ) # must be positive

          for (j in 1:n_groups){
            alpha[j] ~ dnorm(pop_alpha_mu, 1 / (pop_alpha_sd^2))
            omega[j] ~ dnorm(pop_omega_mu, 1 / (pop_omega_sd^2))T(0, )
            xi[j] ~ dnorm(pop_xi_mu, 1 / (pop_xi_sd^2))
          }

      #likelihood
          for (i in 1:length(emission_xi)) {
                L[i] = ((2 / omega[group_j[i]])
                * dnorm((emission_xi[i] - xi[group_j[i]])
                / omega[group_j[i]], 0, 1)
                * pnorm(alpha[group_j[i]] * (emission_xi[i] - xi[group_j[i]])
                / omega[group_j[i]], 0, 1))
                zeros[i] ~ dpois(-log(L[i]) + 10000)
          }
        }", file = JAGS_model)
    } else if (distribution == 'Gamma'){
      JAGS_model = "EmissionGroup_gamma_JAGS.R"
      cat("# Gamma with groups
      model {
      # priors

          pop_rate_mu ~ dunif(0, maxY / (sdY^2))T(0, ) #must be positive
          pop_shape_mu ~ dunif(0, (maxY^2) / (sdY^2))T(0, ) #must be positive
          pop_rate_sd ~ dunif(0, maxY / (sdY^2))T(0, ) #must be positive
          pop_shape_sd ~ dunif(0, (maxY^2) / (sdY^2))T(0, ) #must be positive

          for (j in 1:n_groups){
                group_rate[j] ~ dnorm(pop_rate_mu, 1 / (pop_rate_sd^2))T(0, )
                group_shape[j] ~ dnorm(pop_shape_mu, 1 / (pop_shape_sd^2))T(0, )
          }
      #likelihood
          for (i in 1:length(emission_xi)) {
            emission_xi[i] ~ dgamma(group_shape[group_j[i]],
                                    group_rate[group_j[i]])T(minY, maxY)
            pdf_obs[i] = dgamma(emission_xi[i], group_shape[group_j[i]],
                                group_rate[group_j[i]])
          }

      # derived quantities
          for (g in 1:n_groups){
            source[g] ~ dgamma(group_shape[g], group_rate[g])T(minY, maxY)
          }
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k] ~ dgamma(pop_shape_mu, pop_rate_mu)T(minY, maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h] = dgamma(x_hat[h], pop_shape_mu, pop_rate_mu)
          }
        }", file = JAGS_model)
    } else if (distribution == 'Beta'){
      JAGS_model = "EmissionGroup_beta_JAGS.R"
      cat("# Beta with groups
      model {
      # priors
          # note that both can be pos or neg, but the minimum is always -1
          pop_alpha_mu ~ dunif(-1, (0.75^2 / sdY^2 - 0.75^3 / sdY^2 - 0.75) * 1.25)
          pop_beta_mu ~ dunif(-1, (0.25 - 1 + 0.25 / sdY^2 + 0.25^3 / sdY^2 - 2 *
            0.25^2 / sdY^2) * 1.25)
          pop_alpha_sd ~ dunif(0, (0.75^2 / sdY^2 - 0.75^3 / sdY^2 - 0.75)
            * 1.25)T(0, )
          pop_beta_sd ~ dunif(0, (0.25 - 1 + 0.25 / sdY^2 + 0.25^3 / sdY^2 - 2 *
            0.25^2 / sdY^2) * 1.25)T(0, )

          for (j in 1:n_groups){
                    group_alpha[j] ~ dnorm(pop_alpha_mu, 1 / (pop_alpha_sd^2))T(-1, )
                    group_beta[j] ~ dnorm(pop_beta_mu, 1 / (pop_beta_sd^2))T(-1, )
          }
      #likelihood
          for (i in 1:length(emission_xi)) {
            emission_xi[i] ~ dbeta(group_alpha[group_j[i]],
                                   group_beta[group_j[i]])T(minY, maxY)
            pdf_obs[i] = dbeta(emission_xi[i], group_alpha[group_j[i]],
                               group_beta[group_j[i]])
          }

      # derived quantities
          for (g in 1:n_groups){
            source[g] ~ dbeta(group_alpha[g], group_beta[g])T(minY, maxY)
          }
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k] ~ dbeta(pop_alpha_mu, pop_beta_mu)T(minY, maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h] = dbeta(x_hat[h], pop_alpha_mu, pop_beta_mu)
          }
        }", file = JAGS_model)
    }
  } else if (manual_prior){
    if (distribution == "Normal"){
      JAGS_model = "Manual_emissionGroup_normal_JAGS.R"
      cat("# Normal with groups manual priors
      model {
      # priors
          pop_mu_mu ~ dunif(low1, up1)
          pop_mu_sd ~ dunif(low2, up2)T(0, )
          pop_sd_mu ~ dunif(low3, up3)T(0, )
          pop_sd_sd ~ dunif(low4, up4)T(0, )

          for (j in 1:n_groups){
                    group_sd[j] ~ dnorm(pop_sd_mu, 1 / (pop_sd_sd^2))T(0, )
                    group_mu[j] ~ dnorm(pop_mu_mu, 1 / (pop_mu_sd^2))
          }
      #likelihood
          for (i in 1:length(emission_xi)) {
            emission_xi[i] ~ dnorm(group_mu[group_j[i]],
              1 / (group_sd[group_j[i]]^2))T(minY, maxY)
            pdf_obs[i] = dnorm(emission_xi[i], group_mu[group_j[i]],
              1 / (group_sd[group_j[i]]^2))
          }

      # derived quantities
          for (g in 1:n_groups){
            group[g] ~ dnorm(group_mu[g], 1 / (group_sd[g]^2))T(minY, maxY)
          }
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k] ~ dnorm(pop_mu_mu, 1 / (pop_sd_mu^2))T(minY, maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h] = dnorm(x_hat[h], pop_mu_mu, 1 / (pop_sd_mu^2))
          }
        }", file = JAGS_model)
    } else if (distribution == "Lognormal"){
      JAGS_model = "Manual_emissionGroup_lnorm_JAGS.R"
      cat("# Lognormal with groups manual priors
      model {
      # priors

          # pop_mu_mu is in log-space, so it can be negative
          pop_mu_mu ~ dunif(low1, up1)
          pop_mu_sd ~ dunif(low2, up2)T(0, )
          pop_sd_mu ~ dunif(low3, up3)T(0, )
          pop_sd_sd ~ dunif(low4, up4)T(0, )

          for (j in 1:n_groups){
                    group_sd[j] ~ dnorm(pop_sd_mu, 1 / (pop_sd_sd^2))T(0, )
                    group_mu[j] ~ dnorm(pop_mu_mu, 1 / (pop_mu_sd^2))
          }
      #likelihood
          for (i in 1:length(emission_xi)){
            emission_xi[i] ~ dlnorm(group_mu[group_j[i]],
              1 / (group_sd[group_j[i]]^2))T(minY, maxY)
            pdf_obs[i] = dlnorm(emission_xi[i], group_mu[group_j[i]],
              1 / (group_sd[group_j[i]]^2))
          }
      # derived quantities
          for (g in 1:n_groups){
            group[g] ~ dlnorm(group_mu[g], 1 / (group_sd[g]^2))T(minY, maxY)
          }
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k] ~ dlnorm(pop_mu_mu, 1 / (pop_sd_mu^2))T(minY, maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h] = dlnorm(x_hat[h], pop_mu_mu, 1 / (pop_sd_mu^2))
          }
        }", file = JAGS_model)
    } else if (distribution == "Skewed"){
      JAGS_model = "Manual_emissionGroup_skewed_JAGS.R"
      cat("# Skewed with groups manual priors
      data {
              for(i in 1:length(emission_xi)){
              zeros[i] = 0
              }
        }
        model {
      # priors

          pop_omega_mu ~ dunif(low1, up1)T(0, ) #must be positive
          pop_xi_mu ~ dunif(low2, up2)
          pop_alpha_mu ~ dunif(low3, up3)

          pop_omega_sd ~ dunif(0, maxY)T(0, ) # must be positive
          pop_xi_sd ~ dunif(0, (100 * maxY))T(0, )
          pop_alpha_sd ~ dunif(0, 100)T(0, )

          for (j in 1:n_groups){
            alpha[j] ~ dnorm(pop_alpha_mu, 1 / (pop_alpha_sd^2))
            omega[j] ~ dnorm(pop_omega_mu, 1 / (pop_omega_sd^2))T(0, )
            xi[j] ~ dnorm(pop_xi_mu, 1 / (pop_xi_sd^2))
          }

      #likelihood
          for (i in 1:length(emission_xi)) {
                L[i] = ((2 / omega[group_j[i]])
                * dnorm((emission_xi[i] - xi[group_j[i]])
                / omega[group_j[i]], 0, 1)
                * pnorm(alpha[group_j[i]] * (emission_xi[i] - xi[group_j[i]])
                / omega[group_j[i]], 0, 1))
                zeros[i] ~ dpois(-log(L[i]) + 10000)
          }
        }", file = JAGS_model)
    } else if (distribution == 'Gamma'){
      JAGS_model = "Manual_emissionGroup_gamma_JAGS.R"
      cat("# Gamma with groups manual priors
      model {
      # priors

          pop_rate_mu ~ dunif(low1, up1)T(0, ) #must be positive
          pop_shape_mu ~ dunif(low2, up2)T(0, ) #must be positive
          pop_rate_sd ~ dunif(low3, up3)T(0, ) #must be positive
          pop_shape_sd ~ dunif(low4, up4)T(0, ) #must be positive

          for (j in 1:n_groups){
                group_rate[j] ~ dnorm(pop_rate_mu, 1 / (pop_rate_sd^2))T(0, )
                group_shape[j] ~ dnorm(pop_shape_mu, 1 / (pop_shape_sd^2))T(0, )
          }
      #likelihood
          for (i in 1:length(emission_xi)) {
            emission_xi[i] ~ dgamma(group_shape[group_j[i]],
                                    group_rate[group_j[i]])T(minY, maxY)
            pdf_obs[i] = dgamma(emission_xi[i], group_shape[group_j[i]],
                                group_rate[group_j[i]])
          }

      # derived quantities
          for (g in 1:n_groups){
            source[g] ~ dgamma(group_shape[g], group_rate[g])T(minY, maxY)
          }
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k] ~ dgamma(pop_shape_mu, pop_rate_mu)T(minY, maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h] = dgamma(x_hat[h], pop_shape_mu, pop_rate_mu)
          }
        }", file = JAGS_model)
    } else if (distribution == 'Beta'){
      JAGS_model = "Manual_emissionGroup_beta_JAGS.R"
      cat("# Beta with groups manual priors
      model {
      # priors
          # note that both can be pos or neg, but the minimum is always -1
          pop_alpha_mu ~ dunif(low1, up1)
          pop_beta_mu ~ dunif(low2, up2)
          pop_alpha_sd ~ dunif(low3, up3)T(0, )
          pop_beta_sd ~ dunif(low4, up4)T(0, )

          for (j in 1:n_groups){
                    group_alpha[j] ~ dnorm(pop_alpha_mu, 1 / (pop_alpha_sd^2))T(-1, )
                    group_beta[j] ~ dnorm(pop_beta_mu, 1 / (pop_beta_sd^2))T(-1, )
          }
      #likelihood
          for (i in 1:length(emission_xi)) {
            emission_xi[i] ~ dbeta(group_alpha[group_j[i]],
                                   group_beta[group_j[i]])T(minY, maxY)
            pdf_obs[i] = dbeta(emission_xi[i], group_alpha[group_j[i]],
                               group_beta[group_j[i]])
          }

      # derived quantities
          for (g in 1:n_groups){
            source[g] ~ dbeta(group_alpha[g], group_beta[g])T(minY, maxY)
          }
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k] ~ dbeta(pop_alpha_mu, pop_beta_mu)T(minY, maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h] = dbeta(x_hat[h], pop_alpha_mu, pop_beta_mu)
          }
        }", file = JAGS_model)
    }
  }
  setwd(current_wd)
}
