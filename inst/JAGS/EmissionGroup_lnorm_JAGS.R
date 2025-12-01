# Lognormal with groups
      model {
      # priors

          # pop_mu_mu is in log-space, so it can be negative
          pop_mu_mu ~ dnorm(meanOfLogY, 0.001 / sdOfLogY^2)
          pop_mu_sd ~ dunif(0, 1000 * sdOfLogY) # must be positive
          pop_sd_mu ~ dunif(0, 1000 * sdOfLogY) # must be positive
          pop_sd_sd ~ dunif(0, 1000 * sdOfLogY) # must be positive

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
              for (h in 1:n_x_hat){
                group_emiss[h, g] = dlnorm(x_hat[h], group_mu[g], 1 / (group_sd[g]^2))
              }
          }
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k] ~ dlnorm(pop_mu_mu, 1 / (pop_sd_mu^2))T(minY, maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h] = dlnorm(x_hat[h], pop_mu_mu, 1 / (pop_sd_mu^2))
          }
        }