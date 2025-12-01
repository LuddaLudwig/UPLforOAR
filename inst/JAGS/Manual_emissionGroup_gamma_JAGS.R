# Gamma with groups manual priors
      model {
      # priors

          pop_rate_mu ~ dunif(low1, up1) #must be positive
          pop_shape_mu ~ dunif(low2, up2) #must be positive
          pop_rate_sd ~ dunif(low3, up3) #must be positive
          pop_shape_sd ~ dunif(low4, up4) #must be positive

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
            for (h in 1:n_x_hat){
              group_emiss[h, g] = dgamma(x_hat[h], group_shape[g], group_rate[g])
            }
          }
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k] ~ dgamma(pop_shape_mu, pop_rate_mu)T(minY, maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h] = dgamma(x_hat[h], pop_shape_mu, pop_rate_mu)
          }
        }