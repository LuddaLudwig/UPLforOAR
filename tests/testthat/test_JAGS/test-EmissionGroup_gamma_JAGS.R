# Gamma with groups
      model {
      # priors

          pop_rate_mu ~ dunif(0, maxY / (sdY^2)) #must be positive
          pop_shape_mu ~ dunif(0, (maxY^2) / (sdY^2)) #must be positive
          pop_rate_sd ~ dunif(0, maxY / (sdY^2)) #must be positive
          pop_shape_sd ~ dunif(0, (maxY^2) / (sdY^2)) #must be positive

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
            group_emiss[g] ~ dgamma(group_shape[g], group_rate[g])T(minY, maxY)
          }
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k] ~ dgamma(pop_shape_mu, pop_rate_mu)T(minY, maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h] = dgamma(x_hat[h], pop_shape_mu, pop_rate_mu)
          }
        }