# Normal with groups manual priors
      model {
      # priors
          pop_mu_mu ~ dunif(low2, up2)
          pop_mu_sd ~ dunif(low4, up4)
          pop_sd_mu ~ dunif(low1, up1)
          pop_sd_sd ~ dunif(low3, up3)

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
        }