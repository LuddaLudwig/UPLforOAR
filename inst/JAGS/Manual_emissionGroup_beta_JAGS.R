# Beta with groups manual priors
      model {
      # priors
          # note that both can be pos or neg, but the minimum is always -1
          pop_alpha_mu ~ dunif(low1, up1)
          pop_beta_mu ~ dunif(low2, up2)
          pop_alpha_sd ~ dunif(low3, up3)
          pop_beta_sd ~ dunif(low4, up4)

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
            group_emiss[g] ~ dbeta(group_alpha[g], group_beta[g])T(minY, maxY)
          }
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k] ~ dbeta(pop_alpha_mu, pop_beta_mu)T(minY, maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h] = dbeta(x_hat[h], pop_alpha_mu, pop_beta_mu)
          }
        }