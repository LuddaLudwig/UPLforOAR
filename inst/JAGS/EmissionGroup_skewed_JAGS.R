# Skewed with groups
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
        }