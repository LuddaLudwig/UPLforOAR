model {
      # priors
          emission_mean~dunif(0,10)
          emission_sd~dunif(0,5)
          tau_em<- 1/(emission_sd^2)

      #likelihood
          for (i in 1:length(emission_xi)) {
            emission_xi[i]~dnorm(emission_mean,tau_em)T(0,3*maxOfY)
            pdf_obs[i] <- dnorm(emission_xi[i],emission_mean,tau_em)
          }

      # derived quantities
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k]~dnorm(emission_mean,tau_em)T(0,3*maxOfY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h]<- dnorm(x_hat[h],emission_mean,tau_em)
          }
        }