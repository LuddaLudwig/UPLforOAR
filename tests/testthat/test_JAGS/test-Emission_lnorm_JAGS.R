model {
      # priors
          sd_ln ~ dunif( 0.001*sdOfLogY , 1000*sdOfLogY )
          u_ln ~ dnorm( meanOfLogY , 0.001*1/sdOfLogY^2 )
          emission_mean <- exp(u_ln+sd_ln^2/2)
          emission_sd <- sqrt(exp(2*u_ln+sd_ln^2)*(exp(sd_ln^2)-1))
          tau_ln<-1/(sd_ln^2)

      #likelihood
          for (i in 1:length(emission_xi)) {
            emission_xi[i]~dlnorm(u_ln,tau_ln)T(0,3*maxOfY)
            pdf_obs[i] <- dlnorm(emission_xi[i],u_ln,tau_ln)
          }

      # derived quantities
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k]~dlnorm(u_ln,tau_ln)T(0,3*maxOfY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h]<- dlnorm(x_hat[h],u_ln,tau_ln)
          }
        }