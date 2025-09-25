model {
      # priors
          beta_em~dunif(0,50)
          alpha_em~dunif(0,50)

      #likelihood
          for (i in 1:length(emission_xi)) {
            emission_xi[i]~dbeta(alpha_em,beta_em)T(0,3*maxOfY)
            pdf_obs[i] <- dbeta(emission_xi[i],alpha_em,beta_em)
          }

      # derived quantities
      emission_mean<-alpha_em/(alpha_em+beta_em)
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k]~dbeta(alpha_em,beta_em)T(0,3*maxOfY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h]<- dbeta(x_hat[h],alpha_em,beta_em)
          }
        }