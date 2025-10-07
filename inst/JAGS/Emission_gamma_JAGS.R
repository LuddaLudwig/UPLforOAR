model {
      # priors
          rate_em~dunif(0,maxY/(sdY^2)) #must be positive
          shape_em~dunif(0,(maxY^2)/(sdY^2)) #must be positive

      #likelihood
          for (i in 1:length(emission_xi)) {
            emission_xi[i]~dgamma(shape_em,rate_em)T(0,maxY)
            pdf_obs[i] <- dgamma(emission_xi[i],shape_em,rate_em)
          }

      # derived quantities
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k]~dgamma(shape_em,rate_em)T(0,maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h]<- dgamma(x_hat[h],shape_em,rate_em)
          }
        }