model {
      # priors
          # note that both can be pos or neg, but the minimum is always -1
          alpha_em~dunif(low1,up1)
          beta_em~dunif(low2,up2)

      #likelihood
          for (i in 1:length(emission_xi)) {
            emission_xi[i]~dbeta(alpha_em,beta_em)T(0,maxY)
            pdf_obs[i] <- dbeta(emission_xi[i],alpha_em,beta_em)
          }

      # derived quantities
      emission_mean<-alpha_em/(alpha_em+beta_em)
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k]~dbeta(alpha_em,beta_em)T(0,maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h]<- dbeta(x_hat[h],alpha_em,beta_em)
          }
        }