data {
              for(i in 1:length(emission_xi)) {
              zeros[i] <- 0
              }
        }
        model {
      # priors
          omega ~ dunif(low1,up1) #must be positive
          xi ~ dunif(low2,up2)
          alpha ~ dunif(low3,up3) # this is a very wide range

      #likelihood
          for (i in 1:length(emission_xi)) {
                L[i] <- ( (2/omega)
                * dnorm( (emission_xi[i]-xi)/omega , 0 , 1 )
                * pnorm( alpha*(emission_xi[i]-xi)/omega , 0 , 1 ) )
                zeros[i] ~ dpois(-log(L[i]) + 10000)
          }
        }