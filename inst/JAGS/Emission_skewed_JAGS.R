data {
              for(i in 1:length(emission_xi)) {
              zeros[i] <- 0
              }
        }
        model {
      # priors
          omega ~ dunif(0,maxY) #must be positive
          xi ~ dnorm(0,1/(100*maxY))
          alpha ~ dunif(-500,500) # this is a very wide range

      #likelihood
          for (i in 1:length(emission_xi)) {
                L[i] <- ( (2/omega)
                * dnorm( (emission_xi[i]-xi)/omega , 0 , 1 )
                * pnorm( alpha*(emission_xi[i]-xi)/omega , 0 , 1 ) )
                zeros[i] ~ dpois(-log(L[i]) + 10000)
          }
        }