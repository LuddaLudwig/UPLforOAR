data {
              for(i in 1:length(emission_xi)) {
              zeros[i] <- 0
              }
        }
        model {
      # priors
          scale ~ dunif(0,50)
          locat ~ dnorm(0,1/10^2)
          skew ~ dnorm(0,1/10^2)

      #likelihood
          for (i in 1:length(emission_xi)) {
                L[i] <- ( (2/scale)
                * dnorm( (emission_xi[i]-locat)/scale , 0 , 1 )
                * pnorm( skew*(emission_xi[i]-locat)/scale , 0 , 1 ) )
                zeros[i] ~ dpois(-log(L[i]) + 10000)
          }
        }