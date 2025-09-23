#' Writes likelihood scripts for JAGS model calls
#' @description
#' This function writes an R script for JAGS to call based on the selected distribution and prior. The initial values and priors are uninformative and set based on emissions data. The likelihood distributions are truncated to (0,3*max(emissions))
#' @param distribution any of c('Normal','Gamma','Skewed','Lognormal','Beta').
#' @param data Emissions data from either the best source or top performers, must have a column named 'emissions'.
#' @returns object model_code, which is a string for the written R script that JAGS can call, par_list which is the list of parameters traced while running the JAGS model, dat_inits which is a list of initial parameter values and random seeds for 3 chains, and the distribution used in likelihood model.
#'
write_likelihood=function(distribution='Normal',data){
  if (distribution=="Normal"){
    JAGS_model="Emission_normal_JAGS.R"
    par_list=c('emission_hat','pdf_obs','pdf_hat')
    data_inits=list(
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 5,
           'emission_mean'=mean(data$emissions),
           'emission_sd'=stats::sd(data$emissions)),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 12,
           'emission_mean'=1.5*mean(data$emissions),
           'emission_sd'=0.5*stats::sd(data$emissions)),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 151,
           'emission_mean'=0.5*mean(data$emissions),
           'emission_sd'=1.5*stats::sd(data$emissions)))
    cat("model {
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
          for (h in 1:1024){
            pdf_hat[h]<- dnorm(x_hat[h],emission_mean,tau_em)
          }
        }",file=JAGS_model)
  } else if (distribution=="Lognormal"){
    ln_emiss=log(data$emissions)
    JAGS_model="Emission_lnorm_JAGS.R"
    par_list=c('emission_hat','pdf_obs','pdf_hat')
    data_inits=list(
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 5,
           'u_ln'=mean(ln_emiss,na.rm=TRUE),
           'sd_ln'=stats::sd(ln_emiss,na.rm=TRUE)),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 12,
           'u_ln'=1.5*mean(ln_emiss,na.rm=TRUE),
           'sd_ln'=0.5*stats::sd(ln_emiss,na.rm=TRUE)),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 151,
           'u_ln'=0.5*mean(ln_emiss,na.rm=TRUE),
           'sd_ln'=1.5*stats::sd(ln_emiss,na.rm=TRUE)))
    cat("model {
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
          for (h in 1:1024){
            pdf_hat[h]<- dlnorm(x_hat[h],u_ln,tau_ln)
          }
        }",file=JAGS_model)
  } else if (distribution=="Skewed"){
    JAGS_model="Emission_skew_norm_JAGS.R"
    par_list=c('scale','locat','skew')
    data_inits=list(
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 5,
           'locat'=mean(data$emissions),
           'scale'=stats::sd(data$emissions),
           'skew'=1),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 12,
           'locat'=1.5*mean(data$emissions),
           'scale'=0.5*stats::sd(data$emissions),
           'skew'=2),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 151,
           'locat'=0.5*mean(data$emissions),
           'scale'=1.5*stats::sd(data$emissions),
           'skew'=-2))
    cat("data {
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
        }",file=JAGS_model)
  } else if (distribution=='Gamma'){
    JAGS_model="Emission_gamma_JAGS.R"
    par_list=c('emission_hat','pdf_obs','pdf_hat')
    data_inits=list(
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 5,
           'rate_em'=mean(data$emissions),
           'shape_em'=stats::sd(data$emissions)),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 12,
           'rate_em'=1.5*mean(data$emissions),
           'shape_em'=0.5*stats::sd(data$emissions)),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 151,
           'rate_em'=0.5*mean(data$emissions),
           'shape_em'=1.5*stats::sd(data$emissions)))
    cat("model {
      # priors
          rate_em~dunif(0,50)
          shape_em~dunif(0,50)

      #likelihood
          for (i in 1:length(emission_xi)) {
            emission_xi[i]~dgamma(shape_em,rate_em)T(0,3*maxOfY)
            pdf_obs[i] <- dgamma(emission_xi[i],shape_em,rate_em)
          }

      # derived quantities
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k]~dgamma(shape_em,rate_em)T(0,3*maxOfY)
          }
          for (h in 1:1024){
            pdf_hat[h]<- dgamma(x_hat[h],shape_em,rate_em)
          }
        }",file=JAGS_model)
  } else if (distribution=='Beta'){
    JAGS_model="Emission_beta_JAGS.R"
    par_list=c('emission_hat','pdf_obs','pdf_hat')
    data_inits=list(
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 5,
           'beta_em'=mean(data$emissions),
           'alpha_em'=stats::sd(data$emissions)),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 12,
           'beta_em'=1.5*mean(data$emissions),
           'alpha_em'=0.5*stats::sd(data$emissions)),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 151,
           'beta_em'=0.5*mean(data$emissions),
           'alpha_em'=1.5*stats::sd(data$emissions)))
    cat("model {
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
          for (h in 1:1024){
            pdf_hat[h]<- dbeta(x_hat[h],alpha_em,beta_em)
          }
        }",file=JAGS_model)
  }
  output=list(model_code=JAGS_model,par_list=par_list,
              distribution=distribution,dat_inits=data_inits)
  return(output)
}
