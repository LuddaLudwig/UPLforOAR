#' Writes likelihood scripts for JAGS model calls
#' @description
#' This function writes an R script for JAGS to call based on the selected
#' distribution and prior. The priors are uninformative and
#' set based on emissions data, unless specified manually via [setup_likelihood()].
#' The likelihood distributions are truncated to `(0,maxY)`, where `maxY` can be
#' specified or used with the default `maxY = 3*max(data$emissions)` in [run_likelihood()].
#' @param distribution Any of `'Normal'`, `'Gamma'`, `'Skewed'`, `'Lognormal'`, or `'Beta'`.
#' @param write_wd Default is `NULL`, in which case the JAGS scripts are written
#' into inst/JAGS folder in package directory. This is the location
#' [run_likelihood()] will look for the JAGS scripts assigned via [setup_likelihood()].
#' @param manual_prior Default is `FALSE`, if priors should be specified manually
#' or be uninformative calculated from range of emissions data.
#' @returns object `model_code`, which is a string for the written R script that
#' JAGS can call and the distribution used in likelihood model.
#' @export
write_likelihood=function(distribution,manual_prior=FALSE,write_wd=NULL){
  current_wd=getwd()
  if (is.null(write_wd)){
    write_wd='inst/JAGS/'
  }
  setwd(write_wd)
  if (!manual_prior){
    if (distribution=="Normal"){
      JAGS_model="Emission_normal_JAGS.R"
      cat("model {
      # priors
          emission_sd~dunif(0.001*sdY,1000*sdY)
          emission_mean~dunif(0,maxY)
          tau_em<- 1/(emission_sd^2)

      #likelihood
          for (i in 1:length(emission_xi)) {
            emission_xi[i]~dnorm(emission_mean,tau_em)T(0,maxY)
            pdf_obs[i] <- dnorm(emission_xi[i],emission_mean,tau_em)
          }

      # derived quantities
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k]~dnorm(emission_mean,tau_em)T(0,maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h]<- dnorm(x_hat[h],emission_mean,tau_em)
          }
        }",file=JAGS_model)
    } else if (distribution=="Lognormal"){
      JAGS_model="Emission_lnorm_JAGS.R"
      cat("model {
      # priors
          sd_ln ~ dunif( 0.001*sdOfLogY , 1000*sdOfLogY )
          u_ln ~ dnorm( meanOfLogY , 0.001*1/sdOfLogY^2 ) #this is in log-space, so it can be negative
          tau_ln<-1/(sd_ln^2)

      #likelihood
          for (i in 1:length(emission_xi)) {
            emission_xi[i]~dlnorm(u_ln,tau_ln)T(0,maxY)
            pdf_obs[i] <- dlnorm(emission_xi[i],u_ln,tau_ln)
          }

      # derived quantities
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k]~dlnorm(u_ln,tau_ln)T(0,maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h]<- dlnorm(x_hat[h],u_ln,tau_ln)
          }
        }",file=JAGS_model)
    } else if (distribution=="Skewed"){
      JAGS_model="Emission_skewed_JAGS.R"
      cat("data {
              for(i in 1:length(emission_xi)) {
              zeros[i] <- 0
              }
        }
        model {
      # priors
          omega ~ dunif(0,maxY) #must be positive
          xi ~ dnorm(0,1/(100*maxY))
          alpha ~ dunif(-100,100) # this is a very wide range

      #likelihood
          for (i in 1:length(emission_xi)) {
                L[i] <- ( (2/omega)
                * dnorm( (emission_xi[i]-xi)/omega , 0 , 1 )
                * pnorm( alpha*(emission_xi[i]-xi)/omega , 0 , 1 ) )
                zeros[i] ~ dpois(-log(L[i]) + 10000)
          }
        }",file=JAGS_model)
    } else if (distribution=='Gamma'){
      JAGS_model="Emission_gamma_JAGS.R"
      cat("model {
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
        }",file=JAGS_model)
    } else if (distribution=='Beta'){
      JAGS_model="Emission_beta_JAGS.R"
      cat("model {
      # priors
          # note that both can be pos or neg, but the minimum is always -1
          beta_em~dunif(-1,(0.25-1+0.25/sdY^2+0.25^3/sdY^2-2*0.25^2/sdY^2)*1.25)
          alpha_em~dunif(-1,(0.75^2/sdY^2-0.75^3/sdY^2-0.75)*1.25)

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
        }",file=JAGS_model)
    }
  } else if (manual_prior){
    if (distribution=="Normal"){
      JAGS_model="Manual_emission_normal_JAGS.R"
      cat("model {
      # priors
          emission_sd~dunif(low1,up1)
          emission_mean~dunif(low2,up2)
          tau_em<- 1/(emission_sd^2)

      #likelihood
          for (i in 1:length(emission_xi)) {
            emission_xi[i]~dnorm(emission_mean,tau_em)T(0,maxY)
            pdf_obs[i] <- dnorm(emission_xi[i],emission_mean,tau_em)
          }

      # derived quantities
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k]~dnorm(emission_mean,tau_em)T(0,maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h]<- dnorm(x_hat[h],emission_mean,tau_em)
          }
        }",file=JAGS_model)
    } else if (distribution=="Lognormal"){
      JAGS_model="Manual_emission_lnorm_JAGS.R"
      cat("model {
      # priors
          sd_ln ~ dunif(low1,up1)
          u_ln ~ dnorm(low2,up2) #this is in log-space, so it can be negative
          tau_ln<-1/(sd_ln^2)

      #likelihood
          for (i in 1:length(emission_xi)) {
            emission_xi[i]~dlnorm(u_ln,tau_ln)T(0,maxY)
            pdf_obs[i] <- dlnorm(emission_xi[i],u_ln,tau_ln)
          }

      # derived quantities
      # predict new emission tests
          for (k in 1:n_draws){
            emission_hat[k]~dlnorm(u_ln,tau_ln)T(0,maxY)
          }
          for (h in 1:n_x_hat){
            pdf_hat[h]<- dlnorm(x_hat[h],u_ln,tau_ln)
          }
        }",file=JAGS_model)
    } else if (distribution=="Skewed"){
      JAGS_model="Manual_emission_skewed_JAGS.R"
      cat("data {
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
        }",file=JAGS_model)
    } else if (distribution=='Gamma'){
      JAGS_model="Manual_emission_gamma_JAGS.R"
      cat("model {
      # priors
          rate_em~dunif(low1,up1) #must be positive
          shape_em~dunif(low2,up2) #must be positive

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
        }",file=JAGS_model)
    } else if (distribution=='Beta'){
      JAGS_model="Manual_emission_beta_JAGS.R"
      cat("model {
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
        }",file=JAGS_model)
    }
  }

  setwd(current_wd)
}
