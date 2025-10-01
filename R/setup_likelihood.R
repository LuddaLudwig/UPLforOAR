#' Sets up path to JAGS script, initial values, and variable list to monitor
#' @description
#' This function defines the jagsmodel script to call based on the selected
#' distribution. It also defines the initial values and variables to monitor.
#' @param distribution any of c('Normal','Gamma','Skewed','Lognormal','Beta').
#' @param data Emissions data from either the best source or top performers,
#' must have a column named 'emissions'.
#' @returns object model_code, which is a string for the written R script that
#' JAGS can call, par_list which is the list of parameters traced while running
#' the JAGS model, dat_inits which is a list of initial parameter values and
#' random seeds for 3 chains, and the distribution used in likelihood model.
#'
setup_likelihood=function(distribution,data){
  JAGS_path=system.file("JAGS",package="EPA.MACT.floor.UPL",mustWork=TRUE)
  mu=mean(data$emissions)
  sigma=stats::sd(data$emissions)
  if (distribution=="Normal"){
    JAGS_model=runjags::read.jagsfile(paste0(JAGS_path,
                                             '/Emission_normal_JAGS.R'))
    par_list=c('emission_hat','pdf_obs','pdf_hat',
               'emission_mean','emission_sd')
    data_inits=list(
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 5,
           'emission_mean'=mu,'emission_sd'=sigma),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 12,
           'emission_mean'=1.5*mu, 'emission_sd'=0.5*sigma),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 151,
           'emission_mean'=0.5*mu, 'emission_sd'=1.5*sigma))
  } else if (distribution=="Lognormal"){
    ln_emiss=log(data$emissions)
    ln_mu=mean(ln_emiss,na.rm=TRUE)
    ln_sig=stats::sd(ln_emiss,na.rm=TRUE)
    JAGS_model=runjags::read.jagsfile(paste0(JAGS_path,
                                             '/Emission_lnorm_JAGS.R'))
    par_list=c('emission_hat','pdf_obs','pdf_hat',
               'u_ln','sd_ln')
    data_inits=list(
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 5,
           'u_ln'=ln_mu, 'sd_ln'=ln_sig),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 12,
           'u_ln'=1.5*ln_mu,'sd_ln'=0.5*ln_sig),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 151,
           'u_ln'=0.5*ln_mu, 'sd_ln'=1.5*ln_sig))
  } else if (distribution=="Skewed"){
    skew1=min(0.99,abs((1/length(data$emissions))*sum(((data$emissions-mu)/sigma)^3)))
    delta=sqrt((pi/2)*((abs(skew1)^(2/3))/((abs(skew1)^(2/3))+((4-pi)/2)^(2/3))))
    delta=delta*abs(skew1)/skew1
    alpha=delta/sqrt(1-delta^2)
    omega=sigma/sqrt(1-2*delta^2/pi) #must be positive
    xi=mu-omega*delta*sqrt(2/pi)
    JAGS_model=runjags::read.jagsfile(paste0(JAGS_path,
                                             '/Emission_skewed_JAGS.R'))
    par_list=c('omega','xi','alpha')
    data_inits=list(
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 5,
           'xi'=xi,'omega'=omega,'alpha'=alpha),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 12,
           'xi'=1.5*xi, 'omega'=0.5*omega, 'alpha'=0.01),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 151,
           'xi'=0.5*xi, 'omega'=1.5*omega,'alpha'=-alpha))
  } else if (distribution=='Gamma'){
    shape=mu^2/sigma^2
    rate=mu/sigma^2
    JAGS_model=runjags::read.jagsfile(paste0(JAGS_path,
                                             '/Emission_gamma_JAGS.R'))
    par_list=c('emission_hat','pdf_obs','pdf_hat',
               'rate_em','shape_em')
    data_inits=list(
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 5,
           'rate_em'=rate,
           'shape_em'=shape),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 12,
           'rate_em'=1.5*rate,
           'shape_em'=0.5*shape),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 151,
           'rate_em'=0.5*rate,
           'shape_em'=1.5*shape))
  } else if (distribution=='Beta'){
    if (min(data$emissions)<0){
      stop('Cannot use beta distribution with emissions less than 0')
    }
    if (max(data$emissions)>1){
      stop('Cannot use beta distribution with emissions greater than 1')
    }
    alpha=mu^2/sigma^2-mu^3/sigma^2-mu
    beta=mu-1+mu/sigma^2+mu^3/sigma^2-2*mu^2/sigma^2
    JAGS_model=runjags::read.jagsfile(paste0(JAGS_path,
                                             '/Emission_beta_JAGS.R'))
    par_list=c('emission_hat','pdf_obs','pdf_hat',
               'alpha_em','beta_em')
    data_inits=list(
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 5,
           'beta_em'=beta,
           'alpha_em'=alpha),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 12,
           'beta_em'=1.5*beta,
           'alpha_em'=0.5*alpha),
      list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 151,
           'beta_em'=0.5*beta,
           'alpha_em'=1.5*alpha))
  }
  output=list(model_code=JAGS_model,par_list=par_list,data=data,
              distribution=distribution,dat_inits=data_inits)
  return(output)
}
