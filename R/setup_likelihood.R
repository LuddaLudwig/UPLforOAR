#' Sets up path to JAGS script, initial values, and variable list to monitor
#' @description
#' This function defines the jagsmodel script to call based on the selected distribution. It also defines the initial values and variables to monitor.
#' @param distribution any of c('Normal','Gamma','Skewed','Lognormal','Beta').
#' @param data Emissions data from either the best source or top performers, must have a column named 'emissions'.
#' @returns object model_code, which is a string for the written R script that JAGS can call, par_list which is the list of parameters traced while running the JAGS model, dat_inits which is a list of initial parameter values and random seeds for 3 chains, and the distribution used in likelihood model.
#'
setup_likelihood=function(distribution,data){
  JAGS_path=system.file("JAGS",package="EPA.MACT.floor.UPL",mustWork=TRUE)
  if (distribution=="Normal"){
    JAGS_model=runjags::read.jagsfile(paste0(JAGS_path,
                                             '/Emission_normal_JAGS.R'))
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
  } else if (distribution=="Lognormal"){
    ln_emiss=log(data$emissions)
    JAGS_model=runjags::read.jagsfile(paste0(JAGS_path,
                                             '/Emission_lnorm_JAGS.R'))
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
  } else if (distribution=="Skewed"){
    JAGS_model=runjags::read.jagsfile(paste0(JAGS_path,
                                             '/Emission_skewed_JAGS.R'))
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
  } else if (distribution=='Gamma'){
    JAGS_model=runjags::read.jagsfile(paste0(JAGS_path,
                                             '/Emission_gamma_JAGS.R'))
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
  } else if (distribution=='Beta'){
    JAGS_model=runjags::read.jagsfile(paste0(JAGS_path,
                                             '/Emission_beta_JAGS.R'))
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
  }
  output=list(model_code=JAGS_model,par_list=par_list,
              distribution=distribution,dat_inits=data_inits)
  return(output)
}
