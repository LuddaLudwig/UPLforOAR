#' Bayesian_UPL() wraps setup_likelihood(), run_likelihood(),
#' output_likelihood(), converge_likelihood(), and fit_likelihood()
#' @param distr_list A list including one or more of
#' c('Normal','Skewed','Lognormal','Gamma','Beta'). Note that if prior bounds
#' are supplied manually, only one distribution can be used.
#' @param future_tests Integer of future runs to use in prediction, the default
#' is 3 since compliance uses 1 test average of 3 runs.
#' @param significance Level of significance from 0 to 1, the default is 0.99.
#' @param xvals Ordered sequence of emissions at which to predict probability
#' density. Default is NULL, in which case x_hat is a 1024 length sequence
#' between 0 and 3*max(data$emissions).
#' @param maxY The maximum emission value possible, used to truncate likelihood
#' distributions and set upper ranges on prior distributions, not specified
#' manually. Default is 3*maximum(data$emissions).
#' @param data Emissions data from either the best source or top performers,
#' must have a column named 'emissions'.
#' @param prior_list Optional list of dunif() upper and lower bounds for prior
#' distributions. For 'Normal' they are ordered c(sd_low, sd_high, mean_low, mean_high).
#' For 'Lognormal' they are ordered c(log_sd_low, log_sd_high, log_mean_low, log_mean_high).
#' For 'Skewed' they are ordered c(omega_low, omega_high, xi_low, xi_high, alpha_low, alpha_high).
#' For 'Gamma' they are ordered c(rate_low, rate_high, shape_low, shape_high). For
#' 'Beta' they are ordered c(alpha_low, alpha_high, beta_low, beta_high).
#' @param convergence_report Default is FALSE, if a report containing
#' convergence figures should be generated with results. If TRUE, a document
#' Bayesian_UPL_convergence_MMDDYYY_HHMM.pdf will be written to the current
#' working directory.
#' @param manual_prior Default is FALSE, if priors should be specified manually
#' or be uninformative calculated from range of emissions data. Note that if you
#' are supplying priors manually than you can only run one type of distribution
#' at a time.
#' @returns A list of tibble results from setup_likelihood(), run_likelihood(),
#' output_likelihood(), obs_density(), fit_likelihood(), and
#' converge_likelihood() for each distribution in 'distr_list'.
#' @export
#' @description
#' For each distribution in 'distr_list', Bayesian_UPL() will setup_likelihood(),
#' run_likelihood(), organize mcmc results in output_likelihood(), test for
#' convergence of likelihood parameters using converge_likelihood(), and
#' calculate goodness of fit metrics using fit_likelihood(). Results include
#' $fit_table: a tibble with the UPL, pdf_integral, SSE, and count of
#' observations within 95% CI for each distribution in 'distr_list',
#' $conv_output: a tibble with the parameters, Gelman-Rubin diagnostics, and if
#' the converged for distribution in 'distr_list', $obs_pdf_dat: a tibble with
#' the emissions observations, corresponding observation densities, median,
#' upper, and lower 95% CI around predicted densities for the distribution in
#' 'distr_list', and a 1 if the observation is within the 95% CI, a 0 otherwise,
#' and $pred_pdf_dat:a tibble with the predicted probability density 'pdf_hat',
#' the observation density 'ydens' for each value in the range of emissions in
#' 'x_hat'. The maximum emission value of distributions, 'maxY', the ordered range
#' emissions to predict to 'xvals', and the prior distributions and initial
#' values are all automatically supplied from the emissions data to be fully
#' encompassing and uninformative by default. They can be supplied manually
#' instead however by supplying 'maxY', 'xvals', or setting manual_prior=TRUE
#' with corresponding lower and upper limits in 'prior_list'. If manual priors
#' are used, only a single distribution can be run at a time in 'distr_list'.
#'
Bayesian_UPL=function(distr_list=c('Normal','Skewed','Lognormal','Gamma','Beta'),
                      data,future_tests=3,significance=0.99,xvals=NULL,
                      convergence_report=FALSE, maxY=NULL,
                      manual_prior=FALSE,prior_list=NULL){
  if (convergence_report==TRUE){
    figs_list=list()
  }
  mod_output_list=c()
  conv_output=tibble::tibble()
  if (manual_prior){
    if (length(distr_list)>1){
      stop('You can only run one distribution at a time if supplying priors manually')
    }
    distribution=distr_list[1]
    mod_bayes=setup_likelihood(distribution=distribution,data=data,
                               manual_prior=manual_prior,prior_list=prior_list)
    mod_run=run_likelihood(model_input=mod_bayes,maxY=maxY,
                           future_tests =future_tests,xvals=xvals)
    manual_prior=mod_bayes$manual_prior

    mod_output=output_likelihood(jags_model_run=mod_run,
                                 significance=significance)
    mod_fit=fit_likelihood(likelihood_result=mod_output)
    mod_output_list[[1]]=mod_fit
    mod_converge=converge_likelihood(mod_run)
    conv_output=rbind(conv_output,mod_converge)
    if (convergence_report==TRUE){
      fig_set=converge_figs(distribution,mod_run)
      figs_list[[1]]=fig_set
      rm(fig_set)
    }
    rm(mod_run,mod_output,mod_fit)
    gc()
  }

  if (!manual_prior){
    for (j in 1:length(distr_list)){
      distribution=distr_list[j]
      mod_bayes=setup_likelihood(distribution=distribution,data=data)
      mod_run=run_likelihood(model_input=mod_bayes,maxY=maxY,manual_prior=FALSE,
                             future_tests =future_tests,xvals=xvals)
      manual_prior=mod_bayes$manual_prior

      mod_output=output_likelihood(jags_model_run=mod_run,
                                   significance=significance)
      mod_fit=fit_likelihood(likelihood_result=mod_output)
      mod_output_list[[j]]=mod_fit
      mod_converge=converge_likelihood(mod_run)
      conv_output=rbind(conv_output,mod_converge)
      if (convergence_report==TRUE){
        fig_set=converge_figs(distribution,mod_run)
        figs_list[[j]]=fig_set
        rm(fig_set)
      }
      rm(mod_run,mod_output,mod_fit)
      gc()
    }
  }

  if (convergence_report==TRUE){
    current_wd=getwd()
    template_path=system.file("templates",package="EPA.MACT.floor.UPL",
                              mustWork=TRUE)
    rmarkdown::render(paste0(template_path,'/convergence_template.Rmd'),
                      output_dir = current_wd,
                      output_file = paste0('Bayesian_UPL_convergence_',
                                           format(Sys.time(),"%m%d%Y-%H%M")))
  }
  fit_table=tibble::tibble(distr=unlist(lapply(mod_output_list,'[[','distr')),
                   UPL=(as.numeric(lapply(mod_output_list,'[[','UPL_Bayes'))),
                   pdf_integral=(as.numeric(lapply(mod_output_list,'[[','pdf_integral'))),
                   SSE=(as.numeric(lapply(mod_output_list,'[[','SSE'))),
                   Obs_in_CI=(as.numeric(lapply(mod_output_list,'[[','good_vals')))
                   )
  obs_pdf_dat=tibble::tibble()
  for (i in 1:length(distr_list)){
    obs_temp=mod_output_list[[i]]$obs_pdf_dat
    obs_pdf_dat=rbind(obs_pdf_dat,obs_temp)
  }
  pred_pdf_dat=tibble::tibble()
  for (i in 1:length(distr_list)){
    pred_temp=mod_output_list[[i]]$xhat_pdf_dat
    pred_pdf_dat=rbind(pred_pdf_dat,pred_temp)
  }
  if (any(conv_output$convYN!="Yes")){
    warning('Some parameters have not converged')
  }
  return_list=list(fit_table=fit_table,
                   conv_output=conv_output,
                   obs_pdf_dat=obs_pdf_dat,
                   pred_pdf_dat=pred_pdf_dat)
  return(return_list)
}
