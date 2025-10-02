#' Bayesian_UPL() wraps setup_likelihood(), run_likelihood(),
#' output_likelihood(), converge_likelihood(), and fit_likelihood()
#' @param distr_list any of c('Normal','Skewed','Lognormal','Gamma','Beta')
#' @param future_tests Integer of future runs to use in prediction, the default
#' is 3 since compliance uses 1 test average of 3 runs.
#' @param significance Level of significance from 0 to 1, the default is 0.99.
#' @param xvals ordered sequence of emissions at which to predict probability
#' density. Default is NULL, in which case x_hat is a 1024 length sequence
#' between 0 and 3*max(data$emissions)
#' @param data Emissions data from either the best source or top performers,
#' must have a column named 'emissions'.
#' @param convergence_report Default is FALSE, if a report containing
#' convergence figures should be generated with results.
#' @returns A list of results from output_likelihood(), fit_likelihood,
#' and converge_likelihood() for run_likelihood()
#' using each distribution in distr_list.
#' @export
#' @description
#' For each distribution in distr_list Bayesian_UPL() will setup_likelihood(),
#' run_likelihood(), test for convergence of likelihood parameters using
#' converge_likelihood, and concatenate a list of results from output_likelihood()
#' including the 'predicted_mean' the mean of the fitted distribution,
#' 'UPL_Bayes' the upper predictive limit based on the 'significance' level and
#' average distribution of 'future_tests' number of draws, 'obs_pdf' the
#' predicted probability density at each observation, and 'pred_pdf' the
#' predicted probability density at each point in xvals. Then fit_likelihood will
#' add calculations of fit metrics to each distribution result.
#'
Bayesian_UPL=function(data,distr_list=c('Normal','Skewed','Lognormal','Gamma','Beta'),
                   future_tests=3,significance=0.99,xvals=NULL,
                   convergence_report=FALSE){
  mod_output_list=c()
  conv_output=tibble()
  if (convergence_report==TRUE){
    figs_list=c()
  }
  for (j in 1:length(distr_list)){
    distribution=distr_list[j]
    mod_bayes=setup_likelihood(distribution=distribution,data=data)
    mod_run=run_likelihood(model_input=mod_bayes,
                           future_tests =future_tests,xvals=xvals)
    mod_output=output_likelihood(jags_model_run=mod_run,significance=significance)
    mod_fit=fit_likelihood(likelihood_result=mod_output)
    mod_output_list[[j]]=mod_fit
    mod_converge=converge_likelihood(mod_run)
    conv_output=rbind(conv_output,mod_converge)
    if (convergence_report==TRUE){
      figs_list=cbind(figs_list,converge_figs(distribution,mod_run))
    }
    rm(mod_run,mod_output,mod_fit)
    gc()
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
  fit_table=tibble(distr=unlist(lapply(mod_output_list,'[[','distr')),
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
