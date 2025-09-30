#' Bayesian_UPL() wraps setup_likelihood(), run_likelihood(), and output_likelihood()
#' @param distr_list any of c('Normal','Skewed','Lognormal','Gamma','Beta') distributions
#' @param future_tests Integer of future runs to use in prediction, the default is 3 since compliance uses 1 test average of 3 runs.
#' @param significance Level of significance from 0 to 1, the default is 0.99.
#' @param xvals ordered sequence of emissions at which to predict probability density. Default is NULL, in which case x_hat is a 1024 length sequence between 0 and 3*max(data$emissions)
#' @param data Emissions data from either the best source or top performers, must have a column named 'emissions'.
#' @returns A list of results from output_likelihood() for run_likelihood() using each distribution in distr_list.
#' @export
#' @description
#' For each distribution in distr_list Bayesian_UPL() will setup_likelihood(), run_likelihood(), and concatenate a list of results from output_likelihood() including the 'predicted_mean' the mean of the fitted distribution, 'UPL_Bayes' the upper predictive limit based on the 'significance' level and average distribution of 'future_tests' number of draws, 'obs_pdf' the predicted probability density at each observation, and 'pred_pdf' the predicted probability density at each point in xvals.
#'
Bayesian_UPL=function(data,distr_list=c('Normal','Skewed','Lognormal','Gamma','Beta'),
                   future_tests=3,significance=0.99,xvals=NULL){
  mod_output_list=c()
  for (j in 1:length(distr_list)){
    distribution=distr_list[j]
    mod_bayes=setup_likelihood(distribution=distribution,data=data)
    mod_run=run_likelihood(data=data,model_input=mod_bayes,
                           future_tests =future_tests,xvals=xvals)
    mod_output=output_likelihood(jags_model_run=mod_run,significance=significance)
    mod_output_list[[j]]=mod_output
    rm(mod_run,mod_output)
    gc()
  }
  return(mod_output_list)
}
