#' Runs JAGS model scripts for chosen likelihood
#' @description
#' Runs the JAGS model from setup_likelihood() model_code output.
#' Traces all variables in par_list running 3 chains in parallel with burnin and
#' adapt of 10,000 each and keeping 10,000 iterations per chain.
#' @returns runjags object named run_results, likelihood distribution from the
#' JAGS model script, as well as data and xvals used as inputs.
#' @param model_input results from setup_likelihood(), including JAGS model
#' script, emissions data, distribution, initial values list,
#' and parameters to monitor.
#' @param future_tests Integer of future runs to use in prediction, the default
#' is 3 since compliance uses 1 test average of 3 runs.
#' @param xvals ordered sequence of emissions at which to predict probability
#' density. Default is NULL, in which case x_hat is a 1024 length
#' sequence between 0 and 3*max(data$emissions)
#' @param maxY The maximum emission value possible, used to truncate likelihood
#' distributions and set upper ranges on prior distributions.
#' Default is 3*maximum(data$emissions).
run_likelihood=function(xvals=NULL,model_input,future_tests=3,maxY=NULL){
  data=model_input$data
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = FALSE)
  verify_install=runjags::testjags(silent = TRUE)
  if (!verify_install$JAGS.available){
    stop('Please install JAGS')
  }
  if (is.null(maxY)){
    maxY=3*max(data$emissions)
  }
  if ((model_input$distribution=='Beta')&(maxY>1)){
    stop('Cannot use beta distribution with max emissions greater than 1')
  }
  verify_install$JAGS.available
  n.adapt=10000
  n.update=10000
  n.iter=10000
  if (is.null(xvals)){
    xvals=seq(0,3*max(data$emissions),length.out=1024)
  }
  if ((model_input$distribution=='Beta')&(max(xvals)>1)){
    stop('Cannot use beta distribution with max xvals greater than 1')
  }
  data_list=list(emission_xi=data$emissions,
                 n_draws=future_tests,
                 sdOfLogY=stats::sd(log(data$emissions),na.rm=T),
                 maxY=maxY,sdY=stats::sd(data$emissions),
                 meanOfLogY=mean(log(data$emissions),na.rm=T),
                 pi=pi,x_hat=xvals,n_x_hat=length(xvals))
  cl3=parallel::makeCluster(3)
  rjm=suppressWarnings(runjags::run.jags(model=model_input$model_code,
                                         data=data_list,
               monitor=model_input$par_list,
               method='rjparallel',summarise = FALSE,cl=cl3,
               n.chains=length(model_input$dat_inits), burnin=n.update,
               adapt=n.adapt,sample=n.iter,inits = model_input$dat_inits))
  output=list(run_results=rjm,distribution=model_input$distribution,
              data=model_input$data, xvals=xvals,future_tests=future_tests)
  parallel::stopCluster(cl3)
  return(output)
}

