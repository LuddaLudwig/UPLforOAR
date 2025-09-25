#' Runs JAGS model scripts for chosen likelihood
#' @description
#' Runs the JAGS model from setup_likelihood() model_code output. Traces all variables in par_list running 3 chains in parallel with burnin and adapt of 10,000 each and keeping 10,000 iterations per chain.
#' @returns runjags object named run_results, likelihood distribution from the JAGS model script, as well as data and xvals used as inputs.
#' @param data Emissions data from either the best source or top performers, must have a column named 'emissions'. Should be the same data used in write_likelihood().
#' @param model_input results from setup_likelihood(), including JAGS model script, distribution, initial values list, and parameters to monitor.
#' @param future_tests Integer of future runs to use in prediction, the default is 3 since compliance uses 1 test average of 3 runs.
#' @param xvals ordered sequence of emissions at which to predict probability density. Default is NULL, in which case x_hat is a 1024 length sequence between 0 and 3*max(data$emissions)
#'
run_likelihood=function(data,xvals=NULL,model_input,future_tests=3){
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = FALSE)
  verify_install=runjags::testjags(silent = TRUE)
  if (!verify_install$JAGS.available){
    stop('Please install JAGS')
  }
  verify_install$JAGS.available
  n.adapt=10000
  n.update=10000
  n.iter=10000
  if (is.null(xvals)){
    xvals=seq(0,3*max(data$emissions),length.out=1024)
  }
  data_list=list(emission_xi=data$emissions,
                 n_draws=future_tests,
                 sdOfLogY=stats::sd(log(data$emissions),na.rm=T),
                 maxOfY=max(data$emissions),
                 meanOfLogY=mean(log(data$emissions),na.rm=T),
                 pi=pi,x_hat=xvals,n_x_hat=length(xvals))
  cl3=parallel::makeCluster(3)
  rjm=suppressWarnings(runjags::run.jags(model=model_input$model_code,
                                         data=data_list,
               monitor=model_input$par_list,
               method='rjparallel',summarise = FALSE,cl=cl3,
               n.chains=length(model_input$dat_inits), burnin=n.update,
               adapt=n.adapt,sample=n.iter,inits = model_input$dat_inits))
  output=list(run_results=rjm,distribution=model_input$distribution,data=data,
              xvals=xvals,future_tests=future_tests)
  parallel::stopCluster(cl3)
  return(output)
}

