test_that("run_likelihoodGroup() runs JAGS models from setup_likelihoodGroup()", {
  set.seed(1)
  xseq = seq(0, 4, length.out = 2000)
  xvals1 = stats::rgamma(36, rate = 4, shape = 0.6)
  names1 = rep("A", length(xvals1))
  xvals2 = stats::rgamma(22, rate = 4.3, shape = 0.5)
  names2 = rep("B", length(xvals2)) # top performer
  xvals3 = stats::rgamma(32, rate = 3.8, shape = 0.5)
  names3 = rep("C", length(xvals3))
  xvals4 = stats::rgamma(18, rate = 4.5, shape = 0.9)
  names4 = rep("D", length(xvals4))
  xvals5 = stats::rgamma(19, rate = 4.1, shape = 0.7)
  names5 = rep("E", length(xvals5))
  top5 = tibble::tibble(emissions = c(xvals1, xvals2, xvals3, xvals4, xvals5),
                        sources = c(names1, names2, names3, names4, names5))
  dat_topmeans=dplyr::summarize(top5,means=mean(emissions),.by='sources',
                                counts=dplyr::n())
  dat_topmeans$sources=as.factor(dat_topmeans$sources)
  dat_topmeans$sources=forcats::fct_reorder(dat_topmeans$sources,
                                            dat_topmeans$means,.desc = FALSE)
  top5$sources=factor(top5$sources,levels=levels(dat_topmeans$sources))
  top5=dplyr::arrange(top5,sources)
  ln_emiss=log(top5$emissions)
  JAGS_model_stuff=setup_likelihoodGroup(data=top5,distribution='Gamma')
  xvals=seq(0,2*max(top5$emissions),length.out=1050)
  runcount=4
  N_parameter = 4
  N_groups = length(unique(top5$sources))
  runmod=run_likelihoodGroup(model_input=JAGS_model_stuff, group = 'sources',
                        xvals=xvals,future_runs=runcount)
  expect_equal(runmod$distribution,'Gamma')
  run_results=runmod$run_results
  run_mcmc=as.matrix(runmod$run_results$mcmc[[1]])
  # saveRDS(run_mcmc,test_path('test_run','test_mcmc_group.rds'))
  load_results=readRDS(test_path('test_run','test_mcmc_group.rds'))
  expect_equal(run_mcmc,load_results)
  expect_equal(runmod$manual_prior,FALSE)
  expect_equal(runmod$data,top5)
  expect_equal(runmod$xvals,xvals)
  expect_equal(runmod$future_runs,runcount)
  expect_equal(dim(run_results$mcmc[[1]]),c(10000,length(xvals)+
                                              runcount+nrow(top5)+N_parameter+
                                              N_groups*2))
  expect_equal(run_results$burnin,20000)
  expect_equal(run_results$monitor,c('emission_hat', 'pdf_obs', 'pdf_hat',
                                     'pop_rate_mu', 'pop_shape_mu', 'pop_rate_sd',
                                     'pop_shape_sd',
                                     'group_rate', 'group_shape', 'group'))
})

