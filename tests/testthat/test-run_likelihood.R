test_that("run_likelihood() runs JAGS models from setup_likelihood()", {
  top5=tibble::tibble(emissions=c(1,2,1.5,
                                  1.2,3,2.2,
                                  0.2,0.4,0.7,
                                  0.1,0.2,0.3,1.1,1.2,1.3,
                                  0.1,0.1,0.15,0.16,0.19,1.8),
                      sources=c(rep('A',3),
                                rep('B',3),
                                rep('C',3),
                                rep('D',6),
                                rep('E',6)))
  dat_topmeans=top5%>%dplyr::group_by(sources)%>%dplyr::summarize(means=mean(emissions),
                                                                  counts=dplyr::n())
  dat_topmeans$sources=as.factor(dat_topmeans$sources)
  dat_topmeans$sources=forcats::fct_reorder(dat_topmeans$sources,
                                            dat_topmeans$means,.desc = FALSE)
  top5$sources=factor(top5$sources,levels=levels(dat_topmeans$sources))
  top5=dplyr::arrange(top5,sources)
  ln_emiss=log(top5$emissions)
  JAGS_model_stuff=setup_likelihood(data=top5,distribution='Lognormal')
  xvals=seq(0,2*max(top5$emissions),length.out=1050)
  runcount=4
  runmod=run_likelihood(data=top5,model_input=JAGS_model_stuff,
                        xvals=xvals,future_tests=runcount)
  expect_equal(runmod$distribution,'Lognormal')
  run_results=runmod$run_results
  run_mcmc=as.matrix(runmod$run_results$mcmc[[1]])
  # saveRDS(run_mcmc,'test_mcmc.rds')
  load_results=readRDS(test_path('test_run','test_mcmc.rds'))
  expect_equal(run_results,load_results)
  expect_equal(dim(run_results$mcmc[[1]]),c(10000,length(xvals)+runcount+nrow(top5)))
  expect_equal(run_results$burnin,20000)
  expect_equal(run_results$monitor,c("emission_hat",'pdf_obs','pdf_hat'))
})
