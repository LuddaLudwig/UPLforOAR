test_that("Bayesian_UPL() wraps setup, run, and output likelihood", {
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
  xvals=seq(0,2*max(top5$emissions),length.out=1050)
  runcount=4
  JAGS_model_stuff1=setup_likelihood(data=top5,distribution='Lognormal')
  runmod1=run_likelihood(data=top5,model_input=JAGS_model_stuff1,
                        xvals=xvals,future_tests=runcount)
  outputresult1=output_likelihood(runmod1)
  JAGS_model_stuff2=setup_likelihood(data=top5,distribution='Skewed')
  runmod2=run_likelihood(data=top5,model_input=JAGS_model_stuff2,
                         xvals=xvals,future_tests=runcount)
  outputresult2=output_likelihood(runmod2)

  output_set=Bayesian_UPL(data=top5,distr_list = c("Lognormal","Skewed"),
               future_tests = runcount,significance = 0.99,xvals=xvals)
  # saveRDS(output_set,'test-Bayes_UPL.rds')
  load_results=readRDS(test_path('test_Bayes_UPL','test-Bayes_UPL.rds'))
  expect_equal(output_set,load_results)
  expect_equal(output_set[1],outputresult1)
  expect_equal(output_set[2],outputresult2)

})

