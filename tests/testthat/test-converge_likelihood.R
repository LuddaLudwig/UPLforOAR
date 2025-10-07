test_that("converge_likelihood() runs gelman diagnostics for convergence tests", {
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
  dat_topmeans=dplyr::summarize(top5,means=mean(emissions),.by='sources',
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
  runmod=run_likelihood(model_input=JAGS_model_stuff,
                        xvals=xvals,future_tests=runcount)
  conv_results=converge_likelihood(runmod)
  expect_equal(round(conv_results$gelman_diag,3),c(1.074,1.041))
  expect_equal(conv_results$params,c('u_ln','sd_ln'))
  expect_equal(conv_results$convYN,c('Yes','Yes'))

})
