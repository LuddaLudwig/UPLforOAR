test_that("fit_likelihood() compares predicted and observed density distributions", {
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
                        xvals=xvals,future_runs=runcount)
  outputresult=output_likelihood(runmod)
  fit_results=fit_likelihood(outputresult)
  expect_equal(round(fit_results$pdf_integral,3),0.921)
  expect_equal(fit_results$distr,'Lognormal')
  expect_equal(round(fit_results$SSE,3),0.371)
  expect_equal(nrow(fit_results$obs_pdf_dat),nrow(top5))
  expect_equal(nrow(fit_results$xhat_pdf_dat),length(xvals))
})

