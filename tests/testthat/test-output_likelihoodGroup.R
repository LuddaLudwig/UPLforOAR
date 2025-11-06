test_that("output_likelihoodGroup() organizes mcmc results and calculates UPL", {
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
  runmod=run_likelihoodGroup(model_input=JAGS_model_stuff, group = 'sources',
                            xvals=xvals,future_runs=runcount)
  outputresult=output_likelihoodGroup(runmod)
  # write.csv(outputresult$obs_pdf,test_path('test_output','test-obsGroup_pdf.csv'))
  # write.csv(outputresult$pred_pdf,test_path('test_output','test-predGroup_pdf.csv'))
  # write.csv(outputresult$group_dat,test_path('test_output','test-groupdatGroup_pdf.csv'))
  load_results1=readr::read_csv(test_path('test_output','test-obsGroup_pdf.csv'),
                                col_select = 2:6,show_col_types = FALSE)
  load_results2=readr::read_csv(test_path('test_output','test-predGroup_pdf.csv'),
                                col_select = 2:4,show_col_types = FALSE)
  load_results3=readr::read_csv(test_path('test_output','test-groupdatGroup_pdf.csv'),
                                col_select = 2:3,show_col_types = FALSE)
  attr(load_results1,'spec')=NULL
  attr(load_results2,'spec')=NULL
  attr(load_results3,'spec')=NULL
  attr(outputresult$pred_pdf$pdf_hat,'names')=NULL
  attr(outputresult$UPL_Bayes,'names')=NULL
  expect_equal(outputresult$pred_pdf,load_results2)
  expect_equal(round(outputresult$UPL_Bayes,3),1.324)
  expect_equal(outputresult$distr,'Gamma')
  expect_equal(outputresult$obs_pdf,load_results1)
  expect_equal(outputresult$group_dat,load_results3)
})

