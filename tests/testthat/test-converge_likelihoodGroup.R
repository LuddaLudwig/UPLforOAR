test_that("converge_likelihoodGroup() runs gelman diagnostics for convergence tests", {
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
  JAGS_model_stuff=setup_likelihoodGroup(data=top5, emissions = 'emissions',
                                         group = 'sources',
                                         distribution='Gamma')
  xvals=seq(0,2*max(top5$emissions),length.out=1050)
  runcount=4
  runmod=run_likelihoodGroup(model_input=JAGS_model_stuff,
                             xvals=xvals,future_runs=runcount)
  conv_results=converge_likelihoodGroup(runmod)
  expect_equal(round(conv_results$gelman_diag,3),c(1.001, 1.004, 1.004, 1.022,
                                                   1.002, 1.002, 1.000, 1.001,
                                                   1.002, 1.002, 1.003, 1.001,
                                                   1.002, 1.003))
  expect_equal(conv_results$params,c('pop_rate_mu', 'pop_shape_mu',
                                     'pop_rate_sd', 'pop_shape_sd',
                                     "group_rate_B", "group_rate_C",
                                     "group_rate_A",  "group_rate_E",
                                     "group_rate_D", "group_shape_B",
                                     "group_shape_C", "group_shape_A",
                                     "group_shape_E", "group_shape_D"))
  expect_equal(conv_results$convYN,c('Yes','Yes','Yes','Yes','Yes','Yes','Yes',
                                     'Yes','Yes','Yes','Yes','Yes','Yes','Yes'))

})

