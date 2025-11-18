test_that("converge_figsGroup() makes posterior plots of mcmc iter and histogram", {
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

  part1=setup_likelihoodGroup(distribution = "Gamma",data = top5,
                              emissions = 'emissions')
  part2=run_likelihoodGroup(model_input = part1)
  part3=converge_figsGroup(distribution = "Gamma",jags_model_run = part2)
  expect_equal(length(part3),14)
  fig1=part3[[1]]
  expect_equal(length(fig1),2)
  fig1a=fig1[[1]]
  expect_equal(names(fig1a@layers),c("geom_line","geom_line...2","geom_line...3"))
  expect_equal(fig1a@labels$x,"Iterations")
  fig2=part3[[2]]
  fig2b=fig2[[2]]
  expect_equal(fig2b@labels$x,"pop_shape_mu")
  expect_equal(fig2@meta$patches$annotation$title,"Gamma")

})

