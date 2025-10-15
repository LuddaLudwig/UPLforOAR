test_that("converge_figs() makes posterior plots of mcmc iter and histogram", {
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

  part1=setup_likelihood(distribution = "Gamma",data = top5)
  part2=run_likelihood(model_input = part1)
  part3=converge_figs(distribution = "Gamma",jags_model_run = part2)
  expect_equal(length(part3),2)
  fig1=part3[[1]]
  expect_equal(length(fig1),2)
  fig1a=fig1[[1]]
  expect_equal(names(fig1a@layers),c("geom_line","geom_line...2","geom_line...3"))
  expect_equal(fig1a@labels$x,"Iterations")
  fig2=part3[[2]]
  fig2b=fig2[[2]]
  expect_equal(fig2b@labels$x,"shape_em")
  expect_equal(fig2@meta$patches$annotation$title,"Gamma")

})
