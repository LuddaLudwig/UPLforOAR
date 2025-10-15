test_that("mcmc_theme() contains ggplot theme for converge_figs()", {

  set.seed(1)
  iter1=rnorm(10000,0,1)

  p1=ggplot2::ggplot()+ggplot2::geom_line(aes(x=1:10000,y=iter1))+mcmc_theme()+
    ggplot2::xlab("Iterations")+ggplot2::ylab("Parameter")+ggplot2::ggtitle("Distribution")

  p1@theme
  expect_null(p1@theme$legend.title.position)
  expect_equal(p1@theme$line@colour,"black")
  expect_null(p1@theme$legend.title@family)
  expect_equal(names(p1@layers),c("geom_line"))
  expect_equal(p1@theme$legend.position,'none')
  expect_equal(p1@theme$axis.text.y@size,12)
  expect_equal(p1@theme$axis.text.x@vjust,-2)

})
