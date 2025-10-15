test_that("pop_distr_theme() contains ggplot theme for density", {

  set.seed(1)
  emiss1=rnorm(12,mean=5,sd=1)
  emiss2=rnorm(3,mean=2,sd=0.1)
  emiss3=rnorm(24,mean=6,sd=0.2)
  emiss4=rnorm(12,mean=15,sd=4)
  emiss5=rnorm(6,mean=7,sd=1)
  emissions=c(emiss1,emiss2,emiss3,emiss4,emiss5)
  sources=c(rep("A",12),rep("B",3),rep("C",24),rep("D",12),rep("E",6))
  dat_emiss=tibble::tibble(emissions=emissions,sources=sources)
  p1=ggplot2::ggplot(data=dat_emiss)+geom_density(aes(emissions),fill='black',
                                                  bounds=c(0,Inf),alpha=0.5)+
    pop_distr_theme()+
    xlab("Emissions")+ylab("Density")+ggtitle("Population distribution");p1

  p1@theme
  expect_null(p1@theme$legend.title.position)
  expect_equal(p1@theme$line@colour,"black")
  expect_equal(p1@theme$legend.direction,"horizontal")
  expect_equal(names(p1@layers),c("geom_density"))
  expect_equal(p1@theme$legend.position,'top')
  expect_equal(p1@theme$axis.text.y@size,12)
  expect_equal(p1@theme$axis.text.x@vjust,-2)

})
