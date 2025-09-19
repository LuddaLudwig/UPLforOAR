test_that("MACT_EG() selects top sources", {
  top5=tibble(emissions=c(1,2,1.5,
                          1.2,3,2.2,
                          0.2,0.4,0.7,
                          0.1,0.2,0.3,1.1,1.2,1.3,
                          0.1,0.1,0.15,0.16,0.19,1.8),
              sources=c(rep('A',3),
                        rep('B',3),
                        rep('C',3),
                        rep('D',6),
                        rep('E',6)))
  dat_topmeans=top5%>%group_by(sources)%>%summarize(means=mean(emissions),
                                                       counts=n())
  dat_topmeans$sources=as.factor(dat_topmeans$sources)
  dat_topmeans$sources=fct_reorder(dat_topmeans$sources,
                                   dat_topmeans$means,.desc = FALSE)
  top5$sources=factor(top5$sources,levels=levels(dat_topmeans$sources))
  top5=arrange(top5,sources)
  others=tibble(emissions=c(8,5,6,7,10,11,1,3,1.1,0.2,11,5,
                            15,4,5.7),
                sources=c(rep('F',12),rep('G',3)))
  dat_test=rbind(top5,others)
  expect_equal(MACT_EG(dataset=dat_test),top5)
})
