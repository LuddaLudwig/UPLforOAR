test_that("distribution_type() determines best distribution", {
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
  test_result=distribution_type(dataset=top5)
  expect_equal(test_result,'Lognormal')
})
