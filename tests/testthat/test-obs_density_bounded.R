test_that("obs_density_bounded() calculate densities for emissions observations", {
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
  xhat=seq(0,3*max(dat_top_exist$emissions),length.out=1024)
  test_result=obs_density_bounded(dataset=top5,up=1.1*max(top5$emissions),low=0,xvals=xhat)
  compare1 <- read_csv(test_path("test_obs_densities", "test-Obs_onPoint.csv"),show_col_types = FALSE)
  compare2 <- read_csv(test_path("test_obs_densities", "test-obs_den_df.csv"),show_col_types = FALSE)

  expect_equal(test_result$Obs_onPoint,compare1)
  expect_equal(test_result$obs_den_df,compare2)
})

