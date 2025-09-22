test_that("obs_density() calculate densities for emissions observations", {
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
  xhat=seq(0,3*max(top5$emissions),length.out=1024)
  test_result=obs_density(dataset=top5,low=0,xvals=xhat)
  # ggplot(data=test_result$Obs_onPoint)+
  #   geom_line(data=test_result$obs_den_df,aes(y=y,x=(x),color='red'),size=0.75)+
  #   geom_area(data=test_result$obs_den_df,aes(y=y,x=(x),fill='red'),alpha=0.25)+
  #   geom_point(aes(y=ydens,x=(emissions)),
  #              size=3,alpha=0.5,shape=19,color='black')+
  #   geom_density(data=top5,aes(x=emissions),color='blue',bounds=c(0,Inf),
  #                trim=FALSE)+
  #   ylab("Density")+xlab("emissions")+
  #   ggtitle("Overall observed population")+
  #   scale_x_continuous(expand=expansion(mult=c(0,0.05)))+
  #   scale_y_continuous(expand=expansion(mult=c(0,0.05)))+
  #   coord_cartesian(clip='off')+
  #   geom_rug(sides='b',aes(x=(emissions)),data=top5,
  #            alpha=0.5,outside=TRUE,color='black')
  # write_csv(test_result$Obs_onPoint,"test-Obs_onPoint.csv")
  # write_csv(test_result$obs_den_df,"test-obs_den_df.csv")

  compare1 <- read_csv(test_path("test_obs_densities", "test-Obs_onPoint.csv"),show_col_types = FALSE)
  compare2 <- read_csv(test_path("test_obs_densities", "test-obs_den_df.csv"),show_col_types = FALSE)

  expect_equal(test_result$Obs_onPoint,compare1)
  expect_equal(test_result$obs_den_df,compare2)
})
