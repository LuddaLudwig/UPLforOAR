test_that("setup_likelihood() calls JAGS model scripts with initial values and par_list", {
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
  ln_emiss=log(top5$emissions)
  JAGS_model_stuff=setup_likelihood(data=top5,distribution='Lognormal')

  expect_equal(JAGS_model_stuff$par_list,c('emission_hat','pdf_obs','pdf_hat',
                                           'u_ln','sd_ln'))
  expect_equal(JAGS_model_stuff$dat_inits,list(
    list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 5,
         'u_ln'=mean(ln_emiss,na.rm=TRUE),
         'sd_ln'=stats::sd(ln_emiss,na.rm=TRUE)),
    list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 12,
         'u_ln'=1.5*mean(ln_emiss,na.rm=TRUE),
         'sd_ln'=0.5*stats::sd(ln_emiss,na.rm=TRUE)),
    list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 151,
         'u_ln'=0.5*mean(ln_emiss,na.rm=TRUE),
         'sd_ln'=1.5*stats::sd(ln_emiss,na.rm=TRUE))))
  expect_equal(JAGS_model_stuff$distribution,'Lognormal')
  readjags=runjags::read.jagsfile(test_path('test_JAGS','test-Emission_lnorm_JAGS.R'))
  expect_equal(JAGS_model_stuff$model_code$model,readjags$model)
})

