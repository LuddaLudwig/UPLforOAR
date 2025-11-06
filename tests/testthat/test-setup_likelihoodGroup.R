test_that("setup_likelihoodGroup() calls JAGS model scripts with initial values and par_list", {
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
  dat_topmeans=top5%>%dplyr::group_by(sources)%>%dplyr::summarize(means=mean(emissions),
                                                                  counts=dplyr::n())
  dat_topmeans$sources=as.factor(dat_topmeans$sources)
  dat_topmeans$sources=forcats::fct_reorder(dat_topmeans$sources,
                                            dat_topmeans$means,.desc = FALSE)
  top5$sources=factor(top5$sources,levels=levels(dat_topmeans$sources))
  top5=dplyr::arrange(top5,sources)
  ln_emiss=log(top5$emissions)
  mu = mean(top5$emissions)
  sigma = stats::sd(top5$emissions)
  shape = mu^2 / sigma^2
  rate = mu / sigma^2

  JAGS_model_stuff=setup_likelihoodGroup(data=top5,distribution='Gamma')

  expect_equal(JAGS_model_stuff$par_list,
               c('emission_hat', 'pdf_obs', 'pdf_hat', 'pop_rate_mu',
                 'pop_shape_mu', 'pop_rate_sd', 'pop_shape_sd', 'group_rate',
                 'group_shape', 'group'))
  expect_equal(JAGS_model_stuff$dat_inits, list(
    list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 5,
         'pop_rate_mu' = rate, 'pop_shape_mu' = shape,
         'pop_rate_sd' = 0.1 * rate, 'pop_shape_sd' = 0.1 * shape),
    list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 12,
         'pop_rate_mu' = 1.5 * rate, 'pop_shape_mu' = 0.5 * shape,
         'pop_rate_sd' = 0.5 * rate, 'pop_shape_sd' = 0.5 * shape),
    list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = 151,
         'pop_rate_mu' = 0.5 * rate, 'pop_shape_mu' = 1.5 * shape,
         'pop_rate_sd' = 0.3 * rate, 'pop_shape_sd' = 0.3 * shape)))
  expect_equal(JAGS_model_stuff$distribution,'Gamma')
  expect_equal(length(JAGS_model_stuff),6)
  expect_equal(JAGS_model_stuff$data,top5)
  expect_equal(JAGS_model_stuff$manual_prior,FALSE)
  readjags=runjags::read.jagsfile(test_path('test_JAGS',
                                            'test-EmissionGroup_gamma_JAGS.R'))
  expect_equal(JAGS_model_stuff$model_code$model,readjags$model)
})

