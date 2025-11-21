#' fit_likelihoodGroup() calculates the error between fitted density and
#' observed density distributions with a hierarchical group structure
#' @description
#' This function takes the list of results from
#' [output_likelihoodGroup()] and compares
#' the predicted density distributions to observed density distributions,
#' estimating the `SSE` (sum of squared errors) and counts the number of emissions
#' observations with densities that have overlapping 95 percent CI with predicted
#' densities. Additional parameters can be supplied for use in [obs_density()].
#' Rather than independent runs defining the population distribution, hierarchical
#' dependency within groups is allowed with the
#' group-level distribution parameters drawn from the overall population distribution.
#' @export
#' @param likelihood_result Output list from [output_likelihoodGroup()]
#' @param up Optional upper limit to bound density, default is `Inf`.
#' @param low Optional lower limit to bound density, default is `0`.
#' @param bw Optional bandwidth, default is `NULL` in which case
#' `bw = sd(emissions) * n^(-2/5)`, where `n` is number of emissions. The bandwidth
#' can also be provided manually, or searched for using least squares cross-validation
#' by `bw = "cv.ls"` or likelihood cross-validation with `bw = "cv.ml"`.
#' @param kernel Kernel choice for density function, default is `gamma` defined
#' on `(0,Inf)`. Other options include:
#' `c('gaussian1', 'gaussian2', 'beta1', 'beta2', 'fb', 'fbl', 'fbu', 'rigaussian')`.
#' See [np::npuniden.boundary()] for more information on kernel options.
#' @returns A table `fit_dat` with the sum of squared error (SSE) between
#' predicted and observed probability densities, integration of predicted pdf,
#' and the count of emissions whose 95 percent CI around predicted probability
#' densities overlaps observed probability densities by group. Also includes the
#' distribution type used, and merged data sets of observed and predicted
#' densities at each emission value for group and population levels,
#' and each `xval`, named `obs_pdf_dat`, `xhat_pdf_grp` and `xhat_pdf_pop`. The
#' `obs_pdf_dat` also includes the upper and lower 95 percent and median around predicted
#' pdf. The UPL estimate from [output_likelihoodGroup()]
#' is included as well as the RNG.state for record keeping.
fit_likelihoodGroup = function(likelihood_result, up = Inf, low = 0,
                          kernel = 'gamma', bw = NULL){
  obs_pdf_temp = likelihood_result$obs_pdf
  pred_pdf_temp = likelihood_result$pred_pdf
  grp_pdf_temp = likelihood_result$group_dat
  minY = likelihood_result$minY
  maxY = likelihood_result$maxY
  if (up < maxY){
    up = maxY
  }
  if (low > minY){
    low = minY
  }
  obs_pdf_dat = tibble::tibble()
  obs_den_df= tibble::tibble()
  for (j in 1:length(unique(obs_pdf_temp$group))){
    data_sub = subset(obs_pdf_temp, obs_pdf_temp$group == levels(obs_pdf_temp$group)[j])
    obs_dens_results = obs_density(data = data_sub, emissions = 'emissions',
                                   up = up, low = low,
                                   bw = bw, kernel = kernel,
                                   xvals = pred_pdf_temp$x_hat)
    Obs_onPoint = obs_dens_results$Obs_onPoint
    obs_pdf_group = dplyr::left_join(Obs_onPoint, data_sub,
                                   by = 'emissions', multiple = 'any')
    obs_den_grp = obs_dens_results$obs_den_df
    obs_den_grp$group = rep(levels(obs_pdf_temp$group)[j], nrow(obs_den_grp))
    obs_den_df = rbind(obs_den_df, obs_den_grp)
    obs_pdf_dat = rbind(obs_pdf_dat, obs_pdf_group)
  }
  obs_pdf_dat$inCI = NA
  for (k in 1:nrow(obs_pdf_dat)){
    if ((obs_pdf_dat$ydens[k] > obs_pdf_dat$low[k])&
        (obs_pdf_dat$ydens[k] < obs_pdf_dat$up[k])){
      obs_pdf_dat$inCI[k] = 1
    } else {
      obs_pdf_dat$inCI[k] = 0
    }
  }
  xhat_pdf_grp = dplyr::full_join(grp_pdf_temp, obs_den_df, by = c('x_hat', 'group'))
  SSE = (sum((obs_pdf_dat$ydens - obs_pdf_dat$med)^2))
  pdf_integral = sfsmisc::integrate.xy(pred_pdf_temp$x_hat,
                                       pred_pdf_temp$pdf_hat)
  fit_dat = tibble::tibble(
    distr = rep(likelihood_result$distr, length(unique(obs_pdf_temp$group))),
    SSE = NA,
    good_vals = NA,
    pdf_integral = NA,
    group = NA)
  for (j in 1:length(unique(obs_pdf_temp$group))){
    data_sub = subset(obs_pdf_dat,
                      obs_pdf_dat$group == levels(obs_pdf_dat$group)[j])
    data2_sub = subset(xhat_pdf_grp,
                       xhat_pdf_grp$group == levels(obs_pdf_dat$group)[j])
    SSE = (sum((data_sub$ydens - data_sub$med)^2))
    good_vals = sum(data_sub$inCI)
    pdf_integral = sfsmisc::integrate.xy(data2_sub$x_hat,
                                         data2_sub$pdf_hat)
    fit_dat$SSE[j] = SSE
    fit_dat$good_vals[j] = good_vals
    fit_dat$pdf_integral[j] = pdf_integral
    fit_dat$group[j] = levels(obs_pdf_dat$group)[j]
  }
  fit_temp = list(fit_dat = fit_dat,
                  distr = likelihood_result$distr,
                  SSE_tot = sum(fit_dat$SSE),
                  good_vals_tot = sum(fit_dat$good_vals),
                  obs_pdf_dat = obs_pdf_dat,
                  xhat_pdf_grp = xhat_pdf_grp,
                  xhat_pdf_pop = pred_pdf_temp,
                  group_dat = likelihood_result$group_dat,
                  UPL_Bayes = likelihood_result$UPL_Bayes,
                  state = likelihood_result$state)
  return(fit_temp)
}
