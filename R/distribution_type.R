#' Determines the type of distribution from skewness and kurtosis ratios
#' @param data Emissions data from either the best source or top performers,
#' must have a column named `emissions`
#' @returns String with either `'Normal'`, `'Lognormal'`, or `'Skewed'`
#' @references  On measuring skewness and kurtosis" Dragan Doric, et al.
#' Springer Science + Buisness Media B. V. 2007. September 20, 2007
#' @description
#' This uses ratios of ratios of kurtosis and skewness to evaluate how Normal or
#' non-Normal the emissions data are. For small data sets where `n = 3`, kurtosis
#' and the standard error of kurtosis are estimated differently and the only
#' distribution outcomes are `'Normal'` or `'Lognormal'`. Note that there are
#' multiple methods for evaluating skewness and kurtosis. The Fisher method is
#' what is used in the old Excel workbook and is a good selection for small
#' `n < 300` samples from an unknown population.
#'
#' @export
distribution_type = function(data){
  data$ln_emiss = log(data$emissions)
  data$ln_emiss = replace(data$ln_emiss, !is.finite(data$ln_emiss), NA)
  sigma = stats::sd(data$emissions)
  mean_ln = mean(data$ln_emiss, na.rm = TRUE)
  sigma_ln = stats::sd(data$ln_emiss, na.rm = TRUE)
  n = length(data$emissions)
  emission_mean = mean(data$emissions)
  S = n / ((n - 1) * (n - 2)) * sum(((data$emissions - emission_mean) / sigma)^3)
  S_ln = n / ((n - 1) * (n - 2)) * sum(((data$ln_emiss - mean_ln) / sigma_ln)^3)
  SES = sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)))
  S_SES = abs(S / SES)
  S_SES_ln = abs(S_ln / SES)

  if (n == 3){
    K = sum((data$emissions - emission_mean)^4) / ((n - 1) * (sigma)^4) - 3
    K_ln = sum((data$ln_emiss - mean_ln)^4) / ((n - 1) * (sigma_ln)^4) - 3
    SEK = sqrt(24 / n)
    if(S_SES < S_SES_ln){
      distr_choice = "Normal"
    } else {
      distr_choice = "Lognormal"
    }
  } else if (n > 3){
    K = (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3)) * sum(((data$emissions - emission_mean) / sigma)^4) - (3 * (n - 1)^2) / ((n - 2) * (n - 3))
    K_ln = (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3)) * sum(((data$ln_emiss - mean_ln) / sigma_ln)^4) - (3 * (n - 1)^2) / ((n - 2) * (n - 3))
    SEK = sqrt(24 * n * (n^2 - 1) / ((n - 2) * (n + 3) * (n - 3) * (n + 5)))
    S_SEK = abs(K / SEK)
    S_SEK_ln = abs(K_ln / SEK)
    norm_zscore=stats::qnorm(0.975)
    if (S_SES > norm_zscore){
      raw_distr1 = 'Non-normal'
    } else {
      raw_distr1 = 'Normal'
    }
    if (S_SEK > norm_zscore){
      raw_distr2 = 'Non-normal'
    } else {
      raw_distr2 = 'Normal'
    }
    if (S_SES_ln > norm_zscore){
      ln_distr1 = 'Non-normal'
    } else {
      ln_distr1 = 'Normal'
    }
    if (S_SEK_ln > norm_zscore){
      ln_distr2 = 'Non-normal'
    } else {
      ln_distr2 = 'Normal'
    }
    if ((raw_distr1 == "Normal") & (raw_distr2 == "Normal")){
      raw_distr = "Normal"
    } else {
      raw_distr = 'Non-normal'
    }
    if ((ln_distr1 == "Normal") & (ln_distr2 == "Normal")){
      ln_distr = "Normal"
    } else {
      ln_distr = 'Non-normal'
    }
    if ((ln_distr == "Normal") & (raw_distr == "Normal")){
      if (S_SES < S_SES_ln){
        distr_choice = "Normal"
      } else {
        distr_choice = "Lognormal"
    }
    } else if (raw_distr == "Normal"){
      distr_choice = "Normal"
    } else if (ln_distr == "Normal"){
      distr_choice = "Lognormal"
    } else {
      distr_choice = "Skewed"
    }
  }
  return(distr_choice)
}
