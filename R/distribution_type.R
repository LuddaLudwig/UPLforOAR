#' Determines the type of distribution from skewness and kurtosis ratios
#' @param data Emissions data from either the best source or top performers,
#' must have a column with numeric `emissions`.
#' @param emissions variable name or column number corresponding to the
#' emissions used for selecting top performing sources.
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
#' @export
distribution_type = function(data, emissions){
  data_temp = tibble::tibble(emissions = data[[emissions]])
  data_temp$ln_emiss = log(data_temp$emissions)
  data_temp$ln_emiss = replace(data_temp$ln_emiss,
                               !is.finite(data_temp$ln_emiss), NA)
  sigma = stats::sd(data_temp$emissions)
  mean_ln = mean(data_temp$ln_emiss, na.rm = TRUE)
  sigma_ln = stats::sd(data_temp$ln_emiss, na.rm = TRUE)
  if ((sigma == 0) | (sigma_ln == 0)){
    stop("Cannot perform UPL calculation on data with 0 variance")
  }
  n = length(data_temp$emissions)
  if (n < 3){
    stop("Need at least 3 observations for UPL calculation")
  }
  emission_mean = mean(data_temp$emissions)
  S = n / ((n - 1) * (n - 2)) * sum(((data_temp$emissions - emission_mean) / sigma)^3)
  S_ln = n / ((n - 1) * (n - 2)) * sum(((data_temp$ln_emiss - mean_ln) / sigma_ln)^3, na.rm = T)
  SES = sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)))
  S_SES = abs(S / SES)
  S_SES_ln = abs(S_ln / SES)

  if (n == 3){
    K = sum((data_temp$emissions - emission_mean)^4) / ((n - 1) * (sigma)^4) - 3
    K_ln = sum((data_temp$ln_emiss - mean_ln)^4, na.rm = T) / ((n - 1) * (sigma_ln)^4) - 3
    SEK = sqrt(24 / n)
    if(S_SES < S_SES_ln){
      distr_choice = "Normal"
    } else {
      distr_choice = "Lognormal"
    }
  } else if (n > 3){
    K = (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3)) * sum(((data_temp$emissions - emission_mean) / sigma)^4) - (3 * (n - 1)^2) / ((n - 2) * (n - 3))
    K_ln = (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3)) * sum(((data_temp$ln_emiss - mean_ln) / sigma_ln)^4, na.rm = T) - (3 * (n - 1)^2) / ((n - 2) * (n - 3))
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
