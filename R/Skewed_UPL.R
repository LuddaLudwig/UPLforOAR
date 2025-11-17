#' Calculate UPL assuming skew-normal distributed emissions data
#' @param data Data from either the best source or top performers,
#' must have a column with numeric `emissions`.
#' @param future_runs Integer of future runs to use in prediction, the default
#' is `3` since compliance uses 1 test average of 3 runs.
#' @param significance Level of significance from 0 to 1, the default is `0.99`.
#' @param emissions variable name or column number corresponding to the
#' emissions used for selecting top performing sources.
#' @returns Upper predictive limit (UPL) at significance level for the average
#' of the number of future test runs.
#' @export
Skewed_UPL = function(data, emissions, future_runs = 3, significance = 0.99){
  data_temp = tibble::tibble(emissions = data[[emissions]])
  n = length(data_temp$emissions)
  if (n < 3){
    stop("Need at least 3 observations for UPL calculation")
  }
  if (n > 341){
    warning("slower calculations due to requirement for high precision floating point")
  }
  future_runs = as.integer(future_runs)
  emission_mean = mean(data_temp$emissions, na.rm = T)
  sigma = stats::sd(data_temp$emissions, na.rm = T)
  if (sigma == 0){
    stop("Cannot perform UPL calculation on data with 0 variance")
  }
  if (!is.integer(future_runs)){
    stop("future_runs must be a positive integer")
  }
  if (future_runs < 1){
    stop("future_runs must be a positive integer")
  }
  if (significance >= 1){
    stop("significance must be greater then 0 and less than 1")
  }
  if (significance <= 0){
    stop("significance must be greater then 0 and less than 1")
  }
  if (n <= 3){
    Skewed_UPL = NA
    warning("data must have more than 3 observations for skew UPL method")
  } else {
    K = (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3)) * sum(((data_temp$emissions - emission_mean) / sigma)^4) - (3 * (n - 1)^2) / ((n - 2) * (n - 3))
    S = n / ((n - 1) * (n - 2)) * sum(((data_temp$emissions - emission_mean) / sigma)^3)
    df = n - 1
    var.s = sum((data_temp$emissions - mean(data_temp$emissions))^2) * (1 / (n - 1))
    tscore=stats::qt(significance, df)
    u0 = 1 / (1 + (tscore^2 / (n - 1)))
    b = c(0.5, 0.5, 0.5, 0.5, 1, 1)
    w = b * (u0 / (1 - u0))
    a = c((n - 1) / 2, (n + 1) / 2, (n + 3) / 2, (n + 5) / 2, (n - 1) / 2, (n + 1) / 2)
    I_term = c()
    for (i in 1:6){
      c1 = stats::pgamma(w[i], shape = a[i], rate = 1)
      c4 = (a[i] - 1 - w[i]) / (2 * b[i])
      c5 = (a[i]^3 / 2 - 5 * a[i]^2 / 3 + 3 * a[i] / 2 - 1 / 3)
      c6 = w[i] * (3 * a[i]^2 / 2 - 11 * a[i] / 6 + 1 / 3)
      c7 = w[i]^2 * (3 * a[i] / 2 - 1 / 6)
      if (n > 341){
        c2 = Rmpfr::igamma(a[i], 0)
        c3 = ((exp(-w[i]) * w[i]^Rmpfr::mpfr(a[i], 128)) / Rmpfr::igamma(a[i], 0))
        I_term[i] =  Rmpfr::asNumeric(c1 / c2 + c3 * (c4 + (1 / (2 * b[i])^2) * (c5 - c6 + c7 - w[i]^3 / 2)))
      } else {
        c2 = gamma(a[i])
        c3 = ((exp(-w[i]) * w[i]^a[i]) / gamma(a[i]))
        I_term[i] =  (c1 / c2 + c3 * (c4 + (1 / (2 * b[i])^2) * (c5 - c6 + c7 - w[i]^3 / 2)))
      }    }
    coeff1 = (2 * n - 1) * I_term[[5]] / (6 * sqrt(2 * n * pi)) - (n - 1) * I_term[[6]] / (3 * sqrt(2 * n * pi))
    coeff2 = (n - 1) * I_term[1] / 24 - (n - 1) * (n + 2) * I_term[2] / (12 * n) + (n + 4) * (n - 1) * I_term[3] / (24 * n)
    coeff3 = (n - 1) * (2 * n + 5) * I_term[1] / 72 - (n - 1) * (2 * n^2 + 5 * n + 8) * I_term[2] / (24 * n) + (n - 1) * (2 * n^2 + 5 * n + 12) * I_term[3] / (24 * n) - (n - 1) * (2 * n^2 + 5 * n + 12) * I_term[4] / (72 * n)
    current_prob = 1 - (I_term[1] / 2 + S * coeff1 - K * coeff2 + S^2 * coeff3)
    if (abs(current_prob - significance) < 0.0001){
      Skewed_UPL = emission_mean + tscore * sqrt(var.s * (1 / n + 1 / future_runs))
    } else {
      tstat_list = seq(from = (tscore - 1), to = (tscore + 1), by = 0.0001)
      new_prob = c()
      for (t in 1:length(tstat_list)){
        u0 = 1 / (1 + (tstat_list[t]^2 / (n - 1)))
        b = c(0.5, 0.5, 0.5, 0.5, 1, 1)
        w = b * (u0 / (1 - u0))
        a = c((n - 1) / 2, (n + 1) / 2, (n + 3) / 2, (n + 5) / 2, (n - 1) / 2, (n + 1) / 2)
        I_term = c()
        for (i in 1:6){
          c1 = stats::pgamma(w[i], shape = a[i], rate = 1)
          c4 = (a[i] - 1 - w[i]) / (2 * b[i])
          c5 = (a[i]^3 / 2 - 5 * a[i]^2 / 3 + 3 * a[i] / 2 - 1/3)
          c6 = w[i] * (3 * a[i]^2 / 2 - 11 * a[i] / 6 + 1 / 3)
          c7 = w[i]^2 * (3 * a[i] / 2 - 1 / 6)
          if (n > 341){
            c2 = Rmpfr::igamma(a[i], 0)
            c3 = ((exp(-w[i]) * w[i]^Rmpfr::mpfr(a[i], 128)) / Rmpfr::igamma(a[i], 0))
            I_term[i] =  Rmpfr::asNumeric(c1 / c2 + c3 * (c4 + (1 / (2 * b[i])^2) * (c5 - c6 + c7 - w[i]^3 / 2)))
          } else {
            c2 = gamma(a[i])
            c3 = ((exp(-w[i]) * w[i]^a[i]) / gamma(a[i]))
            I_term[i] =  (c1 / c2 + c3 * (c4 + (1 / (2 * b[i])^2) * (c5 - c6 + c7 - w[i]^3 / 2)))
          }
        }
        coeff1 = (2 * n - 1) * I_term[5] / (6 * sqrt(2 * n * pi)) - (n - 1) * I_term[6] / (3 * sqrt(2 * n * pi))
        coeff2 = (n - 1) * I_term[1] / 24 - (n - 1) * (n + 2) * I_term[2] / (12 * n) + (n + 4) * (n - 1) * I_term[3] / (24 * n)
        coeff3 = (n - 1) * (2 * n + 5) * I_term[1] / 72 - (n - 1) * (2 * n^2 + 5 * n + 8) * I_term[2] / (24 * n) + (n - 1) * (2 * n^2 + 5 * n + 12) * I_term[3] / (24 * n) - (n - 1) * (2 * n^2 + 5 * n + 12) * I_term[4] / (72 * n)
        new_prob[t] = 1 - (I_term[1] / 2 + S * coeff1 - K * coeff2 + S^2 * coeff3)
      }
      good_tscore = which(abs(new_prob - significance) < 0.0001)
      if (length(good_tscore) > 0){
        new_tscore = tstat_list[good_tscore[which.min(abs(good_tscore - which(tstat_list == tscore)))]]
        Skewed_UPL = mean(data_temp$emissions) + new_tscore * sqrt(var.s * (1 / n + 1 / future_runs))
      } else if (between(significance, min(new_prob), max(new_prob))){
        closest_t = tstat_list[which.min(abs(new_prob - significance))]
        reset_t1 = tstat_list[which.min(abs(new_prob - significance)) - 1]
        reset_t2 = tstat_list[which.min(abs(new_prob - significance)) + 1]
        tstat_list = seq(from = reset_t1, to = reset_t2, length.out = 5000)
        new_prob = c()
        for (t in 1:length(tstat_list)){
          u0 = 1 / (1 + (tstat_list[t]^2 / (n - 1)))
          b = c(0.5, 0.5, 0.5, 0.5, 1, 1)
          w = b * (u0 / (1 - u0))
          a = c((n - 1) / 2, (n + 1) / 2, (n + 3) / 2, (n + 5) / 2, (n - 1) / 2, (n + 1) / 2)
          I_term = c()
          for (i in 1:6){
            c1 = stats::pgamma(w[i], shape = a[i], rate = 1)
            c4 = (a[i] - 1 - w[i]) / (2 * b[i])
            c5 = (a[i]^3 / 2 - 5 * a[i]^2 / 3 + 3 * a[i] / 2 - 1/3)
            c6 = w[i] * (3 * a[i]^2 / 2 - 11 * a[i] / 6 + 1 / 3)
            c7 = w[i]^2 * (3 * a[i] / 2 - 1 / 6)
            if (n > 341){
              c2 = Rmpfr::igamma(a[i], 0)
              c3 = ((exp(-w[i]) * w[i]^Rmpfr::mpfr(a[i], 128)) / Rmpfr::igamma(a[i], 0))
              I_term[i] =  Rmpfr::asNumeric(c1 / c2 + c3 * (c4 + (1 / (2 * b[i])^2) * (c5 - c6 + c7 - w[i]^3 / 2)))
            } else {
              c2 = gamma(a[i])
              c3 = ((exp(-w[i]) * w[i]^a[i]) / gamma(a[i]))
              I_term[i] =  (c1 / c2 + c3 * (c4 + (1 / (2 * b[i])^2) * (c5 - c6 + c7 - w[i]^3 / 2)))
            }
          }
          coeff1 = (2 * n - 1) * I_term[5] / (6 * sqrt(2 * n * pi)) - (n - 1) * I_term[6] / (3 * sqrt(2 * n * pi))
          coeff2 = (n - 1) * I_term[1] / 24 - (n - 1) * (n + 2) * I_term[2] / (12 * n) + (n + 4) * (n - 1) * I_term[3] / (24 * n)
          coeff3 = (n - 1) * (2 * n + 5) * I_term[1] / 72 - (n - 1) * (2 * n^2 + 5 * n + 8) * I_term[2] / (24 * n) + (n - 1) * (2 * n^2 + 5 * n + 12) * I_term[3] / (24 * n) - (n - 1) * (2 * n^2 + 5 * n + 12) * I_term[4] / (72 * n)
          new_prob[t] = 1 - (I_term[1] / 2 + S * coeff1 - K * coeff2 + S^2 * coeff3)
        }
        good_tscore = which(abs(new_prob - significance) < 0.0001)
        new_tscore = tstat_list[good_tscore[which.min(abs(good_tscore -  which.min(abs(tstat_list - closest_t))))]]
      } else {
        tstat_list = seq(from = (tscore - 3), to = (tscore + 3), by = 0.0001)
        new_prob = c()
        for (t in 1:length(tstat_list)){
          u0 = 1 / (1 + (tstat_list[t]^2 / (n - 1)))
          b = c(0.5, 0.5, 0.5, 0.5, 1, 1)
          w = b * (u0 / (1 - u0))
          a = c((n - 1) / 2, (n + 1) / 2, (n + 3) / 2, (n + 5) / 2, (n - 1) / 2, (n + 1) / 2)
          I_term = c()
          for (i in 1:6){
            c1 = stats::pgamma(w[i], shape = a[i], rate = 1)
            c4 = (a[i] - 1 - w[i]) / (2 * b[i])
            c5 = (a[i]^3 / 2 - 5 * a[i]^2 / 3 + 3 * a[i] / 2 - 1/3)
            c6 = w[i] * (3 * a[i]^2 / 2 - 11 * a[i] / 6 + 1 / 3)
            c7 = w[i]^2 * (3 * a[i] / 2 - 1 / 6)
            if (n > 341){
              c2 = Rmpfr::igamma(a[i], 0)
              c3 = ((exp(-w[i]) * w[i]^Rmpfr::mpfr(a[i], 128)) / Rmpfr::igamma(a[i], 0))
              I_term[i] =  Rmpfr::asNumeric(c1 / c2 + c3 * (c4 + (1 / (2 * b[i])^2) * (c5 - c6 + c7 - w[i]^3 / 2)))
            } else {
              c2 = gamma(a[i])
              c3 = ((exp(-w[i]) * w[i]^a[i]) / gamma(a[i]))
              I_term[i] =  (c1 / c2 + c3 * (c4 + (1 / (2 * b[i])^2) * (c5 - c6 + c7 - w[i]^3 / 2)))
            }
          }
          coeff1 = (2 * n - 1) * I_term[5] / (6 * sqrt(2 * n * pi)) - (n - 1) * I_term[6] / (3 * sqrt(2 * n * pi))
          coeff2 = (n - 1) * I_term[1] / 24 - (n - 1) * (n + 2) * I_term[2] / (12 * n) + (n + 4) * (n - 1) * I_term[3] / (24 * n)
          coeff3 = (n - 1) * (2 * n + 5) * I_term[1] / 72 - (n - 1) * (2 * n^2 + 5 * n + 8) * I_term[2] / (24 * n) + (n - 1) * (2 * n^2 + 5 * n + 12) * I_term[3] / (24 * n) - (n - 1) * (2 * n^2 + 5 * n + 12) * I_term[4] / (72 * n)
          new_prob[t] = 1 - (I_term[1] / 2 + S * coeff1 - K * coeff2 + S^2 * coeff3)
        }
        good_tscore = which(abs(new_prob - significance) < 0.0001)
        if (length(good_tscore) > 0){
          new_tscore = tstat_list[good_tscore[which.min(abs(good_tscore - which(tstat_list == tscore)))]]
          Skewed_UPL = mean(data_temp$emissions) + new_tscore * sqrt(var.s * (1 / n + 1 / future_runs))
        } else if (between(significance, min(new_prob), max(new_prob))){
          closest_t = tstat_list[which.min(abs(new_prob - significance))]
          reset_t1 = tstat_list[which.min(abs(new_prob - significance)) - 1]
          reset_t2 = tstat_list[which.min(abs(new_prob - significance)) + 1]
          tstat_list = seq(from = reset_t1, to = reset_t2, length.out = 5000)
          new_prob = c()
          for (t in 1:length(tstat_list)){
            u0 = 1 / (1 + (tstat_list[t]^2 / (n - 1)))
            b = c(0.5, 0.5, 0.5, 0.5, 1, 1)
            w = b * (u0 / (1 - u0))
            a = c((n - 1) / 2, (n + 1) / 2, (n + 3) / 2, (n + 5) / 2, (n - 1) / 2, (n + 1) / 2)
            I_term = c()
            for (i in 1:6){
              c1 = stats::pgamma(w[i], shape = a[i], rate = 1)
              c4 = (a[i] - 1 - w[i]) / (2 * b[i])
              c5 = (a[i]^3 / 2 - 5 * a[i]^2 / 3 + 3 * a[i] / 2 - 1/3)
              c6 = w[i] * (3 * a[i]^2 / 2 - 11 * a[i] / 6 + 1 / 3)
              c7 = w[i]^2 * (3 * a[i] / 2 - 1 / 6)
              if (n > 341){
                c2 = Rmpfr::igamma(a[i], 0)
                c3 = ((exp(-w[i]) * w[i]^Rmpfr::mpfr(a[i], 128)) / Rmpfr::igamma(a[i], 0))
                I_term[i] =  Rmpfr::asNumeric(c1 / c2 + c3 * (c4 + (1 / (2 * b[i])^2) * (c5 - c6 + c7 - w[i]^3 / 2)))
              } else {
                c2 = gamma(a[i])
                c3 = ((exp(-w[i]) * w[i]^a[i]) / gamma(a[i]))
                I_term[i] =  (c1 / c2 + c3 * (c4 + (1 / (2 * b[i])^2) * (c5 - c6 + c7 - w[i]^3 / 2)))
              }
            }
            coeff1 = (2 * n - 1) * I_term[5] / (6 * sqrt(2 * n * pi)) - (n - 1) * I_term[6] / (3 * sqrt(2 * n * pi))
            coeff2 = (n - 1) * I_term[1] / 24 - (n - 1) * (n + 2) * I_term[2] / (12 * n) + (n + 4) * (n - 1) * I_term[3] / (24 * n)
            coeff3 = (n - 1) * (2 * n + 5) * I_term[1] / 72 - (n - 1) * (2 * n^2 + 5 * n + 8) * I_term[2] / (24 * n) + (n - 1) * (2 * n^2 + 5 * n + 12) * I_term[3] / (24 * n) - (n - 1) * (2 * n^2 + 5 * n + 12) * I_term[4] / (72 * n)
            new_prob[t] = 1 - (I_term[1] / 2 + S * coeff1 - K * coeff2 + S^2 * coeff3)
          }
          good_tscore = which(abs(new_prob - significance) < 0.0001)
          new_tscore = tstat_list[good_tscore[which.min(abs(good_tscore -  which.min(abs(tstat_list - closest_t))))]]
        }
      }
      Skewed_UPL = emission_mean + new_tscore * sqrt(var.s * (1 / n + 1 / future_runs))
    }
  }
  return(Skewed_UPL)
}
