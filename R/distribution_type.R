#' Determines the type of distribution from skewness and kurtosis ratios
#' @param data Emissions data from either the best source or top performers,
#' must have a column named 'emissions'
#' @returns String with either 'Normal', 'Lognormal', or 'Skewed'
#' @references  On measuring skewness and kurtosis" Dragan Doric, et al.
#' Springer Science + Buisness Media B. V. 2007. September 20, 2007
#' @export
distribution_type=function(data){
  # note that there are multiple methods for evaluating skewness and kurtosis
  # the fisher method in the EnvStats package is the same as what is used in Excel
  # and is a good selection for small (<300) samples from an unknown population
  data$ln_emiss=log(data$emissions)
  data$ln_emiss=replace(data$ln_emiss,
                                 !is.finite(data$ln_emiss),NA)

  moment3=EnvStats::skewness(data$emissions,method='fisher')
  moment3_ln=EnvStats::skewness(data$ln_emiss,method='fisher')
  std.s=stats::sd(data$emissions)
  mean_log=mean(data$ln_emiss,na.rm=TRUE)
  sd_log=stats::sd(data$ln_emiss,na.rm=TRUE)
  n=length(data$emissions)
  emission_mean=mean(data$emissions)

  if (n==3){
    moment4=sum((data$emissions-emission_mean)^4)/((n-1)*(std.s)^4)-3
    moment4_ln=sum((data$ln_emiss-mean_log)^4)/((n-1)*(sd_log)^4)-3
    SE_kurtosis=sqrt(24/n)
    SE_skew=sqrt((6*n*(n-1))/((n-2)*(n+1)*(n+3)))
    S_SE_skew=abs(moment3/SE_skew)
    Sln_SE_skew=abs(moment3_ln/SE_skew)
    if(S_SE_skew<Sln_SE_skew){
      distr_choice="Normal"
    } else {
      distr_choice="Lognormal"
    }

  } else if (n>3){
    moment4=EnvStats::kurtosis(data$emissions,method='fisher')
    moment4_ln=EnvStats::kurtosis(data$ln_emiss,method='fisher')
    SE_kurtosis=sqrt(24*n*(n^2-1)/((n-2)*(n+3)*(n-3)*(n+5)))

    SE_skew=sqrt((6*n*(n-1))/((n-2)*(n+1)*(n+3)))
    S_SE_skew=abs(moment3/SE_skew)
    Sln_SE_skew=abs(moment3_ln/SE_skew)
    S_SE_kurt=abs(moment4/SE_kurtosis)
    Sln_SE_kurt=abs(moment4_ln/SE_kurtosis)

    norm_zscore=stats::qnorm(0.975)

    if (S_SE_skew>norm_zscore){
      raw_distr1='Non-normal'
    } else {
      raw_distr1='Normal'
    }
    if (S_SE_kurt>norm_zscore){
      raw_distr2='Non-normal'
    } else {
      raw_distr2='Normal'
    }

    if (Sln_SE_skew>norm_zscore){
      ln_distr1='Non-normal'
    } else {
      ln_distr1='Normal'
    }
    if (Sln_SE_kurt>norm_zscore){
      ln_distr2='Non-normal'
    } else {
      ln_distr2='Normal'
    }

    if ((raw_distr1=="Normal")&(raw_distr2=="Normal")){
      raw_distr="Normal"
    } else {
      raw_distr='Non-normal'
    }
    if ((ln_distr1=="Normal")&(ln_distr2=="Normal")){
      ln_distr="Normal"
    } else {
      ln_distr='Non-normal'
    }

    if ((ln_distr=="Normal")&(raw_distr=="Normal")){
      if (S_SE_skew<Sln_SE_skew){
        distr_choice="Normal"
      } else {
        distr_choice="Lognormal"
    }
    } else if (raw_distr=="Normal"){
      distr_choice="Normal"
    } else if (ln_distr=="Normal"){
      distr_choice="Lognormal"
    } else {
      distr_choice="Skewed"
    }
  }
  distr_choice
}
