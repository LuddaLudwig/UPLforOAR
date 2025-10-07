#' Calculate UPL assuming skew-normal distributed emissions data
#' @param data Emissions data from either the best source or top performers,
#' must have a column named 'emissions'.
#' @param future_tests Integer of future runs to use in prediction, the default
#' is 3 since compliance uses 1 test average of 3 runs.
#' @param significance Level of significance from 0 to 1, the default is 0.99.
#' @returns Upper predictive limit (UPL) at significance level for the average
#' of the number of future test runs.
#' @export
Skewed_UPL=function(data,future_tests=3,significance=0.99){
  n=length(data$emissions)
  if (n<=3){
    PI99_skew=NA
    warning("data must have more than 3 observations for skew UPL method")
  } else {
    kurtosis=EnvStats::kurtosis(data$emissions,method='fisher')
    skewness=EnvStats::skewness(data$emissions,method='fisher')
    df=n-1
    var.s=sum((data$emissions-mean(data$emissions))^2)*(1/(n-1))
    tscore=stats::qt(significance,df)
    u0=1/(1+(tscore^2/(n-1)))
    b=c(0.5,0.5,0.5,0.5,1,1)
    w=b*(u0/(1-u0))
    a=c((n-1)/2,(n+1)/2,(n+3)/2,(n+5)/2,(n-1)/2,(n+1)/2)
    term=c()
    for (i in 1:6){
      c11=stats::pgamma(w[i],shape=a[i],rate=1)
      c12=gamma(a[i])
      c13=((exp(-w[i])*w[i]^a[i])/gamma(a[i]))
      c14=(a[i]-1-w[i])/(2*b[i])
      c15=(a[i]^3/2-5*a[i]^2/3+3*a[i]/2-1/3)
      c16=w[i]*(3*a[i]^2/2-11*a[i]/6+1/3)
      c17=w[i]^2*(3*a[i]/2-1/6)

      term[i]=c11/c12+c13*(c14+(1/(2*b[i])^2)*(c15-c16+c17-w[i]^3/2))
    }
    calc3=(2*n-1)*term[5]/(6*sqrt(2*n*pi))-(n-1)*term[6]/(3*sqrt(2*n*pi))
    calc4=(n-1)*term[1]/24-(n-1)*(n+2)*term[2]/(12*n)+(n+4)*(n-1)*term[3]/(24*n)
    calc5=(n-1)*(2*n+5)*term[1]/72-(n-1)*(2*n^2+5*n+8)*term[2]/(24*n)+(n-1)*(2*n^2+5*n+12)*term[3]/(24*n)-(n-1)*(2*n^2+5*n+12)*term[4]/(72*n)
    current_prob=1-(term[1]/2+skewness*calc3-kurtosis*calc4+skewness^2*calc5)
    if (abs(current_prob-0.99)<0.0001){
      PI99_skew=mean(data$emissions)+tscore*sqrt(var.s*(1/n+1/future_tests))
    } else if ((current_prob-0.99)>0){
      tstat_list=seq(from=tscore,length.out=20000,by=0.0001)
      new_prob=c()
      for (t in 1:length(tstat_list)){

        u0=1/(1+(tstat_list[t]^2/(n-1)))
        b=c(0.5,0.5,0.5,0.5,1,1)
        w=b*(u0/(1-u0))
        a=c((n-1)/2,(n+1)/2,(n+3)/2,(n+5)/2,(n-1)/2,(n+1)/2)
        term=c()

        for (i in 1:6){
          c11=stats::pgamma(w[i],shape=a[i],rate=1)
          c12=gamma(a[i])
          c13=((exp(-w[i])*w[i]^a[i])/gamma(a[i]))
          c14=(a[i]-1-w[i])/(2*b[i])
          c15=(a[i]^3/2-5*a[i]^2/3+3*a[i]/2-1/3)
          c16=w[i]*(3*a[i]^2/2-11*a[i]/6+1/3)
          c17=w[i]^2*(3*a[i]/2-1/6)

          term[i]=c11/c12+c13*(c14+(1/(2*b[i])^2)*(c15-c16+c17-w[i]^3/2))
        }
        calc3=(2*n-1)*term[5]/(6*sqrt(2*n*pi))-(n-1)*term[6]/(3*sqrt(2*n*pi))
        calc4=(n-1)*term[1]/24-(n-1)*(n+2)*term[2]/(12*n)+(n+4)*(n-1)*term[3]/(24*n)
        calc5=(n-1)*(2*n+5)*term[1]/72-(n-1)*(2*n^2+5*n+8)*term[2]/(24*n)+(n-1)*(2*n^2+5*n+12)*term[3]/(24*n)-(n-1)*(2*n^2+5*n+12)*term[4]/(72*n)

        new_prob[t]=1-(term[1]/2+skewness*calc3-kurtosis*calc4+skewness^2*calc5)
      }

      new_tscore=tstat_list[(abs(new_prob-0.99)<0.0001)][1]
      PI99_skew=mean(data$emissions)+new_tscore*sqrt(var.s*(1/n+1/future_tests))
    } else if ((current_prob-0.99)<0){
      tstat_list=seq(from=tscore,length.out=20000,by=0.0001)
      new_prob=c()
      for (t in 1:length(tstat_list)){

        u0=1/(1+(tstat_list[t]^2/(n-1)))
        b=c(0.5,0.5,0.5,0.5,1,1)
        w=b*(u0/(1-u0))
        a=c((n-1)/2,(n+1)/2,(n+3)/2,(n+5)/2,(n-1)/2,(n+1)/2)
        term=c()

        for (i in 1:6){
          c11=stats::pgamma(w[i],shape=a[i],rate=1)
          c12=gamma(a[i])
          c13=((exp(-w[i])*w[i]^a[i])/gamma(a[i]))
          c14=(a[i]-1-w[i])/(2*b[i])
          c15=(a[i]^3/2-5*a[i]^2/3+3*a[i]/2-1/3)
          c16=w[i]*(3*a[i]^2/2-11*a[i]/6+1/3)
          c17=w[i]^2*(3*a[i]/2-1/6)

          term[i]=c11/c12+c13*(c14+(1/(2*b[i])^2)*(c15-c16+c17-w[i]^3/2))
        }
        calc3=(2*n-1)*term[5]/(6*sqrt(2*n*pi))-(n-1)*term[6]/(3*sqrt(2*n*pi))
        calc4=(n-1)*term[1]/24-(n-1)*(n+2)*term[2]/(12*n)+(n+4)*(n-1)*term[3]/(24*n)
        calc5=(n-1)*(2*n+5)*term[1]/72-(n-1)*(2*n^2+5*n+8)*term[2]/(24*n)+(n-1)*(2*n^2+5*n+12)*term[3]/(24*n)-(n-1)*(2*n^2+5*n+12)*term[4]/(72*n)

        new_prob[t]=1-(term[1]/2+skewness*calc3-kurtosis*calc4+skewness^2*calc5)
      }
      new_tscore=tstat_list[(abs(new_prob-0.99)<0.0001)][1]
      PI99_skew=mean(data$emissions)+new_tscore*sqrt(var.s*(1/n+1/future_tests))
    }
  }
  return(PI99_skew)
}
