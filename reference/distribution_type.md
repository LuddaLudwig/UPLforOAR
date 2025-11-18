# Determines the type of distribution from skewness and kurtosis ratios

This uses ratios of ratios of kurtosis and skewness to evaluate how
Normal or non-Normal the emissions data are. For small data sets where
`n = 3`, kurtosis and the standard error of kurtosis are estimated
differently and the only distribution outcomes are `'Normal'` or
`'Lognormal'`. Note that there are multiple methods for evaluating
skewness and kurtosis. The Fisher method is what is used in the old
Excel workbook and is a good selection for small `n < 300` samples from
an unknown population.

## Usage

``` r
distribution_type(data, emissions)
```

## Arguments

- data:

  Emissions data from either the best source or top performers, must
  have a column with numeric `emissions`.

- emissions:

  variable name or column number corresponding to the emissions used for
  selecting top performing sources.

## Value

String with either `'Normal'`, `'Lognormal'`, or `'Skewed'`

## References

On measuring skewness and kurtosis" Dragan Doric, et al. Springer
Science + Buisness Media B. V. 2007. September 20, 2007
