# Writes likelihood scripts for JAGS model calls

This function writes an R script for JAGS to call based on the selected
distribution and prior. The priors are uninformative and set based on
emissions data, unless specified manually via
[`setup_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/setup_likelihood.md).
The likelihood distributions are truncated to `(minY, maxY)`, where
`minY` and `maxY` can be specified or used with the default `minY = 0`
and `maxY = 3 * max(data$emissions)` in
[`run_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/run_likelihood.md).

## Usage

``` r
write_likelihood(distribution, manual_prior = FALSE, write_wd = NULL)
```

## Arguments

- distribution:

  Any of `'Normal'`, `'Gamma'`, `'Skewed'`, `'Lognormal'`, or `'Beta'`.

- manual_prior:

  Default is `FALSE`, if priors should be specified manually or be
  uninformative calculated from range of emissions data.

- write_wd:

  Default is `NULL`, in which case the JAGS scripts are written into
  inst/JAGS folder in package directory. This is the location
  [`run_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/run_likelihood.md)
  will look for the JAGS scripts assigned via
  [`setup_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/setup_likelihood.md).

## Value

object `model_code`, which is a string for the written R script that
JAGS can call and the distribution used in likelihood model.
