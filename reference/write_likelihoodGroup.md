# Writes likelihood scripts for JAGS model calls with a hierarchical group structure

This function writes an R script for JAGS to call based on the selected
distribution and prior. The priors are uninformative and set based on
emissions data, unless specified manually via
[`setup_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/setup_likelihoodGroup.md).
The likelihood distributions are truncated to `(minY, maxY)`, where
`minY` and `maxY` can be specified or used with the default `minY = 0`
and `maxY = 3 * max(data$emissions)` in
[`run_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/run_likelihood.md).
Rather than independent runs defining the population distribution,
hierarchical dependency within groups is allowed with the group-level
distribution parameters drawn from the overall population distribution.

## Usage

``` r
write_likelihoodGroup(distribution, manual_prior = FALSE, write_wd = NULL)
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
  [`run_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/run_likelihoodGroup.md)
  will look for the JAGS scripts assigned via
  [`setup_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/setup_likelihoodGroup.md).

## Value

object `model_code`, which is a string for the written R script that
JAGS can call and the distribution used in likelihood model.
