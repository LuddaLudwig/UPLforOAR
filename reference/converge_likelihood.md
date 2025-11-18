# Tests for convergence in likelihood parameters

Gelman-Rubin convergence tests for each defining parameter in the
likelihood distribution.

## Usage

``` r
converge_likelihood(jags_model_run, custom_params = NULL)
```

## Arguments

- jags_model_run:

  The output list returned from
  [`run_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/run_likelihood.md).

- custom_params:

  List of parameters to check for convergence if using a custom model,
  e.g. `c('parameter1', 'parameter2')`.

## Value

A tibble of parameters and convergence results from
[`coda::gelman.diag()`](https://rdrr.io/pkg/coda/man/gelman.diag.html),
Values greater than 1.2 indicate problems in convergence. Values between
1.1 and 1.2 indicate weak convergence. Values less than 1.1 indicate
good convergence. This test indicates if the 3 mcmc chains are will
mixed and stable but is insufficient as the only indicator of
convergence. Visual plots of posterior distributions should be
investigated as well.
