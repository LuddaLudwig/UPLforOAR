# Tests for convergence in likelihood parameters with a hierarchical group structure

Gelman-Rubin convergence tests for each defining parameter in the
likelihood distribution. Rather than independent runs defining the
population distribution, hierarchical dependency within groups is
allowed with the group-level distribution parameters drawn from the
overall population distribution.

## Usage

``` r
converge_likelihoodGroup(jags_model_run)
```

## Arguments

- jags_model_run:

  The output list returned from
  [`run_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/run_likelihoodGroup.md).

## Value

A tibble of parameters and convergence results from
[`coda::gelman.diag()`](https://rdrr.io/pkg/coda/man/gelman.diag.html),
Values greater than 1.2 indicate problems in convergence. Values between
1.1 and 1.2 indicate weak convergence. Values less than 1.1 indicate
good convergence. This test indicates if the 3 mcmc chains are will
mixed and stable but is insufficient as the only indicator of
convergence. Visual plots of posterior distributions should be
investigated as well.
