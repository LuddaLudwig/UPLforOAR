# Organizes mcmc output from [`run_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/run_likelihood.md)

Output_likelihood() takes the `jags_model_run` produced by
[`run_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/run_likelihood.md),
merges the mcmc chains and calculates the UPL as well as providing the
predicted pdf and metrics.

## Usage

``` r
output_likelihood(jags_model_run, significance = 0.99)
```

## Arguments

- jags_model_run:

  The output list returned from
  [`run_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/run_likelihood.md),
  which includes the jags model `run_results`, likelihood distribution
  type, and `data`, `xvals`, and `future_runs` used as inputs to
  [`run_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/run_likelihood.md).

- significance:

  Level of significance from 0 to 1, the default is `0.99`.

## Value

A list including `distr`, the distribution used in
[`write_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/write_likelihood.md),
`predicted_mean`, the mean of the fitted distribution, `UPL_Bayes`, the
upper predictive limit based on the `significance` level and average
distribution of `future_runs` number of draws, `obs_pdf`, the predicted
probability density at each observation, and `pred_pdf`, the predicted
probability density at each point in `xvals`.
