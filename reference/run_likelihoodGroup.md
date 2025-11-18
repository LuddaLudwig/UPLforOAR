# Runs JAGS model scripts for chosen likelihood with a hierarchical group structure

Runs the JAGS model from
[`setup_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/setup_likelihood.md)
model_code output using
[`runjags::run.jags()`](https://rdrr.io/pkg/runjags/man/run.jags.html).
Traces all variables in `par_list` running 3 chains in parallel with
`burnin` and `adapt` of 10,000 each and keeping 10,000 iterations per
chain. Rather than independent runs defining the population
distribution, hierarchical dependency within groups is allowed with the
group-level distribution parameters drawn from the overall population
distribution.

## Usage

``` r
run_likelihoodGroup(
  model_input,
  xvals = NULL,
  minY = 0,
  maxY = NULL,
  future_runs = 3
)
```

## Arguments

- model_input:

  Results from
  [`setup_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/setup_likelihoodGroup.md),
  including JAGS model script, emissions data, distribution, initial
  values list, and parameters to monitor.

- xvals:

  Ordered sequence of emissions at which to predict probability density.
  Default is `NULL`, in which case `x_hat` is a 1024 length sequence
  between `0` and `3 * max(data$emissions)` or `minY` and `maxY` if they
  are specified.

- minY:

  The minimum emission value possible, used to truncate likelihood
  distributions. Default is 0.

- maxY:

  The maximum emission value possible, used to truncate likelihood
  distributions and set upper ranges on prior distributions. Default is
  `NULL`, in which case it is calculated as
  `3 * maximum(data$emissions)`.

- future_runs:

  Integer of future runs to use in prediction, the default is `3` since
  compliance uses 1 test average of 3 runs.

## Value

`runjags` object named `run_results`, likelihood distribution from the
JAGS model script, as well as the RNG state, `emissions` and `group`
from the `data` provided in
[`setup_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/setup_likelihoodGroup.md)
and `xval`s used as inputs.
