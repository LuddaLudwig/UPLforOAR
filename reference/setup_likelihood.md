# Sets up path to JAGS script, initial values, and variable list to monitor

This function defines the jagsmodel script to call based on the selected
distribution. It also defines the initial values and variables to
monitor.

## Usage

``` r
setup_likelihood(
  distribution,
  data,
  emissions,
  manual_prior = FALSE,
  prior_list = NULL,
  random = FALSE,
  RNG.state = NULL,
  custom_model = NULL,
  custom_params = NULL,
  custom_init = NULL
)
```

## Arguments

- distribution:

  Any of `'Normal'`, `'Gamma'`, `'Skewed'`, `'Lognormal'`, or `'Beta'`.
  If using a custom model script, set as `'Custom'`.

- data:

  Data from either the best source or top performers, must have a column
  with numeric `emissions`.

- emissions:

  variable name or column number corresponding to the emissions used for
  selecting top performing sources.

- manual_prior:

  Default is `FALSE`, priors are uninformative and calculated from range
  of emissions data. if `TRUE` priors should be specified manually in
  `prior_list`.

- prior_list:

  Optional list of
  [`stats::dunif()`](https://rdrr.io/r/stats/Uniform.html) upper and
  lower bounds for prior distributions. For `'Normal'` they are ordered
  `c(sd_low, sd_high, mean_low, mean_high)`. For `'Lognormal'` they are
  ordered `c(log_sd_low, log_sd_high, log_mean_low, log_mean_high)`. For
  `'Skewed'` they are ordered
  `c(omega_low, omega_high, xi_low, xi_high, alpha_low, alpha_high)`.
  For `'Gamma'` they are ordered
  `c(rate_low, rate_high, shape_low, shape_high)`. For `'Beta'` they are
  ordered `c(alpha_low, alpha_high, beta_low, beta_high)`.

- random:

  Default is `FALSE` where random seeds are defined via `.RNG.name` and
  `.RNG.seed` and returned as `state` so JAGS runs will be exactly
  reproducible. Changing to `TRUE` will generate new random states to
  use for `.RNG.name` and `.RNG.state` instead, also returned as `state`
  so the results can be recreated exactly if desired.

- RNG.state:

  Optional setting to specify a list of three lists setting the
  `.RNG.name` and `.RNG.state` for each MCMC chain. The default is a
  fixed set of RNG states so the results are always reproducible. If
  `random = TRUE` the RNG state is set randomly instead.

- custom_model:

  String for the file path location and name (i.e.
  "working_directory/Custom_JAGS.R") if using a custom JAGS model
  script.

- custom_params:

  List of parameters to monitor in addition to c('emission_hat',
  'pdf_obs', 'pdf_hat') if using a custom model.

- custom_init:

  List of three lists with initial values for MCMC chains corresponding
  to parameters in `custom_params`.

## Value

Object `model_code`, which is a string for the written R script that
JAGS can call, `par_list` which is the list of parameters traced while
running the JAGS model, `dat_inits` which is a list of initial parameter
values and random seeds for 3 chains, and the distribution used in
likelihood model.
