# Sets up path to JAGS script with a hierarchical group structure, initial values, and variable list to monitor

This function defines the jagsmodel script to call based on the selected
distribution. It also defines the initial values and variables to
monitor. Rather than independent runs defining the population
distribution, hierarchical dependency within groups is allowed with the
group-level distribution parameters drawn from the overall population
distribution.

## Usage

``` r
setup_likelihoodGroup(
  distribution,
  data,
  emissions,
  manual_prior = FALSE,
  group = "sources",
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
  with numeric `emissions` and a column with character or factor `group`
  used for hierarchical structure.

- emissions:

  Variable name or column number corresponding to the emissions used for
  selecting top performing sources.

- manual_prior:

  Default is `FALSE`, priors are uninformative and calculated from range
  of emissions data. if `TRUE` priors should be specified manually in
  `prior_list`.

- group:

  Variable name or column number corresponding to the variable name in
  the data set by which to group for the hierarchical structure. If the
  group is not a factor it will be coerced using as.factor(). To avoid
  having unknown factor levels, please convert to factor first. (set
  using `as.factor(data$group_name)` if needed). Defaults to
  `'sources'`.

- prior_list:

  Optional list of
  [`stats::dunif()`](https://rdrr.io/r/stats/Uniform.html) upper and
  lower bounds for prior distributions. For `'Normal'` and `'Lognormal'`
  they are ordered
  `c(pop_sd_mu_low, pop_sd_mu_high, pop_mu_mu_low, pop_mu_mu_high, pop_sd_sd_low, pop_sd_sd_high, pop_mu_sd_low, pop_mu_sd_high)`.
  For `'Skewed'` they are ordered
  `c(pop_omega_mu_low, pop_omega_mu_high, pop_xi_mu_low, pop_xi_mu_high, pop_alpha_mu_low, pop_alpha_mu_high, pop_omega_sd_low, pop_omega_sd_high, pop_xi_sd_low, pop_xi_sd_high, pop_alpha_sd_low, pop_alpha_sd_high)`.
  For `'Gamma'` they are ordered
  `c(pop_rate_mu_low, pop_rate_mu_high, pop_shape_mu_low, pop_shape_mu_high, pop_rate_sd_low, pop_rate_sd_high, pop_shape_sd_low, pop_shape_sd_high)`.
  For `'Beta'` they are ordered
  `c(pop_alpha_mu_low, pop_alpha_mu_high, pop_beta_mu_low, pop_beta_mu_high, pop_alpha_sd_low, pop_alpha_sd_high, pop_beta_sd_low, pop_beta_sd_high)`.

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

  List of parameters to monitor in addition to
  `c('emission_hat', 'pdf_obs', 'pdf_hat', 'group_emiss')` if using a
  custom model.

- custom_init:

  List of three lists with initial values for MCMC chains corresponding
  to parameters in `custom_params`.

## Value

Object `model_code`, which is a string for the written R script that
JAGS can call, `par_list` which is the list of parameters traced while
running the JAGS model, `dat_inits` which is a list of initial parameter
values and random seeds for 3 chains, and the distribution used in
likelihood model. Also included are a data set with `emissions` and
`group` and `prior_list` if applicable to be passed along for use in
[`run_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/run_likelihoodGroup.md).
