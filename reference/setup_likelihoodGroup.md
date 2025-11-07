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
  manual_prior = FALSE,
  prior_list = NULL,
  random = FALSE
)
```

## Arguments

- distribution:

  Any of `'Normal'`, `'Gamma'`, `'Skewed'`, `'Lognormal'`, or `'Beta'`.

- data:

  Emissions data from either the best source or top performers, must
  have a column named `emissions`.

- manual_prior:

  Default is `FALSE`, priors are uninformative and calculated from range
  of emissions data. if `TRUE` priors should be specified manually in
  `prior_list`.

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
  `.RNG.seed` so JAGS runs will be exactly reproducible. Changing to
  `TRUE` will use random values for `.RNG.name` and `.RNG.seed` instead.

## Value

Object `model_code`, which is a string for the written R script that
JAGS can call, `par_list` which is the list of parameters traced while
running the JAGS model, `dat_inits` which is a list of initial parameter
values and random seeds for 3 chains, and the distribution used in
likelihood model.
