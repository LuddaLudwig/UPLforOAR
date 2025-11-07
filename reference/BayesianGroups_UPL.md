# BayesianGroups_UPL() wraps `_likelihoodGroup()` functions into results comparable for multiple distributions with a hierarchical group structure

For each distribution in `distr_list`, `BayesianGroups_UPL()` will
[`setup_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/setup_likelihoodGroup.md),
[`run_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/run_likelihoodGroup.md),
organize mcmc results in
[`output_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/output_likelihoodGroup.md),
test for convergence of likelihood parameters using
[`converge_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/converge_likelihoodGroup.md),
and calculate goodness of fit metrics using
[`fit_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/fit_likelihood.md).
Rather than independent runs defining the population distribution,
hierarchical dependency within groups is allowed with the group-level
distribution parameters drawn from the overall population distribution.
Results include `$fit_table`: a tibble with the `UPL`, `pdf_integral`,
`SSE`, and count of observations within 95 percent CI for each
distribution in `'distr_list'`, `$conv_output`: a tibble with the
parameters, Gelman-Rubin diagnostics, and if the converged for
distribution in `distr_list`, `$obs_pdf_dat`: a tibble with the
emissions observations, corresponding observation densities, median,
upper, and lower 95 percent CI around predicted densities for the
distribution in `distr_list`, and a 1 if the observation is within the
95 percent CI, a 0 otherwise, and `$pred_pdf_dat`:a tibble with the
predicted probability density `pdf_hat`, the observation density `ydens`
for each value in the range of emissions in `x_hat`. The maximum
emission value of distributions, `maxY`, the ordered range emissions to
predict to `xvals`, and the prior distributions and initial values are
all automatically supplied from the emissions data to be fully
encompassing and uninformative by default. They can be supplied manually
instead however by supplying `maxY`, `xvals`, or setting
`manual_prior = TRUE` with corresponding lower and upper limits in
`prior_list`. If manual priors are used, only a single distribution can
be run at a time in `distr_list`.

## Usage

``` r
BayesianGroups_UPL(
  distr_list = c("Normal", "Skewed", "Lognormal", "Gamma", "Beta"),
  data,
  future_runs = 3,
  significance = 0.99,
  xvals = NULL,
  maxY = NULL,
  minY = 0,
  group = "sources",
  convergence_report = FALSE,
  random = FALSE,
  manual_prior = FALSE,
  prior_list = NULL
)
```

## Arguments

- distr_list:

  A list including one or more of
  `c('Normal', 'Skewed', 'Lognormal', 'Gamma', 'Beta')`. Note that if
  prior bounds are supplied manually, only one distribution can be used.

- data:

  Emissions data from either the best source or top performers, must
  have a column named 'emissions'.

- future_runs:

  Integer of future runs to use in prediction, the default is `3` since
  compliance uses 1 test average of 3 runs.

- significance:

  Level of significance from 0 to 1, the default is `0.99`.

- xvals:

  Ordered sequence of emissions at which to predict probability density.
  Default is `NULL`, in which case `x_hat` is a 1024 length sequence
  between `0` and `3 * max(data$emissions)`.

- maxY:

  The maximum emission value possible, used to truncate likelihood
  distributions and set upper ranges on prior distributions, if not
  specified manually. Default is `NULL`, in which case is is calculated
  as `3 * max(data$emissions)`.

- minY:

  The minimum emission value possible, used to truncate likelihood
  distributions. Default is 0.

- group:

  Character string corresponding to the variable name in the data set by
  which to group for the hierarchical structure. If the group is not a
  factor it will be coerced using as.factor(). To avoid having unknown
  factor levels, please convert to factor first.

- convergence_report:

  Default is `FALSE`, if a report containing convergence figures should
  be generated with results. If `TRUE`, a document
  Bayesian_UPL_convergence_MMDDYYY_HHMM.pdf will be written to the
  current working directory. Note that this is an Rmarkdown document
  that requires either LaTex or MikTek installed in order to render the
  PDF.

- random:

  Default is `FALSE` where random seeds are defined via `.RNG.name` and
  `.RNG.seed` so JAGS runs will be exactly reproducible. Changing to
  `TRUE` will use random values for `.RNG.name` and `.RNG.seed` instead.

- manual_prior:

  Default is `FALSE`, if priors should be specified manually or be
  uninformative calculated from range of emissions data. Note that if
  you are supplying priors manually than you can only run one type of
  distribution at a time.

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

## Value

A list of tibble results from
[`setup_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/setup_likelihoodGroup.md),
[`run_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/run_likelihoodGroup.md),
[`output_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/output_likelihoodGroup.md),
[`obs_density()`](https://luddaludwig.github.io/UPLforOAR/reference/obs_density.md),
[`fit_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/fit_likelihood.md),
and
[`converge_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/converge_likelihoodGroup.md)
for each distribution in `distr_list`.
