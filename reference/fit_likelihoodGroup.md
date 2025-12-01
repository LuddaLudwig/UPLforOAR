# fit_likelihoodGroup() calculates the error between fitted density and observed density distributions with a hierarchical group structure

This function takes the list of results from
[`output_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/output_likelihoodGroup.md)
and compares the predicted density distributions to observed density
distributions, estimating the `SSE` (sum of squared errors) and counts
the number of emissions observations with densities that have
overlapping 95 percent CI with predicted densities. Additional
parameters can be supplied for use in
[`obs_density()`](https://luddaludwig.github.io/UPLforOAR/reference/obs_density.md).
Rather than independent runs defining the population distribution,
hierarchical dependency within groups is allowed with the group-level
distribution parameters drawn from the overall population distribution.

## Usage

``` r
fit_likelihoodGroup(
  likelihood_result,
  up = Inf,
  low = 0,
  kernel = "gamma",
  bw = NULL
)
```

## Arguments

- likelihood_result:

  Output list from
  [`output_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/output_likelihoodGroup.md)

- up:

  Optional upper limit to bound density, default is `Inf`.

- low:

  Optional lower limit to bound density, default is `0`.

- kernel:

  Kernel choice for density function, default is `gamma` defined on
  `(0,Inf)`. Other options include:
  `c('gaussian1', 'gaussian2', 'beta1', 'beta2', 'fb', 'fbl', 'fbu', 'rigaussian')`.
  See
  [`np::npuniden.boundary()`](https://rdrr.io/pkg/np/man/npuniden.boundary.html)
  for more information on kernel options.

- bw:

  Optional bandwidth, default is `NULL` in which case
  `bw = sd(emissions) * n^(-2/5)`, where `n` is number of emissions. The
  bandwidth can also be provided manually, or searched for using least
  squares cross-validation by `bw = "cv.ls"` or likelihood
  cross-validation with `bw = "cv.ml"`.

## Value

A table `fit_dat` with the sum of squared error (SSE) between predicted
and observed probability densities, integration of predicted pdf, and
the count of emissions whose 95 percent CI around predicted probability
densities overlaps observed probability densities by group. Also
includes the distribution type used, and merged data sets of observed
and predicted densities at each emission value for group and population
levels, and each `xval`, named `obs_pdf_dat`, `xhat_pdf_grp` and
`xhat_pdf_pop`. The `obs_pdf_dat` also includes the upper and lower 95
percent and median around predicted pdf. The UPL estimate from
[`output_likelihoodGroup()`](https://luddaludwig.github.io/UPLforOAR/reference/output_likelihoodGroup.md)
is included as well as the RNG.state for record keeping.
