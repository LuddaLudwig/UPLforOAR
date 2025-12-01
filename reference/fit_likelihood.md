# fit_likelihood() calculates the error between fitted density and observed density distributions

This function takes the list of results from
[`output_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/output_likelihood.md)
and compares the predicted density distributions to observed density
distributions, estimating the `SSE` (sum of squared errors) and counts
the number of emissions observations with densities that have
overlapping 95 percent CI with predicted densities. Additional
parameters can be supplied for use in
[`obs_density()`](https://luddaludwig.github.io/UPLforOAR/reference/obs_density.md)

## Usage

``` r
fit_likelihood(
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
  [`output_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/output_likelihood.md)

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

The sum of squared error (SSE) between predicted and observed
probability densities, and the count of emissions whose 95 percent CI
around predicted probability densities overlaps observed probability
densities. Also includes the integration of predicted pdf, distribution
type used, and merged data sets of observed and predicted densities at
each emission value and each `xval`, named `obs_pdf_dat` and
`xhat_pdf_dat` respectively. The `obs_pdf_dat` also includes the upper
and lower 95 percent and median around predicted pdf. The UPL estimate
from
[`output_likelihood()`](https://luddaludwig.github.io/UPLforOAR/reference/output_likelihood.md)
is included as well as the RNG.state for record keeping.
