# Makes a list of environments for automating analysis in reg text templates

Takes a data set and variable lists to loop through and create a list of
self-contained environments that the child.Rmd documents can loop
through for the reg text document templates.

## Usage

``` r
make_environment(
  data,
  HAP,
  emissions,
  sources,
  subcat_name = NA,
  subcat_level = NA,
  type = c("New", "Existing", "As-is"),
  CAA_section = 112,
  national_N = NA,
  meas_unit,
  future_runs = 3,
  significance = 0.99,
  ...
)
```

## Arguments

- data:

  A data set containing the sources, all emissions, and any
  subcategories if applicable. Each row should be an individual run.
  Emissions must be numeric, and sources and subcategories must be
  characters or factors.

- HAP:

  A string with the name of the Hazardous Air Pollutant (HAP) as it
  should appear in text or figure labels.

- emissions:

  variable name or column number corresponding to the HAP emissions used
  for selecting top performing sources.

- sources:

  variable name or column number corresponding to the source
  designation, either a character or factor.

- subcat_name:

  The name of the subcategory variable corresponding to the HAP given in
  `emissions`, either a character or factor. If there are no
  subcategories for the HAP, use `NA`.

- subcat_level:

  If a subcategory is provided in `subcat_name`, indicate which level
  here. For example, if the subcategory is `'facility_size'`, and the
  levels are `c('small', 'medium', 'large')`, then one analysis might be
  `subcat_level = 'medium'`. If there are no subcategories for the HAP,
  use `NA`.

- type:

  Either `'As-is'`, `'New'`, or `'Existing'`, Using `'As-is'` will run
  the UPL analysis on the HAP emissions in `data` with no subsetting.
  `'New'` will select the top performing source for the HAP using
  [`MACT_new()`](https://luddaludwig.github.io/UPLforOAR/reference/MACT_new.md),
  and `'Existing'` will select the top performing sources for the HAP
  using
  [`MACT_existing()`](https://luddaludwig.github.io/UPLforOAR/reference/MACT_existing.md).

- CAA_section:

  Applicable Clean Air Act section, either 112 or 129.

- national_N:

  For Clean Air Act section 129 (`CAA_section = 129`), the additional
  argument providing the total number of sources nation-wide regardless
  of whether or not they contributed emissions data is required.

- meas_unit:

  String of measurment units to be used in text and figure axes labels.

- future_runs:

  Integer of future runs to use in prediction, the default is `3` since
  compliance uses 1 test average of 3 runs.

- significance:

  Level of significance from 0 to 1, the default is `0.99`.

- ...:

  Other arguments passed on to
  [`Bayesian_UPL()`](https://luddaludwig.github.io/UPLforOAR/reference/Bayesian_UPL.md)
  or
  [`obs_density()`](https://luddaludwig.github.io/UPLforOAR/reference/obs_density.md).
  Check the child .Rmd to figure match the arguments to expected name in
  template.

## Value

A list of environments with length equal to the number of UPL
calculations used in report
