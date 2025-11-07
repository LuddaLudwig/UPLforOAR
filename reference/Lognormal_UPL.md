# Calculate UPL assuming lognormally distributed emissions data

Uses Gram-Charlier Series-A distribution approximation to determine the
lognormal UPL.

## Usage

``` r
Lognormal_UPL(data, future_runs = 3, significance = 0.99)
```

## Arguments

- data:

  Emissions data from either the best source or top performers, must
  have a column named `emissions`

- future_runs:

  Integer of future runs to use in prediction, the default is `3` since
  compliance uses 1 test average of 3 runs.

- significance:

  Level of significance from 0 to 1, the default is `0.99`.

## Value

Upper predictive limit at significance level for the average of the
number of future test runs

## References

"An upper prediction limit for the arithmetic mean of a lognormal random
variable" authored by Dulal Kumar Bhaumik and Robert David Gibbons 2004
