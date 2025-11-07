# Calculate UPL assuming normally distributed emissions data

Calculate UPL assuming normally distributed emissions data

## Usage

``` r
Normal_UPL(data, future_runs = 3, significance = 0.99)
```

## Arguments

- data:

  Emissions data from either the best source or top performers, must
  have a column named `emissions`.

- future_runs:

  Integer of future runs to use in prediction, the default is `3` since
  compliance uses 1 test average of 3 runs.

- significance:

  Level of significance from 0 to 1, the default is `0.99`.

## Value

Upper predictive limit (UPL) at significance level for the average of
the number of future test runs.
