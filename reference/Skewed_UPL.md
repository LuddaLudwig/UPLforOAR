# Calculate UPL assuming skew-normal distributed emissions data

Calculate UPL assuming skew-normal distributed emissions data

## Usage

``` r
Skewed_UPL(data, emissions, future_runs = 3, significance = 0.99)
```

## Arguments

- data:

  Data from either the best source or top performers, must have a column
  with numeric `emissions`.

- emissions:

  variable name or column number corresponding to the emissions used for
  selecting top performing sources.

- future_runs:

  Integer of future runs to use in prediction, the default is `3` since
  compliance uses 1 test average of 3 runs.

- significance:

  Level of significance from 0 to 1, the default is `0.99`.

## Value

Upper predictive limit (UPL) at significance level for the average of
the number of future test runs.
