# Selects best performer from emissions data

Selects best performer from emissions data

## Usage

``` r
MACT_new(data, emissions, sources)
```

## Arguments

- data:

  Data.frame or tibble with columns for `emissions` (numeric) and
  `sources` (character or factor)

- emissions:

  variable name or column number corresponding to the emissions used for
  selecting top performing sources.

- sources:

  variable name or column number corresponding to the source
  designation, either a character or factor.

## Value

Data set (tibble) containing the emissions data for the best performing
source for New Source Performance Standards.
