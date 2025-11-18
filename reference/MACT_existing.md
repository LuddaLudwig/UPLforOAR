# Selects top sources from emissions data

Ranks the sources by their average emission from best to worst, then
selects either the top 5 or top 12 percent depending on the applicable
CAA section and number of sources with data available.

## Usage

``` r
MACT_existing(data, emissions, sources, CAA_section = 112)
```

## Arguments

- data:

  Data.frame or tibble with columns for `emissions` (numeric) and
  `sources` (character or factor).

- emissions:

  variable name or column number corresponding to the emissions used for
  selecting top performing sources.

- sources:

  variable name or column number corresponding to the source
  designation, either a character or factor.

- CAA_section:

  Applicable Clean Air Act section, either 112 or 129

## Value

Data set (tibble) of the top 5 or 12 percent of sources, depending on
the number of sources, to be used in UPL calculations for Maximum
Achievable Control Technology (MACT) floor analysis for Existing Source
Guidelines.
