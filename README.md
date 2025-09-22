
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EPA.MACT.floor.UPL

<!-- badges: start -->
<!-- badges: end -->

The goal of EPA.MACT.floor.UPL is to provide a set of functions for
handling NESHAP emissions datasets for MACT floor analysis and UPL
calculations. These functions include selecting the best and top
performing sources from emissions data based on appropriate Clean Air
Act sections, determining the appropriate distributions for the
emissions data, and calculating the UPL for EG and NSPS standards.

## Installation

You can install the development version of EPA.MACT.floor.UPL from
[GitHub](https://github.com/LuddaLudwig/EPA.MACT.floor.UPL) with:

``` r
# install.packages("pak")
pak::pak("LuddaLudwig/EPA.MACT.floor.UPL")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(EPA.MACT.floor.UPL)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ forcats   1.0.0     ✔ stringr   1.5.2
#> ✔ lubridate 1.9.4     ✔ tibble    3.3.0
#> ✔ purrr     1.1.0     ✔ tidyr     1.3.1
#> ✔ readr     2.1.5
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library(viridis)
#> Loading required package: viridisLite
library(ggtext)
library(matrixStats)
#> 
#> Attaching package: 'matrixStats'
#> 
#> The following object is masked from 'package:dplyr':
#> 
#>     count
library(scales)
#> 
#> Attaching package: 'scales'
#> 
#> The following object is masked from 'package:viridis':
#> 
#>     viridis_pal
#> 
#> The following object is masked from 'package:purrr':
#> 
#>     discard
#> 
#> The following object is masked from 'package:readr':
#> 
#>     col_factor
library(EnvStats)
#> 
#> Attaching package: 'EnvStats'
#> 
#> The following object is masked from 'package:matrixStats':
#> 
#>     iqr
#> 
#> The following objects are masked from 'package:stats':
#> 
#>     predict, predict.lm
#> 
#> The following object is masked from 'package:base':
#> 
#>     print.default
library(patchwork)
library(grid)
library(np)
#> Nonparametric Kernel Methods for Mixed Datatypes (version 0.60-18)
#> [vignette("np_faq",package="np") provides answers to frequently asked questions]
#> [vignette("np",package="np") an overview]
#> [vignette("entropy_np",package="np") an overview of entropy-based methods]
library(sfsmisc)
#> 
#> Attaching package: 'sfsmisc'
#> 
#> The following object is masked from 'package:dplyr':
#> 
#>     last
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.
