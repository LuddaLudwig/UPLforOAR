
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

## Example emissions data

This is example uses Hg emissions data from the recent [EPA
rule-making](https://www.regulations.gov/document/EPA-HQ-OAR-2009-0234-20132)
National Emission Standards for Hazardous Air Pollutants for Coal- and
Oil-fired Electric Utility Steam Generating Units. This data set
contains a lot of test report information, but only columns for
‘emissions’ and ‘sources’ are needed for the MACT floor UPL analysis.
The ‘emissions’ and ‘sources’ need to be named such explicitly. The
emissions should all be in consistent units, and the sources should be
unique at the unit-level (e.g. a single boiler), not including
sub-categories.

``` r
library(EPA.MACT.floor.UPL)
dat_emiss=read_csv("man/data_example/MATS_Hg.csv",col_names=TRUE)
dat_emiss$sources=paste0(dat_emiss$`Plant Name`,"_",dat_emiss$`Unit Number`,"_",dat_emiss$boiler_id)
dat_emiss$emissions=dat_emiss$Mercury_min_lb_MMBtu
dat_emiss=subset(dat_emiss,select=c(sources,emissions))
summary(dat_emiss$emissions)
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> 2.630e-09 2.805e-07 1.570e-06 2.713e-06 3.820e-06 2.990e-05

dat_EG=MACT_EG(CAA_section=112,dat_emiss)
dat_EG_avg=dat_EG%>%group_by(sources)%>%summarize(avg=mean(emissions),
                                                                 counts=n())
dat_EG_avg=arrange(dat_EG_avg,avg)
distribution_result_EG=distribution_type(dat_EG)
```

Since there were more than 30 sources in the emissions data, the top 12%
were chosen to represent the top sources. This yielded `nrow(dat_EG)`
sources. The data included in this regulatory docket were test averages
as opposed to individual runs. As such the number of future runs used in
UPL calculations will be `1` instead of the default, an average of `3`
runs. The appropriate distribution for the UPL calculation is
`distribution_result_EG`.

| Source                           | Average emission | No. of Tests |
|:---------------------------------|-----------------:|-------------:|
| Spruance Genco, LLC_GEN2_2A      |         2.63e-09 |            1 |
| Spruance Genco, LLC_GEN2_2B      |         2.63e-09 |            1 |
| Spruance Genco, LLC_GEN3_3A      |         4.69e-09 |            1 |
| Spruance Genco, LLC_GEN3_3B      |         4.69e-09 |            1 |
| Logan Generating Plant_Unit1_B01 |         5.33e-09 |            1 |
| Nucla_001_1                      |         5.33e-09 |            1 |

Top sources for existing guidance UPL calculation

    #>      speed           dist       
    #>  Min.   : 4.0   Min.   :  2.00  
    #>  1st Qu.:12.0   1st Qu.: 26.00  
    #>  Median :15.0   Median : 36.00  
    #>  Mean   :15.4   Mean   : 42.98  
    #>  3rd Qu.:19.0   3rd Qu.: 56.00  
    #>  Max.   :25.0   Max.   :120.00

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.
