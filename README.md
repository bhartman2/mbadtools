
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `mbadtools`

<!-- badges: start -->

<!-- badges: end -->

The goal of `mbadtools` is to provide MBAD students with the key
packages the course focuses on, and to also provide some tools (in the
form of functions) for certain tasks often encountered in your work.

## Installation

You can install the current version of `mbadtools` like so:

``` r
# installs latest version, skips if already there.
pak::pak("bhartman2/mbadtools")
#> ✔ Updated metadata database: 6.56 MB in 4 files.
#> ℹ Updating metadata database✔ Updating metadata database ... done
#>  
#> → Package library at 'C:\Users\bruce\AppData\Local\R\win-library\4.6'.
#> ℹ No downloads are needed
#> ✔ 1 pkg + 200 deps: kept 160 [25.7s]
```

## Notebook or Script Usage

This is a basic example which shows you how to load `mbadtools`; that
is, make `mbadtools` available to your notebook. The initial output of
this example is shown here, but you cn suppress it in your notebook;
it’s only informative if you have a problem.

``` r
library(mbadtools)
#> Loading required package: broom
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: ggplot2
#> Loading required package: stringr
#> Loading required package: tibble
#> Loading required package: tidyr
#> Loading required package: parsnip
#> Loading required package: magrittr
#> 
#> Attaching package: 'magrittr'
#> The following object is masked from 'package:tidyr':
#> 
#>     extract
#> Loading required package: rlang
#> 
#> Attaching package: 'rlang'
#> The following object is masked from 'package:magrittr':
#> 
#>     set_names
#> Loading required package: bvartools
#> Loading required package: coda
#> Loading required package: Matrix
#> 
#> Attaching package: 'Matrix'
#> The following objects are masked from 'package:tidyr':
#> 
#>     expand, pack, unpack
#> Loading required package: vars
#> Loading required package: MASS
#> 
#> Attaching package: 'MASS'
#> The following object is masked from 'package:dplyr':
#> 
#>     select
#> Loading required package: strucchange
#> Loading required package: zoo
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
#> Loading required package: sandwich
#> 
#> Attaching package: 'strucchange'
#> The following object is masked from 'package:stringr':
#> 
#>     boundary
#> Loading required package: urca
#> Loading required package: lmtest
#> 
#> Attaching package: 'vars'
#> The following objects are masked from 'package:bvartools':
#> 
#>     fevd, irf
#> Loading mbadtools packages:
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ forcats   1.0.1     ✔ purrr     1.2.2
#> ✔ lubridate 1.9.5     ✔ readr     2.2.0
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ strucchange::boundary() masks stringr::boundary()
#> ✖ Matrix::expand()        masks tidyr::expand()
#> ✖ magrittr::extract()     masks tidyr::extract()
#> ✖ dplyr::filter()         masks stats::filter()
#> ✖ purrr::flatten()        masks rlang::flatten()
#> ✖ purrr::flatten_chr()    masks rlang::flatten_chr()
#> ✖ purrr::flatten_dbl()    masks rlang::flatten_dbl()
#> ✖ purrr::flatten_int()    masks rlang::flatten_int()
#> ✖ purrr::flatten_lgl()    masks rlang::flatten_lgl()
#> ✖ purrr::flatten_raw()    masks rlang::flatten_raw()
#> ✖ purrr::invoke()         masks rlang::invoke()
#> ✖ dplyr::lag()            masks stats::lag()
#> ✖ Matrix::pack()          masks tidyr::pack()
#> ✖ MASS::select()          masks dplyr::select()
#> ✖ purrr::set_names()      masks rlang::set_names(), magrittr::set_names()
#> ✖ purrr::splice()         masks rlang::splice()
#> ✖ Matrix::unpack()        masks tidyr::unpack()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
#> ── Attaching packages ────────────────────────────────────── tidymodels 1.5.0 ──
#> 
#> ✔ dials        1.4.4     ✔ tailor       0.1.0
#> ✔ infer        1.1.0     ✔ tune         2.1.0
#> ✔ modeldata    1.5.1     ✔ workflows    1.3.0
#> ✔ recipes      1.3.3     ✔ workflowsets 1.1.1
#> ✔ rsample      1.3.2     ✔ yardstick    1.4.0
#> 
#> ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
#> ✖ scales::discard()    masks purrr::discard()
#> ✖ Matrix::expand()     masks tidyr::expand()
#> ✖ magrittr::extract()  masks tidyr::extract()
#> ✖ dplyr::filter()      masks stats::filter()
#> ✖ recipes::fixed()     masks stringr::fixed()
#> ✖ purrr::flatten()     masks rlang::flatten()
#> ✖ purrr::flatten_chr() masks rlang::flatten_chr()
#> ✖ purrr::flatten_dbl() masks rlang::flatten_dbl()
#> ✖ purrr::flatten_int() masks rlang::flatten_int()
#> ✖ purrr::flatten_lgl() masks rlang::flatten_lgl()
#> ✖ purrr::flatten_raw() masks rlang::flatten_raw()
#> ✖ purrr::invoke()      masks rlang::invoke()
#> ✖ dplyr::lag()         masks stats::lag()
#> ✖ Matrix::pack()       masks tidyr::pack()
#> ✖ MASS::select()       masks dplyr::select()
#> ✖ purrr::set_names()   masks rlang::set_names(), magrittr::set_names()
#> ✖ yardstick::spec()    masks readr::spec()
#> ✖ purrr::splice()      masks rlang::splice()
#> ✖ recipes::step()      masks stats::step()
#> ✖ Matrix::unpack()     masks tidyr::unpack()
#> ✖ recipes::update()    masks Matrix::update(), stats::update()
#> 
#> Registered S3 method overwritten by 'ggfortify':
#>   method          from   
#>   autoplot.glmnet parsnip
#> 
#> tidyverse  tidymodels  ggfortify  GGally  skimr  gt  patchwork  car  yardstick  ggh4x
```

Because `lmtest` overwrites functions in package `dplyr` (a part of
tidyverse), we reset the most important conflicts to prioritize `dplyr`
over others.

``` r
# Tell R to always prefer dplyr's select or filter function over any other package
conflicted::conflict_prefer_matching("select|filter", "dplyr", quiet=T)
```

## Packages Loaded

Here is a current list of the packages loaded with `mbadtools`.

| Package | Use |
|:---|----|
| tidyverse | host of packages for modern data and code handling in R |
| tidymodels | host of packages for tidy modeling |
| workflowsets | for multi model workflows |
| ggfortify | tools to improve use of `ggplot2` graphics |
| GGally | tools for `ggplot2` graphics especially the `ggpairs()` function |
| skimr | better than `summary()` for quickly exploring and summarizing data |
| gt | displaying neat, readable tabular data |
| patchwork | tools for combining `ggplot` objects |
| car | tools for statistical modeling and data analysis |
| lmtest | tools for advanced statistical modeling and data analysis |
| gh4x | for the `geom_pointpath()` function |

## Basic Notebook Template

A basic notebook template is provided to help you set up a notebook to
write an assignment. It is an .Rmd file like a miniature paper with code
and output interspersed, and with typical key headings you may want to
include.

[View Basic Notebook
Template](https://htmlpreview.github.io/?https://github.com/bhartman2/mbadtools/blob/main/notebook/Basic%20Notebook%20Template.nb.html)

## Functions in `mbadtools`

Some useful functions are included in `mbadtools`.

| Function | Description |
|:---|:---|
| `gg_residual_plots()` | displays 7 different residual plots for linear regression |
| `gg_partial_residual_plots()` | displays partial residual plots for linear regression |
| `gt_add_significance()` | `gt` table for dataframe with a `p.value` column; adds color when p.value is significant |
|  |  |
| `GrangerTest()` | performs Granger causality tests for time series data frame |
| `GrangerPlot()` | plots results of `GrangerTest()` |
| `GrangerTestPvals()` | performs Granger causality tests for time series data |
| `GrangerTestTune()` | plots results of `GrangerTestPvals()` |

The first 3 are demonstrated in [Using
mbadtools](https://htmlpreview.github.io/?https://github.com/bhartman2/mbadtools/blob/main/notebook/Using%20mbadtools.nb.html).

The Granger Causality functions are demonstrated in [Granger
MoodysDemo](https://htmlpreview.github.io/?https://github.com/bhartman2/mbadtools/blob/main/notebook/GrangerMoodysDemo.nb.html)

Others can be viewed in the `mbadtools` help.
