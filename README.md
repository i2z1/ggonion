
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggonion <img src="man/figures/hex.png" width="140px" height="140px" align="right" style="padding-left:10px;" />

R package for drawing onion diagrams with ggplot2

WIP (Work in progress)

## Installation

You can install development version if ggonion from Github with

    devtools::install_github("i2z1/ggonion")

## Usage

``` r
library(ggonion)

x <- c("1", "2", "3","4")
clr <- c("orange", "#ecefa2", "#a2cda3", "#8a99e9")
ggonion(x, color = clr)
```

![](man/figures/README-example-1.png)<!-- -->
