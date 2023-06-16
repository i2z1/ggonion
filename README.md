
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggonion

R package for drawing onion diagrams with ggplot2

WIP (Work in progress)

## Installation

You can install development version if ggonion from Github with

    devtools::install_github("i2z1/ggonion")

## Usage

``` r
library(ggonion)

x <- c("one", "two", "three")
clr <- c("red","yellow", "green")
ggonion(x, color = clr)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
