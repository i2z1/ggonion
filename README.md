# ggonion

R package for drawing onion diagrams with ggplot2

WIP (Work in progress)

## Usage

```
library(ggonion)

x <- c("one", "two", "three")
clr <- c("red","yellow", "green")
ggonion(x, color = clr)
```

![](./man/figures/example.png)

```
library(ggonion)

x <- c("one", "two", "three")
clr <- c("red","yellow", "green")
base <- ggonion(x, ratio = 2, bias = 0, color = clr)
base + theme_grey() + ggtitle("theme_grey()")
base + theme_bw() + ggtitle("theme_bw()")
base + theme_linedraw() + ggtitle("theme_linedraw()")
```

![](./man/figures/example_with_themes.png)

## Install

```
devtools::install_github("i2z1/ggonion")
```
