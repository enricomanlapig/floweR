
# About

This package provides a single function `make_flowers`.

## Installation

You can install the development version of make\_flowers
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("enricomanlapig/floweR")
```

## Example graphic

``` r
library(floweR)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.0.5
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 4.0.5


num_points <- 30

df <- data.frame(
  group = c(rep("A", num_points),
            rep("B", num_points),
            rep("C", num_points)),
  metric = c(runif(num_points)*0.5,
             runif(num_points),
             runif(num_points)*1.2)
)

df %>%
  draw_flowers(group, metric, metric, my_curvature = 0.6, my_angle = 130) +
  scale_colour_gradient(low = "orange", high = "pink")
```

<img src="man/figures/README-example-1.png" width="100%" />
