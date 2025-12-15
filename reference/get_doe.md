# Summarise language profile

Summarise language profile

## Usage

``` r
get_doe(...)
```

## Arguments

- ...:

  Columns with the degree of exposures to be summed up for (all others
  will be considered as `doe_others`).

## Value

A numeric vector with the row-wise sums of the columns specified in
`...`.

## Author

Gonzalo Garcia-Castro

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
x <- data.frame(
  doe_cat_1 = seq(0, 1, 0.1),
  doe_cat_2 = c(0, rep(c(0.1, 0), each = 5)),
  doe_spa_1 = c(0, rep(c(0.1, 0), each = 5)),
  doe_spa_2 = c(1, 0.7, 0.6, 0.5, 0.3, 0.1, 0.4, 0.3, 0.2, 0.1, 0)
)

y <- mutate(x,
  doe_other = 1 - get_doe(matches("cat|spa")),
  doe_cat = get_doe(doe_cat_1, doe_cat_2),
  doe_spa = get_doe(matches("spa"))
)

(y)
#>    doe_cat_1 doe_cat_2 doe_spa_1 doe_spa_2 doe_other doe_cat doe_spa
#> 1        0.0       0.0       0.0       1.0       0.0     0.0     1.0
#> 2        0.1       0.1       0.1       0.7       0.0     0.2     0.8
#> 3        0.2       0.1       0.1       0.6       0.0     0.3     0.7
#> 4        0.3       0.1       0.1       0.5       0.0     0.4     0.6
#> 5        0.4       0.1       0.1       0.3       0.1     0.5     0.4
#> 6        0.5       0.1       0.1       0.1       0.2     0.6     0.2
#> 7        0.6       0.0       0.0       0.4       0.0     0.6     0.4
#> 8        0.7       0.0       0.0       0.3       0.0     0.7     0.3
#> 9        0.8       0.0       0.0       0.2       0.0     0.8     0.2
#> 10       0.9       0.0       0.0       0.1       0.0     0.9     0.1
#> 11       1.0       0.0       0.0       0.0       0.0     1.0     0.0
```
