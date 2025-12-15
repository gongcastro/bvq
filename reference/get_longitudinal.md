# Deal with repeated measures

Deal with repeated measures

## Usage

``` r
get_longitudinal(x, longitudinal = "all")
```

## Arguments

- x:

  A data frame containing a column for participants (each participant
  gets a unique ID), and a column for times (a numeric value indicating
  how many times each participant appears in the data frame counting
  this one). One participant may appear several times in the data frame,
  with each time with a unique value of `time`.

- longitudinal:

  A character string indicating what subset of the participants should
  be returned:

  - `"all"` (default) returns all participants.

  - `"no"` remove all participants with more than one response.

  - `"only"` returns only participants with more than one response in
    the dataset (i.e., longitudinal participants).

  - `"first"` returns the first response of each participant
    (participants with only one appearance are included).

  - `"last"` returns the last response from each participant
    (participants with only one response are included).

## Value

A subset of the data frame `x` with only the selected cases, according
to `longitudinal`.

## Author

Gonzalo Garcia-Castro

## Examples

``` r
child_id <- c(1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 5, 6, 7, 7, 8, 9, 10, 10)
sums <- rle(sort(child_id))[["lengths"]]
dat <- data.frame(child_id, time = unlist(sapply(sums, function(x) seq(1, x))))

(dat)
#>    child_id time
#> 1         1    1
#> 2         1    2
#> 3         1    3
#> 4         2    1
#> 5         2    2
#> 6         3    1
#> 7         4    1
#> 8         4    2
#> 9         4    3
#> 10        4    4
#> 11        5    1
#> 12        6    1
#> 13        7    1
#> 14        7    2
#> 15        8    1
#> 16        9    1
#> 17       10    1
#> 18       10    2

get_longitudinal(dat, "first")
#>    child_id time
#> 1         1    1
#> 2         2    1
#> 3         3    1
#> 4         4    1
#> 5         5    1
#> 6         6    1
#> 7         7    1
#> 8         8    1
#> 9         9    1
#> 10       10    1
get_longitudinal(dat, "only")
#>    child_id time
#> 1         1    1
#> 2         1    2
#> 3         1    3
#> 4         2    1
#> 5         2    2
#> 6         4    1
#> 7         4    2
#> 8         4    3
#> 9         4    4
#> 10        7    1
#> 11        7    2
#> 12       10    1
#> 13       10    2
```
