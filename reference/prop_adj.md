# Proportion, adjusted for zero- and one-inflation

Proportion, adjusted for zero- and one-inflation

## Usage

``` r
prop_adj(x, n)
```

## Arguments

- x:

  Number of successes

- n:

  Number of tries

## Value

A numeric scalar.

## Details

It is very common that a large proportion of the participants know or do
not know some word. Vocabulary sizes and word prevalence norms in
package are calculated using an estimate that adjusts for zero- and
one-inflation so that, at the population level such estimates are more
likely to be accurate.

## Examples

``` r
prop_adj(4, 60)
#> [1] 0.09375
```
