# Word acquisition norms

The function [`bvq_norms()`](../reference/bvq_norms.md) computes the
proportion of children in the sample that understand or produce each
item, sometimes called **word prevalence**. This function returns the
estimated probability of an average participant understanding or
producing each word. The [`bvq_norms()`](../reference/bvq_norms.md)
function allows to condition this probability on the age, language
profile or language dominance of participants, among other variables.
Proportions are adjusted for zero- and one-inflation following Gelman,
Hill, and Vehtari (2020).

If argument `item` is left NULL (default), proportions are computed for
all items (currently 1,590). This may take time. If you need to compute
norms for specific items, you can provide the item identifiers in the
`item` argument. Available items can be consulted in the `pool` data
set:

``` r
library(bvq)
head(pool)
#> # A tibble: 6 × 14
#>   item           language    te label xsampa n_lemmas is_multiword subtlex_lemma
#>   <chr>          <chr>    <int> <chr> <chr>     <int> <lgl>        <chr>        
#> 1 cat_pessigoll… Catalan      1 (fer… "p@.s…        1 FALSE        pessigolles  
#> 2 cat_abracar    Catalan      2 abra… "@.B4…        1 FALSE        abraçar      
#> 3 cat_obrir      Catalan      3 obrir "u\"B…        1 FALSE        obrir        
#> 4 cat_acabar     Catalan      4 acab… "@.k@…        1 FALSE        acabar       
#> 5 cat_llancar    Catalan      5 llan… "L@n\…        1 FALSE        llançar      
#> 6 cat_apagar     Catalan      6 apag… "@.p@…        1 FALSE        apagar       
#> # ℹ 6 more variables: wordbank_lemma <chr>, childes_lemma <chr>,
#> #   semantic_category <chr>, class <chr>, version <list>, include <lgl>
```

Let’s go through an example of how to use the
[`bvq_norms()`](../reference/bvq_norms.md) function for a specific set
of items.

``` r
# norms will be computed from these datasets
participants <- bvq_participants()
responses <- bvq_responses_vocab(participants = participants)

# items we want to compute norms for
my_items <- c("cat_gos", "cat_gat")

norms <- bvq_norms(
  participants = participants,
  responses = responses,
  item = my_items,
  age = c(12, 35)
)
```

    #> # A tibble: 96 × 9
    #>       te item    label   age type     item_dominance  .sum    .n .prop
    #>    <int> <chr>   <chr> <dbl> <chr>    <chr>          <int> <int> <dbl>
    #>  1   173 cat_gat gat      11 produces L2                 0     1 0.4  
    #>  2   173 cat_gat gat      12 produces L1                 0     1 0.4  
    #>  3   173 cat_gat gat      12 produces L2                 0     3 0.286
    #>  4   173 cat_gat gat      13 produces L1                 0     2 0.333
    #>  5   173 cat_gat gat      14 produces L1                 0     2 0.333
    #>  6   173 cat_gat gat      15 produces L1                 0     1 0.4  
    #>  7   173 cat_gat gat      15 produces L2                 0     1 0.4  
    #>  8   173 cat_gat gat      17 produces L1                 0     1 0.4  
    #>  9   173 cat_gat gat      19 produces L2                 0     1 0.4  
    #> 10   173 cat_gat gat      20 produces L2                 1     2 0.5  
    #> # ℹ 86 more rows

If we want to retrieve the acquisition norms of our item of interest,
but also those of its translation equivalent, we can use the argument
`te`. This argument can take the value `TRUE` if we want to also include
the norms of the translation of the items specified in the `item`
argument. For example:

``` r
my_items <- c("cat_gos", "cat_gat")

bvq_norms(
  participants = participants,
  responses = responses,
  item = my_items,
  te = TRUE,
  age = c(15, 16)
)
#> # A tibble: 16 × 9
#>       te item      label             age type   item_dominance  .sum    .n .prop
#>    <int> <chr>     <chr>           <dbl> <chr>  <chr>          <int> <int> <dbl>
#>  1   173 cat_gat   gat                15 produ… L1                 0     1   0.4
#>  2   173 cat_gat   gat                15 produ… L2                 0     1   0.4
#>  3   173 cat_gat   gat                15 under… L1                 0     1   0.4
#>  4   173 cat_gat   gat                15 under… L2                 0     1   0.4
#>  5   173 spa_gato  gato / gatito      15 produ… L2                 0     1   0.4
#>  6   173 spa_gato  gato / gatito      15 produ… L1                 0     1   0.4
#>  7   173 spa_gato  gato / gatito      15 under… L2                 0     1   0.4
#>  8   173 spa_gato  gato / gatito      15 under… L1                 1     1   0.6
#>  9   195 cat_gos   gos / gosset       15 produ… L1                 0     1   0.4
#> 10   195 cat_gos   gos / gosset       15 produ… L2                 0     1   0.4
#> 11   195 cat_gos   gos / gosset       15 under… L1                 1     1   0.6
#> 12   195 cat_gos   gos / gosset       15 under… L2                 0     1   0.4
#> 13   195 spa_perro perro / perrito    15 produ… L2                 0     1   0.4
#> 14   195 spa_perro perro / perrito    15 produ… L1                 0     1   0.4
#> 15   195 spa_perro perro / perrito    15 under… L2                 0     1   0.4
#> 16   195 spa_perro perro / perrito    15 under… L1                 1     1   0.6
```

We can even indicate a specific translation equivalent in the `te`
argument to retrieve its norms, while leaving the argument `item` blank:

``` r
bvq_norms(
  participants = participants,
  responses = responses,
  te = 175,
  age = c(25, 29)
)
#> # A tibble: 24 × 9
#>       te item    label   age type        item_dominance  .sum    .n .prop
#>    <int> <chr>   <chr> <dbl> <chr>       <chr>          <int> <int> <dbl>
#>  1   175 cat_cuc cuc      25 produces    L1                 1     2   0.5
#>  2   175 cat_cuc cuc      26 produces    L1                 1     1   0.6
#>  3   175 cat_cuc cuc      26 produces    L2                 0     1   0.4
#>  4   175 cat_cuc cuc      27 produces    L1                 0     1   0.4
#>  5   175 cat_cuc cuc      29 produces    L1                 1     1   0.6
#>  6   175 cat_cuc cuc      29 produces    L2                 0     1   0.4
#>  7   175 cat_cuc cuc      25 understands L1                 1     2   0.5
#>  8   175 cat_cuc cuc      26 understands L1                 1     1   0.6
#>  9   175 cat_cuc cuc      26 understands L2                 0     1   0.4
#> 10   175 cat_cuc cuc      27 understands L1                 1     1   0.6
#> # ℹ 14 more rows
```

We may be interested in computing the acquisition norms of some items of
interest, while preserving some participant-level or item-level
characteristics. For instance, we can take a look at the acquisition
norms for the item “cat_casa” for monolinguals and bilinguals
separately. We can do this by adding the unquoted variable names of
interest to the arguments (`lp`, in this case):

``` r
bvq_norms(
  participants = participants,
  responses = responses,
  item = "cat_casa",
  age = c(22, 22),
  lp
)
#> # A tibble: 2 × 10
#>      te item     label   age type        item_dominance lp      .sum    .n .prop
#>   <int> <chr>    <chr> <dbl> <chr>       <chr>          <chr>  <int> <int> <dbl>
#> 1   591 cat_casa casa     22 produces    L1             Monol…     1     1   0.6
#> 2   591 cat_casa casa     22 understands L1             Monol…     1     1   0.6
```

To can get acquisition norms for the combination of multiple variables
of interest. For instance, we can get the acquisition norms above,
disaggregated by `lp` and `dominance`:

``` r
bvq_norms(
  participants = participants,
  responses = responses,
  item = "cat_casa",
  age = c(22, 22),
  lp,
  dominance
)
#> # A tibble: 2 × 11
#>      te item  label   age type  item_dominance lp    dominance  .sum    .n .prop
#>   <int> <chr> <chr> <dbl> <chr> <chr>          <chr> <chr>     <int> <int> <dbl>
#> 1   591 cat_… casa     22 prod… L1             Mono… Catalan       1     1   0.6
#> 2   591 cat_… casa     22 unde… L1             Mono… Catalan       1     1   0.6
```
