# Introduction to bvq

The function `bvq_responses()` retrieves participants’ responses to the
Barcelona Vocabulary Questionnaire (BVQ) using the [formr
API](https://formr.org/documentation), and returns them along
participant- and item-level information. This function returns a tidy
data frame in which each row is one participant’s response to an
individual item.

``` r
library(bvq)
library(dplyr)

participants <- bvq_participants()
responses <- bvq_responses(participants)

# select relevant variables
responses %>%
    select(id, time, item, response, randomisation) %>%
    filter(!is.na(response)) # drop unanswered items
```

    ## # A tibble: 34,465 × 16
    ##    child_id response_id  time version      version_list date_birth date_started
    ##    <chr>    <chr>       <dbl> <chr>        <chr>        <date>     <date>      
    ##  1 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    ##  2 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    ##  3 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    ##  4 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    ##  5 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    ##  6 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    ##  7 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    ##  8 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    ##  9 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    ## 10 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    ## # ℹ 34,455 more rows
    ## # ℹ 9 more variables: date_finished <date>, item <chr>, response <int>,
    ## #   sex <chr>, doe_catalan <dbl>, doe_spanish <dbl>, doe_others <dbl>,
    ## #   edu_parent1 <chr>, edu_parent2 <chr>

## Consulting participant-level information

Participant-level properties like language profile variables can be
extracted using the [`bvq_logs()`](../reference/bvq_logs.md) function.

``` r
bvq_logs(participants, responses) %>% 
    select(child_id, time, age, lp, starts_with("doe_"))
```

    ## # A tibble: 38 × 7
    ##    child_id  time   age lp          doe_spanish doe_catalan doe_others
    ##    <chr>    <dbl> <dbl> <chr>             <dbl>       <dbl>      <dbl>
    ##  1 58298        2  29.4 Monolingual        0           1           0  
    ##  2 58361        1  24.4 Bilingual          0.75        0.25        0  
    ##  3 58298        1  24.9 Monolingual        0.1         0.9         0  
    ##  4 58068        2  26.2 Bilingual          0.25        0.75        0  
    ##  5 57177        4  30.6 Bilingual          0.2         0.7         0.1
    ##  6 56911        4  32.2 Monolingual        0.15        0.85        0  
    ##  7 57436        4  30.8 Bilingual          0.35        0.65        0  
    ##  8 57534        1  21.5 Monolingual        0.05        0.95        0  
    ##  9 57436        3  25.5 Bilingual          0.4         0.6         0  
    ## 10 57336        1  21.2 Bilingual          0.4         0.6         0  
    ## # ℹ 28 more rows

## Item properties

Item-level properties can be consulted in the `pool` data frame (see
[`?pool`](../reference/pool.md)):

``` r
data("pool")
pool
```

    ## # A tibble: 1,590 × 14
    ##    item          language    te label xsampa n_lemmas is_multiword subtlex_lemma
    ##    <chr>         <chr>    <int> <chr> <chr>     <int> <lgl>        <chr>        
    ##  1 cat_pessigol… Catalan      1 (fer… "p@.s…        1 FALSE        pessigolles  
    ##  2 cat_abracar   Catalan      2 abra… "@.B4…        1 FALSE        abraçar      
    ##  3 cat_obrir     Catalan      3 obrir "u\"B…        1 FALSE        obrir        
    ##  4 cat_acabar    Catalan      4 acab… "@.k@…        1 FALSE        acabar       
    ##  5 cat_llancar   Catalan      5 llan… "L@n\…        1 FALSE        llançar      
    ##  6 cat_apagar    Catalan      6 apag… "@.p@…        1 FALSE        apagar       
    ##  7 cat_aprendre  Catalan      7 apre… "@\"p…        1 FALSE        aprendre     
    ##  8 cat_esgarrap… Catalan      8 esga… "@z.g…        1 FALSE        esgarrapar   
    ##  9 cat_ajudar    Catalan      9 ajud… "@.Zu…        1 FALSE        ajudar       
    ## 10 cat_ballar    Catalan     10 ball… "b@\"…        1 FALSE        ballar       
    ## # ℹ 1,580 more rows
    ## # ℹ 6 more variables: wordbank_lemma <chr>, childes_lemma <chr>,
    ## #   semantic_category <chr>, class <chr>, version <list>, include <lgl>

## Computing vocabulary sizes

The [`bvq_vocabulary()`](../reference/bvq_vocabulary.md) function allows
to extract vocabulary sizes for individual responses to any of the
questionnaires. It takes the output of the `bvq_responses()` function as
an argument, and returns several measures of vocabulary size base on
such data frame.

To compute vocabulary size, we first need to run `bvq_responses()`
(although if this argument is not provided, `bvq_responses()` is run
under the hood):

``` r
bvq_vocabulary(participants, 
               responses,
               lp, # to keep participants' language profile
               .scale = "prop" # to return estimates as proportions
)
```

    ## # A tibble: 76 × 10
    ##    child_id response_id type       lp    total_prop l1_prop l2_prop concept_prop
    ##    <chr>    <chr>       <chr>      <chr>      <dbl>   <dbl>   <dbl>        <dbl>
    ##  1 58298    BL1879      understan… Mono…    0.518    0.924   0.141        0.881 
    ##  2 58298    BL1879      produces   Mono…    0.430    0.784   0.103        0.743 
    ##  3 58361    BL1863      understan… Bili…    0.703    0.805   0.601        0.826 
    ##  4 58361    BL1863      produces   Bili…    0.476    0.573   0.379        0.617 
    ##  5 58298    BL1848      understan… Mono…    0.370    0.702   0.0623       0.662 
    ##  6 58298    BL1848      produces   Mono…    0.00985  0.0205  0            0.0189
    ##  7 58068    BL1833      understan… Bili…    0.737    0.850   0.633        0.846 
    ##  8 58068    BL1833      produces   Bili…    0.328    0.528   0.146        0.526 
    ##  9 57177    BL1748      understan… Bili…    0.837    0.814   0.857        0.887 
    ## 10 57177    BL1748      produces   Bili…    0.568    0.678   0.466        0.757 
    ## # ℹ 66 more rows
    ## # ℹ 2 more variables: te_prop <dbl>, contents <list>

## Computing word acquisition norms

The function [`bvq::bvq_norms()`](../reference/bvq_norms.md) computes
the proportion of children in the sample that understand or produce each
item, sometimes called **word prevalence**. This function returns the
estimated probability of an average participant understanding or
producing each word. The [`bvq_norms()`](../reference/bvq_norms.md)
function allows to condition this probability on the age, language
profile or language dominance of participants, among other variables.
Proportions are adjusted for zero- and one-inflation following Gelman,
Hill, and Vehtari (2020).

``` r
# items we want to compute norms for
bvq_norms(participants,
          responses,
          item = c("cat_gos", "cat_gat"),
          age = c(12, 35)
)
```

    ## # A tibble: 96 × 9
    ##       te item    label   age type     item_dominance  .sum    .n .prop
    ##    <int> <chr>   <chr> <dbl> <chr>    <chr>          <int> <int> <dbl>
    ##  1   173 cat_gat gat      12 produces L1                 0     1 0.4  
    ##  2   173 cat_gat gat      12 produces L2                 0     3 0.286
    ##  3   173 cat_gat gat      13 produces L1                 0     1 0.4  
    ##  4   173 cat_gat gat      13 produces L2                 0     1 0.4  
    ##  5   173 cat_gat gat      14 produces L2                 0     1 0.4  
    ##  6   173 cat_gat gat      14 produces L1                 0     1 0.4  
    ##  7   173 cat_gat gat      15 produces L1                 0     1 0.4  
    ##  8   173 cat_gat gat      15 produces L2                 0     1 0.4  
    ##  9   173 cat_gat gat      17 produces L1                 0     1 0.4  
    ## 10   173 cat_gat gat      19 produces L2                 0     1 0.4  
    ## # ℹ 86 more rows
