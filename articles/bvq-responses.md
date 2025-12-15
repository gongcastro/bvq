# Retrieving raw data

``` r
library(bvq)
library(tidyr)
library(dplyr)
```

The function
[`bvq_responses_vocab()`](../reference/bvq_responses_vocab.md) retrieves
participants’ responses to the Barcelona Vocabulary Questionnaire (BVQ)
using the [formr API](https://formr.org/documentation), and returns them
along participant- and item-level information. This function returns a
tidy data frame in which each row is one participant’s response to an
individual item.

``` r
responses <- bvq_responses_vocab()

# select relevant variables
responses %>%
  select(child_id, time, item, response, version_list ) %>%
  drop_na(response) # drop unanswered items
```

    #> # A tibble: 34,465 × 16
    #>    child_id response_id  time version      version_list date_birth date_started
    #>    <chr>    <chr>       <dbl> <chr>        <chr>        <date>     <date>      
    #>  1 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    #>  2 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    #>  3 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    #>  4 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    #>  5 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    #>  6 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    #>  7 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    #>  8 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    #>  9 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    #> 10 58298    BL1879          2 bvq-lockdown C            2020-04-27 2022-10-08  
    #> # ℹ 34,455 more rows
    #> # ℹ 9 more variables: date_finished <date>, item <chr>, response <int>,
    #> #   sex <chr>, doe_catalan <dbl>, doe_spanish <dbl>, doe_others <dbl>,
    #> #   edu_parent1 <chr>, edu_parent2 <chr>

This dataset is the base for many analyses of interest, like
participants’ vocabulary size, word prevalence, or modelling item-level
probability of acquisition. This package offers several functions to do
this (e.g., [`bvq_vocabulary()`](../reference/bvq_vocabulary.md),
[`bvq_norms()`](../reference/bvq_norms.md)), but one could already do
this from this dataset.

Additional participant-level properties like language profile variables
can be extracted using the [`bvq_logs()`](../reference/bvq_logs.md)
function.

``` r
logs <- bvq_logs(responses = responses)

logs %>%
  select(
    child_id, time, age, lp,
    starts_with("doe_")
  )
```

Item-level properties can be consulted in the `pool` dataset (See
?pool):

``` r
data("pool")
pool
#> # A tibble: 1,590 × 14
#>    item          language    te label xsampa n_lemmas is_multiword subtlex_lemma
#>    <chr>         <chr>    <int> <chr> <chr>     <int> <lgl>        <chr>        
#>  1 cat_pessigol… Catalan      1 (fer… "p@.s…        1 FALSE        pessigolles  
#>  2 cat_abracar   Catalan      2 abra… "@.B4…        1 FALSE        abraçar      
#>  3 cat_obrir     Catalan      3 obrir "u\"B…        1 FALSE        obrir        
#>  4 cat_acabar    Catalan      4 acab… "@.k@…        1 FALSE        acabar       
#>  5 cat_llancar   Catalan      5 llan… "L@n\…        1 FALSE        llançar      
#>  6 cat_apagar    Catalan      6 apag… "@.p@…        1 FALSE        apagar       
#>  7 cat_aprendre  Catalan      7 apre… "@\"p…        1 FALSE        aprendre     
#>  8 cat_esgarrap… Catalan      8 esga… "@z.g…        1 FALSE        esgarrapar   
#>  9 cat_ajudar    Catalan      9 ajud… "@.Zu…        1 FALSE        ajudar       
#> 10 cat_ballar    Catalan     10 ball… "b@\"…        1 FALSE        ballar       
#> # ℹ 1,580 more rows
#> # ℹ 6 more variables: wordbank_lemma <chr>, childes_lemma <chr>,
#> #   semantic_category <chr>, class <chr>, version <list>, include <lgl>
```

## Longitudinal responses

Several participants have filled the questionnaire more than once. All
questionnaire responses included in any dataset returned by any function
in BVQ have an associated `time` value. This variable indexes how many
times that specific participant has filled the questionnaire (any
version), including their last response. This allows to track each
participant’s responses across time and perform longitudinal analyses.

By default,
[`bvq_responses_vocab()`](../reference/bvq_responses_vocab.md) retrieves
all responses. Using
[`get_longitudinal()`](../reference/get_longitudinal.md), we can filter
what cases we want to keep. The argument `longitudinal` takes one of the
following character strings:

- `"all"`: all responses are returned
- `"no"`: participants with more than one response to the questionnaire
  (any version) are excluded from the output
- `"first"`: only the first response from each participant is returned
  (including responses of participant that only responded once)
- `"last"`: only the most recent response from each participant is
  returned (including responses of participant that only responded once)
- `"only"`: only responses from participants that filled the
  questionnaire more than once are returned.

Setting `longitudinal = "only"` is especially useful to perform repeated
measures analyses. For example:

    #> # A tibble: 16,077 × 8
    #>    child_id  time item           response version_list date_birth date_started
    #>    <chr>    <dbl> <chr>             <int> <chr>        <date>     <date>      
    #>  1 58298        2 cat_astronauta        3 C            2020-04-27 2022-10-08  
    #>  2 58298        2 cat_abella            3 C            2020-04-27 2022-10-08  
    #>  3 58298        2 cat_ales              3 C            2020-04-27 2022-10-08  
    #>  4 58298        2 cat_capo              1 C            2020-04-27 2022-10-08  
    #>  5 58298        2 cat_com               3 C            2020-04-27 2022-10-08  
    #>  6 58298        2 cat_ara               3 C            2020-04-27 2022-10-08  
    #>  7 58298        2 cat_anar2             3 C            2020-04-27 2022-10-08  
    #>  8 58298        2 cat_blanc             3 C            2020-04-27 2022-10-08  
    #>  9 58298        2 cat_i                 3 C            2020-04-27 2022-10-08  
    #> 10 58298        2 cat_aigua1            3 C            2020-04-27 2022-10-08  
    #> # ℹ 16,067 more rows
    #> # ℹ 1 more variable: date_finished <date>

### Extracting responses to grammar questionnaires

The homologous funnction to retrieve responses to grammar questionnaires
i [`bvq_responses_grammar()`](../reference/bvq_responses_grammar.md).
This function returns a datatable with each row corresponding to one
participants’s response to each item in the grammar questionnaire (long
format).

``` r
responses_grammar <- bvq_responses_grammar(participants)

responses_grammar %>%
  select(child_id, time, item, response, version, version_list) %>%
  drop_na(response) # drop unanswered items
```

### Please note

The values of `time` in the outcome of
[`bvq_participants()`](../reference/bvq_participants.md) and the outcome
of the rest of the functions may *not* be identical. This is because in
[`bvq_participants()`](../reference/bvq_participants.md) this value
increases in one unit every time a given participant is sent the
questionnaire, even if they do not end up filling it. In contrast, the
value of `time` in the rest of the functions (e.g.,
[`bvq_responses_vocab()`](../reference/bvq_responses_vocab.md),
[`bvq_logs()`](../reference/bvq_logs.md)) only increases when the
questionnaire is filled. Since the outcome of
[`bvq_participants()`](../reference/bvq_participants.md) is mainly
intended for internal use, you don’t have to worry about this as long as
you don’t try to cross the outcomes of
[`bvq_participants()`](../reference/bvq_participants.md) and the rest of
the functions.
