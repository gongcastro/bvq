# Vocabulary sizes

The [`bvq_vocabulary()`](../reference/bvq_vocabulary.md) function allows
to extract vocabulary sizes for individual responses to any of the
questionnaires. It takes the output of the
[`bvq_responses_vocab()`](../reference/bvq_responses_vocab.md) function
as an argument, and returns several measures of vocabulary size base on
such dataset.

To compute vocabulary size, we first need to run
[`bvq_responses_vocab()`](../reference/bvq_responses_vocab.md) (although
if this argument is not provided,
[`bvq_responses_vocab()`](../reference/bvq_responses_vocab.md) is run
under the hood):

``` r
library(bvq)

# vocabularies will be computed from these datasets
participants <- bvq_participants()
responses <- bvq_responses_vocab(participants = participants)

bvq_vocabulary(participants, responses)
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

The [`bvq_vocabulary()`](../reference/bvq_vocabulary.md) computes four
measures of vocabulary size.

- **Total** (`total_*`): total number of item the child was reported to
  know, summing both languages together.
- **L1** (`l1_*`): number of word the child was reported to know in
  their dominant language (e.g., Catalan words for a child whose
  language of most exposure is Catalan).
- **L2** (`l2_*`): number of word the child was reported to know in
  their non-dominant language (e.g., Spanish words for a child whose
  language of most exposure is Catalan)
- **Conceptual** (`concept_*`): number of concepts the child know at
  least one word for, regardless of the language the word belongs to.
- **TE** (`te_*`): number of translation equivalents the child knows,
  i.e., or how many concepts the child know one word in each language
  for.

Vocabulary sizes are, by default, computed in two different scales:

- **Proportion** (`*_prop`): proportion of the items the child was
  reported to known, from the total of items that were included in the
  questionnaire, and caregivers answered to.
- **Counts** (`*_count`): sum of the total number of items the child was
  reported to know.

The scale returned by
[`bvq_vocabulary()`](../reference/bvq_vocabulary.md) can be modified
with the `.scale` argument, which takes `"prop"` for proportions
(default), and `"count"` for counts. Both can be computed using
`.scale = c("prop", "count")`. For instance we can get vocabulary sizes
as proportions running:

    #> # A tibble: 76 × 9
    #>    child_id response_id type     total_prop l1_prop l2_prop concept_prop te_prop
    #>    <chr>    <chr>       <chr>         <dbl>   <dbl>   <dbl>        <dbl>   <dbl>
    #>  1 58298    BL1879      underst…    0.518    0.924   0.141        0.881   0.114 
    #>  2 58298    BL1879      produces    0.430    0.784   0.103        0.743   0.0838
    #>  3 58361    BL1863      underst…    0.703    0.805   0.601        0.826   0.574 
    #>  4 58361    BL1863      produces    0.476    0.573   0.379        0.617   0.331 
    #>  5 58298    BL1848      underst…    0.370    0.702   0.0623       0.662   0.0486
    #>  6 58298    BL1848      produces    0.00985  0.0205  0            0.0189  0     
    #>  7 58068    BL1833      underst…    0.737    0.850   0.633        0.846   0.563 
    #>  8 58068    BL1833      produces    0.328    0.528   0.146        0.526   0.102 
    #>  9 57177    BL1748      underst…    0.837    0.814   0.857        0.887   0.714 
    #> 10 57177    BL1748      produces    0.568    0.678   0.466        0.757   0.329 
    #> # ℹ 66 more rows
    #> # ℹ 1 more variable: contents <list>

To get vocabulary sizes as counts, we can run this instead:

    #> # A tibble: 76 × 9
    #>    child_id response_id type        total_count l1_count l2_count concept_count
    #>    <chr>    <chr>       <chr>             <int>    <int>    <int>         <int>
    #>  1 58298    BL1879      understands         368      316       52           326
    #>  2 58298    BL1879      produces            306      268       38           275
    #>  3 58361    BL1863      understands         490      281      209           289
    #>  4 58361    BL1863      produces            332      200      132           216
    #>  5 58298    BL1848      understands         263      240       23           245
    #>  6 58298    BL1848      produces              7        7        0             7
    #>  7 58068    BL1833      understands         523      288      235           314
    #>  8 58068    BL1833      produces            233      179       54           195
    #>  9 57177    BL1748      understands         594      276      318           329
    #> 10 57177    BL1748      produces            403      230      173           281
    #> # ℹ 66 more rows
    #> # ℹ 2 more variables: te_count <int>, contents <list>

Finally, two types of vocabulary sizes are computed:

- **Comprehension** (`understands`): number of items the child
  understands.
- **Production**: (`produces`) number of items the child says.

These two measures are returned in the long format under the `type`
column.

## Vocabulary contents

In additional to the vocabulary size scores,
[`bvq_vocabulary()`](../reference/bvq_vocabulary.md) also returns the
column `contents`. This column is a list containing the items marked as
acquired for comprehension or production. For instance:

## Conditional vocabulary size: the `...` extra arguments

We can also compute vocabulary sizes conditional to some variables at
the item or participant level, such as semantic/functional
(`semantic_category`) or language profile (`lp`), using the argument
`...` argument. Just take a look at the variables included in the data
frame returned by [`bvq_logs()`](../reference/bvq_logs.md) or in the
`pool` dataset. For each participant, vocabulary sizes are computed for
each level or combination of levels of the variables included in the
columns included in `...`. You can use this argument to preserve
participant-level information in the output data frame. For instance, we
can keep information about the language profile (`lp`) of the
participant:

``` r
bvq_vocabulary(participants, responses, lp)
```

We can also can also preserve information about the items, like the
semantic/functional category of the words (`semantic_category`). In this
case, the vocabulary sizes will be computed for each level of the
`semantic_category` variable:

``` r
bvq_vocabulary(participants, responses, semantic_category)
#> # A tibble: 1,970 × 10
#>    child_id response_id type        semantic_category total_prop l1_prop l2_prop
#>    <chr>    <chr>       <chr>       <chr>                  <dbl>   <dbl>   <dbl>
#>  1 58298    BL1879      understands Adventures             0.75    1      0.5   
#>  2 58298    BL1879      produces    Adventures             0.7     0.9    0.5   
#>  3 58298    BL1879      understands Animals                0.516   0.968  0.0645
#>  4 58298    BL1879      produces    Animals                0.452   0.839  0.0645
#>  5 58298    BL1879      understands Parts of animals       0.409   0.818  0     
#>  6 58298    BL1879      produces    Parts of animals       0.136   0.273  0     
#>  7 58298    BL1879      understands Parts of things        0.375   0.75   0     
#>  8 58298    BL1879      produces    Parts of things        0.125   0.25   0     
#>  9 58298    BL1879      understands Question words         0.5     1      0     
#> 10 58298    BL1879      produces    Question words         0.5     1      0     
#> # ℹ 1,960 more rows
#> # ℹ 3 more variables: concept_prop <dbl>, te_prop <dbl>, contents <list>
```

Finally, we can preserve more than one variable, including combinations
of participant-level and item-level variables, such as language profile
(`lp`), age (`age`) and grammatical class (`class`):

``` r
bvq_vocabulary(participants, responses, age, lp, semantic_category)
#> # A tibble: 1,970 × 12
#>    child_id response_id type      age lp    semantic_category total_prop l1_prop
#>    <chr>    <chr>       <chr>   <dbl> <chr> <chr>                  <dbl>   <dbl>
#>  1 58298    BL1879      unders…  29.4 Mono… Adventures             0.75    1    
#>  2 58298    BL1879      produc…  29.4 Mono… Adventures             0.7     0.9  
#>  3 58298    BL1879      unders…  29.4 Mono… Animals                0.516   0.968
#>  4 58298    BL1879      produc…  29.4 Mono… Animals                0.452   0.839
#>  5 58298    BL1879      unders…  29.4 Mono… Parts of animals       0.409   0.818
#>  6 58298    BL1879      produc…  29.4 Mono… Parts of animals       0.136   0.273
#>  7 58298    BL1879      unders…  29.4 Mono… Parts of things        0.375   0.75 
#>  8 58298    BL1879      produc…  29.4 Mono… Parts of things        0.125   0.25 
#>  9 58298    BL1879      unders…  29.4 Mono… Question words         0.5     1    
#> 10 58298    BL1879      produc…  29.4 Mono… Question words         0.5     1    
#> # ℹ 1,960 more rows
#> # ℹ 4 more variables: l2_prop <dbl>, concept_prop <dbl>, te_prop <dbl>,
#> #   contents <list>
```
