# Get items included in a questionnaire

This function retrieves information about the items in a particular
section in the BVQ questionnaire. This includes item names, item types,
text, choices, settings, and other metadata.

## Usage

``` r
bvq_items(section, version = "bvq-1.0.0")
```

## Arguments

- section:

  Name of the questionnaire section to retrieve items for. Check the
  output of [`get_bvq_runs()`](get_bvq_runs.md) to see the available
  sections for each version of the questionnaire.

- version:

  Name of the version of the questionnaire for which the items of a
  section will be retrieved. Check the output of `names(get_bvq_runs())`
  to see the available versions of the questionnaire.

## Value

A list of length 3, which includes:

- survey: A
  [tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)
  containing the items included in the questionnaire and several
  properties. Each row corresponds to a single item, and each column
  corresponds to a particular property:

  - type: a character string indicating the type of the item (see [formr
    documentation](https://formr.org/documentation#available_items)).

  - name: a character string indicating the name of the item, as it
    appears in the output of
    [`bvq_responses_vocab()`](bvq_responses_vocab.md).

  - label: a character string indicating the text shown to participants
    when filling out the questionnaire.

  - optional: a logical value indicating whether providing an answer to
    the item is mandatory for participants.

  - class: a character string indicating the CSS class with of the item.

  - showif: a character string indicating R code that determines under
    what conditions the item is shown to participants.

  - value: default value of the item.

  - block_order: character string (a latter) indicating the order in
    which the block that the item belongs to appears in the survey.

  - item_order: integer indicating the order in which the item appears
    within the block it belongs to belongs.

- choices: A
  [tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)
  containing the choices given to participants for some items. Each row
  corresponds to a choice, and each column corresponds to a particular
  choice property:

  - list_name\`: character string indicating the name of the name of the
    choice list (which may repeat across different items).

  - name: character string indicating the name that a particular choice
    will be assigned in the code.

  - label: character string indicating the text that will be show to
    participants for a particular choice.

- settings: A
  [tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)
  containing the settings for the survey. Each row corresponds to one
  setting, and each column indicates the setting names and values:

  - item: name of the setting.

  - value: value of the setting.

## Author

Gonzalo Garcia-Castro

## Examples

``` r
if (FALSE) { # \dontrun{
bvq_items("bvq_06_words_catalan", version = "bvq-1.0.0")
} # }
```
