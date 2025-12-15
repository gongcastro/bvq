# Package index

## Retrieving raw data

Functions for downloading the basic datasets needed to compute
vocabulary norms.

- [`bvq_connect()`](bvq_connect.md) : Authenticate in Google and formr
- [`bvq_participants()`](bvq_participants.md) : Retrieve and update
  local and/or remote data from formr
- [`bvq_responses_vocab()`](bvq_responses_vocab.md) : Retrieve and
  update local and/or remote data from formr
- [`bvq_responses_grammar()`](bvq_responses_grammar.md) : Retrieve and
  update local and/or remote data from formr
- [`pool`](pool.md) : Pool of words

## Computing vocabulary norms

Functions for computing participant information, vocabulary size, and
word acquisition norms.

- [`bvq_logs()`](bvq_logs.md) : Generate participant information and
  progress for each response
- [`bvq_vocabulary()`](bvq_vocabulary.md) : Generate participant
  information and progress for each response
- [`bvq_norms()`](bvq_norms.md) : Generate item-level norms for age,
  sex, language profile and item dominance

## Items and questionnaires

Information about questionnaire sections and items

- [`bvq_items()`](bvq_items.md) : Get items included in a questionnaire
- [`get_bvq_runs()`](get_bvq_runs.md) : Get BVQ formr runs

## Helper functions

Additional functions to access and process the data

- [`track_progress()`](track_progress.md) : Track a participant's
  response progress.
- [`get_longitudinal()`](get_longitudinal.md) : Deal with repeated
  measures
- [`prop_adj()`](prop_adj.md) : Proportion, adjusted for zero- and
  one-inflation
- [`launch_app()`](launch_app.md) : Launch bvq Shiny App in a browser

## Internals

Helper functions used internally

- [`get_doe()`](get_doe.md) : Summarise language profile
