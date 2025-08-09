# bvq 1.0.2

- New grammar questionnaire in Catalan and Spanish (by Mireia Marimon).
- New processing functions for the grammar questionnaires
- New `track_progress` function to show a participant's progress throughout the questionnaire 
- Changes some wording for consistency in the instructions (e.g., in Catalan, "fill/a" and "nado" become "nen/a")
- Minor refactoring in utils.R


# bvq 1.0.1

- New function `launch_app()` that opens the BVQ app (see [https://github.com/gongcastro/bvq-app](https://github.com/gongcastro/bvq-app)) on a browser
- Fixed `n_lemma` score for item `cat_gos` in `pool` (#33)
- Add item `cat_vaixell` to Catalan checklist, which was missing (#32)
- Some minor refactoring for readability

# bvq 1.0.0

## formr implementation

* New formr run named BVQ (`bvq-1.0.0`).
* Remove items about COVID-19 lockdown
* Fix the following item names (according to utils function `fix_item()`):
    - `cat_parc` -> `cat_parc1`
    - `cat_voler` -> `cat_voler1`
    - `cat_voler3` -> `cat_voler2`
    - `cat_despres1` -> `cat_despres`
    - `cat_peix` -> `cat_peix1`
    - `cat_estar` -> `cat_estar1`
    - `cat_anar` -> `cat_anar1`
    - `spa_querer` -> `spa_querer1`
    - `spa_ir` -> `spa_ir1`
* Importing function incorporate this run to the workflow

## New ID scheme:

* `id` is now `child_id`
* `code` is now `response_id`
* Remove any other participant-level identifier (the crossing between `child_id` and `response` is already unambiguous).

## New versioning system

* Subsequent versions of `bvq` will be named using the numeric `0.0.0` format
* Previous version names have been changed in the package too:
    - `BL-Short` -> `bvq-short`
    - `BL-Long` -> `bvq-long`
    - `BL-Lockdown` -> `bvq-lockdown`
* `collect_survey()` now retrieves survey names from new util function `get_bvq_runs()` 
    
## Other changes

* Fixtures have been made smaller in size
* formr surveys are now stored in `inst/formr` for reproducibility
    
# bvq 0.4.0

* Switch most functions to base R. I made this decision in order to learn R base a bit better. Performance differences are negligible, and the user interface remains the same.
* Phonology functions and datasets have been removed, and will be included in a different package.
* `bvq_vocabulary()` now has a better naming system.

# bvq 0.3.2

* Refactor `bvq_vocabulary()` and `bvq_norms()` to use rlang and the dynamic dots (`...`) (#20) operator
* Refactor `bvq_vocabulary()` breaking it up into smaller helper functions
* Update documentation


# bvq 0.3.1

* Refactor `get_time_stamp()` and `get_doe()` to use rlang
* Add examples and tests for `get_time_stamp()` and `get_doe()`
* Add documentation for new version of `bvq_norms()` in its vignette
* Use .covrignore
* Replace `group_by()`/`ungroup()` for experimental argument `.by` in dplyr functions


# bvq 0.3.0

* Add vignettes for `bvq_connect()`, `bvq_responses()`, `bvq_vocabulary()`, and `bvq_norms()`
* Fix citation
* Fix `.by` argument in `bvq_vocabulary()`
* Change base font in site

# bvq 0.2.0

* Package name changes from {bvqdev} to {bvq}
* New phonology functions
* Remove {ipa} dependency
* Tests for the `vowels` and `consonants` datasets
* Now X-SAMPA is used exclusively instead of IPA
* `bvq_vocabulary()` has been refactored (major speed improvement), now column names are shorter, and only "proportion" is set as default `.scale` argument (`scale` in previous versions)
* Removed unused argument `runs` in `import_*()` functions
* Some adjustments for integration in the [r-universe](https://gongcastro.r-universe.dev/bvq)

# bvqdev 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
