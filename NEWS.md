# bvq 1.0.0

* Remove items about COVID-19 lockdown
* Fix the following item names (according to utilx function `fix_item()`):
    - `cat_parc` -> `cat_parc1`
    - `cat_voler` -> `cat_voler1`
    - `cat_voler3` -> `cat_voler2`
    - `cat_despres1` -> `cat_despres`
    - `cat_peix` -> `cat_peix1`
    - `cat_estar` -> `cat_estar1`
    - `cat_anar` -> `cat_anar1`
    - `spa_querer` -> `spa_querer1`
    - `spa_ir` -> `spa_ir1`
    
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
