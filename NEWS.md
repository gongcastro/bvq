# bvq 0.4.1

* Fix buggy `case_when()` in `bvq_logs()`. Apparently the order of statements goes the other way around. This was resulting in the misclassification of ~9 participants' `lp` as not "Other". I moved the evaluation of "Other" `lp` to the top.
* Added additional checks for the `bilingual_threshold` and `other_threshold` arguments in `bvq_logs()`.
* Added more severe unit tests for `bvq_logs()` (mainly doe the `doe_*` variables.
* Added some comments to `bvq_vocabulary()`.

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
