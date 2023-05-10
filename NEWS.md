# bvq 0.3.1

* Refactor `get_time_stamp()` and `get_doe()` to use tidyselect
* Add examples and tests for `get_time_stamp()` and `get_doe()`
* Add documentation for new version of `bvq_norms()` in its vignette
* Use .covrignore

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
