# bvq <a href="https://gongcastro.github.io/bvq"><img src="man/figures/logo.png" align="right" height="139" /></a>
<!-- badges: start -->
[![R-CMD-check](https://github.com/gongcastro/bvq/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/gongcastro/bvq/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/gongcastro/bvq/branch/main/graph/badge.svg)](https://app.codecov.io/gh/gongcastro/bvq?branch=main)
![r-universe](https://gongcastro.r-universe.dev/badges/bvq)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![pkgcheck](https://github.com/gongcastro/bvq/workflows/pkgcheck/badge.svg)](https://github.com/<org>/<repo>/actions?query=workflow%3Apkgcheck)
<!-- badges: end -->

## Overview

This package contains data from a Barcelona Vocabulary Questionnaire (BVQ), a vocabulary checklist aimed at to assessing lexical development in toddlers growing up in Catalan-Spanish bilingual environments. The questionnaire was deployed using [formr](https://formr.org/), and the materials used to generate the items are available in the GitHub repository.

This package also contains multiple functions that extract the data and generate many metrics used frequently in the language acquisition literature to examine participants’ vocabulary or to consult acquisition norms of particular words. Documentation and working examples are available in the [package website](https://gongcastro.github.io/bvq).


## Installation

From [r-universe](https://gongcastro.r-universe.dev/bvq) :rocket::

```r
install.packages('bvq', repos = 'https://gongcastro.r-universe.dev')
```

From the GitHub repository:

```r
install.packages("remotes") # you may need to install remotes first
remotes::install_github("gongcastro/bvq")
```

## Data accesibility

BVQ is currently a developing package for internal use at the [Center for Brain and Cognititon](https://www.upf.edu/web/cbc) (CBC). For this reason, logging in requires a password that can be required by writing to the maintainer of this package ([gongarciacastro@gmail.com](mailto:gongarciacastro@gmail.com)).

## Code of Conduct

Please note that the bvq project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
