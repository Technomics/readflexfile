
<!-- README.md is generated from README.Rmd. Please edit that file -->

# readflexfile

<!-- badges: start -->

[![technomics:
costverse](https://img.shields.io/badge/technomics-costverse-EAC435.svg)](https://github.com/technomics)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Build:
passing](https://img.shields.io/badge/build-passing-green.svg)](https://github.com/technomics/readflexfile.git)
[![coverage:
0%25](https://img.shields.io/badge/coverage-0%25-blue.svg)]()
[![version:
0.1.1](https://img.shields.io/badge/version-0.1.1-blue.svg)]()
<!-- badges: end -->

The goal of readflexfile is to facilitate the reading of the FlexFile
from the original JSON format into R. The package also includes helpful
functions to flatten the schema into a single table.

View the readflexfile [package site](https://technomics.github.com/readflexfile) to
explore the features\!

## Welcome to the costverse\!

The *costverse* is a collection of R packages, inspired by the
*tidyverse*. The goal is to create a cohesive ecosystem of R packages to
streamline tasks encountered by analysts in the cost analysis
profession. This can range from importing common data formats, working
with difficult data structures (e.g., a WBS), or applying more advanced
analytical techniques\!

The *costverse* began as an internal project by [Technomics Employee
Owners](https://www.technomics.net/) to develop a set of tools that we
actually want to use, to solve problems we are actually working on.
While aspects of the project still remain internal, the following
packages are currently available to the public. You are free to use them
under the [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html) - all
that we ask is to please cite us as the authors.

  - [costmisc](https://github.com/Technomics/costmisc/)
  - [readflexfile](https://github.com/Technomics/readflexfile/)

Do not hesitate to contact us if you have questions about what else is
in the works\!

## Installation

First install the package devtools if you haven’t already.

``` r
#install.packages("devtools")
devtools::install_github("Technomics/readflexfile")
```

### Development version

To get a bug fix, or to use a feature from the development version, you
can install readflexfile using the following.

``` r
devtools::dev_mode(on = TRUE)
devtools::install_github("Technomics/readflexfile")
```
