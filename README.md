
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gradeR

<!-- badges: start -->

<!-- badges: end -->

The goal of `gradeR` is to help grade bunches of `R` script assignment
submissions. This package has one function (`calcGrades()`), and most of
the testing functionality is taken from the `testthat`
package.

## Installation

<!-- You can install the released version of gradeR from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("gradeR") -->

<!-- ``` -->

<!-- And the development version from [GitHub](https://github.com/) with: -->

<!-- ``` r -->

<!-- # install.packages("devtools") -->

<!-- devtools::install_github("tbrown122387/gradeR") -->

<!-- ``` -->

You can install the development version of `gradeR` from with:

``` r
# install.packages("devtools")
devtools::install_github("tbrown122387/gradeR")
```

## Example

This is a basic example which shows you how to solve a common problem.
Make sure to change the paths to ones that actually exist on your
machine.

``` r
#library(gradeR)
#setwd("~/gradeR/example/")
#grades <- calcGrades("/path/to/assignment1_submissions/", "/path/to/grade_hw1.R")
```

The directory of the first argument will be walked recursively, and any
file ending in a `.R` or `.r` will be `source`d and tested. The file
with the tests is specified in the second argument. This is a file you
must create on your own. For more information on how to write `testthat`
tests, see [this.](https://cran.r-project.org/package=testthat)
