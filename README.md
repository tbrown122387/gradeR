
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gradeR

<!-- badges: start -->

<!-- badges: end -->

The goal of `gradeR` is to help grade bunches of `R` script assignment
submissions. This package has one function called `calcGrades()`, and
much of the testing functionality is taken from the `testthat` package.

## Installation

You can install the released version of `gradeR` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("gradeR")
```

You can install the development version of `gradeR` from with:

``` r
# install.packages("devtools")
devtools::install_github("tbrown122387/gradeR")
```

## Example

This is a basic overview which shows you how to grade a bunch of
assignment submissions. Make sure to change the paths to ones that
actually exist on your machine. For more details on this example, see
the vignette.

``` r
# load in the package
library(gradeR)

# this is the directory with all of the student submissions
submissionDir <- "../submissions/"

# get the grades
grades <- calcGrades(submission_dir = submissionDir, 
                     your_test_file = "~/your/path/assignment1_test_file.r")
```

The directory of the first argument will be walked recursively, and any
file ending in a `.R` or `.r` will be `source`d and tested using the
tests in the `assignment1_grading_file.r`.

The file with the tests is specified in the second argument. This is a
file you must create on your own. For more information on how to write
`testthat` tests, see
[this.](https://cran.r-project.org/package=testthat)

It might be beneficial to recommend that your students:

  - clear out their workspace and `source` their `.R` submissions before
    submitting them. This makes sure everything runs as expected (at
    least on their machine).
  - to not rename files that are read in as data, or change any raw data
    files on *their* hard drives. If they forget to abide by this rule,
    you will have to edit their submission and rename file names in
    their script.
  - to not use global file paths (have them interactively use `setwd()`,
    and then read in files using only their name). If they forget to do
    this, then you will need to change all of the global paths to local
    paths on their machine.
