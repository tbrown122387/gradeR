# gradeR 1.0.8

## Extra things

A `suppress_warnings` argument was added to `calcGradesForGradescope`. Now you can choose whether or not warnings should be fatal when running student submission scripts.

Second, more flexibility in specifying the [test visibility](https://gradescope-autograders.readthedocs.io/en/latest/specs/#output-format) has been added. Students can see the test results before the submission deadline, after the submission deadline but before the publish date, after the publish date, or never. 


# gradeR 1.0.7

## Bug fixes

Fixed the multiple assertions bugs in `calcGrades` and `calcGradesForGradescope`. If one test had multiple calls to some expect function, all but the first would be ignored. Now, they are all tested, and full points are awarded if and only if they are all true. Thanks to Al Fischer of WCU for spotting this!

Two bugs in the vignette were fixed as well. `data.csv` needed to be in the local directory from where calcGrades is called, and there shouldn't be any `setwd()` calls in the test file. 

# gradeR 1.0.6

## Extra things

- added `calcGradesForGradescope` function, which helps grade assignments on the Gradescope platform
- added `using_gradeR_with_Gradescope` a vignette for helping with the set up of Gradescope's autograder

# gradeR 1.0.5

## Extra things

- added `findGlobalPaths`, which searches student submissions for the very bad global/machine-specific file paths
- added `findBadEncodingFiles`, which searches student submissions for the very bad non-UTF-8 characters
- added a `verbose` argument to `calcGrades`, so you can see which file is being ran in real time

# gradeR 1.0.4

## Extra things

-warnings that get triggered by running student submission can now be ignored.

# gradeR 1.0.3

## Extra things

-added a vignette with a fully-worked example

# gradeR 1.0.2

## Extra things

- made points columns numeric
- inserted `source` call inside `tryCatch` function, so that broken submissions don't interrupt `calcGrades` call
- added printing of which submissions trigger warnings or errors

# gradeR 1.0.1

## Bug fixes

Fixed bug that prevents an odd number of submissions!

## Extra things

Made the columns of the returned data frame a little prettier.


# gradeR 1.0.0

First release!
