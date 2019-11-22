# gradeR 1.0.5

## Extra things

- added `findGlobalPaths`, which searchs student submissions for the very bad global/machine-specific file paths
- added `findBadEncodingFiles`, which searchs student submissions for the very bad non-UTF-8 characters
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
