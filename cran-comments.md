## Test environments
* local OS X install, R 3.6.0
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## R CMD check results

I just released a package to you the other day, but I forgot to change the vignette! That's the only change. Sorry for splitting it up and making it a bit more messy.

0 errors | 0 warnings | 0 notes

## added features

The call to `source` is now wrapped in `tryCatch` for added durability. Also, points are now numeric vectors instead of character vectors.
