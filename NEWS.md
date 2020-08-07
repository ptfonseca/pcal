
# pcal 1.1.0

This is the second CRAN release of `pcal`. This release includes three new utility functions that can be useful to validate inputs or intermediate calculations in user-defined functions or to validate the output of calculations/functions.

## New functions

* `check_prob` checks if an object is a numeric vector of valid probability values. 
* `check_bf` checks if an object is a numeric vector of valid Bayes factor values. 
* `check_log_bf` checks if an object is a numeric vector of valid logarithmic Bayes factor values. It is similar to `check_log_bf` but does not require values to be non-negative. 

## Minor changes

* Documentation improved.
* Code readability improved.
* More unit tests.

# pcal 1.0.0

This is the first CRAN release of `pcal`.
