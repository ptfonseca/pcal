
# pcal 1.1.0

This is the second CRAN release of `pcal`. This realease includes new documented and exported internal functions, improved documentation, code readability and code efficieny, more unit testing, new arguments to `bfactor_interpret()` and `bfactor_log_interpret()` and the deprecation of `bfactor_interpret_kr` and `bfactor_log_interpret_kr`. 

## Major changes 

* `bfactor_interpret()` and `bfactor_log_interpret()` now have a `scale` argument to choose the Bayes factor interpretation scale to be used. The default `scale` is `"Jeffreys"`, which means that `bfactor_interpret(bf)` gives the same output as `bfactor_interpret(bf, scale = "Jeffreys")`. Therefore, by default, `bfactor_interpret()` and `bfactor_log_interpret()`  give the same output as they did in `pcal` 1.0.0.

* `bfactor_interpret_kr` and `bfactor_log_interpret_kr` have been deprecated. `bfactor_interpret_kr(bf)` is now `bfactor_interpret(bf, scale = "kass-raftery")` and `bfactor_log_interpret_kr(bf)` is now `bfactor_log_interpret(bf, scale = "kass-raftery")`.

## Minor changes

* Improved README.

* Improved documentation.

* Improved code readability.

* Improved code efficiency. 

* More unit tests.

## New internal functions

The following internal functions are now documented and exported:

 * `check_prob` checks if an object is a numeric vector of valid probability values. 
 
 * `check_bf` checks if an object is a numeric vector of valid Bayes factor values. 
 
 * `check_log_bf` checks if an object is a numeric vector of valid logarithmic Bayes factor values. It is similar to `check_bf` but does not require values to be non-negative. 
 
 * `check_log_base` checks if an object is a vector of `length` 1 representing a valid logarithmic base.
 
 * `check_scale` checks if an object is a string of characters representing one of the Bayes factor interpretation scales available in the `pcal` package.
 
These are helper functions that are useful to validate inputs, intermediate calculations and outputs in user-defined functions.

# pcal 1.0.0

This is the first CRAN release of `pcal`.






