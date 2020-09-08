
# pcal 1.1.0

- This is a minor release, and the second CRAN release of `pcal`. 

## Major changes 

* `bfactor_interpret()` has been deprecated in favor of `polya::bfactor_interpret()`.

* `bfactor_log_interpret()` has been deprecated in favor of `polya::bfactor_log_interpret()`.

* `bfactor_interpret_kr` has been deprecated. `bfactor_interpret(bf, scale = "kass-raftery")` should be used instead.

* `bfactor_log_interpret_kr` has been deprecated. `bfactor_log_interpret(bf, scale = "kass-raftery")` should be used instead.

* `bfactor_interpret()` and `bfactor_log_interpret()` now have a `scale` argument to choose the Bayes factor interpretation scale to be used. The default `scale` is `"Jeffreys"`, which means that `bfactor_interpret(bf)` gives the same output as `bfactor_interpret(bf, scale = "Jeffreys")`. With the default `scale` both `bfactor_interpret()` and `bfactor_log_interpret()`  give the same output as they did in `pcal` 1.0.0.

## Minor changes

* `package?pcal` is now documented.

* Error and warning messages are now more informative.

* Improved code efficiency. 

* Improved unit tests.

* Improved README.

* Improved documentation.

* Improved code readability.

* Licence changed from GPL 3.0 to MIT.

# pcal 1.0.0

This is the first CRAN release of `pcal`.






