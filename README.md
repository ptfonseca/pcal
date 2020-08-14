
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pcal: Calibration of P-Values for Point Null Hypothesis Testing

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/pcal)](https://CRAN.R-project.org/package=pcal)
[![Build
Status](https://travis-ci.org/pedro-teles-fonseca/pcal.svg?branch=master)](https://travis-ci.org/pedro-teles-fonseca/pcal)
[![R build
status](https://github.com/pedro-teles-fonseca/pcal/workflows/R-CMD-check/badge.svg)](https://github.com/pedro-teles-fonseca/pcal/actions)
![pkgdown](https://github.com/pedro-teles-fonseca/pcal/workflows/pkgdown/badge.svg)
[![codecov](https://codecov.io/gh/pedro-teles-fonseca/pcal/branch/master/graph/badge.svg)](https://codecov.io/gh/pedro-teles-fonseca/pcal)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

## Overview

P-values are the most commonly used tool to measure the evidence
provided by the data against a model or hypothesis. Unfortunately,
p-values are often incorrectly interpreted as the probability that the
null hypothesis is true or as type I error probabilities. The `pcal`
package uses the calibrations developed in Sellke, Bayarri, and Berger
(2001) to calibrate p-values under a robust perspective and obtain
measures of the evidence provided by the data in favor of point null
hypotheses which are safer and more straightforward interpret:

  - `pcal()` calibrates p-values so that they can be directly
    interpreted as either lower bounds on the posterior probabilities of
    point null hypotheses or as lower bounds on type I error
    probabilities. With this calibration one need not fear the
    misinterpretation of a type I error probability as the probability
    that the null hypothesis is true because they coincide. Note that
    the output of this calibration has both Bayesian and Frequentist
    interpretations.

  - `bcal()` calibrates p-values so that they can be interpreted as
    lower bounds on Bayes factors in favor of point null hypotheses.

Some utility functions are also included:

  - `bfactor_to_prob()` turns Bayes factors into posterior probabilities
    using a formula from Berger and Delampady (1987).

  - `bfactor_interpret()` classifies the strength of the evidence
    implied by a Bayes factor according to the scales suggested by
    Jeffreys (1961) and Kass and Raftery (1995).

  - `bfactor_log_interpret()` is similar to `bfactor_interpret()` but
    takes logarithms of Bayes factors as input.

## Installation

The released version of `pcal` can be installed from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("pcal")
```

The development version can be installed from
[GitHub](https://github.com/) using the `devtools` package:

``` r
# install.packages("devtools")
devtools::install_github("pedro-teles-fonseca/pcal")
```

## Usage

First we need a p-value from any statistical test of a point null
hypothesis:

``` r
x <- matrix(c(22, 13, 13, 23), ncol = 2)
pv <- chisq.test(x)[["p.value"]]

pv
#> [1] 0.04377308
```

In classical hypothesis testing, if the typical 0.05 significance
threshold is used then this p-value slightly below 0.05 would result in
the rejection of the null hypothesis.

With `bcal()` we can turn `pv` into a lower bound for the Bayes factor
in favor of the null hypothesis:

``` r
bcal(pv)
#> [1] 0.3722807
```

We can also turn `pv` into a lower bound for the posterior probability
of the null hypothesis using `pcal()`:

``` r
pcal(pv)
#> [1] 0.2712861
```

This is an approximation to the minimum posterior probability of the
null hypothesis that we would find by changing the prior distribution of
the parameter of interest (under the alternative hypothesis) over wide
classes of distributions. The output of `bcal()` has an analogous
interpretation for Bayes factors (instead of posterior probabilities).

Note that according to `pcal()` the posterior probability that the null
hypothesis is true is at least 0.27 (approximately), which implies that
a p-value below 0.05 is not necessarily indicative of strong evidence
against the null hypothesis.

One can avoid the specification of prior probabilities for the
hypotheses by focusing solely on Bayes factors. To compute posterior
probabilities for the hypotheses, however, prior probabilities must by
specified. By default, `pcal()` assigns a prior probability of 0.5 to
the null hypothesis. We can specify different prior probabilities, for
example:

``` r
pcal(pv, prior_prob = .95)
#> [1] 0.8761354
```

In this case we obtain a higher lower bound because the null hypothesis
has a higher prior probability.

Sellke, Bayarri, and Berger (2001) noted that a scenario in which they
definitely recommend the aforementioned calibrations is when
investigating fit to the null model with no explicit alternative in
mind. Pericchi and Torres (2011) warned that despite the usefulness and
appropriateness of these p-value calibrations they do not depend on
sample size, and hence the lower bounds obtained with large samples may
be conservative.

Since the output of `bcal(pv)` is a Bayes factor, we can use
`bfactor_to_prob()` to turn it into a posterior probability:

``` r
bfactor_to_prob(bcal(pv)) # same as pcal(pv)
#> [1] 0.2712861
```

Like `pcal()`, `bfactor_to_prob()` assumes a prior probability of 0.5 to
the null hypothesis. We can change this default:

``` r
bfactor_to_prob(bcal(pv), prior_prob = .95)
#> [1] 0.8761354
```

To classify the strength of the evidence in favor of the null hypothesis
implied by a Bayes factor according to the scale suggested by Jeffreys
(1961) we can use `bfactor_interpret()`:

``` r
bfs <- c(0.1, 2, 5, 20, 50, 150)

bfactor_interpret(bfs)
#> [1] "Negative"    "Weak"        "Substantial" "Strong"      "Very Strong"
#> [6] "Decisive"
```

Alternatively, we can use the interpretation scale suggested by Kass and
Raftery (1995):

``` r
bfactor_interpret(bfs, scale = "kass-raftery")
#> [1] "Negative"    "Weak"        "Positive"    "Strong"      "Strong"     
#> [6] "Very Strong"
```

Because Bayes factors are often reported on a logarithmic scale, there
is also a `bfactor_log_interpret()` function that interprets the
logarithms of Bayes factors:

``` r
log_bfs <- log10(bfs)
 
bfactor_log_interpret(log_bfs, base = 10)
#> [1] "Negative"    "Weak"        "Substantial" "Strong"      "Very Strong"
#> [6] "Decisive"

bfactor_log_interpret(log_bfs, scale = "kass-raftery", base = 10)
#> [1] "Negative"    "Weak"        "Positive"    "Strong"      "Strong"     
#> [6] "Very Strong"
```

To compare Bayes factors with results from standard likelihood ratio
tests it can be useful to obtain the strength of the evidence against
the null hypothesis. If `bf` is a Bayes factor in favor of the null
hypothesis, one can use `1/bf` as input to obtain the strength of the
evidence against the null hypothesis:

``` r
# Evidence in favor of the null hypothesis
bfactor_interpret(c(0.1, 2, 5, 20, 50, 150))
#> [1] "Negative"    "Weak"        "Substantial" "Strong"      "Very Strong"
#> [6] "Decisive"

# Evidence against the null hypothesis
bfactor_interpret(1/c(0.1, 2, 5, 20, 50, 150))
#> [1] "Strong"   "Negative" "Negative" "Negative" "Negative" "Negative"
```

## Getting Help

If you find a bug, please file an issue with a minimal reproducible
example on [GitHub](https://github.com/pedro-teles-fonseca/pcal/issues).
Feature requests are also welcome. You can contact me at
<pedro.teles.fonseca@outlook.com>.

## References

<div id="refs" class="references hanging-indent">

<div id="ref-bergerDelampady1987">

Berger, James O., and Mohan Delampady. 1987. “Testing Precise
Hypotheses.” *Statistical Science* 2 (3): 317–35.

</div>

<div id="ref-jeffreys1961">

Jeffreys, Harold. 1961. *Theory of Probability*. 3rd ed. Oxford Classic
Texts in the Physical Sciences. Oxford University Press.

</div>

<div id="ref-kass1995">

Kass, Robert E., and Adrian E. Raftery. 1995. “Bayes Factors.” *Journal
of the American Statistical Association* 90 (430): 773–95.

</div>

<div id="ref-pericchiTorres2011">

Pericchi, Luis, and David Torres. 2011. “Quick Anomaly Detection by the
Newcomb—Benford Law, with Applications to Electoral Processes Data from
the USA, Puerto Rico and Venezuela.” *Statistical Science* 26 (4):
502–16.

</div>

<div id="ref-sellke2001">

Sellke, Thomas, M. J. Bayarri, and James O. Berger. 2001. “Calibration
of P Values for Testing Precise Null Hypotheses.” *The American
Statistician* 55 (1): 62–71.

</div>

</div>
