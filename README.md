# pcal: Calibration of P-Values for Testing Point Null Hypotheses

## Overview

P-values are the most commonly used measure of the evidence provided by the data against a model/hypothesis. Unfortunately, p-values are often incorrectly interpreted as type I error probabilities or as the probability that the hypotheses are true. The `pcal` package uses the calibrations developed in Sellke et al (2001) to calibrate p-values under a robust Bayesian perspective and obtain measures of the evidence provided by the data in favor of point null hypotheses which are safer and more straightforward interpret: 

  * `pcal()` calibrates p-values so that they can be directly interpreted as either lower bounds on the posterior probabilities of point null hypotheses or as lower bounds on the probability of a type I error. With this calibration one need not fear the misinterpretation of a frequentist type I error probability as the probability that the null hypothesis is true since here they coincide.
   
  *   `bcal()` calibrates p-values so that they can be interpreted as lower bounds on the odds provided by the data (Bayes factors) in favor of point null hypotheses.

Some utility functions are also included:

* `bfactor_to_prob()` turns Bayes factors into Posterior probabilities
  
* `bfactor_interpret()` classifies the strength of the evidence implied by a Bayes factor according the the scale suggested by Kass and Raftery (1995)
  
* `bfactor_log_interpret()` is similar to `bfactor_interpret()` but takes the logarithms (of any base) of Bayes factors as input
     

## Installation

``` pcal```is not yet on CRAN but you can instal it from GitHub using the ```devtools``` package:

```r
# install.packages("devtools")
devtools::install_github("pedro-teles-fonseca/pcal")
```

## Usage 

## References 

Kass RE, Raftery AE (1995). “Bayes factors.” Journal of the American Statistical Association, 90(430), 773–795.

Sellke T, Bayarri MJ, Berger JO (2001). “Calibration of p values for testing precise null hypotheses.” The American Statistician, 55(1), 62–71.

## Getting Help

If you find a bug, please file an issue with a minimal reproducible example on
[GitHub](https://github.com/pedro-teles-fonseca/pcal/issues). Feature requests are also welcome. You can contact me at pedro.teles.fonseca@outlook.com.