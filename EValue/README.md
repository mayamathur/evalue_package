
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The EValue R package

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/EValue)](https://CRAN.R-project.org/package=EValue)
<!-- badges: end -->

The EValue package allows users to calculate bounds and E-values for
unmeasured confounding in observational studies and meta-analyses. The
package also includes functions for the assessment of selection bias and
differential misclassification and the joint impact of all three types
of bias.

## Installation

You can install the released version of EValue from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("EValue")
```

Then load the package:

``` r
library(EValue)
```

## Examples

E-values are simple to calculate. For example, the E-value for the
association between cigarette smoking and lung cancer as observed by
Hammond and Horn in 1958:

``` r
evalues.RR(est = 10.73, lo = 8.02, hi = 14.36)
#>             point    lower upper
#> RR       10.73000  8.02000 14.36
#> E-values 20.94777 15.52336    NA
```

For more on E-values for unmeasured confounding, see the
[vignette](https://cran.r-project.org/package=EValue/vignettes/unmeasured-confounding.html).

More complex assessment of several biases is also easy. To bound the
bias due to unmeasured confounding, selection bias, and differential
outcome misclassification, we can use background knowledge about the
strength of the biases to propose sensitivity analysis parameters:

``` r
biases <- multi_bias(confounding(),
                     selection("general", "increased risk"),
                     misclassification("exposure", rare_outcome = TRUE))

multi_bound(biases,
            RRUcY = 2, RRAUc = 1.5,
            RRSUsA1 = 1.25, RRUsYA1 = 2.5,
            ORYAaS = 1.75)
#> [1] 2.386364
```

Read more about how to specify [multiple
biases](https://cran.r-project.org/package=EValue/vignettes/multiple-bias.html)
and see several worked
[examples](https://cran.r-project.org/package=EValue/vignettes/multiple-bias-examples.html).

## Other options

If all you need to do is calculate an E-value for unmeasured
confounding, just try out the [online
calculator](https://www.evalue-calculator.com). Graphical interfaces are
also linked under each of the types of sensitivity analysis in the
header.
