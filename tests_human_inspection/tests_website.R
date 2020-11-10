
library(testthat)
#library(EValue)
library(devtools)
library(dplyr)
library(ICC)
library(MetaUtility)
library(ggplot2)

# for simulating meta-analysis data
# library(here())
# setwd(here())
# setwd("tests")
source("~/Box Sync/jlee/Maya/evalue/EValue/tests/helper_testthat.R")

source("~/Box Sync/jlee/Maya/evalue/EValue/R/meta-analysis.R")


# setwd(here())
# setwd("tests_human_inspection/Datasets for website test")
d = read.csv("~/Box Sync/jlee/Maya/evalue/tests_human_inspection/Datasets for website test/gbc_prepped.csv")

##### With q = log(0.9) #####
confounded_meta(q = log(.9),
                r = 0.1,
                muB = 0,
                tail = "below",
                yi.name = "yi",
                vi.name = "vi",
                dat = d,
                R = 500)
### R output:
# The confidence interval and/or standard error for Tmin and Gmin were not estimable via bias-corrected and accelerated bootstrapping. You can try increasing R.
# Value        Est        SE CI.lo     CI.hi
# 1  Prop 0.02358491 0.0152887     0 0.0754717
# 2  Tmin 1.00000000        NA    NA        NA
# 3  Gmin 1.00000000        NA    NA        NA
# Warning message:
#   In confounded_meta(q = log(0.9), r = 0.1, muB = 0, tail = "below",  :
#                        Phat is already less than or equal to r even with no confounding, so Tmin and Gmin are simply equal to 1. No confounding at all is required to make the specified shift.

### Website output: 
# Error: estimated adjustment 'w' is infinite (from bootstrapping)



##### With q = log(1.1) #####
confounded_meta(q = log(1.1),
                r = 0.1,
                muB = 0,
                tail = "below",
                yi.name = "yi",
                vi.name = "vi",
                dat = d,
                R = 500)
### R output: 
# The confidence interval and/or standard error for Tmin and Gmin were not estimable via bias-corrected and accelerated bootstrapping. You can try increasing R.
# Value       Est         SE     CI.lo    CI.hi
# 1  Prop 0.6462264 0.05552047 0.5044421 0.743215
# 2  Tmin 1.1423726         NA        NA       NA
# 3  Gmin 1.5456624         NA        NA       NA

### website output
# all wrong (e.g., Phat is 0)


sens_plot(type = "line",
          q = log(1.1),
          tail = "below",
          Bmin = log(1),
          Bmax = log(4),
          yi.name = "yi",
          vi.name = "vi",
          dat = d,
          R = 500)

### R output
# produces a line plot with warning:
# Warning message:
#   In sens_plot(type = "line", q = log(1.1), tail = "below", Bmin = log(1),  :
#                  Some of the pointwise confidence intervals were not estimable via bias-corrected and accelerated bootstrapping, so the confidence band on the plot may not be shown for some values of the bias factor. You can try increasing R.

##### Output from website:
# produces correct plot, but also error:
# object 'give.CI' not found
