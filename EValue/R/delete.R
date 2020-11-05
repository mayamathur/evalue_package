

##### Calibrated Line Plots #####
# bm
d = metafor::escalc(measure="RR",
                    ai=tpos,
                    bi=tneg,
                    ci=cpos,
                    di=cneg,
                    data=metafor::dat.bcg)

# without confidence band
# @@ still has "In min(x) : no non-missing arguments to min; returning Inf"
sens_plot( method = "calibrated",
           type="line",
           q=log(1.1),
           Bmin=log(1),
           Bmax=log(4),
           dat = d,
           yi.name = "yi",
           vi.name = "vi",
           give.CI = FALSE )

# with confidence band
# commented out because  it takes a while
# this example gives bootstrap warnings because of its small sample size
sens_plot( method = "calibrated",
           type="line",
           q=log(1.1),
           Bmin=log(1),
           Bmax=log(4),
           R = 500,  # should be higher in practice (e.g., 1000)
           dat = d,
           yi.name = "yi",
           vi.name = "vi",
           give.CI = TRUE )

##### Parametric Line Plots #####
# with heterogeneous bias across studies and with confidence band
sens_plot( method = "parametric",
           type="line",
           q=log(1.1),
           yr=log(1.3),
           t2=0.4,
           sigB = 0.1,
           Bmin=log(1),
           Bmax=log(4),
           dat,
           yi.name = "yi",
           vi.name = "vi" )

##### Distribution Line Plot #####


# distribution plot: apparently causative
# commented out because takes 5-10 seconds to run
# sens_plot( type="dist", q=log(1.1), muB=log(2),
#           yr=log(1.3), t2=0.4 )

# distribution plot: apparently preventive
# commented out because takes 5-10 seconds to run
# sens_plot( type="dist", q=log(0.90), muB=log(1.5),
#           yr=log(0.7), t2=0.2 )