
#library(EValue)
detach("package:EValue")

# also search "@@"

# confounded_meta(method = "calibrated",
#                 q = log(.9),
#                 r=.1,
#                 muB= -5,
#                 tail="below",
#                 yi.name = "yi",
#                 vi.name = "vi",
#                 dat = d,
#                 R=500,
#                 simplifyWarnings = TRUE)
# 
# confounded_meta(method="parametric",
#                 q=log(1.4),
#                 muB=0,
#                 sigB=0,
#                 yr=log(1.4),
#                 t2=0.1,
#                 simplifyWarnings = TRUE)
# 
# 
# confounded_meta(method = "calibrated",
#                 q = log(.9),
#                 r=.1,
#                 muB=0,
#                 tail="below",
#                 yi.name = "yi",
#                 vi.name = "vi",
#                 dat = d,
#                 R=500)

sens_plot( method = "calibrated",
           type="line",
           q=log(1.1),
           R = 500,  # should be higher in practice (e.g., 1000)
           dat = d,
           yi.name = "yi",
           vi.name = "vyi",
           give.CI = TRUE )
