
#library(EValue)
detach("package:EValue")

# also search "@@"


confounded_meta(method = "calibrated",
                q = log(.9),
                r=.1,
                muB=0,
                tail="below",
                yi.name = "yi",
                vi.name = "vi",
                dat = d,
                R=500)