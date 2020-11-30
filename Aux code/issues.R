
#library(EValue)
detach("package:EValue")

# also search "@@"


setwd("~/Dropbox/Personal computer/Independent studies/Permanent websites/Metasens website/metasens_website (git)/metasens_website/Main site/tests_human_inspection")
d = read.csv("Datasets for website test/flegal_prepped.csv")


# moderate proportion, but no CI
confounded_meta(method= "calibrated",
                q = log(0.9),
                muB = 0,
                tail = "below",
                yi.name = "yi",
                vi.name = "vi",
                dat = d,
                R = 500)