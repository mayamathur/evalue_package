
#library(EValue)
detach("package:EValue")

# also search "@@"


setwd("~/Dropbox/Personal computer/Independent studies/Permanent websites/Metasens website/metasens_website (git)/metasens_website/Main site/tests_human_inspection")
d = read.csv("Datasets for website test/kodama_prepped.csv")



confounded_meta(method="calibrated",
                q=log(.5),
                r=0.75,
                tail="below",
                muB = -0.2,
                
                dat = d, 
                yi.name = "yi",
                vi.name = "vi")
# the nonequal rows error is because of Tmin_Gmin_CI_lims; 
#  g(T.hi) doesn't work when T.hi is NA



# se