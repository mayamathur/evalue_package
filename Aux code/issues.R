
#library(EValue)
detach("package:EValue")

# also search "@@"


setwd("~/Dropbox/Personal computer/Independent studies/Permanent websites/Metasens website/metasens_website (git)/metasens_website/Main site/tests_human_inspection")
d = read.csv("Datasets for website test/kodama_prepped.csv")



q = log(0.5)
muB = log(1.5)
sigB = sqrt(0.5*0.25)
yr = log(1.5)
vyr = 0.5
t2 = 0.25
vt2 = 0.5
r = 0.75
tail = "below"

confounded_meta(method="parametric", q=q, r=r, muB=muB, sigB=sigB,
                     yr=yr, vyr=vyr,
                     t2=t2, vt2=vt2, tail = tail )

