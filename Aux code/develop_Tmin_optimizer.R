

# toy example of finding Tmin(r=., q=0)

Phat_causal = function(shift){
  if (tail == "above") return( mean(calib - shift > q) )
  if (tail == "below") return( mean(calib + shift < q) )
}


set.seed(10)
calib = sort( rnorm(30) )
# repeat one value for testing purposes
calib[4] = calib[5]

# with lots of ties near q
set.seed(10)
calib = rnorm(10)
calib = sort( sample( calib, size = 30, replace = TRUE) )
# pick a q that's between repeated values
q = -0.3
r = 0.20
tail = "above"

# with lots of ties near q
# pick a q that's adjacent to a set of ties
q = -1
r = 0.15  # r can't be matched exactly in this example
tail = "below"

# choose an r such that no shift if needed
q = 0.3
tail = "above"
Phat_causal(0)
r = 0.30  # we already have <30% of calib above this choice of q



Phat_causal(0)  


# here, check if any shifting is actually needed


# what are the possible Phat values?
#Phat.options = 1 / ( 1 : length( unique(calib) ) )
# always possible to choose 0
#( Phat.options = c(Phat.options, 0) )

# evaluate the ECDF of the unshifted calib at those calib themselves
#  to get the possible values that Phat can take
#  this approach handles ties
( Phat.options = unique( ecdf(calib)(calib) ) )
# always possible to choose 0
( Phat.options = c(Phat.options, 0) )

# of Phats that are <= r, find the largest one (i.e., closest to r)
( Phat.target = max( Phat.options[ Phat.options <= r ] ) )


# find calib.star, the calibrated estimate that needs to move to q
# example for tail == "above":
# calib.star is the largest calibrated estimate that needs to move to just
#  BELOW q after shifting
# k * Phat.target is the number of calibrated estimates that should remain
#  ABOVE q after shifting
if ( tail == "above" ) calib.star = calib[ k - (k * Phat.target) ]
if ( tail == "below" ) calib.star = calib[ (k * Phat.target) + 1 ]

calib.star

# pick the bias factor that shifts calib.star to q
#  and then add a tiny bit (0.001) to shift calib.star to just
# below or above q
# if multiple calibrated estimates are exactly equal to calib.star, 
#  all of these will be shifted just below q (if tail == "above")
( Tmin = exp( abs(calib.star - q) + 0.001 ) )

# check it: >q case
calib.t = calib - log(Tmin)
mean(calib.t > q)  # should match r or be less than r if there are ties

# check it: <q case
calib.t = calib + log(Tmin)
mean(calib.t < q)  # should match Phat.target, which will equal r if 




