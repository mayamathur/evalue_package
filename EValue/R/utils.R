#' Internal function used to fit roots of a polynomial made up of the product of
#' bias factors.
#'
#' @param x A number. The variable to solve for.
#' @param y A number. The observed risk ratio.
#' @param n A number. Degree of polynomial in the numerator.
#' @param d A number. Degree of polynomial in the denominator.
#' @return Returns the value of the expression. Used for root solving. At the
#'   function's roots, will return 0.
#' @keywords internal

deg_func <- function(x, y, n, d) {
  (x^n) / ((2 * x - 1)^d) - y
}


#' Internal function used to calculate arbitrary bias factors.
#'
#' @param rr1 A number. A risk ratio that is a component of a bias factors
#' @param rr2 A number. The other risk ratio that is a component of a bias
#'   factors
#' @return Returns the value of the expression. Used for calculating bias
#'   factors.
#' @keywords internal

bf_func <- function(rr1, rr2) {
  (rr1 * rr2) / (rr1 + rr2 - 1)
}


#' Transformation from bias factor to confounding strength scale
#'
#' An internal function. 
#' @noRd
g = Vectorize( function(x) {
  # define transformation in a way that is monotonic over the effective range of B (>1)
  # to avoid ggplot errors in sens_plot
  # helper function for confounded_meta
  if ( is.na(x) ) return(NA)
  if (x < 1) return( x / 1e10 )
  x + sqrt( x^2 - x )
} )


#' Nicely wrap a message
#' @noRd
wrapmessage <- function(mess, width = 0.9 * getOption("width")) {
  message(paste(strwrap(paste(mess, collapse = " "), width = width), collapse = "\n"))
}
