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

#' Nicely wrap a message
#' @noRd
wrapmessage <- function(mess, width = 0.9 * getOption("width")) {
  message(paste(strwrap(paste(mess, collapse = " "), width = width), collapse = "\n"))
}
