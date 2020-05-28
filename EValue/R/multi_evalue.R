#' @title Calculate a multiple-bias E-value
#'
#' @description Calculate an E-value for a specified set of biases.
#' @param biases An object created by [multi_bias()] (or a single bias) to
#'   include in the calculation of the E-value. May include any or all of
#'   [confounding()], [selection()], and [misclassification()], and any of the
#'   options described in the documentation for those functions.
#' @param est The effect estimate that was observed but which is suspected to be
#'   biased. This may be of class "estimate" (constructed with [RR()], [OR()],
#'   or [HR()], or more information can be provided using the other arguments.
#' @param lo Optional. Lower bound of the confidence interval. If not an object
#'   of class "estimate", assumed to be on the same scale as `est`.
#' @param hi Optional. Upper bound of the confidence interval. If not an object
#'   of class "estimate", assumed to be on the same scale as `est`.
#' @param true A number to which to shift the observed estimate to. Defaults to
#'   1. If not an object of class "estimate", assumed to be on the same scale as
#'   `est`.
#' @param verbose Logical indicating whether or not to print information about
#'   which parameters the multi-bias E-value refers to. Defaults to TRUE.
#' @param ... Arguments passed to other methods.
#' @return Returns a multiple bias E-value, of class "multi_evalue", describing
#'   the value that each of a number of parameters would have to have for the
#'   observed effect measure to be completely explained by bias.
#'
#' @keywords multi-bias
#'
#' @examples
#' # Calculate an E-value for unmeasured confounding
#' multi_evalue(est = RR(4), biases = confounding())
#' # Equivalent to
#' evalues.RR(4)
#'
#' # Calculate a multi-bias E-value for selection bias
#' # and misclassification
#' multi_evalue(est = RR(2.5),
#'          biases = multi_bias(selection("selected"),
#'                    misclassification("outcome")))
#'
#' # Calculate a multi-bias E-value for all three
#' # available types of bias
#' biases <- multi_bias(confounding(),
#'                      selection("general", "S = U"),
#'                      misclassification("exposure",
#'                             rare_outcome = TRUE))
#' multi_evalue(est = RR(2.5), biases = biases)
#'
#' # Calculate a multi-bias E-value for a non-rare OR
#' # using the square root approximation
#' multi_evalue(est = OR(2.5, rare = FALSE), biases = biases)
#'
#' # Calculate a non-null multi-bias E-value
#' multi_evalue(est = RR(2.5), biases = biases, true = 2)
#'
#' @export
#' @importFrom stats uniroot
multi_evalue <- function(biases, est, ...) {
  if (!inherits(biases, c("bias", "multi_bias"))) {
    stop('Argument "biases" must be of class "bias" or "multi_bias"')
  }
  
  if (class(biases) == "bias") biases <- multi_bias(biases)
  
  UseMethod("multi_evalue", est)
}



#' @param rare Logical indicating whether outcome is sufficiently rare for 
#'  risk ratio approximation to hold.
#' @keywords multi-bias
#' @export
#' @rdname multi_evalue
multi_evalues.HR = function(biases, est, lo = NA, hi = NA, rare = NULL, true = 1, verbose = TRUE, ...) {
  
  # sanity checks
  if ( est < 0 ) stop("HR cannot be negative")
  
  if ( !inherits(est, "HR") ) est = HR( est, rare = rare )
  if ( !is.na(lo) && !inherits(lo, "HR") ) lo = HR( lo, rare = attr(est, "rare") )
  if ( !is.na(hi) && !inherits(hi, "HR") ) hi = HR( hi, rare = attr(est, "rare") )
  if ( !inherits(true, "HR") ) true = HR( true, rare = attr(est, "rare") )
  
  est <- toRR(est)
  if (!is.na(lo)) lo <- toRR(lo, rare = rare)
  if (!is.na(hi)) hi <- toRR(hi, rare = rare)
  true <- toRR(true, rare = rare)
  
  return( multi_evalues.RR(biases = biases, est = est, lo = lo, hi = hi, true = true, verbose = verbose ) )
}

#' @param rare Logical indicating whether outcome is sufficiently rare for 
#'  risk ratio approximation to hold.
#' @keywords multi-bias
#' @export
#' @rdname multi_evalue
multi_evalues.OR = function(biases, est, lo = NA, hi = NA, rare = NULL, true = 1, verbose = TRUE, ...) {
  
  # sanity checks
  if ( est < 0 ) stop("OR cannot be negative")
  
  if ( !inherits(est, "OR") ) est = OR( est, rare = rare )
  if ( !is.na(lo) && !inherits(lo, "OR") ) lo = OR( lo, rare = attr(est, "rare") )
  if ( !is.na(hi) && !inherits(hi, "OR") ) hi = OR( hi, rare = attr(est, "rare") )
  if ( !inherits(true, "OR") ) true = OR( true, rare = attr(est, "rare"))
  
  est <- toRR(est)
  if (!is.na(lo)) lo <- toRR(lo, rare = rare)
  if (!is.na(hi)) hi <- toRR(hi, rare = rare)
  true <- toRR(true, rare = rare)
  
  return( multi_evalues.RR(biases = biases, est = est, lo = lo, hi = hi, true = true, verbose = verbose ) )
}

#' @keywords multi-bias
#' @export
#' @rdname multi_evalue

multi_evalues.RR <- function(biases, est, lo = NA, hi = NA, true = 1, verbose = TRUE, ...) {
  
  if (!inherits(biases, c("bias", "multi_bias"))) {
    stop('Argument "biases" must be of class "bias" or "multi_bias"')
  }
  
  if (class(biases) == "bias") biases <- multi_bias(biases)
  
  # organize user's values
  values = c(est, lo, hi)
  
  # sanity checks
  if (est < 0) stop("Estimate cannot be negative")
  if (true < 0) stop("True value is impossible")
  
  # warn user if using non-null true value
  if (true != 1) wrapmessage(c("You are calculating a \"non-null\" multi-bias E-value, i.e.,",
                               "a multi-bias E-value for the minimum amount of bias",
                               "needed to move the estimate and confidence",
                               "interval to your specified true value of", true, "rather than to",
                               "the null value."))
  
  # check if CI crosses null
  null.CI = NA
  if (est > true & !is.na(lo)) {
    null.CI = (lo < true)
  }
  
  if (est < true & !is.na(hi)) {
    null.CI = (hi > true)
  }
  
  # sanity checks for CI
  if (!is.na(lo) & !is.na(hi)) {
    # check if lo < hi
    if (lo > hi) stop("Lower confidence limit should be less than upper confidence limit")
  }
  
  if (!is.na(lo) & est < lo) stop("Point estimate should be inside confidence interval")
  if (!is.na(hi) & est > hi) stop("Point estimate should be inside confidence interval")
  
  # compute E-values
  E = sapply( values, FUN = function(x) multi_threshold(biases, x, true = true ) )
  
  # clean up CI reporting
  # if CI crosses null, set its E-value to 1
  if ( !is.na(null.CI) & null.CI == TRUE ){
    E[ 2:3 ] = 1
    wrapmessage("Confidence interval crosses the true value, so its multi-bias E-value is 1.") 
  }
  
  # if user provides either CI limit...
  if ( !is.na(lo) | !is.na(hi) ) {
    # ...then only report E-value for CI limit closer to null
    if ( est > true ) E[3] = NA
    if ( est < true ) E[2] = NA
    if ( est == true ) {
      E[2] = 1
      E[3] = NA
    }
  }
  
  result = rbind(values, E)
  
  rownames(result) = c("RR", "Multi-bias E-values")
  colnames(result) = c("point", "lower", "upper")
  class(result) <- c("multi_evalue", "evalue", "matrix")
  attr(result, "parameters") <- attr(biases, "parameters")
  
  if (verbose) return(result) else return(suppressMessages(print(result)))
}

#' @keywords internal

multi_threshold <- function(biases, x, true = 1) {
  
  if (is.na(x)) return(NA)
  
  rat <- max(c(true / x, x / true))
  
  uniroot(
    f = deg_func, y = rat, n = attr(biases, "n"), d = attr(biases, "d"),
    interval = c(1 - 1e-9, rat), extendInt = "upX"
  )$root
  
}

#' @export
multi_evalue.HR <- function(biases, est, lo = NA, hi = NA, true = 1, verbose = TRUE, ...) {
  multi_evalues.HR(biases = biases, est = est, lo = lo, hi = hi, true = true, verbose = verbose, ...)
}

#' @export
multi_evalue.OR <- function(biases, est, lo = NA, hi = NA, true = 1, verbose = TRUE, ...) {
  multi_evalues.OR(biases = biases, est = est, lo = lo, hi = hi, true = true, verbose = verbose, ...)
}

#' @export
multi_evalue.RR <- function(biases, est, lo = NA, hi = NA, true = 1, verbose = TRUE, ...) {
  multi_evalues.RR(biases = biases, est = est, lo = lo, hi = hi, true = true, verbose = verbose, ...)
}





#' @export
multi_evalue.default <- function(biases, est, ...) {
  
  if (is.null(measure) && !inherits(est, "estimate")) stop("Effect measure must be specified")
  
  measure <- class(est)[1]
  
  if (!measure %in% c("HR", "OR", "RR")) stop("\"measure\" must be one of \"RR\", \"OR\", or \"HR\"")
  
  multi_evalues_func <- switch(measure,
                               "HR" = multi_evalues.HR,
                               "OR" = multi_evalues.OR,
                               "RR" = multi_evalues.RR)
  
  multi_evalues_func(biases, est, ...)
}




#' @export
print.multi_evalue <- function(x, ..., latex = FALSE, verbose = TRUE) {
  if (!inherits(x, "multi_evalue")) stop('Argument must be of class "multi_evalue"')
  params <- attr(x, "parameters")
  if (!latex) params <- params$output else params <- params$latex
  params <- sub("OR", "RR", params)
  if (verbose) {
    wrapmessage(c("This multi-bias e-value refers simultaneously to parameters ",
                   paste(params, collapse = ", "), ". (See documentation for details.)"))
  }
  attr(x, "parameters") <- NULL
  class(x) <- "matrix" # to suppress attr printing
  print.default(x, ...)
}
