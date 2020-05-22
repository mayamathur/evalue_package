#' Declare an effect measure
#'
#' @description These functions allow the user to declare that an estimate is a
#'   certain type of effect measure: risk ratio (`RR`), odds ratio (`OR`),
#'   hazard ratio (`HR`), risk difference (`RD`), linear regression coefficient
#'   (`OLS`), or mean standardized difference (`MD`).
#' @name effect_measures
#' @param est The effect estimate (numeric).
#' @param rare Logical. Whether the outcome is sufficiently rare for use of risk
#'   ratio approximates; if not, approximate conversions are used. Used only for
#'   [HR()] and [OR()]; see Details.
#' @param sd The standard deviation of the outcome (or residual standard
#'   deviation). Used only for [OLS()]; see Details.
#' @return An object of classes "estimate" and the measure of interest,
#'   containing the effect estimate and any other attributes to be used in
#'   future calculations.
#' @details The [conversion functions][convert_measures] use these objects to
#'   convert between effect measures when necessary to calculate E-values. Read
#'   more about the conversions in Table 2 of VanderWeele TJ, Ding P.
#'   *Sensitivity Analysis in Observational Research: Introducing the E-Value.*
#'   Annals of Internal Medicine. 2017;167(4):268–75.
#'
#'   See also VanderWeele TJ. *Optimal approximate conversions of odds ratios
#'   and hazard ratios to risk ratios.* Biometrics. 2019 Jan 6;(September
#'   2018):1–7.
#'
#'   For [OLS()], `sd` must be specified. A true standardized mean difference
#'   for linear regression would use \code{sd} = SD( Y | X, C ), where Y is the
#'   outcome, X is the exposure of interest, and C are any adjusted covariates.
#'   See Examples for how to extract this from \code{lm}. A conservative
#'   approximation would instead use \code{sd} = SD( Y ). Regardless, the
#'   reported E-value for the confidence interval treats \code{sd} as known, not
#'   estimated.
#' @examples
#' # Both odds ratios are 3, but will be treated differently in E-value calculations
#' # depending on whether rare outcome assumption is reasonable
#' OR(3, rare = FALSE)
#' OR(3, rare = TRUE)
#' evalue(OR(3, rare = FALSE))
#' evalue(OR(3, rare = TRUE))
#' attributes(OR(3, rare = FALSE))
#' 
#' # If an estimate was constructed via conversion from another effect measure,
#' # we can see the history of a conversion using the summary() function
#' summary(toRR(OR(3, rare = FALSE)))
#' summary(toRR(OLS(3, sd = 1)))
#' 
#' # Estimating sd for an OLS estimate
#' # first standardizing conservatively by SD(Y)
#' data(lead)
#' ols = lm(age ~ income, data = lead)
#' est = ols$coefficients[2]
#' sd = sd(lead$age)
#' summary(evalue(OLS(est, sd)))
#' # now use residual SD to avoid conservatism
#' # here makes very little difference because income and age are
#' # not highly correlated
#' sd = summary(ols)$sigma
#' summary(evalue(OLS(est, sd)))


#' @rdname effect_measures
#' @export
RR <- function(est) {
  class(est) <- c("RR", "estimate")
  est
}

#' @rdname effect_measures
#' @export
OR <- function(est, rare) {
  class(est) <- c("OR", "estimate")
  attr(est, "rare") <- rare
  est
}

#' @rdname effect_measures
#' @export
HR <- function(est, rare) {
  class(est) <- c("HR", "estimate")
  attr(est, "rare") <- rare
  est
}

#' @rdname effect_measures
#' @export
RD <- function(est) {
  class(est) <- c("RD", "estimate")
  est
}

#' @rdname effect_measures
#' @export
OLS <- function(est, sd) {
  class(est) <- c("OLS", "estimate")
  attr(est, "sd") <- sd
  est
}

#' @rdname effect_measures
#' @export
MD <- function(est) {
  class(est) <- c("MD", "estimate")
  est
}

#' @export
print.estimate <- function(x, ...) {
  attr(x, "sd") <- NULL
  attr(x, "rare") <- NULL
  attr(x, "history") <- NULL
  class(x) <- "numeric"
  print.default(x, ...)
}

#' @export
summary.estimate <- function(object, ...) {
  if (is.null(attr(object, "history"))) return(cat(class(object)[1], "=", object))
  history <- attr(object, "history")
  cat(class(object)[1], "=", object,
      "\nThis is an approximate conversion of the original", 
      history[1,1], "estimate =", history[1,2])
}

#' Convert an effect measure
#'
#' @description These helper functions are mostly used internally to convert
#'   [effect measures][effect_measures] for the calculation of E-values. The
#'   approximate conversion of odds and hazard ratios to risk ratios depends on
#'   whether the rare outcome assumption is made.
#' @name convert_measures
#' @param est The effect estimate; constructed with one of [RR()], [OR()], [HR()],
#'   [MD()], [OLS()].
#' @param rare When converting a [OR()] or [HR()] estimate, a logical indicating
#'   whether the outcome is sufficiently rare to approximate a risk ratio.
#' @param delta When converting an [OLS()] estimate, the contrast of interest 
#'   in the exposure. Defaults to 1 (a 1-unit contrast in the exposure).
#' @param ... Arguments passed to other methods.
#' @return An object of class "estimate" and the desired effect measure. Also
#'   includes as an attribute its conversion history.
#' @details Uses the conversions listed in Table 2 of VanderWeele TJ, Ding P.
#'   *Sensitivity Analysis in Observational Research: Introducing the E-Value.*
#'   Annals of Internal Medicine. 2017;167(4):268–75.
#'
#'   See references.
#'
#'   Regarding the continuous outcome, the function uses the effect-size
#'   conversions in Chinn (2000) and VanderWeele (2017) to approximately convert
#'   the mean difference between these exposure "groups" to the odds ratio that
#'   would arise from dichotomizing the continuous outcome.
#'
#' @references Chinn, S (2000). A simple method for converting an odds ratio to
#' effect size for use in meta-analysis. \emph{Statistics in Medicine}, 19(22),
#' 3127-3131.
#'
#' VanderWeele, TJ (2017). On a square-root transformation of the odds ratio for
#' a common outcome. \emph{Epidemiology}, 28(6), e58.
#'
#' VanderWeele TJ (2020). *Optimal approximate conversions of odds ratios and
#' hazard ratios to risk ratios.* Biometrics.
#' @examples
#' # Both odds ratios are 3, but will be treated differently
#' # depending on whether rare outcome assumption is reasonable
#' OR(3, rare = FALSE)
#' OR(3, rare = TRUE)
#' toRR(OR(3, rare = FALSE))
#' toRR(OR(3, rare = TRUE))
#' attributes(toRR(toMD(OLS(3, sd = 1.2), delta = 1)))
#' @rdname convert_measures
#' @export
toRR <- function(est, rare, delta = 1, ...) {
  UseMethod("toRR", est)
}
#' @rdname convert_measures
#' @export
toMD <- function(est, delta = 1, ...) {
  UseMethod("toMD", est)
}



#' @export
toMD.OLS <- function(est, delta = 1, ... ) {
  sd_attr <- attr(est, "sd")
  
  if (is.null(sd_attr)) 
    stop("Must specify the outcome standard deviation. Use argument sd = in the OLS() function")
  
  MD <- est * delta / sd_attr
  class(MD) <- c("MD", "estimate")
  attr(MD, "history") <- rbind(attr(est, "history"), c("OLS", est))
  MD
}

#' @export
toRR.MD <- function(est, ... ) {
  RR <- exp( 0.91 * est )
  class(RR) <- c("RR", "estimate")
  attr(RR, "history") <- rbind(attr(est, "history"), c("MD", est))
  RR
}

#' @export
toRR.OLS <- function(est, rare = NULL, delta = 1, ... ) {
  toRR(toMD(est, delta = delta))
}

#' @export
toRR.HR <- function(est, rare, ... ) {
  rare_attr <- attr(est, "rare")
  
  if (is.null(rare_attr)) 
    stop("Must specify whether the rare outcome assumption can be made. Use argument rare = in the HR() function.")
 
  if (rare_attr) RR <- est else {
    RR <- ( 1 - 0.5^sqrt( est ) ) / ( 1 - 0.5^sqrt( 1 / est ) )
  }
  class(RR) <- c("RR", "estimate")
  attr(RR, "history") <- rbind(attr(est, "history"), c("HR", est))
  RR
}

#' @export
toRR.OR <- function(est, rare, ... ) {
  rare_attr <- attr(est, "rare")
  
  if (is.null(rare_attr)) 
    stop("Must specify whether the rare outcome assumption can be made. Use argument rare = in the OR() function.")
  
  if (rare_attr) RR <- est else RR <- sqrt(est)
  class(RR) <- c("RR", "estimate")
  attr(RR, "history") <- rbind(attr(est, "history"), c("OR", est))
  RR
}


#' @export
toRR.default <- function(est, ... ) {
  stop("RR conversion is currently available only for estimates of class \"OR\", \"HR\", \"MD\", and \"OLS\"")
}


#' @export
toMD.default <- function(est, ...) {
  stop("MD conversion is currently available only for estimates of class \"OLS\"")
}

