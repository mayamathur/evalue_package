% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_evalue.R
\name{multi_evalue}
\alias{multi_evalue}
\alias{multi_evalues.HR}
\alias{multi_evalues.OR}
\alias{multi_evalues.RR}
\title{Calculate a multiple-bias E-value}
\usage{
multi_evalue(biases, est, ...)

multi_evalues.HR(
  biases,
  est,
  lo = NA,
  hi = NA,
  rare = NULL,
  true = 1,
  verbose = TRUE,
  ...
)

multi_evalues.OR(
  biases,
  est,
  lo = NA,
  hi = NA,
  rare = NULL,
  true = 1,
  verbose = TRUE,
  ...
)

multi_evalues.RR(biases, est, lo = NA, hi = NA, true = 1, verbose = TRUE, ...)
}
\arguments{
\item{biases}{An object created by \code{\link[=multi_bias]{multi_bias()}} (or a single bias) to
include in the calculation of the E-value. May include any or all of
\code{\link[=confounding]{confounding()}}, \code{\link[=selection]{selection()}}, and \code{\link[=misclassification]{misclassification()}}, and any of the
options described in the documentation for those functions.}

\item{est}{The effect estimate that was observed but which is suspected to be
biased. This may be of class "estimate" (constructed with \code{\link[=RR]{RR()}}, \code{\link[=OR]{OR()}},
or \code{\link[=HR]{HR()}}, or more information can be provided using the other arguments.}

\item{...}{Arguments passed to other methods.}

\item{lo}{Optional. Lower bound of the confidence interval. If not an object
of class "estimate", assumed to be on the same scale as \code{est}.}

\item{hi}{Optional. Upper bound of the confidence interval. If not an object
of class "estimate", assumed to be on the same scale as \code{est}.}

\item{rare}{Logical indicating whether outcome is sufficiently rare for
risk ratio approximation to hold.}

\item{true}{A number to which to shift the observed estimate to. Defaults to
\enumerate{
\item If not an object of class "estimate", assumed to be on the same scale as
\code{est}.
}}

\item{verbose}{Logical indicating whether or not to print information about
which parameters the multi-bias E-value refers to. Defaults to TRUE.}
}
\value{
Returns a multiple bias E-value, of class "multi_evalue", describing
the value that each of a number of parameters would have to have for the
observed effect measure to be completely explained by bias.
}
\description{
Calculate an E-value for a specified set of biases.
}
\examples{
# Calculate an E-value for unmeasured confounding
multi_evalue(est = RR(4), biases = confounding())
# Equivalent to
evalues.RR(4)

# Calculate a multi-bias E-value for selection bias
# and misclassification
multi_evalue(est = RR(2.5),
         biases = multi_bias(selection("selected"),
                   misclassification("outcome")))

# Calculate a multi-bias E-value for all three
# available types of bias
biases <- multi_bias(confounding(),
                     selection("general", "S = U"),
                     misclassification("exposure",
                            rare_outcome = TRUE))
multi_evalue(est = RR(2.5), biases = biases)

# Calculate a multi-bias E-value for a non-rare OR
# using the square root approximation
multi_evalue(est = OR(2.5, rare = FALSE), biases = biases)

# Calculate a non-null multi-bias E-value
multi_evalue(est = RR(2.5), biases = biases, true = 2)

}
\keyword{multi-bias}
