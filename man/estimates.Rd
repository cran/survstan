% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimates.R
\name{estimates}
\alias{estimates}
\title{Parameters estimates of a survstan model}
\usage{
estimates(object, ...)
}
\arguments{
\item{object}{an object of the class survstan.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
the parameters estimates of a given survstan model.
}
\description{
Parameters estimates of a survstan model
}
\examples{
\donttest{
library(survstan)
fit <- aftreg(Surv(futime, fustat) ~ ecog.ps + rx, data = ovarian, baseline = "weibull", init = 0)
estimates(fit)
}

}
