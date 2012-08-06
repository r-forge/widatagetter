\name{likfit_prob}
\alias{likfit_prob}

\title{
Likfit probablilities.
}
\description{
Get t-statistics of a \code{\link{likfit}} object.
}
\usage{
likfit_prob(x.reml, x.lm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x.reml}{
A result from the \code{\link{likfit}} function. 
}
  \item{x.lm}{
A linear model with the same structure as the trend matrix of the x.reml argument. This is only used to name the rows of the returned matrix.
}
}
\examples{
reml <- likfitRcpp(s100, ini=c(0.5, 0.5), fix.nug = FALSE, lik.met = "REML")
dat <- data.frame(data=s100$data)
likfit_prob(reml, lm(data~1, data=dat))

}
