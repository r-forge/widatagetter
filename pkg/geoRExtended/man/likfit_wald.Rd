\name{likfit_wald}
\alias{likfit_wald}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wald test for likfit objects.
}
\description{
A quick function to perform wald tests of likfit objects.
}
\usage{
likfit_wald(object, probs = NULL, terms = NULL, levels = NULL, factor = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Result from a likfit based function. Must be of class \code{likGRF}.
}
  \item{probs}{
Result from \code{\link{likfit_prob}}. Must be specified if factor!=NULL. Default is NULL.
}
  \item{terms}{
An index indicating variable to be included. Equivalent to Terms in \code{\link{wald.test}}. Default is NULL.
}
  \item{levels}{
An index indicating variable to be compared. Equivalent to L in \code{\link{wald.test}}. Default is NULL.
}
  \item{factor}{
A character string which represents the factor string of names in the probs argument.  Default is NULL.
}
}
\details{
A wrapping function of the \code{\link{wald.test}} function. \cr If only object is specified the function uses \code{terms=1:length(object$beta)}
}

\seealso{
\code{\link{wald.test}}
}
\examples{
data(meuse)
temp <- na.omit(meuse)
geoData <- as.geodata(temp,coords.col=1:2,data.col=5, covar.col = c(3:4,6:13))
formula. <- "~elev+soil+lime+om+soil+dist+landuse"

fullModel <- likfit_step(geodata=geoData,
                          formula=formula.,
                          ini.=c(0.3,100),
                          nugget=0.01,
                          lambda=0,
                          fix.lambda=FALSE,
                          fix.nugget=FALSE,
                          lik.method="RML",
                          noBackSelection=TRUE,
                          useRcpp=TRUE)
# Perform a wald test on all.
likfit_wald(fullModel[[3]])
# Perform a wald test on landuse.                          
likfit_wald(fullModel[[3]],probs=fullModel[[2]], factor="landuse")
#Perform a wald test on specific covariates
likfit_wald(fullModel[[3]], terms=1:5)
#Perform a wald test on specific levels of a factor. 
l= t(rep(0,length(fullModel[[3]]$beta)))
l[8] = -1
l[9] = 1
likfit_wald(fullModel[[3]], levels=l)

}
