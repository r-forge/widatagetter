\name{likfit_step}
\alias{likfit_step}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Backward elimination using likfit.
}
\description{
Backward elimination using the \code{geoR} \code{\link{likfit}} function.
}
\usage{
likfit_step(geodata, formula, ini., ..., noBackSelection = FALSE, useRcpp = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{geodata}{
  
}
  \item{formula}{
Character string of the model to be run. If noBackSelection=FALSE this specifies the full model.
}
  \item{ini.}{
Initial estimates of the model. See \code{\link{likfit}} for more information.
}
  \item{\dots}{
Additional arguments to be sent to the \code{\link{likfit}} function.
}
  \item{noBackSelection}{
Indicates if backward elimination should be used.
}
  \item{useRcpp}{
Should the \code{Rcpp} version of the \code{\link{likfit}} function be used.
}
}
\details{
The backward elimination is based on the significance of each model variable. It drops the least significant variable each step if it is non-significant. For variables which are factors all levels must be non-signiificant to be droped.
}
\value{
\item{formula}{The final formula of the model.}
\item{probs }{The t-probablities of the final model. See \code{\link{likfit_prob}} for more information.}
\item{likfit_model }{The final model.See \code{\link{likfit}} for more information.}
%% ...
}
\references{
This is based on the geoR package. Please use \code{citation(package="geoR")} for information on the underlying function.
}
\author{
Jason Lessels
}
\seealso{
\code{\link{likfit}}
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

bestModel <- likfit_step(geodata=geoData,
                          formula=formula.,
                          ini.=c(0.3,100),
                          nugget=0.01,
                          lambda=0,
                          fix.lambda=FALSE,
                          fix.nugget=FALSE,
                          lik.method="RML",
                          noBackSelection=FALSE,
                          useRcpp=TRUE)
}
