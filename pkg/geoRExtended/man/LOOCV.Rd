\name{LOOCV}
\alias{LOOCV}
\title{
Leave-one-our-cross-validation statistics.
}
\description{
A function which get some common summary stats of a leave-one-out-cross-validation.
}
\usage{
LOOCV(Xvalid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Xvalid}{
The results from either the \code{\link{xvalid}} function of \code{geoR} or the \code{\link{krige.cv}} function of  the \code{gstat} package.
}
}
\details{
A quick function to to summarise the results of LOOCV.
}
\value{
\item{RMSE}{Root mean square error of the LOOCV.}
\item{ME}{Mean error of the LOOCV.}
\item{thetaMean}{mean theta.}
\item{thetaMedian}{median theta.}
}
\author{
Jason Lessels \email{jlessels@gmail.com}
}

\examples{
reml <- likfitRcpp(s100, ini=c(0.5, 0.5), fix.nug = FALSE, lik.met = "REML")
# Perform cross validation using gstat and geoR methods.
krige_data <- data.frame(data=s100$data, x=s100$coords[,1], y=s100$coords[,2])
model. <- vgm(psill=reml$sigmasq, "Exp", range=reml$phi, reml$nugget)
system.time(gstatXV <- krige.cv(formula=data~1, locations=~x+y, data=krige_data,model=model.))
system.time(geoRXV <- xvalid(s100, model=reml))
# Getting a quick summary of the cross validation results
LOOCV(geoRXV)
LOOCV(gstatXV)
}