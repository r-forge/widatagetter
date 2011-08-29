\name{getLatest}
\alias{getLatest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get latest values.
}
\description{
This function gets the latest 7 days of values of a specific site and variable.
}
\usage{
getLatest(site_number, variable_number = 100)
}
\arguments{
  \item{site_number}{
The site number
}
  \item{variable_number}{
The variable number. See \code{\link{getSiteVariables}}
}
}
\details{
This function is designed to quickly get the latest 7 days of hourly values for a specific variable.
}
\value{
\item{data}{ Data frame with date, requested variable and qualityCode.}
\item{siteNumber}{The site number.}
\item{units}{The units of the measured variable.}
\item{siteShortName} {The short name of the site.}
\item{siteName}{The site name.}
\item{qualityCodes} {A data frame with the quality code value and  comments about that code value. Only revelant codes are provided. }
}

\author{
Jason Lessels <jlessels@gmail.com>
}
\examples{
# Check if the site has latest values avaliable. We are looking for PROV
getSiteDataSources(410044)
# Check which variables are avaiable
getSiteVariables(410044,data_source="PROV")
# Grab the latest values
latest <- getLatest(410044,variable_number=100)
plot(latest$data[,1:2],type="l")

# Or you can just try your luck out.
latest<-getLatest(410043)
plot(latest$data[,1:2],type="l")
}