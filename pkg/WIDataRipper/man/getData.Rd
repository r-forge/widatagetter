\name{getData}
\alias{getData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get data
}
\description{
This function retrieves the data from one site for one particular variable from the requested start time to the request end time. It uses the interval and the multiplier to determine the frequency of the time series data.
}
\usage{
getData(site_number, start_time, end_time, variable_number = 100, interval = "hour", multiplier = 1, data_type = "mean")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{site_number}{
The number of the sampling site. TODO add link to site list function.
}
  \item{start_time}{
The starting time of the time series "yyyymmddHHMMSS".
}
  \item{end_time}{
The ending time of the time series "yyyymmddHHMMSS".
}
  \item{variable_number}{
The number of the desired variable. see \code{\link{getSiteVariables}}.
}
  \item{interval}{
The time series interval. See details below for more info.
}
  \item{multiplier}{
The frequency of the interval (must be an integer).
}
  \item{data_type}{
What to return for the desired interval. See details below for more info.
}
}
\details{
This function acts as a wrapper for the SOAP server of the water info website. The function is limited to one site and one variable per request. This function is designed solely to extract data from the server, The other functions in this package are designed to retrieve more information about the type of data that exists on the server. 

The site number is required for this request. Currently the best method for obtaining the site number is by manually going to the website. In future releases it will be possible to obtain a list of all sites. 

Both start and end time must both be supplied as a string. In future releases this will be fixed to allow for as.POSIXct type objects. However currently the required format is %Y%d%m%H%M%S.

The variable number is the number that the server associates with the type of data being requested. The easiest method for determining this for a specific site is to use the {\link{getSiteVariables}} function.

The interval is one of the following;
\itemize{
\item year
\item month
\item day
\item hour
\item minute
\item second
}
The multiplier is a integer of the amount of the interval to aggregate the data into.  By using data_type it is possible to set the aggregation function for the data during that period. The possible options are;
\itemize{
\item mean
\item maxmin
\item max
item min
\item start
\item end
\item first 
\item last
\item tot
\item poin
}
 }
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

  }