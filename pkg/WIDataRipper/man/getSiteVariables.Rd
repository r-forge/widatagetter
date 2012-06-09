\name{getSiteVariables}
\alias{getSiteVariables}
\title{
Get site variable information.
}
\description{
This function retrieves information about the variables stored on the database. Within the database there are potentially several data sources. TODO add info about using getSiteDataSources.
}
\usage{
getSiteVariables(site_number, data_source = "A")
}
\arguments{
  \item{site_number}{
The number of the desired site.
}
\item{data_source}{
The source of the data within the database. See details.
}
}
\details{
There appears to be two main data sources available. The data source "A" appears to have been verified and all data points appear to have quality ratings. The data source "PROV" appears to be the latest data, or data that has not been verified with quality values. With this reasoning the data source "A" is set as default. 
}
\value{
\item{siteName}{The long name of the site.}
\item{siteShortName}{ The short name of the site.}
\item{siteNumber}{ The site number.}
\item{variables}{A data.base with the starting and end dates. 
\\ Variable number and variable type is also included.}
}

\author{
Jason Lessels <jlessels@gmail.com>
}

\seealso{
TODO add link to the get data source function.
}
\examples{
getSiteVariables(site_number=410044)
getSiteVariables(site_number=410044,data_source="PROV")
}
