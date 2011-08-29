\name{getSiteDataSources}
\alias{getSiteDataSources}
\title{
Get site data sources.
}
\description{
This function retrieves the available data sources for the specified site from the server.
}
\usage{
getSiteDataSources(site_number)
}
\arguments{
  \item{site_number}{
The site number.
}
}
\details{

}
\value{
\item{site}{The site number.}
\item{dataSources}{The available data sources n the server.}
}
\author{
Jason Lessels <jlessels@gmail.com>
}
\examples{
getSiteDataSources(410044)
}
