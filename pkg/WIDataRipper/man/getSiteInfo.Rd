\name{getSiteInfo}
\alias{getSiteInfo}

\title{
Get site information
}
\description{
This function obtains the location and the elevation of the request site. It also obtains any revelant comment about the site. Checks if data is still being collected at the site.
}
\usage{
getSiteInfo(site_number)
}

\arguments{
  \item{site_number}{
The site number
}
}
\details{

}
\value{
\item{siteName}{The name of the site.}
\item{siteShortName}{The sites shortname.}
\item{siteNumber}{The site number.}
\item{siteType}{The type of sampling site.}
\item{latitude}{The latitude of the site.}
\item{longitude}{The longitude of the site.}
\item{elevation}{The elevation of the site.}
\item{geoDatum}{The datum of the spatial attributes}
\item{zone}{The zone of the spatial attributes}
\item{comments}{Any comments on the site.}
\item{stillActive}{Is sampling still active?}}

\author{
Jason Lessels <jlessels@gmail.com>
}
\examples{
getSiteInfo(410044)
}