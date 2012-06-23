\name{getSitesWithinCircle}
\alias{getSitesWithinCircle}

\title{
Get site information within circle area
}
\description{
This function returns all the sites within a given circle. The user must provide the center of the circle and the size of the circle in degrees.
}
\usage{
getSitesWithinCircle(latitude, longitude, degrees, mustBeActive = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{latitude}{
The center latitude.
}
  \item{longitude}{
The center longitude
}
  \item{degrees}{
The size of the circle in degrees. See details for more info.
}
  \item{mustBeActive}{
Boolean. If sampling must still be undertaken at the site. Defaults to TRUE.
}
}
\details{
The size of the circle is in degrees. This is roughly 100km per 1 degree. The server request and the data sorting can be quite time consuming. It is currently unknown if the size is relative to the diameter or the radius. 
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
\item{stillActive}{Is sampling still active?}
}
\author{
Jason Lessels <jlessels@gmail.com>
}

\examples{
getSitesWithinCircle(-34.9,148,0.5,mustBeActive=T)
}