\name{getSitesWithinRectangle}
\alias{getSitesWithinRectangle}

\title{
Get site information within Rectangular area.
}
\description{
This function returns all sites within a given rectangle. The user must supply the bottom right and the top left corner of the rectangle.
}
\usage{
getSitesWithinRectangle(bottom_right, top_left, mustBeActive = T)
}
\arguments{
  \item{bottom_right}{
The longitude and latitude of the bottom right hand corner.
}
  \item{top_left}{
The longitude and latitude of the top left hand corner.
}
  \item{mustBeActive}{
Boolean. If sampling must still be undertaken at the site. Defaults to TRUE.
}
}
\details{
The server request and the data sorting can be quite time consuming. It is currently unknown if the size is relative to the diameter or the radius. 

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
getSitesWithinRectangle(c(-34.5,148),c(-35,148.5))
}